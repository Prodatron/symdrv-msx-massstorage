CHGET	equ	#009F
CHPUT	equ	#00A2

; CH376S Command table
GET_IC_VER		equ	#01
RESET_ALL		equ #05
DISK_CONNECT	equ #30
DISK_MOUNT		equ #31
FILE_OPEN		equ #32
SET_FILE_NAME	equ #2F
CHECK_EXIST		equ #06
FILE_CREATE		equ #34
DISK_CAPACITY	equ #3E
FILE_CLOSE		equ #36
BYTE_WRITE		equ #3C
BYTE_WRITE_GO	equ #3D
BYTE_READ		equ #3A
BYTE_RD_GO		equ #3B
SET_USB_MODE	equ #15
GET_STATUS		equ #22
RD_USB_DATA0	equ #27
WR_USB_DATA		equ #2C
BYTE_LOCATE		equ #39
FILE_ENUM_GO	equ #33
FILE_ERASE		equ #35
WR_REQ_DATA 	equ #2D
DISK_READ		equ #54
DISK_RD_GO		equ #55
DISK_WRITE		equ #56
DISK_WR_GO		equ #57
SEG_LOCATE		equ #4A
SEC_READ		equ #4B
SEC_WRITE		equ #4C
TEST_CONNECT	equ #16
DISK_INQUIRY	equ #58
CH376_DAT_BLOCK_LEN	 equ #40




; interrupt state
USB_INT_SUCCESS		equ #14
USB_INT_CONNECT		equ #15
USB_INT_DISCONNECT	equ #16		
USB_INT_DISK_READ	equ #1d
USB_INT_DISK_WRITE	equ #1E
USB_ERR_OPEN_DIR	equ #41
USB_ERR_MISS_FILE	equ #42

; command state
CMD_RET_SUCCESS		equ #51
CMD_RET_ABORT		equ #5F   

; Address ports for PCB proto #2, proto monster
DATAPORT	equ	 #20 		; Address of data port
CMDPORT		equ	 #21 		; Address of command port


;==============================================================================
;### HEADER ###################################################################
;==============================================================================
include "SymbOS-File-Const.asm"
org #1000-32
relocate_start

defb "SMD1"               ;ID
defw rkend-rkjmp        ;code length
defw  0 ;relocate_count       ;number of relocate table entries
ds 8                    ;*reserved*
db 1,0,1                ;Version Major, Version Minor, Type (0=FDC, 1=IDE, 2=SCSI)
db "Rookie drive "
rkjmp  dw DISKIOREAD,DISKIOWRITE,rkact,rkmof
rkmof  ret 
		defw 0 ; forma parte de idemof
		
        db 64*3+11      ;type/ID
        ds 4
rkslt  defw 0


;### Input      A=device (0-7), IY,IX=logical sector number, B=number of sectors, DE=address, (stobnkx)=banking config
;### Output     CF=0 -> ok
;###            CF=1 -> A=error code (...)
;### Destroyed  AF,BC,DE,HL,IX,IY


DISKIOREAD:
		
		push de		; interchange the values to be used with INIR function
		pop hl      ;
		
		ld a,DISK_READ
		out (CMDPORT),a
		ld	a,(ix)
		out (DATAPORT),a			;LBA_LOW
		ld	a,(ix+1)	
		out (DATAPORT),a
		ld	a,(ix+2)
		out (DATAPORT),a
		ld	a,(ix+3)
		out (DATAPORT),a			; LBA HIGH
		ld	a, b		; # of sectors
		out (DATAPORT),a	
		ld 	c,DATAPORT		;Set data port for INIR fucntion

CONTINUEREAD:
		call GETSTATUS
        cp USB_INT_SUCCESS
        jp z, RETURN_UNSET			
		cp USB_INT_DISK_READ
		jp nz, REPORTDISKIOFAIL
			
READSUCCES:
		ld a,RD_USB_DATA0
		out (CMDPORT),a
		in a,(DATAPORT) 			; 
		ld 	b, a		; first read gets data length
READLOOP:
		call	BUSYWAIT	; optional ?
		INIR				; buffer transfer
		ld a,DISK_RD_GO
		out (CMDPORT),a
        jp      CONTINUEREAD

REPORTDISKIOFAIL:
		jp	RETURN_SET

		

		
	
;### Input      A=device (0-7), IY,IX=logical sector number, B=number of sectors, DE=address, (stobnkx)=banking config
;### Output     CF=0 -> ok
;###            CF=1 -> A=error code (...)
;### Destroyed  AF,BC,DE,HL,IX,IY
;	
; DISKIOWRITE
;	
DISKIOWRITE:
		push hl
		pop de           ;interchange the values.
		ld a,DISK_WRITE
		out (CMDPORT),a
		ld	a,(ix)
		out (DATAPORT),a			;LBA_LOW
		ld	a,(ix+1)	
		out (DATAPORT),a
		ld	a,(ix+2)
		out (DATAPORT),a
		ld	a,(ix+3)
		out (DATAPORT),a			; LBA HIGH
		ld	a, b		; B contains the number of sectors  
		out (DATAPORT),a	
		ld 	c,DATAPORT		;ld 	c,  DATAPORT	; for OTIR function

CONTINUEWRITE:
		call GETSTATUS				; check the result of last operation
		;call PrintHex
        cp USB_INT_SUCCESS
        jp z, RETURN_UNSET			
		cp USB_INT_DISK_WRITE
		jp nz, REPORTDISKIOFAIL
	
WRITESUCCES:
		ld a,WR_USB_DATA		
		out (CMDPORT),a
		ld b,CH376_DAT_BLOCK_LEN  ; Set the size of 
		ld a,b
		out (DATAPORT),a
		
WRITELOOP:
		call	BUSYWAIT 	; may be optional ?
		OTIR				; Executes the buffer transfer
        ld a,DISK_WR_GO	; Continuation command for writing.
        out (CMDPORT),a
        jp      CONTINUEWRITE		
		

rkact:
	call INITHARD
	ret
		
		
;*******************************************************

; INIHRD



INITHARD:
		ld b,0xff

INITLOOP:	
		in a,(DATAPORT)    ;Read data buffer of CH376 to prevent some post reboot hangs. (reset command does nothing if the IC is in the middle of a previous execution, and in this hardware module there isnt a hardware /RESET line)
		djnz INITLOOP
		call RAWRESET
		call DELAY
		 
		
		call  CHECKDEVICE     ; check if device exists
		jp c,	DEVICEFAIL 
	
		call	HRDRESET     ; performs a total reset of the device
		jp  c,	RESETFAIL
		
		 
		call 	SETUSBMODE      ; Sets the correct mode for mass device operations
		jp  c,	SETMODEFAIL
		
		
		call 	DISKCONN        ; check the conection of the device
		jp  c, DISCONFAIL
	
		; Montar la unidad
	
		call 	MOUNT       ; tries to mount the USB device, not sure if needed for low level sector access.
		jp  c, MOUNTFAIL
		

		jp RETURN_UNSET

DEVICEFAIL:
		ld	hl,exist
		call	Print
		ld	hl,	notfound
		call	Print
		jp RETURN_SET
		
RESETFAIL:
		;ld	hl, reset
		jp SHOWFAIL
SETMODEFAIL:
		;ld	hl,	setusb
		jp SHOWFAIL
MOUNTFAIL:
		;ld hl,mount
		jp SHOWFAIL
DISCONFAIL:
		;ld hl, dskconn
SHOWFAIL:
		;call Print
		;ld hl, failmsg
		ld hl, usbnotready
		call Print
		call DELAY
		call DELAY
		jp RETURN_SET
		
DISKREADY:
		
		ld a,	CHECK_EXIST
		out (CMDPORT),a
		ld	a, #BE			; 'A' bit reversed
		out (DATAPORT),a
		in a,(DATAPORT)
		cp #41			; 'A'
		jp nz, RETURN_SET
		
		ld a,DISK_CONNECT
		out (CMDPORT),a
		call	GETSTATUS
		cp	USB_INT_SUCCESS
		jp z, RETURN_UNSET
		cp 	USB_INT_CONNECT	
		jp z, RETURN_UNSET
		jp RETURN_SET
	

		
		
;
; HRDRESET
;
;	Command Reset for CH376
;   This commands tries to reset USB IC without checking the state register, 
;   because if the device doesn't exist the code locks in an endless wait loop
;
RAWRESET:
		ld a,RESET_ALL
		out (CMDPORT),a
		call 	DELAY
		ret
		
;
; HRDRESET
;
; Command Reset for CH376
;
;   Use only after device was detected.
;
HRDRESET:
		ld a,RESET_ALL
		out (CMDPORT),a
		call 	BUSYWAIT
		in a,(DATAPORT)
		jp nz, RETURN_SET
		jp RETURN_UNSET

;
; SETUSBMODE
; 
; Set the correct USB mode for Mass storage?
;
SETUSBMODE:
		ld a,SET_USB_MODE
		out (CMDPORT),a
		ld	a, #06			; valid USB-HOST, auto SOF
		out (DATAPORT),a				
		call	GETSTATUS
		cp	USB_INT_CONNECT
		jp	z, RETURN_UNSET
		jp RETURN_SET

;
; TESTCON
;
; Test  disk connection, around 2 uS
;	
TESTCON:
		ld a,TEST_CONNECT
		out (CMDPORT),a
		call	GETSTATUS
	
		cp	USB_INT_SUCCESS
		jp	nz, RETURN_SET
		jp	RETURN_UNSET
		
		
		
		
;
; DISKCONN
;
; Execute disk connection Command
;	
DISKCONN:
		ld a,	CHECK_EXIST
		out (CMDPORT),a
		ld	a, #BE			; 'A' bit reversed
		out (DATAPORT),a
		in a,(DATAPORT)
		cp #41			; 'A'
		jp	nz, RETURN_UNSET
	
		ld a,DISK_CONNECT
		out (CMDPORT),a
		call	GETSTATUS
		
		cp	USB_INT_SUCCESS
		jp	nz, RETURN_SET
		jp	RETURN_UNSET

		
SILENTMOUNT:

		ld a,DISK_MOUNT
		out (CMDPORT),a
		call	GETSTATUS
		cp      USB_INT_SUCCESS
		jp	nz, RETURN_SET
		
		ld a,RD_USB_DATA0
		out (CMDPORT),a
		call	BUSYWAIT
		in a,(DATAPORT)					; Read data length
		ld 	b, a
SILIENTINFOLOOP:
		in a,(DATAPORT) ;Read data and do nothing
		djnz	SILIENTINFOLOOP
		jp 	RETURN_UNSET

		
		
;
; MOUNT
;	Execute Mount Command
; 	Read Device name on mount succes???
;

MOUNT:
		ld hl, usbdevice
		call Print

		ld a,DISK_MOUNT
		out (CMDPORT),a

		call	GETSTATUS
		cp      USB_INT_SUCCESS
		jp	nz, RETURN_SET
		
INFO:
		ld a,RD_USB_DATA0
		out (CMDPORT),a
		call	BUSYWAIT
		in a,(DATAPORT)					; Read data length
		ld b,a
INFOLOOP:
		in a,(DATAPORT)
		cp	32
		jp c,   INFONOCHAR		; Comparativa para ver si es carater imprimible, algunos pendrives dan cosas raras
		cp	128
		jp nc,  INFONOCHAR
		
		push af
		ld a,b
		cp 21
		jp nc,INFOUNPOP
		cp 9
		jp c,INFOUNPOP
	
		pop af
		call  CHPUT
		jp INFONOCHAR
		
INFOUNPOP:
		pop af		
	
INFONOCHAR:
		djnz	INFOLOOP
		
							
		jp 	RETURN_UNSET


MOUNTINFO:
		ld a,DISK_INQUIRY;DISK_MOUNT
		out (CMDPORT),a

		call	GETSTATUS
		cp      USB_INT_SUCCESS
		jp	nz, RETURN_SET
		
		;Read data
		ld a,RD_USB_DATA0
		out (CMDPORT),a
		call	BUSYWAIT
		in a,(DATAPORT)					; Read data length
		ld 	b, a
	
	
		
MOUNTINFOLOOP:
		in a,(DATAPORT)
		
		ld (hl), a
		
		         ; print only the characters in the middle of the string. (seems the 8 first refer to manufacturer)
		ld a,b
		cp 21                 
		jp nc,INFONOCHAR2
		cp 9
		jp c,INFONOCHAR2
		inc hl
		ld (hl), 0
		
INFONOCHAR2:
		djnz	MOUNTINFOLOOP
	

		ld b,30
INFONOCHAR3:
		ld (hl), ' '
		inc hl
		djnz INFONOCHAR3
		ld (hl), 0

							
		jp 	RETURN_UNSET
		
		
		
		
;
; CHECKDEVICE
;
;	Check exitence of CH376h device
;
CHECKDEVICE:
		ld a,	CHECK_EXIST
		out (CMDPORT),a
		ld	a, #BE				; 'A' bit reversed
		out (DATAPORT),a
		in a,(DATAPORT)
		cp #41				; 'A'
		jp	nz, RETURN_SET
		jp	RETURN_UNSET
		

; 
; BUSYWAIT
;
; 	Checks for the busy bit of CH376 and waits while busy.	
;
BUSYWAIT:
		nop				; optional, nonsense ?
        in a,(CMDPORT)
		and 	16		; bit 4 register HIGH for busy
		jp nz, 	BUSYWAIT
		ret 

;
; GETSTATUS
;
; 	Obtiene el valor del registro de estado del CH376
;
GETSTATUS:
		call	BUSYWAIT
		ld a,GET_STATUS
		out (CMDPORT),a
		call	BUSYWAIT
	 	in a,(DATAPORT)
		ret

;
; DELAY
;
;	Just a delay loop
;
DELAY:
    	ld de,2000000
DELAY_LOOP:
   		nop
		dec de
		ld a,d
		or e
		jp nz,DELAY_LOOP
		ret

	
	
		

;
;	RETURN_SET
;
;	Set the carry flag as a parameter of the return funcions
;
RETURN_SET:
		scf
		ret

		
;	
;	RETURN_UNSET
;
;	Reset the carry flag as a parameter of the return funcions
;
RETURN_UNSET:
		;or a
		scf
		ccf
		ret

;
; Print
;
;	Prints the pointed string by HL in the screen
;	Modifica HL
Print:
		ld a, (hl)
		or a
		ret z
		call CHPUT
		inc hl
		jp Print 

		
		
exist  			db "USB hardware:",0
notfound		db " Not found",0,10,13
usbnotready 	db "USB device not ready!",13,10,0
usbdevice		db "USB device: ",0

		
		

rkend

relocate_table
relocate_end
