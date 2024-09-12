;-----------------------------------------------------------------------------
; MegaSD access. MMC/SD/SDHC support.
; By Manuel Pazos 2012-2013
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; Constants
;-----------------------------------------------------------------------------

; Cards types
CARD_MMC	equ	0
CARD_SD1X	equ	1
CARD_SD2X	equ	2
CARD_SDHC	equ	3

; SD
DATA_TOKEN	equ	#FE
MUL_DAT_TKN_STA	equ	#FC
MUL_DAT_TKN_END	equ	#FD

;-----------------------------------------------------------------------------
; Set SD control area
;-----------------------------------------------------------------------------
SD_ON:
	push	af
	ld	a,#40
	ld	(#6000),a		;Switch bank to SD control area
	pop	af
	ret

SD_OFF:
	push	af
	ld	a,7 * 2
	ld	(#6000),a		;Restore kernel
	pop	af
	ret
	
;-----------------------------------------------------------------------------
; SD initialize: set to SPI mode
; Out: Cy = Timeout
;      NZ = Error
;
;       Z = Ok
;             E = 0 MMC
;             E = 1 SDSC 1.x
;             E = 2 SDSC 2.0 or higher
;             E = 3 SDHC 2.0 or higher
;-----------------------------------------------------------------------------
InitSD:
	call	InitSD0
	ret	c			; Timeout (card removed or damaged?)
	ret	nz			; Command error

	ld	a,e
	ld	(CARD_TYPE),a
	
	xor	a
	ret

InitSD0:
	ld	b,10			; Dummy cycle > 76 clocks
InitSD1:
	ld	a,(#5000)		; Quitamos /CS a la tarjeta
	djnz	InitSD1

	call	SD_CMD
	db	#40,0,0,0,0,#95 	; CMD0: Reset

	//call	SD_INIT			; CMD0: Reset & SPI. Hay que hacerlo de forma especial porque sin o falla en el FS-A1 (!?)

	ld	e,#89	; Debug error code
	ret	c	;response timeout
	and	0f3h	;F7=>F3h Changed to support Nokia
	cp	01h
	ld	e,#88	; Debug error code
	ret	nz

InitSD2:
	call	SD_CMD
	db	#48,0,0,#01,#aa,#87 ; CMD8
	ld	e,#87	; Debug error code
	ret	c
	cp	1
	jr	nz,InitSD3	; SD V1.X or MMC

	ex	de,hl
	ld	e,#86	; Debug error code
	ld	a,(hl)
	nop
	ld	a,(hl)
	nop
	ld	a,(hl)
	and	#f
	cp	1
	ret	nz
	ld	a,(hl)
	cp	#aa
	ret	nz	; Wrong voltage range

InitSD2loop:
	call	SD_CMD
	db	#77,0,0,0,0,#95 ; CMD55
	ret	c

	cp	1
	ld	e,#85	; Debug error code
	ret	nz

	call	SD_CMD
	db	#69,#40,0,0,0,#95	;  ACMD41 (HCS = 1)
	
	ld	e,#84	; Debug error code
	ret	c

	and	1
	cp	1
	jr	z,InitSD2loop

	call	SD_CMD
	db	#7a,#00,0,0,0,#95	; CMD58
	
	ld	e,#83	; Debug error code
	ret	c

	ld	e,#82	; Debug error code
	;or      a
	;ret     nz
	ex	de,hl
	ld	a,(hl)	;CSS 32 bits
	cp	(hl)
	cp	(hl)
	cp	(hl)
	bit	6,a	; bit 30

	ld	e,CARD_SD2X
	jr	z,NOT_SDHC
	inc	e
NOT_SDHC:
	xor	a
	ret

InitSD3:
	call	SD_CMD
	db	#77,#00,0,0,0,#95
	ret	c

	bit	2,a
	jr	nz,MMC_FOUND

	cp	1
	ld	e,#92	; Debug error code
	ret	nz

	call	SD_CMD
	db	#69,#00,0,0,0,#95
	ld	e,#93	; Debug error code
	ret	c

	bit	2,a
	jr	nz,MMC_FOUND

	bit	0,a
	jr	nz,InitSD3

	xor	a
	ld	e,CARD_SD1X	; SD card v1.x
	ret

MMC_FOUND:
	call	SD_CMD
	db	#41,#00,0,0,0,#95
	ret	c		;response timeout

	cp	01h
	jr	z,InitSD3

	;call   SetBlockLen
	ld	e,CARD_MMC	; SD 1.x
	or	a	; z=1: OK  z=0: error
	ret

;-----------------------------------------------------------------------------
; Inicializa la SD y pone el modo SPI.
; Si no se hace así falla en el FS-A1.
; Aparentemente, si se escribe el CRC (#95) desde un registro falla.
;-----------------------------------------------------------------------------
SD_INIT:
	ld	hl,#4000
	ld	a,(hl)		;dummy cycle 8 clocks
	nop			;			[SD_1]
	nop
	ld	(hl),#40	;command
	nop
	ld	(hl),0		;sector(H)
	nop
	ld	(hl),0		;sector(M)
	nop
	ld	(hl),0		;sector(L)
	nop
	ld	(hl),0	;sector(0)
	nop
	ld	(hl),#95	;CRC

	ld	a,(hl)		; CRC
	ld	a,(hl)		; CRC

	ld	bc,#10
.wait:	ld	a,(hl)
	cp	#ff
	ccf
	ret	nc

	djnz	.wait

	scf			;timeout error
	ret
;-----------------------------------------------------------------------------
; In:
;	(DE) = Sector number, 4 bytes
; Out:
;	BCDE  = Sector number
;
; Modify:
;-----------------------------------------------------------------------------
GetSector:
	push	de
	exx
	pop	hl
	
	ld	a,(CARD_TYPE)
	cp	CARD_SDHC
	jr	nz,GetSector2	; Not an SDHC.
	
	ld	e,(hl)
	inc	hl
	ld	d,(hl)
	inc	hl
	ld	c,(hl)
	inc	hl
	ld	b,(hl)
	ret
	
GetSector2:
	; Convert sector number to byte offset (sector * 512)
	ld	e,0	; Address 0
	ld	d,(hl)	; Address 1
	inc	hl
	ld	c,(hl)	; Address 2
	inc	hl
	ld	b,(hl)	; Address 3
	
	sla	d
	rl	c
	rl	b	; Address * 2
	ret

;-----------------------------------------------------------------------------
; SD / MMC Access routine
; Input:
;	A	= Command
;	BCDE	= Access address
;
; Output:	Cy = 0 Z=1 A=0 Successful
;		Cy = 1 Z=0 A=Error code
;-----------------------------------------------------------------------------
MMCCMD:
	ld	hl,#4000
	ld	l,(hl)		;dummy cycle 8 clocks
	nop			;			[SD_1]
	nop
	ld	(hl),a		;command
	nop
	ld	(hl),b		;sector(byte 3)
	nop
	ld	(hl),c		;sector(byte 2)
	nop
	ld	(hl),d		;sector(byte 1)
	nop
	ld	(hl),e		;sector(byte 0)
	nop
	ld	(hl),#95	;CRC

	ld	a,(hl)		; CRC
	ld	a,(hl)		; CRC

	ld	l,#00		; BC=#10? B=0
CMD_L1:
	ld	a,(hl)
	cp	#ff
	ccf
	ret	nc
	
	dec	l
	jr	nz,CMD_L1

	scf			;timeout error
	ret
	

;-----------------------------------------------------------------------------
; Sends a command to SD card
; Modify: hl, bc, de, af
; C = Error
;-----------------------------------------------------------------------------
SD_CMD:
	ex	(sp),hl
	ld	de,#4000
	ld	a,(de)		;dummy cycle 8 clocks
	nop			;			[SD_1]
	nop
	ldi			;command
	ldi			;param 31-24
	ldi			;param 23-16
	ldi			;param 15-8
	ldi			;param 7-0
	ldi			;CRC

	ex	(sp),hl
	ld	a,(de)		; CRC
	ld	a,(de)		; CRC

	ld	b,0
SD_CMD2:
	ld	a,(de)
	cp	#ff
	ccf
	ret	nc

	djnz	SD_CMD2

	scf			;timeout error
	ret


;-----------------------------------------------------------------------------
; Read sectors
;	B    = Number of sectors to read
;	(DE) = First sector number to read
;	HL   = destination address for the transfer
;-----------------------------------------------------------------------------
ReadSD:
	ld	c,2
ReadSD2
	call	GetSector

	ld	a,#40 + 18	; CMD18: READ_MULTIPLE_BLOCK
	call	MMCCMD
	exx
	jr	c,.timeout	; Timeout

	or	a
	jr	nz,.error2	;Cy=0 A=02

	ex	de,hl		;DE = Destination address
	ld	a,DATA_TOKEN	;start data token
	ld	l,b
	;ld	c,0
.loop:
	;ld	b,2
	ld	h,#40
.wait:
	cp	(hl)		;start data token ?
	jr	nz,.wait

	call	transfer
	
	cp	(hl)		;CRC (dummy)
	cp	(hl)		;CRC (dummy)
	dec	l		;Decrement sectors to read
	jp	nz,.loop

	ld	a,#40 + 12	;CMD12 / stop multiblock read
	call	MMCCMD

	ex	de,hl
	xor	a		;A=00 Cy=0 Successful
	ret
	//************************************************ STOP MULTIPLE BLOCK READ *************
.error:
	ld	a,#40 + 12	;CMD12 / stop multiblock read
	call	MMCCMD
	scf			;Cy=1 
.exit:
	ld	a,NRDY		;error code
	ret

.error2:
.timeout:
	push	bc
	push	de
	push	hl		; Destination address
	call	InitSD
	pop	hl
	pop	de
	pop	bc	
	jr	nz,.error	;response error
	jr	c,.exit		;command error
	
	dec	c
	jp	nz,ReadSD2

	ld	a,DISK		;Other error		[SD_1]
	scf			;Card inserted or removed
	ret


;-----------------------------------------------------------------------------
; Write sectors
;	B    = Number of sectors to write
;	(DE) = First sector number to write
;	HL   = source address for the transfer
; Out:
;	Cy   : 1 = Error
;-----------------------------------------------------------------------------

WriteSD:
	push	hl
	call	GetSector	;BCDE = Sector number
	exx
	pop	hl
.try
	ld	a,b
	dec	a
	jp	z,Write1	; Only 1 sector

	exx			; BCDE = access address
	ld	a,#40 + 25	; CMD25: WRITE_MULTIPLE_BLOCK
	call	MMCCMD
	exx
	jr	c,.timeout

	or	a
	ld	a,2
	jr	nz,.exit	;command error

	ld	a,(#4000)		;dummy

.loop:

	ld	de,#4000

	ld	a,#FC
	ld	(de),a

	push	bc
	; Transfer data
	call    transfer

	ex	(sp),hl
	ld	a,(de)		;CRC (dummy)
	ex	(sp),hl
	ld	a,(de)		;CRC (dummy)

	pop	bc

	ld	a,(de)		; Dummy

	ld	a,(de)		; Response
	and	#1f
	cp	#5
	;ld	a,3
	jp	nz,.cancel	;response error
	
	;ACMD22 can be used to find the number of well written write blocks
	call	WaitBusy

	;ld	b,e
	djnz	.loop

	ld	a,(de)		; Dummy
	ld	a,(de)		; Dummy

	ld	a,#fd		; Stop transmission
	ld	(de),a
	nop
	nop			; Extra wait for FS-A1
	ld	a,(de)		; Dummy
	ld	a,(de)		; Dummy

	call	WaitBusy

	xor	a		;A=00 Cy=0 Successful operation
	ret


.cancel:
	call	WaitBusy

	ld	a,(de)		; Dummy
	ld	a,(de)		; Dummy

	ld	a,#fd		; Stop transmission
	ld	(de),a
	nop
	nop			; Extra wait for FS-A1
	ld	a,(de)		; Dummy
	ld	a,(de)		; Dummy

	call	WaitBusy
.exit:
	scf			;Cy=1 
	;ld	a,02h		;Cy=1 A=02
	ret

.timeout:
	push	bc		; Number of cestor
	push	hl		; Source address
	call	InitSD
	pop	hl
	pop	bc
	
	ld	a,1
	jr	nz,.exit	;response error
	jr	c,.exit		;command error
	jp	.try
	
;
; Write a single sector	
;
Write1:
	exx			; BCDE = access address
	ld	a,#40 + 24	; CMD24: WRITE BLOCK
	call	MMCCMD
	exx
	jp	c,.timeout
	or	a
	jp	nz,.exit	;command error

	ld	de,#4000
	ld	a,(de)		;dummy
	ld	a,DATA_TOKEN	;start data token
	ld	(de),a

	call	transfer
	
	ex	(sp),hl
	ld	a,(de)		;CRC (dummy)
	ex	(sp),hl
	ld	a,(de)		;CRC (dummy)
	
	ld	a,(de)		;dummy
	nop
	ld	a,(de)		;receive data response

	and	#1f
	cp	#05
	jr	nz,.exit	;response error
	;ACMD22 can be used to find the number of well written write blocks

.wait:
	ld	a,(de)
	cp	#ff
	jr	nz,.wait

	xor	a		; Read successfully
	ret

.timeout:
	push	bc		; Number of cestor
	push	hl		; Source address
	call	InitSD
	pop	hl
	pop	bc
	
	jr	z,Write1
	jr	nc,Write1

.exit:
	scf			;Cy=1 
	ret

;-----------------------------------------------------------------------------
; Wait until card is not busy
; Modify: AF
;-----------------------------------------------------------------------------	
WaitBusy:
	push	bc

	ld	bc,#8000
.loop:
	ld	a,(#4000)
	or	a
	jr	nz,.end
	
	dec	bc
	ld	a,b
	or	c
	jr	nz,.loop	; Wait while busy

	pop	bc
	scf
	ret
.end:
	pop	bc
	ret
	
	

;-----------------------------------------------------------------------------
; Wait until card is ready
; Modify: AF
;-----------------------------------------------------------------------------
WaitReady:
	push	bc
	ld	bc,#8000
.loop:	
	ld	a,(#4000)
	cp	#ff
	jr	z,.end
	
	dec	bc
	ld	a,b
	or	c
	jr	nz,.loop
	scf
.end	
	pop	bc
	ret

;-----------------------------------------------------------------------------
; 512 LDIs a bit faster than an LDIR
;-----------------------------------------------------------------------------
transfer:
	REPT 512
	ldi
	ENDM
	ret


TestCard:
	call	SD_CMD
	db	#40+16,0,0,2,0,#95
	ret	nc
	
	call	InitSD
	ret	c
	jr	z,TestCard
	scf
	ret

	
;-----------------------------------------------------------------------------
; Read CID register
; Out:
;	Cy   : 1 = Error 0 = Ok
;	HL   : SD data registers
;-----------------------------------------------------------------------------
GetCID:
	call	SD_CMD
	db	#4a,0,0,0,0,#95		; CMD10: SEND_CID
	
	jr	c,GetCID4		; Timeout

	ld	hl,#4000
	ld	a,DATA_TOKEN
	ld	b,0
GetCID2:
	cp	(hl)
	ret	z
	djnz	GetCID2

	scf
	ret

GetCID4:
	call	InitSD			; Modify all registers
	ret	c
	jr	z,GetCID
	scf
	ret
	
;-----------------------------------------------------------------------------
; Get number of available sectors in the card
; Out:
;	DEHL = Number of sectors
;	Cy   : 1 = Error 0 = Ok
;-----------------------------------------------------------------------------
GetSectNum:
	;ld	a,#40+9	; SEND_CSD CMD
	;ld	bc,0
	;ld	de,0
	;call	MMCCMD
	call	SD_CMD
	db	#49,#00,0,0,0,#95	;CMD9: SEND_CSD
	ret	c	; Timeout

	ld	hl,#4000
	ld	a,DATA_TOKEN
	ld	b,0
GetSectNum2:
	cp	(hl)
	jr	z,GetSectNum3
	djnz	GetSectNum2

	scf
	ret

GetSectNum3:
	ld	a,(ix+CARD_TYPE)
	cp	CARD_SDHC
	jr	z,GetSectNumHC ; SDHC
	
	ld	b,5
	call	SkipBytes
	ld	a,(hl)
	and	%1111	; READ_BL_LEN
	ld	c,a

	ld	a,(hl)
	and	%11	; C_SIZE 11-10
	ld	d,a
	ld	e,(hl)
	nop
	ld	a,(hl)
	sla	a
	rl	e
	rl	d
	sla	a
	rl	e
	rl	d
	; DE=C_SIZE

	
	ld	a,(hl)
	and	%11	; C_SIZE_MULT 2-1
	ld	b,(hl)
	sla	b
	rl	a
	;A=C_SIZE_MULT

	add	a,c
	sub	7	; 512 = Sector size
	call	GetExp	; HL = Factor de multiplicacion
	
	ld	b,h
	ld	c,l	; BC = HL
	
	inc	de	; C_SIZE + 1

	call	Mul16	; Numero de sectores = DEHL
	
	ld	b,5
	call	SkipBytes
	
	or	a	; NC = Ok
	ret

GetSectNumHC:
	ld	b,7
	call	 SkipBytes
	ld	a,(hl)
	and	%00111111
	ld	c,a	; C_SIZE 21-16
	
	ld	d,(hl)	; C_SIZE 15-8
	nop
	ld	e,(hl)	; C_SIZE 7-0

	ld	b,6
	call	SkipBytes
	
	inc	de
	ld	h,c
	ld	l,d
	ld	d,e
	ld	e,0

	sla	d
	rl	l
	rl	h

	sla	d
	rl	l
	rl	h

	ex	de,hl
	or	a
	ret

;-----------------------------------------------------------------------------
; Skip B bytes from SD response
;-----------------------------------------------------------------------------
SkipBytes:
	push	af
.loop:
	ld	a,(#4000)
	djnz	.loop
	pop	af
	ret

;-----------------------------------------------------------------------------
; 2^A
;-----------------------------------------------------------------------------
GetExp:
	ld	hl,1
	or	a
	ret	z
GetExp2:
	sla	l
	rl	h
	dec	a
	ret	z
	jr GetExp2

;-----------------------------------------------------------------------------
; Multiplication 16 bits
; DEHL = BC * DE
;-----------------------------------------------------------------------------
Mul16:
	ld hl,0
	ld a,16

Mul16Loop:
	add hl,hl
	rl e
	rl d
	jp nc,NoMul16

	add hl,bc
	jp nc,NoMul16

	inc de
NoMul16:
	dec a
	jp nz,Mul16Loop

	ret


CARD_TYPE:	db	0