;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@            S y m b O S   -   M S X   D e v i c e   D r i v e r             @
;@                           FLASHJACKS IDE INTERFACE 3.0.1 2026              @
;@                                                                            @
;@             (c) 2006-2007 by Prodatron / SymbiosiS (Jörn Mika)             @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


;--- MAIN ROUTINES ------------------------------------------------------------
;### IDEOUT -> write sectors (512b)
;### IDEINP -> read sectors (512b)
;### IDEACT -> read and init media (only hardware- and partiondata, no filesystem)

;--- WORK ROUTINES ------------------------------------------------------------
;### IDERED -> wait for data request and read 512byte
;### IDEWRT -> wait for data request and write 512byte
;### IDEERR -> check error state

;--- SUB ROUTINES -------------------------------------------------------------
;### IDESEC -> converts logical sector number into physical sector number
;### IDEADR -> set sector address
;### IDESHW -> maps IDE memory  to #4000-#7FFF
;### IDERDY -> wait for ready for next command
;### IDEDRQ -> wait for data request


;==============================================================================
;### HEADER ###################################################################
;==============================================================================

org #1000-32
relocate_start

db "SMD3"               ;ID
dw ideend-idejmp        ;code length
dw relocate_count       ;number of relocate table entries
ds 8                    ;*reserved*
db 0,3,1                ;Version Minor, Version Major, Type (-1=NUL, 0=FDC, 1=IDE, 2=SD, 3=SCSI)
db "FlashJacks   "      ;comment

idejmp  dw ideinp,ideout,ideact,idemof
idemof  ret:dw 0
        db 32*1+12      ;bit[0-4]=driver ID (12=flashjacks), bit[5-7]=storage type (1=IDE)
        ds 4
ideslt  ds 3
ideactdev ds 1
idefc	db 0

stobnkx equ #815A       ;memory mapping, when low level routines read/write sector data
bnkmonx equ #8112       ;set special memory mapping during mass storage access
bnkmofx equ #8115       ;reset special memory mapping during mass storage access
bnkdofx equ #8136       ;hide mass storage device rom
stoadrx equ #8157       ;get device data record
clcd16x equ #8166       ;HL=BC/DE, DE=BC mod DE
stobufx equ #815B       ;address of 512byte buffer
stoadry equ #8169       ;current device


;*** Variables and Constants

;IDE-Register
ide_w_cmd       equ #f      ;Command
ide_w_digout    equ #6      ;Bit2 -> 1=start Reset, 0=end Reset
ide_r_error     equ #9      ;Bit1=Track0 not found, Bit2=Command Aborted, Bit4=wrong SectorNo, Bit6=Sector not readable, Bit7=BadMarkedSector
ide_r_status    equ #f      ;Bit0 = error; check error register,     Bit1 = index; set once per rotation
                            ;Bit2 = error correction during reading, Bit3 = device is ready for data transfer
                            ;Bit4 = seek finished,                   Bit5 = write command failed
                            ;Bit6 = device is ready for command,     Bit7 = busy; no register access allowed
ide_x_data      equ #8      ;512byte data input/output
ide_x_seccnt    equ #a      ;number of sectors for read/write (0=256x)
ide_x_secnum    equ #b      ;sector number     (CHS), sector number bit 00-07 (LBA)
ide_x_trklow    equ #c      ;track number low  (CHS), sector number bit 08-15 (LBA)
ide_x_trkhig    equ #d      ;track number high (CHS), sector number bit 16-23 (LBA)
ide_x_sdh       equ #e      ;Bit0-3=head (CHS), sector number bit 24-27 (LBA), Bit4=drive(0=Master, 1=Slave), Bit5-7=addressing type(#101=CHS, #111=LBA)

ideprtdat   equ #7c00       ;data

ideprterr   equ #7e01       ;error
ideprtscn   equ #7e02       ;sector count
ideprtsec   equ #7e03       ;sector number
ideprttrk   equ #7e04       ;track low/high (16bit)
ideprtsdh   equ #7e06       ;sdh
ideprtsta   equ #7e07       ;status(r)/command(w)
ideprtdig   equ #7e0e       ;digital output

;IDE-Kommandos
ide_c_recal     equ #10     ;recalibrate heads
ide_c_rdsec     equ #20     ;read sector(s)
ide_c_wrsec     equ #30     ;write sector(s)
ide_c_identy    equ #ec     ;get drive data
ide_c_lbadat    equ #f8     ;get LBA data

;IDE-Infoblock
ide_i_nmhead    equ #06     ;number of heads (16bit)
ide_i_nmsect    equ #0c     ;number of sectors/track (16bit)

ideparadr   equ #1be        ;start of partition table inside the master boot record (MBR)
ideparact   equ #00         ;#80=boot, #00=inactiv (no boot)
idepartyp   equ #04         ;00=not used, 01/11=FAT12, 04/06/0E/14/16/1E=FAT16, 0B/0C/1B/1C=FAT32, 05/0F=extended
ideparbeg   equ #08         ;first logical sector of the partition
ideparlen   equ #0c         ;number of sectors of the partition

idepartok   db #01,#11, #04,#06,#0e,#14,#16,#1e, #0b,#0c,#1b,#1c
idepartan   equ 12


;==============================================================================
;### MAIN ROUTINES ############################################################
;==============================================================================

;### IDEINP -> read sectors (512b)
;### Input      A=device (0-7), IY,IX=logical sector number, B=number of sectors, DE=address, (stobnkx)=banking config
;### Output     CF=0 -> ok
;###            CF=1 -> A=error code (...)
;### Destroyed  AF,BC,DE,HL,IX,IY
ideinp  ld c,1
ideinp0 push de
        call idesec             ;HL=track, E=sector, D=SDH, B=number
        push bc
        call ideadr
        jr c,ideinp3
        ld a,ide_c_rdsec        ;send read command
        ld (ideprtsta),a
        pop bc                  ;B=number of sectors
        pop hl                  ;HL=destination address
        ld a,(stobnkx)
        call bnkmonx
        ld (idefc),a
        ld c,a
ideinp1 push bc
        ex de,hl
        ld hl,ideprtdat
        call idecop
        ex de,hl
        pop bc
        jr c,ideinp2
        call ideerr
        jr c,ideinp2
	call idehde
        call bnkmofx
        call bnkdofx            ;allow irqs between sectors
        call ideshw
        ld a,c
        out (#fc),a
        djnz ideinp1
ideinp2 push af
        call idehde
	call bnkmofx
        call bnkdofx
        pop af
        ret
ideinp3 push af
	call idehde
        call bnkdofx
        pop af
        pop hl
        pop hl
        ret

;### IDEOUT -> write sectors (512b)
;### Input      A=device (0-7), IY,IX=logical sector number, B=number of sectors, DE=address, (stobnkx)=banking config
;### Output     CF=0 -> ok
;###            CF=1 -> A=error code (...)
;### Destroyed  AF,BC,DE,HL,IX,IY
ideout  ld c,1
ideout0 push de
        call idesec             ;HL=track, E=sector, D=SDH, B=number
        push bc
        call ideadr
        jr c,ideinp3
        ld a,ide_c_wrsec        ;send write command
        ld (ideprtsta),a
        pop bc                  ;B=number of sectors
        pop hl                  ;HL=source address
        ld a,(stobnkx)
        call bnkmonx
        ld (idefc),a
        ld c,a
ideout1 push bc
        ld de,ideprtdat
        call idecop
        pop bc
        jr c,ideinp2
        call ideerr
        jr c,ideinp2
        call idehde
	call bnkmofx
        call bnkdofx             ;allow irqs between sectors
        call ideshw
        ld a,c
        out (#fc),a
        djnz ideout1
        jr ideinp2

;### IDEACT -> read and init media (only hardware- and partiondata, no filesystem)
;### Input      A=device (0-7)
;### Output     CF=0 -> ok
;###            CF=1 -> A=error code (...)
;### Destroyed  AF,BC,DE,HL,IX,IY
ideactd dw 0                    ;address of device data record

;### IDEACT -> initialize FlashJacks and read partition data
;### Input      A=device (0-7)
;### Output     CF=0 -> ok
;###            CF=1 -> A=error code
;### Destroyed  AF,BC,DE,HL,IX,IY
;
; This follows the working FlashJacks Nextor initialization -
; software reset, wait BSY=0/DRDY=1, second reset, then access the MBR.
ideact  push af
        call stoadrx
        ld (ideactd),hl
        push hl
        ld bc,stodatsub
        add hl,bc
        ld a,(hl)
        and #f0
        or 1
        ld (hl),a              ;mark LBA device
        pop hl
        pop af
	ld (ideactdev),a
        
	; Software reset
	call ideshw
        ld a,#04
        ld (ideprtdig),a
	nop
        xor a
        ld (ideprtdig),a
	call idehde

        ; Wait for BSY=0 and DRDY=1
        ld hl,256*30
	call ideshw
ideact_wait:
	ld a,(ideprtsta)
        and #c0
        cp #40
        jr z,ideact_ready
        dec l
        jr nz,ideact_wait
        dec h
        jr nz,ideact_wait
        call idehde
	call bnkdofx           ;Release the ideshw mapping before bailing out
        ld a,stoerrabo
        scf
        ret

ideact_ready:
        ; Second software reset, as in the Nextor driver
        call ideshw
	ld a,#04
        ld (ideprtdig),a
	nop
        xor a
        ld (ideprtdig),a
	nop
	call idehde

        ; Read physical sector 0
	ld a,(ideactdev)
        ld ix,0
        ld iy,0
        ld b,1
        ld de,(stobufx)
        ld c,0
        call ideinp0
        ret c

        ; Locate selected partition
        ld hl,(ideactd)
        ld e,l
        ld d,h
        ld bc,stodatsub
        add hl,bc
        ld a,(hl)
        and #0f
        ld c,a
        ld b,a
        ld iy,0
        jr z,ideact5

        ld ix,(stobufx)
        ld bc,ideparadr
ideact1 add ix,bc
        ld bc,16
        dec a
        jr nz,ideact1
ideact2 ld a,(ix+idepartyp)
        ld b,a
        or a
        ld a,stoerrpno
        scf
        ret z
        ld a,b
        ld hl,idepartok
        ld b,idepartan
ideact3 cp (hl)
        jr z,ideact4
        inc hl
        djnz ideact3
        ld a,stoerrptp
        scf
        ret
ideact4 ld c,(ix+ideparbeg+2)
        ld b,(ix+ideparbeg+3)
        db #fd:ld l,c
        db #fd:ld h,b
        ld c,(ix+ideparbeg+0)
        ld b,(ix+ideparbeg+1)
ideact5 ex de,hl
        ld (hl),stotypoky
        inc hl
        ld (hl),stomedhdi
        ld de,stodatbeg-stodattyp
        add hl,de
        ld (hl),c:inc hl
        ld (hl),b:inc hl
        db #fd:ld a,l:ld (hl),a:inc hl
        db #fd:ld a,h:ld (hl),a
        xor a
        ret


;==============================================================================
;### WORK ROUTINES ############################################################
;==============================================================================

;### IDECOP -> wait for data request und transfer 512 bytes
;### Input      HL=source address, DE=destination address
;### Output     DE,HL+=512, CF=1 -> error (A=error code)
;### Destroyed  AF,BC,DE,HL
idecop  call idedrq
        ret c
	
	push hl
	ld hl,256*30
idecop_bsy_pre1
	ld a,(ideprtsta)
	bit 7,a
	jr z,idecop_bsy_pre_ok
	dec l
	jr nz,idecop_bsy_pre1
	dec h
	jr nz,idecop_bsy_pre1
	pop hl
	ld a,stoerrabo
	scf
	ret
idecop_bsy_pre_ok
	pop hl

	ld BC, #2000        ; 32 iterations x 16 copies = 512 bytes (20h-->32d in B)
idecop1
	push bc
	REPEAT 16           ; WinAPE native syntax for repeating blocks
		ldi
		push af	    ; Delay required for the MSX TurboR to be able to read at R800 speed.
		pop af
	REND                ; Mandatory closure of the repetition loop in WinAPE
	pop bc
	djnz idecop1

idecop_wait
	ld hl,256*30            ;Bounded wait, same limit as IDEACT/IDERDY
idecop_wait1
	nop
	nop
	ld a,(ideprtsta)
	bit 3,a                 ;DRQ
	jr nz,idecop_wait_end   ;If DRQ=1, we exit
	bit 7,a                 ;BSY
	jr z,idecop_wait_end    ;If BSY=0, we exit
	dec l
	jr nz,idecop_wait1
	dec h
	jr nz,idecop_wait1
	ld a,stoerrabo
	scf
	ret
idecop_wait_end
	ret

;### IDEERR -> check error state
;### Output     CF=0 -> ok (A=0 everything ok, A=1 data had to be error corrected),
;###            CF=1 -> error (A=error code, 6=error while read/write, 7=error while positioning, 8=abort, 9=unknown)
;### Destroyed  DE
ideerr  ld a,(ideprtsta)
        and 1+4
        ret z               ;CF=0, A=0 -> ok
        cp 4
        ld a,1              ;CF=0, A=1 -> ok, but error correction
        ret z
        ld a,(ideprterr)
        ld e,a
        ld d,6
        and 128+64          ;sector not readable/BadMarkedSector -> r/w error
        jr nz,ideerr1
        ld a,e
        inc d
        and 2+16            ;Track0 not found/wrong SektorNo     -> positioning error
        jr nz,ideerr1
        inc d
        bit 2,e             ;Command Aborted                     -> Abort
        jr nz,ideerr1
        inc d               ;else                                -> unknown
ideerr1 ld a,d
        scf
        ret


;==============================================================================
;### SUB ROUTINES #############################################################
;==============================================================================

;### IDESEC -> converts logical sector number into physical sector number
;### Input      A=device (0-7), IY,IX=logical sector number, C=flag, if start from partition offset
;### Output     HL=track, E=sector, D=SDH
;### Destroyed  F,C,IX,IY
idesec  push af
        call stoadrx
        pop af
        dec c
        ld c,a
        push bc             ;B=number, C=device
        jr nz,idesec2
        push hl             ;add partition offset to IY,IX
        ld bc,stodatbeg
        add hl,bc
        ld c,(hl)
        inc hl
        ld b,(hl)
        inc hl
        add ix,bc
        ld c,(hl)
        inc hl
        ld b,(hl)
        jr nc,idesec1
        inc bc
idesec1 add iy,bc
        pop hl
idesec2 ld bc,stodatsub
        add hl,bc
        db #dd:ld e,l       ;*** 28BIT LBA
        db #dd:ld c,h           ;E = Sektor = Bit  0- 7
        db #fd:ld b,l           ;BC= Track  = Bit  8-23
        db #fd:ld d,h           ;D = Kopf   = Bit 24-27
        ld a,64 ;+#a0 ##!!##
        or d
        ld d,a              ;D=SDH without drive bit
        ld a,(hl)
        and #10
        or d
        ld d,a              ;D=SDH with drive bit (Master/Slave)
        ld l,c
        ld h,b              ;HL=Track
        pop bc
        ld a,c              ;A,B restored
        ret

;### IDEADR -> set sector address
;### Input      HL=track, E=sector, D=SDH, B=number of sectors
;### Output     CF=0 -> ok
;###            CF=1 -> A=error code (...)
;### Destroyed  AF,BC
ideadr  call ideshw
        ; FlashJacks Nextor pattern -
        ; HEAD/SDH first, then LBA low, mid, high, then sector count.
        ld a,d
        ld (ideprtsdh),a
        ld a,e
        ld (ideprtsec),a
        ld a,l
        ld (ideprttrk+0),a
        ld a,h
        ld (ideprttrk+1),a
        ld a,b
        ld (ideprtscn),a
	
	call iderdy
        ret

;### IDESHW -> maps IDE memory to #4000-#7FFF
;### Destroyed  A
ideshw	ld a,(ideslt+0)
        di
        out (#a8),a
	ld a,(ideslt+1)
        ld (#ffff),a
        ld a,(ideslt+2)
        out (#a8),a
        ld a,1
        ld (#4104),a
        ret

;### IDEHDE -> Hide IDE memory to #4000-#7FFF
;### Destroyed  A
idehde	ld a,(ideslt+0)
        di
        out (#a8),a
	ld a,(ideslt+1)
        ld (#ffff),a
        ld a,(ideslt+2)
        out (#a8),a
        ld a,0
        ld (#4104),a
        ret

;### IDERDY -> wait for ready for next command
;### Output     CF=0 -> ok, CF=1 -> error (A=error code)
;### Destroyed  AF
iderdy  push hl
        push bc
        ld b,3                 ;Limit to a bounded number of reset retries
iderdy0 ld hl,256*30
iderdy1 ld a,(ideprtsta)
        and #c0
        cp #40
        jr z,iderdy2
        dec l
        jr nz,iderdy1
        dec h
        jr nz,iderdy1
        dec b
        jr z,iderdy3           ;Retries exhausted give up with an error
        push bc                ;Preserve retry counter across the external calls
        call idehde
	call bnkmofx
        call bnkdofx
        rst #30
        call ideshw
iderdy4 ld a,(idefc)
        out (#fc),a
        pop bc
        jr iderdy0
iderdy3 ld a,stoerrabo
        scf
iderdy2 pop bc
        pop hl
        ret

;### IDEDRQ -> wait for data request
;### Output     CF=0 -> ok, CF=1 -> error (A=error code)
;### Destroyed  AF
idedrq  push hl
        ld hl,256*30            ;Bounded wait, same limit as IDEACT/IDERDY
idedrq1 nop
        nop
	ld a,(ideprtsta)
        bit 3,a
        jr nz,idedrq_end
        bit 7,a
        jr z,idedrq_end
        dec l
        jr nz,idedrq1
        dec h
        jr nz,idedrq1
        pop hl
        ld a,stoerrabo           ;Timeout report as an error
        scf
        ret
idedrq_end:
        pop hl
        call ideerr
        ret

ideend

relocate_table
relocate_end
