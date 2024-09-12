;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@            S y m b O S   -   M S X   D e v i c e   D r i v e r             @
;@                             BEER IDE INTERFACE                             @
;@                                                                            @
;@             (c) 2022-2022 by Prodatron / SymbiosiS (Jörn Mika)             @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


;--- MAIN ROUTINES ------------------------------------------------------------
;### IDEOUT -> write sectors (512b)
;### IDEINP -> read sectors (512b)
;### IDEACT -> read and init media (only hardware- and partiondata, no filesystem)

;--- WORK ROUTINES ------------------------------------------------------------
;### IDEERR -> check error state

;--- SUB ROUTINES -------------------------------------------------------------
;### IDESEC -> converts logical sector number into physical sector number
;### IDEADR -> set sector address
;### IDERDY -> wait for ready for next command


;==============================================================================
;### HEADER ###################################################################
;==============================================================================

org #1000-32
relocate_start

db "SMD2"               ;ID
dw ideend-idejmp        ;code length
dw relocate_count       ;number of relocate table entries
ds 8                    ;*reserved*
db 0,1,1                ;Version Minor, Version Major, Type (-1=NUL, 0=FDC, 1=IDE, 2=SD, 3=SCSI)
db "Beer IDE     "      ;comment

idejmp  dw ideinp,ideout,ideact,idemof
idemof  ret:dw 0
        db 32*1+6       ;bit[0-4]=driver ID (6=sunrise), bit[5-7]=storage type (1=IDE)
        ds 4
ideslt  ds 3

stobnkx equ #202    ;memory mapping, when low level routines read/write sector data
bnkmonx equ #203    ;set special memory mapping during mass storage access
bnkmofx equ #206    ;reset special memory mapping during mass storage access
bnkdofx equ #209    ;hide mass storage device rom
stoadrx equ #20c    ;get device data record
clcd16x equ #20f    ;HL=BC/DE, DE=BC mod DE
stobufx equ #212    ;address of 512byte buffer
stoadry equ #214    ;current device


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

db "debug here"

;### IDEINP -> read sectors (512b)
;### Input      A=device (0-7), IY,IX=logical sector number, B=number of sectors, DE=address, (stobnkx)=banking config
;### Output     CF=0 -> ok
;###            CF=1 -> A=error code (...)
;### Destroyed  AF,BC,DE,HL,IX,IY
ideinp  ld c,1
ideinp0 push af
        push ix
        push iy
        push bc
        push de

        push de
        call idesec             ;HL=track, E=sector, D=SDH, B=number
        ld b,1
        call ideadr
        pop hl
        call drvbnk

ideinp1 ld a,#21:out (#30),a
        ld a,#87:out (#32),a
        ld a,#C7:out (#32),a
        ld a,#92:out (#33),a
ideinp2 ld a,#C7:out (#32),a
        ld a,#47:out (#32),a
        in a,(#30)
        bit 7,a
        jr nz,ideinp2
        bit 0,a
        ;jp nz,.586error
        bit 3,a
        jr z,ideinp2
        ld a,#C7:out (#32),a
        ld a,#C0:out (#32),a
        ld c,#30
        ld d,0
        call drvshw
ideinp3 ld a,#40:out (#32),a
        ini:inc c
        ini:dec c
        ld a,#C0:out (#32),a
        dec d
        jr nz,ideinp3
        call bnkmofx

        pop de
        inc d:inc d
        pop bc
        pop iy
        pop ix
        inc ix
        ld a,ixl:or ixh
        jr nz,ideinp4
        inc iy
ideinp4 pop af
        djnz ideinp0
        or a
        ret

;### IDEOUT -> write sectors (512b)
;### Input      A=device (0-7), IY,IX=logical sector number, B=number of sectors, DE=address, (stobnkx)=banking config
;### Output     CF=0 -> ok
;###            CF=1 -> A=error code (...)
;### Destroyed  AF,BC,DE,HL,IX,IY
ideout  ld c,1
ideout0 push af
        push ix
        push iy
        push bc
        push de

        push de
        call idesec             ;HL=track, E=sector, D=SDH, B=number
        ld b,1
        call ideadr
        pop hl
        call drvbnk

ideout1 ld a,#31:out (#30),a
        ld a,#87:out (#32),a
        ld a,#C7:out (#32),a
        ld b,48
ideout2 ex (sp),hl
        ex (sp),hl
        djnz ideout2
        ld a,#C0:out (#32),a
        ld c,#30
        ld d,0
        call drvshw
ideout3 outi:inc c
        outi:dec c
        ld a,#80:out (#32),a
        ld a,#C0:out (#32),a
        dec d
        jr nz,ideout3
        call bnkmofx

        pop de
        inc d:inc d
        pop bc
        pop iy
        pop ix
        inc ix
        ld a,ixl:or ixh
        jr nz,ideout4
        inc iy
ideout4 pop af
        djnz ideout0
        or a
        ret

;### IDEACT -> read and init media (only hardware- and partiondata, no filesystem)
;### Input      A=device (0-7)
;### Output     CF=0 -> ok
;###            CF=1 -> A=error code (...)
;### Destroyed  AF,BC,DE,HL,IX,IY
ideactd dw 0                    ;address of device data record
ideact  push af                 ;*** read hardware data
        call stoadrx
        ld (ideactd),hl
        ld bc,stodatflg         ;* use LBA mode
        add hl,bc
        set 0,(hl)              ;set LBA flag (an old version was also able to support CHS, but that has been removed to save memory)
        pop af                  ;*** read partition data

        ld ix,0
        ld iy,0
        ld b,1
        ld de,(stobufx)
        ld c,0
        call ideinp0            ;read first physical sector (=MBR)
        ret c                   ;read failed -> error

        ld hl,(ideactd)
        ld e,l
        ld d,h                  ;DE=device data record
        ld bc,stodatsub
        add hl,bc
        ld a,(hl)
        and #f                  ;A=partition number
        ld c,a
        ld b,a
        ld iy,0
        jr z,ideact5
        ld ix,(stobufx)
        ld bc,ideparadr
ideact1 add ix,bc               ;IX points to partition table
        ld bc,16
        dec a
        jr nz,ideact1
ideact2 ld a,(ix+idepartyp)
        ld b,a
        or a
        ld a,stoerrpno          ;partion does not exist -> error
        scf
        ret z
        ld a,b
        ld hl,idepartok
        ld b,idepartan
ideact3 cp (hl)
        jr z,ideact4
        inc hl
        djnz ideact3
        ld a,stoerrptp          ;partition type not supported -> error
        scf
        ret
ideact4 ld c,(ix+ideparbeg+2)
        ld b,(ix+ideparbeg+3)
        db #fd:ld l,c
        db #fd:ld h,b
        ld c,(ix+ideparbeg+0)
        ld b,(ix+ideparbeg+1)
ideact5 ex de,hl                ;HL=device data record
        ld (hl),stotypoky       ;device ready
        inc hl
        ld (hl),stomedhdi       ;media type is IDE-HD
        ld de,stodatbeg-stodattyp
        add hl,de
        ld (hl),c:inc hl        ;store start sector
        ld (hl),b:inc hl
        db #fd:ld a,l:ld (hl),a:inc hl
        db #fd:ld a,h:ld (hl),a
        xor a
        ret
ideact8 push af
        call bnkdofx
        pop af
        pop hl      ;return on error
ideacta pop hl
ideact9 pop hl
        ret


;==============================================================================
;### WORK ROUTINES ############################################################
;==============================================================================

;### IDEERR -> check error state
;### Output     CF=0 -> ok (A=0 everything ok, A=1 data had to be error corrected),
;###            CF=1 -> error (A=error code, 6=error while read/write, 7=error while positioning, 8=abort, 9=unknown)
;### Destroyed  DE
ideerr  ;...
        ret


;==============================================================================
;### SUB ROUTINES #############################################################
;==============================================================================

;### DRVBNK -> sets bank switching and recalculates source/destination address
;### Input      HL=address, (stobnkx)=banking
;### Output     HL=corrected address, (drvshw1+1)=block
;### Destroyed  AF
drvbnk  push de
        ld a,(stobnkx)
        call bnkmonx
        pop de
        ld (drvshw1+1),a
        jp bnkmofx
        
;### DRVSHW -> maps application memory at #8000-#bfff
;### Destroyed  A
drvshw  di
drvshw1 ld a,0          ;show application ram
        out (#fe),a
        ret

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
;### Destroyed  AF
ideadr  call iderdy
        ret c
        ld a,#80:out (#33),a:ld a,#C2:out (#32),a                     :ld a,b:out (#30),a    ;number of sectors to transfer
        ld a,#82:out (#32),a:ld a,#C2:out (#32),a:inc a   :out (#32),a:ld a,e:out (#30),a    ;sector number
        ld a,#83:out (#32),a:ld a,#C3:out (#32),a:ld a,#C6:out (#32),a:ld a,d:out (#30),a    ;head number (SDH)
        ld a,#86:out (#32),a:ld a,#C6:out (#32),a:ld a,#C4:out (#32),a:ld a,l:out (#30),a    ;cylinder low
        ld a,#84:out (#32),a:ld a,#C4:out (#32),a:inc a   :out (#32),a:ld a,h:out (#30),a    ;cylinder hi
        ld a,#85:out (#32),a:ld a,#C5:out (#32),a:ld a,#C7:out (#32),a
        ret

;### IDERDY -> wait for ready for next command
;### Output     CF=0 -> ok, CF=1 -> error (A=error code)
;### Destroyed  AF
iderdy  ld a,#92
        out (#33),a
iderdy1 ld a,#C7
        out (#32),a
        ld a,#47
        out (#32),a
        in a,(#30)
        and %11010000
        cp #50
        jp nz,iderdy1      ;loop if BUSY flag set
        ld a,#F7
        out (#32),a
        ret

ideend

relocate_table
relocate_end
