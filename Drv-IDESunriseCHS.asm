;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@            S y m b O S   -   M S X   D e v i c e   D r i v e r             @
;@                    SUNRISE IDE INTERFACE (CHS VERSION)                     @
;@                                                                            @
;@             (c) 2006-2016 by Prodatron / SymbiosiS (Jörn Mika)             @
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

db "SMD2"               ;ID
dw ideend-idejmp        ;code length
dw relocate_count       ;number of relocate table entries
ds 8                    ;*reserved*
db 2,1,1                ;Version Minor, Version Major, Type (-1=NUL, 0=FDC, 1=IDE, 2=SD, 3=SCSI)
db "Sunrise IDE  "      ;comment

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
        ld (iderdy4+1),a
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
        call bnkmofx
        call bnkdofx            ;allow irqs between sectors
        call ideshw
        ld a,c
        out (#fe),a
        djnz ideinp1
ideinp2 push af
        call bnkmofx
        call bnkdofx
        pop af
        ret
ideinp3 push af
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
        ld (iderdy4+1),a
        ld c,a
ideout1 push bc
        ld de,ideprtdat
        call idecop
        call nc,iderdy
        pop bc
        jr c,ideinp2
        call ideerr
        jr c,ideinp2
        call bnkmofx
        call bnkdofx             ;allow irqs between sectors
        call ideshw
        ld a,c
        out (#fe),a
        djnz ideout1
        jr ideinp2

;### IDEACT -> read and init media (only hardware- and partiondata, no filesystem)
;### Input      A=device (0-7)
;### Output     CF=0 -> ok
;###            CF=1 -> A=error code (...)
;### Destroyed  AF,BC,DE,HL,IX,IY
ideactd dw 0                    ;address of device data record
ideact  push af                 ;*** read hardware data
        call stoadrx
        ld (ideactd),hl

        push hl
        ld bc,stodatsub
        add hl,bc
        ld a,(hl)               ;test, if LBA mode supported
        and #10
        or #a0
        ld d,a
        call ideadr0            ;WaitReady, SDH setzen für Driveselect, WaitReady
        pop hl
        pop de
        ld a,stoerrrdy
        jp c,ideact8
        push de
        push hl
        ld a,ide_c_identy
        ld (ideprtsta),a
        ld de,(stobufx)
        push de
        ld hl,ideprtdat
        call idecop             ;Identdaten lesen
        call bnkdofx
        pop ix
        pop iy
        pop de
ld a,"Y"
ld (#c000),a
        ld a,stoerrabo
        ret c
ld a,"X"
ld (#c000),a
xor a
jp ideact8
        ld a,(ix+ide_i_nmhead)      ;Anzahl Köpfe speichern
        ld (iy+stodathed),a
        ld a,(ix+ide_i_nmsect+0)    ;Anzahl Sektoren pro Track speichern
        ld (iy+stodatspt+0),a
        ld a,(ix+ide_i_nmsect+1)
        ld (iy+stodatspt+1),a
        res 0,(iy+stodatflg)    ;LBA-Flag löschen

        ld a,d                  ;*** read partition data
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
ideact8 push af                 ;return on error
        call bnkdofx
        pop af
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
        ld a,512/64
idecop1 ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi
        ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi
        ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi
        ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi
        dec a
        jp nz,idecop1
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

;### STOPHY -> Rechnet logische Sektor-Nummer in physikalische Sektor-Adresse um
;### Eingabe    IY,IX=logische Sektor-Nummer, A=Anzahl Köpfe (max.16), DE=Anzahl Sektoren pro Track (max.256)
;###            CF=1 -> LBA-Modus (A,DE ignorieren), CF=0 -> CHS-Modus (normal)
;### Ausgabe    BC=Track (ab 0), D=Kopf (ab 0), E=Sektor (ab 1[CHS]/0[LBA])
;### Veraendert AF,HL,IX,IY
;### (CHS)      SEKTOR = SEKLOG MOD SEKANZ
;###            KOPF   = (SEKLOG / SEKANZ) MOD KPFANZ
;###            TRACK  = SEKLOG / (SEKANZ * KPFANZ)
;### (LBA)      SEKTOR = SEKLOG[Bit00-07]
;###            KOPF   = SEKLOG[Bit24-27]
;###            TRACK  = SEKLOG[Bit08-23]
stophy  jr c,stophy2
        push af
        push de
        call clcm16             ;HL = A*DE = KpfAnz*SekAnz
        db #fd:ld a,l
        db #fd:or h
        jr z,stophy1            ;nur 16Bit -> 16Bit-Routine verwenden
        push ix             ;*** 32BIT SEKTORNUMMER
        pop bc                  ;IY,BC=SekLog
        pop ix                  ;IX=SekAnz
        pop af                  ;A=KpfAnz
        push hl                 ;KpfAnz*SekAnz pushen
        push bc
        push iy                 ;SekLog pushen
        push af                 ;KpfAnz pushen
        call clcd32             ;HL = SekLog MOD SekAnz = SEKTOR; IY,BC = SekLog/SekAnz
        pop af
        push hl                 ;SEKTOR pushen
        db #dd:ld l,a
        db #dd:ld h,0           ;IX=KpfAnz
        call clcd32             ;HL = (SekLog/SekAnz) MOD KpfAnz = KOPF
        pop de                  ;E=SEKTOR
        ld d,l                  ;D=KOPF
        pop iy
        pop bc                  ;IY,BC=SekLog
        pop ix                  ;IX=KpfAnz*SekAnz
        push de
        call clcd32             ;BC = SekLog / (KpfAnz*SekAnz) = TRACK
        pop de                  ;E=SEKTOR, D=KOPF
        inc e
        ret
stophy1 pop de              ;*** 16BIT SEKTORNUMMER
        pop af
        push hl                 ;KpfAnz*SekAnz pushen
        push ix
        pop bc
        push bc                 ;SekLog pushen
        push af
        call clcd16             ;DE = SekLog MOD SekAnz = SEKTOR, HL=SekLog/SekAnz
        pop af
        ld c,l
        ld b,h
        push de                 ;SEKTOR pushen
        ld e,a
        ld d,0
        call clcd16             ;DE = (SekLog/SekAnz) MOD KpfAnz = KOPF
        pop hl
        ld h,e                  ;L=SEKTOR, H=KOPF
        pop bc                  ;BC=SekLog
        pop de                  ;DE=KpfAnz*SekAnz
        push hl
        call clcd16
        ld c,l
        ld b,h                  ;BC=SekLog / (KpfAnz*SekAnz) = TRACK
        pop de                  ;E=SEKTOR, D=KOPF
        inc e
        ret
stophy2 db #dd:ld e,l       ;*** 28BIT LBA
        db #dd:ld c,h           ;E = Sektor = Bit  0- 7
        db #fd:ld b,l           ;BC= Track  = Bit  8-23
        db #fd:ld d,h           ;D = Kopf   = Bit 24-27
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
idesec2 ld bc,stodatspt
        add hl,bc
        push hl
        ld e,(hl)
        inc hl
        ld d,(hl)           ;DE=Anzahl Sektoren pro Track
        inc hl
        ld c,(hl)
        inc hl
        ld a,(hl)
        and 1               ;A=Modus (0=CHS, 1=LBA)
        push af
        cp 1
        ccf                 ;CF=Modus
        ld a,c              ;C=Anzahl Köpfe
        call stophy         ;BC=Track (ab 0), D=Kopf (ab 0), E=Sektor (ab 1[CHS]/0[LBA])
        pop af
        pop hl
        rrca
        rrca                ;A[6]=LBA-Modus
        or #a0
        or d
        ld d,a              ;D=SDH ohne Laufwerk
        dec hl
        dec hl
        ld a,(hl)
        and #10
        or d
        ld d,a              ;D=SDH mit Laufwerk
        ld l,c
        ld h,b              ;HL=Track
        pop bc
        ld a,c              ;A,B wiederhergestellt
        ret

;### IDEADR -> set sector address
;### Input      HL=track, E=sector, D=SDH, B=number of sectors
;### Output     CF=0 -> ok
;###            CF=1 -> A=error code (...)
;### Destroyed  AF,BC
ideadr  call ideadr0
        ret c
        call iderdy
        ret c
        ld (ideprttrk),hl
        ld c,b
        ld b,e
        ld (ideprtscn),bc
        ret
ideadr0 call ideshw
        call iderdy
        ret c
        ld a,d
        ld (ideprtsdh),a    ;write SDH register
        ret

;### IDESHW -> maps IDE memory  to #4000-#7FFF
;### Destroyed  A
ideshw  ld a,(ideslt+0)
        di
        out (#a8),a
ideshw1 ld a,(ideslt+1)
        ld (#ffff),a
        ld a,(ideslt+2)
        out (#a8),a
        ld a,7*32+1
        ld (#4104),a
        ret

;### IDERDY -> wait for ready for next command
;### Output     CF=0 -> ok, CF=1 -> error (A=error code)
;### Destroyed  AF
iderdy  push hl
        ld hl,256*30
iderdy1 ld a,(ideprtsta)
        and #80
        jr z,iderdy2
        dec l
        jr nz,iderdy1
        call bnkmofx
        call bnkdofx
        rst #30
        call ideshw
iderdy4 ld a,0
        out (#fe),a
        dec h
        jr nz,iderdy1
iderdy3 ld a,stoerrabo
        scf
iderdy2 pop hl
        ret

;### IDEDRQ -> wait for data request
;### Output     CF=0 -> ok, CF=1 -> error (A=error code)
;### Destroyed  AF
idedrq  ld a,(ideprtsta)
        bit 3,a         ;data request
        jr nz,idedrq1
        bit 7,a         ;busy
        jr nz,idedrq
idedrq1 rrca
        ld a,stoerrunk
        ret

;### CLCM16 -> Multipliziert zwei Werte (16bit)
;### Eingabe    A=Wert1, DE=Wert2
;### Ausgabe    HL=Wert1*Wert2 (16bit)
;### Veraendert AF,DE
clcm16  ld hl,0
        or a
clcm161 rra
        jr nc,clcm162
        add hl,de
clcm162 sla e
        rl d
        or a
        jr nz,clcm161
        ret

;### CLCD16 -> Dividiert zwei Werte (16bit)
;### Eingabe    BC=Wert1, DE=Wert2
;### Ausgabe    HL=Wert1/Wert2, DE=Wert1 MOD Wert2
;### Veraendert AF,BC,DE
clcd16  ld a,e
        or d
        ld hl,0
        ret z
        ld a,b
        ld b,16
clcd161 rl c
        rla
        rl l
        rl h
        sbc hl,de
        jr nc,clcd162
        add hl,de
clcd162 ccf
        djnz clcd161
        ex de,hl
        rl c
        rla
        ld h,a
        ld l,c
        ret

;### CLCD32 -> Dividiert zwei Werte (32bit)
;### Eingabe    IY,BC=Wert1, IX=Wert2
;### Ausgabe    IY,BC=Wert1/Wert2, HL=Wert1 MOD Wert2
;### Veraendert AF,BC,DE,IY
clcd32c db 0
clcd32  ld hl,0
        db #dd:ld a,l
        db #dd:or h
        ret z           ;IY,BC=Wert1(Zaehler)
        ld de,0         ;DE,HL=RechenVar
        ld a,32         ;Counter auf 32 setzen
clcd321 ld (clcd32c),a
        rl c
        rl b
        db #fd:ld a,l:rla:db #fd:ld l,a
        db #fd:ld a,h:rla:db #fd:ld h,a
        rl l
        rl h
        rl e
        rl d
        ld a,l
        db #dd:sub l
        ld l,a
        ld a,h
        db #dd:sbc h
        ld h,a
        ld a,e
        sbc 0
        ld e,a
        ld a,d
        sbc 0
        ld d,a
        jr nc,clcd322
        ld a,l
        db #dd:add l
        ld l,a
        ld a,h
        db #dd:adc h
        ld h,a
        ld a,e
        adc 0
        ld e,a
        ld a,d
        adc 0
        ld d,a
        scf
clcd322 ccf
        ld a,(clcd32c)
        dec a
        jr nz,clcd321   ;HL=Wert1 MOD Wert2
        rl c
        rl b
        db #fd:ld a,l:rla:db #fd:ld l,a
        db #fd:ld a,h:rla:db #fd:ld h,a
        ret             ;IY,BC=Wert1 DIV Wert2

ideend

relocate_table
relocate_end
