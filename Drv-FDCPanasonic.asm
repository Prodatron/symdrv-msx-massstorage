;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@            S y m b O S   -   M S X   D e v i c e   D r i v e r             @
;@                          FDC TC8566AF (PANASONIC)                          @
;@                                                                            @
;@               (c) 1991, 2006-2014 by Alex Wulms & Prodatron                @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


;--- MAIN ROUTINES ------------------------------------------------------------
;### FDCOUT -> write sectors (512b)
;### FDCINP -> read sectors (512b)
;### FDCACT -> read and init media (only hardware- and partiondata, no filesystem)

;--- WORK ROUTINES ------------------------------------------------------------
;### FDCTRK -> seek track
;### FDCRED -> read sector
;### FDCWRT -> write sector
;### FDCRES -> do result phase
;### FDCSEC -> converts logical sector number into physical sector number
;### FDCSID -> read sector ID

;--- SUB ROUTINES -------------------------------------------------------------
;### FDCSHW -> maps FDC memory  to #4000-#7FFF
;### FDCRDY -> waits, until FDC command has been finished
;### FDCC1T -> prepare for level 1 command (seek, recalibrate)
;### FDCCMD -> send command and its parameters to the FDC
;### FDCSND -> send byte to FDC
;### FDCGET -> get byte from FDC
;### FDCMON -> start drive (switch on motor)
;### FDCMOF -> switch motor off after a while (will be called every 1/50 second)


;==============================================================================
;### HEADER ###################################################################
;==============================================================================

org #1000-32
relocate_start

db "SMD3"               ;ID
dw fdcend-fdcjmp        ;code length
dw relocate_count       ;number of relocate table entries
ds 8                    ;*reserved*
db 0,0,0                ;Version Minor, Version Major, Type (-1=NUL, 0=FDC, 1=IDE, 2=SD, 3=SCSI)
db "FDC TC8566AF "      ;comment

fdcjmp  dw fdcinp,fdcout,fdcact,fdcmof0
fdcmof  jp fdcmofx
        db 32*0+5       ;bit[0-4]=driver ID (5=panasonic), bit[5-7]=storage type (0=FDC)
        ds 4
fdcslt  ds 3

stobnkx equ #815A       ;memory mapping, when low level routines read/write sector data
bnkmonx equ #8112       ;set special memory mapping during mass storage access
bnkmofx equ #8115       ;reset special memory mapping during mass storage access
bnkdofx equ #8136       ;hide mass storage device rom
stoadrx equ #8157       ;get device data record
clcd16x equ #8166       ;HL=BC/DE, DE=BC mod DE
stobufx equ #815B       ;address of 512byte buffer
stoadry equ #8169       ;current device


;*** Variables and Constants

fdctrkpos   db -2,-2,-2,-2  ;aktuelle Trackpositionen für die 4 Laufwerke
fdcresult   ds 7            ;result buffer
fdcmoncnt   db 0            ;Motor-On-Counter

fdc_rdywait     equ 50*3    ;maximal 2 Sekunden auf Ready warten (50 Frames*2 Sekunden)
fdc_trkwait     equ 50*5    ;maximal 5 Sekunden auf Seek  warten

fdc_control0    equ #7ff2   ;Control Register 0
fdc_control1    equ #7ff3   ;Control Register 1
fdc_status      equ #7ff4   ;Status Register
fdc_data        equ #7ff5   ;Data Register

fdc_cmd_sense   equ 4
fdc_cmd_seek    equ 15
fdc_cmd_recal   equ 7
fdc_cmd_snint   equ 8
fdc_cmd_read    equ #46
fdc_cmd_write   equ #45
fdc_cmd_id      equ 0


;==============================================================================
;### MAIN ROUTINES ############################################################
;==============================================================================

;### FDCOUT -> write sectors (512b)
;### Input      A=device (0-7), IX=logical sector number, B=number of sectors, DE=address, (stobnkx)=banking config
;### Output     CF=0 -> ok
;###            CF=1 -> A=error code (...)
;### Destroyed  AF,BC,DE,HL,IX,IY
fdcout  ld hl,fdcwrt
        jr fdcinp0

;### FDCINP -> read sectors (512b)
;### Input      A=device (0-7), IX=logical sector number, B=number of sectors, DE=address, (stobnkx)=banking config
;### Output     CF=0 -> ok
;###            CF=1 -> A=error code (...)
;### Destroyed  AF,BC,DE,HL,IX,IY
fdcinpr db 0                ;number of retries when error

fdcinp  ld hl,fdcred
fdcinp0 ld (fdcinp4+1),hl
        call fdcsec         ;A=drive/head, D=sector, E=Track, HL=address, B=number of sectors, C=sectors per track, IYL=number of heads-1
        push af
        call fdcshw
        ld a,6
        ld (fdcinpr),a
        pop af
        push bc
        push de
        push hl
        db #fd:ld h,a       ;IYH=drive/head
        call fdctrk         ;select first track
        db #fd:ld a,h
        ld (fdcexed),a      ;select first side
        pop hl
        pop ix              ;IXL=track, IXH=sector
        pop bc
        jp c,fdcinp3
fdcinp1 push bc
        push hl
        ld a,#20            ;prepare for level 2 command (read, write)
        ld (fdc_control1),a
        db #dd:ld a,h
fdcinp4 call fdcred
        pop hl
        pop bc
        jr c,fdcinp6        ;error -> try again
        dec b
        jr z,fdcinp3        ;all sectors have been loaded -> finished
        ld a,6
        ld (fdcinpr),a
        inc h:inc h
        db #dd:ld a,h       ;test, if last sector of current track reached
        and #0f
        cp c
        jr c,fdcinp2        ;no -> just increase sector
        ld a,-1
        ld (fdcmoncnt),a
        db #dd:ld a,h
        sub c
        db #dd:ld h,a       ;reset sector
        db #fd:inc l        ;test, if doubleside
        db #fd:dec l
        jr z,fdcinp5        ;no  -> increase track
        db #fd:ld a,h
        xor 4
        db #fd:ld h,a       ;yes -> head change
        ld (fdcexed),a
        and 4
        jr nz,fdcinp2       ;don't increase track, if changing from head 0 to 1
fdcinp5 db #dd:inc l
        push hl
        push bc
        db #fd:ld a,h
        db #dd:ld e,l
        call fdctrk         ;select next track
        pop bc
        pop hl
fdcinp2 db #dd:inc h
        jr fdcinp1
fdcinp6 cp stoerrsec        ;*** error
        scf
        jr nz,fdcinp3       ;no sector error -> end
        ld a,(fdcinpr)
        sub 1
        jr nc,fdcinp7
        ld a,stoerrsec      ;did enough tries -> error
        jr fdcinp3
fdcinp7 ld (fdcinpr),a
        rra
        jr nc,fdcinp1
        push hl
        push bc             ;do recalibrate for every second try
        ld e,39
        db #fd:ld a,h
        call fdctrk
        ld e,0
        db #fd:ld a,h
        call fdctrk
        db #dd:ld e,l
        db #fd:ld a,h
        call fdctrk
        pop bc
        pop hl
        jr fdcinp1
fdcinp3 ld c,a
        call bnkmofx
        call bnkdofx
        ld a,c
        ret

;### FDCACT -> read and init media (only hardware- and partiondata, no filesystem)
;### Input      A=device (0-7)
;### Output     CF=0 -> ok
;###            CF=1 -> A=error code (...)
;### Destroyed  AF,BC,DE,HL,IX,IY
fdcact  call stoadrx
        ld bc,stodatsub
        add hl,bc
        ld a,(hl)
        and #07             ;A=drive/head
        push hl
        call fdcsid         ;A=Sector-Offset
        pop hl
        ret c
        ld e,a              ;E=sectorOffset
        ld a,(hl)           ;C=drive/head + DoubleStepFlag
        and #0f
        ld c,a
        or e
        ld (hl),a           ;write back SectorOffset + drive/head + DoubleStepFlag
        ld a,e
        ld de,#109          ;9 sectors per track, 1 head for AMSDOS format
        and #f0
        ld a,d
        jr nz,fdcact1
        push hl
        push bc
        ld a,(stoadry)
        ld ix,0
        ld de,(stobufx)
        ld b,1
        call fdcinp         ;read first physical media sector
        pop bc
        pop hl
        ret c
        ld ix,(stobufx)
        ld e,(ix+bpb_secpertrk)
        ld d,(ix+bpb_numheads)
        ld a,d
        neg
        add 4
fdcact1 inc hl
        inc hl
        ld (hl),e           ;store number of sectors per track
        inc hl
        ld (hl),0
        inc hl
        ld (hl),d           ;store number of heads
        inc hl
        ld (hl),0           ;Flags=0
        ld de,stodatbeg-stodatflg
        add hl,de
        ld b,4
fdcact3 ld (hl),0           ;PartitionStart=0
        inc hl
        djnz fdcact3
        ld de,stodatsta-stodatbeg-4
        add hl,de
        ld (hl),stotypoky   ;device ready
        inc hl
        ld (hl),a           ;medium type is Floppy disc Amsdos SingleSide (1), Fat SingleSide (3) or Fat DoubleSide (2)
        ld a,c
        and 3
        ld c,a              ;C=drive
        ld b,0
        ld hl,fdctrkpos
        add hl,bc
        ld a,(hl)
        cp -2               ;if first run -> recalibrate
        jr z,fdcact2
        ld (hl),-1          ;make track position invalid for drive
fdcact2 xor a
        ret


;==============================================================================
;### WORK ROUTINES ############################################################
;==============================================================================

;### FDCTRK -> seek track
;### Input      A=drive/head, E=Track (Bit7=1 -> double step)
;### Output     CF=0 -> ok
;###            CF=1 -> A=error code (...)
;### Destroyed  AF,BC,DE,HL
fdctrk  ld d,a
        push de
        call fdcmon
        pop de
        ret c
        bit 7,e
        jr z,fdctrk1
        sla e               ;double step -> double the physical track number
fdctrk1 ld a,d
        and 3
        ld (fdccmdt+1),a
        ld l,a
        ld h,0
        ld bc,fdctrkpos
        add hl,bc
        call fdctrk7
        ld a,(hl)
        cp e                ;test, if track already seeked
        ret z
        ld (hl),e           ;no -> remember track and seek it
        cp -2
        ld a,e
        ld (fdccmdt+2),a
        push af
        call z,fdctrk2      ;drive accessed the first time -> recalibrate first
        pop af
        or a
        jr z,fdctrk2        ;if track 0 -> recalibrate
        call fdcc1t         ;*** seek track
        ld a,fdc_cmd_seek
        ld b,3
        call fdccmd
        jr fdctrk0
fdctrk2 call fdcc1t         ;*** recalibrate (track 0)
        ld a,fdc_cmd_recal
        ld b,2
        call fdccmd
fdctrk0 call fdctrk3
        push af
        call fdcw16
        pop af
        ret
fdctrk3 call fdcrdy         ;*** Wait until end of seek
        and 15
        ret z
        call bnkmofx
        call bnkdofx
        rst #30
        call fdcshw
        call fdctrk7
        ld a,fdc_cmd_snint
        ld b,1
        call fdccmd
        call fdcget
        ld b,a
        call fdcget
        ld a,b
        bit 5,a
        jr z,fdctrk3
        or a
        ret
fdctrk7 ld a,0
        out (#fc),a
        ret

;### FDCEXE -> prepare FDC for execution phase (sector read or sector write)
;### Input      A=sector, B=command, C=mode (0=read, 1=write), HL=address, (stobnkx)=bankconfig
;### Output     CF=0 -> ok, DE=converted address, BC=512, HL=statusregister
;###            CF=1 -> A=error code (...)
;### Destroyed  AF
fdcexed db 0                ;Drive/Head
fdcexe  ex de,hl
        ld (fdccmdt+4),a    ;Sektor bei +4 merken
        ld a,(fdcexed)
        and 7
        ld (fdccmdt+1),a    ;Head/Drive merken
        srl a:srl a
        ld (fdccmdt+3),a    ;Head merken
        ld a,b
        ld b,9
        call fdccmd
        ld bc,#200
        ld hl,fdc_status
        ret

;### FDCRED -> read sector
;### Input      A=sector, HL=destination address
;### Output     CF=0 -> ok
;###            CF=1 -> A=error code (...)
;### Destroyed  AF,BC,DE,HL
fdcred  ld bc,fdc_cmd_read*256+0
        call fdcexe
fdcred1 ld a,(hl)
        add a
        jr nc,fdcred1
        add a
        jp p,fdcred2
        ld a,(fdc_data)
        ld (de),a
        inc de
        dec c
        jr nz,fdcred1
        djnz fdcred1
        jr fdcres
fdcred2 call fdcw30
        jr fdcres

;### FDCWRT -> write sector
;### Input      A=sector, HL=source address
;### Output     CF=0 -> ok
;###            CF=1 -> A=error code (...)
;### Destroyed  AF,BC,DE,HL
fdcwrt  ld bc,fdc_cmd_write*256+1
        call fdcexe
fdcwrt1 ld a,(hl)
        add a
        jr nc,fdcwrt1
        add a
        jp p,fdcred2
        ld a,(de)
        ld (fdc_data),a
        inc de
        dec c
        jr nz,fdcwrt1
        djnz fdcwrt1
        jr fdcres

;### FDCRES -> do result phase
;### Output     CF=1 -> A=Fehler-Code (...)
;### Destroyed  AF,DE
fdcres  ld a,2
        ld (fdc_control1),a
        inc a
        ld (fdc_control1),a
        nop
        dec a
        ld (fdc_control1),a
        ld hl,fdcresult
fdcres1 call fdcget
        jr c,fdcres2
        ld (hl),a
        inc hl
        jr fdcres1
fdcres2 ld a,(fdcresult+0)
        and #c8         ;check IC en NOT READY
        ret z           ;ze moeten 0 zijn
        and 8
        ld a,stoerrrdy
        scf
        ret nz
        ld a,(fdcresult+1) ;status 1 (want lees/schrijf fout)
        and %10110111   ;bit 6 en 3 hebben geen betekenis
        ld l,a
        ld a,(fdcresult+2)
        and %01000000   ;selecteer control mark
        or l            ;maak samengestelde fout
        ret

;### FDCSEC -> converts logical sector number into physical sector number
;### Input      A=device (0-7), IX=sector, DE=address
;### Output     A=drive/head, E=track (Bit7=1 -> double step), D=sector number, HL=address, C=sectors per Track, IYL=number of head -1
;### Destroyed  F
fdcsec  push af
        ld a,(stobnkx)
        ex de,hl
        di
        call bnkmonx
        ld (fdcmon5+1),a
        ld (fdctrk7+1),a
        call bnkmofx
        pop af
        push hl
        call stoadrx
        ld de,stodathed
        add hl,de
        ld c,(hl)
        dec c
        db #fd:ld l,c       ;IYL=number of heads -1
        dec hl
        dec hl
        ld c,(hl)
        push bc             ;push number,sec per track
        ld e,c
        ld d,0              ;DE=sectors per track
        ld bc,stodatsub-stodatspt
        add hl,bc
        ld a,(hl)           ;A=drive / sector offset
        push af
        db #dd:ld c,l
        db #dd:ld b,h       ;BC=logical sector
        call clcd16x        ;L=track
        ld d,e              ;D=physical sector (starting with 0)
        pop af
        ld e,l              ;E=track
        db #fd:dec l
        db #fd:inc l
        jr z,fdcsec2
        srl e               ;doubleside drive -> half track
        jr nc,fdcsec2
        set 2,a             ;previouse track was odd -> head 1
fdcsec2 bit 3,a             ;test, if double step
        jr z,fdcsec1
        set 7,e             ;E7=1 -> double step
fdcsec1 ld l,a
        and #f0             ;A=sector offset
        add d
        inc a
        ld d,a              ;D=sectorID = offset + sector + 1
        ld a,l
        and #07             ;A=drive/head
        pop bc
        pop hl
        ret

;### FDCSID -> read sector ID
;### Input      A=drive/head
;### Output     CF=0 -> ok, A=sector-ID (#00=FAT, #40=System, #C0=Data)
;###            CF=1 -> A=error code (...)
;### Destroyed  F,BC,DE,HL
fdcsid  xor a
        ret


;==============================================================================
;### SUB ROUTINES #############################################################
;==============================================================================

;### FDCSHW -> maps FDC memory  to #4000-#7FFF
;### Destroyed  A
fdcshw  ld a,(fdcslt+0)
        di
        out (#a8),a
fdcshw1 ld a,(fdcslt+1)
        ld (#ffff),a
        ld a,(fdcslt+2)
        out (#a8),a
        ret

;### FDCRDY -> waits, until FDC command has been finished
;### Destroyed  AF
fdcrdy  ld a,(fdc_status)
        bit 4,a
        ret z
        jr fdcrdy

;### FDCC1T -> prepare for level 1 command (seek, recalibrate)
;### Destroyed  A
fdcc1t  ld a,#30
        ld (fdc_control1),a
        jp fdcw18

;### FDCCMD -> send command and its parameters to the FDC
;### Input      A=Kommando, B=Anzahl Parameter
;### Destroyed  AF,BC,HL
fdccmdt ds 5:db 2,9,#50,#ff
fdccmd  ld hl,fdccmdt
        ld (hl),a
        call fdcrdy
fdccmd1 ld a,(hl)
        call fdcsnd
        inc hl
        djnz fdccmd1
        ret

;### FDCSND -> send byte to FDC
;### Input      A=byte
;### Destroyed  AF,C
fdcsnd  ld c,a
fdcsnd1 ld a,(fdc_status)
        add a
        jr nc,fdcsnd1
        ret m
        ld a,c
        ld (fdc_data),a
		ret

;### FDCGET -> get byte from FDC
;### Output     CF=0 -> A=byte
;###            CF=1 -> nothing available
;### Destroyed  F
fdcget  ld a,(fdc_status)
        add	a,a
        jr nc,fdcget
        ret	p
        ld a,(fdc_data)
        ccf
        ret

;### FDCMON -> start drive (switch on motor)
;### Input      A=drive/head
;### Output     CF=0 -> ok
;###            CF=1 -> A=error code (...)
;### Destroyed  AF,BC,DE,HL
fdcmonv db 4
fdcmon  and 3
        ld (fdccmdt+1),a
        ld e,a
        ld a,-1
        ld (fdcmoncnt),a
fdcmon0 nop
        ld d,e
        inc e
        ld a,8
fdcmon1 add a
        dec e
        jr nz,fdcmon1
        ld hl,fdcmonv
        or (hl)
        ld (hl),a
        or d
fdcmon2 ld (fdc_control0),a
        ld a,#20
        ld (fdc_control1),a
        ld e,fdc_rdywait
fdcmon3 ld a,fdc_cmd_sense
        ld b,2
        call fdccmd
        call fdcget
        and 32
        jr nz,fdcmon4
        call bnkmofx
        call bnkdofx
        rst #30
        call fdcshw
fdcmon5 ld a,0
        out (#fc),a
        dec e
        jr nz,fdcmon3
        ld a,stoerrrdy
        scf
        ret
fdcmon4 ld a,#c9
        ld (fdcmon0),a
        ret

;### FDCMOF -> switch motor off after a while (will be called every 1/50 second)
;### Destroyed  AF
fdcmofx ld a,(fdcmoncnt)
        or a
        ret z
        dec a
        ld (fdcmoncnt),a
        ret nz
        call fdcshw
fdcmof0 ld a,4
        ld (fdc_control0),a
        xor a
        ld (fdcmon0),a
        jp bnkdofx

;### FDCWAI -> makes a delay
;### Destroyed  AF,BC,DE,HL
fdcw18  ld bc,459   ;** waits 1,8ms
        jr fdcwai
fdcw16  ld bc,4086  ;** waits 16ms
        jr fdcwai
fdcw30  ld bc,7650  ;** waits 30ms
fdcwai  push bc
        call fdcwai0
        ex de,hl
        pop bc
fdcwai1 push bc
        call fdcwai0
        sbc hl,de
        pop bc
        or a
        sbc hl,bc
        jr c,fdcwai1
        ret
fdcwai0 ld c,#e7
        in h,(c)
        dec c
        in l,(c)
        in a,(#e7)
        cp h
        ret z
        jr fdcwai0

fdcend
relocate_table
relocate_end
