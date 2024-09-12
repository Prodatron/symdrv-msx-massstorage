;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@            S y m b O S   -   M S X   D e v i c e   D r i v e r             @
;@                                   MMC/SD                                   @
;@                                                                            @
;@                   (c) 2007-2015 by Sharksym & Prodatron                    @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


;==============================================================================
;### HEADER ###################################################################
;==============================================================================

org #1000-32
relocate_start

db "SMD2"               ;ID
dw drvend-drvjmp        ;code length
dw relocate_count       ;number of relocate table entries
ds 8                    ;*reserved*
db 0,1,2                ;Version Minor, Version Major, Type (-1=NUL, 0=FDC, 1=IDE, 2=SD, 3=SCSI)
db "MMCSDSharksym"      ;comment

drvjmp  dw drvinp,drvout,drvact,drvmof
drvmof  ret:dw 0
        db 32*2+8       ;bit[0-4]=driver ID (8=mmc/sd), bit[5-7]=storage type (2=SD)
        ds 4
drvslt  ds 3        ;slot config for switching device rom at #4000

stobnkx equ #202    ;memory mapping, when low level routines read/write sector data
bnkmonx equ #203    ;set special memory mapping during mass storage access (show destination/source memory block at #8000), HL=address; will be corrected from #0000-#ffff to #8000-#bfff
bnkmofx equ #206    ;reset special memory mapping during mass storage access (show OS memory at #8000)
bnkdofx equ #209    ;hide mass storage device rom (switch back to memory mapper slot config at #4000)
stoadrx equ #20c    ;get device data record
stobufx equ #212    ;address of 512byte buffer
stoadrd equ #214    ;actual device

ideparadr   equ #1be        ;start of partition table inside the master boot record (MBR)
idepartyp   equ #04         ;00=not used, 01/11=FAT12, 04/06/0E/14/16/1E=FAT16, 0B/0C/1B/1C=FAT32, 05/0F=extended
ideparbeg   equ #08         ;first logical sector of the partition

idepartok   db #01,#11, #04,#06,#0e,#14,#16,#1e, #0b,#0c,#1b,#1c
idepartan   equ 12

.l1012      db #fe,#fe,#fe,#fe
drvchnflg   db 0    ;bit6=1 SD channel0 available, bit7=1 SD channel1 available
drvchnsel   db 0    ;channel select code


;### DRVINP -> read sectors (512b)
;### Input      A=device (0-7), IY,IX=logical sector number, B=number of sectors, DE=destination address, (stobnkx)=banking config
;### Output     CF=0 -> ok
;###            CF=1 -> A=error code (...)
;### Destroyed  AF,BC,DE,HL,IX,IY
drvinp  push de
        push bc
        call drvsec         ;bc,de=sector address
        pop ix              ;ixh=number of sectors
        pop hl              ;hl=memory address
scf
ld a,99
ret
drvinp0 call drvshw
        ld a,(drvchnsel)    ;select SD channel
        ld (#6ffe),a
drvinp1 push bc
        push de
        push hl
        push ix
        call #7e00          ;prepare sector read
        ld a,(stobnkx)
        call bnkmonx        ;show memory
        ex de,hl
        ld hl,#6ffd
        call #7e20          ;load sector
        pop ix
        pop hl
        pop de
        pop bc
        call drvnxt
        inc h
        inc h
        call bnkmofx
        call bnkdofx
        call drvshw
        db #dd:dec h
        jr nz,drvinp1
        ld a,#ff
        ld (#6ffe),a
        call bnkmofx
        call bnkdofx
        xor a
        ret

;### DRVINI -> inits SD card interface
;### Destroyed  AF,BC,DE,HL,IX,IY
drvini  call drvshw         ;show device ROM
        ld a,#ff
        ld (#6ffe),a
        xor a
        ld (#5000),a
        ld a,#fe
        ld (#6ffe),a
        ld a,#0a
        ld (#5000),a
        ld a,(#4000)
        cp #41
        jr nz,drvini1
        ld a,#ff
        ld (#6ffe),a
        ld a,#0e
        ld (#5000),a
        inc a
        ld (#7000),a
        jr drvini2
drvini1 ld a,#ff
        ld (#6ffe),a
        ld a,#0c
        ld (#5000),a
        inc a
        ld (#7000),a
drvini2 call #7e70          ;get available slots
        ld a,c
        ld (drvchnflg),a
        ld a,#ff
        ld (#6ffe),a
        call bnkmofx
        jp bnkdofx

;### DRVOUT -> write sectors (512b)
;### Input      A=device (0-7), IY,IX=logical sector number, B=number of sectors, DE=source address, (stobnkx)=banking config
;### Output     CF=0 -> ok
;###            CF=1 -> A=error code (...)
;### Destroyed  AF,BC,DE,HL,IX,IY
drvout  push de
        push bc
        call drvsec
        pop ix              ;ixh=number of sectors
        pop hl              ;hl=memory address
drvout1 push bc
        push de
        push hl
        push ix
        call drvshw
        ld a,(drvchnsel)
        ld (#6ffe),a
        call #7e10
        ld a,(stobnkx)
        call bnkmonx
        ld de,#6ffd
        call #7e40
        ld a,#ff
        ld (#6ffe),a
        call bnkmofx
        call bnkdofx
        pop ix
        pop hl
        pop de
        pop bc
        call drvnxt
        inc h
        inc h
        db #dd:dec h
        jr nz,drvout1
        xor a
        ret

;### DRVACT -> read and init media (only hardware- and partiondata, no filesystem)
;### Input      A=device (0-7)
;### Output     CF=0 -> ok
;###            CF=1 -> A=error code (...)
;### Destroyed  AF,BC,DE,HL,IX,IY
drvact  push af
        call drvini
        pop af
        push af                 ;*** read hardware data
        call stoadrx
        pop bc
        ex de,hl
        ld hl,stodatsub
        add hl,de
        ld a,(hl)               ;get channel (0-15)
        rra:rra:rra:rra
        and #f
        ld c,a
        cp 2
        ccf
        ld a,stoerrxch          ;>2 -> wrong channel
        ret c
        push hl
        push de
        ld de,stodatflg-stodatsub
        add hl,de
        ld (hl),c               ;store channel (0/1)
        ld a,c
        add a
        cpl
        add #fd+1
        ld (drvchnsel),a
        ld bc,0
        ld de,0
        db #dd:ld h,1
        ld hl,(stobufx)
        call drvinp0            ;read first physical sector (=Bootsector or MBR)
push af
ld hl,(stobufx)
ld de,4
add hl,de
ld de,#c000
ld a,(hl):inc hl
call clchex
ld a,(hl):inc hl
call clchex
ld a,(hl):inc hl
call clchex
ld a,(hl):inc hl
call clchex
pop af
        pop de
        pop hl
        ret c
        ld a,(hl)
        and #f                  ;A=partition number
xor a
        ld c,a
        ld b,a
        ld iy,0
        jr z,drvact5
        ld ix,(stobufx)
        ld bc,ideparadr
drvact1 add ix,bc               ;IX points to partition table
        ld bc,16
        dec a
        jr nz,drvact1
drvact2 ld a,(ix+idepartyp)
        ld b,a
        or a
        ld a,stoerrpno          ;partion does not exist -> error
        scf
        ret z
        ld a,b
        ld hl,idepartok
        ld b,idepartan
drvact3 cp (hl)
        jr z,drvact4
        inc hl
        djnz drvact3
        ld a,stoerrptp          ;partition type not supported -> error
        scf
        ret
drvact4 ld c,(ix+ideparbeg+2)
        ld b,(ix+ideparbeg+3)
        db #fd:ld l,c
        db #fd:ld h,b
        ld c,(ix+ideparbeg+0)
        ld b,(ix+ideparbeg+1)
drvact5 ex de,hl                ;HL=device data record
        ld (hl),stotypoky       ;device ready
        inc hl
        ld (hl),stomedsdc       ;media type is SD-Card
        ld de,stodatbeg-stodattyp
        add hl,de
        ld (hl),c:inc hl        ;store start sector
        ld (hl),b:inc hl
        db #fd:ld a,l:ld (hl),a:inc hl
        db #fd:ld a,h:ld (hl),a
        xor a
        ret

;### DRVSEC -> converts logical sector number into physical sector number
;### Input      A=device (0-7), IY,IX=logical sector number
;### Output     BC,DE=physical sector number
;### Destroyed  AF,HL,IX,IY
drvsec  call stoadrx
        ld bc,stodatbeg     ;add partition offset to IY,IX
        add hl,bc
        ld c,(hl)
        inc hl
        ld b,(hl)
        inc hl
        add ix,bc
        ld c,(hl)
        inc hl
        ld b,(hl)
        jr nc,drvsec1
        inc bc
drvsec1 add iy,bc
        ld bc,stodatflg-3-stodatbeg
        add hl,bc
        ld a,(hl)           ;set channel
        and 1
        add a
        cpl
        add #fd+1
ld a,#fb
        ld (drvchnsel),a
        bit 4,(hl)          ;check sector addressing mode
        jr nz,drvsec2
        xor a                   ;MMC/SD1.x/SD2.x
        ld (drvnxt+1),a
        ld e,a
        db #dd:ld d,l
        db #dd:ld c,h
        db #fd:ld b,l
        sla d
        rl c
        rl b
        ret
drvsec2 ld a,drvnxt1-drvnxt-2   ;SD-HC
        ld (drvnxt+1),a
        db #dd:ld e,l
        db #dd:ld d,h
        db #fd:ld c,l
        db #fd:ld b,h
        ret

;### DRVNXT -> calculates next sector address
;### Input      BC,DE=current address
;### Output     BC,DE=next address
;### Destroyed  AF
drvnxt  jr drvnxt0
drvnxt0 inc d:inc d     ;MMC/SD1.x/SD2.x
        ret nz
        inc bc
        ret
drvnxt1 inc de          ;SD-HC
        ld a,d
        or e
        ret nz
        inc bc
        ret

;### DRVSHW -> maps device memory  to #4000-#7FFF and activates SD card access
;### Destroyed  A
drvshw  ld a,(drvslt+0) ;slot selection
        di
        out (#a8),a
        ld a,(drvslt+1)
        ld (#ffff),a
        ld a,(drvslt+2)
        out (#a8),a
        ret



;### CLCHEX -> Converts 8bit value into hex string
;### Input      A=value, (DE)=string
;### Output     DE=DE+2
;### Destroyed  F,C
clchex  ld c,a          ;a=number -> (DE)=hexdigits, DE=DE+2
        rlca:rlca:rlca:rlca
        call clchex1
        ld a,c
clchex1 and 15
        add "0"
        cp "9"+1
        jr c,clchex2
        add "A"-"9"-1
clchex2 ld (de),a
        inc de
        ret

drvend

relocate_table
relocate_end
