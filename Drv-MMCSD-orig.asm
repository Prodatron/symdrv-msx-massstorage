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

.l1012  db #fe,#fe,#fe,#fe
.l1016  db 0


drvinp  ld hl,l1016
        and a
        jp nz,l1026
        bit 6,(hl)
        jp nz,l102e
.l1023
        xor a
        scf
        ret
.l1026
        bit 7,(hl)
        jp nz,l102e
        xor a
        scf
        ret
.l102e
        push ix
        pop hl
        or a
        jr z,l103a
        dec a
        jr z,l1044
        xor a
        scf
        ret
.l103a
        call l1252
        ld a,#fd
        ld (#6ffe),a
        jr l104c
.l1044
        call l1252
        ld a,#fb
        ld (#6ffe),a
.l104c
        push bc
        push de
        push hl
        push iy
        ex de,hl
        call l1241
        call #7e00
        ld a,(stobnkx)
        call bnkmonx
        ex de,hl
        ld hl,#6ffd
        call #7e20
        pop iy
        pop hl
        pop de
        pop bc
        inc hl
        ld a,h
        or l
        jr nz,l1071
        inc iy
.l1071
        inc d
        inc d
        call bnkmofx
        call bnkdofx
        call l1252
        dec b
        jr nz,l104c
        ld a,#ff
        ld (#6ffe),a
        call bnkmofx
        call bnkdofx
        xor a
        ret
.l108c
        push ix
        pop hl
        or a
        jr z,l1098
        dec a
        jr z,l1098
        xor a
        scf
        ret
.l1098
        push bc
        push de
        push hl
        push iy
        call l10b3
        pop iy
        pop hl
        pop de
        pop bc
        inc hl
        ld a,h
        or l
        jr nz,l10ac
        inc iy
.l10ac
        inc d
        inc d
        dec b
        jr nz,l1098
        xor a
        ret
.l10b3
        ex de,hl
        call l1252
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
        jr nz,l10e1
        ld a,#ff
        ld (#6ffe),a
        ld a,#0e
        ld (#5000),a
        inc a
        ld (#7000),a
        jr l10ef
.l10e1
        ld a,#ff
        ld (#6ffe),a
        ld a,#0c
        ld (#5000),a
        inc a
        ld (#7000),a
.l10ef
        push bc
        push de
        push hl
        call #7e70
        ld a,c
        ld (l1016),a
        pop hl
        pop de
        pop bc
        ld a,#fd
        ld (#6ffe),a
        call l1241
        call #7e00
        ld a,(stobnkx)
        call bnkmonx
        ex de,hl
        ld hl,#6ffd
        call #7e20
        ld a,#ff
        ld (#6ffe),a
        call bnkmofx
        call bnkdofx
        ret

drvout
        ld hl,l1016
        and a
        jp nz,l112f
        bit 6,(hl)
        jp nz,l1137
        xor a
        scf
        ret
.l112f
        bit 7,(hl)
        jp nz,l1137
        xor a
        scf
        ret
.l1137
        push ix
        pop hl
        or a
        jr z,l1143
        dec a
        jr z,l1181
        xor a
        scf
        ret
.l1143
        push bc
        push de
        push hl
        push iy
        ex de,hl
        call l1252
        ld a,#fd
        ld (#6ffe),a
        call l1241
        call #7e10
        ld a,(stobnkx)
        call bnkmonx
        ld de,#6ffd
        call #7e40
        ld a,#ff
        ld (#6ffe),a
        call bnkmofx
        call bnkdofx
        pop iy
        pop hl
        pop de
        pop bc
        inc hl
        ld a,h
        or l
        jr nz,l117a
        inc iy
.l117a
        inc d
        inc d
        dec b
        jr nz,l1143
        xor a
        ret
.l1181
        push bc
        push de
        push hl
        push iy
        ex de,hl
        call l1252
        ld a,#fb
        ld (#6ffe),a
        call l1241
        call #7e10
        ld a,(stobnkx)
        call bnkmonx
        ld de,#6ffd
        call #7e40
        ld a,#ff
        ld (#6ffe),a
        call bnkmofx
        call bnkdofx
        pop iy
        pop hl
        pop de
        pop bc
        inc hl
        ld a,h
        or l
        jr nz,l11b8
        inc iy
.l11b8
        inc d
        inc d
        dec b
        jr nz,l1143
        xor a
        ret
        nop
        nop
        nop
        nop
        nop
        nop
.l11c5
        ld c,a
        xor a
        ret

drvact  call stoadrx
        ld bc,#001a
        add hl,bc
        ld a,(hl)
        and #07
        push hl
        call l11c5
        pop hl
        ret c
        ld e,a
        ld a,(hl)
        and #0f
        ld c,a
        or e
        ld (hl),a
        ld a,e
        ld de,#0109
        and #f0
        ld a,d
        jr nz,l1210
        push hl
        push bc
        ld a,(stoadrd)
        ld ix,#0000
        ld iy,#0000
        ld de,(stobufx)
        ld b,#01
        call l108c
push af
ld hl,(stobufx)
inc hl:inc hl:inc hl
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
        pop bc
        pop hl
        ret c
        ld ix,(stobufx)
        ld e,(ix+#18)
        ld d,(ix+#1a)
        ld a,d
        neg
        add #04
.l1210
        inc hl
        inc hl
        ld (hl),e
        inc hl
        ld (hl),#00
        inc hl
        ld (hl),d
        inc hl
        ld (hl),#00
        ld de,#ffed
        add hl,de
        ld b,#04
.l1221
        ld (hl),#00
        inc hl
        djnz l1221
        ld de,#fff0
        add hl,de
        ld (hl),#01
        inc hl
        ld (hl),a
        ld a,c
        and #03
        ld c,a
        ld b,#00
        ld hl,l1012
        add hl,bc
        ld a,(hl)
        cp #fe
        jr z,l123f
        ld (hl),#ff
.l123f
        xor a
        ret
.l1241
        push iy
        pop bc
        xor a
        ld b,c
        ld c,d
        ld d,e
        ld e,a
        rl e
        rl d
        rl c
        rl b
        ret
.l1252
        push hl
        ld a,(drvslt+0) ;slot selection
        di
        out (#a8),a
        ld a,(drvslt+1)
        ld (#ffff),a
        ld a,(drvslt+2)
        out (#a8),a
        pop hl
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
