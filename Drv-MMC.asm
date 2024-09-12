org #0ff0
db #01,#00,#00,#4d,#4d,#43,#2f,#53
db #44,#20,#44,#49,#53,#4b,#20,#20
db #16,#10,#a3,#10,#f6,#10,#08,#10
db #c9,#00,#00,#01,#00,#00,#00,#00
.l1012 equ $ + 2
.l1011 equ $ + 1
.l1010
db #00,#00,#fe,#fe,#fe,#fe

        push ix
        pop hl
        or a
        jr z,l101f
        xor a
        scf
        ret
.l101f
        push bc
        push de
        push hl
        call l1030
        pop hl
        pop de
        pop bc
        inc hl
        inc d
        inc d
        dec b
        jr nz,l101f
        xor a
        ret
.l1030
        ex de,hl
        call l117b
        ld a,#fd
        ld (#6ffe),a
        call l116b
        call #7e00
        ld a,(#0202)
        call #0203
        ex de,hl
        ld hl,#6ffd
        call #7e20
        ld a,#ff
        ld (#6ffe),a
        call #0206
        call #0209
        ret
.l1058
        push ix
        pop hl
        or a
        jr z,l1061
        xor a
        scf
        ret
.l1061
        push bc
        push de
        push hl
        call l1072
        pop hl
        pop de
        pop bc
        inc hl
        inc d
        inc d
        dec b
        jr nz,l1061
        xor a
        ret
.l1072
        ex de,hl
        call l117b
        ld a,#02
        ld (#5000),a
        inc a
        ld (#7000),a
        ld a,#fd
        ld (#6ffe),a
        call l116b
        call #7e00
        ld a,(#0202)
        call #0203
        ex de,hl
        ld hl,#6ffd
        call #7e20
        ld a,#ff
        ld (#6ffe),a
        call #0206
        call #0209
        ret
        push ix
        pop hl
        or a
        jr z,l10ac
        xor a
        scf
        ret
.l10ac
        push bc
        push de
        push hl
        call l10bd
        pop hl
        pop de
        pop bc
        inc hl
        inc d
        inc d
        dec b
        jr nz,l10ac
        xor a
        ret
.l10bd
        ex de,hl
        call l117b
        ld a,#02
        ld (#5000),a
        inc a
        ld (#7000),a
        ld a,#fd
        ld (#6ffe),a
        call l116b
        call #7e10
        ld a,(#0202)
        call #0203
        ld de,#6ffd
        call #7e40
        ld a,#ff
        ld (#6ffe),a
        call #0206
        call #0209
        ret
        db #00,#00,#00,#00,#00,#00
.l10f3
        ld c,a
        xor a
        ret
        call #020c
        ld bc,#001a
        add hl,bc
        ld a,(hl)
        and #07
        push hl
        call l10f3
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
        jr nz,l113a
        push hl
        push bc
        ld a,(#0214)
        ld ix,#0000
        ld de,(#0212)
        ld b,#01
        call l1058
        pop bc
        pop hl
        ret c
        ld ix,(#0212)
        ld e,(ix+#18)
        ld d,(ix+#1a)
        ld a,d
        neg
        add #04
.l113a
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
.l114b
        ld (hl),#00
        inc hl
        djnz l114b
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
        jr z,l1169
        ld (hl),#ff
.l1169
        xor a
        ret
.l116b
        ld b,#00
        ld c,d
        ld d,e
        ld e,#00
        xor a
        rl e
        rl d
        rl c
        rl b
        ret
.l117b
        ld a,(l1010)
        di
        out (#a8),a
        ld a,(l1011)
        ld (#ffff),a
        ret
