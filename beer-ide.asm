;›››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››
;››                                                                         ››
;››          IDE Hard Disk (lowest level) Read/Write                        ››
;››                                                                         ››
;››                                                                         ››
;›››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››

HDD_SectorIO:
	ei
	push	hl
	push	de
	push	bc
	push	af
	cp	5
	jr	nc,.581			;Wrong partition number
	call	GetWorkBuf
	pop	af
	push	af
	ld	e,a
	ld	d,0
	ex	de,hl
	add	hl,hl
	add	hl,hl
	add	hl,de
	push	hl
	pop	ix
	ld	a,(ix)			;Test if partition exist (must have
	or	(ix+1)			;nonzero start cylinder)
	jp	z,.581
	pop	af
	pop	bc
	pop	de
	pop	hl
.582:	call	Wait_HDD
	push	bc
	push	de
	push	af
	call	SectorTrans
	pop	af
	push	af
	call	XFER_HDD
	pop	af
	pop	de
	inc	de
	pop	bc
	djnz	.582
	xor	a
	ret
;
.581:	pop	bc
	ld	a,4
	scf
	pop	bc
	pop	de
	pop	hl
	ret

;ŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸ
;	Logical sector -> physical
;	CHS addressing used.
;œœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœ

SectorTrans:
	ex	de,hl
	ld	a,(0FD0Bh)
	ld	c,a
	xor	a
	ld	b,10h
.584:	add	hl,hl
	rla
	cp	c
	jr	c,.583
	sub	c
	inc	l
.583:	djnz	.584
	inc	a
	push	de
	ld	e,(ix)
	ld	d,(ix+1)
	add	hl,de
	ex	de,hl
	pop	hl
SetHDDParam:
	ex	af,af'
	ld	a,80h
	out	(33h),a
	ld	a,0C2h
	out	(32h),a
	ld	a,1
	out	(30h),a
	ld	a,82h
	out	(32h),a
	ld	a,0C2h
	out	(32h),a
	inc	a
	out	(32h),a
	ex	af,af'
	out	(30h),a
	ld	a,83h
	out	(32h),a
	ld	a,0C3h
	out	(32h),a
	ld	a,0C6h
	out	(32h),a
	call	calc_HeadCyl
	or	0A0h
	out	(30h),a
	ld	a,86h
	out	(32h),a
	ld	a,0C6h
	out	(32h),a
	ld	a,0C4h
	out	(32h),a
	ld	a,e
	out	(30h),a
	ld	a,84h
	out	(32h),a
	ld	a,0C4h
	out	(32h),a
	inc	a
	out	(32h),a
	ld	a,d
	out	(30h),a
	ld	a,85h
	out	(32h),a
	ld	a,0C5h
	out	(32h),a
	ld	a,0C7h
	out	(32h),a
	ret

;ŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸ
;	HDD data transfer <=>
;œœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœ

XFER_HDD:
	jp	c,hdd_wrsec
hdd_rdsec:
	ld	a,21h
	out	(30h),a
	ld	a,87h
	out	(32h),a
	ld	a,0C7h
	out	(32h),a
	ld	a,92h
	out	(33h),a
.587:	ld	a,0C7h
	out	(32h),a
	ld	a,47h
	out	(32h),a
	in	a,(30h)
	bit	7,a
	jr	nz,.587
	bit	0,a
	jp	nz,.586
	bit	3,a
	jr	z,.587
	ld	a,0C7h
	out	(32h),a
	ld	a,0C0h
	out	(32h),a
	bit	7,h
	push	af
	push	hl
	jr	nz,.588
	ld	hl,(0F34Dh)
.588:	ld	c,30h
	ld	d,0

.589:	ld	a,40h   ;2
	out	(32h),a     ;3
	ini             ;5
	inc	c           ;1
	ini             ;5
	dec	c           ;1
	ld	a,0C0h      ;2
	out	(32h),a     ;3
	dec	d           ;1
	jr	nz,.589     ;3 26

if 0
    ld de,#c040
    ld b,0
    ld a,#30
    ld c,#32

    out (c),e       ;4
    ld c,a          ;1
    ini             ;5
    inc c           ;1
    ini             ;5
    inc c           ;1
    out (c),d       ;4 21 (11b)
    inc b
    jr nz,.589      ;25
endif

	pop	de
	pop	af
	ret	nz
	ld	hl,(0F34Dh)
	ld	bc,0200h
	call	0F1D9h
	ex	de,hl
	ret

;ŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸ
;	Write sector
;œœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœ

hdd_wrsec:
	ld	a,31h
	out	(30h),a
	ld	a,87h
	out	(32h),a
	ld	a,0C7h
	out	(32h),a
	ld	b,30h
.590:	ex	(sp),hl
	ex	(sp),hl
	djnz	.590
	ld	a,0C0h
	out	(32h),a
	push	hl
	bit	7,h
	jr	nz,.591
	ld	de,(0F34Dh)
	ld	bc,0200h
	call	0F1D9h
	ld	hl,(0F34Dh)
.591:	ld	c,30h
	ld	d,0
.592:	outi
	inc	c
	outi
	dec	c
	ld	a,80h
	out	(32h),a
	ld	a,0C0h
	out	(32h),a
	dec	d
	jr	nz,.592
	pop	hl
	inc	h
	inc	h
	ret
;
.586:	pop	bc		;HDD I/O error recovery
	pop	bc
	pop	de
	pop	bc
	ld	a,0C1h
	out	(32h),a
	ld	a,'A'
	out	(32h),a
	in	a,(30h)
	ld	c,a
	ld	a,0C1h
	out	(32h),a
	ld	a,8
	rr	c
	ret	c
	rr	c
	ret	c
	ld	a,0Ch
	rr	c
	ret	c
	rr	c
	ld	a,8
	rr	c
	ret	c
	rr	c
	ld	a,4
	rr	c
	ret	c
	ld	a,2
	scf
	ret

;ŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸ
;	Head/cylinder calculation
;œœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœ

calc_HeadCyl:
	ex	de,hl
	ld	a,(0FD0Ch)
	ld	c,a
	xor	a
	ld	b,10h
.594:	add	hl,hl
	rla
	cp	c
	jr	c,.593
	sub	c
	inc	l
.593:	djnz	.594
	ex	de,hl
	ret

;ŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸ
;	Set DPB using sector 0
;œœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœ

HDD_SetBPB:
	ei
	push	hl
	ld	de,0
	ld	hl,(0F34Dh)
	ld	b,1
	or	a
	call	HDD_SectorIO
	pop	iy
	ret	c
	ld	ix,(0F34Dh)
	ld	a,(ix+15h)
	ld	(iy+1),a
	ld	l,(ix+0Bh)
	ld	(iy+2),l
	ld	h,(ix+0Ch)
	ld	(iy+3),h
	add	hl,hl
	add	hl,hl
	add	hl,hl
	dec	h
	ld	(iy+4),h
	ld	a,0FFh
.595:	inc	a
	srl	h
	jr	c,.595
	ld	(iy+5),a
	ld	a,(ix+0Dh)
	dec	a
	ld	(iy+6),a
	ld	c,0
.596:	inc	c
	rra
	jr	c,.596
	ld	(iy+7),c
	ld	l,(ix+1Ch)
	ld	h,(ix+1Dh)
	inc	hl
	ld	(iy+8),l
	ld	(iy+9),h
	ld	e,(ix+16h)
	ld	(iy+10h),e
	ld	d,0
	ld	b,(ix+10h)
	ld	(iy+0Ah),b
.597:	add	hl,de
	djnz	.597
	ld	(iy+11h),l
	ld	(iy+12h),h
	ld	a,(ix+11h)
	ld	(iy+0Bh),a
	dec	a
	ld	c,(iy+4)
	inc	c
.598:	inc	hl
	sub	c
	jr	nc,.598
	ld	(iy+0Ch),l
	ld	(iy+0Dh),h
	ex	de,hl
	ld	l,(ix+13h)
	ld	h,(ix+14h)
	or	a
	sbc	hl,de
	ld	a,(iy+7)
.600:	dec	a
	jr	z,.599
	srl	h
	rr	l
	jr	.600
.599:	inc	hl
	ld	(iy+0Eh),l
	ld	(iy+0Fh),h
	xor	a
	ret

;ŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸ
;	Choice for FORMAT - no choice
;œœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœ

HDD_Choice:
	xor	a
	ld	l,a
	ld	h,a
	ret

;ŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸ
;	Disk change - NO CHANGE
;œœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœ

HDD_DiskChg:
	push	af
	call	GetWorkBuf
	pop	af
	cp	(ix+14h)
	ld	(ix+14h),a
	jr	nz,.601
	ld	b,1
	xor	a
	ret
;
.601:	ld	b,0FFh
	xor	a
	ret

;ŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸ
; format removed for ROM space freeing
;œœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœ

HDD_Format:
	call	PrintMsg
	db	0Dh,'Use HDPREP utility to format,',0Dh,0Ah
	db	'and test your HDD.',0,0C9h

;ŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸ
;	Initialize Hard Disk
;œœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœ

HDD_StopMotor:
	di
	ld	hl,0
	ld	a,92h
	out	(33h),a
.610:	ld	a,0C7h
	out	(32h),a
	ex	(sp),hl
	ex	(sp),hl
	ex	(sp),hl
	ex	(sp),hl
	ex	(sp),hl
	ex	(sp),hl
	ex	(sp),hl
	ex	(sp),hl
	ex	(sp),hl
	ex	(sp),hl
	ex	(sp),hl
	ex	(sp),hl
	ex	(sp),hl
	ex	(sp),hl
	ld	a,47h
	out	(32h),a
	in	a,(30h)
.602:	or	a
	jp	p,.609
	dec	hl
	ld	a,h
	or	l
	jp	nz,.610
	scf
	ei
	ret
;
.609:	ld	a,0C7h
	out	(32h),a
	ld	a,80h
	out	(33h),a
	ld	a,0C7h
	out	(32h),a
	ld	a,11h		;Recalibrate
	out	(30h),a
	ld	a,87h
	out	(32h),a
	ld	a,0C7h
	out	(32h),a
	ex	(sp),hl
	ex	(sp),hl
	ex	(sp),hl
	ex	(sp),hl
	xor	a
	ret

;ŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸ
;	Get HDD's sectors & heads
;œœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœ

InitFileSystem:
	ld	hl,2000h
ifs00:	dec	hl		;slight delay added...
	ld	a,h
	or	l
	jr	nz,ifs00
        call	HDD_StopMotor
	ret	c
;
	call	IDE_Inf1
	ld	b,3
ifs4:	ld	a,40h
	out	(32h),a
	ld	a,0F0h
	out	(32h),a	;9000
	djnz	ifs4
	ld	a,40h
	out	(32h),a
	in	a,(30h)
	ld	(0FD0Ch),a
	ld	a,0F0h
	out	(32h),a	;9006
	ld	a,40h
	out	(32h),a
	ld	a,0F0h
	out	(32h),a	;9008
	ld	a,40h
	out	(32h),a
	ld	a,0F0h
	out	(32h),a	;900A
	ld	a,40h
	out	(32h),a
	in	a,(30h)
	ld	(0FD0Bh),a
	ld	a,0F0h
	out	(32h),a	;900C
	ld	b,240
ifs5:	ld	a,40h
	out	(32h),a
	ld	a,0F0h
	out	(32h),a
	djnz	ifs5
;
	call	OutputHDDLogo
	ld	b,6
ifs1:	ld	hl,0
ifs2:	dec	hl
	ld	a,h
	or	l
	jr	nz,ifs2
	djnz	ifs1
	xor	a
	ret

;ŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸ
;	Wait for HDD ready
;œœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœ

Wait_HDD:
	push	af
	ld	a,92h
	out	(33h),a
.611:	ld	a,0C7h
	out	(32h),a
	ld	a,47h
	out	(32h),a
	in	a,(30h)
	and	11010000b
	cp	50h
	jp	nz,.611		;loop if BUSY flag set
	ld	a,0F7h
	out	(32h),a
	pop	af
	ret

;ŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸ
;	Get maximal disk count
;œœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœ

GetHardDiskCount:
	ld	l,5
	ret

;ŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸ
;	Read MASTER BOOT (0/0/1)
;œœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœ

ReadMBR:push	bc
	push	de
	ld	de,0
	ld	c,0
	ld	a,1
	push	hl
	call	HDD_Abs_Read
	pop	hl
	pop	de
	pop	bc
	xor	a
	ret

;ŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸ
;	Hard Disk MASTER BOOT
;œœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœ

MasterBoot:
	ld	hl,9000h
	call	ReadMBR
	ret	c
	call	9000h
	ret	c
	call	GetWorkBuf
	ex	de,hl
	ld	hl,9100h
	ld	bc,0500h
.613:	push	bc
	ldi
	ldi
	ex	de,hl
	ld	(hl),0
	inc	hl
	ld	(hl),0
	inc	hl
	ex	de,hl
	ld	a,(hl)
	ld	bc,0Eh
	add	hl,bc
	pop	bc
	or	a
	jr	z,.612
	ld	a,5
	sub	b
	ld	c,a
.612:	djnz	.613
	ex	de,hl
	ld	(hl),0FFh
	push	hl
	push	bc
	call	GetMySlot
	ld	hl,0FB21h
	ld	b,a
	ld	c,0
.614:	ld	a,(hl)
	add	a,c
	ld	c,a
	inc	hl
	ld	a,(hl)
	inc	hl
	cp	b
	jr	nz,.614
	dec	hl
	dec	hl
	ld	a,c
	sub	(hl)
	pop	bc
	add	a,c
	pop	hl
	inc	hl
	ld	(hl),a
	ret

;ŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸ
;	Boot MSX-DOS from HDD
;œœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœ

DOS_HDD_BOOT:
	di
	in	a,(0AAh)
	and	0F0h
	or	7
	out	(0AAh),a
	nop
	in	a,(0A9h)
	and	40h		;If [SELECT] do not boot 
	scf
	ret	z
	call	GetWorkBuf
	ld	a,(ix+15h)
	and	7
	ld	(0F247h),a
	ld	bc,01F8h
	ld	hl,(0F351h)
	ld	de,0
	or	a
	call	0144h
	ld	hl,(0F351h)
	ret

;ŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸ
;	Default DPB pattern
;œœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœ

DEFAULT_DPB:
	db	0F9h
	dw	0200h
	db	0Fh,04h,03h,03h,01h,00h,02h,70h
	dw	0Eh
	dw	02CAh
	db	03h
	dw	7
	db	0D0h,02h

;ŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸ
;	_PARK for BASIC
;œœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœ

call_park:
	push	hl
	ld	hl,(0F34Dh)
	call	IDE_Info
	jr	c,.616
	ld	hl,(0F34Dh)
	ld	de,2
	add	hl,de
	ld	a,(hl)
	inc	hl
	ld	h,(hl)
	ld	l,a
	call	HDD_seek
	call	PrintMsg
	db	0Dh,0Ah,'Heads parked successfully',13,10,0
	pop	hl
	or	a
	ret
;
.616:	call	PrintMsg
	db	0Dh,0Ah,'Identify command rejected',13,10,0
	pop	hl
	or	a
	ret

;ŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸ
;	Get IDE device capabilities
;œœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœ

IDE_Info:
	call	Wait_HDD
	ld	a,80h
	out	(33h),a
	ld	a,0C7h
	out	(32h),a
	ld	a,0ECh
	out	(30h),a
	ld	a,87h
	out	(32h),a
	ld	a,0C7h
	out	(32h),a
	ld	a,92h
	out	(33h),a
	ld	a,0C7h
	out	(32h),a
.621:	ld	a,47h
	out	(32h),a
	in	a,(30h)
	ld	c,a
	ld	a,0C7h
	out	(32h),a
	bit	7,c
	jr	nz,.621		;wait if BUSY bit set
	bit	0,c
	jr	z,erexpl	;explain error
	bit	3,c		;DTRQ set? if no -> loop
	jr	z,.621
	ld	c,30h
	ld	d,0
.622:	ld	a,40h
	out	(32h),a
	ini
	inc	c
	ini
	dec	c
	ld	a,0F0h
	out	(32h),a
	dec	d
	jr	nz,.622
	xor	a
	ret
;
erexpl:	ld	a,0C1h
	out	(032h),a
	ld	a,041h
	out	(032h),a
	in	a,(030h)
	ld	c,a
	ld	a,0C1h
	out	(032h),a
	ld	a,c
	scf
	ret

IDE_Inf1:
	call	Wait_HDD
	ld	a,80h
	out	(33h),a
	ld	a,0C7h
	out	(32h),a
	ld	a,0ECh
	out	(30h),a
	ld	a,87h
	out	(32h),a
	ld	a,0C7h
	out	(32h),a
	ld	a,92h
	out	(33h),a
	ld	a,0C7h
	out	(32h),a
.621a:	ld	a,47h
	out	(32h),a
	in	a,(30h)
	ld	c,a
	ld	a,0C7h
	out	(32h),a
	bit	7,c
	jr	nz,.621a	;wait if BUSY bit set
	bit	0,c
	jr	z,erexpl	;explain error
	bit	3,c		;DTRQ set? if no -> loop
	jr	z,.621
	ret

;ŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸ
;	Seek to track
;œœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœ

HDD_seek:
	call	Wait_HDD
	ld	a,80h
	out	(33h),a
	ld	a,0C4h
	out	(32h),a
	ld	a,l
	out	(30h),a
	ld	a,84h
	out	(32h),a
	ld	a,0C4h
	out	(32h),a
	inc	a
	out	(32h),a
	ld	a,h
.620:	out	(30h),a
	ld	a,85h
	out	(32h),a
	ld	a,0C5h
	out	(32h),a
	ld	a,0C7h
	out	(32h),a
	ld	a,71h
	out	(30h),a
	ld	a,87h
	out	(32h),a
.624:	ld	a,0C7h
	out	(32h),a
	ret

;ŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸ
;	Hard disk logo output
;œœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœ

OutputHDDLogo:
	di
	push	af
	push	hl
	ld	hl,$_logo
	call	sputc
	ld	a,(0FD0Ch)
	call	MakeDec
	ld	hl,$logo2
	call	sputc
	ld	a,(0FD0Bh)
	call	MakeDec
	pop	hl
	pop	af
	ret

;ŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸ
;	create decimal
;œœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœ

MakeDec:
	ld	c,a
	ld	b,8
	xor	a
.623:	rl	c
	adc	a,a
	daa
	djnz	.623
	push	af
	rrca
	rrca
	rrca
	rrca
	call	S79DC
	pop	af
S79DC:	and	0Fh
	add	a,'0'
	call	0A2h
	ret
;
sputc:	ld	a,(hl)
	or	a
	ret	z
	call	0A2h
	inc	hl
	jr	sputc
;
$_logo:	db	12,'BEER 202: IDE HDD driver',13,10
	db	'version 1.8  (c) SOLID, 1995',13,10
	db	'HDD connected ok;',13,10
	db	'Heads: ',0
$logo2:	db	13,10,'Sectors/track: ',0

;ŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸ
;	Illegal XBIOS call
;œœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœ

BadBIOSfun:
	scf
	sbc	a,a
	ret

;ŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸ
;	Read absolute 1 sector
;œœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœ

HDD_Abs_Read:
	push	hl
	push	bc
	push	de
	push	af
	call	Wait_HDD
	pop	af
	pop	de
	pop	bc
	call	SetAbsoluteParm
	pop	hl
	call	hdd_rdsec
	xor	a
	ret

;ŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸ
;	Write absolute 1 sector
;œœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœ

HDD_Abs_write:
	push	hl
	push	bc
	push	de
	push	af
	call	Wait_HDD
	pop	af
	pop	de
	pop	bc
	call	SetAbsoluteParm
	pop	hl
	call	hdd_wrsec
	xor	a
	ret

;ŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸŸ
;	Set register file absolute
;œœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœœ

SetAbsoluteParm:
	ex	af,af'
	ld	a,80h
	out	(33h),a
	ld	a,0C2h
	out	(32h),a
	ld	a,1		;1 sector to transfer
	out	(30h),a
	ld	a,82h
	out	(32h),a
	ld	a,0C2h
	out	(32h),a
	inc	a
	out	(32h),a
	ex	af,af'
	out	(30h),a		;Number of sector on track
	ld	a,83h
	out	(32h),a
	ld	a,0C3h
	out	(32h),a
	ld	a,0C6h
	out	(32h),a
	ld	a,c
	and	00Fh
	or	0A0h
	out	(30h),a		;head number
	ld	a,86h
	out	(32h),a
	ld	a,0C6h
	out	(32h),a
	ld	a,0C4h
	out	(32h),a
	ld	a,e		;cylinder low
	out	(30h),a
	ld	a,84h
	out	(32h),a
	ld	a,0C4h
	out	(32h),a
	inc	a
	out	(32h),a
	ld	a,d		;cylinder hi
	out	(30h),a
	ld	a,85h
	out	(32h),a
	ld	a,0C5h
	out	(32h),a
	ld	a,0C7h
	out	(32h),a
	ret

;››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››

	ds	7F80h - $

;››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››

	db	'SOLIDSOFT XBIOS '	;locates @ 7F80h
					;must NOT change since is used for
					;HDD ROM search/identification.
	jp	XBIOS_ver		;7F90
	jp	BadBIOSfun		;7F93
	jp	IDE_Info		;7F96
	jp	BadBIOSfun		;7F99
	jp	HDD_seek		;7F9C
	jp	HDD_Abs_Read		;7F9F
	jp	HDD_Abs_write		;7FA2
	jp	0
	jp	0
	jp	0
	jp	0
	jp	0
	jp	0
XBIOS_ver:
	ld	hl,0180h
	xor	a
	ret
;
;›››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››››
;
	END
