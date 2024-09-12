;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@            S y m b O S   -   M S X   D e v i c e   D r i v e r             @
;@                                   GR8NET                                   @
;@                                                                            @
;@             (c) 2015-2015 by Prodatron / SymbiosiS (Jörn Mika)             @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;todo


;--- MAIN ROUTINES ------------------------------------------------------------
;### DRVOUT -> write sectors (512b)
;### DRVINP -> read sectors (512b)
;### DRVACT -> read and init media (only hardware- and partiondata, no filesystem)

;--- SUB ROUTINES -------------------------------------------------------------
;### DRVSEC -> converts logical sector number into physical sector number
;### DRVBNK -> sets bank switching and recalculates source/destination address
;### DRVSHW -> maps device memory to #4000-#7FFF, activates SD card access and show application memory at #8000-#bfff
;### DRVSTA -> checks SD card status and tries to re-init it once, if there is an issue
;### DRVRDY -> wait until command has been proceeded
;### DRVCOP -> Copies 512 bytes


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
db "Gr8net       "      ;comment

drvjmp  dw drvinp,drvout,drvact,drvmof
drvmof  ret:dw 0
        db 32*2+10      ;bit[0-4]=driver ID (10=gr8net), bit[5-7]=storage type (2=SD)
        ds 4
drvslt  ds 3        ;slot config for switching device rom at #4000

drvct_mmc   equ 0
drvct_sd1   equ 1
drvct_sd2   equ 2
drvct_shc   equ 3

stobnkx equ #202    ;memory mapping, when low level routines read/write sector data
bnkmonx equ #203    ;set special memory mapping during mass storage access (show destination/source memory block at #8000), HL=address; will be corrected from #0000-#ffff to #8000-#bfff
bnkmofx equ #206    ;reset special memory mapping during mass storage access (show OS memory at #8000)
bnkdofx equ #209    ;hide mass storage device rom (switch back to memory mapper slot config at #4000)
stoadrx equ #20c    ;get device data record
stobufx equ #212    ;address of 512byte buffer

ideparadr   equ #1be        ;start of partition table inside the master boot record (MBR)
idepartyp   equ #04         ;00=not used, 01/11=FAT12, 04/06/0E/14/16/1E=FAT16, 0B/0C/1B/1C=FAT32, 05/0F=extended
ideparbeg   equ #08         ;first logical sector of the partition

idepartok   db #01,#11, #04,#06,#0e,#14,#16,#1e, #0b,#0c,#1b,#1c
idepartan   equ 12

g8n_sel_port    equ #5e     ;bit0-3=register, bit4-5=default adapter, bit6-7=selected adapter
g8n_reg_port    equ #5f     ;reg0="G", reg1=adapter slot (RDSLT format), reg2=mapper, reg3=majver, reg4=minver

g8n_sd_status   equ #7fc0   ;status
g8n_sd_bufpos   equ #7fc1   ;buffer position
g8n_sd_secadr   equ #7fc2   ;sector address
g8n_sd_secnum   equ #7fc6   ;number of sectors
g8n_sd_cmd      equ #7fc7   ;command

g8n_sd_cmdred   equ #e1     ;read command
g8n_sd_cmdwrt   equ #e9     ;write command

g8n_sd_dlyrdy   equ -1      ;wait until card is ready ##!!##
g8n_sd_dlycmd   equ -1      ;wait until command is finished ##!!##

;==============================================================================
;### MAIN ROUTINES ############################################################
;==============================================================================

;### DRVINP -> read sectors (512b)
;### Input      A=device (0-7), IY,IX=logical sector number, B=number of sectors, DE=destination address, (stobnkx)=banking config
;### Output     CF=0 -> ok
;###            CF=1 -> A=error code (...)
;### Destroyed  AF,BC,DE,HL,IX,IY
drvinp  push de
        push bc
        call drvsec                 ;BC,DE=sector address
        pop ix                      ;IXH=number of sectors
        pop hl                      ;HL=address
drvinp0 call drvbnk                 ;set banking and correct address
        call drvshw
        call drvsta                 ;check SD card status
        jr c,drvinp6
        ld (g8n_sd_secadr+0),de
        ld (g8n_sd_secadr+2),bc     ;set start sector
        ex de,hl                    ;de=destination address
drvinp1 xor a                       ;** pre-load first sector
        ld (g8n_sd_bufpos),a        ;reset buffer position
        ld a,#91
        ld (g8n_sd_secnum),a        ;1 sector
        ld a,g8n_sd_cmdred
        ld (g8n_sd_cmd),a           ;start loading
        call drvrdy                 ;wait until loading finished
        jr c,drvinp6
        db #dd:ld a,h               ;check, if more than 1 sector is required
        dec a
        jr z,drvinp3
        cp 7                        ;** pre-load up to 7 more sectors
        jr c,drvinp2
        ld a,7
drvinp2 db #dd:ld l,a
        add #90
        ld (g8n_sd_secnum),a
        ld a,g8n_sd_cmdred
        ld (g8n_sd_cmd),a           ;start loading
drvinp3 ld hl,#4000                 ;copy first sector (while gr8net may load more ones)
        call drvcop
        db #dd:dec h
        jr z,drvinp5
        call drvrdy                 ;wait until loading 1-7 sectors finished
        jr c,drvinp6
drvinp4 call bnkmofx                ;allow IRQs inbetween
        call bnkdofx
        call drvshw
        call drvcop                 ;copy remaining sectors
        db #dd:dec h
        db #dd:dec l
        jr nz,drvinp4               ;proceed all additional pre-loaded sectors
        db #dd:inc h
        db #dd:dec h
        jr nz,drvinp1               ;sectors left -> proceed the remaining ones
drvinp5 xor a
drvinp6 push af                     ;** exit
        call bnkmofx
        call bnkdofx
        pop af
        ret

;### DRVOUT -> write sectors (512b)
;### Input      A=device (0-7), IY,IX=logical sector number, B=number of sectors, DE=source address, (stobnkx)=banking config
;### Output     CF=0 -> ok
;###            CF=1 -> A=error code (...)
;### Destroyed  AF,BC,DE,HL,IX,IY
drvout  push de
        push bc
        call drvsec                 ;BC,DE=sector address
        pop ix                      ;IXH=number of sectors
        pop hl                      ;HL=address
drvout0 call drvbnk                 ;set banking and correct address
        call drvshw
        call drvsta                 ;check SD card status
        jr c,drvout6
        ld (g8n_sd_secadr+0),de
        ld (g8n_sd_secadr+2),bc     ;set start sector
drvout1 ld de,#4000
        xor a                       ;** write ...
        ld (g8n_sd_bufpos),a        ;reset buffer position
        db #dd:ld a,h
        dec a
        jr z,drvout4
        cp 7                        ;** pre-write up to 7 sectors
        jr c,drvout2
        ld a,7
drvout2 db #dd:ld l,a
        push af
drvout3 call drvcop                 ;copy sectors
        call bnkmofx                ;allow IRQs inbetween
        call bnkdofx
        call drvshw
        db #dd:dec h
        db #dd:dec l
        jr nz,drvout3               ;proceed all additional pre-written sectors
        pop af
        add #90
        ld (g8n_sd_secnum),a        ;x sectors
        ld a,g8n_sd_cmdwrt
        ld (g8n_sd_cmd),a           ;start writing
        call drvcop                 ;copy last sector
        call bnkmofx                ;allow IRQs inbetween
        call bnkdofx
        call drvshw
        call drvrdy                 ;wait until first 1-7 sectors have been written
        jr c,drvout6
        jr drvout5
drvout4 call drvcop
drvout5 ld a,#91
        ld (g8n_sd_secnum),a        ;1 sector
        ld a,g8n_sd_cmdwrt
        ld (g8n_sd_cmd),a           ;start writing
        call drvrdy                 ;wait until sector has been written
        jr c,drvout6
        db #dd:dec h
        jr nz,drvout1               ;sectors left -> proceed the remaining ones
        xor a
drvout6 push af                     ;** exit
        call bnkmofx
        call bnkdofx
        pop af
        ret

;### DRVACT -> read and init media (only hardware- and partiondata, no filesystem)
;### Input      A=device (0-7)
;### Output     CF=0 -> ok
;###            CF=1 -> A=error code (...)
;### Destroyed  AF,BC,DE,HL,IX,IY
drvact  push af                 ;*** read hardware data
        call stoadrx
        pop bc
        ex de,hl
        ld hl,stodatsub
        add hl,de
        ld a,(hl)               ;get channel (0-15)
        and #f0
        ld c,a
        cp 16*4
        ccf
        ld a,stoerrxch          ;>4 -> wrong channel
        ret c
        ld a,c
        rlca:rlca
        ld c,a
        in a,(g8n_sel_port)
        and #30
        or c
        out (g8n_sel_port),a
        add 2
        ld (drvshw1+1),a
        ld c,a
        in a,(g8n_reg_port)
        cp "G"                  ;check, if GR8NET existing
        scf
        ld a,stoerrxch
        ret nz

        ld a,c
        out (g8n_sel_port),a
        in a,(g8n_reg_port)
        and #0F                 ;keep mapper type
        or #20
        ld (drvshw0+1),a

        push de
        push hl
        ld a,b
        ld bc,0
        ld de,0
        db #dd:ld h,1
        ld hl,(stobufx)
        call drvinp0            ;read sector 0 (MBR or boot sector)
        pop hl
        pop de
        ret c

        ld a,(hl)
        and #f                  ;A=partition number
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


;==============================================================================
;### SUB ROUTINES #############################################################
;==============================================================================

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
        db #dd:ld e,l
        db #dd:ld d,h
        db #fd:ld c,l
        db #fd:ld b,h
        ret

;### DRVBNK -> sets bank switching and recalculates source/destination address
;### Input      HL=address, (stobnkx)=banking
;### Output     HL=corrected address, (drvshw2+1)=block
;### Destroyed  AF
drvbnk  push de
        ld a,(stobnkx)
        call bnkmonx
        pop de
        ld (drvshw2+1),a
        jp bnkmofx
        
;### DRVSHW -> maps device memory to #4000-#7FFF, activates SD card access and show application memory at #8000-#bfff
;### Destroyed  A
drvshw  ld a,(drvslt+0) ;slot selection
        di
        out (#a8),a
        ld a,(drvslt+1)
        ld (#ffff),a
        ld a,(drvslt+2)
        out (#a8),a
drvshw1 ld a,0
        out (g8n_sel_port),a
drvshw0 ld a,32+0       ;set mapper type X and make special control register visible in gr8net bank 1 (#6000-#7fff)
        out (g8n_reg_port),a
        ld a,#c8        ;show SD card buffer at gr8net bank0 (#4000-#5fff)
        ld (#7fe0),a
drvshw2 ld a,0          ;show application ram
        out (#fe),a
        ret

;### DRVSTA -> checks SD card status and tries to re-init it once, if there is an issue
;### Output     CF=0 -> ok
;###            CF=1 -> A=error code (...)
;### Destroyed  A
drvsta  ld a,(g8n_sd_status)
        bit 7,a
        jr z,drvsta4            ;no SD card present
        bit 6,a
        jr nz,drvsta2           ;still in init status
        bit 1,a
        jr nz,drvsta1           ;error -> try one more init
        or a
        bit 0,a                 ;ready -> ok
        ret nz
drvsta1 ld a,64                 ;do a new init
        ld (g8n_sd_status),a
drvsta2 push bc
        ld bc,g8n_sd_dlyrdy
drvsta5 ld a,(g8n_sd_status)
        bit 6,a
        jr z,drvsta6            ;wait until init is done
        dec bc
        ld a,c
        or b
        jr nz,drvsta5
        jr drvrdy0
drvsta6 pop bc
        bit 1,a
        jr nz,drvsta3           ;error -> give up
        or a
        bit 0,a
        ret nz                  ;ready -> ok
drvsta3 ld a,stoerrunk
        scf
        ret
drvsta4 ld a,stoerrrdy
        scf
        ret

;### DRVRDY -> wait until command has been proceeded
;### Output     CF=0 -> ok
;###            CF=1 -> A=error code (...)
;### Destroyed  A
drvrdy  push bc
        ld bc,g8n_sd_dlycmd
drvrdy1 ld a,(g8n_sd_cmd)
        bit 0,a
        jr z,drvrdy2
        dec bc
        ld a,c
        or b
        jr nz,drvrdy1
drvrdy0 pop bc
        ld a,stoerrabo
        scf
        ret
drvrdy2 pop bc
        and #06
        ret z
        call drvsta1
        ld a,stoerrsec
        scf
        ret

;### DRVCOP -> Copies 512 bytes
;### Input      HL=source, DE=destination
;### Destroyed  F,BC,DE,HL
drvcop  ld bc,512
drvcop1 ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi
        ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi
        ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi
        ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi:ldi
        jp pe,drvcop1
        ret

drvend

relocate_table
relocate_end
