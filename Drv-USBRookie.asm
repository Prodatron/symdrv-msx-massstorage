;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@            S y m b O S   -   M S X   D e v i c e   D r i v e r             @
;@                       ROOKIE DRIVE USB (CH376 BASED)                       @
;@                                                                            @
;@                (c) 2016-2017 by Javier Peirats & Prodatron                 @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;todo


;--- MAIN ROUTINES ------------------------------------------------------------
;### DRVOUT -> write sectors (512b)
;### DRVINP -> read sectors (512b)
;### DRVACT -> read and init media (only hardware- and partiondata, no filesystem)

;--- SUB ROUTINES -------------------------------------------------------------
;### DRVSEC -> converts logical sector number into physical sector number
;### DRVSHW -> maps device memory  to #4000-#7FFF and activates SD card access
;### DRVCMD -> Sends command to SD controller
;### DRVINI -> Inits SD card and detects card type
;### DRVRED -> read sectors
;### DRVWRT -> write sectors
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
db 0,1,3                ;Version Minor, Version Major, Type (-1=NUL, 0=FDC, 1=IDE, 2=SD, 3=USB/SCSI)
db "RookieUSB    "      ;comment

drvjmp  dw drvinp,drvout,drvact,drvmof
drvmof  ret:dw 0
        db 32*2+11       ;bit[0-4]=driver ID (11=Rookie USB), bit[5-7]=storage type (3=USB/SCSI)
        ds 4
drvslt  ds 3        ;slot config for switching device rom at #4000

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

;CH376 ports
prt_cmd         equ #21
prt_data        equ #20

;CH376 commands
cmd_check_exist equ #06
cmd_get_status  equ #22
cmd_rd_usb_data equ #27
cmd_wr_usb_data equ #2c
cmd_disk_mount  equ #31
cmd_disk_read   equ #54
cmd_disk_rd_go  equ #55
cmd_disk_write  equ #56
cmd_disk_wr_go  equ #57

;CH376 status
sta_success     equ #14
sta_connect     equ #15
sta_disk_read   equ #1d
sta_disk_write  equ #1e


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
        ld a,cmd_disk_read
        out (prt_cmd),a
        ld a,c
        ld c,prt_data
        out (c),e
        out (c),d
        out (c),a
        out (c),b
        db #dd:ld a,h
        out (c),a

drvinp1 call drvsta                 ;main loop
        cp sta_success
        ret z
        cp sta_disk_read
        scf
        ld a,stoerrsec
        ret nz
        ld a,cmd_rd_usb_data
        out (prt_cmd),a
        in a,(prt_data)
        cp 64
        ld a,stoerrsec+1
        ret nz
        call drvshw
        ld c,prt_data
        call drvrdy
        ini:ini:ini:ini:ini:ini:ini:ini:ini:ini:ini:ini:ini:ini:ini:ini
        ini:ini:ini:ini:ini:ini:ini:ini:ini:ini:ini:ini:ini:ini:ini:ini
        ini:ini:ini:ini:ini:ini:ini:ini:ini:ini:ini:ini:ini:ini:ini:ini
        ini:ini:ini:ini:ini:ini:ini:ini:ini:ini:ini:ini:ini:ini:ini:ini
        ld a,cmd_disk_rd_go
        out (prt_cmd),a
        call bnkmofx
        jp drvinp1

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
        call drvbnk                 ;set banking and correct address
        ld a,cmd_disk_write
        out (prt_cmd),a
        ld a,c
        ld c,prt_data
        out (c),e
        out (c),d
        out (c),a
        out (c),b
        db #dd:ld a,h
        out (c),a

drvout1 call drvsta                 ;main loop
        cp sta_success
        ret z
        cp sta_disk_write
        scf
        ld a,stoerrsec
        ret nz
        ld a,cmd_wr_usb_data
        out (prt_cmd),a
        ld a,64
        out (prt_data),a
        call drvshw
        ld c,prt_data
        call drvrdy
        outi:outi:outi:outi:outi:outi:outi:outi:outi:outi:outi:outi:outi:outi:outi:outi
        outi:outi:outi:outi:outi:outi:outi:outi:outi:outi:outi:outi:outi:outi:outi:outi
        outi:outi:outi:outi:outi:outi:outi:outi:outi:outi:outi:outi:outi:outi:outi:outi
        outi:outi:outi:outi:outi:outi:outi:outi:outi:outi:outi:outi:outi:outi:outi:outi
        ld a,cmd_disk_wr_go
        out (prt_cmd),a
        call bnkmofx
        jp drvout1

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
        push bc
        call drvini             ;check, if hardware and medium is existing
        pop bc
        ret c

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
        ld (hl),stomedusb       ;media type is USB
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
;### Output     HL=corrected address, (drvshw1+1)=block
;### Destroyed  AF
drvbnk  push de
        ld a,(stobnkx)
        di
        call bnkmonx
        pop de
        ld (drvshw1+1),a
        call bnkmofx
        ei
        ret
        
;### DRVSHW -> maps application memory at #8000-#bfff
;### Destroyed  A
drvshw  di
drvshw1 ld a,0          ;show application ram
        out (#fe),a
        ret


;==============================================================================
;### SUB ROUTINES (CH376) #####################################################
;==============================================================================

;### DRVRDY -> waits, until CH376 isn't busy
;### Destroyed  AF
drvrdy  nop
        in a,(prt_cmd)
        and 16
        jr nz,drvrdy
        ret

;### DRVSTA -> returns CH376 status
;### Output     A=status
;### Destroyed  F
drvsta  call drvrdy
        ld a,cmd_get_status
        out (prt_cmd),a
        call drvrdy
        in a,(prt_data)
        ret

;### DRVINI -> checks for CH376 and USB storage device
;### Output     CF=0 -> ok
;###            CF=1 -> A=error code (...)
;### Destroyed  AF,BC
drvini  ld b,0
di
drvini1 in a,(prt_data)         ;empty data buffer to enable next command
        djnz drvini1
if 0
        ld a,cmd_check_exist    ;check, if CH376 is existing
        out (prt_cmd),a
        ld a,#55
        out (prt_data),a
        ld a,(prt_data)
ld a,#d4
ld c,a
rrca:rrca:rrca:rrca
and #f
add "0"
ld (#c000+98),a
ld a,c
and #f
add "0"
ld (#c000+99),a
ld a,c
        cp #aa
        ld a,stoerrmis
        scf
        ret nz
xor a
ret
endif
        ld a,cmd_disk_mount     ;try disk_mount
        out (prt_cmd),a
        call drvsta
ei
        cp sta_success
        ret z
        ld a,stoerrrdy
        ret

drvend

relocate_table
relocate_end
