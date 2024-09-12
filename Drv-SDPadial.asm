;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@            S y m b O S   -   M S X   D e v i c e   D r i v e r             @
;@                                 PADIAL SD                                  @
;@                                                                            @
;@                 (c) 2015 by Armando Pérez Abad & Prodatron                 @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


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
db 0,1,2                ;Version Minor, Version Major, Type (-1=NUL, 0=FDC, 1=IDE, 2=SD, 3=SCSI)
db "PadialSD     "      ;comment

drvjmp  dw drvinp,drvout,drvact,drvmof
drvmof  ret:dw 0
        db 32*2+9       ;bit[0-4]=driver ID (9=PadialSD), bit[5-7]=storage type (2=SD)
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


;### DRVINP -> read sectors (512b)
;### Input      A=device (0-7), IY,IX=logical sector number, B=number of sectors, DE=destination address, (stobnkx)=banking config
;### Output     CF=0 -> ok
;###            CF=1 -> A=error code (...)
;### Destroyed  AF,BC,DE,HL,IX,IY
drvinp  push de
        push bc
        call drvsec     ;BC,DE=sector address
        pop ix          ;IXH=number of sectors
        pop hl
scf
ld a,98
ret
drvinp0 push hl
        call mmc_fastreadsector
        pop hl
        push af
        ld de,#c000
        ld a,(hl):inc hl:call clchex
        ld a,(hl):inc hl:call clchex
        ld a,(hl):inc hl:call clchex
        ld a,(hl):inc hl:call clchex
        pop af
        ret

;### DRVOUT -> write sectors (512b)
;### Input      A=device (0-7), IY,IX=logical sector number, B=number of sectors, DE=source address, (stobnkx)=banking config
;### Output     CF=0 -> ok
;###            CF=1 -> A=error code (...)
;### Destroyed  AF,BC,DE,HL,IX,IY
drvout  push de
        push bc
        call drvsec     ;BC,DE=sector address
        pop ix          ;IXH=number of sectors
        pop hl
        jp mmc_fastwritesector

;### DRVACT -> read and init media (only hardware- and partiondata, no filesystem)
;### Input      A=device (0-7)
;### Output     CF=0 -> ok
;###            CF=1 -> A=error code (...)
;### Destroyed  AF,BC,DE,HL,IX,IY
drvact  push af                 ;*** read hardware data
ld a,0
ld b,%101000
call drvini
pop bc
ld a,99
ret c
push bc
ld a,b

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
        ;##!!##

        push hl
        ld a,b
        ld bc,0
        ld de,0
        db #dd:ld h,1
        ld hl,(stobufx)
        call drvinp0            ;read first physical sector (=Bootsector or MBR)
        pop hl
        push af                 ;detect card type
        push hl
ld a,0
ld b,%101000
        call drvini
        pop hl
;        ld a,(drvinic)
;        cp drvct_shc
;        jr nz,drvact6
;        set 4,(hl)              ;set SD-HC bit, if detected
drvact6 pop af
        pop de
        pop hl
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

;### DRVSHW -> maps device memory  to #4000-#7FFF
;### Output     DI
;### Destroyed  A
drvshw  ld a,(drvslt+0)
        di
        out (#a8),a
        ld a,(drvslt+1)
        ld (#ffff),a
        ld a,(drvslt+2)
        out (#a8),a
        ret

;### DRVSEC -> converts logical sector number into physical sector number
;### Input      A=device (0-7), IY,IX=logical sector number
;### Output     BC,DE=physical sector address
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
        and #1
        ;##!!##
        bit 4,(hl)          ;check sector addressing mode
;        jr nz,drvsec2
        ld e,0                  ;MMC/SD1.x/SD2.x
        db #dd:ld d,l
        db #dd:ld c,h
        db #fd:ld b,l
        sla d
        rl c
        rl b
        ret
drvsec2 db #dd:ld e,l           ;SD-HC
        db #dd:ld d,h
        db #fd:ld c,l
        db #fd:ld b,h
        ret


                ; MMC PORTS

MMC_CS          EQU  #7FF0
MMC_DATA        EQU  #7FF1

                ; REGCTRL
                ; BIT 0 -> Not used
                ; BIT 1 -> 0 MMC_A 1 MMC_B
                ; BIT 2 -> CS MMC_A 0 Off 1 On
                ; BIT 3 -> CS MMC_B 0 Off 1 On
                ; BIT 4 -> CLK_A 0 ENABLED 1 DISABLED
                ; BIT 5 -> CLK_B 0 ENABLED 1 DISABLED

CS2_OFF         EQU %01010110
CS2_ON          EQU %01011110
CS2_OFF_R800    EQU %00010110
CS2_ON_R800     EQU %00011110
;CS1_OFF         EQU 01101000b
;CS1_ON          EQU 01101100b
CS1_OFF_R800    EQU %00101000
CS1_ON_R800     EQU %00101100
CS1_OFF         EQU CS1_OFF_R800
CS1_ON          EQU CS1_ON_R800
CS_ON_OR        EQU %00001100
CS_OFF_OR       EQU %11110011

MMC_B           EQU CS2_OFF
MMC_A           EQU CS1_OFF 
MMC_B_R800      EQU CS2_OFF_R800
MMC_A_R800      EQU CS1_OFF_R800


                ; MMC COMMANDS

CMD_GEN         EQU #40
CMD0_DATA       EQU CMD_GEN+0       ; GO_IDLE_STATE
CMD0_SIZE       EQU 6
CMD1_DATA       EQU CMD_GEN+1       ; SEND_OP_COND
CMD1_SIZE       EQU 6
ACMD41_DATA     EQU CMD_GEN+41      ; APP_SEND_OP_COND (Only for MMC)
ACMD41_SIZE     EQU 6
CMD55_DATA      EQU CMD_GEN+55      ; APP_CMD (Only SD and new MMCs)
CMD55_SIZE      EQU 6
CMD2_DATA       EQU CMD_GEN+2       ; ALL_SEND_CSD
CMD2_SIZE       EQU 6
CMD8_DATA       EQU CMD_GEN+8       
CMD8_SIZE       EQU 6
CMD9_DATA       EQU CMD_GEN+9       ; SEND_CSD
CMD9_SIZE       EQU 6
CMD10_DATA      EQU CMD_GEN+10      ; SEND_CID
CMD10_SIZE      EQU 6
CMD12_DATA      EQU CMD_GEN+12      ; STOP_TRANSMISSION
CMD12_SIZE      EQU 6
CMD16_DATA      EQU CMD_GEN+16      ; SET_BLOCKLEN
CMD16_SIZE      EQU 6
CMD17_DATA      EQU CMD_GEN+17      ; READ_SINGLE_BLOCK
CMD24_DATA      EQU CMD_GEN+24      ; WRITE_SINGLE_BLOCK
CMD18_DATA      EQU CMD_GEN+18      ; READ_MULTIPLE_BLOCKs
CMD25_DATA      EQU CMD_GEN+25      ; WRITE_MULTIPLE_BLOCKs


                    ; EXTRA

MMC_REP_INIT        EQU 2048+1024
MMC_PUL_INIT        EQU 80  ; More than 74
MMC_REP_TOKEN       EQU 2048+1024
MMC_REP_COMMANDS    EQU 2048+1024

TYPE_SD             EQU 1
TYPE_MMC            EQU 0



        ld a,(MMC_CS)
        and #11000000
        or 0
        ld (MMC_CS),a


; -----------------
; MMC_INIT
; A -> 0 MMC_A
;   -> 1 MMC_B
; B ->   CS_ON
; Init MMC Card
; And set SPI Mode
; Cy -> 1 Error
; B-> 0 MMC
; B-> 1 SD
; -----------------
drvini
mmc_init
        push af
        call drvshw
        pop af
                push    af
                ld      a,b
                and     %00111111
                ld      b,a
                ld      a,(MMC_CS)
                and     %11000000
                or      b
                ld      (MMC_CS),a
                ld      bc,MMC_PUL_INIT
mmc_init0      
                ld      a,#ff
                ld      (MMC_DATA),a
                ds 9
                dec     bc
                ld      a,b
                or      c
                jp      nz,mmc_init0
                ; Send CMD0 MMC_REP_INIT times
                pop     af
                and     %00111111
                ld      b,a
                ld      a,(MMC_CS)              ; CS_LOW
                and     %11000000
                or      b
                ld      (MMC_CS),a
                ld      hl,CMD0
                ld      b,CMD0_SIZE
                call    mmc_sendcommand
                ld      bc,MMC_REP_INIT 
mmc_init1                      
                ld      a,(MMC_DATA)
                bit     7,a
                jr      nz,mmc_init11           ; Wrong Response...
                bit     6,a
                jr      nz,mmc_init11           ; Parameter error
                bit     5,a
                jr      nz,mmc_init11           ; Adress Error
                bit     4,a
                jr      nz,mmc_init11           ; Erase sequence error?
                bit     3,a
                jr      nz,mmc_init11           ; CRC Command
                bit     2,a
                jr      nz,mmc_init11           ; Illegal command
                ; The good...
                bit     0,a                     ; Idle state?
                jp      nz,mmc_init2            ; Ok! Card in IDLE_STATE
mmc_init11
                dec     bc
                ld      a,b
                or      c
                jp      nz,mmc_init1            ; Try other time ... 
                ; Oops ... not MMC/SD inserted
                ; Or Error 
mmc_init_err   
                ds 3
                ld      a,(MMC_CS)
                and     %11000000
                or      %00111111
                ld      (MMC_CS),a
        call bnkdofx
        ld a,stoerrsec
                scf
                ret
mmc_init2
                ; Check SD or MMC
                ld      bc,MMC_REP_INIT
mmc_init3      
                push    bc
                ld      hl,ACMD41
                ld      b,ACMD41_SIZE
                call    mmc_sendcommandapp      ; Send ACM41 command, for initialize SD, if response is error, is a MMC
                pop     bc
                ld      a,(MMC_DATA)
                or      a
                jp      z,mmc_init_sd           ; Ok is a SD!!!
                dec     bc
                ld      a,b
                or      c
                jp      nz,mmc_init3
                ; Not SD, check MMC
                ; Is a MMC init
mmc_init_mmc   
                ld      bc,MMC_REP_INIT
mmc_init_mmc0
                push    bc
                ld      hl,CMD1
                ld      b,CMD1_SIZE
                call    mmc_sendcommand
                pop     bc
                ld      a,(MMC_DATA)    
                bit     7,a
                jr      nz,mmc_init_mmc00           ; Wrong Response...
                bit     6,a
                jr      nz,mmc_init_mmc00           ; Parameter error
                bit     5,a
                jr      nz,mmc_init_mmc00           ; Adress Error
                bit     4,a
                jr      nz,mmc_init_mmc00           ; Erase sequence error?
                bit     3,a
                jr      nz,mmc_init_mmc00           ; CRC Command
                bit     2,a
                jr      nz,mmc_init_mmc00           ; Illegal command
                bit     0,a
                jp      z,mmc_init_mmc1
mmc_init_mmc00             
                dec     bc
                ld      a,b
                or      c               
                jp      nz,mmc_init_mmc0
                jp      mmc_init_err
mmc_init_mmc1
                ld      b,TYPE_MMC
                jp      mmc_init_set_bl_len
mmc_init_sd    
                ld      b,TYPE_SD
                ; Set BL_LEN 512 bytes (sector)
mmc_init_set_bl_len
                push    bc
                ld      hl,CMD16
                ld      b,CMD16_SIZE
                call    mmc_sendcommand
                ld      bc,MMC_REP_COMMANDS
mmc_init_set_bl_len0               
                ld      a,(MMC_DATA)
                or      a
                jp      z,mmc_init_set_bl_lenend
                dec     bc
                ld      a,b
                or      c
                jp      nz,mmc_init_set_bl_len0
                pop     bc
                jp      mmc_init_err
mmc_init_set_bl_lenend
                pop     bc
mmc_initend    
                ds 3
                ld      a,(MMC_CS)
                and     %11000000
                or      %00111111
                ld      (MMC_CS),a
        call bnkdofx
                xor     a
                ret


; *** FAST ROUTINES MSX *** 
                
; ------------------------------------------
; MMC_FASTREADSECTOR
; MSX Fast Read Sector
; UNIT (A OR B) PREPARED
; IXH-> sectors
; BCDE-> First sector
; HL-> DEST POINTER
; RETURN->
; Cy Error 
; B-> sectors read
; ------------------------------------------                
mmc_fastreadsector
        call drvshw
        push de
        ld a,(stobnkx)
        call bnkmonx
        ld (redlop0+1),a
        pop de
        call bnkmofx
                ld      a,(MMC_CS)
                and     %11000000
                or      CS1_OFF_R800
                ld      (MMC_CS),a
            db #dd:ld a,h
                cp      1
                jr      z,mmc_fastreadsectorone0
                push    hl
                ld      hl,MMC_DATA
                ld      a,(hl)  :ds 4
                ld      a,#ff
                ld      (hl),a
                ld      a,CMD18_DATA
                jr      mmc_fastreadsectoronegen
mmc_fastreadsectorone0
                push    hl
                ld      hl,MMC_DATA
                ld      a,(hl)  :ds 4
                ld      a,#ff
                ld      (hl),a  :ds 2
                ld      a,CMD17_DATA
mmc_fastreadsectoronegen
                         ld (hl),a:ds 4
                ld a,b  :ld (hl),a:ds 4
                ld a,c  :ld (hl),a:ds 4
                ld a,d  :ld (hl),a:ds 4
                ld a,e  :ld (hl),a:ds 4
                ld a,#ff:ld (hl),a:ds 4
                nop     :ld (hl),a:ds 3
            db #dd:ld c,h
                ; Flush
                ld      a,(hl)
                ld      de,MMC_REP_COMMANDS
mmc_fastreadsector2                
                ld      a,(hl)
                cp      #ff
                ccf
                jp      nc,mmc_fastreadsector3      ; Yes!
                dec     de
                ld      a,d
                or      e
                jp      nz,mmc_fastreadsector2
                ; Error /
                pop     de
                ld      b,0             ; 0 Sectors read
                jp      mmc_init_err
                ; Command Accepted
mmc_fastreadsector3
                ; Read command
                ld      de,MMC_REP_COMMANDS
                ds 4
mmc_fastreadsector4
                ld      a,(hl)
                cp      #fe
                jp      z,mmc_fastreadsector8
                dec     de
                ld      a,d
                or      e
                jp      nz,mmc_fastreadsector4
                pop     de
                ; Token not received
                ld      b,0
                jp      mmc_init_err
                ; Token received
mmc_fastreadsector8    
                pop     de
mmc_fastreadsector8_1              
                ld      hl,MMC_DATA
mmc_fastreadsector8loop            

redlop0 ld a,0          ;show destination/source memory at #8000
        out (#fe),a
        dec de
        ld b,0
redlop1 ld a,(hl)
        inc de
        ld (de),a
        inc de
        ld a,(hl)
        ld (de),a
        djnz redlop1
        nop
redlop2 ld a,(hl)
        inc de
        ld (de),a
        inc de
        ld a,(hl)
        ld (de),a
        djnz redlop2

                ; Get CRC
                ld      a,(hl)  :ds 5
                ld      a,(hl)

        call bnkmofx
        call bnkdofx
        call drvshw

                ; Check multisector read
            db #dd:dec h
                jr      z,mmc_fastreadsector9
                ; Check FE and reloop
                ld      hl,MMC_REP_COMMANDS
mmc_fastreadsector8mc              
                ld      a,(MMC_DATA)
                cp      #fe
                jp      z,mmc_fastreadsector8_1
                dec     hl
                ld      a,h
                or      l
                jr      nz,mmc_fastreadsector8mc
                ld      b,0
                jp      mmc_init_err
mmc_fastreadsector9
                ld      b,c
                ld      a,c
                dec     a    
                jr      z,mmc_fastreadsector10
                ; Stop command
                ld      a,(hl)  :ds 4
                ld      a,#fe
                ld      (hl),a  :ds 5
                ld      a,CMD12_DATA            ; End transmision
                ld      (hl),a  :ds 4
                xor     a
                ld      (hl),a  :ds 5
                ld      (hl),a  :ds 5
                ld      (hl),a  :ds 5
                ld      (hl),a  :ds 5
                ld      (hl),#ff:ds 5
                ld      (hl),#ff:ds 3
mmc_fastreadsector10   ; Flush buffer
                ld      a,(hl)
                inc     a       :ds 3
                jp      nz,mmc_fastreadsector10
                ex      de,hl
                jp      mmc_initend


; ------------------------------------------
; MMC_FASTWRITESECTOR
; MSX Fast Write Sector (a cascoporro)
; UNIT (A OR B) PREPARED
; B-> sectors
; CDE-> First sector, 24 bits, 4096 M, 4 gigas max
; HL-> ORG POINTER
; RETURN->
; Cy Error 
; B-> sectors read
; ------------------------------------------                
mmc_fastwritesector
        call drvshw
        push de
        ld a,(stobnkx)
        call bnkmonx
        ld (wrtlop0+1),a
        pop de
        call bnkmofx
                ld      a,(MMC_CS)
                and     %11000000
                or      CS1_OFF_R800
                ld      (MMC_CS),a
                push    hl
                ld      hl,MMC_DATA
                ld      a,(hl)
                ld      a,#ff
                ld      (hl),a
            db #dd:ld a,h
                cp      1
                jr      z,mmc_fastwritesectorone0
            ld a,#fc
            ld (mmc_fastwrite_x+1),a
                ld      a,CMD25_DATA
                jr      mmc_fastwritesectorgen
mmc_fastwritesectorone0                
            ld a,#fe
            ld (mmc_fastwrite_x+1),a
                ld      a,CMD24_DATA
            db #dd:ld h,1
mmc_fastwritesectorgen
                ld      (hl),a
                ld      a,b
                ld      (hl),a
                ld      a,c
                ld      (hl),a
                ld      a,d
                ld      (hl),a
                ld      a,e
                ld      (hl),a
                ld      a,#ff              
                ld      (hl),a:ds 1
                ld      (hl),a:ds 1
                ; Flush
                ld      a,(hl)
                ld      de,MMC_REP_COMMANDS
mmc_fastwritesector2               
                ; Command acepted ? 
                ld      a,(hl)
                cp      #ff
                ccf
                jp      nc,mmc_fastwritesector3     ; Yes!
                dec     de
                ld      a,d
                or      e
                jp      nz,mmc_fastwritesector2
                ; Error /
                pop     de
                ld      b,0             ; 0 Sectors write
                jp      mmc_init_err
                ; Command Accepted
mmc_fastwritesector3
                pop     de
                ; Send  FE or FC
mmc_fastwrite_x ld      (hl),0
                ld      c,b
mmc_fastwritesector30
                ; Send fast data

wrtlop0 ld a,0          ;show destination/source memory at #8000
        out (#fe),a
        ld b,0
wrtlop1 ld a,(de)
        ld (hl),a
        inc de
        ld a,(de)
        inc de
        ld (hl),a
        djnz wrtlop1
        nop
wrtlop2 ld a,(de)
        ld (hl),a
        inc de
        ld a,(de)
        inc de
        ld (hl),a
        djnz wrtlop2

                ld      (hl),#ff:ds 1
                ld      (hl),#ff:ds 3
                ; And Check
                ld      hl,MMC_REP_COMMANDS
mmc_fastwritesector7
                ld      a,(MMC_DATA)
                and     #1F     ;0111b
                cp      %0101
                jp      z,mmc_fastwritesector8
                dec     hl
                ld      a,h
                or      l
                jp      nz,mmc_fastwritesector7
                ex      de,hl
                ld      b,0                                 ; Error Writing 
                jp      mmc_init_err                
                ; Flush bytes
mmc_fastwritesector8
                ld      hl,MMC_DATA
                ld      a,(hl)
                inc     a
                jr      nz,mmc_fastwritesector8
            db #dd:dec h
                jr      z,mmc_fastwritesector9
                ld      (hl),#fc
                jp      mmc_fastwritesector30
mmc_fastwritesector9
                ex      de,hl               
            db #dd:ld h,c
                ld      a,c
                dec     a
                jp      z,mmc_initend
                ex      de,hl
                ld      (hl),#fd:ds 1
                ; Stop Command
                ld      a,#ff
                ld      (hl),a
                ld      a,CMD12_DATA            ; End transmision
                ld      (hl),a
                xor     a
                ld      (hl),a  :ds 1
                ld      (hl),a  :ds 1
                ld      (hl),a  :ds 1
                ld      (hl),a  :ds 1
                ld      (hl),#ff:ds 1
                ld      (hl),#ff:ds 1
                ld      a,(hl)
                jp      mmc_fastreadsector10

; *** LOW LEVEL ***                 
mmc_flush
                ld      a,(MMC_DATA)
                cp      #ff
                ret     z
                ds 3
                jr      mmc_flush
                
; ---------------------------
; MMC_SENDCOMMAND
; Send Complete Command
; HL Pointer to command DATA
; B -> Long 
; CS should be 0
; ---------------------------               
                
mmc_sendcommand
                ; Init Command
                ds 9
                ld      a,#ff
                ld      (MMC_DATA),a
                ds 5
mmc_sendcommand0               
                ld      a,(hl)              ; 3
                inc     hl                  ; 1
                ld      (MMC_DATA),a        ; 4
                ds 3
                djnz    mmc_sendcommand0    ; 2
                ; End Command
                ld      a,#ff
                ld      (MMC_DATA),a
                ds 9
                ;ld     a,a
                                                
                ; Flush
                ld      a,(MMC_DATA)
                or      a
                ds 9
                ;ld     a,a
                ret


; -----------------------------------
; MMC_SENDCOMMANDAPP
; HL -> Pointer to Command
; B-> Long
; CS -> 0 
; -------------------------------------             
mmc_sendcommandapp
                push    hl
                push    bc
                ld      hl,CMD55
                ld      b,6
                call    mmc_sendcommand
                ld      a,(MMC_DATA)
                ds 4
                pop     bc
                pop     hl
                jp      mmc_sendcommand


; *** MMC COMMANDS *** 

; GO_IDLE_STATE

CMD0            db      CMD0_DATA,  0,0,0,0,#95
CMD1            db      CMD1_DATA,  0,0,0,0,#ff
CMD55           db      CMD55_DATA, 0,0,0,0,#ff     ; Only SD and new MMCs
ACMD41          db      ACMD41_DATA,0,0,0,0,#ff     ; Only SD

CMD2            db      CMD2_DATA,  0,0,0,0,#ff
CMD9            db      CMD9_DATA,  0,0,0,0,#ff
CMD10           db      CMD10_DATA, 0,0,0,0,#ff
CMD12           db      CMD12_DATA, 0,0,0,0,#ff
CMD16           db      CMD16_DATA, 0,0,2,0,#ff     ; Set Block Leng 512 bytes
CMD8            db      CMD8_DATA,  0,0,0,1,#AA


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
