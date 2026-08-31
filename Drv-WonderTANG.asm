; SPDX-License-Identifier: GPL-3.0-only
; WonderTANG SymbOS MSX mass-storage driver
;
; SymbOS driver ABI and slot/page switching are based on Drv-SDMega.asm
; and Drv-FDCNational.asm from Prodatron/symdrv-msx-massstorage.
; WonderTANG SD register handling is based on the Nextor Tang driver.

; -----------------------------------------------------------------------------
; SymbOS 4 MSX mass-storage ABI entry points

stobnkx        EQU 0815Ah      ; caller buffer banking configuration
bnkmonx        EQU 08112h      ; map caller buffer into page 0
bnkmofx        EQU 08115h      ; restore the SymbOS page-0 mapping
bnkdofx        EQU 08136h      ; restore page 1 and enable interrupts
stoadrx        EQU 08157h      ; HL = current device data record
stobufx        EQU 0815Bh      ; address of the shared 512-byte buffer

; SymbOS storage errors and device record offsets used here
stoerrpno      EQU 4           ; partition does not exist
stoerrptp      EQU 5           ; unsupported partition type
stoerrsec      EQU 6           ; sector read/write error
stoerrrdy      EQU 26          ; device not ready
stoerrxch      EQU 32          ; channel not available

stotypoky      EQU 1
stomedsdc      EQU 17
stodatsta      EQU 0
stodattyp      EQU 1
stodatbeg      EQU 12
stodatsub      EQU 26

; MBR partition table fields
ideparadr      EQU 01BEh
idepartyp      EQU 4
ideparbeg      EQU 8

; WonderTANG SD controller in expanded subslot 0, page 1
SDC_SDATA      EQU 07C00h      ; 512-byte sector transfer RAM
SDC_ENABLE     EQU 07E00h      ; write 1 to expose transfer RAM/registers
SDC_CMD        EQU 07E01h
SDC_STATUS     EQU 07E02h
SDC_SADDR      EQU 07E03h      ; four-byte LBA, least significant byte first
SDC_CTYPE      EQU 07E0Ch

SDC_BUSY       EQU 080h
SDC_CRC        EQU 001h
SDC_TIMEOUT    EQU 002h
SDC_READ       EQU 001h
SDC_WRITE      EQU 002h

; -----------------------------------------------------------------------------
; Driver header

ORG 01000h-32
RELOCATE_START

DB "SMD3"
DW drvend-drvjmp
DW relocate_count
DS 8
DB 0,1,2                       ; version 1.0, SD storage
DB "WonderTANG   "

drvjmp:
        DW drvinp,drvout,drvact,drvmof
drvmof:
        RET
        DW 0
        DB 32*2+12             ; SD storage, WonderTANG driver ID 12
        DS 4
drvslt:
        DS 3                   ; filled by SymbOS: slot state for page 1

idepartok:
        DB 01h,11h,04h,06h,0Eh,14h,16h,1Eh,0Bh,0Ch,1Bh,1Ch
idepartan      EQU 12

buffer_bank:
        DB 0
sector_count:
        DB 0
current_sector:
        DS 4

; -----------------------------------------------------------------------------
; Public driver operations

; Read sectors.
; In: A=device, IY:IX=logical sector, B=count, DE=destination.
drvinp:
        PUSH AF                 ; preserve device number for stoadrx in drvsec
        LD A,B
        LD (sector_count),A
        POP AF
        PUSH DE
        CALL drvsec
        CALL store_sector
        POP HL
        CALL prepare_buffer
        JP drvred

; Write sectors.
; In: A=device, IY:IX=logical sector, B=count, DE=source.
drvout:
        PUSH AF                 ; preserve device number for stoadrx in drvsec
        LD A,B
        LD (sector_count),A
        POP AF
        PUSH DE
        CALL drvsec
        CALL store_sector
        POP HL
        CALL prepare_buffer
        JP drvwrt

; Read and initialize the medium and selected MBR partition.
; In: A=device.  The high nibble of stodatsub is the channel (only 0 exists),
; and the low nibble is the partition number (0=unpartitioned/superfloppy,
; 1-4=MBR primary partition).
drvact:
        PUSH AF
        CALL stoadrx
        POP BC                  ; B=device number
        EX DE,HL                ; DE=device record
        LD HL,stodatsub
        ADD HL,DE
        LD A,(HL)
        AND 0F0h
        JP NZ,drvact_bad_channel

        PUSH HL                 ; selected partition byte
        PUSH DE                 ; device record
        CALL drvini
        JR C,drvact_init_error

        XOR A
        LD (current_sector+0),A
        LD (current_sector+1),A
        LD (current_sector+2),A
        LD (current_sector+3),A
        INC A
        LD (sector_count),A
        LD HL,(stobufx)
        CALL prepare_buffer
        CALL drvred             ; read physical sector zero
        POP DE                  ; device record
        POP HL                  ; selected partition byte
        RET C

        LD A,(HL)
        AND 0Fh
        LD C,A
        LD B,A
        LD IY,0                 ; default partition offset is zero
        JR Z,drvact_ready

        LD IX,(stobufx)
        LD BC,ideparadr
        ADD IX,BC
        LD BC,16
        DEC A
        JR Z,drvact_partition
drvact_partition_loop:
        ADD IX,BC
        DEC A
        JR NZ,drvact_partition_loop

drvact_partition:
        LD A,(IX+idepartyp)
        OR A
        LD A,stoerrpno
        SCF
        RET Z

        LD A,(IX+idepartyp)
        LD HL,idepartok
        LD B,idepartan
drvact_type_loop:
        CP (HL)
        JR Z,drvact_type_ok
        INC HL
        DJNZ drvact_type_loop
        LD A,stoerrptp
        SCF
        RET

drvact_type_ok:
        LD C,(IX+ideparbeg+2)
        LD B,(IX+ideparbeg+3)
        PUSH BC
        POP IY
        LD C,(IX+ideparbeg+0)
        LD B,(IX+ideparbeg+1)

drvact_ready:
        EX DE,HL                ; HL=device record
        LD (HL),stotypoky
        INC HL
        LD (HL),stomedsdc
        LD DE,stodatbeg-stodattyp
        ADD HL,DE
        LD (HL),C
        INC HL
        LD (HL),B
        INC HL
        PUSH IY
        POP BC
        LD (HL),C
        INC HL
        LD (HL),B
        XOR A
        RET

drvact_init_error:
        POP DE
        POP HL
        RET

drvact_bad_channel:
        LD A,stoerrxch
        SCF
        RET

; Add the selected partition start to IY:IX and return the physical LBA in
; BC:DE. WonderTANG always uses 512-byte sector addressing, including SDSC.
drvsec:
        CALL stoadrx
        LD BC,stodatbeg
        ADD HL,BC
        LD C,(HL)
        INC HL
        LD B,(HL)
        INC HL
        ADD IX,BC
        LD C,(HL)
        INC HL
        LD B,(HL)
        JR NC,drvsec_no_carry
        INC BC
drvsec_no_carry:
        ADD IY,BC
        PUSH IX
        POP DE
        PUSH IY
        POP BC
        RET

; -----------------------------------------------------------------------------
; WonderTANG sector I/O

drvred:
        CALL drvshw
drvred_loop:
        CALL program_sector
        LD A,SDC_READ
        LD (SDC_CMD),A
        CALL wait_command
        JR C,drvred_error

        LD A,(buffer_bank)
        OUT (0FCh),A            ; caller's buffer in page 0
        PUSH HL
        LD HL,SDC_SDATA
        POP DE
        LD BC,512
        LDIR
        EX DE,HL                ; HL=next destination
        CALL bnkmofx

        LD A,(sector_count)
        DEC A
        LD (sector_count),A
        JR Z,drvio_success

        CALL drvhide            ; allow normal page state between sectors
        CALL drvshw
        JR drvred_loop

drvred_error:
        PUSH AF
        CALL bnkmofx
        CALL drvhide
        POP AF
        JP status_error

drvwrt:
        CALL drvshw
drvwrt_loop:
        CALL program_sector

        LD A,(buffer_bank)
        OUT (0FCh),A            ; caller's buffer in page 0
        LD DE,SDC_SDATA
        LD BC,512
        LDIR
        CALL bnkmofx

        LD A,SDC_WRITE
        LD (SDC_CMD),A
        CALL wait_command
        JR C,drvwrt_error

        LD A,(sector_count)
        DEC A
        LD (sector_count),A
        JR Z,drvio_success

        CALL drvhide
        CALL drvshw
        JR drvwrt_loop

drvwrt_error:
        PUSH AF
        CALL bnkmofx
        CALL drvhide
        POP AF
        JP status_error

drvio_success:
        CALL drvhide
        XOR A
        RET

; Convert controller status to a SymbOS storage error.
status_error:
        BIT 1,A
        JR NZ,status_not_ready
        LD A,stoerrsec
        SCF
        RET
status_not_ready:
        LD A,stoerrrdy
        SCF
        RET

; Store BC:DE as a little-endian 32-bit sector number.
store_sector:
        LD A,E
        LD (current_sector+0),A
        LD A,D
        LD (current_sector+1),A
        LD A,C
        LD (current_sector+2),A
        LD A,B
        LD (current_sector+3),A
        RET

; Program the current LBA and advance the saved value for the next sector.
; HL (the caller buffer pointer) is preserved.
program_sector:
        PUSH HL
        LD HL,current_sector
        LD A,(HL)
        LD (SDC_SADDR+0),A
        INC HL
        LD A,(HL)
        LD (SDC_SADDR+1),A
        INC HL
        LD A,(HL)
        LD (SDC_SADDR+2),A
        INC HL
        LD A,(HL)
        LD (SDC_SADDR+3),A

        LD HL,current_sector
        INC (HL)
        JR NZ,program_sector_done
        INC HL
        INC (HL)
        JR NZ,program_sector_done
        INC HL
        INC (HL)
        JR NZ,program_sector_done
        INC HL
        INC (HL)
program_sector_done:
        POP HL
        RET

; Ask SymbOS to translate/map the transfer address into page 0, remember its
; mapper segment, then restore page 0 until the actual 512-byte copy.
prepare_buffer:
        LD A,(stobnkx)
        DI
        CALL bnkmonx
        LD (buffer_bank),A
        CALL bnkmofx
        RET

; -----------------------------------------------------------------------------
; Controller initialization and slot/page switching

drvini:
        CALL drvshw
        ; SymbOS is launched by MSX-DOS/Nextor, so the FPGA controller and
        ; card are already initialized. The current FPGA SD state machine
        ; accepts init only from STANDBY and cannot be retriggered after the
        ; original init edge; do not disturb or pretend to reinitialize it.
        LD A,(SDC_CTYPE)
        OR A
        JR NZ,drvini_ok
        LD A,SDC_TIMEOUT
        SCF
        JR drvini_finish
drvini_ok:
        XOR A
drvini_finish:
        PUSH AF
        CALL drvhide
        POP AF
        RET NC
        LD A,stoerrrdy
        SCF
        RET

; Map the WonderTANG expanded slot into page 1, select subslot 0 (the slot
; configuration supplied by SymbOS already identifies it), and enable the SD
; register/transfer-RAM overlay.
drvshw:
        LD A,(drvslt+0)
        DI
        OUT (0A8h),A
        LD A,(drvslt+1)
        LD (0FFFFh),A
        LD A,(drvslt+2)
        OUT (0A8h),A
        LD A,1
        LD (SDC_ENABLE),A
        RET

; Disable the overlay before restoring SymbOS page 1. bnkdofx also restores
; the interrupt state expected by the mass-storage subsystem.
drvhide:
        XOR A
        LD (SDC_ENABLE),A
        JP bnkdofx

; Wait for BUSY to clear. Hardware timeout/CRC bits and the software timeout
; all return with carry set and the controller-style status in A.
wait_command:
        PUSH BC
        PUSH DE
        LD DE,2047
wait_command_outer:
        LD B,255
wait_command_inner:
        LD A,(SDC_STATUS)
        BIT 7,A
        JR Z,wait_command_done
        DJNZ wait_command_inner
        DEC DE
        LD A,D
        OR E
        JR NZ,wait_command_outer
        LD A,SDC_TIMEOUT
        SCF
        JR wait_command_return
wait_command_done:
        AND SDC_TIMEOUT+SDC_CRC
        JR Z,wait_command_return
        SCF
wait_command_return:
        POP DE
        POP BC
        RET

drvend:

RELOCATE_TABLE
RELOCATE_END
