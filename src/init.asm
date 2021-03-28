SECTION "Init Code", ROM0
Start:
    ldh [hIsGBC], a

Init:
    di
    xor a
    ldh [hCurBank], a
    ldh [rSVBK], a
    ld [MBC5RomBankLo], a
    ld [MBC5RomBankHi], a
    ld [MBC5SRamBank], a
    ld a, SRAM_ENABLE
    ld [MBC5SRamEnable], a
    ld a, [.dummyIRQ]
    ld [VBlankInt], a
    ld [LCDInt], a
    ld [TimerInt], a
.waitvb
    ldh a, [rLY]
    cp LY_VBLANK
    jr c, .waitvb
    xor a ; turn off screen
    ldh [rLCDC], a
    ld sp, $d000

    ldh a, [hIsGBC]
    cp $11
    jr nz, NotGBC

    ld a, 1
    ldh [rKEY1], a
    stop ; switch to double speed

    ei
    jp Main

.dummyIRQ
    reti

NotGBC:
    ; TODO error screen
    jr NotGBC
