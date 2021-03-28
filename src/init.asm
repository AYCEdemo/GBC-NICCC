SECTION "Init Code", ROM0
Start:
    ldh [hIsGBC], a

Init:
    di
    xor a
    ldh [rIE], a
    ldh [rIF], a
    ldh [hCurBank], a
    ld [MBC5RomBankLo], a
    ld [MBC5RomBankHi], a
    ld [MBC5SRamBank], a
    ld a, SRAM_ENABLE
    ld [MBC5SRamEnable], a
    ld a, [.dummyIRQ]
    ld [VBlankInt], a
    ld [LCDInt], a
    ld [TimerInt], a
    ld a, [rLCDC]
    add a ; bit 7 -> carry
    jr nc, .nowait
.waitvb
    ldh a, [rLY]
    cp LY_VBLANK
    jr c, .waitvb
.nowait
    xor a ; turn off screen
    ldh [rLCDC], a
    ld sp, $d000

    ldh a, [hIsGBC]
    cp $11
    jr nz, NotGBC
    ; WRAM banks test just in case the above test fails
    ld hl, $d000
    ld [hl], $a1
    ld a, 2
    ldh [rSVBK], a
    ld [hl], $ce
    dec a
    ldh [rSVBK], a
    ld a, [hl]
    cp $a1
    jr nz, NotGBC

    ld a, 1
    ldh [rKEY1], a
    stop ; switch to double speed

    ei
    jp Main

.dummyIRQ
    reti

NotGBC:
    xor a
    ld hl, Map1
    ld bc, 32*19
    rst Fill
    coord hl, 1, 9, Map1
    ld b, 19
.writetiles
    inc a
    ld [hl+], a
    dec b
    jr nz, .writetiles
    ld a, BANK(NotGBCTiles)
    ld [MBC5RomBankLo], a
    ld hl, NotGBCTiles
    ld de, Tileset1
    ld bc, NotGBCTiles.end - NotGBCTiles
    rst Copy
    ld a, %11111100
    ldh [rBGP], a
    ld a, 4
    ldh [rSCX], a
    ldh [rSCY], a
    ld a, LCDC_ON | LCDC_BG8000 | LCDC_BGPRIO
    ldh [rLCDC], a
.loop
    halt
    jr .loop

SECTION "Not GBC Graphics", ROMX
NotGBCTiles:    INCBIN "data/gfx/notgbc.2bpp"
.end
