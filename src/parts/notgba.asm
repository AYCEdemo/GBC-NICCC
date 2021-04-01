INCLUDE "src/macros.asm"
INCLUDE "src/hw.asm"

SECTION "Not GBA screen - RAM", WRAM0

NotGBA_VBlankFlag:  db
NotGBA_DoTech:      db
NotGBA_FrameCount:  dw
NotGBA_Timer:       dw

NotGBA_WaitTime     EQU 17*38 - 48 ; time (in frames) to wait before next screen

SECTION "Not GBA Screen - Code", ROM0

NotGBA::
    di
    xor a
    ld [NotGBA_FrameCount], a
    ld [NotGBA_FrameCount+1], a
    ld a, low(NotGBA_WaitTime)
    ld [NotGBA_Timer], a
    ld a, high(NotGBA_WaitTime)
    ld [NotGBA_Timer+1], a
    ld a, 1
    ld [NotGBA_DoTech], a
    ld a, BANK(NotGBASine)
    ldh [hCurBank], a
    ld [MBC5RomBankLo], a

    ld hl, NotGBATiles
    ld de, Tileset1
    call DecodeWLE

    ld hl, NotGBAMap
    ld bc, NotGBAMap_End-NotGBAMap
    ld de, Map2
    rst Copy

    ld hl, NotGBAPal
    ld a, $80 ; palette index 0, auto-increment
    ldh [rBGPI], a
    ld c, LOW(rBGPD)
    rept 4 _COLORS
        ld a, [hl+]
        ldh [c], a
    endr

    copycode LCDInt_NotGBA, LCDInt

    xor a
    ldh [rSCX], a
    ld a, 129 ; just right below the graphics
    ldh [rSCY], a
    ld a, 7
    ldh [rWX], a
    ld a, LCDC_ON | LCDC_WINON | LCDC_BG9C00 | LCDC_BG8000
    ldh [rLCDC], a
    ld a, (1 << IF_VBLANK)
    ldh [rIE], a
    ei

NotGBA_ScreenSlideIn:
    halt
    ldh a, [rWY]
    add 6
    ldh [rWY], a
    call UpdateMusic
    ldh a, [rWY]
    cp 144 ; 24 frames
    jr c, NotGBA_ScreenSlideIn

    ld a, LCDC_ON | LCDC_BG9C00 | LCDC_BG8000
    ldh [rLCDC], a
    ld a, (1 << IF_VBLANK) | (1 << IF_LCD_STAT)
    ldh [rIE], a
    xor a
    ldh [rLYC], a
    ld a, STAT_LYC
    ldh [rSTAT], a

NotGBA_MainLoop:
.waitvblank
    ld hl, NotGBA_VBlankFlag
    ld [hl], 1
    xor a
    ldh [rSCX], a
    ldh [rSCY], a

    halt
    ld a, [hl]
    and a
    jr z, .waitvblank

    call UpdateMusic

    ld hl, NotGBA_FrameCount
    ld a, [hl+]
    ld h, [hl]
    ld l, a
    inc hl
    ld a, l
    ld [NotGBA_FrameCount], a
    ld a, h
    cp 2
    jr nz, .nostoptech
    xor a
    ld [NotGBA_DoTech], a
    ldh [rSTAT], a ; disable LYC interrupt
    ld a, h
.nostoptech
    ld [NotGBA_FrameCount+1], a

    ld hl, NotGBA_Timer
    ld a, [hl+]
    ld h, [hl]
    ld l, a
    dec hl
    ld a, h
    or l
    jr z, .next
    ld a, h
    ld [NotGBA_Timer+1], a
    ld a, l
    ld [NotGBA_Timer], a
    jr NotGBA_MainLoop

.next
    ld a, (1 << IF_VBLANK)
    ldh [rIE], a
    ld a, LCDC_ON | LCDC_WINON | LCDC_WIN9C00 | LCDC_BG8000
    ldh [rLCDC], a
    ld a, 7
    ldh [rWX], a
    xor a
    ldh [rWY], a
    ; fall through

NotGBA_ScreenSlide:
    halt
    ld a, (3 _COLORS) | $80
    ldh [rBGPI], a
    xor a ; black
    ldh [rBGPD], a
    ldh [rBGPD], a
    ldh a, [rWY]
    add 6
    ldh [rWY], a
    call UpdateMusic
    ldh a, [rWY]
    cp 144 ; 24 frames
    jr c, NotGBA_ScreenSlide
    ; exit
    ret

LCDInt_NotGBA:
    push af
    ld a, [NotGBA_DoTech]
    and a
    jr z, .notech
.dotech
    ld a, e
    ldh [rSCX], a
    ld a, d
    ldh [rSCY], a
    push bc
    push hl
    ld hl, rLYC
    ld a, [hl]
    cp 144
    jr nc, .reset
    inc [hl]
    ; get frame count
    ld hl, NotGBA_FrameCount
    ld a, [hl+]
    ld b, [hl]
    ld c, a
    ld hl, NotGBASine
    add hl, bc
    ldh a, [rLY]
    ld c, a
    ld b, 0
    add hl, bc
    ld a, [hl]
    ld e, a
    ld d, e
    ldh a, [rLY]
    bit 0, a
    jr nz, .odd
    ld a, e
    neg
    ld e, a
.odd
    ; done
    ld hl, NotGBA_VBlankFlag
    ld [hl], 0
    jr .exit
.reset
    ld [hl], 0
.exit
    pop hl
    pop bc
.notech
    pop af
    reti
.end

SECTION "Not GBA Screen - Graphics data", ROMX
NotGBASine:     INCBIN "data/notgba_sine.bin"

NotGBATiles:    INCBIN "data/gfx/notgba.2bpp.wle"

NotGBAMap:      INCBIN "data/gfx/notgba_map.bin"
NotGBAMap_End:

NotGBAPal:
    color 12, 12, 25
    color  4,  4, 12
    color 31, 31, 31
    color 31, 31, 31
