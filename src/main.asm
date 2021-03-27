INCLUDE "src/macros.asm"
INCLUDE "src/hw.asm"

INCLUDE "src/header.asm"
INCLUDE "src/init.asm"

SECTION "Utility Code", ROM0
LCDOff::
    ; safely turn off screen
    ld hl, rLCDC
    bit 7, [hl]
    ; no need to do anything if it's already off
    ret z
.waitvb
    ldh a, [rLY]
    cp LY_VBLANK
    jr c, .waitvb
    res 7, [hl]
    ret

SECTION "Main Code", ROM0
Main:
    ; prepare system
    ld a, $80 ; turn LCD on
    ldh [rLCDC], a
    call HHDMA_Install

    ; prepare music
    call SoundSystem_Init
    ld bc, BANK(Inst_chcknbnk)
    ld de, Inst_chcknbnk
    call Music_PrepareInst
    ld bc, BANK(Music_chcknbnk)
    ld de, Music_chcknbnk+4*1 ; TEMP
    call Music_Play

    ; TEMP skip the music to row 16
    ld a, 136
.TEMP
    push af
    call SoundSystem_Process
    pop af
    dec a
    jr nz, .TEMP

    ; demo parts
    call DotPlotter_Precalc
    call DotPlotter
    call PolyStream
    jp Credits

DecodeWLE::
    ; Walle Length Encoding decoder
    ld c, 0
DecodeWLELoop:
    ld a, [hl+]
    ld b, a
    and $c0
    jr z, .literal
    ; We now know that at least 1 bit is set, so check which one is reset, if any
    add a, a ; Bit 7 goes into carry, and only bit 6 remains, so it's essentially copied to Z
    jr nc, .repeat ; B == $40, so carry was clear
    jr z, .increment ; B == $80, so bit 6 was clear
    ; B == $C0

.copy
    ld a, b
    inc b
    ret z

    and $3f
    inc a
    ld b, a
    ld a, [hl+]
    push hl
    ld l, a
    ld a, e
    scf
    sbc l
    ld l, a
    ld a, d
    sbc 0
    ld h, a
    call .copyCommon
    pop hl
    jr DecodeWLELoop

.literal
    ld a, b
    and $1f
    bit 5, b
    ld b, a
    jr nz, .longl
    inc b
    call .copyCommon
    jr DecodeWLELoop

.longl
    push bc
    ld a, [hl+]
    ld c, a
    inc bc
    rst Copy
    pop bc
    jr DecodeWLELoop

.repeat
    call .repeatIncrementCommon
.loopr
    ld [de], a
    inc de
    dec b
    jr nz, .loopr
    jr DecodeWLELoop

.increment
    call .repeatIncrementCommon
.loopi
    ld [de], a
    inc de
    inc a
    dec b
    jr nz, .loopi
    ld c, a
    jr DecodeWLELoop

.repeatIncrementCommon
    bit 5, b
    jr z, .nonewr
    ld c, [hl]
    inc hl
.nonewr
    ld a, b
    and $1f
    inc a
    ld b, a
    ld a, c
    ret

.copyCommon
    ld a, [hl+]
    ld [de], a
    inc de
    dec b
    jr nz, .copyCommon
    ret

BlackPalette::
    ld a, $80
    ldh [rBGPI], a
    ldh [rOBPI], a
    ld c, LOW(rBGPD) ; a bit speedup
    ldh a, [rLCDC]
    add a ; bit 7 -> carry
    ld a, 0
    jr nc, .lcdoff

    ld b, 4
    ld hl, rSTAT
.lcdonLoop
    waitmode0
    rept 16
    ldh [c], a ; 8
    ldh [rOBPD], a ; 12
    endr ; = 20 * 16 = 320, sprites safe
    dec b ; 4
    jr nz, .lcdonLoop ; 12
        ; = 336
    ret

.lcdoff
    ; regular fill, no need to wait for mode 0
    ld b, 64
.lcdoffLoop
    ldh [c], a
    ldh [rOBPD], a
    dec b
    jr nz, .lcdoffLoop
    ret

UpdateMusic::
    call SoundSystem_Process
    ; SoundSystem changes ROM bank while updating music so let's restore that back
    ldh a, [hCurBank]
    ld [MBC5RomBankLo], a
    ret

INCLUDE "src/hyperhdma.asm"

SECTION "Common Data", ROM0, ALIGN[8]
OnePixelTable::
    rept 256/8
        db %10000000, %01000000, %00100000, %00010000, %00001000, %00000100, %00000010, %00000001
    endr
