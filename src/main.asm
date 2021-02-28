INCLUDE "src/macros.asm"
INCLUDE "src/hw.asm"

INCLUDE "src/header.asm"
INCLUDE "src/init.asm"

SECTION "Utility Code", ROM0[$61]
_Copy:
    dec bc
    inc b
    inc c
.loop
    ld a, [hl+]
    ld [de], a
    inc de
    dec c
    jr nz, .loop
    dec b
    jr nz, .loop
    ret

_Fill:
    dec bc
    inc b
    inc c
.loop
    ld [hl+], a
    dec c
    jr nz, .loop
    dec b
    jr nz, .loop
    ret

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

INCLUDE "src/hyperhdma.asm"
