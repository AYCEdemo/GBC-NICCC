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
    call LCDOff

    ; prepare music
    call SoundSystem_Init
    ld bc, BANK(Inst_chcknbnk)
    ld de, Inst_chcknbnk
    call Music_PrepareInst
    ld bc, BANK(Music_chcknbnk)
    ld de, Music_chcknbnk
    call Music_Play
    call SoundSystem_Process

    ; demo parts
    ; call DotPlotter_Precalc
    ; call NotGBA
    ; call DotPlotter
    ; call PolyStream
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

; fade stuff
; hl = packed colors source
; de = packed colors destination
; b = steps per update
; c = num colors

SetFadeToWhite::
    call SetFadeCommon
    ld a, 31
    ld bc, wFadeTargets
    ld de, wFadeCurrents
    jp SetFadeCommon2

SetFadeFromWhite::
    call SetFadeCommon
    ld a, 31
    ld bc, wFadeCurrents
    ld de, wFadeTargets
    jp SetFadeCommon2

SetFadeCommon:
    ld a, e
    ld [wFadeDestPtr], a
    ld a, d
    ld [wFadeDestPtr+1], a
    ld a, c
    ld [wFadeCount], a
    ld a, b
    ld [wFadeStep], a
    ret

SetFadeCommon2:
    push af
    push bc
    ld b, %00011111
    ld a, [wFadeCount]
.unpack
    push af
    ld a, [hl+]     ; gggrrrrr
    ld c, a
    and b           ; 000rrrrr
    ld [de], a
    inc de
    ld a, [hl]      ; xbbbbbgg
    rept 3
        sla c
        rla
    endr            ; bbbggggg
    and b           ; 000ggggg
    ld [de], a
    inc de
    ld a, [hl+]     ; xbbbbbgg
    rra             ; 0xbbbbbg
    rra             ; g0xbbbbb
    and b           ; 000bbbbb
    ld [de], a
    inc de
    pop af
    dec a
    jr nz, .unpack

    pop hl
    ld b, 0
    ld a, [wFadeCount]
    ld c, a
    add a ; x2
    add c ; x3
    ld c, a
    ld b, 0
    pop af
    jp Fill

ProcFade::
    ld hl, wFadeCurrents
    ld de, wFadeTargets
    ld a, [wFadeStep]
    ld b, a
    ld a, [wFadeCount]
    ld c, a
    add a ; x2
    add c ; x3
.loop
    push af
    ld a, [de]
    inc de
    ld c, a
    ld a, [hl]
    cp c
    jr z, .skip
    jr nc, .sub
.add
    add b
    cp c
    jr c, .skip
    jr .clamp
.sub
    sub b
    jr c, .clamp
    cp c
    jr nc, .skip
.clamp
    ld a, c
.skip
    ld [hl+], a
    pop af
    dec a
    jr nz, .loop
    ld a, [wFadeDestPtr]
    ld e, a
    ld a, [wFadeDestPtr+1]
    ld d, a
    ld a, [wFadeCount]
    ; fall through

PackFadeCurrents::
    ld hl, wFadeCurrents
.loop
    push af
    ld a, [hl+] ; 000rrrrr
    ld c, a
    ld a, [hl+] ; 000ggggg
    swap a      ; gggg000g
    rrca        ; ggggg000
    ld b, [hl]  ; 000bbbbb
    inc hl
    add a       ; gggg0000
    rl b        ; 00bbbbbg
    add a       ; ggg00000
    rl b        ; 0bbbbbgg
    or c        ; gggrrrrr
    ld [de], a
    inc de
    ld a, b
    ld [de], a
    inc de

    pop af
    dec a
    jr nz, .loop
    ret

INCLUDE "src/hyperhdma.asm"

SECTION "Common Data", ROM0, ALIGN[8]
SineTable::
_x = 0.5
    rept 256
        db MUL(SIN(_x), 128.0) >> 16
_x = _x + 256.0
    endr

OnePixelTable::
    rept 256/8
        db %10000000, %01000000, %00100000, %00010000, %00001000, %00000100, %00000010, %00000001
    endr
