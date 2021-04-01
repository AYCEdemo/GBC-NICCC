INCLUDE "src/macros.asm"
INCLUDE "src/hw.asm"

SECTION "Credits - Code", ROM0

Credits_TileDataDst EQU $8000
Credits_TileMapDst  EQU $9800
Y_OFFSET            EQU (128-100) / 2

Credits::
    call SoundSystem_Init
    ld bc, BANK(Inst_credits)
    ld de, Inst_credits
    call Music_PrepareInst
    ld bc, BANK(Music_credits)
    ld de, Music_credits
    call Music_Play

    xor a
    ld [wLoadPal], a
    ld [wOddFrame], a
    ld [CreditsTimer], a
    ld [CreditsTimerLast], a
    ld hl, sRenderBuf
    ld bc, 16*256
    rst Fill

    di
    copycode Credits_VBlankUpdate, VBlankInt
    call LCDOff

    xor a
    ldh [rVBK], a
    ld a, $80
    ldh [rBGPI], a
    ld a, $ff
    ld hl, wPalTab
    ld c, LOW(rBGPD)
    rept 4 _COLORS
        ldh [c], a
    endr

    ld a, BANK(Credits_Font)
    ldh [hCurBank], a
    ld [MBC5RomBankLo], a
    ld hl, Credits_Font
    ld de, wStrokeTab ; reuse
    call DecodeWLE
    ld hl, Credits_TileMapDst
    ld bc, 32*18
    rst Fill
    coord hl, 2, 1, Credits_TileMapDst
    xor a
    ld de, 32
    ld b, 16
.settiles
    ld c, 16
    push hl
.settiles2
    ld [hl+], a
    add 16
    dec c
    jr nz, .settiles2
    pop hl
    add hl, de
    sub 16 * 16 - 1
    dec b
    jr nz, .settiles

    ld a, 1
    ldh [rVBK], a
    xor a
    ld hl, Credits_TileDataDst
    ld bc, $1000
    rst Fill
    ld hl, Credits_TileMapDst
    ld bc, 32*20
    rst Fill
    ld a, 0 | ATTR_VRAM_BANK_1
    coord hl, 2, 1, Credits_TileMapDst
    ld de, 32 - 16
    ld b, 16
.setattr
    ld c, 16
.setattr2
    ld [hl+], a
    dec c
    jr nz, .setattr2
    add hl, de
    dec b
    jr nz, .setattr

    ld hl, Credits_TextOffset
    ld a, LOW(CreditsText)
    ld [hl+], a
    ld [hl], HIGH(CreditsText)
    call Credits_DrawText

    ; LCD on, win off, tile $8000, map $9800, obj off
    ld a, %10010001
    ldh [rLCDC], a
    xor a
    ldh [rSCX], a
    ldh [rSCY], a

    call HHDMA_Install
    call HHDMA_NoCallback

    ; set up interrupts
    ld a, 1 << IF_VBLANK
    ldh [rIE], a
    xor a
    ldh [rIF], a
    ei

    ; Play the entire polygon stream again, with a twist!
    ld a, STREAM_BANK
    ldh [hCurBank], a
    ld [MBC5RomBankLo], a
    ld hl, $4000
Credits_Loop:
    ld a, [hl+]
    ld e, a
    ; always clear buffer and use one palette
    ld a, 1
    ld [wClearBuffer], a
    bit 1, e
    call nz, Credits_ReadPalette
    xor a
    bit 2, e
    call nz, PolyStream_ReadVertArray
    ld [wUseVertIndex], a

Credits_FaceLoop:
    ld a, [hl+]
    cp $fd ; end of stream?
    jp z, Credits_End
    cp $fe ; end of bank?
    jp z, Credits_DoneBank
    cp $ff ; end of frame?
    jp z, Credits_DonePoly

    call PolyStream_LoadVerts
    ; close the polygon
    ; c still holds the next index
    ldh a, [hVertTabX]
    ldh [c], a
    set 4, c
    ldh a, [hVertTabY]
    ldh [c], a
    ; save polystream address
    push hl

Credits_ClearBuffer:
    ; weirdly placed here to minimize wait time until
    ; the last render buffer transfer is completed
    ld a, [wClearBuffer]
    and a
    jp z, .skip
    call HHDMA_Wait
    xor a
    ld h, HIGH(sRenderBuf)
    ld b, 16
.loop
    ld l, Y_OFFSET * 2
    rept 100
        ld [hl+], a
        inc l ; only first layer, the other is cleared by a textbox
    endr
    ; next column
    inc h
    dec b
    jp nz, .loop
    ; no more clearing until the next frame
    ld [wClearBuffer], a
.skip

credits_stroke_dy0: MACRO
    srl d
    jr nc, .pixloop\@
    inc d
    jr .noreload\@
.pixloop\@
    ld a, e
    or [hl]
    ld [hl], a
    srl e
    jr nc, .noreload\@
    inc h
    ld e, %10000000
.noreload\@
    ; unroll a bit
    ld a, e
    or [hl]
    ld [hl], a
    srl e
    jr nc, .noreload2\@
    inc h
    ld e, %10000000
.noreload2\@
    dec d
    jr nz, .pixloop\@
    ld a, e
    ENDM

credits_stroke_dx0: MACRO
    ; \1 = dy is positive
    srl e
    jr nc, .pixloop\@
    inc e
    jr .noreload\@
.pixloop\@
    ld a, d
    or [hl]
    IF \1
        ld [hl+], a
        inc l
    ELSE
        ld [hl-], a
        dec l
    ENDC
.noreload\@
    ; unroll a bit
    ld a, d
    or [hl]
    IF \1
        ld [hl+], a
        inc l
    ELSE
        ld [hl-], a
        dec l
    ENDC
    dec e
    jr nz, .pixloop\@
    ld a, d
    ENDM

credits_stroke_fract: MACRO
    ; \1 = dy is positive
.pixloop\@
    ld a, c
    or [hl]
    ld [hl], a
    ldh a, [hFraction]
    add e
    ldh [hFraction], a
    ldh a, [hLoopReload]
    adc 0
    jr z, .novert\@
    ld d, a
.loopvert\@
    ld a, c
    or [hl]
    IF \1
        ld [hl+], a
        inc l
    ELSE
        ld [hl-], a
        dec l
    ENDC
    dec d
    jr nz, .loopvert\@
.novert\@
    srl c
    jr nc, .noreload\@
    inc h
    ld c, %10000000
.noreload\@
    dec b
    jr nz, .pixloop\@
    ld a, c
    ENDM

credits_stroke_whole: MACRO
    ; \1 = dy is positive
.pixloop\@
    ld e, d
.loopvert\@
    ld a, c
    or [hl]
    IF \1
        ld [hl+], a
        inc l
    ELSE
        ld [hl-], a
        dec l
    ENDC
    dec e
    jr nz, .loopvert\@
    srl c
    jr nc, .noreload\@
    inc h
    ld c, %10000000
.noreload\@
    dec b
    jr nz, .pixloop\@
    ld a, c
    ENDM

Credits_Stroke:
    ; stroke edges normally
    ; polygons are wound counterclockwise
    ; 01000000    01000000
    ; 00010000    10110000
    ; 10000000 -> 11001000
    ; 00001000    00111000
    ; 00000000    00000000

    ld hl, hVertTabX
    ld a, [wVertCount]
.edgeloop
    push af
    ld a, [hl+] ; x1
    ld b, a
    push hl ; save hl for next iteration
    ld a, [hl-] ; x2
    ld d, a
    set 4, l
    ld a, [hl+] ; y1
    ld c, a
    ld e, [hl] ; y2
    ld a, d
    sub b
    jr c, .reversex
.noreversex
    ld d, a ; dx
    ld a, e
    sub c
    push af ; save dy flags
    jr c, .reversey
    jr .noreversey
.reversex
    ld b, d ; start at x2 instead
    neg
    ld d, a ; dx
    ld a, c
    ld c, e ; start at y2 instead
    sub e
    push af ; save dy flags
    jr nc, .noreversey
.reversey
    neg
.noreversey
    ld e, a ; dy

    ; b = starting x
    ; c = starting y
    ; d = x difference
    ; e = y difference
    pop af ; get dy flags
    jr nz, .normal

.unchangedy
    ; y doesn't change, we have faster routine for this
    ld a, c
    ; no need to stroke if it's part of the display edge
    and a
    jp z, .pixskip
    cp 99
    jp nc, .pixskip
    ; no need to stroke if both x and y don't change
    ld a, d
    and a
    jp z, .pixskip
    call .loadpixtab
    ld e, [hl]
    call .loadbufptr
    credits_stroke_dy0
    jp .pixdone

.normal
    push af ; save e's sign for later
    push de ; save difference for loop counter
    ld a, d
    and a ; == 0?
    jr nz, .normal2

.unchangedx
    ; x doesn't change, we have faster routine for this
    pop de ; restore loop counter
    ld a, b
    ; no need to stroke if it's part of the display edge
    and a
    jr z, .skipx
    cp 127
    jr nc, .skipx
    call .loadpixtab
    ld d, [hl]
    call .loadbufptr
    pop af ; e's flag
    jr c, .unchangedx_minus
    credits_stroke_dx0 1
    jp .pixdone

.unchangedx_minus
    credits_stroke_dx0 0
    jp .pixdone

.skipx
    pop af
    jp .pixskip

.normal2
    dec a ; == 1?
    jr nz, .dodivide
    ld d, e
    ld e, 0
    jr .skipdivide

.dodivide
    ; d.e = e / d
    xor a
    srl d
    rra
    or e
    ld l, a
    ld a, d
    add $40
    ld h, a
    ld a, DIVTAB_BANK
    ; turn off interrupts since VBlank relies on hCurBank to restore
    ; the bank back and could potentially mess up the table lookup
    di
    ld [MBC5RomBankLo], a
    ld e, [hl]
    inc a
    ld [MBC5RomBankLo], a
    ld d, [hl]
    ldh a, [hCurBank]
    ld [MBC5RomBankLo], a
    ld hl, rIF
    ; avoid HHDMA firing right after enabling interrupts and miss the timing
    res IF_TIMER, [hl]
    ei

.skipdivide
    ; calculate starting address and pixel
    call .loadpixtab
    ld a, [hl]
    ldh [hFraction], a ; temp
    call .loadbufptr

    pop bc ; get x difference for loop counter
    ldh a, [hFraction] ; starting pixel
    ld c, a
    ; is increment a whole number?
    ld a, e
    and a
    jr z, .whole ; use faster routine
    ld a, d
    ldh [hLoopReload], a
    ld a, $80 ; round
    ldh [hFraction], a
    pop af ; e's sign
    jr c, .fract_minus
    credits_stroke_fract 1
    jr .pixdone

.fract_minus
    credits_stroke_fract 0
    jr .pixdone

.whole
    pop af ; e's sign
    jr c, .whole_minus
    credits_stroke_whole 1
    jr .pixdone

.whole_minus
    credits_stroke_whole 0

.pixdone
    ; also stroke the ending pixel
    or [hl]
    ld [hl], a
.pixskip
    pop hl
.skip
    pop af
    dec a
    jp nz, .edgeloop

    ; restore polystream address for next iteration
    pop hl
    jp Credits_FaceLoop

.loadpixtab
    ld a, b
    and %00000111
    add LOW(OnePixelTable)
    ld l, a
    ld h, HIGH(OnePixelTable)
    ret

.loadbufptr
    ld a, c
    add Y_OFFSET
    add a ; x2
    ld l, a
    ld a, b
    srl a
    srl a
    srl a
    add HIGH(sRenderBuf)
    ld h, a
    ret

Credits_End:
    ; restart the stream
    ld a, STREAM_BANK
    ldh [hCurBank], a
    ld [MBC5RomBankLo], a
    ld hl, $4000
    jr Credits_DonePoly

Credits_DoneBank:
    ld hl, hCurBank
    inc [hl]
    ld a, [hl]
    ld [MBC5RomBankLo], a
    ld hl, $4000

Credits_DonePoly:
    push hl
    ld a, 1
    ldh [rVBK], a
    ld hl, sRenderBuf
    ld de, Credits_TileDataDst
    lb bc, 0, 5 ; 256, 8
    call HHDMA_Transfer
    ld hl, CreditsTimerLast
    ld a, [CreditsTimer]
    cp [hl]
    ld [hl], a
    jr nc, .noskip
    ld hl, hCurBank
    ld a, [hl]
    add 3
    cp STREAM_BANK + STREAM_BANKS
    jr c, .norestart
    sub STREAM_BANKS
.norestart
    ld [hl], a
    ld [MBC5RomBankLo], a
    pop hl
    ld hl, $4000
    jp Credits_Loop

.noskip
    pop hl
    jp Credits_Loop

Credits_ReadPalette:
    ; no need to parse palette data here, just skip them
    ld a, [hl+]
    ld b, a
    ld a, [hl+]
    ld c, a
    ld d, 16
.dummyread
    sla c
    rl b
    jr nc, .skip2
    inc hl
    inc hl
.skip2
    dec d
    jr nz, .dummyread
    ret

; ================

Credits_DrawText::
    ld hl, Credits_Palette
    ld de, wPalTab
    lb bc, 2, 4
    call SetFadeFromWhite

    ; Luckily this routine is part of VBlank interrupt
    ; so there's no need to save a bank
    ld a, BANK(CreditsText)
    ld [MBC5RomBankLo], a
    ld hl, Credits_TextOffset
    ld a, [hl+]
    ld h, [hl]
    ld l, a
    ld de, sRenderBuf + 1 ; odd plane
    call Credits_DrawString
    ld de, sRenderBuf + 80*2 + 1
    call Credits_DrawString
    ld de, sRenderBuf + 96*2 + 1
    call Credits_DrawString
    ld de, sRenderBuf + 112*2 + 1
    call Credits_DrawString

    cp16 hl, CreditsText.end
    jr c, .noreset
    ld hl, CreditsText
.noreset
    ld a, h
    ld [Credits_TextOffset+1], a
    ld a, l
    ld [Credits_TextOffset], a
    ; restore the current bank
    ldh a, [hCurBank]
    ld [MBC5RomBankLo], a
    ret

; WARNING: This is assumed to be running during VBlank!
Credits_DrawString::
    ASSERT wStrokeTab % 16 == 0
    ld b, 16
    ld a, [hl]
    and a
    jr z, .blankline
.loop1
    ld a, [hl+]
    sub " "
    push hl
    ld h, HIGH(wStrokeTab) >> 4 ; speedup
    rept 4
        add a
        rl h
    endr
    ld l, a
    push de
    ld c, 16
.loop2
    ld a, [hl+]
    ld [de], a
    inc e
    inc e
    dec c
    jr nz, .loop2
    pop de
    pop hl
    inc d ; next 8 pixels
    dec b
    jr nz, .loop1
    ret

.blankline
    push hl
    ld h, d
    xor a
.loop3
    ld l, e
    rept 15
        ld [hl+], a
        inc l
    endr
    ld [hl], a
    inc h ; next 8 pixels
    dec b
    jr nz, .loop3
    pop hl
    inc hl
    ret

Credits_UpdateFade:
    ld a, [CreditsTimer]
    cp 3
    ret c ; wait for renderer to catch up
    cp 3 + 8
    jr c, .loadpal2
    cp 256 - 8
    ret c
    jr nz, .loadpal2
    ld a, [wOddFrame]
    and a
    jr nz, .loadpal2
    ld hl, Credits_Palette
    ld de, wPalTab
    lb bc, 2, 4
    call SetFadeToWhite
    ld a, 1
.loadpal2
    ld [wLoadPal], a
    jp ProcFade

Credits_VBlankUpdate:
    push af
    push bc
    push de
    push hl

    ld a, [wLoadPal]
    and a
    jr z, .loadpal_done
    ld a, $80
    ldh [rBGPI], a
    ld hl, wPalTab
    lb bc, 1 _PALETTES, LOW(rBGPD)
.loadpal
    ld a, [hl+]
    ldh [c], a
    dec b
    jr nz, .loadpal
    xor a
    ld [wLoadPal], a
.loadpal_done

    ld hl, rIF
    ; avoid HHDMA firing right after enabling interrupts and miss the timing
    res IF_TIMER, [hl]
    ei

    call UpdateMusic

    ld a, [wOddFrame]
    xor 1
    ld [wOddFrame], a
    jr nz, .odd
    ld hl, CreditsTimer
    inc [hl]
    call z, Credits_DrawText
.odd
    call Credits_UpdateFade

    pop hl
    pop de
    pop bc
    pop af
    reti
.end

Credits_Palette:
    color  0,  0,  0
    color  0, 31,  0
    color 11, 23, 15
    color 11, 31, 15

SECTION "Credits Text", ROMX

CreditsText:
    db "   GBC-NICCC    "
    db 0
    db "ANOTHER DEMO BY "
    db "     -AYCE-     "

    db 0
    db 0
    db "  THE CREDITS..."
    db 0

    db "3D-MOVIE GFX    "
    db 0
    db "       MON / OXG"
    db 0

    db "2D-GRAPHICS     "
    db "             DOC"
    db "            NATT"
    db " TWOFLOWER/TRIAD"

    db "DIGITAL MUSIC   "
    db "           DEVED"
    db "            NATT"
    db "            ZLEW"

    db "CODING          "
    db "            NATT"
    db "           DEVED"
    db "          ISSOTM"

    db "GB SOUND CODE   "
    db 0
    db "   S. HOCKENHULL"
    db 0

    db 0
    db 0
    db "   OUR HEARTS   "
    db "   GO OUT TO... "

    db 0
    db "OXYGENE  LEONARD"
    db "TITAN     STROBE"
    db "  MARQUEE DESIGN"

    db 0
    db "BOTB  DESIRE DOX"
    db "JOKER   SNORPUNG"
    db "  T LOVRS COMITY"

    db 0
    db "DALTON     WERMI"
    db "AGENDA  PHANTASY"
    db "FAIRLIGHT  TRIAD"

    db "  -AYCE 2021-   "
    db 0
    db "      CREDITS DO"
    db " THE LOOP NOW..."

.end

Credits_Font:
    INCBIN "data/gfx/font.1bpp.wle"

SECTION "Credits - RAM", WRAM0

Credits_TextOffset:     dw
CreditsTimer:           db
CreditsTimerLast:       db
