INCLUDE "src/macros.asm"
INCLUDE "src/hw.asm"

SECTION "Polygon Stream", ROM0

PolyStream_TileDataDst  EQU $8000
PolyStream_TileMapDst   EQU $9800
PolyStream_TileMapDst2  EQU $9c00

; 70224 * 65536 * 100 / 4194304
CENTISEC_ADD_FRAME      EQU 109725
; 456 * 2 * 65536 * 100 / 4194304
CENTISEC_ADD_2LINES     EQU 1425

; bit   0: fading, call ProcFade every VBlank
; bit 1-2: state
;   0: pre-fade in, wait for the first frame to be rendered
;   1: stream running
;   2: stream finished, display the total time
wDemoState  EQUS "(wDemoTimer + 1)"

PolyStream::
    ; clear the entire sRenderBuf
    xor a
    ld hl, sRenderBuf
    ld bc, 32*256
    rst Fill
    ld hl, wPolyStream_ClearBegin
    ld bc, wPolyStream_ClearEnd - wPolyStream_ClearBegin
    rst Fill
    copycode PolyStream_Fill_RAMCode, RAMCode

    di
    ; This part starts with fade from white, yaay
    call LCDOff
    copycode PolyStream_VBlankUpdate, VBlankInt

    ld a, $80
    ldh [rBGPI], a
    xor a
    ldh [rVBK], a
    ldh [rSVBK], a
    ld [wDemoTimer], a
    ld [wDemoState], a
    dec a ; ld a, $ff
    ld hl, wPalTab
    lb bc, 3 _PALETTES, LOW(rBGPD)
.whitepal
    ldh [c], a
    ld [hl+], a
    dec b
    jr nz, .whitepal
    ; Also write white to pal 0 col 2
    ld [wPalTabTemp+4], a
    ld [wPalTabTemp+5], a

    ; Clear tile $ff
    xor a
    ld hl, $8ff0
    ld bc, 1 _TILES
    rst Fill

    ld hl, PolyStream_InfoTileData
    ld de, PolyStream_TileDataDst + $e9 _TILES
    call DecodeWLE
    ld a, $ff
    ld hl, PolyStream_TileMapDst
    ld bc, 32*18
    rst Fill
    ld hl, PolyStream_TileMapDst2
    ld bc, 32*18
    rst Fill
    coord hl, 2, 4, PolyStream_TileMapDst
    call PolyStream_SetTiles
    coord hl, 2, 4, PolyStream_TileMapDst2
    call PolyStream_SetTiles
    ld hl, PolyStream_InfoTiles
    coord de, 2, 1, PolyStream_TileMapDst
    ld bc, 16
    rst Copy

    ld a, 1
    ldh [rVBK], a
    xor a
    ld hl, PolyStream_TileMapDst
    ld bc, 32*18
    rst Fill
    ld hl, PolyStream_TileMapDst2
    ld bc, 32*15
    rst Fill
    ld a, 1
    coord hl, 2, 4, PolyStream_TileMapDst
    call PolyStream_SetAttrs
    ld a, 2 | ATTR_VRAM_BANK_1
    coord hl, 2, 4, PolyStream_TileMapDst2
    call PolyStream_SetAttrs

    ld a, LCDC_ON | LCDC_WINON | LCDC_BG8000 | LCDC_BGPRIO
    ldh [rLCDC], a
    xor a
    ldh [rSCX], a
    ld a, 16
    ldh [rSCY], a
    ld a, 7
    ldh [rWX], a
    ld a, 100+16
    ldh [rWY], a
    ld a, (1 << IF_VBLANK)
    ldh [rIE], a
    xor a
    ldh [rIF], a

    call HHDMA_Install
    ld hl, PolyStream_HHDMACallback
    call HHDMA_SetCallback

.waitvb
    ldh a, [rLY]
    cp LY_VBLANK
    jr c, .waitvb
    xor a
    ldh [rIF], a

    ei

    ld a, STREAM_BANK
    ldh [hCurBank], a
    ld [MBC5RomBankLo], a
    ld hl, $4000
PolyStream_Loop:
    ld a, [hl+]
    ld e, a
    xor a
    bit 0, e
    jr z, .noclear
    inc a
.noclear
    ld [wClearBuffer], a
    bit 1, e
    call nz, PolyStream_ReadPalette
    xor a
    bit 2, e
    call nz, PolyStream_ReadVertArray
    ld [wUseVertIndex], a

PolyStream_FaceLoop:
    ld a, [hl+]
    cp $fd ; end of stream?
    jp z, PolyStream_End
    cp $fe ; end of bank?
    jp z, PolyStream_DoneBank
    cp $ff ; end of frame?
    jp z, PolyStream_DonePoly

    ; code shared with credits part
    call PolyStream_LoadVerts
    ; save polystream address
    push hl

PolyStream_FindMinMax:
    ASSERT (wMaxX - wMinX) == 1
    ASSERT (wMinY - wMaxX) == 1
    ASSERT (wMaxY - wMinY) == 1
    xor a
    ld [wMaxX], a
    ld [wMaxY], a
    dec a
    ld [wMinX], a
    ld [wMinY], a
    ld de, wVertTab
    ld a, [wVertCount]
    ld b, a
.loop
    ld a, [de]
    inc de
    ld hl, wMinX
    cp [hl]
    jr nc, .notminx
    ld [hl], a
.notminx
    inc hl ; ld hl, wMaxX
    cp [hl]
    jr c, .notmaxx
    ld [hl], a
.notmaxx
    ld a, [de]
    inc de
    inc hl ; ld hl, wMinY
    cp [hl]
    jr nc, .notminy
    ld [hl], a
.notminy
    inc hl ; ld hl, wMaxY
    cp [hl]
    jr c, .notmaxy
    ld [hl], a
.notmaxy
    dec b
    jr nz, .loop

    ; divide x by 8 for tiles
    ld hl, wMinX
    ld a, [hl]
    rrca
    rrca
    rrca
    and $1f
    ld [hl+], a
    ld b, a
    ld a, [hl] ; wMaxX
    ; inc a ; adjust for end pos
    ; dec a ; ceil
    rrca
    rrca
    rrca
    and $1f
    inc a
    sub b
    ; sanity check from coordinate division: width is 0
    jr nz, .nz2
    inc a
.nz2
    ld [hl+], a ; wBoundWidth
    ; round y to multiple of 4 due to unrolled loops
    ld a, [hl] ; wMinY
    and %11111100
    ld [hl+], a
    ld b, a
    ld a, [hl] ; wMaxY
    ; inc a ; adjust for end pos
    ; dec a ; ceil
    and %11111100
    add 4
    sub b
    ; sanity check from coordinate division: height is 0
    jr nz, .nz4
    ld a, 4
.nz4
    ld [hl], a ; wBoundHeight

PolyStream_ClearStrokeTable:
    ; clear a stroke table based on current calculated bounds
    ld a, [wMinX]
    add HIGH(wStrokeTab)
    ld h, a
    ld a, [wMinY]
    ld c, a
    ld a, [wBoundWidth]
    ld b, a
    ; luckily, since `ld [hl+], a` takes 1 byte, this hack is possible
    ld a, [wBoundHeight]
    ld e, a
    ld a, LOW(.clear)
    sub e
    ld e, a
    ld a, HIGH(.clear)
    sbc 0
    ld d, a
    xor a
.loop
    ld l, c
    ; works like jp [de]
    push de
    ret

    rept 104
        ld [hl+], a
    endr
.clear
    inc h
    dec b
    jp nz, .loop

PolyStream_Stroke:
    ; stroke edges for flood filling
    ; polygons are wound counterclockwise
    ; 01000000    01000000
    ; 00010000    00110000
    ; 10000000 -> 10000000
    ; 00001000    11001000
    ; 00000000    00111000

    ld hl, wVertTab
    ld a, [wVertCount]
.edgeloop
    push af
    ld a, [hl+] ; x1
    ld b, a
    ld a, [hl+] ; y1
    ld c, a
    ld a, [hl+] ; x2
    ld d, a
    ld a, [hl-] ; y2
    ld e, a
    ld a, d
    sub b
    ; no need to stroke if x doesn't change :)
    jp z, .skip
    push hl ; save hl for next iteration
    jr c, .reversex
.noreversex
    ld d, a ; dx
    ld a, e
    sub c
    push af ; keep flags
    inc c ; adjust for "stopper"
    jr c, .reversey
    jr .noreversey
.reversex
    ld b, d ; start at x2 instead
    neg
    ld d, a ; dx
    ld a, c
    ld c, e ; start at y2 instead
    sub e
    push af ; keep flags
    jr nc, .noreversey
.reversey
    neg
.noreversey
    ld e, a
    pop af ; get flags from the last sub
    jr nz, .normal

    ; y doesn't change, we have faster routine for this
    ld a, c
    cp 100
    ; no need to stroke if it's always outside the display :)
    jp nc, .pixdone
    ld l, b
    ld h, HIGH(OnePixelTable)
    ld e, [hl]
    ld l, c
    ld a, b
    rrca
    rrca
    rrca
    and $f
    add HIGH(wStrokeTab)
    ld h, a
    inc d ; also stroke the ending pixel
    srl d
    jr nc, .pixloop2
    inc d
    jr .noreload2
.pixloop2
    ld a, e
    or [hl]
    ld [hl], a
    srl e
    jr nc, .noreload2
    inc h
    ld e, %10000000
.noreload2
    ; unroll a bit
    ld a, e
    or [hl]
    ld [hl], a
    srl e
    jr nc, .noreload3
    inc h
    ld e, %10000000
.noreload3
    dec d
    jr nz, .pixloop2
    jp .pixdone

.normal
    ; b = starting x
    ; c = starting y
    ; d = x difference
    ; e = y difference
    push de ; save difference for loop counter
    push af ; save e's sign for later
    ; ld a, d
    ; dec a ; == 1?
    ; jr nz, .dodivide
    ; ld d, e
    ; ld e, 0
    ; jr .skipdivide

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
    pop af ; sign
    jr nc, .positive
    xor a
    sub e
    ld e, a
    ld a, 0
    sbc d
    ld d, a
.positive
    ; calculate starting address and pixel
    ld l, b
    ld h, HIGH(OnePixelTable)
    ld a, [hl]
    ldh [hFraction], a ; temp
    ld l, c
    ld a, b
    rrca
    rrca
    rrca
    and $f
    add HIGH(wStrokeTab)
    ld h, a

    pop bc ; get x difference for loop counter
    inc b ; also stroke the ending pixel
    ldh a, [hFraction] ; starting pixel
    ld c, a
    ; is increment a whole number?
    ld a, e
    and a
    jr z, .whole ; use faster routine
    ld a, $80 ; round
    ldh [hFraction], a
    srl b
    jr nc, .pixloop
    inc b
    jr .noreload
.pixloop
    ld a, c
    or [hl]
    ld [hl], a
    ldh a, [hFraction]
    add e
    ldh [hFraction], a
    ld a, l
    adc d
    ld l, a
    srl c
    jr nc, .noreload
    inc h
    ld c, %10000000
.noreload
    ; unroll a bit
    ld a, c
    or [hl]
    ld [hl], a
    ldh a, [hFraction]
    add e
    ldh [hFraction], a
    ld a, l
    adc d
    ld l, a
    srl c
    jr nc, .noreload4
    inc h
    ld c, %10000000
.noreload4
    dec b
    jr nz, .pixloop
    jr .pixdone

.whole
    srl b
    jr nc, .pixloop5
    inc b
    jr .noreload5
.pixloop5
    ld a, c
    or [hl]
    ld [hl], a
    ld a, l
    add d
    ld l, a
    srl c
    jr nc, .noreload5
    inc h
    ld c, %10000000
.noreload5
    ; unroll a bit
    ld a, c
    or [hl]
    ld [hl], a
    ld a, l
    add d
    ld l, a
    srl c
    jr nc, .noreload6
    inc h
    ld c, %10000000
.noreload6
    dec b
    jr nz, .pixloop5

.pixdone
    pop hl
.skip
    pop af
    dec a
    jp nz, .edgeloop

PolyStream_ClearBuffer:
    ; weirdly placed here to minimize wait time until
    ; the last render buffer transfer is completed
    ld a, [HHDMA_Status]
    bit 0, a
    ; do the check beforehand to prevent wasting cycles calling this
    call nz, HHDMA_Wait
    ld a, [wClearBuffer]
    and a
    jp z, .skip
    xor a
    ld hl, sRenderBuf
    ld b, 32
.loop
    rept 200 ; yep
        ld [hl+], a
    endr
    ; next page
    ld l, a
    inc h
    dec b
    jp nz, .loop
    ; no more clearing until the next frame
    ld [wClearBuffer], a
.skip

polystream_fill_ramcode_stofs:  MACRO
    ld [RAMCode + PolyStream_Fill_RAMCode.\1 - PolyStream_Fill_RAMCode], a
    ENDM

PolyStream_Fill:
    ; flood fill faces from edges in stroke table
    ; then draw it with appropriate color to the render buffer
    ; 01000000    01000000
    ; 00110000    01110000
    ; 10000000 -> 11110000
    ; 11001000    00111000
    ; 00111000    00000000

    ; patch RAM code so that drawing instructions will replace
    ; bit patterns accordingly to the current color
    ld a, [wCurColor]
    add a ; x2
    add a ; x4
    add LOW(PolyStream_Fill_Insns)
    ld l, a
    adc HIGH(PolyStream_Fill_Insns)
    sub l
    ld h, a
    ld a, [hl+]
    polystream_fill_ramcode_stofs op0_0
    polystream_fill_ramcode_stofs op1_0
    polystream_fill_ramcode_stofs op2_0
    polystream_fill_ramcode_stofs op3_0
    ld a, [hl+]
    polystream_fill_ramcode_stofs op0_1
    polystream_fill_ramcode_stofs op1_1
    polystream_fill_ramcode_stofs op2_1
    polystream_fill_ramcode_stofs op3_1
    ld a, [hl+]
    polystream_fill_ramcode_stofs op0_2
    polystream_fill_ramcode_stofs op1_2
    polystream_fill_ramcode_stofs op2_2
    polystream_fill_ramcode_stofs op3_2
    ld a, [hl]
    polystream_fill_ramcode_stofs op0_3
    polystream_fill_ramcode_stofs op1_3
    polystream_fill_ramcode_stofs op2_3
    polystream_fill_ramcode_stofs op3_3

    ld a, [wMinY]
    ld e, a
    add a
    ld l, a
    ld a, [wMinX]
    add HIGH(wStrokeTab)
    ld d, a
    ld a, [wMinX]
    add HIGH(sRenderBuf)
    ld h, a
    ld a, [wBoundHeight]
    ; last two bits should be cleared
    rrca
    rrca
    ldh [hLoopReload], a
    ld a, [wBoundWidth]
    call RAMCode ; PolyStream_Fill_RAMCode

    ; restore polystream address for next iteration
    pop hl
    jp PolyStream_FaceLoop

PolyStream_DoneBank:
    ld hl, hCurBank
    inc [hl]
    ld a, [hl]
    ld [MBC5RomBankLo], a
    ld hl, $4000

PolyStream_DonePoly:
    push hl
    xor a
    ld [wCurRender], a
    call PolyStream_HHDMACallback

    ; advance demo state for the first frame
    ld a, [wDemoState]
    and a
    jr nz, .skip
    ld hl, wPalTabTemp
    ld de, wPalTab
    lb bc, 3, 12
    call SetFadeFromWhite
    ld a, 11
    ld [wDemoTimer], a
    ld a, 1
    ld [wDemoState], a
.skip
    pop hl
    jp PolyStream_Loop

PolyStream_End:
    ; we are finally done
    ; get the current line position for later
    ldh a, [rLY]
    push af
    ; copy the current timer from the vram
    di ; don't let interupts get in the way
    ld [hSavedSP], sp
    ld c, LOW(rVBK)
    ldh a, [c]
    ld b, a
    xor a
    ldh [c], a
    coord sp, 11, 1, PolyStream_TileMapDst
    ld hl, rSTAT
    waitmode0
    ld hl, wInfoTilesBuf
    rept 7 / 2
    pop de
    ld a, e
    ld [hl+], a
    ld a, d
    ld [hl+], a
    endr
    pop de
    ld [hl], e

    ld a, b
    ldh [c], a
    ldh a, [hSavedSP]
    ld l, a
    ldh a, [hSavedSP+1]
    ld h, a
    ld sp, hl
    ld hl, rIF
    res IF_TIMER, [hl]
    ei

    ld a, 2 << 1
    ld [wDemoState], a
    ; approximate final centisec from the current line position
    pop af
    add 10 ; VBlank lines
    ld de, CENTISEC_ADD_2LINES
    ; ahl = de * a
    lb bc, 8, 0
    ld h, c
    ld l, c
.mulloop
    add hl, hl
    rla
    jr nc, .noadd
    add hl, de
    adc c ; adc 0
.noadd
    dec b
    jr nz, .mulloop
    ; /2 to get 1 line time
    rra
    rr h
    rr l
    ld c, a
    ; no need to write back, just wanted to propagate the carry
    ld a, [wCSecFraction]
    add l
    ld a, [wCSecFraction+1]
    adc h
    ld hl, wInfoTilesBuf + 6
    ; a bit speedup
    lb de, $fa, 10
    ld a, [hl]
    adc c
    cp d ; 10
    jr c, .donenum
    sub e
    ld [hl-], a
    ld a, [hl] ; ds
    inc a
    cp d
    jr c, .donenum
    sub e
    ld [hl-], a
    dec hl ; .
    ld a, [hl] ; s
    inc a
    cp d
    jr c, .donenum
    sub e
    ld [hl-], a
    ld a, [hl] ; 10s
    inc a
    cp $f6
    jr c, .donenum
    sub 6
    ld [hl-], a
    dec hl ; :
    ld a, [hl] ; m
    inc a
.donenum
    ld [hl], a

    ; TODO pause according to music sync byte
    call HHDMA_Wait
    xor a ; 256 frames
.delay
    halt
    dec a
    jr nz, .delay

    ; I'm too lazy to write another VBlank routine for this
    di
    ld a, $80
    ldh [rBGPI], a
    xor a
    ldh [rVBK], a
    ; blank / white
    dec a ; ld a, $ff
    halt ; wait for vblank
    call .clear
    ldh [rBGPD], a
    ldh [rBGPD], a
    ldh [rVBK], a ; set to bank 1 basically
    xor a
    call .clear
    ldh [rSCY], a
    ldh [rIF], a
    ld a, LCDC_ON | LCDC_BG8000 | LCDC_BGPRIO
    ldh [rLCDC], a

    ld b, 15
.fadeout
    push bc
    ; we only have one color to fade out
    ld a, $80
    ldh [rBGPI], a
    ld a, b
    dec a       ; 0000rrrr
    cp 8        ; bit 3 -> carry
    ccf
    rla         ; 000rrrrr
    ld c, a
    swap a      ; gggg000g
    rrca        ; ggggg000
    ld b, c     ; 000bbbbb
    add a       ; gggg0000
    rl b        ; 00bbbbbg
    add a       ; ggg00000
    rl b        ; 0bbbbbgg
    or c        ; gggrrrrr

    halt ; wait for vblank
    ldh [rBGPD], a
    ld a, b
    ldh [rBGPD], a

    call UpdateMusic
    xor a
    ldh [rIF], a

    pop bc
    dec b
    jr nz, .fadeout
    ret

.clear
    ld hl, Map1
    ld de, 32 - 20
    ld b, 18
.clearloop
    rept 20
        ld [hl+], a
    endr
    add hl, de
    dec b
    jr nz, .clearloop
    ret

polystream_fill_ramcode_:   MACRO
    ld a, [de]
    inc e
    xor b
    ld b, a
    jr z, .skip\@
    cpl
    ld c, a
    ld a, [hl]
.op\1_0
    and c
    ld [hl+], a
    ld a, [hl]
.op\1_1
    and c
    ld [hl-], a
    ; change to the second buffer which is luckily 16 pages away
    set 4, h
    ld a, [hl]
.op\1_2
    and c
    ld [hl+], a
    ld a, [hl]
.op\1_3
    and c
    ld [hl-], a
    ; change back to the first buffer
    res 4, h
.skip\@
    inc l
    inc l
    ENDM

PolyStream_Fill_RAMCode:
    ldh [hLoopCnt], a
    ldh a, [hLoopReload]
    ld b, 0
    push de
    push hl

.loop2
    ldh [hLoopCnt2], a
    polystream_fill_ramcode_ 0
    polystream_fill_ramcode_ 1
    polystream_fill_ramcode_ 2
    polystream_fill_ramcode_ 3
    ldh a, [hLoopCnt2]
    dec a
    jr nz, .loop2

    pop hl
    pop de
    inc d
    inc h
    ldh a, [hLoopCnt]
    dec a
    jp nz, RAMCode

    ret
.end

PolyStream_Fill_Insns:
_x = 0
    rept 16
_y = 1
        rept 4
        IF _x & _y
            or b
        ELSE
            and c
        ENDC
_y = _y << 1
        endr
_x = _x + 1
    endr

PolyStream_HHDMACallback:
    ld a, [wCurRender]
    cp 32
    ret z ; we're done

    ld c, a
    and 15
    add a
    add LOW(.dests)
    ld l, a
    adc HIGH(.dests)
    sub l
    ld h, a
    ld a, [hl+]
    ld d, [hl]
    ld e, a
    xor a
    bit 4, c ; >= 16?
    jr z, .loadbank0
    inc a
.loadbank0
    ldh [rVBK], a
    ld a, c
    add HIGH(sRenderBuf)
    ld h, a
    ld l, LOW(sRenderBuf)
    lb bc, 13, 7
    call HHDMA_Transfer
    ld hl, wCurRender
    inc [hl]
    ret

.dests
_x = PolyStream_TileDataDst
    rept 16
        dw _x
_x = _x + 104 * 2
    endr

PolyStream_SetTiles:
    xor a
    ld de, 32
    ld b, 13
.settiles
    ld c, 16
    push hl
.settiles2
    ld [hl+], a
    add 13
    dec c
    jr nz, .settiles2
    pop hl
    add hl, de
    sub 16 * 13 - 1
    dec b
    jr nz, .settiles
    ret

PolyStream_SetAttrs:
    ld de, 32 - 16
    ld b, 13
.setattr
    ld c, 16
.setattr2
    ld [hl+], a
    dec c
    jr nz, .setattr2
    add hl, de
    dec b
    jr nz, .setattr
    ret

PolyStream_ReadPalette:
    ; reminder that the original demo is written for big-endian machine (m68k)
    push de ; save flags
    ld a, [hl+]
    ld b, a
    ld a, [hl+]
    ld c, a
    ld de, wPalTab + 1 _PALETTES
    ; direct this to fade source instead for fading in
    ld a, [wDemoState]
    cp 1 << 1
    jr nc, .normal
    ld de, wPalTabTemp + 1 _PALETTES
.normal
    ld a, 8
.writecolor
    push af
    sla c
    rl b
    jr nc, .skip
    ; ST stores 3-bit component in nibbles, GBC stores 5-bit components continuously
    ; %00000RRR 0GGG0BBB -> %0BBBbbGG GggRRRrr
    push bc
    ; extend 3-bit to 5-bit
    ; this bit twiddling part is hard to explain
    ld a, [hl+]
    and %00000111   ; 00000RRR
    ld c, a
    rlca    ; 0000RRR0
    rlca    ; 000RRR00
    rlca    ; 00RRR000
    or c    ; 00RRRrrr
    rra     ; 000RRRrr
    ld b, a
    ld a, [hl]
    and %01110000   ; 0GGG0000
    ld c, a
    swap a  ; 00000GGG
    rrca    ; G00000GG
    or c    ; Gggg00GG
    ld c, a
    and %11100000   ; Ggg00000
    or b    ; GggRRRrr
    ld [de], a
    inc de
    ld a, [hl+]
    and %00000111   ; 00000BBB
    ld b, a
    swap b  ; 0BBB0000
    rlca    ; 0000bbb0
    and %0001100   ; 0000bb00
    or b    ; 0BBBbb00
    xor c   ; GxxBbbGG  where x = B^g
    and %11111100  ; GxxBbb00
    xor c   ; 0BBBbbGG
    ld [de], a
    dec de
    pop bc
.skip
    inc de
    inc de
    pop af
    dec a
    jr nz, .writecolor
    ; sadly, GBC can't do full 4bpp colors
    ld e, 8
.dummyread
    sla c
    rl b
    jr nc, .skip2
    inc hl
    inc hl
.skip2
    dec e
    jr nz, .dummyread
    ld a, 1
    ld [wLoadPal], a
    pop de
    ret

PolyStream_ReadVertArray::
    ASSERT (wVertArrayY - wVertArrayX) == $100
    push de ; save flags
    ld a, [hl+]
    ld b, a
    ld de, wVertArrayX
.loop
    ld a, [hl+] ; x pos
    srl a ; /2
    ld [de], a
    inc d ; wVertArrayY
    ld a, [hl+] ; y pos
    srl a ; /2
    ld [de], a
    dec d
    inc e
    dec b
    jr nz, .loop
    pop de
    ld a, 1 ; for setting wUseVertIndex
    ret

PolyStream_LoadVerts::
    ld b, a
    swap a
    and $f
    ld [wCurColor], a
    ld a, b
    and $f ; verts count
    ld [wVertCount], a
    ld b, a
    ld de, wVertTab
    ld a, [wUseVertIndex]
    and a
    jr nz, .usevertindex
.novertindex
    ld a, [hl+] ; x
    srl a ; /2
    ld [de], a
    inc de
    ld a, [hl+] ; y
    srl a ; /2
    ld [de], a
    inc de
    dec b
    jr nz, .novertindex
    jr .done
.usevertindex
    push bc
    ld a, [hl+]
    ld c, a
    ld b, HIGH(wVertArrayX)
    ld a, [bc]
    ld [de], a
    inc de
    inc b ; wVertArrayY
    ld a, [bc]
    ld [de], a
    inc de
    pop bc
    dec b
    jr nz, .usevertindex
.done
    ; close the polygon
    ld a, [wVertTab]
    ld [de], a
    inc de
    ld a, [wVertTab+1]
    ld [de], a
    ret

PolyStream_VBlankUpdate:
    push af
    push bc
    push hl

    ld a, [wLoadPal]
    and a
    jr z, .loadpal_done
    ld a, $80
    ldh [rBGPI], a
    ld hl, wPalTab
    lb bc, 3 _PALETTES, LOW(rBGPD)
.loadpal
    ld a, [hl+]
    ldh [c], a
    dec b
    jr nz, .loadpal
    xor a
    ld [wLoadPal], a
.loadpal_done

    call PolyStream_UpdateInfoTiles

    ld a, [wOddFrame]
    xor 1
    ld [wOddFrame], a
    ld a, LCDC_ON | LCDC_WINON | LCDC_BG8000 | LCDC_BGPRIO
    jr nz, .even
    ld a, LCDC_ON | LCDC_WINON | LCDC_BG8000 | LCDC_BG9C00 | LCDC_BGPRIO
.even
    ldh [rLCDC], a

    ld hl, rIF
    ; avoid HHDMA firing right after enabling interrupts and miss the timing
    res IF_TIMER, [hl]
    ei

    push de

    ; are we fading?
    ld a, [wDemoState]
    rra ; bit 0 -> carry
    jr nc, .notfading
    call ProcFade
    ld a, 1
    ld [wLoadPal], a
    ld hl, wDemoTimer
    dec [hl]
    jr nz, .notfading
    ; mark fading as done and advance to the next state
    ld hl, wDemoState
    inc [hl]
.notfading

    call UpdateMusic

    pop de
    pop hl
    pop bc
    pop af
    reti
.end

PolyStream_UpdateInfoTiles:
    ldh a, [rVBK]
    ld c, a
    xor a
    ldh [rVBK], a

    ld a, [wDemoState]
    cp 2 << 1
    jr nc, .noadd

    ld hl, wCSecFraction
    ld a, [hl]
    add LOW(CENTISEC_ADD_FRAME)
    ld [hl+], a
    ld a, [hl]
    adc HIGH(CENTISEC_ADD_FRAME)
    ld [hl], a
    ; where the last number is (cs)
    coord hl, 17, 1, PolyStream_TileMapDst
    ld b, 10 ; a bit speedup
    ld a, [hl]
    adc CENTISEC_ADD_FRAME >> 16
    cp $fa ; 10
    jr c, .donenum
    sub b
    ld [hl-], a
    ld a, [hl] ; ds
    inc a
    cp $fa
    jr c, .donenum
    sub b
    ld [hl-], a
    dec hl ; .
    ld a, [hl] ; s
    inc a
    cp $fa
    jr c, .donenum
    sub b
    ld [hl-], a
    ld a, [hl] ; 10s
    inc a
    cp $f6
    jr c, .donenum
    sub 6
    ld [hl-], a
    dec hl ; :
    ld a, [hl] ; m
    inc a
.donenum
    ld [hl], a

.return
    ld a, c
    ldh [rVBK], a
    ret

.noadd
    ; timer is stopped, animate it a bit by flashing
    ; where the first number is (m)
    coord de, 11, 1, PolyStream_TileMapDst
    ld hl, wDemoTimer
    inc [hl]
    bit 5, [hl] ; 32 frames
    jr z, .show
.hide
    ld h, d
    ld l, e
    ld a, $ff ; blank
    rept 7
        ld [hl+], a
    endr
    jr .return

.show
    ld hl, wInfoTilesBuf
    ; we are not in that tight timing here, so let's just do the normal copy :)
    ld b, 7
.copytiles
    ld a, [hl+]
    ld [de], a
    inc de
    dec b
    jr nz, .copytiles
    jr .return

PolyStream_InfoTiles:
    ;  G   B   C  -   N   I   C   C   C
    db $e9, $ea, $eb, $ec, $ed, $ee, $ef, $ff
    ;        0    :    0    0    .    0    0
    db $ff, $f0, $fa, $f0, $f0, $fb, $f0, $f0

PolyStream_InfoTileData:    INCBIN "data/gfx/numbers.2bpp.wle"

; div table

DIVTAB_BANK = 1
EXPORT DIVTAB_BANK

polystream_div:  MACRO
    IF \2 == 0 ; div by 0
        db $ff
    ELIF \3
        db HIGH(\1 * 256 / \2)
    ELSE
        db LOW(\1 * 256 / \2)
    ENDC
    ENDM

SECTION "Polygon Stream Div Table Low", ROMX[$4000], BANK[DIVTAB_BANK]
_x = 0
    rept 128
_y = 0
        rept 128
            polystream_div _y, _x, 0
_y = _y + 1
        endr
_x = _x + 1
    endr

SECTION "Polygon Stream Div Table High", ROMX[$4000], BANK[DIVTAB_BANK+1]
_x = 0
    rept 128
_y = 0
        rept 128
            polystream_div _y, _x, 1
_y = _y + 1
        endr
_x = _x + 1
    endr

; polygon stream data

STREAM_BANK = 3
STREAM_BANKS = 40
EXPORT STREAM_BANK, STREAM_BANKS

_x = 0
rept 40

_bank = STREAM_BANK + _x
SECTION "Polygon Stream {_x}", ROMX[$4000], BANK[_bank]
    INCBIN "data/scene1_16k.bin", _x * 16384, 16384

_x = _x + 1
endr
