INCLUDE "src/macros.asm"
INCLUDE "src/hw.asm"

SECTION "Dot Plotter", ROM0

DotPlotter_Buffer       EQU $a000
DotPlotter_TileMap      EQU $a000+256*20
DotPlotter_AttrMap      EQU $a000+256*20+32*18

DotPlotter_TileDataDst  EQU $8000
DotPlotter_TileMapDst   EQU $9800
DotPlotter_CoordTable   EQU $d000
DotPlotter_Pat1Bank     EQU 1
DotPlotter_Pat1Dots     EQU $300
DotPlotter_Pat2Bank     EQU 2
DotPlotter_Pat2Dots     EQU 28 ; 28*28 = $310, matches original
DotPlotter_Pat2Time     EQU 17*18
DotPlotter_Pat3Bank     EQU 3
DotPlotter_Pat3Dots     EQU 28 ; 28*28 = $310, matches original
DotPlotter_Pat3Time     EQU 17*34
DotPlotter_TotalTime    EQU 17*48

DotPlotter::
    call HHDMA_Install
    call HHDMA_NoCallback
    di
    copycode DotPlotter_VBlankUpdate, VBlankInt
    ei
    call BlackPalette
    ; LCD on, win off, tile $8000, map $9800, obj off
    ld a, %10010001
    ldh [rLCDC], a

    xor a
    ldh [rVBK], a
    ld hl, DotPlotter_TileMap
    ld de, DotPlotter_TileMapDst
    lb bc, 32*18/16, 8
    call HHDMA_Transfer

    call HHDMA_Wait
    ld a, 1
    ldh [rVBK], a
    ld hl, DotPlotter_AttrMap
    ld de, DotPlotter_TileMapDst
    lb bc, 32*18/16, 8
    call HHDMA_Transfer

    ; Clear tile 1:ff for border
    call HHDMA_Wait
    ld hl, DotPlotter_Buffer ; should contain zeros
    ld de, $8ff0
    lb bc, 1, 1
    call HHDMA_Transfer

    xor a
    ldh [rSCX], a
    ldh [rSCY], a
    ldh [rWX], a
    ldh [rWY], a

    ld [wLoadPal], a
    ld [wDemoTimer], a
    ld [wDemoTimer+1], a
    ld [wOddFrame], a
    inc a
    ld [wAutoCam], a
    ; Match the original demo a bit
    ld a, $80
    ld [wAutoCamX], a
    ld [wAutoCamZ], a

    ld hl, DotPlotter_HHDMACallback
    call HHDMA_SetCallback
    ld a, (1 << IF_VBLANK)
    ldh [rIE], a

    ld a, BANK(DotPlotter_ProjTable)
    ldh [hCurBank], a
    ld [MBC5RomBankLo], a
    ld a, DotPlotter_Pat1Bank
    ldh [rSVBK], a
    ld a, LOW(DotPlotter_Pat1Dots)
    ld [wNumDots], a
    ld a, HIGH(DotPlotter_Pat1Dots)
    ld [wNumDots+1], a

DotPlotter_Loop:
    ld a, [wDemoTimer]
    ld l, a
    ld a, [wDemoTimer+1]
    ld h, a
    cp16 hl, DotPlotter_Pat2Time
    jr c, .done

.pat2
    ld a, DotPlotter_Pat2Bank
    ldh [rSVBK], a
    ld a, LOW(DotPlotter_Pat2Dots * DotPlotter_Pat2Dots)
    ld [wNumDots], a
    ld a, HIGH(DotPlotter_Pat2Dots * DotPlotter_Pat2Dots)
    ld [wNumDots+1], a
    cp16 hl, DotPlotter_Pat3Time
    jr c, .done

.pat3
    ld a, DotPlotter_Pat3Bank
    ldh [rSVBK], a
    ld a, LOW(DotPlotter_Pat3Dots * DotPlotter_Pat3Dots)
    ld [wNumDots], a
    ld a, HIGH(DotPlotter_Pat3Dots * DotPlotter_Pat3Dots)
    ld [wNumDots+1], a
    cp16 hl, DotPlotter_TotalTime
    jr c, .done
    ; Might be one frame off, but that's the next part's job
    ret

.done
    ld a, [HHDMA_Status]
    bit 0, a
    call nz, HHDMA_Wait

DotPlotter_ClearBuffer:
    xor a
    ld hl, DotPlotter_Buffer
    ld b, 20
.loop
    rept 128
        ld [hl+], a
        inc l ; only the first layer
    endr
    ; next column
    inc h
    dec b
    jp nz, .loop

    ; Latch camera coords to reduce tearing
    ld hl, wCamX
    di
    ld a, [hl+]
    ld [wCamXL], a
    ld a, [hl+]
    ld [wCamYL], a
    ld a, [hl+]
    ld [wCamZL], a
    ei

DotPlotter_PlotPixels:
    ld hl, DotPlotter_CoordTable
    ld a, [wNumDots]
    ld c, a
    ld a, [wNumDots+1]
    ld b, a
    dec bc
    inc b
    inc c
.loop
    push bc
    ; the orignal demo culls dots in a cube instead of an actual view frustum
    ; which makes this a lot easier
    ; z is wrapped to 0 - 63 range
    ld a, [wCamXL]
    add [hl]
    inc hl
    ld c, a
    ld a, [wCamYL]
    add [hl]
    inc hl
    ld b, a
    ld a, [wCamZL]
    add [hl]
    inc hl

    push hl
    and $3f
    add HIGH(DotPlotter_ProjTable)
    ld h, a

    ld l, b
    ld e, [hl]
    sla e ; x2
    jr c, .skip ; out of display area
    ; x needs a bit of re-centering to 80
    ld l, c
    ld a, [hl]
    add 80 - 64
    ld l, a
    ld h, HIGH(RenderBufTable)
    ld d, [hl]
    ld h, HIGH(OnePixelTable)
    ld h, [hl] ; pixel data
    ld a, [de]
    or h
    ld [de], a

.skip
    pop hl
    pop bc
    dec c
    jr nz, .loop
    dec b
    jr nz, .loop

DotPlotter_Upload:
    xor a
    ldh [rVBK], a
    ld [wCurRender], a
    ld hl, DotPlotter_Buffer
    ld de, DotPlotter_TileDataDst
    lb bc, 0, 8 ; 256, 8
    call HHDMA_Transfer

    jp DotPlotter_Loop

DotPlotter_Precalc::
    ; Since this is the second part and the first part doesn't use buffer resources
    ; we can precalc quite a lot here
    xor a
    ld hl, DotPlotter_ClearBegin
    ld bc, DotPlotter_ClearEnd - DotPlotter_ClearBegin
    rst Fill
    ld hl, DotPlotter_Buffer
    ld bc, 256*20
    rst Fill

    dec a ; ld a, $ff
    coord hl, 0, 0, DotPlotter_TileMap
    ld bc, 20
    rst Fill
    coord hl, 0, 17, DotPlotter_TileMap
    ld bc, 20
    rst Fill
    xor a
    coord hl, 0, 1, DotPlotter_TileMap
    ld de, 32 - 20
    ld b, 16
.settiles
    ld c, 20
.settiles2
    ld [hl+], a
    add 16
    dec c
    jr nz, .settiles2
    add hl, de
    sub 16 * 4 - 1 ; 16 * 20 - 1
    dec b
    jr nz, .settiles

    ld a, 0 | ATTR_VRAM_BANK_1
    ld hl, DotPlotter_AttrMap
    ld bc, 32*18
    rst Fill
    xor a
    coord hl, 0, 1, DotPlotter_AttrMap
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

    ; Precalc patterns
    ; Pattern 1: Random
    ; Uses MWC PRNG
DotPlotter_Pat1GenCnt   EQU (DotPlotter_Pat1Dots * 3 + 1) / 2
    ; I wish RGBDS has a constant function so I can just wordloopadj(x)
    ASSERT LOW(DotPlotter_Pat1GenCnt) != 0
    ld a, DotPlotter_Pat1Bank
    ldh [rSVBK], a
    ld bc, DotPlotter_CoordTable
    ld de, $a9ce
    ld h, d
    ld l, e
    ld a, LOW(DotPlotter_Pat1GenCnt)
    ldh [hLoopCnt2], a
    ld a, HIGH(DotPlotter_Pat1GenCnt) + 1
.genpat1_1
    ldh [hLoopCnt], a
    ldh a, [hLoopCnt2]
.genpat1_4
    ldh [hLoopCnt2], a
    push bc
    ; dehl = hl * 65184 + de
    push de
    ld d, h
    ld e, l
    ld hl, 0
    ld bc, 65184
    ld a, 16
.genpat1_2
    add hl, hl
    rl e
    rl d
    jr nc, .genpat1_3
    add hl, bc
    jr nc, .genpat1_3
    inc de
.genpat1_3
    dec a
    jr nz, .genpat1_2
    pop bc
    add hl, bc
    pop bc
    ld a, l
    ld [bc], a
    inc bc
    ld a, h
    ld [bc], a
    inc bc
    ldh a, [hLoopCnt2]
    dec a
    jr nz, .genpat1_4
    ldh [hLoopCnt2], a
    ldh a, [hLoopCnt]
    dec a
    jr nz, .genpat1_1

    ; Pattern 2: Sine plane
    ; y = sin(x) * sin(z)
    ; 4 dots per iteration can be done due to how sine works
    ASSERT (DotPlotter_Pat2Dots % 2) == 0
    ld a, DotPlotter_Pat2Bank
    ldh [rSVBK], a
    ld hl, DotPlotter_CoordTable
    ld bc, 64 * 256 / DotPlotter_Pat2Dots / 2 ; z.8
    ld a, DotPlotter_Pat2Dots / 2
.genpat2_1
    push af
    push bc
    ld a, b
    ldh [hLoopCnt], a ; save b
    rept 2 ; 1 turn
    sla c
    rla
    endr
    ld e, a
    ld d, HIGH(SineTable)
    ld a, [de]
    ldh [hLoopCnt2], a ; save the lookup

    ld de, 256 * 256 / DotPlotter_Pat2Dots / 2 ; x.8
    ld a, DotPlotter_Pat2Dots / 2
.genpat2_2
    push af
    push hl
    ld l, d ; 1 turn
    ld h, HIGH(SineTable)
    ld b, [hl]
    ldh a, [hLoopCnt2]
    ld c, a

    xor a
    ld l, a
    ld h, a
    bit 7, b
    jr z, .genpat2_3
    sub c
    ld l, a
.genpat2_3
    ld a, b
    sra a
    and $c0
    add c
    ld c, a
    ld a, 8
.genpat2_4
    add hl, hl
    sla b
    jr nc, .genpat2_5
    add hl, bc
.genpat2_5
    dec a
    jr nz, .genpat2_4
    ld c, h ; mul result

    pop hl
    ; +x +z = +y
    ld a, d
    ld [hl+], a
    ld a, c
    ld [hl+], a
    ldh a, [hLoopCnt]
    ld b, a
    ld [hl+], a
    ; -x -z = +y
    ld a, d
    neg
    ld [hl+], a
    ld a, c
    ld [hl+], a
    ld a, b
    neg ; b = -z
    ld b, a
    ld [hl+], a
    ; +x -z = -y
    ld a, d
    ld [hl+], a
    ld a, c
    neg
    ld c, a ; c = -y
    ld [hl+], a
    ld a, b
    ld [hl+], a
    ; -x +z = -y
    ld a, d
    neg
    ld [hl+], a
    ld a, c
    ld [hl+], a
    ld a, b
    neg ; +z
    ld [hl+], a

    ld a, e
    add LOW(256 * 256 / DotPlotter_Pat2Dots)
    ld e, a
    ld a, d
    adc HIGH(256 * 256 / DotPlotter_Pat2Dots)
    ld d, a
    pop af
    dec a
    jr nz, .genpat2_2
    pop bc
    ld a, c
    add LOW(64 * 256 / DotPlotter_Pat2Dots)
    ld c, a
    ld a, b
    adc HIGH(64 * 256 / DotPlotter_Pat2Dots)
    ld b, a
    pop af
    dec a
    jr nz, .genpat2_1

    ; Pattern 3: Sphere
    ; The original demo generated this with layered circles of same dot count
    ; Even number symmetry speeds this up to 4 dots per iteration
    ASSERT (DotPlotter_Pat3Dots % 2) == 0
    ld a, DotPlotter_Pat3Bank
    ldh [rSVBK], a
    ld hl, DotPlotter_CoordTable
    ld de, 64 * 256 / DotPlotter_Pat3Dots / 2
    xor a
.genpat3_1
    push af
    add LOW(.pat3sizes)
    ld c, a
    ld a, HIGH(.pat3sizes)
    adc 0
    ld b, a
    ld a, [bc]
    ld c, a ; scale
    ld a, d
    ldh [hLoopCnt], a
    neg
    ldh [hLoopCnt2], a
    push de

    ld de, 256 * 256 / DotPlotter_Pat3Dots / 2
    ld a, DotPlotter_Pat3Dots / 2
.genpat3_2
    push af
    push de
    push hl
    ld l, d
    ld h, HIGH(SineTable)
    ld e, [hl]
    ld a, l
    add 64 ; sin -> cos
    ld l, a
    ld b, [hl]
    call .pat3mul
    ld d, h
    ld b, e
    call .pat3mul
    ld e, h

    pop hl
    ; +x +y +z
    ld a, e
    ld [hl+], a
    ld a, d
    ld [hl+], a
    ldh a, [hLoopCnt]
    ld [hl+], a
    ; +x +y -z
    ld a, e
    ld [hl+], a
    neg
    ld e, a
    ld a, d
    ld [hl+], a
    neg
    ld d, a
    ldh a, [hLoopCnt2]
    ld [hl+], a
    ; -x -y +z
    ld a, e
    ld [hl+], a
    ld a, d
    ld [hl+], a
    ldh a, [hLoopCnt]
    ld [hl+], a
    ; -x -y -z
    ld a, e
    ld [hl+], a
    ld a, d
    ld [hl+], a
    ldh a, [hLoopCnt2]
    ld [hl+], a

    pop de
    ld a, e
    add LOW(256 * 256 / DotPlotter_Pat3Dots)
    ld e, a
    ld a, d
    adc HIGH(256 * 256 / DotPlotter_Pat3Dots)
    ld d, a
    pop af
    dec a
    jr nz, .genpat3_2
    pop de
    ld a, e
    add LOW(64 * 256 / DotPlotter_Pat3Dots)
    ld e, a
    ld a, d
    adc HIGH(64 * 256 / DotPlotter_Pat3Dots)
    ld d, a
    pop af
    inc a
    cp DotPlotter_Pat3Dots / 2
    jr nz, .genpat3_1
    ret

.pat3mul
    xor a
    ld l, a
    ld h, a
    bit 7, b
    jr z, .pat3mulskip2
    sub c
    ld l, a
.pat3mulskip2
    ld a, 8
.pat3mulloop
    add hl, hl
    sla b
    jr nc, .pat3mulskip
    add hl, bc
.pat3mulskip
    dec a
    jr nz, .pat3mulloop
    ret

.pat3sizes
    ; this could be automated but RGBDS doesn't have a square root function
    db  67, 115, 145, 169, 188, 203, 216, 226, 235, 242, 247, 251, 254, 255

DotPlotter_VBlankUpdate:
    push af
    push bc
    push hl

    ld a, [wLoadPal]
    and a
    jr z, .loadpal_done
    ld a, $82 ; pal 0 color 1
    ldh [rBGPI], a
    ld hl, wPalTab
    ld a, [hl+]
    ldh [rBGPD], a
    ld a, [hl]
    ldh [rBGPD], a
    xor a
    ld [wLoadPal], a
.loadpal_done

    ld c, 128
    call HHDMA_Interrupt_VBlank
    ei

    call DotPlotter_UpdateTimerAndFade
    call DotPlotter_UpdateCamera

    push de
    call UpdateMusic
    pop de

    pop hl
    pop bc
    pop af
    reti
.end

DotPlotter_UpdateTimerAndFade:
FADE_TIME   EQU 20
    ld a, [wDemoTimer]
    inc a
    ld l, a
    ld [wDemoTimer], a
    ld a, [wDemoTimer+1]
    jr nz, .noinc
    inc a
.noinc
    ld h, a
    ld [wDemoTimer+1], a
    and a
    jr nz, .skip1in
    ld a, l
    cp FADE_TIME
    jr nc, .skip1in
    add FADE_TIME ; skip to fade in
    jr .dofade
.skip1in
    cp16 hl, DotPlotter_Pat2Time - FADE_TIME
    ret c
    cp16 hl, DotPlotter_Pat2Time + FADE_TIME
    jr nc, .skip1out
    ld a, l
    sub LOW(DotPlotter_Pat2Time - FADE_TIME)
    jr .dofade
.skip1out
    cp16 hl, DotPlotter_Pat3Time - FADE_TIME
    ret c
    cp16 hl, DotPlotter_Pat3Time + FADE_TIME
    jr nc, .skip2out
    ld a, l
    sub LOW(DotPlotter_Pat3Time - FADE_TIME)
    jr .dofade
.skip2out
    cp16 hl, DotPlotter_TotalTime - FADE_TIME
    ret c
    ld a, l
    sub LOW(DotPlotter_TotalTime - FADE_TIME)

.dofade
    sub FADE_TIME
    jr nc, .nocpl
    cpl
.nocpl
    add a
    add LOW(.colors)
    ld l, a
    adc HIGH(.colors)
    sub l
    ld h, a
    ld a, [hl+]
    ld [wPalTab], a
    ld a, [hl]
    ld [wPalTab+1], a
    ld a, 1
    ld [wLoadPal], a
    ret

.colors
    dw 0, 0, 0, 0
_x = 1
    rept FADE_TIME - 4
_v = _x * 31 / (FADE_TIME - 4)
        color _v, _v, _v
_x = _x + 1
    endr

DotPlotter_UpdateCamera:
    ld hl, wCamX
    ld a, $20 ; dpad
    ldh [rJOYP], a
    ldh a, [rJOYP]
    ldh a, [rJOYP]
    ld b, a
    ld a, [wAutoCam]
    srl b
    jr c, .noright
    dec [hl]
    xor a
.noright
    srl b
    jr c, .noleft
    inc [hl]
    xor a
.noleft
    inc hl ; wCamY
    srl b
    jr c, .noup
    inc [hl]
    xor a
.noup
    srl b
    jr c, .nodown
    dec [hl]
    xor a
.nodown

    ld c, a ; save wAutoCam flag
    inc hl ; wCamZ
    ld a, $10 ; abss
    ldh [rJOYP], a
    rept 6
    ldh a, [rJOYP]
    endr
    ld b, a
    ld a, c
    srl b
    jr c, .noa
    inc [hl]
    xor a
.noa
    srl b
    jr c, .nob
    dec [hl]
    xor a
.nob
    and a
    ld [wAutoCam], a
    ld a, $30 ; done
    ldh [rJOYP], a
    ret z ; manual cam activated

    ; Update auto cam
    ; x += 1.5, y += 0.5, z += 1.0
    ld h, HIGH(SineTable)
    ld a, [wOddFrame]
    xor 1
    ld [wOddFrame], a
    jr z, .skipy

    ld a, [wAutoCamY]
    ld l, a
    inc a
    ld [wAutoCamY], a
    ld a, [hl]
    add a ; x2
    ld [wCamY], a
    or 1 ; force nz

.skipy
    ld a, [wAutoCamX]
    ld l, a
    jr z, .skipx
    inc a
.skipx
    inc a
    ld [wAutoCamX], a
    ld a, [hl]
    add a ; x2
    ld [wCamX], a

    ld a, [wAutoCamZ]
    ld l, a
    inc a
    ld [wAutoCamZ], a
    ld a, [hl]
    sra a ; /2
    ld [wCamZ], a

    ret

DotPlotter_HHDMACallback:
    ld a, [wCurRender]
    and a
    ret nz ; we're done
    ; Transfer the remaining tiles to another bank
    inc a ; ld a, 1
    ldh [rVBK], a
    ld [wCurRender], a
    ld hl, DotPlotter_Buffer+256*16
    ld de, DotPlotter_TileDataDst
    lb bc, 16*4, 8
    jp HHDMA_Transfer

SECTION "Dot Plotter Projection Table", ROMX, ALIGN[8]
DotPlotter_ProjTable:
_z = 0
    rept 64
_x = 0
        rept 128
            db _x * 16 / (20 + _z * 3 / 4) + 64
_x = _x + 1
        endr
_x = -128
        rept 128
            db _x * 16 / (20 + _z * 3 / 4) + 64
_x = _x + 1
        endr
_z = _z + 1
    endr

SECTION "Dot Plotter Variables", WRAM0
DotPlotter_ClearBegin:

wNumDots:   dw
wPat3Dots:  dw
wAutoCam:   db
wAutoCamX:  db
wAutoCamY:  db
wAutoCamZ:  db
wCamX:      db
wCamY:      db
wCamZ:      db
ASSERT (wCamY - wCamX) == 1
ASSERT (wCamZ - wCamY) == 1
wCamXL:     db
wCamYL:     db
wCamZL:     db

DotPlotter_ClearEnd:
