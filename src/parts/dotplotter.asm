INCLUDE "src/macros.asm"
INCLUDE "src/hw.asm"

SECTION "Dot Plotter", ROM0

DotPlotter_Buffer       EQUS "sRenderBuf"
DotPlotter_TileMap      EQUS "(sRenderBuf+256*20)"
DotPlotter_AttrMap      EQUS "(sRenderBuf+256*20+32*18)"

DotPlotter_TileDataDst  EQU $8000
DotPlotter_TileMapDst   EQU $9800
DotPlotter_CoordTable   EQU $d000
DotPlotter_Pat1Bank     EQU 1
DotPlotter_Pat1Dots     EQU $1000 / 3 ; = $555
DotPlotter_Pat2Bank     EQU 2
DotPlotter_Pat2Dots     EQU 18 ; 18*18*4 = $510
DotPlotter_Pat3Bank     EQU 3
DotPlotter_Pat3Dots     EQU 28 ; 36*36 = $510

DotPlotter::
    call HHDMA_NoCallback
    di

    copycode DotPlotter_VBlankUpdate, VBlankInt
    ld a, (1 << IF_VBLANK)
    ldh [rIE], a
    ei
    call BlackPalette
    ; LCD on, win off, tile $8000, map $9800, obj off
    ld a, %10010001
    ldh [rLCDC], a

    xor a
    ldh [rVBK], a
    dec a ; ld a, $ff
    ld [wPalTab], a ; TEMP white
    ld [wPalTab+1], a
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

    ld hl, DotPlotter_HHDMACallback
    call HHDMA_SetCallback

    ld a, BANK(DotPlotter_ProjTable)
    ldh [hCurBank], a
    ld [MBC5RomBankLo], a
    ; TODO
    ld a, DotPlotter_Pat3Bank
    ldh [rSVBK], a
    ld a, LOW(DotPlotter_Pat3Dots * DotPlotter_Pat3Dots)
    ld [wNumDots], a
    ld a, HIGH(DotPlotter_Pat3Dots * DotPlotter_Pat3Dots)
    ld [wNumDots+1], a

DotPlotter_Loop:
    ; TODO
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
    ld a, [wCamX]
    add [hl]
    inc hl
    ld c, a
    ld a, [wCamY]
    add [hl]
    inc hl
    ld b, a
    ld a, [wCamZ]
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
    ld h, HIGH(OnePixelTable)
    ld h, [hl] ; pixel data
    and %11111000
    rrca
    rrca
    rrca
    add HIGH(DotPlotter_Buffer)
    ld d, a
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
    ld a, DotPlotter_Pat1Bank
    ldh [rSVBK], a
    ld bc, DotPlotter_CoordTable
    ld de, $a9ce
    ld h, d
    ld l, e
    ld a, LOW(DotPlotter_Pat1Dots * 3 / 2)
    ldh [hLoopCnt2], a
    ld a, HIGH(DotPlotter_Pat1Dots * 3 / 2) + ((DotPlotter_Pat1Dots * 3 / 2) != 0)
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
    ld a, DotPlotter_Pat2Bank
    ldh [rSVBK], a
    ; TODO

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
    ld [hLoopCnt], a
    neg
    ld [hLoopCnt2], a
    push de

    ld de, 65536 / DotPlotter_Pat3Dots / 2
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
    ld a, [hLoopCnt]
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
    ld a, [hLoopCnt2]
    ld [hl+], a
    ; -x -y +z
    ld a, e
    ld [hl+], a
    ld a, d
    ld [hl+], a
    ld a, [hLoopCnt]
    ld [hl+], a
    ; -x -y -z
    ld a, e
    ld [hl+], a
    ld a, d
    ld [hl+], a
    ld a, [hLoopCnt2]
    ld [hl+], a

    pop de
    ld a, e
    add LOW(65536 / DotPlotter_Pat3Dots)
    ld e, a
    ld a, d
    adc HIGH(65536 / DotPlotter_Pat3Dots)
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

    ; ld a, [wLoadPal]
    ; and a
    ; jr z, .loadpal_done
    ld a, $82 ; pal 0 color 1
    ldh [rBGPI], a
    ld hl, wPalTab
    ld a, [hl+]
    ldh [rBGPD], a
    ld a, [hl]
    ldh [rBGPD], a
    ; xor a
    ; ld [wLoadPal], a
.loadpal_done

    call DotPlotter_UpdateCamera

    ld hl, rIF
    ; avoid HHDMA firing right after enabling interrupts and miss the timing
    res IF_TIMER, [hl]
    ei

    push de
    call UpdateMusic
    pop de

    pop hl
    pop bc
    pop af
    reti
.end

DotPlotter_UpdateCamera:
    ASSERT (wCamY - wCamX) == 1
    ASSERT (wCamZ - wCamY) == 1

    ld hl, wCamX
    ld a, $20 ; dpad
    ldh [rJOYP], a
    ldh a, [rJOYP]
    ldh a, [rJOYP]
    ld b, a
    srl b
    jr c, .noright
    dec [hl]
.noright
    srl b
    jr c, .noleft
    inc [hl]
.noleft
    inc hl ; wCamY
    srl b
    jr c, .noup
    inc [hl]
.noup
    srl b
    jr c, .nodown
    dec [hl]
.nodown

    inc hl ; wCamZ
    ld a, $10 ; abss
    ldh [rJOYP], a
    rept 6
    ldh a, [rJOYP]
    endr
    ld b, a
    srl b
    jr c, .noa
    inc [hl]
.noa
    srl b
    jr c, .nob
    dec [hl]
.nob
    ld a, $30 ; done
    ldh [rJOYP], a
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
wCamX:      db
wCamY:      db
wCamZ:      db

DotPlotter_ClearEnd:
