INCLUDE "src/macros.asm"
INCLUDE "src/hw.asm"

SECTION "Dot Plotter", ROM0

DotPlotter_TileDataDst  EQU $8000
DotPlotter_TileMapDst   EQU $9800

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
    ; ld hl, wDotPlotter_ClearBegin
    ; ld bc, wDotPlotter_ClearEnd - wDotPlotter_ClearBegin
    ; rst Fill

    dec a ; ld a, $ff
    ld [wPalTab], a ; TEMP white
    ld [wPalTab+1], a
    coord hl, 0, 0, sRenderBuf
    ld bc, 20
    rst Fill
    coord hl, 0, 17, sRenderBuf
    ld bc, 20
    rst Fill
    xor a
    coord hl, 0, 1, sRenderBuf
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
    ld hl, sRenderBuf
    ld de, DotPlotter_TileMapDst
    lb bc, 32*18/16, 8
    call HHDMA_Transfer

    ld a, 0 | ATTR_VRAM_BANK_1
    ld hl, sRenderBuf + 32*18
    ld bc, 32*18
    rst Fill
    xor a
    coord hl, 0, 1, sRenderBuf + 32*18
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
    call HHDMA_Wait
    ld a, 1
    ldh [rVBK], a
    ld hl, sRenderBuf + 32*18
    ld de, DotPlotter_TileMapDst
    lb bc, 32*18/16, 8
    call HHDMA_Transfer

    ; xor a
    ; ld hl, sRenderBuf
    ; ld bc, 20*256
    ; rst Fill

    ; Clear tile 1:ff for border
    call HHDMA_Wait
    ld hl, sRenderBuf
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

DotPlotter_Loop:
    ; TODO
    call HHDMA_Wait
    xor a
    ldh [rVBK], a
    ld [wCurRender], a
    ld hl, sRenderBuf
    ld de, DotPlotter_TileDataDst
    lb bc, 0, 8 ; 256, 8
    call HHDMA_Transfer

    jp DotPlotter_Loop

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

DotPlotter_HHDMACallback:
    ld a, [wCurRender]
    and a
    ret nz ; we're done
    ; Transfer the remaining tiles to another bank
    inc a ; ld a, 1
    ldh [rVBK], a
    ld [wCurRender], a
    ld hl, sRenderBuf+256*16
    ld de, DotPlotter_TileDataDst
    lb bc, 16*4, 8
    jp HHDMA_Transfer
