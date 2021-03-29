INCLUDE "src/macros.asm"
INCLUDE "src/hw.asm"

SECTION "Intro screens - RAM", WRAM0

IntroScreens_VBlankFlag:  db
IntroScreens_FrameCount:  dw
IntroScreens_Timer:       dw

IntroScreens_WaitTime     EQU 2*60 ; time (in frames) to wait before next screen

SECTION "Intro screens - Code", ROM0

IntroScreens::
    ; LCD isn't disabled so do that first
    halt
    xor     a
    ldh     [rLCDC],a
    di
    xor     a
    ld      [IntroScreens_FrameCount],a
    ld      [IntroScreens_FrameCount+1],a
    ld      a,low(IntroScreens_WaitTime)
    ld      [IntroScreens_Timer],a
    ld      a,high(IntroScreens_WaitTime)
    ld      [IntroScreens_Timer+1],a
    
    ld      a,bank(GBCNICCCTiles)
    ldh     [hCurBank],a
    ld      [MBC5RomBankLo],a


    ld      hl,GBCNICCCTiles
    ld      de,Tileset1
    call    DecodeWLE
    
    ld      hl,GBCNICCCMap
    ld      bc,GBCNICCCMap_End-GBCNICCCMap
    ld      de,Map1
    push    de
    rst     Copy

    ld      a,1
    ldh     [rVBK],a
    ld      hl,GBCNICCCAttr
    ld      bc,GBCNICCCAttr_End-GBCNICCCAttr
    pop     de
    rst     Copy

    ld      hl,GBCNICCCPal
    ld      a,%10000000 | 0 ; palette index 0, auto-increment
    ldh     [rBGPI],a
    ld      c,LOW(rBGPD)
    rept (4 _COLORS) * 4 ; 4 palettes to load
        ld      a,[hl+]
        ldh     [c],a
    endr

    xor     a
    ldh     [rSCX],a
    ldh     [rSCY],a
    
    ld      a,LCDC_ON | LCDC_BG8000
    ldh     [rLCDC],a
    ld      a,(1 << IF_VBLANK)
    ldh     [rIE],a
    ei

IntroScreens_MainLoop:
    halt
    jr      IntroScreens_MainLoop

; ================

SECTION "Intro screens - Graphics data", ROMX
GBCNICCCTiles:      INCBIN "data/gfx/gbcniccc.2bpp.wle"
GBCNICCCMap:        INCBIN "data/gfx/gbcnicccmap.bin"
GBCNICCCMap_End:
GBCNICCCAttr:       INCBIN "data/gfx/gbcniccc.atr"
GBCNICCCAttr_End:
GBCNICCCPal:        INCBIN "data/gfx/gbcniccc.pal"

AYCE1Tiles:         INCBIN "data/gfx/ayce1.2bpp.wle"
AYCE1Map:           INCBIN "data/gfx/ayce1map.bin"
AYCE1Map_End:
AYCE1Attr:          INCBIN "data/gfx/ayce1.atr"
AYCE1Attr_End:
AYCE1Pal:           INCBIN "data/gfx/ayce1.pal"    