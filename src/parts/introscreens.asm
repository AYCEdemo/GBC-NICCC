INCLUDE "src/macros.asm"
INCLUDE "src/hw.asm"

SECTION "Intro screens - Code", ROM0

IntroScreens::

GBCNICCCScreen:
    ; No, I don't want white flashes :P
    call    IntroScreen_Init

    ld      hl,GBCNICCCTiles
    ld      de,sRenderBuf
    call    DecodeWLE
    call    DecodedWLESize2HHDMATiles
    ld      hl,sRenderBuf
    ld      de,Tileset1
    ld      c,8
    call    HHDMA_Transfer

    ld      hl,IntroScreen_OAMBufferOdd
    ld      bc,40*4
    xor     a
    rst     Fill
    ld      hl,IntroScreen_OAMBufferEven
    ld      bc,40*4
    xor     a
    rst     Fill

    ld      hl,AYCE_OAMOdd
    ld      de,IntroScreen_OAMBufferOdd
    ld      bc,AYCE_OAMOdd.end-AYCE_OAMOdd
    rst     Copy
    ld      hl,AYCE_OAMEven
    ld      de,IntroScreen_OAMBufferEven
    ld      bc,AYCE_OAMEven.end-AYCE_OAMEven
    rst     Copy

    call    HHDMA_Wait
    ld      hl,GBCNICCCMap
    ld      de,Map1
    lb      bc,(GBCNICCCMap.end-GBCNICCCMap)>>4,8
    call    HHDMA_Transfer

    call    HHDMA_Wait
    ld      a,1
    ldh     [rVBK],a
    ld      hl,GBCNICCCAttr
    ld      de,Map1
    lb      bc,(GBCNICCCAttr.end-GBCNICCCAttr)>>4,8
    call    HHDMA_Transfer

    ld      a,GBCNICCCPal.end-GBCNICCCPal
    ld      [IntroScreen_BGCols],a
    ld      hl,GBCNICCCPal
    ld      de,wPalTabLarge
    lb      bc,1,(GBCNICCCPal.end-GBCNICCCPal)/2
    call    SetFadeFromBlack

    xor     a
    ldh     [rSCX],a
    ldh     [rSCY],a
    
    ld      a,LCDC_ON | LCDC_BG8000
    ldh     [rLCDC],a
    ei

    ld      hl,GBCNICCCPal
    call    IntroScreen_MainLoop
    ; fall through

AYCEScreen:
    di
    xor     a
    ldh     [rVBK],a
    ld      a,IntroScreen_WaitTime
    ld      [IntroScreen_Timer],a
    ld      a,high(IntroScreen_OAMBufferEven)
    ld      [IntroScreen_CurOAMBuffer],a
    copycode    IntroScreens_VBlank,VBlankInt
    ei

    ld      hl,AYCEBGTiles
    ld      de,sRenderBuf
    call    DecodeWLE
    call    DecodedWLESize2HHDMATiles
    ld      hl,sRenderBuf
    ld      de,Tileset2
    ld      c,8
    call    HHDMA_Transfer
    
    call    HHDMA_Wait
    ld      hl,AYCESprTiles
    ld      de,sRenderBuf
    call    DecodeWLE
    call    DecodedWLESize2HHDMATiles
    ld      hl,sRenderBuf
    ld      de,Tileset1
    ld      c,8
    call    HHDMA_Transfer
    
    call    HHDMA_Wait
    ld      a,$7f
    ld      hl,sRenderBuf
    ld      bc,32*19
    push    hl
    push    bc
    rst     Fill
    
    ld      hl,AYCEMap
    ld      bc,AYCEMap.end-AYCEMap
    coord   de,0,3,sRenderBuf
    rst     Copy

    ld      hl,sRenderBuf
    ld      de,Map1
    lb      bc,(32*19)>>4,8
    call    HHDMA_Transfer

    call    HHDMA_Wait
    pop     bc
    pop     hl
    xor     a
    rst     Fill

    ; Clear tile 0:17f for border
    ld      hl,sRenderBuf ; should contain zeros
    ld      de,$97f0
    lb      bc,1,1
    call HHDMA_Transfer

    call    HHDMA_Wait
    ld      hl,AYCEAttr
    ld      bc,AYCEAttr.end-AYCEAttr
    coord   de,0,3,sRenderBuf
    rst     Copy

    ld      a,1
    ldh     [rVBK],a
    ld      hl,sRenderBuf
    ld      de,Map1
    lb      bc,(32*19)>>4,8
    call    HHDMA_Transfer
    
    ld      a,3 _PALETTES
    ld      [IntroScreen_BGCols],a
    ld      a,2 _PALETTES
    ld      [IntroScreen_OBCols],a
    ld      hl,AYCE_BGPal
    ld      de,wPalTabLarge
    lb      bc,1,6*4
    call    SetFadeFromBlack

    xor     a
    ldh     [rSCX],a
    ld      a,2
    ldh     [rSCY],a
    
    ld      a,LCDC_ON | LCDC_OBJON | LCDC_OBJSIZE
    ldh     [rLCDC],a

AYCEScreen_MainLoop:
    halt
    ld      a,[wOddFrame]
    ld      b,a
    ld      a,[IntroScreen_Timer]
    ld      c,a
    or      b
    ret     z ; done
    ld      a,c
    cp      IntroScreen_WaitTime-4
    jr      nc,AYCEScreen_MainLoop
    cp      IntroScreen_WaitTime-20
    jr      c,AYCEScreen_MainLoop
.procfade
    ld      [wLoadPal],a
    call    ProcFade
    jr      AYCEScreen_MainLoop

IntroScreen_Init:
    call HHDMA_NoCallback
    di
    xor     a
    ldh     [rVBK],a
    ld      [IntroScreen_CurOAMBuffer],a
    ld      [IntroScreen_BGCols],a
    ld      [IntroScreen_OBCols],a
    ld      hl,wPalTabLarge
    ld      bc,6 _PALETTES
    rst     Fill
    ld      a,IntroScreen_WaitTime
    ld      [IntroScreen_Timer],a
    copycode    IntroScreens_VBlank,VBlankInt
    ei

    call    BlackPalette
    ld      a,bank(GBCNICCCTiles)
    ldh     [hCurBank],a
    ld      [MBC5RomBankLo],a
    ret

PostScreen::
    call    IntroScreen_Init

    ld      hl,PostScreenTiles
    ld      de,sRenderBuf
    call    DecodeWLE
    call    DecodedWLESize2HHDMATiles
    ld      hl,sRenderBuf
    ld      de,Tileset1
    ld      c,8
    call    HHDMA_Transfer

    call    HHDMA_Wait
    ld      hl,PostScreenMap
    ld      de,Map1
    lb      bc,(PostScreenMap.end-PostScreenMap)>>4,8
    call    HHDMA_Transfer

    call    HHDMA_Wait
    ld      a,1
    ldh     [rVBK],a
    ld      hl,PostScreenAttr
    ld      de,Map1
    lb      bc,(PostScreenAttr.end-PostScreenAttr)>>4,8
    call    HHDMA_Transfer

    ld      a,PostScreenPal.end-PostScreenPal
    ld      [IntroScreen_BGCols],a
    ld      hl,PostScreenPal
    ld      de,wPalTabLarge
    lb      bc,1,(PostScreenPal.end-PostScreenPal)/2
    call    SetFadeFromBlack

    xor     a
    ldh     [rSCX],a
    ldh     [rSCY],a
    
    ld      a,LCDC_ON | LCDC_BG8000
    ldh     [rLCDC],a
    ei

    ld      hl,PostScreenPal
    ; fall through

IntroScreen_MainLoop:
    halt
    ld      a,[wOddFrame]
    ld      b,a
    ld      a,[IntroScreen_Timer]
    ld      c,a
    or      b
    ret     z ; done
    push    hl
    ld      a,c
    cp      20
    jr      z,.setfade
    jr      c,.procfade
    cp      IntroScreen_WaitTime-4
    jr      nc,.noproc
    cp      IntroScreen_WaitTime-20
    jr      c,.noproc
    jr      .procfade
.setfade
    srl     b
    jr      c,.procfade ; odd frame, fade already set
    ld      de,wPalTabLarge
    ld      a,[IntroScreen_BGCols]
    ld      c,a
    ld      a,[IntroScreen_OBCols]
    add     c
    srl     a
    ld      c,a
    ld      b,1
    call    SetFadeToBlack
    ld      a,1
.procfade
    ld      [wLoadPal],a
    call    ProcFade
.noproc
    pop     hl
    jr      IntroScreen_MainLoop

DecodedWLESize2HHDMATiles:
    ; assuming the destination was aligned to $1000 bytes
    ; de holds last address after DecodeWLE
    ld      a,e
    swap    a
    and     $0f
    ld      b,a
    ld      a,d
    swap    a
    and     $f0
    or      b
    ld      b,a
    ret

IntroScreens_VBlank:
    push    af
    push    bc
    push    de
    push    hl

    ld      a,[wLoadPal]
    and     a
    jr      z,.loadpal_done
    ld      a,$80
    ldh     [rBGPI],a
    ldh     [rOBPI],a
    ld      hl,wPalTabLarge
    ld      a,[IntroScreen_BGCols]
    and     a
    jr      z,.nobg
    ld      b,a
    ld      c,LOW(rBGPD)
.loadpal
    ld      a,[hl+]
    ldh     [c],a
    dec     b
    jr      nz,.loadpal
.nobg
    ld      a,[IntroScreen_OBCols]
    and     a
    jr      z,.noob
    ld      b,a
    ld      c,LOW(rOBPD)
.loadpal2
    ld      a,[hl+]
    ldh     [c],a
    dec     b
    jr      nz,.loadpal2
.noob
    xor     a
    ld      [wLoadPal],a
.loadpal_done

    ld      a,[IntroScreen_CurOAMBuffer]
    and     a
    jr      z,.skip
    xor     high(IntroScreen_OAMBufferEven^IntroScreen_OAMBufferOdd)
    ld      [IntroScreen_CurOAMBuffer],a
    call    OAMDMA
.skip
    call    UpdateMusic
    ld      a,[wOddFrame]
    xor     1
    ld      [wOddFrame],a
    jr      nz,.odd
    ld      hl,IntroScreen_Timer
    dec     [hl]
.odd
    pop     hl
    pop     de
    pop     bc
    pop     af
    reti
.end

; ================

SECTION "Intro screens - Aligned Graphics data", ROMX, ALIGN[4]
GBCNICCCMap:        INCBIN "data/gfx/gbcniccc_map.bin"
.end
GBCNICCCAttr:       INCBIN "data/gfx/gbcniccc_attr.bin"
.end

AYCEMap:            INCBIN "data/gfx/ayce_map.bin"
.end
AYCEAttr:           INCBIN "data/gfx/ayce_attr.bin"
.end

PostScreenMap:      INCBIN "data/gfx/ayceback_map.bin"
.end
PostScreenAttr:     INCBIN "data/gfx/ayceback_attr.bin"
.end

SECTION "Intro screens - Graphics data", ROMX
GBCNICCCTiles:      INCBIN "data/gfx/gbcniccc.2bpp.wle"
.end
GBCNICCCPal:        INCBIN "data/gfx/gbcniccc.pal"
.end

AYCEBGTiles:        INCBIN "data/gfx/ayce_bg.2bpp.wle"
.end
AYCESprTiles:       INCBIN "data/gfx/ayce_spr.2bpp.wle"
.end

PostScreenTiles:    INCBIN "data/gfx/ayceback.2bpp.wle"
.end
PostScreenPal:      INCBIN "data/gfx/ayceback.pal"
.end

AYCE_OAMOdd:
    ; line 0
    db 70,  44,  0, 0
    db 70,  52,  2, 0
    db 70,  60,  4, 0
    db 70,  73,  6, 0
    db 70,  81,  8, 1
    db 70,  89, 10, 1
    db 70,  97, 12, 1
    db 70, 105, 14, 1
    db 70, 115, 16, 1
    db 70, 123, 18, 1
    ; line 1
    db 86,  45, 20, 0
    db 86,  53, 22, 0
    db 86,  61, 24, 0
    db 86,  72, 26, 1
    db 86,  80, 28, 1
    db 86,  96, 30, 1
    db 86, 104, 32, 1
    db 86, 112, 34, 1
    db 86, 120, 36, 1
    db 86, 128, 38, 1
.end

AYCE_OAMEven:
    ; line 0
    db 70,  44, 40, 0
    db 70,  52, 42, 0
    db 70,  60, 44, 0
    db 70,  73, 46, 0
    db 70,  81, 48, 1
    db 70,  89, 50, 1
    db 70,  97, 52, 1
    db 70, 105, 54, 1
    db 70, 115, 56, 1
    db 70, 123, 58, 1
    ; line 1
    db 86,  45, 60, 0
    db 86,  53, 62, 0
    db 86,  61, 64, 0
    db 86,  72, 66, 1
    db 86,  80, 68, 1
    db 86,  96, 70, 1
    db 86, 104, 72, 1
    db 86, 112, 74, 1
    db 86, 120, 76, 1
    db 86, 128, 78, 1
.end

AYCE_BGPal:
    color  0,  0,  4
    color  0,  4,  8
    color  4,  8, 12
    color  8, 12, 16
    color  0,  0,  4
    color  0,  4,  8
    color  4,  8, 12
    color  5,  4,  0
    color  0,  0,  4
    color  0,  4,  8
    color  5,  4,  0
    color 12,  8,  0
AYCE_OBPal:
    color  0,  0,  0
    color 17, 12,  0
    color 25, 17,  0
    color 31, 31, 25
    color  0,  0,  0
    color 25, 17,  0
    color 31, 25,  4
    color 31, 31, 25

; ================

SECTION "Intro screens - RAM", WRAM0

IntroScreen_BGCols:         db
IntroScreen_OBCols:         db
IntroScreen_Timer:          db
IntroScreen_CurOAMBuffer:   db

SECTION "Intro screens - Odd OAM buffer", WRAM0, ALIGN[8]
IntroScreen_OAMBufferOdd:   ds  40*4
SECTION "Intro screens - Even OAM buffer", WRAM0, ALIGN[8]
IntroScreen_OAMBufferEven:  ds  40*4

IntroScreen_WaitTime        EQU 17*9 ; time (in 2 frames) to wait before next screen
