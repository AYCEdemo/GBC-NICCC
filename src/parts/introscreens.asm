INCLUDE "src/macros.asm"
INCLUDE "src/hw.asm"

SECTION "Intro screens - Code", ROM0

IntroScreens::

GBCNICCCScreen:
    ; LCD isn't disabled so do that first
    halt
    xor     a
    ldh     [rLCDC],a
    ldh     [rVBK],a
    di
    ld      a,IntroScreen_WaitTime
    ld      [IntroScreen_Timer],a
    
    ld      a,bank(GBCNICCCTiles)
    ldh     [hCurBank],a
    ld      [MBC5RomBankLo],a
    
    ld      hl,GBCNICCCTiles
    ld      de,Tileset1
    call    DecodeWLE
    
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

    xor     a
    ldh     [rSCX],a
    ldh     [rSCY],a
    
    copycode    GBCNICCC_VBlank,VBlankInt    
    
    ld      a,LCDC_ON | LCDC_BG8000
    ldh     [rLCDC],a
    ld      a,(1 << IF_VBLANK)
    ldh     [rIE],a
	
    ld 		hl,GBCNICCCPal
    ld 		de,wPalTab
    lb 		bc,2,(4 _COLORS) * 4
    call 	SetFadeFromBlack
    
	ei

GBCNICCCScreen_MainLoop:
    halt
    ld      hl,IntroScreen_Timer
    dec     [hl]
	push	af
	ld		a,[hl]
	cp		20
    ld 		hl,GBCNICCCPal
    ld 		de,wPalTab
    lb 		bc,2,(4 _COLORS) * 4
	call	z,SetFadeToBlack
	pop		af
    jr      nz,GBCNICCCScreen_MainLoop
    
AYCEScreen:
    halt
    xor     a
    ldh     [rLCDC],a
    ldh     [rVBK],a
    di
    ld      a,IntroScreen_WaitTime
    ld      [IntroScreen_Timer],a

    ld      hl,AYCEBGTiles
    ld      de,Tileset2
    call    DecodeWLE
    
    ld      hl,AYCESprTiles
    ld      de,Tileset1
    call    DecodeWLE
    
    xor     a
    ld      hl,Map1
    ld      bc,Map2-Map1
    push    hl
    push    bc
    rst     Fill
    
    ld      hl,AYCEMap
    ld      bc,AYCEMap_End-AYCEMap
    ld      de,Map1
    push    de
    rst     Copy
    
    ld      a,1
    ldh     [rVBK],a
    pop     de
    pop     bc
    pop     hl
    ld      a,$6d
    rst     Fill
    
    ld      hl,AYCEAttr
    ld      bc,AYCEAttr_End-AYCEAttr
    ld      de,Map1
    rst     Copy
        
    copycode    AYCE_VBlank,OAMDMA
    ld      hl,VBlankInt
    ld      a,$c3   ; opcode for jp
    ld      [hl+],a
    ld      a,low(OAMDMA)
    ld      [hl+],a
    ld      a,high(OAMDMA)
    ld      [hl],a
    
	ld 		hl,AYCE_BGPal
    ld 		de,wPalTab
    lb 		bc,2,(4 _COLORS) * 4
    call 	SetFadeFromBlack
   
     ld      hl,AYCE_OBJPal
     ld      a,%10000000 | 0 ; palette index 0, auto-increment
     ldh     [rOBPI],a
     ld      c,LOW(rOBPD)
     rept (4 _COLORS) * 2 ; 2 palettes to load
         ld      a,[hl+]
         ldh     [c],a
    endr

    xor     a
    ldh     [rSCX],a
    ld      a,-23
    ldh     [rSCY],a
    
    ld      a,LCDC_ON | LCDC_OBJON | LCDC_OBJSIZE
    ldh     [rLCDC],a
    ld      a,(1 << IF_VBLANK)
    ldh     [rIE],a
	
	ei

AYCEScreen_MainLoop:
    halt
    ld      hl,IntroScreen_Timer
    dec     [hl]
	push	af
	ld		a,[hl]
	cp		20
    ld 		hl,AYCE_BGPal
    ld 		de,wPalTab
    lb 		bc,2,(4 _COLORS) * 4
	call	z,SetFadeToWhite
    ld      a,[hl]
    rrca
    jr      nc,.even
.odd
    ld      a,high(IntroScreen_OAMBufferOdd)
    jr      .continue
.even
    ld      a,high(IntroScreen_OAMBufferEven)
    ; fall through
.continue
    ldh     [OAMDMA+((AYCE_VBlank.bufloc-AYCE_VBlank)+1)],a
    pop     af
    jr      nz,AYCEScreen_MainLoop
    xor     a
    ldh     [rLCDC],a
    di
    
    ; temp hack to stop freezing on exit
    ld      hl,HHDMA_Status
    res     0,[hl]
    ret ; done

GBCNICCC_VBlank:
    push    af
    push    bc
    push    de
    push    hl
	call	ProcFade
    
	ld      hl,wPalTab
    ld      a,%10000000 | 0 ; palette index 0, auto-increment
    ldh     [rBGPI],a
    ld      c,LOW(rBGPD)
    rept (4 _COLORS) * 4 ; 4 palettes to load
        ld      a,[hl+]
        ldh     [c],a
    endr
	
    call    SoundSystem_Process
    ld      a,[hCurBank]
    ld      [MBC5RomBankLo],a
    pop     hl
    pop     de
    pop     bc
    pop     af
    reti
.end

AYCE_VBlank:
    push    af
    push    bc
    push    de
    push    hl
	call	ProcFade
	
    ld      hl,wPalTab
    ld      a,%10000000 | 0 ; palette index 0, auto-increment
    ldh     [rBGPI],a
    ld      c,LOW(rBGPD)
    rept (4 _COLORS) * 4 ; 4 palettes to load
        ld      a,[hl+]
        ldh     [c],a
    endr
	
    call    SoundSystem_Process
    ld      a,[hCurBank]
    ld      [MBC5RomBankLo],a
.bufloc
    ld      a,high(IntroScreen_OAMBufferOdd)
    ld      [rDMA],a
    ld      a,$28
.wait
    dec     a
    jr      nz,.wait
    pop     hl
    pop     de
    pop     bc
    pop     af
    reti
.end

; ================

SECTION "Intro screens - Graphics data", ROMX
GBCNICCCTiles:      INCBIN "data/gfx/gbcniccc.2bpp.wle"
GBCNICCCMap:        INCBIN "data/gfx/gbcnicccmap.bin"
GBCNICCCMap_End:
GBCNICCCAttr:       INCBIN "data/gfx/gbcniccc.atr"
GBCNICCCAttr_End:
GBCNICCCPal:        INCBIN "data/gfx/gbcniccc.pal"

AYCEBGTiles:        INCBIN "data/gfx/ayce_bg.2bpp.wle"
AYCESprTiles:       INCBIN "data/gfx/ayce_spr.2bpp.wle"
AYCEMap:            INCBIN "data/gfx/ayce_map.bin"
AYCEMap_End:
AYCEAttr:           INCBIN "data/gfx/ayce_attr.bin"
AYCEAttr_End:

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

AYCE_OBJPal:
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

IntroScreen_Timer:          db

SECTION "Intro screens - Odd OAM buffer", WRAM0, ALIGN[8]
IntroScreen_OAMBufferOdd:   ds  40*4
SECTION "Intro screens - Even OAM buffer", WRAM0, ALIGN[8]
IntroScreen_OAMBufferEven:  ds  40*4

IntroScreen_WaitTime        EQU 3*60 ; time (in frames) to wait before next screen

section "Intro screens - HRAM", HRAM
OAMDMA_SIZE     EQU AYCE_VBlank.end-AYCE_VBlank
OAMDMA:         ds  OAMDMA_SIZE