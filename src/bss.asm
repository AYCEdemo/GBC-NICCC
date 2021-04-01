INCLUDE "src/macros.asm"

SECTION "Global Variables", WRAM0
buf: MACRO
\1:: ds (\2)
\1_SIZE equ @ - \1
    EXPORT \1_SIZE
ENDM
    buf VBlankInt, $80
    buf LCDInt,    $80
    buf TimerInt,  $80
    buf RAMCode,   $80

; bit 0 = running
; bit 1 = has callback
HHDMA_Status::  db

wReqVB::        db

wPolyStream_ClearBegin::
wDemoTimer::    dw
wOddFrame::     db
wClearBuffer::  db
wUseVertIndex:: db
wLoadPal::      db
wPalTab::       ds 12 _COLORS
wPalTabTemp::   ds 12 _COLORS
wVertCount::    db
wCurColor::     db
wMinX::         db
wBoundWidth::
wMaxX::         db
wMinY::         db
wBoundHeight::
wMaxY::         db
wCurRender::    db
wCSecFraction:: dw
wInfoTilesBuf:: ds 7 ; m:ss:cc
wPolyStream_ClearEnd::

; only 8 palettes for now
wFadeCurrents:: ds 8 * 4 * 3
wFadeTargets::  ds 8 * 4 * 3
wFadeDestPtr::  dw
wFadeCount::    db
wFadeStep::     db
wPalTabLarge::  ds 32 _COLORS

SECTION "Aligned Variables", WRAM0, ALIGN[8]
wVertArrayX::   ds $100
wVertArrayY::   ds $100

SECTION "Render Buffer", SRAM, ALIGN[8]
sRenderBuf::    ds 16*256
sRenderBuf2:    ds 16*256

SECTION "Stroke Table", WRAMX, ALIGN[8]
wStrokeTab::    ds 16*256

SECTION "OAMDMA code", HRAM
OAMDMA::        ds 8

SECTION "High RAM", HRAM
HHDMA_Count::           db
HHDMA_PerLine::         db
HHDMA_NextTransfer::    db

hCurBank::      db
hFraction::     db
hLoopCnt::      db
hLoopCnt2::     db
hLoopReload::   db
hSavedSP::      dw

hCurX0::        dw
hCurX1::        dw
hCurXAdd0::     dw
hCurXAdd1::     dw
hLastX0::       db
hLastX1::       db
hCurY::         db
hVertTabIdx0::  db
hVertTabIdx1::  db
hVertTabY0::    db
hVertTabY1::    db
hVertTabLen::   db
hVertRemain::   db

SECTION "Vertex Table", HRAM, ALIGN[5]
hVertTabX::     ds 16
hVertTabY::     ds 16

hIsGBC          EQU $fffe
EXPORT hIsGBC
