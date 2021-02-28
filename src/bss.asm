INCLUDE "src/macros.asm"

SECTION "Global Variables", WRAM0
VBlankInt:: ds $80
LCDInt::    ds $80
TimerInt::  ds $80
RAMCode::   ds $100

; bit 0 = running
; bit 1 = has callback
HHDMA_Status::  db

wReqVB::        db

wPolyStream_ClearBegin::
wOddFrame::     db
wClearBuffer::  db
wUseVertIndex:: db
wLoadPal::      db
wPalTab::       ds 8 _COLORS
wVertCount::    db
wVertTab::      ds 17*2
wCurColor::     db
wMinX::         db
wBoundWidth::
wMaxX::         db
wMinY::         db
wBoundHeight::
wMaxY::         db
wCurRender::    db
wCSecFraction:: dw
wPolyStream_ClearEnd::

SECTION "Aligned Variables", WRAM0, ALIGN[8]
wVertArrayX::   ds $100
wVertArrayY::   ds $100

SECTION "Render Buffer", SRAM, ALIGN[8]
sRenderBuf::    ds 16*256
sRenderBuf2:    ds 16*256

SECTION "Stroke Table", WRAMX, ALIGN[8]
wStrokeTab::    ds 16*256

SECTION "High RAM", HRAM
HHDMA_Count::           db
HHDMA_PerLine::         db
HHDMA_NextTransfer::    db

hCurBank::      db
hFraction::     db
hLoopCnt::      db
hLoopCnt2::     db
hLoopReload::   db

hIsGBC          EQU $fffe
EXPORT hIsGBC
