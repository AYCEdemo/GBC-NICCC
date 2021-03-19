INCLUDE "src/macros.asm"
INCLUDE "src/hw.asm"

SECTION "Not GBA screen - RAM",WRAM0

NotGBA_VBlankFlag::	db
NotGBA_DoTech::		db
NotGBA_FrameCount::	dw
NotGBA_Timer::		dw

NotGBA_WaitTime		equ	$2a5	; time (in frames) to wait before next screen

SECTION "Not GBA Screen - Code", ROM0

NotGBA::
	di
	xor		a
	ld		[NotGBA_FrameCount],a
	ld		[NotGBA_FrameCount+1],a
	ld		a,low(NotGBA_WaitTime)
	ld		[NotGBA_Timer],a
	ld		a,high(NotGBA_WaitTime)
	ld		[NotGBA_Timer+1],a
	ld		a,1
	ld		[NotGBA_DoTech],a
	
	ld		hl,NotGBATiles
	ld		de,Tileset1
	call	DecodeWLE
	
	ld		hl,NotGBAMap
	ld		bc,NotGBAMap_End-NotGBAMap
	ld		de,Map2
	rst		Copy
	
	ld		hl,NotGBAPal
	ld		a,%10000000|0	; palette index 0, auto-increment
	ldh		[rBGPI],a
	rept	2*2	; 2 colors to write, 1 word per color
		ld		a,[hl+]
		ldh		[rBGPD],a
	endr
	
	ld		hl,LCDInt
	ld		a,$c3		; opcode for jp
	ld		[hl+],a
	ld		a,low(LCDInt_NotGBA)
	ld		[hl+],a
	ld		a,high(LCDInt_NotGBA)
	ld		[hl+],a
	
	ld		a,LCDC_ON|LCDC_BG9C00|LCDC_BG8000
	ldh		[rLCDC],a
    ld		a,(1 << IF_VBLANK) | (1 << IF_LCD_STAT)
    ldh		[rIE], a
	xor		a
	ldh		[rLYC],a
	ld		a,STAT_LYC
	ldh		[rSTAT],a
	ei
	
.loop	
.waitvblank
	ld		hl,NotGBA_VBlankFlag
	ld		[hl],1
	halt
	ld		a,[hl]
	and		a
	jr		z,.waitvblank
	
	xor		a
	ldh		[rSCX],a
	ldh		[rSCY],a
	
	call	SoundSystem_Process
	
	ld		hl,NotGBA_FrameCount
	ld		a,[hl+]
	ld		h,[hl]
	ld		l,a
	inc		hl
	ld		a,l
	ld		[NotGBA_FrameCount],a
	ld		a,h
	cp		2
	jr		nz,.nostoptech
	xor		a
	ld		[NotGBA_DoTech],a
	ldh		[rSTAT],a		; disable LYC interrupt
	ld		a,h
.nostoptech
	ld		[NotGBA_FrameCount+1],a
	
	ld		hl,NotGBA_Timer
	ld		a,[hl+]
	ld		h,[hl]
	ld		l,a
	dec		hl
	ld		a,h
	or		l
	jr		z,.next
	ld		a,h
	ld		[NotGBA_Timer+1],a
	ld		a,l
	ld		[NotGBA_Timer],a
	jr		.loop
.next
	ld		a,(1 << IF_VBLANK)
	ldh		[rIE],a
	ld		a,LCDC_ON|LCDC_WINON|LCDC_WIN9C00|LCDC_BG8000
	ldh		[rLCDC],a
	ld		a,7
	ldh		[rWX],a
	ei

NotGBA_ScreenSlide::
	halt
	call	SoundSystem_Process
	ldh		a,[rWY]
	add		7
	ldh		[rWY],a
	cp		144
	jr		c,NotGBA_ScreenSlide
	
	; TODO

NotGBA_Exit::
	ld		a,$d9		; opcode for reti
	ld		[LCDInt],a
	xor		a
	ldh		[rLCDC],a
	ret
	
; TODO: Fix garbage scanline
LCDInt_NotGBA:
	push	af
	ld		a,[NotGBA_DoTech]
	and		a
	jr		z,.notech
.dotech
	ld		a,e
	ldh		[rSCX],a
	ld		a,d
	ldh		[rSCY],a
	push	bc
	push	hl
	ld		hl,rLYC
	ld		a,[hl]
	cp		144
	jr		nc,.reset
	inc		[hl]
	; get frame count
	ld		hl,NotGBA_FrameCount
	ld		a,[hl+]
	ld		b,[hl]
	ld		c,a
	ld		hl,NotGBASine
	add		hl,bc
	ld		a,[rLY]
	ld		c,a
	ld		b,0
	add		hl,bc
	ld		a,[hl]
	ld		e,a
	ld		d,e
	ldh		a,[rLY]
	bit		0,a
	jr		nz,.odd
	ld		a,e
	neg
	ld		e,a
.odd
	; done
	ld		hl,NotGBA_VBlankFlag
	ld		[hl],0
	jr		.exit
.reset
	ld		[hl],0
.exit
	pop		hl
	pop		bc
.notech
	pop		af
	reti
	
SECTION "Not GBA Screen - Sine table", ROM0, ALIGN[8]
NotGBASine:		incbin	"data/notgba_sine.bin"

SECTION "Not GBA Screen - Graphics data", ROM0
NotGBATiles::	incbin	"data/gfx/notgba.2bpp.wle"

NotGBAMap::		incbin	"data/gfx/notgba.map"
NotGBAMap_End:

NotGBAPal::		incbin	"data/gfx/notgba.pal"