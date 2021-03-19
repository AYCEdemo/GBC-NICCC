INCLUDE "src/macros.asm"
INCLUDE "src/hw.asm"

SECTION "Not GBA Screen - Code", ROM0

NotGBA::
	ld		b,b
	
	ld		hl,NotGBATiles
	ld		de,VRAM_Begin
	call	DecodeWLE
	
	ld		hl,NotGBAMap
	ld		bc,NotGBAMap_End-NotGBAMap
	ld		de,Map1_Begin
	rst		Copy
	
	ld		hl,NotGBAPal
	ld		a,%10000000
	ldh		[rBGPI],a
	rept	4
		ld		a,[hl+]
		ldh		[rBGPD],a
	endr
	
	ld		a,LCDC_ON|LCDC_BG8000
	ldh		[rLCDC],a
    ld		a,(1 << IF_VBLANK)
    ldh		[rIE], a
	
	ld		b,$ff
.loop
	push bc
    call SoundSystem_Process
    ; SoundSystem changes ROM bank while updating music so let's restore that back
    ldh a, [hCurBank]
    ld [MBC5RomBankLo], a
    pop bc
	halt
	dec		b
	jr		nz,.loop
	
	xor		a
	ldh		[rLCDC],a
	ret

SECTION "Not GBA Screen - Graphics data", ROM0
NotGBATiles::	incbin	"data/gfx/notgba.2bpp.wle"

NotGBAMap::		incbin	"data/gfx/notgba.map"
NotGBAMap_End:

NotGBAPal::		incbin	"data/gfx/notgba.pal"