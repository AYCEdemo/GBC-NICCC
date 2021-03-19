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
	
	ret

SECTION "Not GBA Screen - Graphics data", ROM0
NotGBATiles::	incbin	"data/gfx/notgba.2bpp.wle"
NotGBAMap::		incbin	"data/gfx/notgba.map"
NotGBAMap_End: