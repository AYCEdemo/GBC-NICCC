INCLUDE "src/macros.asm"
INCLUDE "src/hw.asm"

SECTION "Credits - Code", ROM0

Credits_TileDataDst EQU $8000
Credits_TileMapDst	EQU $9800
Y_OFFSET			EQU (128-100) / 2

Credits::
	call	SoundSystem_Init
	ld		bc, BANK(Inst_credits)
	ld		de, Inst_credits
	call	Music_PrepareInst
	ld		bc, BANK(Music_credits)
	ld		de, Music_credits
	call	Music_Play
	
	xor		a
	ld		[wLoadPal], a
	ld		hl, sRenderBuf
	ld		bc, 16*256
	rst		Fill

	di
	copycode Credits_VBlankUpdate, Credits_VBlankInt
	call	LCDOff
	ld		a, $80
	ldh		[rBGPI], a
	xor		a
	ldh		[rBGPD], a
	ldh		[rBGPD], a
	ldh		[rVBK], a
	; not GBA screen overwrites the second color so set it to white
	ld		a, $ff
	ld		[rBGPD], a
	and		$7f
	ld		[rBGPD], a
	ld hl, Credits_Font
	ld de, Credits_TileDataDst
	call DecodeWLE
	xor 	a
	ld		hl, Credits_TileMapDst
	ld		bc, 32*18
	rst		Fill
	coord hl, 2, 1, Credits_TileMapDst
	xor		a
	ld		de, 32
	ld		b, 15
.settiles
	ld		c, 16
	push	hl
.settiles2
	ld		[hl+], a
	add 	16
	dec		c
	jr		nz, .settiles2
	pop		hl
	add		hl, de
	sub		16 * 16 - 1
	dec		b
	jr		nz, .settiles

	ld		a, 1
	ldh		[rVBK], a
	xor		a
	ld		hl, Credits_TileMapDst
	ld		bc, 32*20
	rst		Fill
	ld		a, 0 | ATTR_VRAM_BANK_1
	coord	hl, 2, 2, Credits_TileMapDst
	ld		de, 32 - 16
	ld		b, 14
.setattr
	ld		c, 16
.setattr2
	ld		[hl+], a
	dec		c
	jr		nz, .setattr2
	add		hl, de
	dec		b
	jr		nz, .setattr

	; LCD on, win off, tile $8000, map $9800, obj off
	ld		a, %10010001
	ldh		[rLCDC], a
	xor		a
	ldh		[rSCX], a
	ldh		[rSCY], a
	
	; set up interrupts
	ld		a, (1 << IF_VBLANK)
	ldh		[rIE], a
	xor		a
	ldh		[rIF], a
	ld		hl,VBlankInt
	ld		a,$c3		; opcode for jp
	ld		[hl+],a
	ld		a,low(Credits_VBlankInt)
	ld		[hl+],a
	ld		a,high(Credits_VBlankInt)
	ld		[hl],a

	call	Credits_ResetText

	call	HHDMA_Install ; TEMP
	call	HHDMA_NoCallback
	ei

	; Play the entire polygon stream again, with a twist!
	ld		a, STREAM_BANK
	ldh		[hCurBank], a
	ld		[MBC5RomBankLo], a
	ld		hl, $4000
Credits_Loop:
	ld		a, [hl+]
	ld		e, a
	; always clear buffer and use one palette
	ld		a, 1
	ld		[wClearBuffer], a
	bit		1, e
	call	nz, Credits_ReadPalette
	xor		a
	bit		2, e
	call	nz, PolyStream_ReadVertArray
	ld		[wUseVertIndex], a

Credits_FaceLoop:
	ld		a, [hl+]
	cp		$fd ; end of stream?
	jp		z, Credits_End
	cp		$fe ; end of bank?
	jp		z, Credits_DoneBank
	cp		$ff ; end of frame?
	jp		z, Credits_DonePoly
	
	call	PolyStream_LoadVerts
	; save polystream address
	push	hl

Credits_ClearBuffer:
	; weirdly placed here to minimize wait time until
	; the last render buffer transfer is completed
	ld		a, [wClearBuffer]
	and		a
	jp		z, .skip
	call	HHDMA_Wait
	xor		a
	ld		h, HIGH(sRenderBuf)
	ld		b, 16
.loop
	ld		l, Y_OFFSET * 2
	rept 100
		ld		[hl+], a
		inc		l ; only first layer, the other is cleared by a textbox
	endr
	; next column
	inc		h
	dec		b
	jp		nz, .loop
	; no more clearing until the next frame
	ld		[wClearBuffer], a
.skip

credits_stroke_dy0: MACRO
	srl		d
	jr		nc, .pixloop\@
	inc		d
	jr		.noreload\@
.pixloop\@
	ld		a, e
	or		[hl]
	ld		[hl], a
	srl		e
	jr		nc, .noreload\@
	inc		h
	ld		e, %10000000
.noreload\@
	; unroll a bit
	ld		a, e
	or		[hl]
	ld		[hl], a
	srl		e
	jr		nc, .noreload2\@
	inc		h
	ld		e, %10000000
.noreload2\@
	dec		d
	jr		nz, .pixloop\@
	ld		a, e
	ENDM

credits_stroke_dx0: MACRO
	; \1 = dy is positive
	srl		e
	jr		nc, .pixloop\@
	inc		e
	jr		.noreload\@
.pixloop\@
	ld		a, d
	or		[hl]
	IF		\1
		ld		[hl+], a
		inc		l
	ELSE
		ld		[hl-], a
		dec		l
	ENDC
.noreload\@
	; unroll a bit
	ld		a, d
	or		[hl]
	IF		\1
		ld		[hl+], a
		inc		l
	ELSE
		ld		[hl-], a
		dec		l
	ENDC
	dec		e
	jr		nz, .pixloop\@
	ld		a, d
	ENDM

credits_stroke_fract: MACRO
	; \1 = dy is positive
.pixloop\@
	ld		a, c
	or		[hl]
	ld		[hl], a
	ldh		a, [hFraction]
	add		e
	ldh		[hFraction], a
	ldh		a, [hLoopReload]
	adc		0
	jr		z, .novert\@
	ld		d, a
.loopvert\@
	ld		a, c
	or		[hl]
	IF		\1
		ld		[hl+], a
		inc		l
	ELSE
		ld		[hl-], a
		dec		l
	ENDC
	dec		d
	jr		nz, .loopvert\@
.novert\@
	srl		c
	jr		nc, .noreload\@
	inc		h
	ld		c, %10000000
.noreload\@
	dec		b
	jr		nz, .pixloop\@
	ld		a, c
	ENDM

credits_stroke_whole: MACRO
	; \1 = dy is positive
.pixloop\@
	ld		e, d
.loopvert\@
	ld		a, c
	or [hl]
	IF \1
		ld		[hl+], a
		inc		l
	ELSE
		ld		[hl-], a
		dec		l
	ENDC
	dec		e
	jr		nz, .loopvert\@
	srl		c
	jr		nc, .noreload\@
	inc		h
	ld		c, %10000000
.noreload\@
	dec		b
	jr		nz, .pixloop\@
	ld		a, c
	ENDM

Credits_Stroke:
	; stroke edges normally
	; polygons are wound counterclockwise
	; 01000000	  01000000
	; 00010000	  10110000
	; 10000000 -> 11001000
	; 00001000	  00111000
	; 00000000	  00000000

	ld		hl, wVertTab
	ld		a, [wVertCount]
.edgeloop
	push	af
	ld		a, [hl+] ; x1
	ld		b, a
	ld		a, [hl+] ; y1
	ld		c, a
	ld		a, [hl+] ; x2
	ld		d, a
	ld		a, [hl-] ; y2
	ld		e, a
	ld		a, d
	sub		b
	push	hl ; save hl for next iteration
	jr		c, .reversex
.noreversex
	ld		d, a ; dx
	ld		a, e
	sub		c
	push	af ; save dy flags
	jr		c, .reversey
	jr		.noreversey
.reversex
	ld		b, d ; start at x2 instead
	neg
	ld		d, a ; dx
	ld		a, c
	ld		c, e ; start at y2 instead
	sub		e
	push	af ; save dy flags
	jr		nc, .noreversey
.reversey
	neg
.noreversey
	ld		e, a ; dy

	; b = starting x
	; c = starting y
	; d = x difference
	; e = y difference
	pop		af ; get dy flags
	jr		nz, .normal

.unchangedy
	; y doesn't change, we have faster routine for this
	ld		a, c
	; no need to stroke if it's part of the display edge
	and		a
	jp		z, .pixskip
	cp		99
	jp		nc, .pixskip
	; no need to stroke if both x and y don't change
	ld		a, d
	and		a
	jp		z, .pixskip
	call	.loadpixtab
	ld		e, [hl]
	call	.loadbufptr
	credits_stroke_dy0
	jp		.pixdone

.normal
	push	af ; save e's sign for later
	push	de ; save difference for loop counter
	ld		a, d
	and		a ; == 0?
	jr		nz, .normal2

.unchangedx
	; x doesn't change, we have faster routine for this
	pop		de ; restore loop counter
	ld		a, b
	; no need to stroke if it's part of the display edge
	and		a
	jr		z, .skipx
	cp		127
	jr		nc, .skipx
	call	.loadpixtab
	ld		d, [hl]
	call	.loadbufptr
	pop		af ; e's flag
	jr		c, .unchangedx_minus
	credits_stroke_dx0 1
	jp		.pixdone

.unchangedx_minus
	credits_stroke_dx0 0
	jp		.pixdone

.skipx
	pop		af
	jp		.pixskip

.normal2
	dec		a ; == 1?
	jr		nz, .dodivide
	ld		d, e
	ld		e, 0
	jr		.skipdivide

.dodivide
	; d.e = e / d
	xor		a
	srl		d
	rra
	or		e
	ld		l, a
	ld		a, d
	add		$40
	ld		h, a
	ld		a, DIVTAB_BANK
	; turn off interrupts since VBlank relies on hCurBank to restore
	; the bank back and could potentially mess up the table lookup
	di
	ld		[MBC5RomBankLo], a
	ld		e, [hl]
	inc		a
	ld		[MBC5RomBankLo], a
	ld		d, [hl]
	ldh		a, [hCurBank]
	ld		[MBC5RomBankLo], a
	ld		hl, rIF
	; avoid HHDMA firing right after enabling interrupts and miss the timing
	res		IF_TIMER, [hl]
	ei

.skipdivide
	; calculate starting address and pixel
	call	.loadpixtab
	ld		a, [hl]
	ldh		[hFraction], a ; temp
	call	.loadbufptr

	pop		bc ; get x difference for loop counter
	ldh		a, [hFraction] ; starting pixel
	ld		c, a
	; is increment a whole number?
	ld		a, e
	and		a
	jr		z, .whole ; use faster routine
	ld		a, d
	ldh		[hLoopReload], a
	ld		a, $80 ; round
	ldh		[hFraction], a
	pop		af ; e's sign
	jr		c, .fract_minus
	credits_stroke_fract 1
	jr		.pixdone

.fract_minus
	credits_stroke_fract 0
	jr		.pixdone

.whole
	pop		af ; e's sign
	jr		c, .whole_minus
	credits_stroke_whole 1
	jr		.pixdone

.whole_minus
	credits_stroke_whole 0

.pixdone
	; also stroke the ending pixel
	or		[hl]
	ld		[hl], a
.pixskip
	pop		hl
.skip
	pop		af
	dec		a
	jp		nz, .edgeloop

	; restore polystream address for next iteration
	pop		hl
	jp		Credits_FaceLoop

.loadpixtab
	ld		a, b
	and		%00000111
	add		LOW(PolyStream_Pixels)
	ld		l, a
	ld		h, HIGH(PolyStream_Pixels)
	ret

.loadbufptr
	ld		a, c
	add		Y_OFFSET
	add		a ; x2
	ld		l, a
	ld		a, b
	srl		a
	srl		a
	srl		a
	add		HIGH(sRenderBuf)
	ld		h, a
	ret

Credits_End:
	; restart the stream
	ld		a, STREAM_BANK
	ldh		[hCurBank], a
	ld		[MBC5RomBankLo], a
	ld		hl, $4000
	jr		Credits_DonePoly

Credits_DoneBank:
	ld		hl, hCurBank
	inc		[hl]
	ld		a, [hl]
	ld		[MBC5RomBankLo], a
	ld		hl, $4000

Credits_DonePoly:
	push	hl
	ld		a, 1
	ldh		[rVBK], a
	ld		hl, sRenderBuf
	ld		de, Credits_TileDataDst
	lb		bc, 0, 8 ; 256, 8
	call	HHDMA_Transfer
	pop		hl
	jp		Credits_Loop

Credits_ReadPalette:
	; no need to parse palette data here, just skip them
	ld		a, [hl+]
	ld		b, a
	ld		a, [hl+]
	ld		c, a
	ld		d, 16
.dummyread
	sla		c
	rl		b
	jr		nc, .skip2
	inc		hl
	inc		hl
.skip2
	dec		d
	jr		nz, .dummyread
	ret
	
; ================

Credits_DrawText::
	xor		a
	ldh		[rVBK],a
	ld		hl,Credits_TextOffset
	ld		a,[hl+]
	ld		h,[hl]
	ld		l,a
	ld		de,Map1
	call	Credits_DrawString
	ld		de,Map1+$204
	call	Credits_DrawString
	ld		a,1
	ldh		[rVBK],a
	ret
	
Credits_ResetText:
	ld		hl,Credits_TextOffset
	ld		a,low(CreditsText)
	ld		[hl+],a
	ld		a,high(CreditsText)
	ld		[hl],a
	ld		a,(CreditsText_End-CreditsText)/32	; computed at assembly time because magic numbers are Bad(tm)
	ld		[Credits_LoopCounter],a
	ret
	
; WARNING: This is assumed to be running during VBlank!
Credits_DrawString::
	ld		b,16	; ...he says and then uses a magic number :V
	push	hl
.loop1
	ld		a,[hl+]
	add		a
	sub		64	; top char tile offset
	ld		[de],a
	inc		e
	dec		b
	jr		nz,.loop1
	ld		b,16
	ld		a,e
	add		16
	ld		e,a
	pop		hl
.loop2
	ld		a,[hl+]
	add		a
	sub		63	; bottom char tile offset
	ld		[de],a
	inc		e
	dec		b
	jr		nz,.loop2
	ret
	
Credits_VBlankUpdate:
	push	af
	push	bc
	push	de
	push	hl
	
	call	Credits_DrawText
	
	ld		hl,CreditsTimer
	dec		[hl]
	jr		nz,.skip
	
	ld		hl,Credits_TextOffset
	ld		a,[hl+]
	ld		h,[hl]
	ld		l,a
	ld		de,32
	add		hl,de
	ld		a,h
	ld		[Credits_TextOffset+1],a
	ld		a,l
	ld		[Credits_TextOffset],a
	ld		hl,Credits_LoopCounter
	dec		[hl]
	call	z,Credits_ResetText
.skip
	call	SoundSystem_Process
	ldh		a, [hCurBank]
	ld		[MBC5RomBankLo], a

	pop		hl
	pop		de
	pop		bc
	pop		af
	reti
.end
Credits_VBlankInt_SIZE EQU @-Credits_VBlankUpdate

CreditsText:
	db		"THIS HAS BEEN   "
	db		"       GBC-NICCC"
	db		"FIRST SHOWN AT  "
	db		"   REVISION 2021"
	db		"*CODE*          "
	db		"            NATT"
	db		"DEVED           "
	db		"          ISSOTM"
	db		"*GFX*           "
	db		" TWOFLOWER/TRIAD"
	db		"NATT            "
	db		"             DOC"
	db		"*MUSIC*         "
	db		"           DEVED"
	db		"ZLEW            "
	db		"            NATT"
	db		"GREETINGS       "
	db		"           TO..."
	db		"OXYGENE         "
	db		"         LEONARD"
	db		"TITAN           "
	db		"          DESIRE"
	db		"BOTB            "
	db		"  T LOVRS COMITY"
	db		"DOX             "
	db		"          DALTON"
	db		"SNORPUNG        "
	db		"        PHANTASY"
	db		"FAIRLIGHT       "
	db		"           TRIAD"
	db		"CREDITS         "
	db		"        LOOP NOW"
CreditsText_End:
	
Credits_Font:
	incbin	"data/gfx/font.2bpp.wle"
	
SECTION "Credits - RAM", WRAM0

Credits_VBlankInt:		ds	Credits_VBlankInt_SIZE
Credits_TextOffset:		dw
CreditsTimer:			db
Credits_LoopCounter:	db
