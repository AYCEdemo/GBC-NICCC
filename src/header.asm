SECTION "RST00 Vector", ROM0[$00]
    jp Init

SECTION "RST08 Vector", ROM0[$08]
Fill::
    dec bc
    inc b
    inc c
.loop
    ld [hl+], a
    dec c
    jr nz, .loop
    dec b
    jr nz, .loop
    ret

SECTION "RST18 Vector", ROM0[$18]
Copy::
    dec bc
    inc b
    inc c
.loop
    ld a, [hl+]
    ld [de], a
    inc de
    dec c
    jr nz, .loop
    dec b
    jr nz, .loop
    ret

SECTION "RST28 Vector", ROM0[$28]
    ret

SECTION "RST30 Vector", ROM0[$30]
    ret

SECTION "RST38 Vector", ROM0[$38]
    ret

SECTION "VBlank IRQ Vector", ROM0[$40]
    jp VBlankInt

SECTION "LCD IRQ Vector", ROM0[$48]
    jp LCDInt

SECTION "Timer IRQ Vector", ROM0[$50]
    jp TimerInt

SECTION "Serial IRQ Vector", ROM0[$58]
    reti

SECTION "Joypad IRQ Vector", ROM0[$60]
    reti

SECTION "Start", ROM0[$100]
    nop
    jp Start

    ds $150 - @, 0 ; will be generated later by RGBFIX
