; Graciously aped from:
; http://nocash.emubase.de/pandocs.htm
; http://gameboy.mongenel.com/dmg/asmmemmap.html

; memory map
VRAM_Begin  EQU $8000
Tileset1	EQU	$8000
Tileset2	EQU	$8800
Map1		EQU	$9800
Map2		EQU	$9c00
VRAM_End    EQU $a000
SRAM_Begin  EQU $a000
SRAM_End    EQU $c000
WRAM0_Begin EQU $c000
WRAM0_End   EQU $d000
WRAM1_Begin EQU $d000
WRAM1_End   EQU $e000
; hardware registers $ff00-$ff80 (see below)
HRAM_Begin  EQU $ff80
HRAM_End    EQU $ffff

; MBC5
MBC5SRamEnable EQU $0000
MBC5RomBankLo  EQU $2000
MBC5RomBankHi  EQU $3000
MBC5SRamBank   EQU $4000

SRAM_DISABLE EQU $00
SRAM_ENABLE  EQU $0a

; interrupt flags
IF_VBLANK   EQU 0
IF_LCD_STAT EQU 1
IF_TIMER    EQU 2
IF_SERIAL   EQU 3
IF_JOYPAD   EQU 4

; LCDC flags
LCDC_ON			EQU	1<<7 ; 0 = off, 1 = on
LCDC_WIN9C00	EQU	1<<6 ; 0 = 9800, 1 = 9C00
LCDC_WINON		EQU	1<<5 ; 0 = off, 1 = on
LCDC_BG8000		EQU	1<<4 ; 0 = 8800, 1 = 8000
LCDC_BG9C00		EQU	1<<3 ; 0 = 9800, 1 = 9C00
LCDC_OBJSIZE	EQU 1<<2 ; 0 = 8x8, 1 = 8x16
LCDC_OBJON		EQU	1<<1 ; 0 = off, 1 = on
LDCD_BGPRIO		EQU	1<<0 ; 0 = off, 1 = on

; LCD STAT flags
STAT_LYC		EQU	1<<6

; OAM/BG Map attribute flags
ATTR_PALETTE_MASK EQU %111
ATTR_VRAM_BANK_1  EQU 1 << 3 ; $08
ATTR_OBP_NUM      EQU 1 << 4 ; $10
ATTR_X_FLIP       EQU 1 << 5 ; $20
ATTR_Y_FLIP       EQU 1 << 6 ; $40
ATTR_PRIORITY     EQU 1 << 7 ; $80

; Hardware registers
rJOYP       EQU $ff00 ; Joypad (R/W)
rSB         EQU $ff01 ; Serial transfer data (R/W)
rSC         EQU $ff02 ; Serial Transfer Control (R/W)
rDIV        EQU $ff04 ; Divider Register (R/W)
rTIMA       EQU $ff05 ; Timer counter (R/W)
rTMA        EQU $ff06 ; Timer Modulo (R/W)
rTAC        EQU $ff07 ; Timer Control (R/W)
TAC_ON        EQU %100
TAC_4096_HZ   EQU %00
TAC_262144_HZ EQU %01
TAC_65536_HZ  EQU %10
TAC_16384_HZ  EQU %11
rIF         EQU $ff0f ; Interrupt Flag (R/W)
rNR10       EQU $ff10 ; Channel 1 Sweep register (R/W)
rNR11       EQU $ff11 ; Channel 1 Sound length/Wave pattern duty (R/W)
rNR12       EQU $ff12 ; Channel 1 Volume Envelope (R/W)
rNR13       EQU $ff13 ; Channel 1 Frequency lo (Write Only)
rNR14       EQU $ff14 ; Channel 1 Frequency hi (R/W)
rNR20       EQU $ff15 ; Channel 2 Sweep register (R/W)
rNR21       EQU $ff16 ; Channel 2 Sound Length/Wave Pattern Duty (R/W)
rNR22       EQU $ff17 ; Channel 2 Volume Envelope (R/W)
rNR23       EQU $ff18 ; Channel 2 Frequency lo data (W)
rNR24       EQU $ff19 ; Channel 2 Frequency hi data (R/W)
rNR30       EQU $ff1a ; Channel 3 Sound on/off (R/W)
rNR31       EQU $ff1b ; Channel 3 Sound Length
rNR32       EQU $ff1c ; Channel 3 Select output level (R/W)
rNR33       EQU $ff1d ; Channel 3 Frequency's lower data (W)
rNR34       EQU $ff1e ; Channel 3 Frequency's higher data (R/W)
rNR40       EQU $ff1f ; Channel 4 Sweep register (R/W)
rNR41       EQU $ff20 ; Channel 4 Sound Length (R/W)
rNR42       EQU $ff21 ; Channel 4 Volume Envelope (R/W)
rNR43       EQU $ff22 ; Channel 4 Polynomial Counter (R/W)
rNR44       EQU $ff23 ; Channel 4 Counter/consecutive; Inital (R/W)
rNR50       EQU $ff24 ; Channel control / ON-OFF / Volume (R/W)
rNR51       EQU $ff25 ; Selection of Sound output terminal (R/W)
rNR52       EQU $ff26 ; Sound on/off
rWave_0     EQU $ff30
rWave_1     EQU $ff31
rWave_2     EQU $ff32
rWave_3     EQU $ff33
rWave_4     EQU $ff34
rWave_5     EQU $ff35
rWave_6     EQU $ff36
rWave_7     EQU $ff37
rWave_8     EQU $ff38
rWave_9     EQU $ff39
rWave_a     EQU $ff3a
rWave_b     EQU $ff3b
rWave_c     EQU $ff3c
rWave_d     EQU $ff3d
rWave_e     EQU $ff3e
rWave_f     EQU $ff3f
rLCDC       EQU $ff40
rSTAT       EQU $ff41 ; LCDC Status (R/W)
rSCY        EQU $ff42 ; Scroll Y (R/W)
rSCX        EQU $ff43 ; Scroll X (R/W)
rLY         EQU $ff44 ; LCDC Y-Coordinate (R)
LY_VBLANK EQU 144
rLYC        EQU $ff45 ; LY Compare (R/W)
rDMA        EQU $ff46 ; DMA Transfer and Start Address (W)
rBGP        EQU $ff47 ; BG Palette Data (R/W) - Non CGB Mode Only
rOBP0       EQU $ff48 ; Object Palette 0 Data (R/W) - Non CGB Mode Only
rOBP1       EQU $ff49 ; Object Palette 1 Data (R/W) - Non CGB Mode Only
rWY         EQU $ff4a ; Window Y Position (R/W)
rWX         EQU $ff4b ; Window X Position minus 7 (R/W)
rLCDMODE    EQU $ff4c
rKEY1       EQU $ff4d ; CGB Mode Only - Prepare Speed Switch
rVBK        EQU $ff4f ; CGB Mode Only - VRAM Bank
rBLCK       EQU $ff50
rHDMA1      EQU $ff51 ; CGB Mode Only - New DMA Source, High
rHDMA2      EQU $ff52 ; CGB Mode Only - New DMA Source, Low
rHDMA3      EQU $ff53 ; CGB Mode Only - New DMA Destination, High
rHDMA4      EQU $ff54 ; CGB Mode Only - New DMA Destination, Low
rHDMA5      EQU $ff55 ; CGB Mode Only - New DMA Length/Mode/Start
rRP         EQU $ff56 ; CGB Mode Only - Infrared Communications Port
rBGPI       EQU $ff68 ; CGB Mode Only - Background Palette Index
rBGPD       EQU $ff69 ; CGB Mode Only - Background Palette Data
rOBPI       EQU $ff6a ; CGB Mode Only - Sprite Palette Index
rOBPD       EQU $ff6b ; CGB Mode Only - Sprite Palette Data
rUNKNOWN1   EQU $ff6c ; (FEh) Bit 0 (Read/Write) - CGB Mode Only
rSVBK       EQU $ff70 ; CGB Mode Only - WRAM Bank
rUNKNOWN2   EQU $ff72 ; (00h) - Bit 0-7 (Read/Write)
rUNKNOWN3   EQU $ff73 ; (00h) - Bit 0-7 (Read/Write)
rUNKNOWN4   EQU $ff74 ; (00h) - Bit 0-7 (Read/Write) - CGB Mode Only
rUNKNOWN5   EQU $ff75 ; (8Fh) - Bit 4-6 (Read/Write)
rUNKNOWN6   EQU $ff76 ; (00h) - Always 00h (Read Only)
rUNKNOWN7   EQU $ff77 ; (00h) - Always 00h (Read Only)
rIE         EQU $ffff ; Interrupt Enable (R/W)
