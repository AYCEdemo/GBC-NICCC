neg:    MACRO
    cpl
    inc a
    ENDM

lb:     MACRO
    ld \1, ((\2) << 8) | (\3)
    ENDM

coord:  MACRO
    ; get tilemap offset
    IF _NARG == 3
        ld \1, ((\3) * 32) + (\2)
    ELSE
        ld \1, ((\3) * 32) + (\2) + (\4)
    ENDC
    ENDM

color:  MACRO
    dw (\1) | ((\2) << 5) | ((\3) << 10)
    ENDM

pagecross:  MACRO
    ASSERT HIGH(@) == HIGH(\1)
    ENDM

_COLORS     EQUS "* 2"
_PALETTES   EQUS "* 8"
_TILES      EQUS "* 16"

copycode:   MACRO
    ld hl, \1
    ld de, \2
    ld bc, \1.end - \1
    rst Copy
    ASSERT \1.end - \1 <= \2_SIZE
    ENDM

waitmode0:  MACRO
    ; assuming hl already points to rSTAT
.\@0
    ; let the current mode 0/2 finish
    bit 0, [hl]
    jr z, .\@0
    ; make sure this is mode 3
    bit 1, [hl]
    jr z, .\@0
.\@1
    ; wait until mode 3 finish
    bit 0, [hl]
    jr nz, .\@1
    ; now it's at the very start of mode 0
    ENDM

