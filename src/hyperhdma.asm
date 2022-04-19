; AYCE HyperHDMA™ System
; Up to 128 bytes per scanline HDMA transfer using timer interrupt

; It's advised to clear timer IF in more prioritized handlers
; (VBLank, STAT) to avoid it firing right after return and
; miss the timing

; TODO make this work on other interrupt install methods

; Configuration variables

HHDMA_TIMERINT_INSTALL_DEST EQUS "TimerInt"
HHDMA_Copy_Code             EQUS "rst Copy"

CYCLES_PER_LINE     = 456
HHDMA_CallbackAddress       EQUS "((HHDMA_TIMERINT_INSTALL_DEST) + HHDMA_Interrupt.callback - HHDMA_Interrupt + 1)"

HHDMA_Install::
    ; install transfer routine, uses dihalt and modifies STAT
    ; doesn't automatically enable interrupts back
    ; LCD must be turned on and CPU must be in double speed mode
    ; when calling this
    di
    ld hl, HHDMA_Interrupt
    ld de, HHDMA_TIMERINT_INSTALL_DEST
    ld bc, HHDMA_Interrupt.end - HHDMA_Interrupt
    HHDMA_Copy_Code

    ; DIV should be loaded when it's at exactly 504 - 104 = 400 clocks
    ; after the beginning of the line (mode 2). So that the transfer
    ; routine's first check for STAT is at the beginning of HBlank
    ; when the system is in double speed mode with no sprites
    ldh a, [rIE]
    push af
    set IF_LCD_STAT, a ; enable STAT interrupts
    ldh [rIE], a
    ld a, %00100000 ; mode 2 interrupt
    ldh [rSTAT], a
    xor a
    ldh [rIF], a ; clear all interrupt flags so halt can exit properly
    ; I want to put this code after halt but at the time of writing
    ; there's no way to force `ld [nn], a` without manually adding
    ; -L flag to the assembler
    ld [HHDMA_Status], a
    halt
    ld a, 17 ; 8
.wait
    dec a ; 4
    jr nz, .wait ; 12
    nop ; 4
                 ; = (17 * 16 - 4) + 12 = 280
    xor a ; 4
    ; turn off timer
    ldh [rTAC], a ; 12
    ldh [rDIV], a ; 12
    ; assuming double speed mode, which is two times faster than PPU clock
    ld a, 256 - (CYCLES_PER_LINE * 2 / 16) ; 8
    ldh [rTMA], a ; 12
    ldh [rTIMA], a ; 12
    pop af ; 12 ; get original interrupts back
    ldh [rIE], a ; 12
    xor a ; 4
    ldh [rIF], a ; 12 ; clear any interrupts that happened
    ld a, TAC_ON | TAC_262144_HZ ; 8
    ldh [rDIV], a ; 12 ; it's time
                  ; = 120 + 280 = 400
    ldh [rTAC], a
    ret

HHDMA_NoCallback::
    call HHDMA_Wait
    ld hl, HHDMA_Status
    res 1, [hl]
    ret

HHDMA_SetCallback::
    ; set a callback function after the transfer is finished to hl
    ; this will be called inside the interrupt handler
    push hl
    call HHDMA_Wait
    pop hl
    ld a, l
    ld [HHDMA_CallbackAddress], a
    ld a, h
    ld [HHDMA_CallbackAddress+1], a
    ld hl, HHDMA_Status
    set 1, [hl]
    ret

HHDMA_Transfer::
    ; start HHDMA transfer from hl to de for b*16 bytes
    ; at a rate of c*16 bytes per line
    ; hl and de must be 16 bytes aligned per HDMA's requirements
    ld a, h
    ldh [rHDMA1], a
    ld a, l
    ldh [rHDMA2], a
    ld a, d
    ldh [rHDMA3], a
    ld a, e
    ldh [rHDMA4], a
    ld a, c
    ldh [HHDMA_PerLine], a
    ld a, b
    and a
    jr z, .remain ; interpret 0 as 256 tiles instead
    cp c
    jr nc, .remain
.last
    ld c, a
.remain
    sub c
    ldh [HHDMA_Count], a
    ld a, c
    dec a ; adjust for HDMA5's length - 1
    ldh [HHDMA_NextTransfer], a
    di
    ; set status to active
    ld hl, HHDMA_Status
    set 0, [hl]
    ; enable timer interrupt
    ld hl, rIE
    set IF_TIMER, [hl]
    ; clear stuck flag
    ld hl, rIF
    res IF_TIMER, [hl]
    ei
    ret

HHDMA_Wait::
    ld hl, HHDMA_Status
.wait
    bit 0, [hl]
    ret z
    ; halt
    jr .wait

HHDMA_Interrupt:
            ; 20 + 16 ; IRQ transfer, jp in header
    push af ; 16
    push hl ; 16
    ldh a, [HHDMA_NextTransfer] ; 12
    ld hl, rSTAT ; 12
.wait
    bit 1, [hl] ; 12 ; V/HBlank?
                ; = 104
    jr nz, .wait
    ldh [rHDMA5], a ; transfer
    ldh a, [HHDMA_PerLine] ; 12
    ld l, a ; 4
    ldh a, [HHDMA_Count] ; 12
    and a ; 4
    jr z, .finish ; 8
    cp l ; 4
    jr nc, .remain ; 8
.last
    ld l, a ; 4
.remain
    sub l ; 4
    ldh [HHDMA_Count], a ; 12
    ld a, l ; 4
    dec a ; 4
    ldh [HHDMA_NextTransfer], a ; 12
.finish_done
    pop hl ; 12
    pop af ; 12
    reti ; 16
         ; = 132

.finish
    ld hl, rIE
    res IF_TIMER, [hl] ; no more timer interrupts
    ld hl, HHDMA_Status
    res 0, [hl] ; done
    bit 1, [hl] ; has callback?
    jr z, .finish_done
    push bc
    push de
    ; let interrupts happen just in case the callback function needs it
    ; especially those that calls HHDMA_Transfer again
    ei
    call HHDMA_CallbackAddress - 1
    pop de
    pop bc
    jr .finish_done

.callback
    jp 0
.end

HHDMA_Interrupt_VBlank::
    ; called from vblank handler to take advantage of a bigger window
    ; since it enable interrupts for callback, this should be called
    ; last among timing-sensitive codes
    ; modifies af, bc, hl
    ; c = maximum tiles (max 128 due to rHDMA5 limitaion)
    ld a, [HHDMA_Status]
    rra ; bit 0 -> carry
    ret nc ; not running

    dec c ; turn into count - 1
    ldh a, [HHDMA_NextTransfer]
    ld b, a
    ldh a, [HHDMA_Count]
    add b ; should hold actual remaining - 1
    ld l, a
    cp c
    jr c, .less
    ld a, c
.less
    ldh [rHDMA5], a ; transfer
    ldh a, [HHDMA_PerLine]
    ld b, a
    ld a, l
    sub c
    jr c, .finish
    jr z, .finish
    cp b
    jr nc, .remain
.last
    ld b, a
.remain
    sub b
    ldh [HHDMA_Count], a
    ld a, b
    dec a
    ldh [HHDMA_NextTransfer], a
    ld hl, rIF
    ; avoid HHDMA firing right after enabling interrupts and miss the timing
    res IF_TIMER, [hl]
    ret

.finish
    ld hl, rIE
    res IF_TIMER, [hl] ; no more timer interrupts
    ld hl, rIF
    res IF_TIMER, [hl]
    ld hl, HHDMA_Status
    res 0, [hl] ; done
    bit 1, [hl] ; has callback?
    ret z
    push de
    ei
    call HHDMA_CallbackAddress - 1
    pop de
    ret
