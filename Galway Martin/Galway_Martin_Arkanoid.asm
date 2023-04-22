; Arkanoid reverse enginnering source
; all copyright for this code remains to Martin Galway

; Reverse Engineered by Stefano Tognon (SIDIn Magazine #4)
; Converted to ACME by dmx87

; The classic loader tune was a conversion from Martin's previous work on
; Cobra for the ZX Spectrum
; https://www.youtube.com/watch?v=2lacCH2h70w

; One of the first, if not the first, game music featuring digitized samples.

    !to "Martin_Galway_Arkanoid.sid",plain


    * = 0

    !text "RSID"
    !wo $0200 ; version 2
    !wo $7C00 ; data offset
    !wo $0000 ; load address in cbm format
    !by $08
    !by $01
    !by $0000
    !by $0000
    !wo $1400 ; 20 song
    !wo $0100 ; default song 1
    !wo $0000
    !wo $0000
    !text "Arkanoid",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    !text "Martin Galway",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    !text "1986 Imagine",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    !wo $0000
    !wo $0000
    !wo $0000

    !warn "data",*

    !word $0801

!pseudopc $0801 {

    * = $0801



; 31..+7 low addr into stack voice 1
; 39..+7 high addr into stack voice 1
; 40..+7 number of repeat voice 1
; A9..+7 low addr into stack voice 2
; B1..+7 high addr into stack voice 2
; B9..+7 number of repeat voice 2
; 61..+7 low addr into stack voice 3
; 69..+7 high addr into stack voice 3
; 71..+7 number of repeat voice 3
; 78..+7 low addr into stack sample
; 80..+7 high addr into stack sample
; 89..+7 number of repeat sample
; DA low addr sample pattern index
; DB high addr sample pattern index
; DC duration of this sample
; DD stack index for sample
; DE
; DF sample generator routine index
; E0 low pattern addr. voice 1
; E1 high pattern addr. voice 1
; E2 low pattern addr. voice 2
; E3 high pattern addr. voice 2
; E4 low pattern addr. voice 3
; E5 high pattern addr. voice 3
; E6 actual note duration (length) voice 1
; E7 actual note duration (length) voice 2
; E8 actual note duration (length) voice 3
; E9 stack index for voice 1
; EA stack index for voice 2
; EB stack index for voice 3
; EC halftone to add to current note voice 1
; ED halftone to add to current note voice 2
; EE halftone to add to current note voice 3
; EF
; F0
; F1 wave low v1
; F2 wave high v1
; F3 wave low v2
; F4 wave high v2
; F5 wave low v3
; F6 wave high v3
; F7 freq. low voice 1
; F8 freq. high voice 1
; F9 freq. low voice 2
; FA freq. high voice 2
; FB freq. low voice 3
; FC freq. high voice 3
; FD tmp track offset / low address of table
; FE high address of table
; FF current note to play
; pattern format:
; xx nn:
; xx=00..5F play note xx for time from table(nn)
; xx=60..BF play note xx-60h for time nn
; C0 RTS instr.
; C2 lo hi JSR instr.
; C4 lo hi JMP instr.
; C6 ht lo hi JSRT instr. (1,3)
; CA id va SET instr.
; CC nn FOR instr.
; CE NEXT instr.
; D2 nn lo hi SETNI instr.
; D4 lo hi INSTR instr.
; D6 nn va SETCI instr.
; D8 lo hi EXCT instr. (3)
; DC id v1 v2 SET2I instr. (2,3)
; DE id v1 v2 SET2CI instr.
; E0 LF3 instr. (3)
; E2 lo hi FILTA instr. (3)
; F0 lo hi SETFI instr.
; sample pattern format:

; 81 xx : Sample 1
; 82 xx : Sample 2
; 83 xx : Sample 3
; 84 xx : Sample 4
; 85 xx : Sample 5
; 86 xx : Sample 6
; 87 xx : Nothing
; 20 lo hi : JSR yyxx
; 40 : NEXT
; 60 : RTS
; 49 xx : FOR
; 4C lo hi : JMP yyxx

TEMP = $5FFF ; 3FFF in the original code
    sta $1FFF
    sei
    LDA #$35
    STA $01 ; 6510 I/O register
;JSR $1FC0
    lda #$00
    sta $DC0E ; Control register A of CIA #1
    lda #<IRQ
    sta $FFFE ; Masckerable Interrupt (IRQ) vector
    lda #>IRQ
    sta $FFFF
    jsr initEngine
    jsr initSample
    ldx $1FFF
    cpx #$02
    bcs notSample
    lda #$F0
    sta $DC04 ; Timer A #1: Lo Byte
    lda #$49
    sta $DC05 ; Timer A #1: Hi Byte
    cpx #$00
    bne isTune2
    lda #$02 ; sample duration
    ldx #<Sample_Tune1 ; low address
    ldy #>Sample_Tune1 ; high address
    jsr setSample
    jmp nextP
isTune2:
    lda #$02 ; sample duration
    ldx #<Sample_Tune2 ; low address
    ldy #>Sample_Tune2 ; high address
    jsr setSample
    jmp nextP

notSample:
    lda #$F8
    sta $DC04 ; Timer A #1: Lo Byte
    lda #$24
    sta $DC05 ; Timer A #1: Hi Byte

nextP:
    ldx $1FFF
    cpx #$08
    bcc normalTune
    jmp calcAddress ; calcolate and set address for effects

normalTune:
    ldy offsetTracks,x
    jsr setTracks

setInterrupt:
    lda #$81
    sta $DC0D ; Interrupt control register CIA #1
    lda #$01
    sta $DC0E ; Control register A of CIA #1
    cli
    lda $1FFF ; tune to play
    cmp #$02 ; first two tunes use sample
    bcc SLoop
    rts

SLoop:
    jsr SampleGeneration
    jmp SLoop
offsetTracks:
    !by $3D, $28, $05, $0C, $13, $21, $2F, $36
    !by $36, $44, $4B, $52, $59, $60, $67, $6E

IRQ:
    pha
    tya
    pha
    txa
    pha
    lda $1FFF ; number of tune to play
    cmp #$02
    bcs skipSample
    jsr playSample ; play sample for the first 2 tunes
skipSample:
    jsr makeFilterEff
    jsr executePatternV1
    jsr executePatternV2
    jsr executePatternV3
    jsr makeTimbreV1 ; voice 1
    jsr makeTimbreV2 ; voice 2
    jsr makeTimbreV3 ; voice 3
    lda $DC0D ; Interrupt control register CIA #1
    pla
    tax
    pla
    tay
    pla
    rti

    * = $2000

!by $3A, $20 ; freq low/high add 1
!by $00, $00 ; freq low/high add 2
!by $C6, $DF ; freq low/high add 3
!by $00, $00 ; freq low/high add 4
!by $01, $02 ; freq (to reload) cycle 1/2
!by $01, $01 ; freq (to reload) cycle 3/4
!by $01 ; freq initial delay
!by $05 ; freq effect flag
!by $00, $00 ; wave (to reload) cycle 1/2
!by $00 ; wave initial delay
!by $00 ; wave effect flag
!by $00, $00 ; wave low/high add 1
!by $00, $00 ; wave low/high add 2
!by $00, $00 ; wave low/high
!by $21 ; control
!by $03, $8A ; AD/SR
!by $0F, $5A
!by $3A, $20 ; freq. low/high

    * = $201f

;    * = $201F
    !by $C6, $DF ; freq low/high add 1
    !by $00, $00 ; freq low/high add 2
    !by $3A, $20 ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $01, $01 ; freq (to reload) cycle 1/2
    !by $01, $02 ; freq (to reload) cycle 3/4
    !by $01 ; freq initial delay
    !by $05 ; freq effect flag
    !by $00, $00 ; wave (to reload) cycle 1/2
    !by $00 ; wave initial delay
    !by $00 ; wave effect flag
    !by $00, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $00 ; wave low/high
    !by $21 ; control
    !by $03, $8A ; AD/SR
    !by $0F, $5A
    !by $D8, $40 ; freq. low/high

    * = $203E

    !by $9E, $20 ; freq low/high add 1
    !by $00, $00 ; freq low/high add 2
    !by $62, $DF ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $01, $01 ; freq (to reload) cycle 1/2
    !by $01, $01 ; freq (to reload) cycle 3/4
    !by $02 ; freq initial delay
    !by $05 ; freq effect flag
    !by $00, $00 ; wave (to reload) cycle 1/2
    !by $00 ; wave initial delay
    !by $00 ; wave effect flag
    !by $00, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $00 ; wave low/high
    !by $21 ; control
    !by $03, $8A ; AD/SR
    !by $0F, $5A
    !by $3C, $41 ; freq. low/high

    * = $205D

    !by $1F, $18 ; freq low/high add 1
    !by $00, $00 ; freq low/high add 2
    !by $E1, $E7 ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $01, $02 ; freq (to reload) cycle 1/2
    !by $01, $01 ; freq (to reload) cycle 3/4
    !by $01 ; freq initial delay
    !by $05 ; freq effect flag
    !by $00, $00 ; wave (to reload) cycle 1/2
    !by $00 ; wave initial delay
    !by $00 ; wave effect flag
    !by $00, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $00 ; wave low/high
    !by $21 ; control
    !by $03, $8A ; AD/SR
    !by $0F, $5A
    !by $3E, $30 ; freq. low/high

    * = $207C

    !by $5E, $CF ; freq low/high add 1
    !by $00, $00 ; freq low/high add 2
    !by $A2, $30 ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $01, $01 ; freq (to reload) cycle 1/2
    !by $01, $02 ; freq (to reload) cycle 3/4
    !by $01 ; freq initial delay
    !by $05 ; freq effect flag
    !by $00, $00 ; wave (to reload) cycle 1/2
    !by $00 ; wave initial delay
    !by $00 ; wave effect flag
    !by $00, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $00 ; wave low/high
    !by $21 ; control
    !by $03, $8A ; AD/SR
    !by $0F, $5A
    !by $44, $61 ; freq. low/high

    * = $209B

    !by $06, $31 ; freq low/high add 1
    !by $00, $00 ; freq low/high add 2
    !by $FA, $CE ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $01, $01 ; freq (to reload) cycle 1/2
    !by $01, $01 ; freq (to reload) cycle 3/4
    !by $02 ; freq initial delay
    !by $05 ; freq effect flag
    !by $00, $00 ; wave (to reload) cycle 1/2
    !by $00 ; wave initial delay
    !by $00 ; wave effect flag
    !by $00, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $00 ; wave low/high
    !by $21 ; control
    !by $03, $8A ; AD/SR
    !by $0F, $5A
    !by $0C, $62 ; freq. low/high

    * = $20BA

    !by $04, $00 ; freq low/high add 1
    !by $00, $00 ; freq low/high add 2
    !by $00, $00 ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $FF, $00 ; freq (to reload) cycle 1/2
    !by $00, $00 ; freq (to reload) cycle 3/4
    !by $00 ; freq initial delay
    !by $04 ; freq effect flag
    !by $00, $00 ; wave (to reload) cycle 1/2
    !by $00 ; wave initial delay
    !by $00 ; wave effect flag
    !by $00, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $08 ; wave low/high
    !by $41 ; control
    !by $00, $F0 ; AD/SR
    !by $32, $01
    !by $20, $03 ; freq. low/high

    * = $20D9

    !by $04, $00 ; freq low/high add 1
    !by $00, $00 ; freq low/high add 2
    !by $00, $00 ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $FF, $00 ; freq (to reload) cycle 1/2
    !by $00, $00 ; freq (to reload) cycle 3/4
    !by $00 ; freq initial delay
    !by $04 ; freq effect flag
    !by $00, $00 ; wave (to reload) cycle 1/2
    !by $00 ; wave initial delay
    !by $00 ; wave effect flag
    !by $00, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $08 ; wave low/high
    !by $41 ; control
    !by $00, $F0 ; AD/SR
    !by $32, $01
    !by $2A, $03 ; freq. low/high

    * = $20F8

    !by $04, $00 ; freq low/high add 1
    !by $00, $00 ; freq low/high add 2
    !by $00, $00 ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $FF, $00 ; freq (to reload) cycle 1/2
    !by $00, $00 ; freq (to reload) cycle 3/4
    !by $00 ; freq initial delay
    !by $04 ; freq effect flag
    !by $00, $00 ; wave (to reload) cycle 1/2
    !by $00 ; wave initial delay
    !by $00 ; wave effect flag
    !by $00, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $08 ; wave low/high
    !by $41 ; control
    !by $00, $F0 ; AD/SR
    !by $32, $01
    !by $34, $03 ; freq. low/high

    * = $2117

    !by $F0, $D8 ; freq low/high add 1
    !by $18, $FC ; freq low/high add 2
    !by $9C, $FF ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $04, $05 ; freq (to reload) cycle 1/2
    !by $0A, $00 ; freq (to reload) cycle 3/4
    !by $00 ; freq initial delay
    !by $85 ; freq effect flag
    !by $00, $00 ; wave (to reload) cycle 1/2
    !by $00 ; wave initial delay
    !by $00 ; wave effect flag
    !by $00, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $08 ; wave low/high
    !by $41 ; control
    !by $09, $B9 ; AD/SR
    !by $1E, $32
    !by $50, $C3 ; freq. low/high

    * = $2136

    !by $F0, $D8 ; freq low/high add 1
    !by $18, $FC ; freq low/high add 2
    !by $9C, $FF ; freq low/high add 3
    !by $7C, $15 ; freq low/high add 4
    !by $04, $05 ; freq (to reload) cycle 1/2
    !by $0A, $00 ; freq (to reload) cycle 3/4
    !by $0A ; freq initial delay
    !by $87 ; freq effect flag
    !by $00, $00 ; wave (to reload) cycle 1/2
    !by $00 ; wave initial delay
    !by $00 ; wave effect flag
    !by $00, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $08 ; wave low/high
    !by $43 ; control
    !by $09, $B9 ; AD/SR
    !by $1E, $32
    !by $01, $00 ; freq. low/high

    * = $2155

    !by $F0, $D8 ; freq low/high add 1
    !by $18, $FC ; freq low/high add 2
    !by $9C, $FF ; freq low/high add 3
    !by $B8, $0B ; freq low/high add 4
    !by $04, $05 ; freq (to reload) cycle 1/2
    !by $0A, $00 ; freq (to reload) cycle 3/4
    !by $14 ; freq initial delay
    !by $87 ; freq effect flag
    !by $00, $00 ; wave (to reload) cycle 1/2
    !by $00 ; wave initial delay
    !by $00 ; wave effect flag
    !by $00, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $08 ; wave low/high
    !by $41 ; control
    !by $09, $B9 ; AD/SR
    !by $1E, $32
    !by $01, $00 ; freq. low/high

    * = $2174

    !by $74, $40 ; freq low/high add 1
    !by $00, $00 ; freq low/high add 2
    !by $8C, $BF ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $01, $02 ; freq (to reload) cycle 1/2
    !by $01, $01 ; freq (to reload) cycle 3/4
    !by $01 ; freq initial delay
    !by $05 ; freq effect flag
    !by $00, $00 ; wave (to reload) cycle 1/2
    !by $00 ; wave initial delay
    !by $00 ; wave effect flag
    !by $00, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $00 ; wave low/high
    !by $21 ; control
    !by $03, $8A ; AD/SR
    !by $0F, $5A
    !by $74, $40 ; freq. low/high

    * = $2193

    !by $8C, $BF ; freq low/high add 1
    !by $00, $00 ; freq low/high add 2
    !by $74, $40 ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $01, $01 ; freq (to reload) cycle 1/2
    !by $01, $02 ; freq (to reload) cycle 3/4
    !by $01 ; freq initial delay
    !by $05 ; freq effect flag
    !by $00, $00 ; wave (to reload) cycle 1/2
    !by $00 ; wave initial delay
    !by $00 ; wave effect flag
    !by $00, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $00 ; wave low/high
    !by $21 ; control
    !by $03, $8A ; AD/SR
    !by $0F, $5A
    !by $D8, $40 ; freq. low/high

    * = $21B2

    !by $3C, $41 ; freq low/high add 1
    !by $00, $00 ; freq low/high add 2
    !by $C4, $BE ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $01, $01 ; freq (to reload) cycle 1/2
    !by $01, $01 ; freq (to reload) cycle 3/4
    !by $02 ; freq initial delay
    !by $05 ; freq effect flag
    !by $00, $00 ; wave (to reload) cycle 1/2
    !by $00 ; wave initial delay
    !by $00 ; wave effect flag
    !by $00, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $00 ; wave low/high
    !by $21 ; control
    !by $03, $8A ; AD/SR
    !by $0F, $5A
    !by $78, $82 ; freq. low/high

    * = $21D1

    !by $E2, $04 ; freq low/high add 1
    !by $38, $CD ; freq low/high add 2
    !by $00, $00 ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $09, $01 ; freq (to reload) cycle 1/2
    !by $00, $00 ; freq (to reload) cycle 3/4
    !by $00 ; freq initial delay
    !by $05 ; freq effect flag
    !by $00, $00 ; wave (to reload) cycle 1/2
    !by $00 ; wave initial delay
    !by $00 ; wave effect flag
    !by $00, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $00 ; wave low/high
    !by $21 ; control
    !by $15, $9B ; AD/SR
    !by $14, $46
    !by $3A, $20 ; freq. low/high

    * = $21F0

    !by $36, $F7 ; freq low/high add 1
    !by $38, $4A ; freq low/high add 2
    !by $00, $00 ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $09, $01 ; freq (to reload) cycle 1/2
    !by $01, $00 ; freq (to reload) cycle 3/4
    !by $00 ; freq initial delay
    !by $05 ; freq effect flag
    !by $00, $00 ; wave (to reload) cycle 1/2
    !by $00 ; wave initial delay
    !by $00 ; wave effect flag
    !by $00, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $00 ; wave low/high
    !by $21 ; control
    !by $15, $9B ; AD/SR
    !by $14, $46
    !by $D8, $40 ; freq. low/high

    * = $220F
    !by $16, $0D ; freq low/high add 1
    !by $88, $96 ; freq low/high add 2
    !by $00, $00 ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $07, $01 ; freq (to reload) cycle 1/2
    !by $00, $00 ; freq (to reload) cycle 3/4
    !by $00 ; freq initial delay
    !by $05 ; freq effect flag
    !by $00, $00 ; wave (to reload) cycle 1/2
    !by $00 ; wave initial delay
    !by $00 ; wave effect flag
    !by $00, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $00 ; wave low/high
    !by $21 ; control
    !by $15, $9B ; AD/SR
    !by $14, $46
    !by $3C, $41 ; freq. low/high
    * = $222E
    !by $90, $E8 ; freq low/high add 1
    !by $DD, $FF ; freq low/high add 2
    !by $0A, $00 ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $0A, $50 ; freq (to reload) cycle 1/2
    !by $FF, $00 ; freq (to reload) cycle 3/4
    !by $00 ; freq initial delay
    !by $04 ; freq effect flag
    !by $FF, $00 ; wave (to reload) cycle 1/2
    !by $00 ; wave initial delay
    !by $00 ; wave effect flag
    !by $08, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $08 ; wave low/high
    !by $41 ; control
    !by $19, $BB ; AD/SR
    !by $28, $C8
    !by $0C, $F8 ; freq. low/high
    * = $224D
    !by $90, $E8 ; freq low/high add 1
    !by $DD, $FF ; freq low/high add 2
    !by $0A, $00 ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $0A, $50 ; freq (to reload) cycle 1/2
    !by $FF, $00 ; freq (to reload) cycle 3/4
    !by $04 ; freq initial delay
    !by $04 ; freq effect flag
    !by $FF, $00 ; wave (to reload) cycle 1/2
    !by $00 ; wave initial delay
    !by $00 ; wave effect flag
    !by $08, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $08 ; wave low/high
    !by $41 ; control
    !by $19, $BB ; AD/SR
    !by $28, $C8
    !by $00, $FA ; freq. low/high

    * = $226C

    !by $90, $E8 ; freq low/high add 1
    !by $DD, $FF ; freq low/high add 2
    !by $0A, $00 ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $0A, $50 ; freq (to reload) cycle 1/2
    !by $FF, $00 ; freq (to reload) cycle 3/4
    !by $08 ; freq initial delay
    !by $04 ; freq effect flag
    !by $FF, $00 ; wave (to reload) cycle 1/2
    !by $00 ; wave initial delay
    !by $00 ; wave effect flag
    !by $08, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $08 ; wave low/high
    !by $41 ; control
    !by $19, $BB ; AD/SR
    !by $28, $C8
    !by $F4, $FB ; freq. low/high

    * = $228B

    !by $3A, $20 ; freq low/high add 1
    !by $00, $00 ; freq low/high add 2
    !by $C6, $DF ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $01, $02 ; freq (to reload) cycle 1/2
    !by $01, $01 ; freq (to reload) cycle 3/4
    !by $01 ; freq initial delay
    !by $05 ; freq effect flag
    !by $00, $00 ; wave (to reload) cycle 1/2
    !by $00 ; wave initial delay
    !by $00 ; wave effect flag
    !by $00, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $00 ; wave low/high
    !by $21 ; control
    !by $02, $A6 ; AD/SR
    !by $05, $08
    !by $3A, $20 ; freq. low/high

    * = $22AA
    !by $C6, $DF ; freq low/high add 1
    !by $00, $00 ; freq low/high add 2
    !by $3A, $20 ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $01, $01 ; freq (to reload) cycle 1/2
    !by $01, $02 ; freq (to reload) cycle 3/4
    !by $01 ; freq initial delay
    !by $05 ; freq effect flag
    !by $00, $00 ; wave (to reload) cycle 1/2
    !by $00 ; wave initial delay
    !by $00 ; wave effect flag
    !by $00, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $00 ; wave low/high
    !by $21 ; control
    !by $02, $A6 ; AD/SR
    !by $05, $08
    !by $D8, $40 ; freq. low/high

    * = $22C9
    !by $9E, $20 ; freq low/high add 1
    !by $00, $00 ; freq low/high add 2
    !by $62, $DF ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $01, $01 ; freq (to reload) cycle 1/2
    !by $01, $01 ; freq (to reload) cycle 3/4
    !by $02 ; freq initial delay
    !by $05 ; freq effect flag
    !by $00, $00 ; wave (to reload) cycle 1/2
    !by $00 ; wave initial delay
    !by $00 ; wave effect flag
    !by $00, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $00 ; wave low/high
    !by $21 ; control
    !by $02, $A6 ; AD/SR
    !by $05, $08
    !by $3C, $41 ; freq. low/high

    * = $22E8
    !by $E2, $04 ; freq low/high add 1
    !by $08, $D5 ; freq low/high add 2
    !by $00, $00 ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $0A, $01 ; freq (to reload) cycle 1/2
    !by $00, $00 ; freq (to reload) cycle 3/4
    !by $01 ; freq initial delay
    !by $05 ; freq effect flag
    !by $00, $00 ; wave (to reload) cycle 1/2
    !by $00 ; wave initial delay
    !by $00 ; wave effect flag
    !by $00, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $00 ; wave low/high
    !by $21 ; control
    !by $15, $9B ; AD/SR
    !by $14, $8C
    !by $3A, $20 ; freq. low/high

    * = $2307
    !by $36, $F7 ; freq low/high add 1
    !by $08, $52 ; freq low/high add 2
    !by $00, $00 ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $09, $01 ; freq (to reload) cycle 1/2
    !by $00, $00 ; freq (to reload) cycle 3/4
    !by $02 ; freq initial delay
    !by $05 ; freq effect flag
    !by $00, $00 ; wave (to reload) cycle 1/2
    !by $00 ; wave initial delay
    !by $00 ; wave effect flag
    !by $00, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $00 ; wave low/high
    !by $21 ; control
    !by $15, $9B ; AD/SR
    !by $14, $8C
    !by $D8, $40 ; freq. low/high

    * = $2326
    !by $16, $0D ; freq low/high add 1
    !by $70, $9A ; freq low/high add 2
    !by $00, $00 ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $08, $01 ; freq (to reload) cycle 1/2
    !by $00, $00 ; freq (to reload) cycle 3/4
    !by $03 ; freq initial delay
    !by $05 ; freq effect flag
    !by $00, $00 ; wave (to reload) cycle 1/2
    !by $00 ; wave initial delay
    !by $00 ; wave effect flag
    !by $00, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $00 ; wave low/high
    !by $21 ; control
    !by $15, $9B ; AD/SR
    !by $14, $8C
    !by $3C, $41 ; freq. low/high

    * = $2345

    !by $90, $E8 ; freq low/high add 1
    !by $CE, $FF ; freq low/high add 2
    !by $30, $F2 ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $0A, $28 ; freq (to reload) cycle 1/2
    !by $01, $00 ; freq (to reload) cycle 3/4
    !by $00 ; freq initial delay
    !by $05 ; freq effect flag
    !by $FF, $00 ; wave (to reload) cycle 1/2
    !by $00 ; wave initial delay

    !by $00 ; wave effect flag
    !by $08, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $08 ; wave low/high
    !by $41 ; control
    !by $19, $BB ; AD/SR
    !by $28, $8C
    !by $31, $F2 ; freq. low/high

    * = $2364

    !by $90, $E8 ; freq low/high add 1
    !by $CE, $FF ; freq low/high add 2
    !by $30, $F2 ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $0A, $28 ; freq (to reload) cycle 1/2
    !by $01, $00 ; freq (to reload) cycle 3/4
    !by $02 ; freq initial delay
    !by $05 ; freq effect flag
    !by $FF, $00 ; wave (to reload) cycle 1/2
    !by $00 ; wave initial delay
    !by $00 ; wave effect flag
    !by $08, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $08 ; wave low/high
    !by $41 ; control
    !by $19, $BB ; AD/SR
    !by $28, $8C
    !by $19, $F6 ; freq. low/high

    * = $2383

    !by $90, $E8 ; freq low/high add 1
    !by $CE, $FF ; freq low/high add 2
    !by $30, $F2 ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $0A, $28 ; freq (to reload) cycle 1/2
    !by $01, $00 ; freq (to reload) cycle 3/4
    !by $04 ; freq initial delay
    !by $05 ; freq effect flag
    !by $FF, $00 ; wave (to reload) cycle 1/2
    !by $00 ; wave initial delay
    !by $00 ; wave effect flag
    !by $08, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $08 ; wave low/high
    !by $41 ; control
    !by $19, $BB ; AD/SR
    !by $28, $8C
    !by $01, $FA ; freq. low/high

    * = $23A2
    !by $E2, $04 ; freq low/high add 1
    !by $38, $CD ; freq low/high add 2
    !by $00, $00 ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $09, $02 ; freq (to reload) cycle 1/2
    !by $01, $00 ; freq (to reload) cycle 3/4
    !by $00 ; freq initial delay
    !by $05 ; freq effect flag
    !by $00, $00 ; wave (to reload) cycle 1/2
    !by $00 ; wave initial delay
    !by $00 ; wave effect flag
    !by $00, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $00 ; wave low/high
    !by $15 ; control
    !by $15, $E9 ; AD/SR
    !by $0A, $5A
    !by $3A, $20 ; freq. low/high
    * = $23C1
    !by $36, $F7 ; freq low/high add 1
    !by $38, $4A ; freq low/high add 2
    !by $00, $00 ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $01, $02 ; freq (to reload) cycle 1/2
    !by $09, $00 ; freq (to reload) cycle 3/4
    !by $05 ; freq initial delay
    !by $05 ; freq effect flag
    !by $00, $00 ; wave (to reload) cycle 1/2
    !by $00 ; wave initial delay
    !by $00 ; wave effect flag
    !by $00, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $00 ; wave low/high
    !by $15 ; control
    !by $15, $99 ; AD/SR
    !by $0A, $5A
    !by $D8, $40 ; freq. low/high
    * = $23E0
    !by $16, $0D ; freq low/high add 1
    !by $88, $96 ; freq low/high add 2
    !by $00, $00 ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $02, $07 ; freq (to reload) cycle 1/2
    !by $01, $00 ; freq (to reload) cycle 3/4
    !by $00 ; freq initial delay
    !by $05 ; freq effect flag
    !by $00, $00 ; wave (to reload) cycle 1/2
    !by $00 ; wave initial delay
    !by $00 ; wave effect flag
    !by $00, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $00 ; wave low/high
    !by $15 ; control
    !by $15, $99 ; AD/SR
    !by $0A, $5A
    !by $3C, $41 ; freq. low/high
    * = $23FF
    !by $1E, $FB ; freq low/high add 1
    !by $F8, $2A ; freq low/high add 2
    !by $E8, $03 ; freq low/high add 3
    !by $08, $D5 ; freq low/high add 4
    !by $0A, $01 ; freq (to reload) cycle 1/2
    !by $0A, $01 ; freq (to reload) cycle 3/4
    !by $01 ; freq initial delay
    !by $05 ; freq effect flag
    !by $00, $00 ; wave (to reload) cycle 1/2
    !by $00 ; wave initial delay
    !by $00 ; wave effect flag
    !by $00, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $08 ; wave low/high
    !by $41 ; control
    !by $CC, $FC ; AD/SR
    !by $FE, $FE
    !by $10, $27 ; freq. low/high
    * = $241E
    !by $CA, $08 ; freq low/high add 1
    !by $F8, $AD ; freq low/high add 2
    !by $00, $00 ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $09, $01 ; freq (to reload) cycle 1/2
    !by $00, $00 ; freq (to reload) cycle 3/4
    !by $02 ; freq initial delay
    !by $05 ; freq effect flag
    !by $00, $00 ; wave (to reload) cycle 1/2
    !by $00 ; wave initial delay
    !by $00 ; wave effect flag
    !by $00, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $08 ; wave low/high
    !by $41 ; control
    !by $CC, $FC ; AD/SR
    !by $FE, $FE
    !by $CE, $56 ; freq. low/high
    * = $243D
    !by $EA, $F2 ; freq low/high add 1
    !by $90, $65 ; freq low/high add 2
    !by $00, $00 ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $08, $01 ; freq (to reload) cycle 1/2
    !by $00, $00 ; freq (to reload) cycle 3/4
    !by $03 ; freq initial delay
    !by $05 ; freq effect flag
    !by $00, $00 ; wave (to reload) cycle 1/2
    !by $00 ; wave initial delay
    !by $00 ; wave effect flag
    !by $00, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $08 ; wave low/high
    !by $41 ; control
    !by $CC, $FC ; AD/SR
    !by $FE, $FE
    !by $07, $87 ; freq. low/high

;=================================
; Instrument table voice 1
;=================================
InstTableV1:
    !by $23 ; 00h: freq low add 1 voice 1
    !by $00 ; 01h: freq high add 1 voice 1
    !by $DD ; 02h: freq low add 2 voice 1
    !by $FF ; 03h: freq high add 2 voice 1
    !by $23 ; 04h: freq low add 3 voice 1
    !by $00 ; 05h: freq high add 3 voice 1
    !by $00 ; 06h: freq low add 4 voice 1
    !by $00 ; 07h: freq high add 4 voice 1
    !by $03 ; 08h: freq cycle 1 voice 1
    !by $05 ; 09h: freq cycle 2 voice 1
    !by $02 ; 0Ah: freq cycle 3 voice 1
    !by $00 ; 0Bh: freq cycle 4 voice 1
    !by $0A ; 0Ch: freq delay initial voice 1
    !by $05 ; 0Dh: freq effect flag voice 1
    !by $00 ; 0Eh: wave cycle 1 voice 1
    !by $00 ; 0Fh: wave cycle 2 voice 1
    !by $00 ; 10h: wave delay initial voice 1
    !by $00 ; 11h: wave effect flag voice 1
    !by $00 ; 12h: wave low add 1 voice 1
    !by $00 ; 13h: wave high add 1 voice 1
    !by $00 ; 14h: wave low add 2 voice 1
    !by $00 ; 15h: wave high add 2 voice 1
    !by $00 ; 16h: wave low voice 1
    !by $00 ; 17h: wave high voice 1
    !by $19 ; 18h: control of voice 1
    !by $A4 ; 19h: AD voice 1
    !by $F9 ; 1Ah: SR voice 1
    !by $14 ; 1Bh
    !by $FE ; 1Ch
;=================================
; Instrument table voice 2
;=================================
InstTableV2:
    !by $00 ; 00h: freq low add 1 voice 2
    !by $00 ; 01h: freq high add 1 voice 2
    !by $00 ; 02h: freq low add 2 voice 2
    !by $00 ; 03h: freq high add 2 voice 2
    !by $01 ; 04h: freq low add 3 voice 2
    !by $00 ; 05h: freq high add 3 voice 2
    !by $01 ; 06h: freq low add 4 voice 2
    !by $00 ; 07h: freq high add 4 voice 2
    !by $13 ; 08h: freq cycle 1 voice 2
    !by $35 ; 09h: freq cycle 2 voice 2
    !by $00 ; 0Ah: freq cycle 3 voice 2
    !by $03 ; 0Bh: freq cycle 4 voice 2
    !by $00 ; 0Ch: freq delay initial voice 2
    !by $00 ; 0Dh: freq effect flag voice 2
    !by $32 ; 0Eh: wave cycle 1 voice 2
    !by $32 ; 0Fh: wave cycle 2 voice 2
    !by $14 ; 10h: wave delay initial voice 2
    !by $00 ; 11h: wave effect flag voice 2
    !by $0A ; 12h: wave low add 1 voice 2
    !by $00 ; 13h: wave high add 1 voice 2
    !by $F6 ; 14h: wave low add 2 voice 2
    !by $FF ; 15h: wave high add 2 voice 2
    !by $00 ; 16h: wave loh voice 2
    !by $08 ; 17h: wave high voice 2
    !by $41 ; 18h: control of voice 2
    !by $01 ; 19h: AD voice 2
    !by $F7 ; 1Ah: SR voice 2
    !by $04 ; 1Bh
    !by $14 ; 1Ch
;=================================
; Instrument table voice 3
;=================================
InstTableV3:
    !by $19 ; 00h: freq low add 1 voice 3
    !by $00 ; 01h: freq high add 1 voice 3
    !by $E7 ; 02h: freq low add 2 voice 3
    !by $FF ; 03h: freq high add 2 voice 3
    !by $19 ; 04h: freq low add 3 voice 3
    !by $00 ; 05h: freq high add 3 voice 3
    !by $00 ; 06h: freq low add 4 voice 3
     !by $00 ; 07h: freq high add 4 voice 3
    !by $02 ; 08h: freq cycle 1 voice 3
    !by $04 ; 09h: freq cycle 2 voice 3
    !by $02 ; 0Ah: freq cycle 3 voice 3
    !by $00 ; 0Bh: freq cycle 4 voice 3
    !by $06 ; 0Ch: freq delay initial voice 3
    !by $05 ; 0Dh: freq effect flag voice 3
    !by $32 ; 0Eh: wave cycle 1 voice 3
    !by $32 ; 0Fh: wave cycle 2 voice 3
    !by $00 ; 10h: wave delay initial voice 1
    !by $05 ; 11h: wave effect flag voice 1
    !by $14 ; 12h: wave low add 1 voice 3
    !by $00 ; 13h: wave high add 1 voice 3
    !by $EC ; 14h: wave low add 2 voice 3
    !by $FF ; 15h: wave high add 2 voice 3
    !by $00 ; 16h: wave lo voice 3
    !by $06 ; 17h: wave hi voice 3
    !by $41 ; 18h: control of voice 3
    !by $14 ; 19h: AD voice 3
    !by $E8 ; 1Ah: SR voice 3
    !by $1E ; 1Bh
    !by $28 ; 1Ch

; main filter table
MainFilterTable:
    !by $4D, $01, $D3, $FF, $FB, $FF, $FF, $FF
    !by $03, $14, $0A, $32, $00, $04, $01, $00

; current filter table
CurFilterTable:
    !by $4D ; 0h: add filter low value 1
    !by $01 ; 1h: add filter high value 1
    !by $D3 ; 2h: add filter low value 2
    !by $FF ; 3h: add filter high value 2
    !by $FB ; 4h: add filter low value 3
    !by $FF ; 5h: add filter high value 3
    !by $FF ; 6h: add filter low value 4
    !by $FF ; 7h: add filter high value 4
    !by $03 ; 8h: filter cycle 1
    !by $14 ; 9h: filter cycle 2
    !by $0A ; Ah: filter cycle 3
    !by $32 ; Bh: filter cycle 4
    !by $00 ; Ch: filter initial delay
    !by $00 ; Dh: filter effect flag
    !by $01 ; Eh: filter low value (8 bit)
    !by $00 ; Fh: filter high value (3 bit)

ActFilterTable:
    !by $00 ; 0h: actual filter low value
    !by $00 ; 1h: actual filter high value
    !by $00 ; 2h: actual filter cycle 1
    !by $00 ; 3h: actual filter cycle 2
    !by $00 ; 4h: actual filter cycle 3
    !by $00 ; 5h: actual filter cycle 4
;=================================
;current istrument table voice 1
;=================================
CurInstTableV1:
    !by $23, $00 ; 00h: freq low/high add 1 voice 1
    !by $DD, $FF ; 02h: freq low/high add 2 voice 1
    !by $23, $00 ; 04h: freq low/high add 3 voice 1
    !by $00, $00 ; 06h: freq low/high add 4 voice 1
    !by $03, $05 ; 08h: freq (to reload) cycle 1/2 voice 1
    !by $02, $00 ; 0ah: freq (to reload) cycle 3/4 voice 1
    !by $00 ; 0ch: freq initial delay voice 1
    !by $05 ; 0dh: freq effect flag voice 1
    !by $32, $32 ; 0eh: wave (to reload) cycle 1/2 voice 1
    !by $00 ; 10h: wave initial delay voice 1
    !by $00 ; 11h: wave effect flag voice 1
    !by $0A, $00 ; 12h: wave low/high add 1 voice 1
    !by $F6, $FF ; 14h: wave low/high add 2 voice 1
    !by $00, $08 ; 16h: wave low/high voice 1
    !by $2C, $1A ; 18h: freq low/high voice 1
    !by $10 ; 1Ah: control of voice 1
    !by $00 ; 1Bh:
    !by $00 ; 1Ch:
    !by $00, $00 ; 1Dh: freq cycle 1/2 voice 1
    !by $01, $00 ; 1Fh: freq cycle 3/4 voice 1
    !by $00, $04 ; 21h: wave cycle 1/2 voice 1
;=================================
;current istrument table voice 2
;=================================
CurInstTableV2:
    !by $00, $00 ; 00h: freq low/high add 1 voice 2
    !by $00, $00 ; 02h: freq low/high add 2 voice 2
     !by $01, $00 ; 04h: freq low/high add 3 voice 2
    !by $01, $00 ; 06h: freq low/high add 4 voice 2
    !by $13, $35 ; 08h: freq (to reload) cycle 1/2 voice 2
    !by $37, $03 ; 0ah: freq (to reload) cycle 3/4 voice 2
    !by $00 ; 0ch: freq. initial delay voice 2
    !by $00 ; 0dh: freq. effect flag voice 2
    !by $32, $32 ; 0eh: wave cycle 1/2 voice 2
    !by $00 ; 10h: wave initial delay voice 2
    !by $00 ; 11h: wave effect flag voice 2
    !by $0A, $00 ; 12h: wave low/high add 1 voice 2
    !by $F6, $FF ; 14h: wave low/high add 2 voice 2
    !by $00, $08 ; 16h: wave low/high voice 2
    !by $2C, $1A ; 18h: freq low/high voice 2
    !by $41 ; 1A: control of voice 2
    !by $00 ; 1B:
    !by $00 ; 1C:
    !by $00, $00 ; 1D: freq cycle 1/2 voice 2
    !by $02, $00 ; 1F: freq cycle 3/4 voice 2
    !by $1E, $32 ; 21h: wave cycle 1/2 voice 1
;=================================
;current istrument table voice 3
;=================================
CurInstTableV3:
    !by $19, $00 ; 00h: freq low/high add 1 voice 3
    !by $E7, $FF ; 02h: freq low/high add 2 voice 3
    !by $19, $00 ; 04h: freq low/high add 3 voice 3
    !by $00, $00 ; 06h: freq low/high add 4 voice 3
    !by $02, $04 ; 08h: freq (to reload) cycle 1/2 voice 3
    !by $02, $00 ; 0ah: freq (to reload) cycle 3/4 voice 3
    !by $00 ; 0ch: freq. initial delay voice 3
    !by $05 ; 0dh: freq. effect flag voice 3
    !by $32, $32 ; 0eh: wave cycle 1/2 voice 3
    !by $00 ; 10h: wave initial delay voice 3
    !by $05 ; 11h: wave effect flag voice 3
    !by $14, $00 ; 12h: wave low/high add 1 voice 3
    !by $EC, $FF ; 14h: wave low/high add 2 voice 3
    !by $00, $06 ; 16h: wave low/high voice 3
    !by $B0, $0E ; 18h: freq low/high voice 3
    !by $41 ; 1Ah: control of voice 3
    !by $00 ; 1Bh:
    !by $00 ; 1Ch:
    !by $00, $00 ; 1D: freq cycle 1/2 voice 2
    !by $01, $00 ; 1F: freq cycle 3/4 voice 2
    !by $00, $1F ; 21h: wave cycle 1/2 voice 1
voiceNumber:
    !by $00 ; voice number (0,1,2) on where apply filter
tmpCutF:
    !by $00
minDuration:
    !by $04
; Play music flag: 0=no music, 1=music
playMusicV1:
    !by $00
playMusicV2:
    !by $00
playMusicV3:
    !by $01
tmpVIndex:
    !by $FF ; tmp voices index
tmpAVIndex:
    !by $FE ; tmp address index voice
durationTable:
    !by $04, $08, $0C, $10
    !by $14, $18, $1C, $20
    !by $24, $28, $2C, $30
    !by $34, $38, $3C, $40
    !by $44, $48, $4C, $50
    !by $54, $58, $5C, $60
    !by $64, $68, $6C, $70
    !by $74, $78, $7C, $80
; offset for control register of each voice
offControlVoice:
    !by $02 ; $D404 control voice 1
    !by $09 ; $D40A control voice 2
    !by $10 ; $D411 control voice 3
; offset on one voice into instrument table
offInstrVoice:
    !by $00, $1D, $3A
; offset for current instrument into music effect

offCurrEffect
    !by $17, $3A, $5D
highFreq:
    !by $17, $28, $3A, $4C, $60, $75, $8B, $A3
    !by $BC, $D6, $F2, $10, $2F, $50, $73, $99
    !by $C0, $EA, $16, $45, $77, $AC, $E4, $1F
    !by $5E, $A0, $E7, $32, $80, $D4, $2D, $8B
    !by $EE, $58, $C8, $3E, $BC, $41, $CD, $63
    !by $01, $A8, $5A, $16, $DD, $B0, $90, $7C
    !by $77, $81, $9B, $C5, $02, $51, $B4, $2C
    !by $BA, $60, $1F, $F9, $EF, $03, $36, $8B
    !by $03, $A1, $67, $57, $73, $C0, $3E, $F1
    !by $DE, $05, $6C, $16, $06, $43, $CE, $AE
    !by $00
lowFreq:
    !by $01, $01, $01, $01, $01, $01, $01, $01
    !by $01, $01, $01, $02, $02, $02, $02, $02
    !by $02, $02, $03, $03, $03, $03, $03, $04
    !by $04, $04, $04, $05, $05, $05, $06, $06
    !by $06, $07, $07, $08, $08, $09, $09, $0A
    !by $0B, $0B, $0C, $0D, $0D, $0E, $0F, $10
    !by $11, $12, $13, $14, $16, $17, $18, $1A
    !by $1B, $1D, $1F, $20, $22, $25, $27, $29
    !by $2C, $2E, $31, $34, $37, $3A, $3E, $41
    !by $45, $4A, $4E, $53, $58, $5D, $62, $68
    !by $00

;2615 jsr executePatternV1
; jsr executePatternV2
; jsr executePatternV3
; jsr makeTimbreV1
; jsr makeTimbreV2
; jsr makeTimbreV3
; jsr makeFilterEff
; rts
;=================================
; Set tracks at offset given by y
;=================================
setTracks:
    lda trackTable+1,y ; read the minime note duration
    sta minDuration
    and #$0F
    sta buildDurTable+1
    ldx #$02
    stx tmpVIndex ; tmp voices index
    ldx #$04
    stx tmpAVIndex ; tmp address voice index
    dey
nextVoice:
    lda trackTable,y ; test if low and high address are 0
    ora trackTable+1,y
    beq afterSetting ; skip if address are 0
    ldx tmpAVIndex ; tmp address voice index
    lda trackTable,y
    sta $E0,x ; store low pattern addr. voice in x
    lda trackTable+1,y
    sta $E1,x ; store high pattern addr. voice in x
    sty $FD ; tmp track offset
    ldx tmpVIndex ; tmp voices index
    ldy offInstrVoice,x ; offset of one voice into instr. table
    lda #$00
    sta $EC,x
    sta InstTableV1+13,y ; freq. effect voice from x
    sta InstTableV1+17,y ; wave effect voice from x
    lda #$07
    sta $E9,x ; init stack index for voice x
    lda #$01
    sta $E6,x ; store actual note duration (length) voice in x
    sta playMusicV1,x ; play music flag
    ldy $FD ; tmp track offset
afterSetting:
    dey
    dey
    dec tmpAVIndex ; tmp address voice index
    dec tmpAVIndex ; tmp address voice index
    dec tmpVIndex ; tmp voices index
    bpl nextVoice
    clc
    lda #$00
buildDurTable: ; build the table of note duration
    adc #$04 ; add minime duration
    sta durationTable,x
    inx
    cpx #$20
    bcc buildDurTable
    rts

;==================================
; Init the engine
;==================================
initEngine:
    ldx #$16
loopInitE:
    lda #$08
    sta $D400,x ; Voice 1: Frequency control (lo byte)
    lda #$00
    sta $D400,x ; Voice 1: Frequency control (lo byte)
    dex
    bpl loopInitE
    sta CurInstTableV1+28
    sta CurInstTableV2+28
    sta CurInstTableV3+28
    sta CurFilterTable+13 ; filter flag effect
    sta playMusicV1 ; play music flag voice 1
    sta playMusicV2 ; play music flag voice 2
    sta playMusicV3 ; play music flag voice 3
    stx voiceNumber ; voice number (0,1,2) on where apply filter
    lda #$F0
    sta $D417 ; Filter resonance control/voice input control
    lda #$0F
    sta TEMP
    sta $D418 ; Select volume and filter mode
    rts
;=================================
; Voice 1: Reload the cycle for
; Wave effect
;=================================
reloadWaveCycleV1:
    ldx CurInstTableV1+22 ; read current wave low voice 1
    ldy CurInstTableV1+23 ; read current wave high voice 1
reloadWCycleV1:
    stx $F1 ; wave low v1
    sty $F2 ; wave high v1
    lda CurInstTableV1+14 ; read current (to reload) wave cycle 1 voice 1
    sta CurInstTableV1+33 ; store current wave cycle 1 voice 1
    lda CurInstTableV1+15 ; read current (to reload) wave cycle 2 voice 1
    sta CurInstTableV1+34 ; store current wave cycle 2 voice 1
    rts
;=================================
; Voice 2: Reload the cycle for
; Wave effect
;=================================
reloadWaveCycleV2:
    ldx CurInstTableV2+22 ; read current wave low voice 2
    ldy CurInstTableV2+23 ; read current wave high voice 2
reloadWCycleV2:
    stx $F3 ; wave low v2
    sty $F4 ; wave high v2
    lda CurInstTableV2+14 ; read current (to reload) wave cycle 1 voice 2
    sta CurInstTableV2+33 ; store current wave cycle 1 voice 2
    lda CurInstTableV2+15 ; read current (to reload) wave cycle 2 voice 2
    sta CurInstTableV2+34 ; store current wave cycle 2 voice 2
    rts

;=================================
; Voice 3: Reload the cycle for
; Wave effect
;=================================
reloadWaveCycleV3:
    ldx CurInstTableV3+22 ; read current wave low voice 3
    ldy CurInstTableV3+23 ; read current wave high voice 3
reloadWCycleV3:
    stx $F5 ; wave low v3
    sty $F6 ; wave high v3
    lda CurInstTableV3+14 ; read current (to reload) wave cycle 1 voice 3
    sta CurInstTableV3+33 ; store current wave cycle 1 voice 3
    lda CurInstTableV3+15 ; read current (to reload) wave cycle 2 voice 3
    sta CurInstTableV3+34 ; store current wave cycle 2 voice 3
    rts

;=================================
; Reload the filter cycle
;=================================

reloadFilterCycle:
    ldx CurFilterTable+14 ; read filter low value (8 bit)
    ldy CurFilterTable+15 ; read filter high value (3 bit)
    stx ActFilterTable+0 ; store actual filter low value (8 bit)
    sty ActFilterTable+1 ; store actual filter high value (3 bit)
reloadFCycle:
    lda CurFilterTable+8 ; filter cycle 1
    sta ActFilterTable+2 ; store actual filter cycle 1
    lda CurFilterTable+9 ; filter cycle 2
    sta ActFilterTable+3 ; store actual filter cycle 2
    lda CurFilterTable+10 ; filter cycle 3
    sta ActFilterTable+4 ; store actual filter cycle 3
    lda CurFilterTable+11 ; filter cycle 4
    sta ActFilterTable+5 ; store actual filter cycle 4
    rts

;=================================
; Set Sound Effect for voice
; a/y address of table | x voice
;=================================
setSoundEffect:
    sta $FD
    sty $FE
    stx tmpVIndex ; tmp voices index
    lda #$00
    sta $D417 ; Filter resonance control/voice input control
    lda #$0F
    sta $D418 ; Select volume and filter mode
    lda offControlVoice,x
    sta voiceOffset+1
    lda #$08 ; control: test bit on
    ldy offControlVoice,x
    sta $D402,y ; Voice 1: Wave form pulsation amplitude (lo byte)
    ; set voice ADSR, control and wave hi/lo
    ldy #$1A
    ldx #$04
loopVoiceSet:
    lda ($FD),y ; read value from table
voiceOffset:
    sta $D410,x ; store in sid register of given voice
    dey
    dex
    bpl loopVoiceSet
    ldy #$1D
    ldx voiceOffset+1
    lda ($FD),y
    sta $D3FE,x ; store freq. low voice x
    iny
    lda ($FD),y
    sta $D3FF,x ; store freq. high voice x
    ldy tmpVIndex ; tmp voices index
    ldx offCurrEffect,y
    ldy #$1E
    sta CurInstTableV1+2,x ; current freq high voice from x
    dey
    lda ($FD),y
    sta CurInstTableV1+1,x ; current freq low voice from x
    dey
    lda ($FD),y
    sta CurInstTableV1+5,x ;
    dey
    lda ($FD),y
    sta CurInstTableV1+4,x ;
    ldy #$18
    lda ($FD),y
    sta CurInstTableV1+3,x ; current control of voice from x
    ldy #$17
copyLoop:
    lda ($FD),y
    sta CurInstTableV1+0,x ; current instrument table voice 1
    dex
    dey
    bpl copyLoop
    inx ; zero means voice 1
    bne notVoice1
    lda CurInstTableV1+17 ; read current wave effect flag voice 1
    beq reloadFreqCycleV1
    jsr reloadWaveCycleV1

;=================================
; Voice 1: Reload the cycle for
; Freq effect
;=================================
reloadFreqCycleV1:
    ldx CurInstTableV1+24 ; read current freq. low voice 1
    ldy CurInstTableV1+25 ; read current freq. high voice 1
    stx $F7 ; store freq. low voice 1
    sty $F8 ; store freq. high voice 1
reloadFCycleV1:
    lda CurInstTableV1+11 ; read current (to reload) freq cycle 4 voice 1
    sta CurInstTableV1+32 ; store current freq cycle 4 voice 1
    lda CurInstTableV1+10 ; read current (to reload) freq cycle 3 voice 1
    sta CurInstTableV1+31 ; store current freq cycle 3 voice 1
    lda CurInstTableV1+9 ; read current (to reload) freq cycle 2 voice 1
    sta CurInstTableV1+30 ; store current freq cycle 2 voice 1
    lda CurInstTableV1+8 ; read current (to reload) freq cycle 1 voice 1
    sta CurInstTableV1+29 ; store current freq cycle 1 voice 1
    rts

notVoice1:
    cpx #$46 ; $46 means voice 2
    beq notVoice2
    lda CurInstTableV2+17 ; read current wave effect flag voice 2
    beq skipReloadWCV2
    jsr reloadWaveCycleV2
skipReloadWCV2:
    lda CurInstTableV2+13 ; read current freq. flag effect voice 2
    beq exitRFCV2

;=================================
; Voice 2: Reload the cycle for
; Freq effect
;=================================
reloadFreqCycleV2:
    ldx CurInstTableV2+24 ; read current freq. low voice 2
    ldy CurInstTableV2+25 ; read current freq. high voice 2
    stx $F9 ; store freq. low voice 2
    sty $FA ; store freq. high voice 2
reloadFCycleV2:
    lda CurInstTableV2+11 ; read current (to reload) freq cycle 4 voice 2
    sta CurInstTableV2+32 ; store current freq cycle 4 voice 2
    lda CurInstTableV2+10 ; read current (to reload) freq cycle 3 voice 2
    sta CurInstTableV2+31 ; store current freq cycle 3 voice 2
    lda CurInstTableV2+9 ; read current (to reload) freq cycle 2 voice 2
    sta CurInstTableV2+30 ; store current freq cycle 2 voice 2
    lda CurInstTableV2+8 ; read current (to reload) freq cycle 1 voice 2
    sta CurInstTableV2+29 ; store current freq cycle 1 voice 2
exitRFCV2:
    rts

notVoice2:
    lda CurInstTableV3+17 ; read wave effect flag voice 3
    beq skipReloadWCV3
    jsr reloadWaveCycleV3
skipReloadWCV3:
    lda CurInstTableV3+13 ; read freq. effect flag voice 3
    beq exitRFCV2
;=================================
; Voice 3: Reload the cycle for
; Freq effect
;=================================
reloadFreqCycleV3:
    ldx CurInstTableV3+24 ; read current freq. low voice 3
    ldy CurInstTableV3+25 ; read current freq. high voice 3
    stx $FB ; store freq. low voice 3
    sty $FC ; store freq. high voice 3
reloadFCycleV3:
    lda CurInstTableV3+11 ; read current (to reload) freq cycle 4 voice 3
    sta CurInstTableV3+32 ; store current freq cycle 4 voice 3
    lda CurInstTableV3+10 ; read current (to reload) freq cycle 3 voice 3
    sta CurInstTableV3+31 ; store current freq cycle 3 voice 3
    lda CurInstTableV3+9 ; read current (to reload) freq cycle 2 voice 3
    sta CurInstTableV3+30 ; store current freq cycle 2 voice 3
    lda CurInstTableV3+8 ; read current (to reload) freq cycle 1 voice 3
    sta CurInstTableV3+29 ; store current freq cycle 1 voice 3
    rts

;===================================
; Voice 1: JSRT instruction
; execute a subroutine and set the
; number of halftone to transpose
; #1 halfnote to add (?)
; #2 low address
; #3 high address
;==================================
inst_C6_v1:
    lda ($E0),y
    sta $EC
    iny
    lda #$04
    !by $2C ; bit instruction (hide next instruction)
;==================================
; Voice 1: JSR instruction
; execute a subroutine pattern
; #1 low address
; #2 high address
;==================================
inst_C2_v1:
    lda #$03
    ldx $E9 ; read stack index of voive 1
    clc
    adc $E0 ; fix for return
    sta $30,x ; store return address low
    lda #$00
    adc $E1
    sta $38,x ; store return address high
    dec $E9 ; dec stack index of voice 1
    lda ($E0),y ; read next byte: low address
    tax
    iny
    lda ($E0),y ; read next byte: high address
    stx $E0 ; change address
    sta $E1
    jmp readNextV1
;=================================
; Voice 1: SET2CI instruction
; Set 2 values of current instument
; #1 index
; #2 val1
; #3 val2
;=================================
inst_DE_v1:
    lda ($E0),y ; read next byte: index
    tax
    iny
    lda ($E0),y ; read next byte: first value
    sta CurInstTableV1+0,x
    iny
    lda ($E0),y ; read next byte: second value
    sta CurInstTableV1+1,x
    lda #$04
    jmp adjustPatternV1
;==================================
; Voice 1: SETNI instruction
; Set first N items of Instrument
; table
; #1 number of items
; #2 low addr.
; #3 high addr.
;=================================
inst_D2_v1:
    lda ($E0),y ; read next byte: number of items
    tax
    iny
    lda ($E0),y ; read next byte: low addr
    sta copyV1+1
    iny
    lda ($E0),y ; read next byte: high addr
    sta copyV1+2
copyV1:
    lda ins02,x ; read istrument byte to copy
    sta InstTableV1+0,x ; store istrument byte to use
    dex
    bpl copyV1
    lda #$04
    jmp adjustPatternV1
;=================================
; Voice 1: FOR instruction
; Repeat a block n times
; #1 the number of repeat to performe
;=================================
inst_CC_v1:
    ldx $E9 ; read stack index of voice 1
    lda #$02
    clc
    adc $E0 ; fix for next inst.
    sta $30,x ; store load addr. into stack
    lda #$00
    adc $E1
    sta $38,x ; store high addr. into stack
    lda ($E0),y ; read next byte: number of repeat
    sta $40,x
    dec $E9 ; dec stack index of voice 1
    lda #$02
    jmp adjustPatternV1
;==================================
; Voice 1: SETFI instruction
; Set instrument frequency effect
; by reading from the given table
; #1 low addr
; #1 high addr
;==================================
inst_F0_v1:
    lda ($E0),y ; read next byte: low addr
    sta ftableV1+1
    iny
    lda ($E0),y ; read next byte: high addr
    sta ftableV1+2
    ldx #$0D
ftableV1:
    lda insF03,x ; read from table
    sta InstTableV1+0,x ; store istrument freq. values
    dex
    bpl ftableV1
    jmp adjust3PatternV1
;=================================
; Voice 1: JMP instruction
; jump to the given location
; #1 low address
; #2 high address
;=================================
inst_C4_v1:
    lda ($E0),y ; read next byte of pattern: low addr
    tax
    iny
    lda ($E0),y ; read next byte of pattern: high addr
    stx $E0 ; change address
    sta $E1
    jmp readNextV1
;=================================
; Voice 1: SET instruction
; set a value in the table at the
; given index
; #1 index
; #2 value
;=================================
inst_CA_v1:
    lda ($E0),y ; read next byte of pattern
    tax
    iny
    lda ($E0),y ; read next byte of pattern
    sta InstTableV1+0,x
    jmp adjust3PatternV1
;=================================
; Voice 1: NEXT instruction
; Execute the next cycle of the FOR
;=================================
inst_CE_v1:
    ldx $E9 ; read stack index of voice 1
    dec $41,x ; number of repeat for voice 1
    beq endCycle1 ; stop to cycle if 0
    ldy $31,x ; read low addr into stack of voice 1
    lda $39,x ; read high addr into stack of voice 1
    sty $E0 ; change address
    sta $E1
    jmp readNextV1
endCycle1:
    inc $E9 ; inc stack index of voice 1
    tya
    jmp adjustPatternV1
;=================================
; Voice 1: RTS instruction
; return to the stored location
; in the stack
; If there is not an stored address,
; stop music
;=================================
inst_C0_v1:
    ldy $E9 ; read stack index of voice 1
    cpy #$07
    beq stop_v1
    inc $E9 ; inc stack index of voice 1
    ldx $31,y ; read low address
    lda $0039,y ; read high address
    stx $E0 ; change address
    sta $E1
    jmp readNextV1
stop_v1:
    dec playMusicV1 ; stop music
    rts
;=================================
; Voice1: SETCI istruction
; Set the current instrument value
; at the given index
; #1 index
; #2 value
;=================================
inst_D6_v1:
    lda ($E0),y ; read index of current instrument value
    tax
    iny
    lda ($E0),y ; read value to set
    sta CurInstTableV1+0,x ; store value in current instrument
    jmp adjust3PatternV1
;=================================
; Voice 1: INSTR instruction
; set 5 instrument parameters
; from the given location
; #1 low address
; #2 high address
;=================================
inst_D4_v1:
    lda ($E0),y ; read next byte of pattern: low address
    sta $EF
    iny
    lda ($E0),y ; read next byte of pattern: high address
    sta $F0
    ldy #$04
loopD4v1:
    lda ($EF),y ; read bytes from given address
    sta InstTableV1+24,y ; copy to instrument voice location
    dey
    bpl loopD4v1
    jmp adjust3PatternV1
;==================================
; Voice 2: JSR instruction
; execute a subroutine pattern
; #1 low address
; #2 high address
;==================================
inst_C2_v2:
    lda #$03
    ldx $EA ; read stack index of voice 2
    clc
    adc $E2 ; fix for return
    sta $A8,x ; store return address low
    lda #$00
    adc $E3
    sta $B0,x ; store return address high
    dec $EA ; dec stack index of voice 2
    lda ($E2),y ; read next byte: low address
    tax
    iny
    lda ($E2),y ; read next byte: high address
    stx $E2 ; change address
    sta $E3
    jmp readNextV2
;==================================
; Voice 2: SET2I instruction
; Set 2 values of instruction table
; at the given index
; #1 index
; #2 val1
; #3 val3
;==================================
inst_DC_v2:
    lda ($E2),y ; read next byte: index
    tax
    iny
    lda ($E2),y ; read next byte: first value
    sta InstTableV2+0,x
    iny
    lda ($E2),y ; read next byte: second value
    sta InstTableV2+1,x
    lda #$04
    jmp adjustPatternV2

;=================================
; Voice 2: SET2CI instruction
; Set 2 values of current instument
; #1 index
; #2 val1
; #3 val2
;=================================
inst_DE_v2:
    lda ($E2),y ; read next byte: index
    tax
    iny
    lda ($E2),y ; read next byte: first value
    sta CurInstTableV2+0,x
    iny
    lda ($E2),y ; read next byte: second value
    sta CurInstTableV2+1,x
    lda #$04
    jmp adjustPatternV2
;==================================
; Voice 2: SETNI instruction
; Set first N items of Istrument
; table
; #1 number of items
; #2 low addr.
; #3 high addr.
;==================================
inst_D2_v2:
    lda ($E2),y ; read next byte: number of items
    tax
    iny
    lda ($E2),y ; read next byte: low addr
    sta copyV2+1
    iny
    lda ($E2),y ; read next byte: high addr
    sta copyV2+2
copyV2:
    lda $3517,x ; read istrument byte to copy
    sta InstTableV2+0,x ; store istrument byte to use
    dex
    bpl copyV2
    lda #$04
    jmp adjustPatternV2

;=================================
; Voice 2: FOR instruction
; Repeat a block n times
; #1 the number of repeat to performe
;=================================
inst_CC_v2:
    ldx $EA ; read stack index of voice 2
    lda #$02
    clc
    adc $E2 ; fix for next inst.
    sta $A8,x ; store load addr. into stack
    lda #$00
    adc $E3
    sta $B0,x ; store high addr. into stack
    lda ($E2),y ; read next byte: number of repeat
    sta $B8,x
    dec $EA ; dec stack index of voice 2
    lda #$02
    jmp adjustPatternV2

;==================================
; Voice 2: SETFI instruction
; Set instrument frequency effect
; by reading from the given table
; #1 low addr
; #1 high addr
;==================================
inst_F0_v2:
    lda ($E2),y ; read next byte: low addr
    sta ftableV2+1
    iny
    lda ($E2),y ; read next byte: high addr
    sta ftableV2+2
    ldx #$0D
ftableV2:
    lda insF03,x ; read from table
    sta InstTableV2+0,x ; store istrument freq. values
    dex
    bpl ftableV2
    jmp adjust3PatternV2

;=================================
; Voice 2: JMP instruction
; jump to the given location
; #1 low address
; #2 high address
;=================================
inst_C4_v2:
    lda ($E2),y ; read next byte of pattern: low addr
    tax
    iny
    lda ($E2),y ; read next byte of pattern: high addr
    stx $E2 ; change address
    sta $E3
    jmp readNextV2

;=================================
; Voice 2: SET instruction
; set a value in the table at the
; given index
; #1 index
; #2 value
;=================================

inst_CA_v2:
    lda ($E2),y ; read next byte of pattern
    tax
    iny
    lda ($E2),y ; read next byte of pattern
    sta InstTableV2+0,x
    jmp adjust3PatternV2

;=================================
; Voice 2: NEXT instruction
; Execute the next cycle of the FOR
;=================================
inst_CE_v2:
    ldx $EA ; read stack index of voice 2
    dec $B9,x ; number of repeat for voice 2
    beq endCycle2 ; stop to cycle if 0
    ldy $A9,x ; read low addr into stack of voice 2
    lda $B1,x ; read high addr into stack of voice 2
    sty $E2 ; change address
    sta $E3
    jmp readNextV2
endCycle2:
    inc $EA ; inc stack index of voice 2
    tya
    jmp adjustPatternV2
;=================================
; Voice 2: RTS instruction
; return to the stored location
; in the stack
;=================================
inst_C0_v2:
    ldy $EA ; read stack index of voice 2
    cpy #$07
    beq stop_v2
    inc $EA ; inc stack index of voice 2
    ldx $A9,Y ; read low address
    lda $00B1,Y ; read high address
    stx $E2 ; change address
    sta $E3
    jmp readNextV2
stop_v2:
    dec playMusicV2 ; stop music
    rts
;=================================
; Voice2: istruction
; Set the current istrument value
; at the given index
; #1 index
; #2 value
;=================================
inst_D6_v2:
    lda ($E2),y ; read index of current instrument value
    tax
    iny
    lda ($E2),y ; read value to set
    sta CurInstTableV2+0,x ; store value in current instrument
    jmp adjust3PatternV2
;=================================
; Voice 2: INSTR instruction
; set 5 instrument parameters
; from the given location
; #1 low address
; #2 high address
;=================================
inst_D4_v2:
    lda ($E2),y ; read next byte of pattern: low address
    sta $EF
    iny
    lda ($E2),y ; read next byte of pattern: high address
    sta $F0
    ldy #$04
loopD4v2:
    lda ($EF),y ; read bytes from given address
    sta InstTableV2+24,y ; copy to instrument voice location
    dey
    bpl loopD4v2
    jmp adjust3PatternV2
;===================================
; Voice 3: JSRT instruction
; execute a subroutine and set the
; number of halftone to transpose
; #1 halfnote to add (?)
; #2 low address
; #3 high address
;==================================
inst_C6_v3:
    lda ($E4),y
    sta $EE
    iny
    lda #$04
    !by $2C
;==================================
; Voice 3: JSR instruction
; execute a subroutine pattern
; #1 low address
; #2 high address
;==================================
inst_C2_v3:
    lda #$03
    ldx $EB ; read stack index of voice 3
    clc
    adc $E4 ; fix for return
    sta $60,x ; store return address low
    lda #$00
    adc $E5
    sta $68,x ; store return address high
    dec $EB ; dec stack index of voice 3
    lda ($E4),y ; read next byte: low address
    tax
    iny
    lda ($E4),y ; read next byte: high address
    stx $E4 ; change address
    sta $E5
    jmp readNextV3
;==================================
; Voice 3: EXCT instruction
; Excecute a code gave by paramethers
; #1 low addr.
; #2 high addr.
;==================================
inst_D8_v3:
    lda #$2E ; return high point
    pha
    lda #$D2 ; return low point
    pha
    lda ($E4),y ; read low address
    sta $EF
    iny
    lda ($E4),y ; read high address
    sta $F0
    jmp ($00EF) ; execute code at read address
;==================================
; Voice 3: SET2I instruction
; Set 2 values of instruction table
; at the given index
; #1 index
; #2 val1
; #3 val3
;==================================
inst_DC_v3:
    lda ($E4),y ; read next byte: index
    tax
    iny
    lda ($E4),y ; read next byte: first value
    sta InstTableV3+0,x
    iny
    lda ($E4),y ; read next byte: second value
    sta InstTableV3+1,x
    lda #$04
    jmp adjustPatternV3
;=================================
; Voice 3: SET2CI instruction
; Set 2 values of current instument
; #1 index
; #2 val1
; #3 val2
;=================================
inst_DE_v3:
    lda ($E4),y ; read next byte of pattern: index
    tax
    iny
    lda ($E4),y ; read next byte of pattern: first value
    sta CurInstTableV3+0,x
    iny
    lda ($E4),y ; read next byte of pattern: second value
    sta CurInstTableV3+1,x
    lda #$04
    jmp adjustPatternV3
;==================================
; Voice 3: FILTA instruction
; Set filter table
; #1: low addr.
; #2: high addr.
;==================================
inst_E2_v3:
    lda ($E4),y ; read next byte of pattern
    sta copyFil+1
    iny
    lda ($E4),y ; read next byte of pattern
    sta copyFil+2
    ldx #$0F
copyFil:
    lda fil02,x ; value to copy
    sta MainFilterTable+0,x ; copy in main filter table
    dex
    bpl copyFil
    jmp adjust3PatternV3
;==================================
; Voice 3: SETNI instruction
; Set first N items of Istrument
; table
; #1 number of items
; #2 low addr.
; #3 high addr.
;=================================
inst_D2_v3:
    lda ($E4),y ; read next byte: number of items
    tax
    iny
    lda ($E4),y ; read next byte: low addr
    sta copyV3+1
    iny
    lda ($E4),y ; read next byte: high addr
    sta copyV3+2
copyV3:
    lda insF01,x ; read istrument byte to copy
    sta InstTableV3+0,x ; store istrument byte to use
    dex
    bpl copyV3
    lda #$04
    jmp adjustPatternV3
;=================================
; Voice 3: FOR instruction
; Repeat a block n times
; #1 the number of repeat to performe
;=================================
inst_CC_v3:
    ldx $EB ; read stack index of voice 3
    lda #$02
    clc
    adc $E4 ; fix for next inst.
    sta $60,x ; store load addr. into stack
    lda #$00
    adc $E5
    sta $68,x ; store high addr. into stack
    lda ($E4),y ; read next byte: number of repeat
    sta $70,x
    dec $EB ; dec stack index of voice 3
    lda #$02
    jmp adjustPatternV3
;==================================
; Voice 3: SETFI instruction
; Set instrument frequency effect
; by reading from the given table
; #1 low addr
; #1 high addr
;==================================
inst_F0_v3:
    lda ($E4),y ; read next byte: low addr
    sta ftableV3+1
    iny
    lda ($E4),y ; read next byte: high addr
    sta ftableV3+2
    ldx #$0D
ftableV3:
    lda insF01,x ; read from table
    sta InstTableV3+0,x ; store istrument freq. values
    dex
    bpl ftableV3
    jmp adjust3PatternV3
;=================================
; Voice 3: JMP instruction
; jump to the given location
; #1 low address
; #2 high address
;=================================
inst_C4_v3:
    lda ($E4),y ; read next byte of pattern: low addr
    tax
    iny
    lda ($E4),y ; read next byte of pattern: high addr
    stx $E4 ; change address
    sta $E5
    jmp readNextV3
setMax:
    ldy #$F4 ; max resonance/filter on voice 3
    sty $D417 ; Filter resonance control/voice input control
    stx voiceNumber ; voice number (0,1,2) on where apply filter
    ldx #$1F ; low pass filter
    stx $D418 ; Select volume and filter mode
    lda #$01
    jmp adjustPatternV3
;=================================
; Voice 3: LF3 instruction
; Set low filter on voice 3 with
; max resonance
;=================================
inst_E0_v3:
    ldx #$02
    bne setMax
;=================================
; Voice 3: SET instruction
; set a value in the table at the
; given index
; #1 index
; #2 value
;=================================
inst_CA_v3:
    lda ($E4),y ; read next byte of pattern
    tax
    iny
    lda ($E4),y ; read next byte of pattern
    sta InstTableV3+0,x
    jmp adjust3PatternV3
;=================================
; Voice 3: NEXT instruction
; Execute the next cycle of the FOR
;=================================
inst_CE_v3:
    ldx $EB ; read stack index of voice 3
    dec $71,x ; number of repeat for voice 3
    beq endCycle3 ; stop to cycle if 0
    ldy $61,x ; read low addr into stack of voice 3
    lda $69,x ; read high addr into stack of voice 3
    sty $E4 ; change address
    sta $E5
    jmp readNextV3
endCycle3:
    inc $EB ; inc stack index of voice 3
    tya
    jmp adjustPatternV3
;=================================
; Voice 3: RTS instruction
; return to the stored location
; in the stack
;=================================
inst_C0_v3:
    ldy $EB ; read stack index of voice 3
    cpy #$07
    beq stop_v3
    inc $EB ; inc stack index of voice 3
    ldx $61,y ; read low address
    lda $0069,y ; read high address
    stx $E4 ; change address
    sta $E5
    jmp readNextV3
stop_v3:
    dec playMusicV3 ; stop music
    rts
;=================================
; Voice 3: INSTR instruction
; set 5 instrument parameters
; from the given location
; #1 low address
; #2 high address
;=================================
inst_D4_v3:
    lda ($E4),y ; read next byte of pattern: low address
    sta $EF
    iny
    lda ($E4),y ; read next byte of pattern: high address
    sta $F0
    ldy #$04
loopD4v3:
    lda ($EF),y ; read bytes from given address
    sta InstTableV3+24,y ; copy to instrument voice location
    dey
    bpl loopD4v3
    jmp adjust3PatternV3
;=================================
; Make the dinamic filter effect
; If the filter flag has bit 1=1,
; during initial delay it is
; performed a cycle4 adding
;=================================
makeFilterEff:
    lda CurFilterTable+13 ; read filter flag effect
    beq exitMFE ; no filter if 0
    ldx ActFilterTable+0 ; read actual filter low value (8 bit)
    ldy ActFilterTable+1 ; read actual filter high value (3 bit)
    clc
    lda CurFilterTable+12 ; read initial filter delay
    beq filterAdd1
    dec CurFilterTable+12 ; dec filter delay
    lda CurFilterTable+13 ; read filter flag effect
    and #$02
    bne makeFilterAdd4
exitMFE:
    rts
;=================================
; Filter add cycle 1
;=================================
filterAdd:
    clc
filterAdd1:
    lda ActFilterTable+2 ; actual filter cycle 1
    beq filterAdd2 ; jump if 0
    dec ActFilterTable+2 ; dec actual filter cycle 4
    txa
    adc CurFilterTable+0 ; add filter low value 1
    tax
    tya
    adc CurFilterTable+1 ; add filter high value 1
    jmp changeFilter
;=================================
; Filter add cycle 2
;=================================
filterAdd2:
    lda ActFilterTable+3 ; actual filter cycle 2
    beq filterAdd3 ; jump if 0
    dec ActFilterTable+3 ; dec actual filter cycle 2
    txa
    adc CurFilterTable+2 ; add filter low value 2
    tax
    tya
    adc CurFilterTable+3 ; add filter high value 2
    jmp changeFilter
;=================================
; Filter add cycle 3
;=================================
filterAdd3:
    lda ActFilterTable+4 ; actual filter cycle 3
    beq filterAdd4 ; jump if zero
    dec ActFilterTable+4 ; dec actual filter cycle 3
    txa
    adc CurFilterTable+4 ; add filter low value 3
    tax
    tya
    adc CurFilterTable+5 ; add filter high value 3
    jmp changeFilter
;=================================
; filter add cycle 4
;=================================
filterAdd4:
    lda ActFilterTable+5 ; actual filter cycle 4
    beq resetFilter ; jump if zero
    dec ActFilterTable+5 ; dec actual filter cycle 4
makeFilterAdd4:
    txa
    adc CurFilterTable+6 ; add filter low value 4
    tax
    tya
    adc CurFilterTable+7 ; add filter high value 4
changeFilter:
    tay
changeRFilter:
    stx ActFilterTable+0 ; store actual filter low value (8 bit)
    sty ActFilterTable+1 ; store actual filter high value (3 bit)
changeNoSFilter: ; change and not store
    txa
    and #$07
    sta $D415 ; Filter cut frequency: lo byte (bit 2-0)
    tya ; shift high bits into filter register high
    stx tmpCutF
    lsr
    ror tmpCutF
    lsr
    ror tmpCutF
    lsr
    lda tmpCutF
    ror
    sta $D416 ; Filter cut frequency: hi byte
    rts
;=================================
; Reset Filter
; Flag meanings:
; xyyy yyyyz
; x=1 -> reload the filter cycle with filter value of istrument table again
; z=1 -> reload the filter cycle but use the actual filter value
; y=1 -> continue with actual filter, no more cycle
;=================================
resetFilter:
    lda CurFilterTable+13 ; read filter effect flag
    and #$81
    beq changeRFilter
    bpl useActualF
    jsr reloadFilterCycle
    jmp filterAdd
useActualF:
    jsr reloadFCycle
    jmp filterAdd
;=================================
; copy main filter table to current
; one and reload the cycle
;=================================
copyMainCurrentFilter:
    ldx #$07
loopCopy:
    lda MainFilterTable+0,x ; copy main filter table part 1
    sta CurFilterTable+0,x ; to current filter table part 1
    lda MainFilterTable+8,x ; copy main filter table part 2
    sta CurFilterTable+8,x ; to current filter table part 2
    dex
    bpl loopCopy
    jsr reloadFilterCycle
    jmp changeNoSFilter
;=================================
; Execute the pattern of note
; voice 1
;=================================
executePatternV1:
    lda playMusicV1 ; play music flag voice 1
    beq no_execv1 ; exit if no music
    dec $E6 ; dec actual duration of note voice 1
    beq readNextV1
no_execv1:
    rts

fixHighV1:
    inc $E1
    bne readNextV1
adjust3PatternV1: ; adjust the pattern index with 3
    lda #$03
adjustPatternV1: ; adjust the pattern index with A reg.
    clc
    adc $E0
    sta $E0
    bcs fixHighV1
;==================================
; read next byte from pattern of voice 1
;==================================
readNextV1:
    ldy #$00
    lda ($E0),y ; read next byte of pattern
    cmp #$C0 ; is an instruction?
    bcc isNoteV1
    iny
    adc #$3F ; calcolate right offset to the inst. table
    sta addressV1+1
addressV1:
    jmp (InstrVoice1) ; execute instruction read from the pattern
jmpSetDurationV1:
    jmp setDurationV1
isNoteV1:
    sta $FF ; store the read byte: note to play
    cmp #$60 ; <60 means use custom durations
    bcc alreadyReducedV1
    sbc #$60 ; reduce to 0..5F
alreadyReducedV1:
    cmp #$5F ; 5Fh means rest note
    beq jmpSetDurationV1
    adc $EC ; haftone to transpose
    tax
    lda #$08
    sta $D404 ; Voice 1: Control registers
    lda voiceNumber ; voice number (0,1,2) on where apply filter
    bne continueInitV1 ; skip if not 0
    stx $EF
    jsr copyMainCurrentFilter
    ldx $EF
continueInitV1:
    ldy lowFreq,x ; read freq high from table
    lda highFreq,x ; read freq low from table
    sta CurInstTableV1+24 ; store current freq low voice 1
    sty CurInstTableV1+25 ; store current freq high voice 1
    sta $D400 ; Voice 1: Frequency control (lo byte)
    sty $D401 ; Voice 1: Frequency control (hi byte)
    ldx InstTableV1+22 ; read wave low voice 1
    ldy InstTableV1+23 ; read wave high voice 1
    stx $D402 ; Voice 1: Wave form pulsation amplitude (lo byte)
    sty $D403 ; Voice 1: Wave form pulsation amplitude (hi byte)
    lda InstTableV1+25 ; read AD voice 1
    sta $D405 ; Generator 1: Attack/Decay
    lda InstTableV1+26 ; read SR voice 1
    sta $D406 ; Generator 1: Sustain/Release
    lda InstTableV1+24 ; read control registers voice 1
    sta CurInstTableV1+26 ; store current control registers
    and #$F7 ; set test bit to 0
    sta $D404 ; Voice 1: Control registers
    lda InstTableV1+17 ; read wave effect flag voice 1
    sta CurInstTableV1+17 ; store current wave effect flag voice 1
    beq freqInitV1 ; skip other wave init if 0
    stx CurInstTableV1+22 ; store current wave low voice 1
    sty CurInstTableV1+23 ; store current wave high voice 1
    stx $F1 ; wave low voice 1
    sty $F2 ; wave high voice 1
    lda InstTableV1+21 ; read wave high add 2 voice 1
    sta CurInstTableV1+21 ; store current wave high add 2 voice 1
    lda InstTableV1+20 ; read wave low add 2 voice 1
    sta CurInstTableV1+20 ; store current wave low add 2 voice 1
    lda InstTableV1+19 ; read wave high add 1 voice 1
    sta CurInstTableV1+19 ; store current high add 1 voice 1
    lda InstTableV1+18 ; read wave low add 1 voice 1
    sta CurInstTableV1+18 ; store current wave low add 1 voice 1
    lda InstTableV1+16 ; read wave delay initial voice 1
    sta CurInstTableV1+16 ; store current wave initial delay voice 1
    ldx InstTableV1+14 ; read wave cycle 1 voice 1
    ldy InstTableV1+15 ; read wave cycle 2 voice 1
    stx CurInstTableV1+14 ; store current (to reload) wave cycle 1 voice 1
    stx CurInstTableV1+33 ; store current wave cycle 1 voice 1
    sty CurInstTableV1+15 ; store current (to reload) wave cycle 2 voice 1
    sty CurInstTableV1+34 ; store current wave cycle 2 voice 1
freqInitV1:
    lda InstTableV1+13 ; read freq. flag effect voice 1
    sta CurInstTableV1+13 ; store current freq. flag effect voice 1
    beq skipFreqSetV1
    ldx InstTableV1+12 ; read freq delay initial voice 1
    stx CurInstTableV1+12 ; store current freq. initial delay voice 1
    ldx InstTableV1+11 ; read freq cycle 4 voice 1
    stx CurInstTableV1+11 ; store current (to reload) freq cycle 4 voice 1
    ldx InstTableV1+10 ; read freq cycle 3 voice 1
    stx CurInstTableV1+10 ; store current (to reload) freq cycle 3 voice 1
    ldx InstTableV1+9 ; read freq cycle 2 voice 1
    stx CurInstTableV1+9 ; store current (to reload) freq cycle 2 voice 1
    ldx InstTableV1+8 ; read freq cycle 1 voice 1
    stx CurInstTableV1+8 ; store current (to reload) freq cycle 1 voice 1
    ldx InstTableV1+7 ; read freq high add 4 voice 1
    stx CurInstTableV1+7 ; store current freq high add 4 voice 1
    ldx InstTableV1+6 ; read freq low add 4 voice 1
    stx CurInstTableV1+6 ; store current freq low add 4 voice 1
    ldx InstTableV1+5 ; read freq high add 3 voice 1
    stx CurInstTableV1+5 ; store current freq high add 3 voice 1
    ldx InstTableV1+4 ; read freq low add 3 voice 1
    stx CurInstTableV1+4 ; store current freq low add 3 voice 1
    ldx InstTableV1+3 ; read freq high add 2 voice 1
    stx CurInstTableV1+3 ; store current freq high add 2 voice 1
    ldx InstTableV1+2 ; read freq low add 2 voice 1
    stx CurInstTableV1+2 ; store current freq low add 2 voice 1
    ldx InstTableV1+1 ; read freq high add 1 voice 1
    stx CurInstTableV1+1 ; store current freq high add 1 voice 1
    ldx InstTableV1+0 ; read freq low add 1 voice 1
    stx CurInstTableV1+0 ; store current freq low add 1 voice 1
    and #$08
    beq reloadFCV1
    lda $FF ; read store note to play
    cmp #$60
    bcc alreadyReduced2V1
    sbc #$5F
alreadyReduced2V1:
    adc $EC ; half tone to transpose
    sta CurInstTableV1+10 ; store current (to reload) freq cycle 3 voice 1
    bne skipReloadFCV1
reloadFCV1:
    jsr reloadFreqCycleV1
skipReloadFCV1:
skipFreqSetV1:
    ldx InstTableV1+27
    ldy InstTableV1+28
    stx CurInstTableV1+27
    sty CurInstTableV1+28
setDurationV1:
    ldy #$01
    lda ($E0),y ; read next byte of pattern: duration index voice 1
    ldx $FF ; read stored note to play
    cpx #$60 ; <60h means custom durations
    bcs skipDurTableV1
    tax
    lda durationTable-1,x ; read duration from table
skipDurTableV1:
    sta $E6 ; store duration of note voice 1
    lda #$02
    clc
    adc $E0
    sta $E0 ; update pointer for next note
    bcs fixHigh2V1
    rts

fixHigh2V1:
    inc $E1
    rts

;=================================
; Execute the pattern of note
; voice 2
;=================================
executePatternV2:
    lda playMusicV2 ; play music flag voice 2
    beq no_execv2 ; exit if no music
    dec $E7 ; dec actual note duration voice 2
    beq readNextV2
no_execv2:
    rts

fixHighV2:
    inc $E3
    bne readNextV2

adjust3PatternV2: ; adjust the pattern index with 3
    lda #$03
adjustPatternV2: ; adjust the pattern index with A reg.
    clc
    adc $E2
    sta $E2
    bcs fixHighV2
;=================================
; read next byte from pattern of voice 2
;=================================
readNextV2:
    ldy #$00
    lda ($E2),y ; read next byte of pattern
    cmp #$C0 ; is an instruction?
    bcc isNoteV2
    iny
    adc #$71 ; calcolate right offset to the inst. table
    sta addressV2+1
addressV2:
    jmp (InstrVoice2) ; execute instruction read from the pattern
jmpSetDurationV2:
    jmp setDurationV2
isNoteV2:
    sta $FF ; store the read byte: note to play
    cmp #$60 ; <60 means use custom durations
    bcc alreadyReducedV2
    sbc #$60 ; reduce to 0..5F
alreadyReducedV2:
    cmp #$5F ; 5Fh means rest note
    beq jmpSetDurationV2
    adc $ED ; haftone to transpose
    tax
    lda #$08
    sta $D40B ; Voice 2: Control registers
    lda voiceNumber ; voice number (0,1,2) on where apply filter
    cmp #$01
    bne continueInitV2 ; skip if not 1
    stx $EF
    jsr copyMainCurrentFilter
    ldx $EF
continueInitV2:
    ldy lowFreq,x ; read freq high from table
    lda highFreq,x ; read freq low from table
    sta CurInstTableV2+24 ; store current freq low voice 2
    sty CurInstTableV2+25 ; store current freq high voice 2
    sta $D407 ; Voice 2: Frequency control (lo byte)
    sty $D408 ; Voice 2: Frequency control (hi byte)
    ldx InstTableV2+22 ; read wave low voice 2
    ldy InstTableV2+23 ; read wave high voice 2
    stx $D409 ; Voice 2: Wave form pulsation amplitude (lo byte)
    sty $D40A ; Voice 2: Wave form pulsation amplitude (hi byte)
    lda InstTableV2+25 ; read AD voice 2
    sta $D40C ; Generator 2: Attack/Decay
    lda InstTableV2+26 ; read SR voice 1
    sta $D40D ; Generator 2: Sustain/Release
    lda InstTableV2+24 ; read control registers voice 2
    sta CurInstTableV2+26 ; store current control registers voice 2
    and #$F7 ; set test bit to 0
    sta $D40B ; Voice 2: Control registers
    lda InstTableV2+17 ; read wave effect flag voice 2
    sta CurInstTableV2+17 ; store current wave effect flag voice 2
    beq freqInitV2 ; skip other wave init if 0
    stx CurInstTableV2+22 ; store current wave low voice 2
    sty CurInstTableV2+23 ; store current wave high voice 2
    stx $F3 ; wave low voice 2
    sty $F4 ; wave high voice 2
    lda InstTableV2+21 ; read wave high add 2 voice 2
    sta CurInstTableV2+21 ; store current wave high add 2 voice 2
    ldx InstTableV2+20 ; read wave low add 2 voice 2
    stx CurInstTableV2+20 ; store current wave low add 2 voice 2
    lda InstTableV2+19 ; read wave high add 1 voice 2
    sta CurInstTableV2+19 ; store current high add 1 voice 2
    lda InstTableV2+18 ; read wave low add 1 voice 2
    sta CurInstTableV2+18 ; store current wave low add 1 voice 2
    lda InstTableV2+16 ; read wave delay initial voice 2
    sta CurInstTableV2+16 ; store current wave initial delay voice 2
    ldx InstTableV2+14 ; read wave cycle 1 voice 2
    ldy InstTableV2+15 ; read wave cycle 2 voice 2
    stx CurInstTableV2+14 ; store current (to reload) wave cycle 1 voice 2
    stx CurInstTableV2+33 ; store current wave cycle 1 voice 2
    sty CurInstTableV2+15 ; store current (to reload) wave cycle 2 voice 2
    sty CurInstTableV2+34 ; store current wave cycle 1 voice 2
freqInitV2:
    lda InstTableV2+13 ; read freq. flag effect voice 2
    sta CurInstTableV2+13 ; store current freq. flag effect voice 2
    beq skipFreqSetV2
    ldx InstTableV2+12 ; read freq delay initial voice 2
    stx CurInstTableV2+12 ; store current freq. initial delay voice 2
    ldx InstTableV2+11 ; read freq cycle 4 voice 2
    stx CurInstTableV2+11 ; store current (to reload) freq cycle 4 voice 2
    ldx InstTableV2+10 ; read freq cycle 3 voice 2
    stx CurInstTableV2+10 ; store current (to reload) freq cycle 3 voice 2
    ldx InstTableV2+9 ; read freq cycle 2 voice 2
    stx CurInstTableV2+9 ; store current (to reload) freq cycle 2 voice 2
    ldx InstTableV2+8 ; read freq cycle 1 voice 2
    stx CurInstTableV2+8 ; store current (to reload) freq cycle 1 voice 2
    ldx InstTableV2+7 ; read freq high add 4 voice 2
    stx CurInstTableV2+7 ; store current freq high add 4 voice 2
    ldx InstTableV2+6 ; read freq low add 4 voice 2
    stx CurInstTableV2+6 ; store current freq low add 4 voice 2
    ldx InstTableV2+5 ; read freq high add 3 voice 2
    stx CurInstTableV2+5 ; store current freq high add 3 voice 2
    ldx InstTableV2+4 ; read freq low add 3 voice 2
    stx CurInstTableV2+4 ; store current freq low add 3 voice 2
    ldx InstTableV2+3 ; read freq high add 2 voice 2
    stx CurInstTableV2+3 ; store current freq high add 2 voice 2
    ldx InstTableV2+2 ; read freq low add 2 voice 2
    stx CurInstTableV2+2 ; store current freq low add 2 voice 2
    ldx InstTableV2+1 ; read freq high add 1 voice 2
    stx CurInstTableV2+1 ; store current freq high add 1 voice 2
    ldx InstTableV2+0 ; read freq low add 1 voice 2
    stx CurInstTableV2+0 ; store current freq low add 1 voice 2
    and #$08
    beq reloadFCV2

    lda $FF ; read store note to play
    cmp #$60
    bcc alreadyReduced2V2
    sbc #$5F
alreadyReduced2V2:
    adc $ED
    sta CurInstTableV2+10 ; store current (to reload) freq cycle 3 voice 2
    bne skipReloadFCV2
reloadFCV2:
    jsr reloadFreqCycleV2
skipFreqSetV2:
skipReloadFCV2:
    ldx InstTableV2+27
    ldy InstTableV2+28
    stx CurInstTableV2+27
    sty CurInstTableV2+28
setDurationV2:
    ldy #$01
    lda ($E2),y ; read next byte of pattern: duration index voice 2
    ldx $FF ; read stored note to play
    cpx #$60 ; <60h means custom durations
    bcs skipDurTableV2
    tax
    lda durationTable-1,x ; read duration from table
skipDurTableV2:
    sta $E7 ; store actual note duration voice 2
    lda #$02
    clc
    adc $E2
    sta $E2 ; update pointer for next note
    bcs fixHigh2V2
    rts
fixHigh2V2:
    inc $E3
    rts

;=================================
; Execute the pattern of note
; voice 3
;=================================
executePatternV3:
    lda playMusicV3 ; play music flag voice 3
    beq no_execv3 ; exit if no music
    dec $E8 ; dec actual note duration voice 3
    beq readNextV3
no_execv3:
    rts

fixHighV3:
    inc $E5
    bne readNextV3
adjust3PatternV3: ; adjust the pattern index with 3
    lda #$03
adjustPatternV3: ; adjust the pattern index with A reg.
    clc
    adc $E4
    sta $E4
    bcs fixHighV3

;==================================
; read next byte from pattern of voice 3
;==================================
readNextV3:
    ldy #$00
    lda ($E4),y ; read next byte of pattern
    cmp #$C0 ; is an instruction
    bcc isNoteV3
    iny
    adc #$A3 ; calcolate right offset to the inst. table
    sta addressV3+1
addressV3:
    jmp (InstrVoice3) ; execute instruction read from the pattern
jmpSetDurationV3:
    jmp setDurationV3
isNoteV3:
    sta $FF ; store the read byte: note to play
    cmp #$60 ; <60 means use custom durations
    bcc alreadyReducedV3
    sbc #$60 ; reduce to 0..5F
alreadyReducedV3:
    cmp #$5F ; 5Fh means rest note
    beq jmpSetDurationV3
    cmp #$50
    beq noTransposeV3
    adc $EE ; haftone to transpose
noTransposeV3:
    tax
    lda #$08
    sta $D412 ; Voice 3: Control registers
    lda voiceNumber ; voice number (0,1,2) on where apply filter
    cmp #$02
    bne continueInitV3 ; skip if not 2
    stx $EF
    jsr copyMainCurrentFilter
    ldx $EF
continueInitV3:
    ldy lowFreq,x ; read freq high from table
    lda highFreq,x ; read freq low from table
    sta CurInstTableV3+24 ; store current freq low voice 3
    sty CurInstTableV3+25 ; store current freq high voice 3
    sta $D40E ; Voice 3: Frequency control (lo byte)
    sty $D40F ; Voice 3: Frequency control (hi byte)
    ldx InstTableV3+22 ; read wave low voice 3
    ldy InstTableV3+23 ; read wave high voice 3
    stx $D410 ; Voice 3: Wave form pulsation amplitude (lo byte)
    sty $D411 ; Voice 3: Wave form pulsation amplitude (hi byte)
    lda InstTableV3+25 ; read AD voice 3
    sta $D413 ; Generator 3: Attack/Decay
    lda InstTableV3+26 ; read SR voice 3
    sta $D414 ; Generator 3: Sustain/Release
    lda InstTableV3+24 ; read control registers voice 3
    sta CurInstTableV3+26 ; store current control register
    and #$F7 ; set test bit to 0
    sta $D412 ; Voice 3: Control registers
    lda InstTableV3+17 ; read wave effect flag voice 3
    sta CurInstTableV3+17 ; store current wave effect flag voice 3
    beq freqInitV3 ; skip other wave init if 0
    stx CurInstTableV3+22 ; store current wave low voice 3
    sty CurInstTableV3+23 ; store current wave high voice 3
    stx $F5 ; wave low voice 3
    sty $F6 ; wave high voice 3
    lda InstTableV3+21 ; read wave high add 2 voice 3
    sta CurInstTableV3+21 ; store current wave high add 2 voice 3
    lda InstTableV3+20 ; read wave low add 2 voice 3
    sta CurInstTableV3+20 ; store current wave low add 2 voice 3
    lda InstTableV3+19 ; read wave high add 1 voice 3
    sta CurInstTableV3+19 ; store current high add 1 voice 3
    lda InstTableV3+18 ; read wave low add 1 voice 3
    sta CurInstTableV3+18 ; store current wave low add 1 voice 3
    lda InstTableV3+16 ; read wave delay initial voice 3
    sta CurInstTableV3+16 ; store current wave initial delay voice 3
    ldx InstTableV3+14 ; read wave cycle 1 voice 3
    ldy InstTableV3+15 ; read wave cycle 2 voice 3
    stx CurInstTableV3+14 ; store current (to reload) wave cycle 1 voice 3
    stx CurInstTableV3+33 ; store current wave cycle 1 voice 3
    sty CurInstTableV3+15 ; store current (to reload) wave cycle 2 voice 3
    sty CurInstTableV3+34 ; store current wave cycle 1 voice 3
freqInitV3:
    lda InstTableV3+13 ; read freq. flag effect voice 3
    sta CurInstTableV3+13 ; store current freq. flag effect voice 3
    beq skipFreqSetV3
    ldx InstTableV3+12 ; read freq delay initial voice 3
    stx CurInstTableV3+12 ; store current freq. initial delay voice 3
    ldx InstTableV3+11 ; read freq cycle 4 voice 3
    stx CurInstTableV3+11 ; store current (to reload) freq cycle 4 voice 3
    ldx InstTableV3+10 ; read freq cycle 3 voice 3
    stx CurInstTableV3+10 ; store current (to reload) freq cycle 3 voice 3
    ldx InstTableV3+9 ; read freq cycle 2 voice 3
    stx CurInstTableV3+9 ; store current (to reload) freq cycle 2 voice 3
    ldx InstTableV3+8 ; read freq cycle 1 voice 3
    stx CurInstTableV3+8 ; store current (to reload) freq cycle 1 voice 3
    ldx InstTableV3+7 ; read freq high add 4 voice 3
    stx CurInstTableV3+7 ; store current freq high add 4 voice 3
    ldx InstTableV3+6 ; read freq low add 4 voice 3
    stx CurInstTableV3+6 ; store current freq low add 4 voice 3
    ldx InstTableV3+5 ; read freq high add 3 voice 3
    stx CurInstTableV3+5 ; store current freq high add 3 voice 3
    ldx InstTableV3+4 ; read freq low add 3 voice 3
    stx CurInstTableV3+4 ; store current freq low add 3 voice 3
    ldx InstTableV3+3 ; read freq high add 2 voice 3
    stx CurInstTableV3+3 ; store current freq high add 2 voice 3
    ldx InstTableV3+2 ; read freq low add 2 voice 3
    stx CurInstTableV3+2 ; store current freq low add 2 voice 3
    ldx InstTableV3+1 ; read freq high add 1 voice 3
    stx CurInstTableV3+1 ; store current freq high add 1 voice 3
    ldx InstTableV3+0 ; read freq low add 1 voice 3
    stx CurInstTableV3+0 ; store current freq low add 1 voice 3
    and #$08
    beq reloadFCV3
    lda $FF ; read store note to play
    cmp #$60
    bcc alreadyReduced2V3
    sbc #$5F
alreadyReduced2V3:
    adc $EE
    sta CurInstTableV3+10 ; store current (to reload) freq cycle 3 voice 3
    bne skipReloadFCV3
reloadFCV3:
    jsr reloadFreqCycleV3
skipFreqSetV3:
skipReloadFCV3:
    ldx InstTableV3+27
    ldy InstTableV3+28
    stx CurInstTableV3+27
    sty CurInstTableV3+28
setDurationV3:
    ldy #$01
    lda ($E4),y ; read next byte of pattern: duration index voice 3
    ldx $FF ; read stored note to play
    cpx #$60 ; <60h means custom durations
    bcs skipDurTableV3
    tax
    lda durationTable-1,x ; read duration from table
skipDurTableV3:
    sta $E8 ; store actual note duration voice 3
    lda #$02
    clc
    adc $E4
    sta $E4 ; update pointer for next note
    bcs fixHigh2V3
    rts
fixHigh2V3:
    inc $E5
exitRTS:
    rts

;=================================
; Make the timbre of voice 1
;=================================
makeTimbreV1:
    ldx CurInstTableV1+28
    beq exitRTS ; exit if 0
    lda CurInstTableV1+26 ; read current Control registers voice 1
    and #$08 ; test bit
    beq noTestB1
    lda $E6 ; read duration of note voice 1
    cmp CurInstTableV1+27
    bcs testIfWaveEffV1
    lda #$00
    sta CurInstTableV1+27
    lda CurInstTableV1+26 ; read current control registers voice 1
    and #$F6 ; gate and test bit to 0
    sta CurInstTableV1+26 ; store current control registers voice 1
    bne outControl1
noTestB1:
    lda CurInstTableV1+27
    bne r3066
    ldy CurInstTableV1+28
    iny
    beq testIfWaveEffV1
    dec CurInstTableV1+28
    bne testIfWaveEffV1
; hard restart
    ldx #$06
loopSid1:
    sta $D400,x ; Voice 1: Frequency control (lo byte)
    dex
    bpl loopSid1
testFilterV1:
    cmp voiceNumber ; voice number (0,1,2) on where apply filter
    bne exitRTS ; exit if not equal
    inx
    stx CurFilterTable+13 ; filter effect flag
    rts

r3066:
    ldy CurInstTableV1+27
r3069:
    iny
    beq testIfWaveEffV1
    dec CurInstTableV1+27
    bne testIfWaveEffV1
    lda CurInstTableV1+26 ; read current control registers voice 1
    and #$F6 ; gate and test bit to 0
outControl1:
    sta $D404 ; Voice 1: Control registers
testIfWaveEffV1:
    lda CurInstTableV1+17 ; read current wave effect flag voice 1
    beq testIfFreqEffV1 ; no wave if 0
    lda CurInstTableV1+16 ; read current wave initial delay voice 1
    beq waveAdd1V1
    dec CurInstTableV1+16 ; dec delay voice 1
    jmp testIfFreqEffV1

;=================================
; wave add cycle 1 voice 1
;=================================
waveAdd1V1:
    clc
    ldx $F1 ; wave low v1 voice 1
    ldy $F2 ; wave high v1 voice 1
    lda CurInstTableV1+33 ; wave cycle 1 voice 1
    beq waveAdd2V1 ; goto cycle 2 if 0
    dec CurInstTableV1+33 ; decrement wave cycle 1 voice 1
    txa
    adc CurInstTableV1+18 ; add current low wave value for cycle 1 voice 1
    tax
    tya
    adc CurInstTableV1+19 ; add current high wave value for cycle 1 voice 1
    tay
    jmp changeWave1

;=================================
; wave add cycle 2 voice 1
;=================================

waveAdd2V1:
    lda CurInstTableV1+34 ; wave cycle 2 voice 1
    beq resetWave1
    dec CurInstTableV1+34 ; decrement wave cycle 2 voice 1
    txa
    adc CurInstTableV1+20 ; add current low value for cycle 2 voice 1
    tax
    tya
    adc CurInstTableV1+21 ; add current high value for cycle 2 voice 1
    tay
    jmp changeWave1

;=================================
; Reset Wave voice 1
; Flag meanings:
; xyyy yyyyz
; x=1 -> reload the wave cycle with wave value of istrument table again
; z=1 -> reload the wave cycle but use the actual wave value
; y=1 -> continue with actual wave, no more cycle
;=================================
resetWave1:
    lda CurInstTableV1+17 ; read current wave effect flag voice 1
    and #$81
    beq changeWave1 ; go to continue with actual wave
    bpl useCycleActualV1
    jsr reloadWaveCycleV1 ; reload wave cycle using wave in current ins. table
    jmp waveAdd1V1
useCycleActualV1:
    jsr reloadWCycleV1 ; reload wave cycle using wave in register
    jmp waveAdd1V1

;=================================
; change wave setting of voice 1
;=================================
changeWave1:
    stx $F1 ; wave low v1
    sty $F2 ; wave high v1
    stx $D402 ; Voice 1: Wave form pulsation amplitude (lo byte)
    sty $D403 ; Voice 1: Wave form pulsation amplitude (hi byte)
testIfFreqEffV1:
    lda CurInstTableV1+13 ; read current freq. flag effect voice 1
    beq exitCW1 ; exit if zero
    ldx $F7 ; low of freq.
    ldy $F8 ; high of freq.
    clc
    lda CurInstTableV1+12 ; read current freq. initial delay voice 1
    beq freqAdd1V1_
    dec CurInstTableV1+12 ; dec current freq. initial delay voice 1
    lda CurInstTableV1+13 ; read current freq. flag effect voice 1
    and #$02
    bne addF4V1
exitCW1:
    rts

;==================================
; freq add cycle1 voice 1
;==================================
freqAdd1V1:
    clc
freqAdd1V1_:
    lda CurInstTableV1+29 ; read current freq cycle 1 voice 1
    beq freqAdd2V1
    dec CurInstTableV1+29 ; dec current freq cycle 1 voice 1
    txa
    adc CurInstTableV1+0 ; add current low freq value for cycle 1 voice 1
    tax
    tya
    adc CurInstTableV1+1 ; add current high freq value for cycle 1 voice 1
    jmp change_freq1

;=================================
; freq add cycle2 voice 1
;=================================
freqAdd2V1:
    lda CurInstTableV1+30 ; read current freq cycle 2 voice 1
    beq freqAdd3V1
    dec CurInstTableV1+30 ; dec current freq cycle 2 voice 1
    txa
    adc CurInstTableV1+2 ; add current freq low add 2 voice 1
    tax
    tya
    adc CurInstTableV1+3 ; add current freq high add 2 voice 1
    jmp change_freq1

;=================================
; freq add cycle3 voice 1
;=================================
freqAdd3V1:
    lda CurInstTableV1+31 ; read current freq cycle 3 voice 1
    beq freqAdd4V1
    dec CurInstTableV1+31 ; dec current freq cycle 3 voice 1
    txa
    adc CurInstTableV1+4 ; add current freq low add 3 voice 1
    tax
    tya
    adc CurInstTableV1+5 ; add current freq high add 3 voice 1
    jmp change_freq1

;=================================
; freq add cycle4 voice 1
;=================================
freqAdd4V1:
    lda CurInstTableV1+32 ; read current freq cycle 4 voice 1
    beq resetFreqV1
    dec CurInstTableV1+32 ; dec current freq cycle 4 voice 1
addF4V1:
    txa
    adc CurInstTableV1+6 ; add current freq low add 4 voice 1
    tax
    tya
    adc CurInstTableV1+7 ; add current freq high add 4 voice 1

;=================================
; change frequecy: X=lo, A=hi voice 1
;=================================
change_freq1:
    tay
useCurrF1: ; use current frequency
    stx $D400 ; Voice 1: Frequency control (lo byte)
    sty $D401 ; Voice 1: Frequency control (hi byte)
    stx $F7
    sty $F8
exitUseCurrF1:
    rts

;=================================
; Reset Frequency Voice 1
; Flag meandings:
; xyyy yyyyz
; x=1 -> reload the freq. cycle with freq. value of istrument table again
; z=1 -> reload the freq. cycle but use the actual freq value
; y=1 -> continue with actual freq., no more cycle
;=================================
resetFreqV1:
    lda CurInstTableV1+13 ; read current freq. flag effect voice 1
    and #$81
    beq useCurrF1 ; go to continue with actual freq.
    bpl useCycleFActualV1
    jsr reloadFreqCycleV1
    jmp freqAdd1V1
useCycleFActualV1:
    jsr reloadFCycleV1
    jmp freqAdd1V1

;==================================
; Make the timbre of voice 2
;==================================
makeTimbreV2:
    ldx CurInstTableV2+28
    beq exitUseCurrF1
    lda CurInstTableV2+26 ; read current control registers voice 2
    and #$08
    beq noTestB2
    lda $E7 ; read actual note duration voice 2
    cmp CurInstTableV2+27
    bcs testIfWaveEffV2
    lda #$00
    sta CurInstTableV2+27
    lda CurInstTableV2+26 ; read current control registers voice 2
    and #$F6
    sta CurInstTableV2+26 ; store current control registers voice 2
    bne outControl2
noTestB2:
    lda CurInstTableV2+27
    bne r319F
    ldy CurInstTableV2+28
    iny
    beq testIfWaveEffV2
    dec CurInstTableV2+28
    bne testIfWaveEffV2

; hard restart
    ldx #$06
loopSid2:
    sta $D407,x ; Voice 2: Frequency control (lo byte)
    dex
    bpl loopSid2
testFilterV2:
    lda #$01
    jmp testFilterV1
r319F:
    ldy CurInstTableV2+27
    iny
    beq testIfWaveEffV2
    dec CurInstTableV2+27
    bne testIfWaveEffV2
    lda CurInstTableV2+26 ; read current control registers voice 2
    and #$F6 ; gate and test bit to 0
outControl2:
    sta $D40B ; Voice 2: Control registers
testIfWaveEffV2:
    lda CurInstTableV2+17 ; read current wave effect flag voice 2
    beq testIfFreqEffV2 ; no wave if 0
    lda CurInstTableV2+16 ; read current wave initial delay voice 2
    beq waveAdd1V2
    dec CurInstTableV2+16 ; dec current wave initial delay voice 2
    jmp testIfFreqEffV2

;==================================
; wave add cycle 1 voice 2
;==================================
waveAdd1V2:
    clc
    ldx $F3 ; wave low v1 voice 2
    ldy $F4 ; wave high v1 voice 2
    lda CurInstTableV2+33 ; wave cycle 1 voice 2
    beq waveAdd2V2 ; goto cycle 2 if 0
    dec CurInstTableV2+33 ; decrement wave cycle 1 voice 2
    txa
    adc CurInstTableV2+18 ; add current low wave value for cycle 1 voice 2
    tax
    tya
    adc CurInstTableV2+19 ; add current high wave value for cycle 1 voice 2
    tay
    jmp changeWave2

;=================================
; wave add cycle 2 voice 2
;=================================
waveAdd2V2:
    lda CurInstTableV2+34 ; wave cycle 2 voice 2
    beq resetWave2
    dec CurInstTableV2+34 ; decrement wave cycle 2 voice 2
    txa
    adc CurInstTableV2+20 ; add current low value for cycle 2 voice 2
    tax
    tya
    adc CurInstTableV2+21 ; add current high value for cycle 2 voice 2
    tay
    jmp changeWave2

;=================================
; Reset Wave voice 2
; Flag meanings:
; xyyy yyyyz
; x=1 -> reload the wave cycle with wave value of istrument table again
; z=1 -> reload the wave cycle but use the actual wave value
; y=1 -> continue with actual wave, no more cycle
;=================================
resetWave2:
    lda CurInstTableV2+17 ; read current wave effect flag voice 2
    and #$81
    beq changeWave2 ; go to continue with actual wave
    bpl useCycleActualV2
    jsr reloadWaveCycleV2 ; reload wave cycle using wave in current ins. table
    jmp waveAdd1V2
useCycleActualV2:
    jsr reloadWCycleV2 ; reload wave cycle using wave in register
    jmp waveAdd1V2

;=================================
; change wave setting of voice 2
;=================================
changeWave2:
    stx $F3 ; wave low v2
    sty $F4 ; wave high v2
    stx $D409 ; Voice 2: Wave form pulsation amplitude (lo byte)
    sty $D40A ; Voice 2: Wave form pulsation amplitude (hi byte)
testIfFreqEffV2:
    lda CurInstTableV2+13 ; read current freq. flag effect voice 2
    beq exitCW2 ; exit if zero
    and #$08
    bne r322E
    ldx $F9
    ldy $FA
    clc
    lda CurInstTableV2+12 ; current freq. initial delay voice 2
    beq freqAdd1V2_
    dec CurInstTableV2+12 ; dec current freq. initial delay voice 2
    lda CurInstTableV2+13 ; read current freq. flag effect voice 2
    and #$02
    bne addF4V2
exitCW2:
    rts

r322E:
    dec CurInstTableV2+4 ; dec current freq low add 3 voice 2
    bne exitCW2
    ldy CurInstTableV2+6 ; read current freq low add 4 voice 2
    sty CurInstTableV2+4 ; store current freq low add 3 voice 2
    ldy CurInstTableV2+12 ; read current freq. initial delay voice 2
    bpl r3241
    ldy CurInstTableV2+11 ; read current (to reload) freq cycle 4 voice 2
r3241:
    ldx CurInstTableV2+8 ; read current (to reload) freq cycle 1 voice 2
    stx $EF
    ldx CurInstTableV2+9 ; read current (to reload) freq cycle 2 voice 2
    stx $F0
    lda CurInstTableV2+10 ; read current (to reload) freq cycle 3 voice 2
    clc
    adc ($EF),y
    dey
    sty CurInstTableV2+12 ; store current freq. initial delay voice 2
    tay
    ldx highFreq,y ; read freq low from table
    lda lowFreq,y ; read freq high from table
    stx $D407 ; Voice 2: Frequency control (lo byte)
    sta $D408 ; Voice 2: Frequency control (hi byte)
    rts

;==================================
; freq add cycle1 voice 2
;==================================
freqAdd1V2:
    clc
freqAdd1V2_:
    lda CurInstTableV2+29 ; read current freq cycle 1 voice 2
    beq freqAdd2V2
    dec CurInstTableV2+29 ; dec current freq cycle 1 voice 2
    txa
    adc CurInstTableV2+0 ; add current low freq value for cycle 1 voice 2
    tax
    tya
    adc CurInstTableV2+1 ; add current high freq value for cycle 1 voice 2
    jmp change_freq2

;=================================
; freq add cycle2 voice 2
;=================================
freqAdd2V2:
    lda CurInstTableV2+30 ; read current freq cycle 2 voice 2
    beq freqAdd3V2
    dec CurInstTableV2+30 ; dec current freq cycle 2 voice 2
    txa
    adc CurInstTableV2+2 ; add current freq low add 2 voice 2
    tax
    tya
    adc CurInstTableV2+3 ; add current freq high add 2 voice 2
    jmp change_freq2

;=================================
; freq add cycle3 voice 2
;=================================
freqAdd3V2:
    lda CurInstTableV2+31 ; read current freq cycle 3 voice 2
    beq freqAdd4V2
    dec CurInstTableV2+31 ; dec current freq cycle 3 voice 2
    txa
    adc CurInstTableV2+4 ; add current freq low add 3 voice 2
    tax
    tya
    adc CurInstTableV2+5 ; add current freq high add 3 voice 2
    jmp change_freq2

;=================================
; freq add cycle4 voice 2
;=================================
freqAdd4V2:
    lda CurInstTableV2+32 ; read current freq cycle 4 voice 2
    beq resetFreqV2
    dec CurInstTableV2+32 ; dec current freq cycle 4 voice 2
addF4V2:
    txa
    adc CurInstTableV2+6 ; add current freq low add 4 voice 2
    tax
    tya
    adc CurInstTableV2+7 ; add current freq high add 4 voice 2

;=================================
; change frequecy: X=lo, A=hi voice 2
;=================================
change_freq2:
    tay
useCurrF2: ; use current frequency
    stx $D407 ; Voice 2: Frequency control (lo byte)
    sty $D408 ; Voice 2: Frequency control (hi byte)
    stx $F9 ; freq. low voice 2
    sty $FA ; freq. high voice 2
exitCF2:
    rts

;=================================
; Reset Frequency voice 2
; Flag meandings:
; xyyy yyyyz
; x=1 -> reload the freq. cycle with freq. value of istrument table again
; z=1 -> reload the freq. cycle but use the actual freq value
; y=1 -> continue with actual freq., no more cycle
;=================================
resetFreqV2:
    lda CurInstTableV2+13 ; read current freq. flag effect voice 2
    and #$81
    beq useCurrF2
    bpl useCycleFActualV2
    jsr reloadFreqCycleV2
    jmp freqAdd1V2
useCycleFActualV2:
    jsr reloadFCycleV2
    jmp freqAdd1V2

;==================================
; Make the timbre of the voice 3
;==================================
makeTimbreV3:
    ldx CurInstTableV3+28
    beq exitCF2
    lda CurInstTableV3+26 ; read current control registers voice 3
    and #$08
    beq noTestB3
    lda $E8 ; read actual note duration (length) voice 3
    cmp CurInstTableV3+27
    bcs testIfWaveEffV3
    lda #$00
    sta CurInstTableV3+27
    lda CurInstTableV3+26 ; read current control registers voice 3
    and #$F6
    sta CurInstTableV3+26 ; store current control registers voice 3
    bne outControl3

noTestB3:
    lda CurInstTableV3+27
    bne r3311
    ldy CurInstTableV3+28
    iny
    beq testIfWaveEffV3
    dec CurInstTableV3+28
    bne testIfWaveEffV3
; hard restart
    ldx #$06
loopSid3:
    sta $D40E,x ; Voice 3: Frequency control (lo byte)
    dex
    bpl loopSid3

testFilterV3:
    lda #$02
    jmp testFilterV1
r3311:
    ldy CurInstTableV3+27
    iny
    beq testIfWaveEffV3
    dec CurInstTableV3+27
    bne testIfWaveEffV3
    lda CurInstTableV3+26 ; read current control registers voice 3
    and #$F6 ; gate and test bit to 0
outControl3:
    sta $D412 ; Voice 3: Control registers
testIfWaveEffV3:
    lda CurInstTableV3+17 ; read wave effect flag voice 3
    beq testIfFreqEffV3
    lda $FB
    ora $FC
    beq testIfFreqEffV3
    lda CurInstTableV3+16 ; read current wave initial delay voice 3
    beq waveAdd1V3
    dec CurInstTableV3+16 ; dec current wave initial delay voice 3
    jmp testIfFreqEffV3

;=================================
; wave add cycle 1 voice 3
;=================================
waveAdd1V3:
    clc
    ldx $F5 ; read wave low v3
    ldy $F6 ; read wave high v3
    lda CurInstTableV3+33 ; read current wave cycle 1 voice 3
    beq waveAdd2V3
    dec CurInstTableV3+33 ; dec current wave cycle 1 voice 3
    txa
    adc CurInstTableV3+18 ; add current wave low add 1 voice 3
    tax
    tya
    adc CurInstTableV3+19 ; add current high add 1 voice 3
    tay
    jmp changeWave3

;=================================
; wave add cycle 2 voice 3
;=================================
waveAdd2V3:
    lda CurInstTableV3+34 ; read current wave cycle 2 voice 3
    beq resetWave3
    dec CurInstTableV3+34 ; dec current wave cycle 2 voice 3
    txa
    adc CurInstTableV3+20 ; add current wave low add 2 voice 3
    tax
    tya
    adc CurInstTableV3+21 ; add current wave high add 2 voice 3
    tay
    jmp changeWave3

;=================================
; Reset Wave voice 3
; Flag meanings:
; xyyy yyyyz
; x=1 -> reload the wave cycle with wave value of istrument table again
; z=1 -> reload the wave cycle but use the actual wave value
; y=1 -> continue with actual wave, no more cycle
;=================================
resetWave3:
    lda CurInstTableV3+17 ; wave effect flag voice 3
    and #$81
    beq changeWave3 ; go to continue with actual wave
    bpl useCycleActualV3
    jsr reloadWaveCycleV3 ; reload wave cycle using wave in current ins. table
    jmp waveAdd1V3
useCycleActualV3:
    jsr reloadWCycleV3 ; reload wave cycle using wave in register
    jmp waveAdd1V3

;=================================
; change wave setting of voice 3
;=================================
changeWave3:
    stx $F5 ; store wave low v3
    sty $F6 ; store wave high v3
    stx $D410 ; Voice 3: Wave form pulsation amplitude (lo byte)
    sty $D411 ; Voice 3: Wave form pulsation amplitude (hi byte)
testIfFreqEffV3:
    lda CurInstTableV3+13 ; read freq. effect flag voice 3
    beq exitCW3 ; exit if zero
    lda $FB ; read freq. low voice 3
    ora $FC ; freq. high voice 3
    beq exitCW3
    ldx $FB
    ldy $FC
    clc
    lda CurInstTableV3+12 ; read current freq. initial delay voice 3
    beq freqAdd1V3_
    dec CurInstTableV3+12 ; dec current freq. initial delay voice 3
    lda CurInstTableV3+13 ; read freq. effect flag voice 3
    and #$02
    bne addF4V3
exitCW3:
    rts

;==================================
; freq add cycle1 voice 3
;==================================
freqAdd1V3:
    clc
freqAdd1V3_:
    lda CurInstTableV3+29 ; read current freq cycle 1 voice 3
    beq freqAdd2V3
    dec CurInstTableV3+29 ; dec current freq cycle 1 voice 3
    txa
    adc CurInstTableV3+0 ; add current low freq value for cycle 1 voice 3
    tax
    tya
    adc CurInstTableV3+1 ; add current high freq value for cycle 1 voice 3
    jmp change_freq3

;=================================
; freq add cycle2 voice 3
;=================================
freqAdd2V3:
    lda CurInstTableV3+30 ; read current freq cycle 2 voice 3
    beq freqAdd3V3
    dec CurInstTableV3+30 ; dec current freq cycle 2 voice 3
    txa
    adc CurInstTableV3+2 ; add current freq low add 2 voice 3
    tax
    tya
    adc CurInstTableV3+3 ; add current freq high add 2 voice 3
    jmp change_freq3

;=================================
; freq add cycle3 voice 3
;=================================
freqAdd3V3:
    lda CurInstTableV3+31 ; read current freq cycle 3 voice 3
    beq freqAdd4V3
    dec CurInstTableV3+31 ; dec current freq cycle 3 voice 3
    txa
    adc CurInstTableV3+4 ; add current freq low add 3 voice 3
    tax
    tya
    adc CurInstTableV3+5 ; add current freq high add 3 voice 3
    jmp change_freq3

;=================================
; freq add cycle4 voice 3
;=================================
freqAdd4V3:
    lda CurInstTableV3+32 ; read current freq cycle 4 voice 3
    beq resetFreqV3
    dec CurInstTableV3+32 ; dec current freq cycle 4 voice 3
addF4V3:
    txa
    adc CurInstTableV3+6 ; add current freq low add 4 voice 3
    tax
    tya
    adc CurInstTableV3+7 ; add current freq high add 4 voice 3

;=================================
; change frequecy: X=lo, A=hi voice 3
;=================================
change_freq3:
    tay
useCurrF3: ; use current frequency
    stx $D40E ; Voice 3: Frequency control (lo byte)
    sty $D40F ; Voice 3: Frequency control (hi byte)
    stx $FB
    sty $FC
    rts

;=================================
; Reset Frequency voice 3
; Flag meandings:
; xyyy yyyyz
; x=1 -> reload the freq. cycle with freq. value of istrument table again
; z=1 -> reload the freq. cycle but use the actual freq value
; y=1 -> continue with actual freq., no more cycle
;=================================
resetFreqV3:
    lda CurInstTableV3+13 ; read current freq. flag effect voice 3
    and #$81
    beq useCurrF3
    bpl useCycleFActualV3
    jsr reloadFreqCycleV3
    jmp freqAdd1V3
useCycleFActualV3:
    jsr reloadFCycleV3
    jmp freqAdd1V3
;;3417
    lda playMusicV1 ; play music flag voice 1
    ora playMusicV2 ; play music flag voice 2
    ora playMusicV3 ; play music flag voice 3
    ora CurInstTableV1+28
    ora CurInstTableV2+28
    ora CurInstTableV3+28
    rts

;===================================
; Define the table with the track
; offset: last byte is the minime
; duration of a note
; The offset used by setTrack is
; calculated from the second byte
; of the table
;===================================
trackTable:
tune3:
    !by <tune3_voice1, >tune3_voice1
    !by <tune3_voice2, >tune3_voice2
    !by <tune3_voice3, >tune3_voice3
    !by $09 ; min note duration
tune4:
    !by <tune4_voice1, >tune4_voice1
    !by <tune4_voice2, >tune4_voice2
    !by <tune4_voice3, >tune4_voice3
    !by $0B ; min note duration
tune5:
    !by <tune5_voice1, >tune5_voice1
    !by <tune5_voice2, >tune5_voice2
    !by <tune5_voice3, >tune4_voice3
    !by $09 ; min note duration
;343F
    !by $DD, $DD
    !by $DD, $DD
    !by $DD, $DD
    !by $DD

tune6:
    !by <tune6_voice1, >tune6_voice1
    !by <tune6_voice2, >tune6_voice2
    !by <tune6_voice3, >tune6_voice3
    !by $0D ; min note duration
tune2:
    !by <tune2_voice1, >tune2_voice1
    !by <tune2_voice2, >tune2_voice2
    !by <tune2_voice3, >tune2_voice3
    !by $03 ; min note duration
tune7:
    !by <tune7_voice1, >tune7_voice1
    !by <tune7_voice2, >tune7_voice2
    !by <tune7_voice3, >tune7_voice3
    !by $0B ; min note duration
tune8:
    !by <tune8_voice1, >tune8_voice1
    !by <tune8_voice2, >tune8_voice2
    !by <tune8_voice3, >tune8_voice3
    !by $07 ; min note duration
tune1:
    !by <tune1_voice1, >tune1_voice1
    !by <tune1_voice2, >tune1_voice2
    !by <tune1_voice3, >tune1_voice3
    !by $04 ; min note duration

; Instrument 01 definition
ins01:
    !by $14, $00 ; freq low/high add 1
    !by $EC, $FF ; freq low/high add 2
    !by $14, $00 ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $03, $06 ; freq cycle 1/2
    !by $03, $00 ; freq cycle 3/4
    !by $1E, $05 ; freq delay initial/freq effect flag
    !by $32, $32 ; wave cycle 1/2
    !by $0A, $05 ; wave delay initial/wave effect flag
    !by $0A, $00 ; wave low/high add 1
    !by $F6, $FF ; wave low/high add 2
    !by $00, $08 ; wave low/high
    !by $41 ; control to rectangular
    !by $14, $C8 ; AD/SR
    !by $FF, $FA

; Instrument 02 definition
ins02:
    !by $23, $00 ; freq low/high add 1
    !by $DD, $FF ; freq low/high add 2
    !by $23, $00 ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $03, $05 ; freq cycle 1/2
    !by $02, $00 ; freq cycle 3/4
    !by $0A, $05 ; freq delay initial/freq effect flag
    !by $00, $00 ; wave not used
    !by $00, $00
    !by $00, $00
    !by $00, $00
    !by $00, $00
    !by $19 ; control to triangular + test bit
    !by $A4, $F9 ; AD SR
    !by $14, $FE

tune1_voice1:
    !by $D2, $1C, <ins01, >ins01 ; SETNI instr: set all the instrument table
    !by $5F, $20
    !by $1D, $20
    !by $CC, $03 ; FOR instr: repeat 3h times
    !by $5F, $20
    !by $CE ; NEXT instr
    !by $1F, $20
    !by $CC, $03 ; FOR instr: repeat 3h times
    !by $5F, $20
    !by $CE ; NEXT instr
    !by $DE, $0C, $80, $07 ; SET2CI instr: delay initial | freq effect flag
    !by $DE, $06, $3C, $00 ; SET2CI instr: freq low|high add 4
    !by $C6, $F4, <sub01, >sub01 ; JSRT: execute subroutine with transpose
    !by $CA, $1A, $8D ; SET instr: set SR of instrument
    !by $C6, $00, <sub_1, >sub_1 ; JSRT: execute subroutine with transpose
    !by $D6, $1B, $01 ; SETCI instr (??????????????????)
    !by $D2, $1C, <ins02, >ins02 ; SETNI instr: set all the instrument table
    !by $CC, $04 ; FOR instr: repeat 4h times
    !by $5F, $20
    !by $CE ; NEXT instr
    !by $CC, $04 ; FOR inst: repeat 4h times
    !by $43, $10
    !by $42, $10
    !by $40, $10
    !by $3E, $10
    !by $3C, $10
    !by $3B, $10
    !by $39, $10
    !by $37, $10
    !by $CE ; NEXT instr
    !by $C0 ; RTS instruction

; instrument 03 definition
ins03:
    !by $14, $00 ; freq low/high add 1
    !by $EC, $FF ; freq low/high add 2
    !by $14, $00 ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $03, $06 ; freq cycle 1/2
    !by $03, $00 ; freq cycle 3/4
    !by $0A, $05 ; freq delay initial/freq effect flag
    !by $32, $32 ; wave cycle 1/2
    !by $14, $05 ; wave delay initial/wave effect flag
    !by $0A, $00 ; wave low/high add 1
    !by $F6, $FF ; wave low/high add 2
    !by $00, $08 ; wave low/high
    !by $41 ; control to rectangular
    !by $14, $C4 ; AD/SR
    !by $FF, $05

arp39:
    !by $0C, $00, $03, $09
arp37:
    !by $0C, $00, $03, $07
arp5b:
    !by $0C, $00, $05, $0B
arp59:
    !by $0C, $00, $05, $09

; instrument 04 definition
ins04:
    !by $00, $00 ; freq low/high add 1
    !by $00, $00 ; freq low/high add 2
    !by $01, $00 ; freq low/high add 3
    !by $01, $00 ; freq low/high add 4
    !by $07, $35 ; freq cycle 1/2
    !by $00, $03 ; freq cycle 3/4
    !by $00, $08 ; freq delay initial/freq effect flag
    !by $32, $32 ; wave cycle 1/2
    !by $14, $00 ; wave delay initial/wave effect flag
    !by $0A, $00 ; wave low/high add 1
    !by $F6, $FF ; wave low/high add 2
    !by $00, $08 ; wave low/high
    !by $41 ; control: rectangular
    !by $01, $F7 ; AD/SR
    !by $04, $14

tune1_voice2:
    !by $D2, $1C, <ins03, >ins03 ; SETNI instr: set all the instrument table
    !by $5F, $20
    !by $1D, $20
    !by $CC, $03 ; FOR inst: repeat 3h times
    !by $5F, $20
    !by $CE ; NEXT instr
    !by $1F, $20
    !by $CC, $03 ; FOR inst: repeat 3h times
    !by $5F, $20
    !by $CE ; NEXT instr
    !by $DE, $0C, $80, $07 ; SET2CI instr: set freq delay initial/freq effect flag
    !by $DE, $06, $28, $00 ; SET2CI instr: set freq low/high add 4
    !by $D2, $1C, <ins04, >ins04 ; SETNI instr: set all the instrument table
    !by $5F, $20
    !by $CC, $02 ; FOR inst: repeat 2h times (level 1)
    !by $DC, $08, <arp39, >arp39 ; SET2I instr: freq cycle 1/2
    !by $CC, $10 ; FOR inst: repeat 10h times (level 2)
    !by $39, $02
    !by $CE ; NEXT (level 2)
    !by $DC, $08, <arp37, >arp37 ; SET2I instr: freq cycle 1/2
    !by $CC, $10 ; FOR inst: repeat 10h times (level 2)
    !by $39, $02
    !by $CE ; NEXT (level 2)
    !by $DC, $08, <arp5b, >arp5b ; SET2I instr: freq cycle 1/2
    !by $CC, $10 ; FOR inst: repeat 10h times (level 2)
    !by $37, $02
    !by $CE ; NEXT (level 2)
    !by $DC, $08, <arp59, >arp59 ; SET2I instr: freq cycle 1/2
    !by $CC, $10 ; FOR inst: repeat 10h times (level 2)
    !by $37, $02
    !by $CE ; NEXT (level 2)
    !by $CE ; NEXT (level 1)
    !by $CC, $07 ; FOR inst: repeat 7h times (level 1)
    !by $DC, $08, <arp39, >arp39 ; SET2I instr: freq cycle 1/2
    !by $CC, $03 ; FOR inst: repeat 3h times (level 2)
    !by $CA, $0D, $08 ; SETI: set freq effect flag on
    !by $39, $02
    !by $CA, $0D, $00 ; SETI: set freq effect flag off
    !by $39, $02
    !by $CE ; NEXT (level 2)
    !by $45, $02
    !by $CA, $0D, $08 ; SETI: set freq effect flag on
    !by $39, $02
    !by $CA, $0D, $00 ; SETI: set freq effect flag off
    !by $45, $02
    !by $CA, $0D, $08 ; SETI: set freq effect flag on
    !by $39, $02
    !by $CA, $0D, $00 ; SETI: set freq effect flag off
    !by $39, $02
    !by $45, $02
    !by $CA, $0D, $08 ; SETI: set freq effect flag on
    !by $39, $02
    !by $CA, $0D, $00 ; SETI: set freq effect flag off
    !by $39, $02
    !by $39, $02
    !by $39, $02
    !by $DC, $08, <arp37, >arp37 ; SET2I instr: freq cycle 1/2
    !by $CA, $0D, $08 ; SETI: set freq effect flag on
    !by $39, $02
    !by $CA, $0D, $00 ; SETI: set freq effect flag off
    !by $39, $02
    !by $CA, $0D, $08 ; SETI: set freq effect flag on
    !by $CC, $04 ; FOR inst: repeat 4h times (level 2)
    !by $39, $02
    !by $CE ; NEXT (level 2)
    !by $CC, $02 ; FOR inst: repeat 2h times (level 2)
    !by $CA, $0D, $00 ; SETI: set freq effect flag off
    !by $45, $02
    !by $CA, $0D, $08 ; SETI: set freq effect flag on
    !by $39, $02
    !by $CE ; NEXT (level 2)
    !by $39, $02
    !by $CA, $0D, $00 ; SETI: set freq effect flag off
    !by $45, $02
    !by $CA, $0D, $08 ; SETI: set freq effect flag on
    !by $39, $02
    !by $CA, $0D, $00 ; SETI: set freq effect flag off
    !by $45, $02
    !by $39, $02
    !by $39, $02
    !by $DC, $08, <arp5b, >arp5b ; SET2I instr: freq cycle 1/2
    !by $CC, $03 ; FOR inst: repeat 3h times (level 2)
    !by $CA, $0D, $08 ; SETI: set freq effect flag on
    !by $37, $02
    !by $CA, $0D, $00 ; SETI: set freq effect flag off
    !by $37, $02
    !by $CE ; NEXT (level 2)
    !by $48, $02
    !by $CA, $0D, $08 ; SETI: set freq effect flag on
    !by $37, $02
    !by $CA, $0D, $00 ; SETI: set freq effect flag off
    !by $48, $02
    !by $CA, $0D, $08 ; SETI: set freq effect flag on
    !by $37, $02
    !by $CA, $0D, $00 ; SETI: set freq effect flag off
    !by $37, $02
    !by $48, $02
    !by $CA, $0D, $08 ; SETI: set freq effect flag on
    !by $37, $02
    !by $CA, $0D, $00 ; SETI: set freq effect flag off
    !by $37, $02
    !by $37, $02
    !by $37, $02
    !by $DC, $08, <arp59, >arp59 ; SET2I instr: freq cycle 1/2
    !by $CA, $0D, $08 ; SETI: set freq effect flag on
    !by $37, $02
    !by $CA, $0D, $00 ; SETI: set freq effect flag off
    !by $37, $02
    !by $CA, $0D, $08 ; SETI: set freq effect flag on
    !by $CC, $04 ; FOR inst: repeat 3h times (level 2)
    !by $37, $02
    !by $CE ; NEXT (level 2)
    !by $CC, $02 ; FOR inst: repeat 3h times (level 2)
    !by $CA, $0D, $00 ; SETI: set freq effect flag off
    !by $48, $02
    !by $CA, $0D, $08 ; SETI: set freq effect flag on
    !by $37, $02
    !by $CE ; NEXT (level 2)
    !by $37, $02
    !by $CA, $0D, $00 ; SETI: set freq effect flag off
    !by $48, $02
    !by $CA, $0D, $08 ; SETI: set freq effect flag on
    !by $37, $02
    !by $CA, $0D, $00 ; SETI: set freq effect flag off
    !by $48, $02
    !by $37, $02
    !by $37, $02
    !by $CE ; NEXT (level 1)
    !by $C0 ; RTS instruction

; filter table 01 definition
fil01:
    !by $4D, $01 ; add filter low/high value 1
    !by $E2, $FF ; add filter low/high value 2
    !by $00, $00 ; add filter low/high value 3
    !by $00, $00 ; add filter low/high value 4
    !by $03 ; filter cycle 1
    !by $14 ; filter cycle 2
    !by $00 ; filter cycle 3
    !by $00 ; filter cycle 4
    !by $00 ; filter initial delay
    !by $04 ; filter effct flag
    !by $01 ; filter low value (8 bit)
    !by $00 ; filter high value (3 bit)

; instrument 05 definition
ins05:
    !by $14, $00 ; freq low/high add 1
    !by $EC, $FF ; freq low/high add 2
    !by $14, $00 ; freq low/high add 3
    !by $1C, $00 ; freq low/high add 4
    !by $03, $06 ; freq cycle 1/2
    !by $03, $00 ; freq cycle 3/4
    !by $28, $07 ; freq delay initial/freq effect flag
    !by $32, $32 ; wave cycle 1/2
    !by $28, $05 ; wave delay initial/wave effect flag
    !by $0A, $00 ; wave low/high add 1
    !by $F6, $FF ; wave low/high add 2
    !by $00, $08 ; wave low/high
    !by $41 ; control: rectangular
    !by $14, $C8 ; AD/SR
    !by $FF
    !by $32

; filter table 02 definition
fil02:
    !by $4D, $01 ; add filter low/high value 1
    !by $D3, $FF ; add filter low/high value 2
    !by $FB, $FF ; add filter low/high value 3
    !by $FF, $FF ; add filter low/high value 4
    !by $03, $14 ; filter cycle 1/2
    !by $0A, $32 ; filter cycle 3/4
    !by $00 ; filter initial delay
    !by $04 ; filter effect flag
    !by $01 ; filter low value (8 bit)
    !by $00 ; filter high value (3 bit)

ins06: ; instrument 06 definition
insF01: ; instrument frequency table 1
    !by $19, $00 ; freq low/high add 1
    !by $E7, $FF ; freq low/high add 2
    !by $19, $00 ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $02, $04 ; freq cycle 1/2
    !by $02, $00 ; freq cycle 3/4
    !by $06, $05 ; freq delay initial/freq effect flag
    !by $32, $32 ; wave cycle 1/2
    !by $00, $05 ; wave delay initial/wave effect flag
    !by $14, $00 ; wave low/high add 1
    !by $EC, $FF ; wave low/high add 2
    !by $00, $06 ; wave low/high
    !by $41 ; control: rectangular
    !by $14, $E8 ; AD/SR
    !by $1E
    !by $28

insF02: ; instrument frequency table 2
    !by $20, $00 ; freq low/high add 1
    !by $00, $00 ; freq low/high add 2
    !by $00, $00 ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $FF, $00 ; freq cycle 1/2
    !by $00, $00 ; freq cycle 3/4
    !by $0A, $04 ; freq delay initial/freq effect flag

sub01:
    !by $5F, $20
sub_1:
    !by $21, $20
    !by $5F, $1A
    !by $23, $06
    !by $24, $20
    !by $5F, $1A
    !by $26, $02
    !by $28, $02
    !by $24, $02
    !by $21, $20
    !by $5F, $1A
    !by $23, $06
    !by $24, $20
    !by $5F, $20
    !by $C0 ; RTS instruction

sub02:
    !by $21, $04
    !by $21, $04
    !by $2D, $01
    !by $50, $01
    !by $21, $04
    !by $21, $08
    !by $21, $02
    !by $2D, $02
    !by $1C, $02
    !by $1F, $04
    !by $21, $04
    !by $21, $02
    !by $21, $02
    !by $2D, $01
    !by $50, $01
    !by $21, $04
    !by $21, $04
    !by $21, $04
    !by $21, $02
    !by $2D, $02
    !by $21, $02
    !by $23, $04
    !by $24, $04
    !by $24, $02
    !by $24, $02
    !by $30, $01
    !by $50, $03
    !by $24, $02
    !by $24, $04
    !by $24, $02
    !by $24, $02
    !by $24, $02
    !by $30, $02
    !by $F0, <insF02, >insF02 ; SETFI: set frequency effect for instrument
    !by $1F, $06
    !by $F0, <insF01, >insF01 ; SETFI: set frequency effect for instrument
    !by $24, $02
    !by $18, $02
    !by $24, $02
    !by $18, $02
    !by $30, $04
    !by $24, $02
    !by $24, $04
    !by $24, $02
    !by $34, $02
    !by $24, $02
    !by $30, $02
    !by $28, $02
    !by $2B, $02
    !by $2D, $02
    !by $C0 ; RTS instruction

exec01:
    lda #$10
    sta TEMP
    rts
exec02:
    lda #$00
    sta TEMP
    sta $D417 ; Filter resonance control/voice input control
    sta voiceNumber ; voice number (0,1,2) on where apply filter
    rts

tune1_voice3:
    !by $E2, <fil02, >fil02 ; FILTA instr: set filter table
    !by $D2, $1C, <ins05, >ins05 ; SETNI instr: set all the instrument table
    !by $BF, $58
    !by $65, $28
    !by $CC, $04 ; FOR inst: repeat 4h times
    !by $5F, $20
    !by $CE ; NEXT instr
    !by $CA, $0D, $05 ; SETI: set freq effect flag on
    !by $1F, $20
    !by $CC, $03 ; FOR inst: repeat 3h times
    !by $5F, $20
    !by $CE ; NEXT instr
    !by $DE, $0C, $80, $07 ; SET2CI instr: delay initial | freq effect flag
    !by $DE, $06, $14, $00 ; SET2CI instr: freq low|high add 4
    !by $CA, $0D, $05 ; SETI: set freq effect flag on
    !by $C2, <sub01, >sub01 ; JSR instruction
    !by $D2, $1C, <ins06 >ins06 ; SETNI instr: set all the instrument table
    !by $E0
    !by $D8, <exec01, >exec01 ; EXCT: execute given address code
    !by $C2, <sub02, >sub02 ; JSR instruction: execute subroutine
    !by $C2, <sub02, >sub02 ; JSR instruction: execute subroutine
    !by $D2, $1C, <ins02, >ins02 ; SETNI instr: set all the instrument table
    !by $E2, <fil01, >fil01 ; FILTA instr: set filter table
    !by $CC, $03 ; FOR inst: repeat 3h times
    !by $5F, $20
    !by $CE ; NEXT instr

    !by $CC, $02 ; FOR inst: repeat 2h times
    !by $43, $10
    !by $42, $10
    !by $40, $10
    !by $3E, $10
    !by $3C, $10
    !by $3B, $10
    !by $39, $10
    !by $37, $10
    !by $CE ; NEXT instr
    !by $43, $10
    !by $A2, $17
    !by $E2, <fil02, >fil02 ; FILTA instr: set filter table
    !by $D2, $1C, <ins05, >ins05 ; SETNI instr: set all the instrument table
    !by $BF, $01
    !by $65, $28
    !by $D2, $1C, <ins06, >ins06 ; SETNI instr: set all the instrument table
    !by $C2, <sub02, >sub02 ; JSR instruction: execute subroutine
    !by $C2, <sub02, >sub02 ; JSR instruction: execute subroutine
    !by $D8, <exec02, >exec02 ; EXCT: execute given address code: filter to 0
    !by $5F, $10
    !by $CC, $0A ; FOR inst: repeat Ah times
    !by $5F, $20
    !by $CE ; NEXT instr
    !by $C0 ; RTS instruction

; instrument 07 definition
ins07:
    !by $1E, $00 ; freq low/high add 1
    !by $E2, $FF ; freq low/high add 2
    !by $1E, $00 ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $03, $05 ; freq cycle 1/2
    !by $02, $00 ; freq cycle 3/4
    !by $08, $05 ; freq delay initial/freq effect flag
    !by $00, $00 ; wave cycle 1/2
    !by $00, $00 ; wave delay initial/wave effect flag
    !by $00, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $08 ; wave low/high
    !by $49 ; control: rectangular, test bit on
    !by $06, $99 ; AD/SR
    !by $04, $28
sub03:
    !by $37, $04
    !by $39, $02
    !by $3A, $02
    !by $39, $03
    !by $5F, $01
    !by $37, $03
    !by $5F, $01
    !by $C0 ; RTS instruction
sub04:
    !by $39, $03
    !by $5F, $01
    !by $3E, $03
    !by $5F, $01
    !by $39, $07
    !by $5F, $01
    !by $C0 ; RTS instruction
sub05:
    !by $C2, <sub03, >sub03 ; JSR instruction: execute subroutine
    !by $C2, <sub04, >sub04 ; JSR instruction: execute subroutine
    !by $C2, <sub03, >sub03 ; JSR instruction: execute subroutine
    !by $37, $04
    !by $36, $04
    !by $37, $04
    !by $39, $04
    !by $C2, <sub03, >sub03 ; JSR instruction: execute subroutine
    !by $C2, <sub04, >sub04 ; JSR instruction: execute subroutine
    !by $3A, $04
    !by $3C, $02
    !by $3E, $02
    !by $3C, $03
    !by $5F, $01
    !by $3A, $03
    !by $5F, $01
    !by $3C, $03
    !by $5F, $01
    !by $41, $03
    !by $5F, $01
    !by $3C, $07
    !by $5F, $01
    !by $CC, $02 ; FOR inst: repeat 2h times (level 1)
    !by $CC, $03 ; FOR inst: repeat 3h times (level 2)
    !by $45, $02
    !by $45, $02
    !by $5F, $04
    !by $CE ; NEXT instr (level 2)
    !by $45, $02
    !by $43, $02
    !by $41, $02
    !by $3C, $02
    !by $3E, $1C
    !by $5F, $04
    !by $CE ; NEXT instr (level 1)
    !by $C0 ; RTS instruction

tune2_voice1:
    !by $D2, $1C, <ins07, >ins07 ; SETNI instr: set all the instrument table
    !by $BF, $01
t2v1_:
    !by $CC, $04 ; FOR inst: repeat 4h times
    !by $C2, <sub05, >sub05 ; JSR instruction: execute subroutine
    !by $CE ; NEXT instr
    !by $CC, $40 ; FOR inst: repeat 40h times
    !by $5F, $04 ; no sound (rest)
    !by $CE ; NEXT instr
    !by $C4, <t2v1_, >t2v1_ ; JMP instr.: jump to given address location

; instrument 08 definition
ins08:
    !by $1E, $00 ; freq low/high add 1
    !by $E2, $FF ; freq low/high add 2
    !by $1E, $00 ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $02, $04 ; freq cycle 1/2
    !by $02, $00 ; freq cycle 3/4
    !by $06, $05 ; freq delay initial/freq effect flag
    !by $00, $00 ; wave cycle 1/2
    !by $00, $00 ; wave delay initial/wave effect flag
    !by $00, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $08 ; wave low/high
    !by $29 ; control: sawtooth and test bit on
    !by $06, $99 ; AD/SR
    !by $04
    !by $28
tune2_voice2:
    !by $D2, $1C, <ins08, >ins08 ; SETNI instr: set all the instrument table
    !by $BF, $01
t2v2_:
    !by $CC, $03 ; FOR inst: repeat 3h times
    !by $C2, <sub05, >sub05 ; JSR instruction: execute subroutine
    !by $5F, $02
    !by $CE ; NEXT instr
    !by $CC, $FA ; FOR inst: repeat FAh times
    !by $5F, $01
    !by $CE ; NEXT instr
    !by $C2, <sub05, >sub05 ; JSR instruction: execute subroutine
    !by $C4, <t2v2_, >t2v2_ ; JMP inst.: jump to given address location

; instrument 09 definition
ins09:
    !by $00, $00 ; freq low/high add 1
    !by $00, $00 ; freq low/high add 2
    !by $00, $00 ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $00, $00 ; freq cycle 1/2
    !by $00, $00 ; freq cycle 3/4
    !by $00, $00 ; freq delay initial/freq effect flag
    !by $FF, $00 ; wave cycle 1/2
    !by $01, $04 ; wave delay initial/wave effect flag
    !by $64, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $02 ; wave low/high
    !by $41 ; control: rectangular
    !by $06, $48 ; AD/SR
    !by $0A
    !by $14

sub06:
    !by $CC, $02 ; FOR inst: repeat 2h times
    !by $00, $02
    !by $00, $02
    !by $0C, $02
    !by $0C, $02
    !by $CE ; NEXT inst
    !by $C0 ; RTS instruction

sub07:
    !by $C6, $1F, <sub06, >sub06 ; JSRT: execute subroutine with transpose
    !by $C6, $1D, <sub06, >sub06 ; JSRT: execute subroutine with transpose
    !by $C6, $1B, <sub06, >sub06 ; JSRT: execute subroutine with transpose
    !by $C6, $1A, <sub06, >sub06 ; JSRT: execute subroutine with transpose
    !by $C0 ; RTS instruction

tune2_voice3:
    !by $D2, $1C, <ins09, >ins09 ; SETNI instr: set all the instrument table
    !by $BF, $01
t2v3_:
    !by $C2, <sub07, >sub07 ; JSR instruction: execute subroutine
    !by $C6, $1F, <sub06, >sub06 ; JSRT: execute subroutine with transpose
    !by $C6, $1D, <sub06, >sub06 ; JSRT: execute subroutine with transpose
    !by $C6, $1B, <sub06, >sub06 ; JSRT: execute subroutine with transpose
    !by $C6, $1D, <sub06, >sub06 ; JSRT: execute subroutine with transpose
    !by $C2, <sub07, >sub07 ; JSR instruction: execute subroutine
    !by $C2, <sub07, >sub07 ; JSR instruction: execute subroutine
    !by $C4, <t2v3_, >t2v3_ ; JMP instr.

par01:
    !by $11 ; control: triangle
    !by $23, $E4 ; AD/SR
    !by $14
    !by $0A
tune7_voice1:
    !by $D4, <par01, >par01 ; INSTR instruction: select instrument parameter
    !by $BF, $15
    !by $37, $01
    !by $3D, $01
    !by $3E, $01
    !by $41, $01
    !by $43, $01
    !by $47, $01
    !by $C0 ; RTS instruction
tune7_voice2:
    !by $D4, <par01, >par01 ; INSTR instruction: select instrument parameter
    !by $BF, $0B
    !by $37, $01
    !by $3D, $01
    !by $3E, $01
    !by $41, $01
    !by $43, $01
    !by $47, $01
    !by $4A, $01
    !by $C0 ; RTS instruction

par02:
    !by $41 ; control: rectangular
    !by $24, $A4 ; AD/SR
    !by $14
    !by $04

tune7_voice3:
    !by $D4, <par02, >par02 ; INSTR instruction: select instrument parameter
    !by $DC, $16, $00, $08 ; SET2I: set wave low/high of voice
    !by $BF, $01
    !by $37, $01
    !by $3D, $01
    !by $3E, $01
    !by $41, $01
    !by $43, $01
    !by $47, $01
    !by $4A, $01
    !by $C0 ; RTS instruction

; instrument frequency table 3
insF03:
    !by $EB, $F2 ; freq low/high add 1
    !by $00, $00 ; freq low/high add 2
    !by $15, $0D ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $01, $03 ; freq cycle 1/2
    !by $01, $09 ; freq cycle 3/4
    !by $09, $05 ; freq delay initial/freq effect flag

par03:
    !by $19 ; control: triangle, test bit on
    !by $13, $E4 ; AD/SR
    !by $02
    !by $0A

tune6_voice1:
    !by $D4, <par03, >par03 ; INSTR instruction: select instrument parameter
    !by $BF, $1B
t6v1_:
    !by $3A, $01
    !by $35, $01
    !by $37, $01
    !by $32, $01
    !by $35, $01
    !by $30, $01
    !by $32, $01
    !by $2E, $01
    !by $F0, <insF03, >insF03 ; SETFI: set frequency effect for instrument
    !by $2B, $03
    !by $CA, $0D, $00 ; SETI: set freq effect flag off
    !by $2B, $02
    !by $C0 ; RTS instr.

tune6_voice2:
    !by $D4, <par03, >par03 ; INSTR: select all instrument table
    !by $BF, $04
    !by $C4, <t6v1_, >t6v1_ ; JMP instr.: jump to given address location
par04:
    !by $49 ; control: rectangular + test
    !by $24, $A4 ; AD/SR
    !by $02, $04

tune6_voice3:
    !by $D4, <par04, >par04 ; INSTR inst.: select all instrument table
    !by $DC, $16, $00, $08 ; SET2I: set wave low/high of voice
    !by $BF, $01
    !by $C4, <t6v1_, >t6v1_ ; JMP instr.: jump to given address location

; instrument 0A definition
ins0A:
    !by $0E, $00 ; freq low/high add 1
    !by $F2, $FF ; freq low/high add 2
    !by $0E, $00 ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $05, $0A ; freq cycle 1/2
    !by $05, $00 ; freq cycle 3/4
    !by $14, $05 ; freq delay initial/freq effect flag
    !by $FF, $00 ; wave cycle 1/2
    !by $00, $04 ; wave delay initial/wave effect flag
    !by $0A, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $08 ; wave low/high
    !by $49 ; control: rectangular, test bit on
    !by $06, $98 ; AD/SR
    !by $05
    !by $1E

tune3_voice1:
    !by $D2, $1C, <ins0A, >ins0A ; SETNI instr: set all the instrument table
    !by $30, $03
    !by $30, $01
    !by $37, $08
    !by $96, $0C
    !by $97, $0C
    !by $99, $0C
    !by $37, $10
    !by $30, $03
    !by $30, $01
    !by $37, $08
    !by $39, $01
    !by $37, $01
    !by $36, $01
    !by $39, $01
    !by $37, $08
    !by $C0 ; RTS instruction

tune3_voice2:
    !by $D2, $1C, <ins0A, >ins0A ; SETNI instr: set all the instrument table
    !by $2B, $03
    !by $2B, $01
    !by $34, $08
    !by $93, $0C
    !by $94, $0C
    !by $95, $0C
    !by $34, $10
    !by $2B, $03
    !by $2B, $01
    !by $34, $08
    !by $35, $01
    !by $34, $01
    !by $33, $01
    !by $35, $01
    !by $34, $08
    !by $C0 ; RTS instruction

; filter table 03 definition
fil03:
    !by $4D, $01 ; add filter low/high value 1
    !by $D3, $FF ; add filter low/high value 2
    !by $14, $00 ; add filter low/high value 3
    !by $EC, $FF ; add filter low/high value 4
    !by $03, $14 ; filter cycle 1/2
    !by $32, $32 ; filter cycle 3/4
    !by $00 ; filter initial delay
    !by $04 ; filter effect flag
    !by $01 ; filter low value (8 bit)
    !by $00 ; filter high value (3 bit)

; instrument 0B definition
ins0B:
    !by $0F, $00 ; freq low/high add 1
    !by $F1, $FF ; freq low/high add 2
    !by $0F, $00 ; freq low/high add 3
    !by $00, $00 ; freq low/high add 4
    !by $04, $08 ; freq cycle 1/2
    !by $04, $00 ; freq cycle 3/4
    !by $0E, $05 ; freq delay initial/freq effect flag
    !by $19, $00 ; wave cycle 1/2
    !by $00, $04 ; wave delay initial/wave effect flag
    !by $0A, $00 ; wave low/high add 1
    !by $00, $00 ; wave low/high add 2
    !by $00, $08 ; wave low/high
    !by $49 ; control: rectangular and test bit on
    !by $06, $98 ; AD/SR
    !by $05
    !by $1E

tune3_voice3:
    !by $D2, $1C, <ins0B, >ins0B ; SETNI instr: set all the instrument table
    !by $E0 ; LF3: low filter (max resonance) on voice 3
    !by $E2, <fil03, >fil03 ; FILTA instr: set filter table
    !by $CC, $03 ; FOR inst: repeat 3h times
    !by $24, $04
    !by $1F, $04
    !by $CE ; NEXT instr
    !by $24, $02
    !by $1F, $02
    !by $21, $02
    !by $23, $02
    !by $CC, $02 ; FOR inst: repeat 2h times
    !by $24, $04
    !by $1F, $04
    !by $CE ; NEXT instr
    !by $24, $10
    !by $5F, $08
    !by $C0 ; RTS instruction

tune4_voice3:
    !by $D2, $1C, <ins0B, >ins0B ; SETNI instr: set all the instrument table
    !by $E0 ; LF3: low filter (max resonance) on voice 3
    !by $E2, <fil03, >fil03 ; FILTA instr: set filter table
    !by $5F, $03
    !by $1B, $01
    !by $1B, $01
    !by $1B, $01
    !by $1B, $03
    !by $1D, $03
    !by $21, $03
    !by $CC, $02 ; FOR inst: repeat 2h times
    !by $84, $10
    !by $7F, $11
    !by $CE ; NEXT instr
    !by $24, $06
    !by $C0 ; RTS instruction

tune4_voice2
    !by $BF, $14

tune4_voice1:
    !by $D2, $1C, <ins0A, >ins0A ; SETNI instr: set all the instrument table
    !by $37, $02
    !by $37, $01
    !by $3A, $06
    !by $99, $10
    !by $97, $11
    !by $95, $10
    !by $99, $11
    !by $37, $06
    !by $C0 ; RTS instruction

sub08:
    !by $42, $01
    !by $3F, $01
    !by $3C, $01
    !by $3A, $01
    !by $36, $01
    !by $33, $01
    !by $30, $01
    !by $2E, $01
    !by $2A, $01
    !by $2E, $01
    !by $30, $01
    !by $33, $01
    !by $36, $01
    !by $3A, $01
    !by $3C, $01
    !by $C0 ; RTS instruction

tune5_voice1:
    !by $D2, $1C, <ins0B, >ins0B ; SETNI instr: set all the instrument table
    !by $37, $02
    !by $36, $02
    !by $33, $02
    !by $35, $04
    !by $36, $02
    !by $37, $04
    !by $CC, $03 ; FOR inst: repeat 3h times
    !by $36, $02
    !by $36, $04
    !by $CE ; NEXT instr
    !by $5F, $01
    !by $C2, <sub08, >sub08 ; JSR instruction: execute subroutine
    !by $3F, $01
    !by $42, $01
    !by $5F, $02
    !by $36, $02
    !by $C0 ; RTS instruction

tune5_voice2:
    !by $D2, $1C, <ins0B, >ins0B ; SETNI instr: set all the instrument table
    !by $30, $02
    !by $30, $02
    !by $CC, $05 ; FOR inst: repeat 5h times
    !by $30, $02
    !by $30, $04
    !by $CE ; NEXT instr
    !by $5F, $01
    !by $42, $01
    !by $C2, <sub08, >sub08 ; JSR instruction: execute subroutine
    !by $3F, $01
    !by $5F, $02
    !by $30, $02
    !by $C0 ; RTS instruction

tune5_voice3:
    !by $D2, $1C, <ins0B, >ins0B ; SETNI instr: set all the instrument table
    !by $E0 ; LF3: low filter (max resonance) on voice 3
    !by $E2, <fil03, >fil03 ; FILTA instr: set filter table
    !by $CC, $04 ; FOR inst: repeat 4h times
    !by $18, $02
    !by $24, $02
    !by $CE ; NEXT instr
    !by $CC, $04 ; FOR inst: repeat 4h times
    !by $1B, $02
    !by $27, $02
    !by $CE ; NEXT instr
    !by $5F, $03
    !by $3F, $01
    !by $42, $01
    !by $C2, <sub08, >sub08 ; JSR instruction: execute subroutine
    !by $5F, $02
    !by $1E, $02
    !by $C0 ; RTS instruction

par05:
    !by $41 ; control: rectangular
    !by $06, $59 ; AD/SR
    !by $19
    !by $14

tune8_voice1:
    !by $D2, $17, <ins0B, >ins0B ; SETNI instr: set all the instrument table
    !by $D4, <par05, >par05 ; INSTR instruction: select instrument parameter
    !by $37, $01
    !by $37, $01
    !by $37, $04
    !by $3C, $04
    !by $3B, $04
    !by $3E, $04
    !by $3C, $04
    !by $37, $0A
    !by $37, $01
    !by $37, $01
    !by $37, $04
    !by $3C, $04
    !by $3B, $04
    !by $3E, $04
    !by $A0, $13
    !by $9E, $09
    !by $40, $01
    !by $C0 ; RTS instruction

tune8_voice2:
    !by $D2, $17, <ins0B, >ins0B ; SETNI instr: set all the instrument table
    !by $D4, <par05, >par05 ; INSTR instruction: select instrument parameter
    !by $43, $01
    !by $43, $01
    !by $43, $04
    !by $48, $04
    !by $47, $04
    !by $4A, $04
    !by $48, $04
    !by $40, $01
    !by $40, $01
    !by $40, $01
    !by $50, $01
    !by $41, $01
    !by $41, $01
    !by $41, $01
    !by $50, $01
    !by $40, $01
    !by $40, $01
    !by $40, $01
    !by $43, $01
    !by $43, $04
    !by $48, $04
    !by $47, $04
    !by $4A, $04
    !by $9C, $13
    !by $9B, $09
    !by $3C, $01
    !by $C0 ; RTS instruction

tune8_voice3:
    !by $D2, $1C, <ins0B, >ins0B ; SETNI instr: set all the instrument table
    !by $E0 ; LF3: low filter (max resonance) on voice 3
    !by $E2, <fil03, >fil03 ; FILTA instr: set filter table
    !by $5F, $02
    !by $CC, $06 ; FOR inst: repeat 6h times
    !by $24, $04
    !by $1F, $04
    !by $CE ; NEXT instr
    !by $CA, $18, $41 ; SET instr: 18h set control to rectangular
    !by $CA, $1B, $28 ; ??
    !by $84, $13
    !by $7F, $09
    !by $24, $01
    !by $C0 ; RTS instruction

    * = $3F00
InstrVoice1:
    !by <inst_C0_v1, >inst_C0_v1 ; C0: RTS
    !by <inst_C2_v1, >inst_C2_v1 ; C2: JSR
    !by <inst_C4_v1, >inst_C4_v1 ; C4: JMP
    !by <inst_C6_v1, >inst_C6_v1 ; C6: JSRT
    !by $5A, $0A
    !by <inst_CA_v1, >inst_CA_v1 ; CA: SET
    !by <inst_CC_v1, >inst_CC_v1 ; CC: FOR
    !by <inst_CE_v1, >inst_CE_v1 ; CE: NEXT
    !by $5A, $0A
    !by <inst_D2_v1, >inst_D2_v1 ; D2: SETNI
    !by <inst_D4_v1, >inst_D4_v1 ; D4: INSTR
    !by <inst_D6_v1, >inst_D6_v1 ; D6: SETCI
    !by $5A, $0A
    !by $5A, $0A
    !by $5A, $0A
    !by <inst_DE_v1, >inst_DE_v1 ; DE: SET2CI
    !by $5A, $0A
    !by $5A, $0A
    !by $5A, $0A
    !by $5A, $0A
    !by $5A, $0A
    !by $5A, $0A
    !by $5A, $0A
    !by $5A, $0A
    !by <inst_F0_v1, >inst_F0_v1 ; F0: SETFI

InstrVoice2:
    !by <inst_C0_v2, >inst_C0_v2 ; C0: RTS
    !by <inst_C2_v2, >inst_C2_v2 ; C2: JSR
    !by <inst_C4_v2, >inst_C4_v2 ; C4: JMP
    !by $5D, $0A
    !by $5D, $0A
    !by <inst_CA_v2, >inst_CA_v2 ; CA: SET
    !by <inst_CC_v2, >inst_CC_v2 ; CC: FOR
    !by <inst_CE_v2, >inst_CE_v2 ; CE: NEXT
    !by $5D, $0A
    !by <inst_D2_v2, >inst_D2_v2 ; D2: SETNI
    !by <inst_D4_v2, >inst_D4_v2 ; D4: INSTR
    !by <inst_D6_v2, >inst_D6_v2 ; D6: SETCI
    !by $5D, $0A
    !by $5D, $0A
    !by <inst_DC_v2, >inst_DC_v2 ; DC: SET2I
    !by <inst_DE_v2, >inst_DE_v2 ; DE: SET2CI
    !by $5D, $0A
    !by $5D, $0A
    !by $5D, $0A
    !by $5D, $0A
    !by $5D, $0A
    !by $5D, $0A
    !by $5D, $0A
    !by $5D, $0A
    !by <inst_F0_v2, >inst_F0_v2 ; F0: SETFI

InstrVoice3:
    !by <inst_C0_v3, >inst_C0_v3 ; C0: RTS
    !by <inst_C2_v3, >inst_C2_v3 ; C2: JSR
    !by <inst_C4_v3, >inst_C4_v3 ; C4: JMP
    !by <inst_C6_v3, >inst_C6_v3 ; C6: JSRT
    !by $60, $0A
    !by <inst_CA_v3, >inst_CA_v3 ; CA: SET
    !by <inst_CC_v3, >inst_CC_v3 ; CC: FOR
    !by <inst_CE_v3, >inst_CE_v3 ; CE: NEXT
    !by $60, $0A
    !by <inst_D2_v3, >inst_D2_v3 ; D2: SETNI
    !by <inst_D4_v3, >inst_D4_v3 ; D4: INSTR
    !by $60, $0A
    !by <inst_D8_v3, >inst_D8_v3 ; D8: EXCT
    !by $60, $0A
    !by <inst_DC_v3, >inst_DC_v3 ; DC: SET2I
    !by <inst_DE_v3, >inst_DE_v3 ; DE: SET2CI
    !by <inst_E0_v3, >inst_E0_v3 ; E0: LF3
    !by <inst_E2_v3, >inst_E2_v3 ; E2: FILTA
    !by $60, $0A
    !by $60, $0A
    !by $60, $0A
    !by $60, $0A
    !by $60, $0A
    !by $60, $0A
    !by <inst_F0_v3, >inst_F0_v3 ; F0: SETFI

;=================================
; Calculate the right address
;=================================
calcAddress:
    lda $1FFF
    sec
    sbc #$08
    tax
    lda #$00
    sta $FB ; low address
    sta $FC ; high address
    cpx #$00
    beq skipCalc
iterate:
    lda $FB
    clc
    adc #$5D
    sta $FB
    lda $FC
    adc #$00
    sta $FC
    dex
    bne iterate
skipCalc:
    lda $FC
    ora #$20 ; this is according with     * = $2000
    sta $FC
    ldx #$00
    lda $FB
    ldy $FC
    jsr setSoundEffect
    jsr fixForNext
    ldx #$01
    lda $FB
    ldy $FC
    jsr setSoundEffect
    jsr fixForNext
    ldx #$02
    lda $FB
    ldy $FC
    jsr setSoundEffect
    jmp setInterrupt
fixForNext:
    lda $FB
    clc
    adc #$1F
    sta $FB
    lda $FC
    adc #$00
    sta $FC
    rts
;985E AE FF 1F LDX $1FFF
;9861 BD 90 40 LDA $4090,X
;9864 D0 03 BNE $9869
;9866 4C 67 1F JMP setInterrupt
;9869 A9 F0 LDA #$F0
;986B 8D 04 DC STA $DC04 Timer A #1: Lo Byte
;986E A9 49 LDA #$49
;9870 8D 05 DC STA $DC05 Timer A #1: Hi Byte
;9873 4C 67 1F JMP setInterrupt
rout13:
    !by $84, $0C ; Play Sample 4
    !by $86, $06 ; Play Sample 6
    !by $86, $06 ; Play Sample 6
    !by $82, $0C ; Play Sample 2
    !by $86, $06 ; Play Sample 6
    !by $86, $06 ; Play Sample 6
    !by $84, $06 ; Play Sample 4
    !by $84, $06 ; Play Sample 4
    !by $86, $06 ; Play Sample 6
    !by $86, $06 ; Play Sample 6
    !by $82, $0C ; Play Sample 2
    !by $86, $06 ; Play Sample 6
    !by $86, $06 ; Play Sample 6
    !by $84, $0C ; Play Sample 4
    !by $86, $06 ; Play Sample 6
    !by $86, $06 ; Play Sample 6
    !by $82, $0C ; Play Sample 2
    !by $86, $06 ; Play Sample 6
    !by $86, $06 ; Play Sample 6
    !by $84, $06 ; Play Sample 4
    !by $84, $06 ; Play Sample 4
    !by $86, $06 ; Play Sample 6
    !by $86, $06 ; Play Sample 6
    !by $60 ; RTS

;=================================
; Set sample
; A=sample duration
; X=low address
; Y=high address
;=================================
setSample:
    sta $DC ; store sample duration
    stx $DA ; store low address of sample pattern
    sty $DB ; store high address of sample pattern
    lda #$07
    sta $DD ; store stack index for sample
    rts

Sample_Tune1:
    !by $20, <rout01, >rout01 ; JSR instr
    !by $20, <rout02, >rout02 ; JSR instr
    !by $20, <rout02, >rout02 ; JSR instr
    !by $20, <rout01, >rout01 ; JSR instr
    !by $49, $02 ; FOR instr: repeat 02 times (level 1)
    !by $20, <rout02, >rout02 ; JSR instr
    !by $20, <rout03, >rout03 ; JSR instr
    !by $49, $03 ; FOR instr: repeat 03 times (level 2)
    !by $82, $01 ; Play Sample 2
    !by $82, $01 ; Play Sample 2
    !by $82, $06 ; Play Sample 2
    !by $40 ; NEXT instr (level 2)
    !by $40 ; NEXT instr (level 1)
    !by $20, <rout06, >rout06 ; JSR instr
    !by $87, $78 ; Play Nothing
    !by $49, $07 ; FOR instr: repeat 07 times
    !by $87, $80 ; Play Nothing
    !by $40 ; NEXT instr
    !by $20, <rout04, >rout04 ; JSR instr
    !by $20, <rout05, >rout05 ; JSR instr
    !by $20, <rout04, >rout04 ; JSR instr
    !by $20, <rout07, >rout07 ; JSR instr
    !by $20, <rout04, >rout04 ; JSR instr
    !by $20, <rout05, >rout05 ; JSR instr
    !by $20, <rout06, >rout06 ; JSR instr
    !by $20, <rout08, >rout08 ; JSR instr
    !by $20, <rout06, >rout06 ; JSR instr
    !by $49, $04 ; FOR instr: repeat 04 times
    !by $20, <rout06, >rout06 ; JSR instr
    !by $20, <rout08, >rout08 ; JSR instr
    !by $40 ; NEXT instr
    !by $20, <rout06, >rout06 ; JSR instr
    !by $20, <rout06, >rout06 ; JSR instr
    !by $87, $18 ; Play Nothing
    !by $20, <rout02, >rout02 ; JSR instr
    !by $20, <rout02, >rout02 ; JSR instr
    !by $49, $03 ; FOR instr: repeat 03 times
    !by $20, <rout09, >rout09 ; JSR instr
    !by $40 ; NEXT instr
    !by $20, <rout06, >rout06 ; JSR instr
    !by $20, <rout08, >rout08 ; JSR instr
    !by $84, $02 ; Play Sample 4
    !by $84, $06 ; Play Sample 4
    !by $20, <rout08, >rout08 ; JSR instr
    !by $20, <rout10, >rout10 ; JSR instr
    !by $20, <rout06, >rout06 ; JSR instr
    !by $84, $02 ; Play Sample 4
    !by $84, $06 ; Play Sample 4
    !by $20, <rout06, >rout06 ; JSR instr
    !by $49, $03 ; FOR instr: repeat 03 times
    !by $20, <rout09, >rout09 ; JSR instr
    !by $40 ; NEXT instr
    !by $20, <rout06, >rout06 ; JSR instr
    !by $20, <rout08, >rout08 ; JSR instr
    !by $84, $02 ; Play Sample 4
    !by $84, $06 ; Play Sample 4
    !by $85, $04 ; Play Sample 5
    !by $85, $04 ; Play Sample 5
    !by $20, <rout10, >rout10 ; JSR instr
    !by $85, $02 ; Play Sample 5
    !by $85, $06 ; Play Sample 5
    !by $20, <rout06, >rout06 ; JSR instr
    !by $49, $04 ; FOR instr: repeat 04 times
    !by $86, $02 ; Play Sample 6
    !by $40 ; NEXT instr
    !by $49, $03 ; FOR instr: repeat 03 times
    !by $20, <rout11, >rout11 ; JSR instr
    !by $40 ; NEXT instr
    !by $84, $02 ; Play Sample 4
    !by $84, $06 ; Play Sample 4
    !by $20, <rout08, >rout08 ; JSR instr
    !by $84, $02 ; Play Sample 4
    !by $84, $06 ; Play Sample 4
    !by $20, <rout08, >rout08 ; JSR instr
    !by $20, <rout10, >rout10 ; JSR instr
    !by $49, $03 ; FOR instr: repeat 03 times
    !by $84, $02 ; Play Sample 4
    !by $84, $06 ; Play Sample 4
    !by $40 ; NEXT instr
    !by $49, $03 ; FOR instr: repeat 03 times
    !by $20, <rout11, >rout11 ; JSR instr
    !by $40 ; NEXT instr
    !by $84, $02 ; Play Sample 4
    !by $84, $06 ; Play Sample 4
    !by $20, <rout08, >rout08 ; JSR instr
    !by $84, $02 ; Play Sample 4
    !by $84, $06 ; Play Sample 4
    !by $85, $04 ; Play Sample 5
    !by $85, $04 ; Play Sample 5
    !by $20, <rout10, >rout10 ; JSR instr
    !by $85, $02 ; Play Sample 5
    !by $85, $06 ; Play Sample 5
    !by $84, $02 ; Play Sample 4
    !by $84, $06 ; Play Sample 4
    !by $49, $04 ; FOR instr: repeat 04 times
    !by $86, $02 ; Play Sample 6
    !by $40 ; NEXT instr
    !by $49, $02 ; FOR instr: repeat 02 times
    !by $84, $02 ; Play Sample 4
    !by $84, $0E ; Play Sample 4
    !by $20, <rout08, >rout08 ; JSR instr
    !by $20, <rout08, >rout08 ; JSR instr
    !by $20, <rout10, >rout10 ; JSR instr
    !by $87, $08 ; Play Nothing
    !by $20, <rout08, >rout08 ; JSR instr
    !by $20, <rout08, >rout08 ; JSR instr
    !by $40 ; NEXT instr
    !by $84, $02
    !by $84, $0E
    !by $20, <rout08, >rout08 ; JSR instr
    !by $20, <rout08, >rout08 ; JSR instr
    !by $20, <rout10, >rout10 ; JSR instr
    !by $20, <rout06, >rout06 ; JSR instr
    !by $20, <rout08, >rout08 ; JSR instr
    !by $20, <rout08, >rout08 ; JSR instr
    !by $20, <rout06, >rout06 ; JSR instr
noSample:
    !by $87, $64 ; Play Nothing
    !by $4C, <noSample, >noSample

;=================================
; Set the retun address for
; sample routine
;=================================
set_return_sample:
    ldx $DD ; read stack index for sample
    clc
    adc $DA ; add to low addr sample pattern index
    sta $78,x ; store low addr sample pattern
    lda $DB
    adc #$00
    sta $80,x ; store high addr sample pattern
    dex
    stx $DD ; store actual stack index for sample
    rts

;=================================
; RTS istruction for sample
; return from subroutine
;=================================
inst_60_sample:
    inc $DD ; inc stack index for sample
    ldx $DD
restoreSampleAddr:
    lda $78,x ; read low addr from stack
    sta $DA ; store low addr sample pattern index
    lda $80,x ; read high addr from stack
    sta $DB ; store high addr sample pattern index
    rts

;high address to play sample routine
highPSample:
    !by >PSample1, >PSample2, >PSample3
    !by >PSample4, >PSample5, >PSample6
    !by >PNothing

;=================================
; JSR sample instruction
; #1 low address
; #2 high address
;=================================
Inst_20_sample:
    lda #$03
    jsr set_return_sample ; store return pointer in stack

;=================================
; JMP sample instruction
;=================================
inst_4C_sample:
    iny
    lda ($DA),y ; read low address
    tax
    iny
    lda ($DA),y ; read high address
    stx $DA ; set new low address
    sta $DB ; set new high address
    rts

rout01:
    !by $49, $08 ; FOR instr: repeat 08 times
    !by $83, $01 ; Play Sample 3
    !by $83, $03 ; Play Sample 3
    !by $40 ; NEXT instr
    !by $49, $08 ; FOR instr: repeat 08 times
    !by $85, $01 ; Play Sample 5
    !by $85, $03 ; Play Sample 5
    !by $40 ; NEXT instr
    !by $20, <rout12, >rout12 ; JSR instr
    !by $20, <rout12, >rout12 ; JSR instr
    !by $20, <rout08, >rout08 ; JSR instr
    !by $20, <rout08, >rout08 ; JSR instr

rout12:
    !by $86, $02 ; Play Sample 6
    !by $86, $0E ; Play Sample 6
    !by $60 ; RTS

rout04:
    !by $20, <rout06, >rout06 ; JSR instr
    !by $20, <rout08, >rout08 ; JSR instr
    !by $20, <rout06, >rout06 ; JSR instr
    !by $20, <rout08, >rout08 ; JSR instr
    !by $20, <rout10, >rout10 ; JSR instr
    !by $20, <rout08, >rout08 ; JSR instr
    !by $20, <rout08, >rout08 ; JSR instr
    !by $20, <rout06, >rout06 ; JSR instr
    !by $20, <rout08, >rout08 ; JSR instr
    !by $20, <rout06, >rout06 ; JSR instr
    !by $20, <rout06, >rout06 ; JSR instr
    !by $20, <rout08, >rout08 ; JSR instr

rout10:
    !by $82, $01 ; Play Sample 2
    !by $82, $01 ; Play Sample 2
    !by $82, $06 ; Play Sample 2
    !by $60 ; RTS

rout05:
    !by $20, <rout08, >rout08 ; JSR instr
    !by $20, <rout06, >rout06 ; JSR instr

rout08:
    !by $86, $02 ; Play Sample 6
    !by $86, $06 ; Play Sample 6
    !by $60 ; RTS

rout07:
    !by $20, <rout06, >rout06 ; JSR instr
    !by $20, <rout08, >rout08 ; JSR instr

rout06:
    !by $81, $02 ; Play Sample 1
    !by $81, $06 ; Play Sample 1
    !by $60 ; RTS

rout02:
    !by $20, <rout03, >rout03 ; JSR instr
    !by $4C, <rout02_, >rout02_ ; JMP instr

rout02_:
    !by $20, <rout06, >rout06 ; JSR instr
    !by $84, $02 ; Play Sample 4
    !by $84, $06 ; Play Sample 4
    !by $84, $02 ; Play Sample 4
    !by $84, $06 ; Play Sample 4
    !by $60 ; RTS

;=================================
; FOR instr for sample
; #1 number of cycle
;=================================
inst_49_sample:
    lda #$02
    jsr set_return_sample ; set return address for cycle
    pha
    iny
    lda ($DA),y ; read number of cycle
    sta $89,x ; store the number of cycle for the for
    pla
    sta $DB ; set low addr sample pattern index
    lda $79,x
    sta $DA ; set high addr sample pattern index
    rts

;=================================
; NEXT instr. for sample
;=================================
inst_40_sample:
    inc $DD ; inc stack index for sample
    ldx $DD
    dec $88,x ; dec number of cycle
    beq cycleEndSample ; end of cycles?
    jsr restoreSampleAddr
    dec $DD
    rts

cycleEndSample:
    stx $DD
    inc $DA ; go to next instruction (low addr)
    bne skipFixHigh
    inc $DB ; fix high address
skipFixHigh:
    rts

rout03:
    !by $20, <rout04, >rout04 ; JSR instr
    !by $20, <rout05, >rout05 ; JSR instr
    !by $20, <rout04, >rout04 ; JSR instr
    !by $20, <rout07, >rout07 ; JSR instr
    !by $20, <rout04, >rout04 ; JSR instr
    !by $20, <rout05, >rout05 ; JSR instr
    !by $4C, <rout04, >rout04 ; JMP instr

rout09:
    !by $20, <rout06, >rout06 ; JSR instr
    !by $20, <rout08, >rout08 ; JSR instr
    !by $83, $02 ; Play Sample 3
    !by $83, $06 ; Play Sample 3
    !by $20, <rout08, >rout08 ; JSR instr
    !by $20, <rout10, >rout10 ; JSR instr
    !by $20, <rout08, >rout08 ; JSR instr
    !by $83, $02
    !by $83, $06
    !by $4C, <rout08, >rout08 ; JMP instr

rout11:
    !by $84, $02 ; play Sample 4
    !by $84, $06 ; play Sample 4
    !by $20, <rout08, >rout08 ; JSR instr
    !by $83, $02 ; Play Sample 3
    !by $83, $06 ; Play Sample 3
    !by $20, <rout08, >rout08 ; JSR instr
    !by $20, <rout10, >rout10 ; JSR instr
    !by $20, <rout08, >rout08 ; JSR instr
    !by $83, $02 ; Play Sample 3
    !by $83, $06 ; Play Sample 3
    !by $4C, <rout08, >rout08 ; JMP instr

Sample_Tune2:
    !by $87, $01 ; Play Nothing

STune2_:
    !by $49, $07 ; FOR instr: repeat 07 times
    !by $85, $18 ; Play Sample 5
    !by $40 ; NEXT instr
    !by $85, $0C ; Play Sample 5
    !by $85, $06 ; Play Sample 5
    !by $85, $06 ; Play Sample 5
    !by $49, $06 ; FOR instr: repeat 06 times
    !by $85, $02 ; Play Sample 5
    !by $85, $16 ; Play Sample 5
    !by $40 ; NEXT instr
    !by $82, $02 ; Play Sample 2
    !by $82, $16 ; Play Sample 2
    !by $49, $02 ; FOR instr: repeat 02 times
    !by $83, $02 ; Play Sample 3
    !by $83, $04 ; Play Sample 3
    !by $40 ; NEXT instr
    !by $49, $02 ; FOR instr: repeat 02 times
    !by $84, $02 ; Play Sample 4
    !by $84, $04 ; Play Sample 4
    !by $40 ; NEXT instr
    !by $49, $04 ; FOR instr: repeat 04 times (level 1)
    !by $49, $03 ; FOR instr: repeat 03 times (level 2)
    !by $20, <rout13, >rout13 ; JSR instr
    !by $82, $06 ; Play Sample 2
    !by $83, $06 ; Play Sample 3
    !by $86, $06 ; Play Sample 6
    !by $86, $06 ; Play Sample 6
    !by $40 ; NEXT instr (level 2)
    !by $20, <rout13, >rout13 ; JSR instr
    !by $82, $06 ; Play Sample 2
    !by $83, $06 ; Play Sample 3
    !by $82, $06 ; Play Sample 2
    !by $82, $06 ; Play Sample 2
    !by $40 ; NEXT instr (level 1)
    !by $4C, <STune2_, >STune2_ ; JMP istr

;=================================
; Play Sample 6 routine
;=================================
PSample6:
    ldy #$05
    lda #$94
    clc
    adc #$40 ; $94+$40=$D4
    sta Vol6+2 ; unmask D418 address
nextDelayP6:
    ldx #$14
againP6:
    lda delayTabP6,y ; read duration
delayP6: ; waste some times
    sec
    sbc #$01
    bne delayP6
    lda $DE ; increase up volume sequence
    clc
    adc #$98
    sta $DE
    and #$0F
Vol6:
    sta $9418 ; D418: play sample
    dex
    bne againP6
    dey
    bpl nextDelayP6
    lda #$94
    sta Vol6+2 ; mask again D418 address
    rts

;=================================
; Play Sample 3 routine
;=================================
PSample3:
    ldy #$05
    lda #$94
    clc
    adc #$40
    sta Vol3+2 ; unmask D418 address
nextDelayP3:
    ldx #$19 ; set repeating value
againP3:
    lda delayTabP3,y
delayP3: ; waste some times
    sec
    sbc #$01
    bne delayP3
    lda $DE ; increase up volume sequence
    clc
    adc #$65
    sta $DE
    and #$0F
Vol3:
    sta $9418 ; D418: play sample
    dex
    bne againP3
    dey ; select next delay in table
    bpl nextDelayP3
    lda #$94
    sta Vol3+2 ; mask again D418 address
    rts

;low address to play sample routine
lowPSample:
    !by <PSample1, <PSample2, <PSample3
    !by <PSample4, <PSample5, <PSample6
    !by <PNothing

;=================================
; Play Sample 5 routine
;=================================
PSample5:
    ldy #$05
    lda #$94
    clc
    adc #$40
    sta Vol5+2 ; unmask D418 address
nextDelayP5:
    ldx #$19 ; set repeating value
againP5:
    lda delayTabP5,y
delayP5: ; waste some times
    sec
    sbc #$01
    bne delayP5
    lda $DE ; increase up volume sequence
    clc
    adc #$DD
    sta $DE
    and #$0F
Vol5:
    sta $9418 ; D418: play sample
    dex
    bne againP5
    dey
    bpl nextDelayP5
    lda #$94
    sta Vol5+2 ; mask again D418 address
    rts

; high pointers of operations
highOper:
    !by >Inst_20_sample
    !by >inst_40_sample
    !by >inst_60_sample
    !by >inst_49_sample
    !by >inst_4C_sample

;=================================
; Play Sample 4 routine
;=================================
PSample4:
    ldy #$05
    lda #$94
    clc
    adc #$40
    sta Vol4+2 ; unmask D418 address
nextDelayP4:
    ldx #$19 ; set repeating value
againP4:
    lda delayTabP4,y
delayP4: ; waste some times
    sec
    sbc #$01
    bne delayP4
    lda $DE ; increase up volume sequence
    clc
    adc #$DD
    sta $DE
    and #$0F
Vol4:
    sta $9418 ; D418: play sample
    dex
    bne againP4
    dey
    bpl nextDelayP4
    lda #$94
    sta Vol4+2 ; mask again D418 address
    rts

;=================================
; Init sample routine
;=================================
initSample:
    ldx #$00
    stx $DF ; reset sample generation index routine
    dex
    stx $DC ; sample duration
    ldx #<noSample
    ldy #>noSample
    stx $DA ; store low address of sample pattern
    sty $DB ; store high address of sample pattern
    rts

;=================================
; Play Sample 2 routine
;=================================
PSample2:
    ldy #$0D
    lda #$94
    clc
    adc #$40
    sta Vol2+2 ; unmask D418 address
nextDelayP2:
    ldx #$0C ; set repeating value
againP2:
    lda delayTabP2,y
delayP2: ; waste some times
    sec
    sbc #$01
    bne delayP2
    lda $DE ; increase up volume sequence
    clc
    adc #$0E
    sta $DE
    and #$0F
Vol2:
    sta $9418 ; D418: play sample
    dex
    bne againP2
    dey ; select next delay in table
    bpl nextDelayP2
    lda #$94
    sta Vol2+2 ; mask again D418 address
    rts

;=================================
; Play Sample 1 routine
;=================================
PSample1:
    ldy #$05
    lda #$94
    clc
    adc #$40
    sta Vol1+2 ; unmask D418 address
nextDelayP1:
    ldx #$0C ; set repeating value
againP1:
    lda delayTabP1,y
delayP1: ; waste some times
    sec
    sbc #$01
    bne delayP1
    lda $DE ; increase up volume sequence
    clc
    adc #$01
    sta $DE
    and #$0F
Vol1:
    sta $DD18 ; play sample
    dex
    bne againP1
    dey ; select next delay in table
    bpl nextDelayP1
    lda #$DD
    sta Vol1+2 ; mask again D418 address
PNothing:
    rts

; table of operation
operTable:
    !by $20, $40, $60, $49, $4C
delayTabP3:
    !by $20, $10, $08, $04, $02, $01
; low pointer of operations

lowOper:
    !by <Inst_20_sample
    !by <inst_40_sample
    !by <inst_60_sample
    !by <inst_49_sample
    !by <inst_4C_sample

delayTabP5:
    !by $23, $14, $0C, $09, $06, $03
delayTabP4:
    !by $05, $1E, $19, $14, $0F, $0A
delayTabP6:
    !by $28, $0A, $14, $0A, $0F, $0A
delayTabP2:
    !by $20, $10, $08, $04, $02, $01, $28
    !by $0A, $3C, $0A, $14, $0A, $20, $40
delayTabP1: ; table of delay for Play sample 1
    !by $40, $1E, $3E, $19, $3C, $14

;=================================
; Generate sample
; If one routine of sample is
; available, it will be called
;=================================
SampleGeneration:
    lda $DF ; load sample generation routine index
    beq exitSample ; if 0 exit
    lda #$00
    sta $DF
callPSample:
    jsr PSample3 ; execute play sample routine
    lda #$0F ; turn the volume to max
    ora TEMP ; turn filter to the store value
    sta $D418 ; Select volume and filter mode
    rts

;=================================
; Play sample engine routine
;=================================
playSample:
    dec $DC ; dec sample duration
    bne exitSample
nextPattern:
    ldy #$00
    lda ($DA),y ; read sample pattern
    bmi isPlay ; jump if >=80
    ldx #$FF
nextOper:
    inx
    cmp operTable,x ; compare value to table for getting index of instr.
    bne nextOper
    lda lowOper,x ; read low address of routine
    sta decoded+1
    lda highOper,x ; read high address of routine
    sta decoded+2
decoded:
    jsr inst_60_sample ; execute decoded instruction
    jmp nextPattern
isPlay:
    sta $DF ; store readed sample generator routine
    tax
    iny
    lda ($DA),y ; read sample duration
    sta $DC ; store sample duration
    lda $DA
    clc
    adc #$02 ; adjust pattern pointer low
    sta $DA
    lda $DB
    adc #$00 ; adjust pattern pointer high
    sta $DB
    lda lowPSample-$81,x ; low address of play sample routines
    sta callPSample+1
    lda highPSample-$81,x ; high address of play sample routine
    sta callPSample+2
exitSample:
    rts

}