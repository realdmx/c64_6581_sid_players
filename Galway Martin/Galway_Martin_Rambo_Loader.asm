
;	Rambo II Loader
;	By Martin Galway

;	Reverse engineered by dmx87

	!to "Galway_Martin_Rambo_Loader.sid",plain

	* = $0000

	!text "PSID"
	!be16 2							; version 2
	!be16 $7c						; data offset
	!be16 $6f00						; Load (0 = auto)
	!be16 Init						; Init
	!be16 Play						; Play
	!be16 1						; num songs
	!be16 1							; first song
	!word 0
	!word 0
-	!text "Rambo II Loader"
	!fill 32 - (* - -)
-	!text "Martin Galway"
	!fill 32 - (* - -)
-	!text "1986 Ocean"
	!fill 32 - (* - -)
	!word 0							; v2 flags
	!word 0							; Start page, page length (reloc)
	!word 0							; Reserved
	
!pseudopc $6f00 {
	
	* = $6f00
	
SIDV1FREQLO = $d400
SIDV1FREQHI = $d401
SIDV1PWLO = $d402
SIDV1PWHI = $d403
SIDV1CTRL = $d404
SIDV1AD = $d405
SIDV1SR = $d406

SIDV2FREQLO = $d407
SIDV2FREQHI = $d408
SIDV2PWLO = $d409
SIDV2PWHI = $d40a
SIDV2CTRL = $d40b
SIDV2AD = $d40c
SIDV2SR = $d40d

SIDV3FREQLO = $d40e
SIDV3FREQHI = $d40f
SIDV3PWLO = $d410
SIDV3PWHI = $d411
SIDV3CTRL = $d412
SIDV3AD = $d413
SIDV3SR = $d414


;    * = $2000

V1PAT	= $10		; track pointers
V1PATH	= $11
V2PAT	= $12
V2PATH	= $13
V3PAT	= $14
V3PATH	= $15
PARAM1	= $16		; opcode parameter 1
PARAM2	= $17		; opcode parameter 2
CVAL	= $18		; current dur/length/note
VEN		= $19		; voice enabled bit set 0-2
V1CNT	= $1a		; note counters
V2CNT	= $1b
V3CNT	= $1c
V1SP	= $1d		; stack pointers
V2SP	= $1e
V3SP	= $1f

Play
playl = * + 1
playh = * + 2
    jmp playmusic

Init
    jmp initmusic

    jmp L_25FA

v1_pmretrig
    ldx v1_pmlo
    ldy v1_pmhi
    stx v1_pwl
    sty v1_pwh
v1_pmcont
    lda v1_p1time
    sta v1_p1c
    lda v1_p2time
    sta v1_p2c
    rts

v2_pmretrig
    ldx v2_pmlo
    ldy v2_pmhi
    stx v2_pwl
    sty v2_pwh
v2_pmcont
    lda v2_p1time
    sta v2_p1c
    lda v2_p2time
    sta v2_p2c
    rts

v3_pmretrig
    ldx v3_pmlo
    ldy v3_pmhi
    stx v3_pwl
    sty v3_pwh
v3_pmcont
    lda v3_p1time
    sta v3_p1c
    lda v3_p2time
    sta v3_p2c
    rts

v1_freqreset
    lda v1_fq4time
    sta v1_f4c
    lda v1_fq3time
    sta v1_f3c
    lda v1_fq2time
    sta v1_f2c
    lda v1_fq1time
    sta v1_f1c
    rts

v2_freqreset
    lda v2_fq4time
    sta v2_f4c
    lda v2_fq3time
    sta v2_f3c
    lda v2_fq2time
    sta v2_f2c
    lda v2_fq1time
    sta v2_f1c
    rts

v3_freqreset
    lda v3_fq4time
    sta v3_f4c
    lda v3_fq3time
    sta v3_f3c
    lda v3_fq2time
    sta v3_f2c
    lda v3_fq1time
    sta v3_f1c
    rts

; ============================================================================
;
;	VOICE 1
;
; ============================================================================

	!zn voice1

dovoice1
    lda VEN						; Is voice active
    lsr
    bcc +

    dec V1CNT
    beq .step

+   jmp .sound

.execreturn = * - 1				; EXEC returns here
.skip3
	lda #3						; skip 3 bytes

.next
    clc
    adc V1PAT
    sta V1PAT
    bcc .step

    inc V1PATH

.step
    ldy #0
    lda (V1PAT),y
    cmp #$c0
    bcc .other

	; >= $c0 opcode

    and #$3f
    tax
    lda v1ops,x
    sta .opl
    lda v1opsh,x
    sta .oph
    iny
    lda (V1PAT),y
    tax
    sta PARAM1        			; param 1
    iny
    lda (V1PAT),y
    sta PARAM2        			; param 2

.opl = * + 1
.oph = * + 2
    jmp $0000					; exec op

.noop
    jmp .duration

	; <$c0 = note or duration

.other							; 20de
    sta CVAL

    cmp #$60
    bcc +

    sbc #$60

+   cmp #$5f					; 5f/bf = noop
    beq .noop

    adc v1_ntrans				; transpose
    tax							; x = note

    ldy #4
-   lda #0
    sta $d402,y 	            ; clear pwlo/hi/ctrl/ad/sr
    lda v1_cipw,y
    sta $d402,y  	            ; load pwlo/hi/ctrl/ad/sr
    dey
    bpl -

    lda v1_cictrl
    sta v1_ctrl
    ldy freqsh,x
    lda freqsl,x
    sta v1_fql
    sty v1_fqh
    sta SIDV1FREQLO
    sty SIDV1FREQHI
    lda v1_cipmflags
    sta v1_pmflags
    beq +

    ldy #9
-   lda v1_cipm,y
    sta v1_p1time,y
    dey
    bpl -

    jsr v1_pmretrig

+   ldx v1_cifqflag
    stx v1_fqflag
    beq L_2153

	; copy first 14 bytes of instrument

    ldy #13
-   lda v1_instr,y
    sta v1_cinstr,y
    dey
    bpl -

    txa
    and #8
    beq +

    lda CVAL
    clc
    adc v1_ntrans
    sta v1_fq3time
    sty v1_fqdelay    		; y = $ff
+
    jsr v1_freqreset

L_2153
    ldx v1_cirlsc
    ldy v1_cihrtime
    stx v1_rlsc
    sty v1_hrtime

	; duration byte

.duration
    ldy #1
    lda (V1PAT),y			; get next byte
    ldx CVAL
    cpx #$60             	; if note >= $60 then next is dur
    bcs +

    tax                  	; otherwise an index
    lda v1_durtable,x

+   sta V1CNT
    lda #2
    clc
    adc V1PAT
    sta V1PAT
    bcc +
    inc V1PATH
+   jmp .sound

	; C0 RET - Return to caller or end track

op_c0_v1
    inc V1SP
    ldy V1SP
    cpy #8
    beq .off

.sprestore
    ldx v1_stackl,y
    lda v1_stackh,y
    jmp .setpos

.off
    lda VEN
    and #$fe
    sta VEN					; disable this track
    rts

	; DA REPEAT,NN - Set repeat the following NN times

op_da_v1
	ldx V1SP
	clc
	tya
	adc V1PAT
	sta v1_stackl,x
	lda #0
	adc V1PATH
	sta v1_stackh,x
	lda PARAM1
	sta v1_repstack,x
	dec V1SP
	tya
	jmp .next

	; DC NEXT - Repeat

op_dc_v1
	ldx V1SP
	dec v1_repstack + 1,X
	beq +
	inx
	txa
	tay
	bpl .sprestore
+	inc V1SP
	lda #1
	jmp .next

	; C2 [addr] INSTR5 - Set 5 bytes in instrument
	; Sets Ctrl, AD, SR, Release, HRTime

op_c2_v1
	ldy #4
	ldx #$1c

	; y = count + 1
	; x = instrument offset
.cpyxy
	lda (PARAM1),y
	sta v1_instr,x
	dex
	dey
	bpl .cpyxy
	jmp .skip3

	; C6 [addr] - SETFQ - Copy 14 bytes (freq bytes)

op_c6_v1
	ldy #13
	ldx #$d
	bne .cpyxy

	; C8 [addr] - SETPM - Copy 10 bytes to $17 (pulse bytes)

op_c8_v1
	ldy #9
	ldx #$17
	bne .cpyxy

	; unused

op21df
	ldy #$30
	ldx #$30
	bne .cpyxy

	; D8 [addr] - ARP - Copy 10 bytes to freq area

op_d8_v1
	ldy #9
	ldx #9
	bne .cpyxy

L_21EB
    lda PARAM2
.setpos
    stx V1PAT
    sta V1PATH
    jmp .step

	; D4 [Hi,Lo] EXEC - Execute 6502 at hi/lo

op_d4_v1
    lda #>.execreturn 		;$20
    pha
    lda #<.execreturn		;$aa
    pha
    jmp (PARAM1)

	; CA [HI, LO] - CALL - Call subtrack

op_ca_v1
    lda #3
.skipa
    ldy V1SP
    clc
    adc V1PAT
    sta v1_stackl,y
    lda #0
    adc V1PAT + 1
    sta v1_stackh,y
    dec V1SP
    jmp L_21EB

	; CE [HI, LO, NT] - CALLT - Call with Note Transpose

op_ce_v1
    iny
    lda (V1PAT),y
    sta v1_ntrans
    lda #4             		; skip 4 bytes
	bne .skipa

	; CC [NT] - TRANS - Note transpose

op_cc_v1
    stx v1_ntrans
    tya
    jmp .next

	; D0 [OFF,VAL] - ISET - Set instrument byte

op_d0_v1
	sta v1_instr,x
	jmp .skip3

	; D6 [OFF,VAL] - CISET - Set value in Current Instr.

op_d6_v1
	sta v1_cinstr,x
	jmp .skip3

.end
    rts

; ============================================================================
;
;	VOICE 1 SOUND UPDATE
;
; ============================================================================

.sound								; $2231
    ldx v1_hrtime
    beq .end

    lda v1_ctrl
    and #8						; test bit?
    beq +

    lda V1CNT
    cmp v1_rlsc
    bcs .norls

    lda #0
    sta v1_rlsc
    lda v1_ctrl
    and #$f6					; clear gate and test bit
    sta v1_ctrl
    bne .setctrl

+   lda v1_rlsc
    bne .chkrls

    dec v1_hrtime
    bne .norls

	; clear all regs
    ldx #6
-   sta $d400,x  				; a = 0
    dex
    bpl -
    rts

.chkrls
    dec v1_rlsc
    bne .norls

    lda v1_ctrl
    and #$f6       				; clear gate and test

.setctrl
    sta SIDV1CTRL

	; ========================================================================
	; PULSE
	; ========================================================================

.norls
    lda v1_pmflags
    beq .freq

    lda v1_pmdelay
    beq .dopulse

    dec v1_pmdelay
    jmp .freq

.dopulse
    clc
    ldx v1_pwl
    ldy v1_pwh

	; Pulse 1

    lda v1_p1c
    beq +

    txa
    adc v1_pm1addl
    tax
    tya
    adc v1_pm1addh
    tay
    dec v1_p1c
    jmp .loadpulse

	; Pulse 2

+	lda v1_p2c
    beq +

    txa
    adc v1_pm2addl
    tax
    tya
    adc v1_pm2addh
    tay
    dec v1_p2c
    jmp .loadpulse

+   lda v1_pmflags
    and #$81
    beq .loadpulse
    bpl +

    jsr v1_pmretrig		; retrig pulse

    jmp .dopulse

+   jsr v1_pmcont		; continue without retrig

    jmp .dopulse

.loadpulse
    stx v1_pwl
    sty v1_pwh
    stx SIDV1PWLO
    sty SIDV1PWHI

	; ========================================================================
	; FREQ
	; ========================================================================

.freq
    lda v1_fqflag
    beq +

    and #8					; bit 3 = arpeggio?
    bne .arp

    ldx v1_fql
    ldy v1_fqh
    clc
    lda v1_fqdelay
    beq L_2313

    dec v1_fqdelay
    lda v1_fqflag
    and #2					; bit 1
    bne L_2357

+	rts

	; Arpeggio

.arp
    ldx v1_fqdelay
    bpl +

    ldx v1_fq4time

+   lda v1_fq3time
    clc
    adc v1_fq1addl,x
    dex
    stx v1_fqdelay
    tay
    ldx freqsl,y
    lda freqsh,y
    jmp .loadfreq

	; FREQ
	; x = fql
	; y = fqh

L_2312
    clc
L_2313

	; Freq 1 Adder

    lda v1_f1c
    beq +

    dec v1_f1c
    txa
    adc v1_fq1addl
    tax
    tya
    adc v1_fq1addh
    jmp .loadfreq

	; Freq 2 Adder

+   lda v1_f2c
    beq +

    dec v1_f2c
    txa
    adc v1_fq2addl
    tax
    tya
    adc v1_fq2addh
    jmp .loadfreq

	; Freq 3 Adder

+   lda v1_f3c
    beq +

    dec v1_f3c
    txa
    adc v1_fq3addl
    tax
    tya
    adc v1_fq3addh
    jmp .loadfreq

	; Freq 4 Adder

+   lda v1_f4c
    beq +

    dec v1_f4c
L_2357
    txa
    adc v1_fq4addl
    tax
    tya
    adc v1_fq4addh
.loadfreq
    tay
    stx SIDV1FREQLO
    sty SIDV1FREQHI
    stx v1_fql
    sty v1_fqh
    rts

+   jsr v1_freqreset

    jmp L_2312

; ============================================================================
;
;	VOICE 2
;
; ============================================================================

	!zn voice2

dovoice2
    lda VEN
    and #2
    beq L_237E

    dec V2CNT
    beq .step

L_237E
    jmp L_24E0

L_2381
    lda #$03
L_2383
    clc
    adc V2PAT
    sta V2PAT
    bcc .step

    inc $13

.step
    ldy #0
    lda (V2PAT),y
    cmp #$c0
    bcc L_23B4

    and #$3f
    tax
    lda v2opsl,x
    sta .opl
    lda v2opsh,x
    sta .oph
    iny
    lda (V2PAT),y
    tax
    sta $16
    iny
    lda (V2PAT),y
    sta PARAM2
.opl = * + 1
.oph = * + 2
    jmp $0000

.noop
    jmp L_2424

L_23B4
    sta CVAL
    cmp #$60
    bcc +

    sbc #$60

+   cmp #$5f
    beq .noop

    adc v2_ntrans
    tax
    ldy #4
-   lda #0
    sta $d409,y
    lda v2_cipw,y
    sta $d409,y
    dey
    bpl -

    lda v2_cictrl
    sta v2_ctrl
    ldy freqsh,x
    lda freqsl,x
    sta v2_fql
    sty v2_fqh
    sta SIDV2FREQLO
    sty SIDV2FREQHI
    lda v2_cipmflags
    sta v2_pmflags
    beq L_2402

    ldy #$09
L_23F6
    lda v2_cipm,y
    sta v2_p1time,y
    dey
    bpl L_23F6

    jsr v2_pmretrig

L_2402
    ldx v2_cifqflag
    stx v2_fqflags
    beq L_2418

    ldy #13
L_240C
    lda v2_instr,y
    sta v2_fq1addl,y
    dey
    bpl L_240C

    jsr v2_freqreset

L_2418
    ldx v2_cirlsc
    ldy v2_cihrtime
    stx v2_rlsc
    sty v2_hrtime
L_2424
    ldy #$01
    lda (V2PAT),y
    ldx CVAL
    cpx #$60
    bcs L_2432

    tax
    lda v2_durtable,x

L_2432
    sta V2CNT
    lda #2
    clc
    adc V2PAT
    sta V2PAT
    bcc L_243F

    inc V2PATH
L_243F
    jmp L_24E0

op_c0_v2
    inc V2SP
    ldy V2SP
    cpy #8
    beq L_2453
L_244A
    ldx v2_stackl,y
    lda v2_stackh,y
    jmp L_24AC

L_2453
    lda VEN
    and #$fd
    sta VEN
    rts

op_da_v2
	ldx V2SP
	clc
	tya
	adc V2PAT
	sta v2_stackl,x
	lda V2PATH
	adc #0
	sta v2_stackh,x
	lda $16
	sta v2_repstack,x
	dec V2SP
	tya
	jmp L_2383

op_dc_v2
	ldx V2SP
	dec v2_repstack + 1,x
	beq L_2481
	inx
	txa
	tay
	bpl L_244A
L_2481
	inc V2SP
	lda #1
	jmp L_2383

op_c2_v2
	ldy #4
	ldx #$1c
L_248c
	lda ($16),y
	sta v2_instr,x
	dex
	dey
	bpl L_248c
	jmp L_2381

op_c6_v2
	ldy #$d
	ldx #$d
	bne L_248c

op_c8_v2
	ldy #9
	ldx #$17
	bne L_248c

op_c4_v2
	ldy #$1c
	ldx #$1c
	bne L_248c

L_24AA
    lda PARAM2
L_24AC
    stx V2PAT
    sta V2PATH
    jmp .step

op_ca_v2
    lda #3
L_24B5
    ldy V2SP
    clc
    adc V2PAT
    sta v2_stackl,y
    lda V2PATH
    adc #0
    sta v2_stackh,y
    dec V2SP
    jmp L_24AA

op_ce_v2
    iny
    lda (V2PAT),y
    sta v2_ntrans
    lda #4
    bne L_24B5

op_d0_v2
    sta v2_instr,x
    jmp L_2381

op_d6_v2
	sta v2_fq1addl,x
	jmp L_2381

L_24DF
    rts

L_24E0
    ldx v2_hrtime
    beq L_24DF

    lda v2_ctrl
    and #8
    beq L_2502

    lda V2CNT
    cmp v2_rlsc
    bcs L_2522

    lda #0
    sta v2_rlsc
    lda v2_ctrl
    and #$f6
    sta v2_ctrl
    bne L_251F

L_2502
    lda v2_rlsc
    bne L_2515

    dec v2_hrtime
    bne L_2522

    ldx #6
-	sta SIDV2FREQLO,x
    dex
    bpl -

    rts

L_2515
    dec v2_rlsc
    bne L_2522

    lda v2_ctrl
    and #$f6
L_251F
    sta SIDV2CTRL
L_2522
    lda v2_pmflags
    beq L_2584

    lda v2_pmdelay
    beq L_2532

    dec v2_pmdelay
    jmp L_2584

L_2532
    clc
    ldx v2_pwl
    ldy v2_pwh
    lda v2_p1c
    beq L_254E

    txa
    adc v2_pm1addl
    tax
    tya
    adc v2_pm1addh
    tay
    dec v2_p1c
    jmp L_2578

L_254E
    lda v2_p2c
    beq L_2563

    txa
    adc v2_pm2addl
    tax
    tya
    adc v2_pm2addh
    tay
    dec v2_p2c
    jmp L_2578

L_2563
    lda v2_pmflags
    and #$81
    beq L_2578

    bpl L_2572

    jsr v2_pmretrig

    jmp L_2532

L_2572
    jsr v2_pmcont

    jmp L_2532

L_2578
    stx v2_pwl
    sty v2_pwh
    stx SIDV2PWLO
    sty SIDV2PWHI
L_2584
    lda v2_fqflags
    beq L_259F

    ldx v2_fql
    ldy v2_fqh
    clc
    lda v2_fqdelay
    beq L_25A1

    dec v2_fqdelay
    lda v2_fqflags
    and #2
    bne L_25DD

L_259F
    rts

L_25A0
    clc
L_25A1
    lda v2_f1c
    beq L_25B5

    dec v2_f1c
    txa
    adc v2_fq1addl
    tax
    tya
    adc v2_fq1addh
    jmp L_25E6

L_25B5
    lda v2_f2c
    beq L_25C9

    dec v2_f2c
    txa
    adc v2_fq2addl
    tax
    tya
    adc v2_fq2addh
    jmp L_25E6

L_25C9
    lda v2_f3c
    beq L_25F4

    dec v2_f3c
    txa
    adc v2_fq3addl
    tax
    tya
    adc v2_fq3addh
    jmp L_25E6

L_25DD
    txa
    adc v2_fq4addl
    tax
    tya
    adc v2_fq4addh
L_25E6
    tay
    stx SIDV2FREQLO
    sty SIDV2FREQHI
    stx v2_fql
    sty v2_fqh
    rts

L_25F4
    jsr v2_freqreset

    jmp L_25A0

L_25FA
    lda VEN
    ora v1_hrtime
    ora v2_hrtime
    ora v3_hrtime
    rts

; ============================================================================
;
;	PLAY
;
; ============================================================================

playmusic
    jsr dovoice1
    jsr dovoice2
    jmp dovoice3

	; Voice 3

	!zn v3

dovoice3
    lda VEN
    and #4				; bit 2
    beq L_2619

    dec $1c
    beq .step

L_2619
    jmp L_2799

L_261C
	lda #3
L_261E
    clc
    adc V3PAT
    sta V3PAT
    bcc .step

    inc $15
.step
    ldy #0
    lda (V3PAT),y
    cmp #$c0
    bcc L_264F

    and #$3f
    tax
    lda v3ops,x
    sta .opl
    lda v3opsh,x
    sta .oph
    iny
    lda (V3PAT),y
    tax
    sta PARAM1
    iny
    lda (V3PAT),y
    sta PARAM2
.opl = * + 1
.oph = * + 2
    jmp $0000

L_264C
    jmp L_26D0

L_264F
    sta CVAL
    cmp #$60
    bcc L_2657

    sbc #$60
L_2657
    cmp #$5f
    beq L_264C

    adc v3_ntrans
    tax
    ldy #4
L_2661
    lda #$00
    sta SIDV3PWLO,y
    lda v3_cipw,y
    sta SIDV3PWLO,y
    dey
    bpl L_2661

    lda v3_cictrl
    sta v3_ctrl
    ldy freqsh,x
    lda freqsl,x
    sta v3_fql
    sty v3_fqh
    sta SIDV3FREQLO
    sty SIDV3FREQHI
    lda v3_cipmflags
    sta v3_pmflags
    beq L_269D

    ldy #$09
L_2691
    lda v3_cipm,y
    sta v3_p1time,y
    dey
    bpl L_2691

    jsr v3_pmretrig

L_269D
    ldx v3_cifqflag
    stx v3_fqflags
    beq L_26C4

    ldy #$0d
-   lda v3_instr,y
    sta v3_fq1addl,y
    dey
    bpl -

    txa
    and #$08
    beq +

    lda CVAL
    clc
    adc v3_ntrans
    sta v3_fq3time
    sty v3_fqdelay
+   jsr v3_freqreset

L_26C4
    ldx v3_cirlsc
    ldy v3_cihrtime
    stx v3_rlsc
    sty v3_hrtime
L_26D0
    ldy #$01
    lda (V3PAT),y
    ldx CVAL
    cpx #$60
    bcs L_26DE

    tax
    lda v3_durtable,x
L_26DE
    sta $1c
    lda #$02
    clc
    adc V3PAT
    sta V3PAT
    bcc L_26EB

    inc V3PATH
L_26EB
    jmp L_2799

op_c0_v3
    inc V3SP
    ldy V3SP
    cpy #8
    beq L_26FF
l26f6
    ldx v3_stackl,y
    lda v3_stackh,y
    jmp L_275E

L_26FF
    lda VEN
    and #$fb
    sta VEN
    rts

op_da_v3
	ldx V3SP
	clc
	tya
	adc V3PAT
	sta v3_stackl,x
	lda #0
	adc V3PATH
	sta v3_stackh,x
	lda PARAM1
	sta v3_repstack,x
	dec V3SP
	tya
	jmp L_261E

op_dc_v3
	ldx V3SP
	dec v3_repstack + 1,x
	beq l_272d
	inx
	txa
	tay
	bpl l26f6
l_272d
	inc V3SP
	lda #1
	jmp L_261E

op_c2_v3
	ldy #4
	ldx #$1c
l2738
	lda ($16),y
	sta v3_instr,x
	dex
	dey
	bpl l2738
	jmp L_261C

op_c4_v3
	ldy #$1c
	ldx #$1c
	bne l2738

op_c6_v3
	ldy #$0d
	ldx #$0d
	bne l2738



op_d8_v3
	ldy #9
	ldx #9
	bne l2738

	; D2

op_d2_v3
	iny
	lda (V3PAT),y
	sta v3_ntrans

L_275C
    lda PARAM2
L_275E
    stx V3PAT
    sta V3PATH
    jmp .step

op_ca_v3
    lda #3
L_2767
    ldy V3SP
    clc
    adc V3PAT
    sta v3_stackl,y
    lda V3PATH
    adc #0
    sta v3_stackh,y
    dec V3SP
    jmp L_275C
op_ce_v3
    iny
    lda (V3PAT),y
    sta v3_ntrans
    lda #4
    bne L_2767
op_cc_v3
    stx v3_ntrans
    tya
    jmp L_261E

op_d0_v3
	sta v3_instr,x
	jmp L_261C
op_d6_v3
	sta v3_fq1addl,x
	jmp L_261C

L_2798
    rts

L_2799
    ldx v3_hrtime
    beq L_2798

    lda v3_ctrl
    and #$08
    beq L_27BB

    lda $1c
    cmp v3_rlsc
    bcs L_27DB

    lda #$00
    sta v3_rlsc
    lda v3_ctrl
    and #$f6
    sta v3_ctrl
    bne L_27D8

L_27BB
    lda v3_rlsc
    bne L_27CE

    dec v3_hrtime
    bne L_27DB

    ldx #$06
-   sta SIDV3FREQLO,x
    dex
    bpl -

    rts

L_27CE
    dec v3_rlsc
    bne L_27DB

    lda v3_ctrl
    and #$f6
L_27D8
    sta SIDV3CTRL
L_27DB
    lda v3_pmflags
    beq L_283D

    lda v3_pmdelay
    beq L_27EB

    dec v3_pmdelay
    jmp L_283D

L_27EB
    clc
    ldx v3_pwl
    ldy v3_pwh
    lda v3_p1c
    beq L_2807

    txa
    adc v3_pm1addl
    tax
    tya
    adc v3_pm1addh
    tay
    dec v3_p1c
    jmp L_2831

L_2807
    lda v3_p2c
    beq L_281C

    txa
    adc v3_pm2addl
    tax
    tya
    adc v3_pm2addh
    tay
    dec v3_p2c
    jmp L_2831

L_281C
    lda v3_pmflags
    and #$81
    beq L_2831

    bpl L_282B

    jsr v3_pmretrig

    jmp L_27EB

L_282B
    jsr v3_pmcont

    jmp L_27EB

L_2831
    stx v3_pwl
    sty v3_pwh
    stx SIDV3PWLO
    sty SIDV3PWHI
L_283D
    lda v3_fqflags
    beq L_285C

    and #$08
    bne L_285D

    ldx v3_fql
    ldy v3_fqh
    clc
    lda v3_fqdelay
    beq L_287B

    dec v3_fqdelay
    lda v3_fqflags
    and #$02
    bne L_28B7

L_285C
    rts

L_285D
    ldx v3_fqdelay
    bpl L_2865

    ldx v3_fq4time
L_2865
    lda v3_fq3time
    clc
    adc v3_fq1addl,x
    dex
    stx v3_fqdelay
    tay
    ldx freqsl,y
    lda freqsh,y
    jmp L_28C0

L_287A
    clc
L_287B
    lda v3_f1c
    beq L_288F

    dec v3_f1c
    txa
    adc v3_fq1addl
    tax
    tya
    adc v3_fq1addh
    jmp L_28C0

L_288F
    lda v3_f2c
    beq L_28A3

    dec v3_f2c
    txa
    adc v3_fq2addl
    tax
    tya
    adc v3_fq2addh
    jmp L_28C0

L_28A3
    lda v3_f3c
    beq L_28CE

    dec v3_f3c
    txa
    adc v3_fq3addl
    tax
    tya
    adc v3_fq3addh
    jmp L_28C0

L_28B7
    txa
    adc v3_fq4addl
    tax
    tya
    adc v3_fq4addh
L_28C0
    tay
    stx SIDV3FREQLO
    sty SIDV3FREQHI
    stx v3_fql
    sty v3_fqh
    rts

L_28CE
    jsr v3_freqreset

    jmp L_287A

	; voice 1 data
	
I_FQ1ADD	= 0
I_FQ2ADD	= 2
I_FQ3ADD	= 4
I_FQ4ADD	= 6
I_FQ1TIME	= 8
I_FQ2TIME	= 9
I_FQ3TIME	= 10
I_FQ4TIME	= 11
I_FQDELAY	= 12
I_FQFLAGS	= 13
I_PM1TIME	= 14
I_PM2TIME	= 15
I_PMDELAY	= 16
I_PMFLAGS	= 17
I_PM1ADD	= 18
I_PM2ADD	= 20
I_PMINIT	= 22
; 25,26 appears to be unused
I_CTRL		= 27

	
v1_instr
	!by $f8,$ff,$08,$00,$f8,$ff,$b8,$ff,$03,$06,$03,$00,$1e
v1_cifqflag ; $0d
	!by $05
v1_cipm ; $0e
	!by $14,$14,$00
v1_cipmflags ; $11
	!by $05,$14,$00,$ec,$ff

 ; pwl, pwh $16, $17
v1_cipw
	!wo $0800
v1_cictrl
	!by $41,$dd,$cc		; $18 ctrl, $19 ad, $1a sr
v1_cirlsc
	!by $82	; $1b
v1_cihrtime
	!by $ff	; $1c

	!by $00,$00,$00	; $1d, $1e, $1f

v1_durtable
	!by $00,$04,$08,$0c,$10,$14,$18,$1c,$20,$24,$28,$2c,$30,$34,$38,$3c,$40

v1_stackl	!fill 8
v1_stackh	!fill 8
v1_repstack	!fill 8
v1_ntrans	!by 0

; voice 2

v2_instr
	!by $1e,$00,$e2,$ff,$1e,$00,$97,$00,$03,$06,$03,$00,$32
v2_cifqflag
	!by $07
v2_cipm
	!by $1e,$1e,$00
v2_cipmflags
	!by $05,$e2,$ff,$1e,$00
v2_cipw
	!wo $0b84
v2_cictrl
	!by $41,$88,$cc
v2_cirlsc
	!by $96
v2_cihrtime
	!by $c8
	!by $00,$00,$00

v2_durtable
	!by $00,$04,$08,$0c,$10,$14,$18,$1c,$20,$24,$28,$2c,$30,$34,$38,$3c,$40
v2_stackl
	!fill 8
v2_stackh
	!fill 8
v2_repstack
	!fill 8
v2_ntrans
	!by 0

 ; voice 3

v3_instr
	!by $1e,$00	; fq1add
	!by $e2,$ff	; fq2add
	!by $1e,$00	; fq3add
	!by $63,$00 ; fq4add
	!by $03,$06,$03,$00	; fq1-4 time
	!by $32 ; fqdelay
v3_cifqflag
	!by $07			; $0d
v3_cipm
	!by $1e,$1e,$00
v3_cipmflags
	!by $05			; $11
	!by $e2,$ff,$1e,$00	; pm1add, pm2add
v3_cipw
	!wo $0b84

v3_cictrl
	!by $41,$88,$cc
v3_cirlsc
	!by $96
v3_cihrtime
	!by $c8
	!by $00,$00,$00

v3_durtable
	!by $00,$04,$08,$0c,$10,$14,$18,$1c,$20,$24,$28,$2c,$30,$34,$38,$3c,$40

v3_stackl
	!fill 8
v3_stackh
	!fill 8
v3_repstack
	!fill 8
v3_ntrans
	!by 0



v1_cinstr				; current instrument
v1_fq1addl	!by 0		; 00
v1_fq1addh	!by 0		; 01
v1_fq2addl	!by 0		; 02
v1_fq2addh	!by 0		; 03
v1_fq3addl	!by 0		; 04
v1_fq3addh	!by 0		; 05
v1_fq4addl	!by 0		; 06
v1_fq4addh	!by 0		; 07
v1_fq1time	!by 0		; 08
v1_fq2time	!by 0		; 09
v1_fq3time	!by 0		; 0a
v1_fq4time	!by 0		; 0b
v1_fqdelay	!by 0		; 0c
v1_fqflag	!by 0		; 0d

v1_p1time	!by 0		; 0e
v1_p2time	!by 0		; 0f
v1_pmdelay	!by 0		; 10
v1_pmflags	!by 0		; 11
v1_pm1addl	!by 0		; 12
v1_pm1addh	!by 0		; 13
v1_pm2addl	!by 0		; 14
v1_pm2addh	!by 0		; 15
v1_pmlo		!by 0		; 16
v1_pmhi		!by 0,0,0	; 17, 18, 19
v1_ctrl		!by 0		; 1a
v1_rlsc		!by 0		; 1b
v1_hrtime	!by 0		; 1c
v1_fql		!by 0		; 1d
v1_fqh		!by 0		; 1e
v1_f1c		!by 0		; 1f
v1_f2c		!by 0		; 20
v1_f3c		!by 0		; 21
v1_f4c		!by 0		; 22
v1_pwl		!by 0		; 23
v1_pwh		!by	0		; 24
v1_p1c		!by 0		; 25
v1_p2c		!by 0		; 26		(= 38 bytes)

; voice 2 instrument

v2_fq1addl	!by 0
v2_fq1addh	!by 0
v2_fq2addl	!by 0
v2_fq2addh	!by 0
v2_fq3addl	!by 0
v2_fq3addh	!by 0
v2_fq4addl	!by 0
v2_fq4addh	!by 0
v2_fq1time	!by 0
v2_fq2time	!by 0
v2_fq3time	!by 0
v2_fq4time	!by 0
v2_fqdelay	!by 0
v2_fqflags	!by 0
v2_p1time	!by 0
v2_p2time	!by 0
v2_pmdelay	!by 0
v2_pmflags	!by 0
v2_pm1addl	!by 0
v2_pm1addh	!by 0
v2_pm2addl	!by 0
v2_pm2addh	!by 0
v2_pmlo		!by 0
v2_pmhi		!by 0,0,0
v2_ctrl		!by 0
v2_rlsc		!by 0
v2_hrtime	!by 0
v2_fql		!by 0
v2_fqh		!by 0
v2_f1c		!by 0
v2_f2c		!by 0
v2_f3c		!by 0
v2_f4c		!by 0
v2_pwl		!by 0
v2_pwh		!by 0
v2_p1c		!by 0
v2_p2c		!by 0

; voice 3 instrument

v3_fq1addl	!by 0
v3_fq1addh	!by 0
v3_fq2addl	!by 0
v3_fq2addh	!by 0
v3_fq3addl	!by 0
v3_fq3addh	!by 0
v3_fq4addl	!by 0
v3_fq4addh	!by 0
v3_fq1time	!by 0
v3_fq2time	!by 0
v3_fq3time	!by 0
v3_fq4time	!by 0
v3_fqdelay	!by 0
v3_fqflags	!by 0
v3_p1time	!by 0
v3_p2time	!by 0
v3_pmdelay	!by 0
v3_pmflags	!by 0
v3_pm1addl	!by 0
v3_pm1addh	!by 0
v3_pm2addl	!by 0
v3_pm2addh	!by 0
v3_pmlo		!by 0
v3_pmhi		!by 0,0,0
v3_ctrl		!by 0
v3_rlsc		!by 0
v3_hrtime	!by 0
v3_fql		!by 0
v3_fqh		!by 0
v3_f1c		!by 0
v3_f2c		!by 0
v3_f3c		!by 0
v3_f4c		!by 0
v3_pwl		!by 0
v3_pwh		!by 0
v3_p1c		!by 0
v3_p2c		!by 0

	; zp initial values

zeropage
	!wo track1data		; $10-$11	track 1 ($2ce0)
	!wo track2data		; $12-$13	track 2 ($302e)
	!wo track3data		; $14-$15	track 3 ($2ecc)
	!by 0, 0, 0			; $16-$18	param1, param2, cval
	!by %00000111		; $19		voice enable 3,2,1
	!by 1, 1, 1			; $1a-$1c	note counters
	!by 7, 7, 7			; $1d-$1f	stack pointers

	; lo $0112 16.09 hz = C0
	; hi $acd2 2598.12 hz = E7

freqsh
	!by $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$02	; 00-0c c0
	!by $02,$02,$02,$02,$02,$02,$03,$03,$03,$03,$03,$04 	; 0d-18 c1
	!by $04,$04,$04,$05,$05,$05,$06,$06,$06,$07,$07,$08 	; 19-24 c2
	!by $08,$09,$09,$0a,$0a,$0b,$0c,$0c,$0d,$0e,$0f,$10 	; 25-30 c3
	!by $11,$12,$13,$14,$15,$16,$18,$19,$1b,$1c,$1e,$20 	; 31-3c c4
	!by $22,$24,$26,$28,$2b,$2d,$30,$33,$36,$39,$3d,$40 	; 3d-48 c5
	!by $44,$48,$4c,$51,$56,$5b,$60,$66,$6c,$73,$7a,$81 	; 49-54 c6
	!by $89,$91,$99,$a3,$ac									; 55-59 c7 - e7

freqsl
    !by $12,$23,$34,$46,$5a,$6e,$84,$9b,$b3,$cd,$e9,$06
	!by $25,$45,$68,$8c,$b3,$dc,$08,$36,$67,$9b,$d2,$0c
	!by $49,$8b,$d0,$19,$67,$b9,$10,$6c,$ce,$35,$a3,$17
	!by $93,$15,$9f,$3c,$cd,$72,$20,$d8,$9c,$6b,$46,$2f
	!by $25,$2a,$3f,$64,$9a,$e3,$3f,$b1,$38,$d6,$8d,$5e
	!by $4b,$55,$7e,$c8,$34,$c6,$7f,$61,$6f,$ac,$7e,$bc
	!by $95,$a9,$fc,$a1,$69,$8c,$fe,$c2,$df,$58,$34,$78
	!by $2b,$53,$f7,$1f,$d2

v1ops
v1opsh = * + 1
	!wo op_c0_v1			; c0
	!wo op_c2_v1			; c2
	!wo 0					; c4 (unused)
	!wo op_c6_v1			; c6
	!wo op_c8_v1			; c8
	!wo op_ca_v1			; ca
	!wo op_cc_v1			; cc
	!wo op_ce_v1			; ce
	!wo op_d0_v1			; d0
	!wo 0					; d2 (unused)
	!wo op_d4_v1			; d4
	!wo op_d6_v1			; d6
	!wo op_d8_v1			; d8
	!wo op_da_v1			; da
	!wo op_dc_v1			; dc

v3ops
v3opsh = * + 1
	!wo op_c0_v3			; c0
	!wo op_c2_v3			; c2
	!wo op_c4_v3			; c4
	!wo op_c6_v3			; c6
	!wo 2	; unused
	!wo op_ca_v3			; ca
	!wo op_cc_v3			; cc
	!wo op_ce_v3			; ce
	!wo op_d0_v3			; d0
	!wo op_d2_v3			; d2
	!wo 2	; unused
	!wo op_d6_v3			; d6
	!wo op_d8_v3			; d8
	!wo op_da_v3			; da
	!wo op_dc_v3			; dc

v2opsl
v2opsh = * + 1
	!wo op_c0_v2			; c0
	!wo op_c2_v2			; c2
	!wo op_c4_v2			; c4
	!wo op_c6_v2			; c6
	!wo op_c8_v2			; c8
	!wo op_ca_v2			; ca
	!wo 1					; cc
	!wo op_ce_v2			; ce
	!wo op_d0_v2			; d0
	!wo 1					; d2
	!wo 1					; d4
	!wo op_d6_v2			; d6
	!wo 1					; d8
	!wo op_da_v2			; da
	!wo op_dc_v2			; dc

; ============================================================================
;
;	SONG DATA
;
; ============================================================================

SYNCBIT = 1
RINGBIT = 2


	!zn songdata

ins2b43
	!by $f8,$ff,$00,$00,$00,$00,$00,$00,$ff,$00,$00,$00,$0a,$05
	!by $00,$00,$00,$00,$00,$00,$00,$00,$00,$08,$21,$00,$99,$05,$28

pm2b60
	!by $0a,$ff,$00,$05,$c8,$00,$ec,$ff,$00,$04

l2b6a
	!by $21,$00,$c9,$05,$32
l2b6f
	!by $21,$33,$cb,$05,$96
l2b74
	!by $21,$be,$cd,$c8,$ff

fq2b79
	!by $f8,$ff,$08,$00,$f8,$ff,$b8,$ff,$03,$06,$03,$00,$1e,$05

l2b87
	!by $41,$01,$e7,$04,$0c

	; instr5 for rising ringmod effect
l2b8c
	!by $41 | RINGBIT,$ee,$0f,$05,$32

	; freq
fq2b91
	!wo 30
	!wo -30
	!wo 30
	!wo 0
	!by $03,$06,$03,$00	; fq 1-4 time
	!by $16,$05	; fqdel

pm2b9f
	!by $14,$64,$03,$05,$46,$00,$19,$00,$64,$00

l2ba9
	!by $41,$83,$cb,$5a,$64

fq2bae
	!by $1e,$00,$e2,$ff,$1e,$00,$8c,$00,$03,$06,$03,$00,$0a,$07

fq2bbc
	!by $1e,$00,$e2,$ff,$1e,$00,$10,$00,$03,$06,$03,$00,$14,$07

fq2bca
	!by $00,$00,$6d,$00,$00,$00,$93,$ff,$03,$03,$02,$03,$00,$05

fq2bd8
	!by $1e,$00,$e2,$ff,$1e,$00,$cc,$fe,$03,$06,$03,$00,$20,$07

fq2be6
	!by $c4,$00,$00,$00,$00,$00,$00,$00,$ff,$00,$00,$00,$00,$04

fq2bf4
	!by $e5,$ff,$ff,$ff,$fc,$ff,$fd,$ff,$64,$96,$64,$ff,$19,$05

fq2c02
	!by $00,$00,$fd,$ff,$fd,$ff,$ea,$ff,$32,$ff,$ff,$00,$7d,$05

fq2c10
	!by $55,$00,$00,$00,$ab,$ff,$00,$00,$0a,$1c,$0a,$00,$00,$05

fq2c1e
	!by $08,$00,$f8,$ff,$08,$00,$00,$00,$03,$06,$03,$00,$0a,$05

ins2c2c
	!by $14,$00,$ec,$ff,$14,$00,$00,$00,$03,$06,$03,$00,$0f,$05
	!by $19,$19,$1e,$05

pm2c2e
	!by $0a,$00,$f6,$ff,$00,$08

l2c44
	!by $41,$cc,$9d,$96,$c8

	; arpeggios

arp47c
	!by $00,$04,$07,$0c
	!by $00,$04,$07,$0c
	!by $00,$00

arp38c
	!by $00,$03,$08,$0c
	!by $00,$03,$08,$0c
	!by $00,$00

arp59c
	!by $00,$05,$09,$0c
	!by $00,$05,$09,$0c
	!by $00,$00

	!by $00,$08,$00,$0d

arp27c
	!by $00,$02,$07,$0c
	!by $00,$02,$07,$0c
	!by $00,$00

arp37c
	!by $00,$03,$07,$0c
	!by $00,$03,$07,$0c
	!by $00,$00

arp58c
	!by $00,$05,$08,$0c
	!by $00,$05,$08,$0c
	!by $00,$00

arp368
	!by $00,$03,$06,$08
	!by $00,$03,$06,$08
	!by $00,$00

arp48c
	!by $00,$04,$08,$0c
	!by $00,$04,$08,$0c
	!by $00,$00

	; notes

C0	= $00
Cx0 = $01
D0	= $02
Dx0 = $03
E0	= $04
F0	= $05
Fx0 = $06
G0	= $07
Gx0 = $08
A0	= $09
Ax0 = $0a
H0	= $0b

C1	= $0c
Cx1 = $0d
D1	= $0e
Dx1 = $0f
E1	= $10
F1	= $11
Fx1 = $12
G1	= $13
Gx1 = $14
A1	= $15
Ax1 = $16
H1	= $17

C2	= $18
Cx2 = $19
D2	= $1a
Dx2 = $1b
E2	= $1c
F2	= $1d
Fx2 = $1e
G2	= $1f
Gx2 = $20
A2	= $21
Ax2 = $22
H2	= $23

C3	= $24
Cx3 = $25
D3	= $26
Dx3 = $27
E3	= $28
F3	= $29
Fx3 = $2a
G3	= $2b
Gx3 = $2c
A3	= $2d
Ax3 = $2e
H3	= $2f

C4	= $30		; $60
Cx4 = $31
D4	= $32
Dx4 = $33
E4	= $34
F4	= $35
Fx4 = $36
G4	= $37
Gx4 = $38
A4	= $39
Ax4 = $3a
H4	= $3b

C5	= $3c
Cx5 = $3d
D5	= $3e
Dx5 = $3f
E5	= $40
F5	= $41
Fx5 = $42
G5	= $43
Gx5 = $44
A5	= $45
Ax5 = $46
H5	= $47

C6	= $48
Cx6 = $49
D6	= $4a
Dx6 = $4b
E6	= $4c
F6	= $4d
Fx6 = $4e
G6	= $4f
Gx6 = $50
A6	= $51
Ax6 = $52
H6	= $53

C7	= $54
Cx7 = $55
D7	= $56
Dx7 = $57
E7	= $58

	; commands

NOOP 	= $bf
NOOP2	= $5f

	; PLAY ADDRESS - Play data at address
	!macro PLAY .addr {
		!by $ca
		!wo .addr
	}

	; PLAYT TRANSPOSE,ADDRESS - Play data at addr with note transpose
	!macro PLAYT .nt, .addr {
		!by $ce
		!wo .addr
		!by .nt
	}

	; EXEC ADDRESS - Execute 6502 at address
	!macro EXEC .addr {
		!by $d4
		!wo .addr
	}

	; ARP ADDRESS - Set arpeggio data (10 bytes)
	!macro ARP .addr {
		!by $d8
		!wo .addr
	}

	; ISET OFF,VAL - Set Instrument data
	!macro ISET .off, .val {
		!by $d0, .off, .val
	}

	; CISET OFF,VAL - Set current Instrument data
	!macro CISET .off, .val {
		!by $d6, .off, .val
	}

	!macro INSTR5 .addr {
		!by $c2
		!wo .addr
	}

	; SETPM ADDR - Set pulse mod bytes
	!macro SETPM .addr {
		!by $c8
		!wo .addr
	}

	; SETFQ ADDR - Set freq bytes
	!macro SETFQ .addr {
		!by $c6
		!wo .addr
	}

	; INSTR ADDR - Set complete instrument data
	!macro INSTR .addr {
		!by $c4
		!wo .addr
	}

	; D2 VAL,ADDR
	!macro D2 .v, .a {
		!by $d2
		!wo .a
		!by .v
	}

	; REPEAT NUM - Set repeat point here and number of loops
	!macro REPEAT .num {
		!by $da,.num
	}

	; NEXT - Loop repeat
	!macro NEXT {
		!by $dc
	}

	; RET - Return from PLAY, PLAYT or end track
	!macro RET {
		!by $c0
	}

	; NOOP - No operation / rest (imm time)
	!macro NOOP .time {
		!by $bf,.time
	}

	; NOOP2 - No operation / rest (table time)
	!macro NOOPT .time {
		!by $5f,.time
	}
	
	; TRANS VAL - Set note transpose
	!macro TRANS .v {
		!by $cc, .v
	}

	

p2c9d
	+REPEAT 8
	!by 0,1
	+NEXT
	+REPEAT 4
	!by 0,2
	+NEXT
	+RET

p2ca8
	+PLAYT G4,p2cb1
	+PLAY p2cb1
	+TRANS F4

 ; pattern
p2cb1
	+PLAY p2c9d

p2cb4
	!by 0,3
	!by 0,3
	!by 0,2
	!by 0,2
	!by 0,2
	!by 0,4
	+RET

p2cc1
	+ARP arp59c
	+PLAYT F3,p2e52
	+ARP arp47c
	!by C0,$10
	!by NOOP2,$10
	+ARP arp27c
	+PLAYT F3,p2e52
	+ARP arp38c
	+TRANS 0
	!by E3,$10
	+NOOPT $10
	+RET

; ============================================================================
;
;	TRACK 1 DATA
;
; ============================================================================


	; $2ce0

track1data
	+NOOP $32

	; morsecode
	
	+EXEC .morse1

	+REPEAT 3
	+NOOP 0
	+NEXT

	+EXEC .morse2
	+NOOP 0
	+NOOP 0

	+EXEC .morse3

	; morsecode done

	; 00:31 lead start

	+NOOP 0
	!by G4 + $60,$a8
	+ISET $19,$38			; ad
	+CISET $1d,$b1			; fql
	+CISET $1e,$19			; fqh
	+CISET $0c,$0a			; fqdelay
	+CISET $0d,%00000111	; fqflag
	+NOOPT 2
	!by F4,4
	+SETFQ fq2c10
	!by Gx4,12
	+SETFQ fq2b79
	!by Gx4,4
	!by G4,12
	!by Dx4,4
	!by C4 + $60,$98

	+REPEAT 2
	!by G3,2
	!by Ax3,2
	+NEXT

	!by G3,2
	+ISET $1b,$32			; release
	!by C4 + $60,$80

	; 00:45 drums

	+ISET $00,$b0			; fq1add
	+ISET $08,$ff			; fq1time
	+ISET $0c,$00			; fqdelay
	+ISET $1a,$a9			; sr
	+ISET $1b,$0a			; release
	!by D4,1
	!by D4,1
	!by D4,1
	!by D4,1
	!by D4,2
	!by Gx3,2
	!by Fx3,2
	!by Gx3,4
	!by Gx3,2
	!by Gx3,2
	!by Fx3,4
	
	; 00:52 lead
	
	+SETFQ fq2be6
	+SETPM pm2b9f
	+INSTR5 l2ba9
	!by $00,$0a
	+SETFQ fq2b91
	
	!by Ax4 + $60, $80
	!by A4 + $60, $80
	!by G4 + $60, $c0
	
	!by G4,8
	!by A4,8
	!by Ax4 + $60, $80
	!by A4 + $60, $50
	+SETFQ fq2bae
	!by A4, $0a
	+SETFQ fq2b91
	!by Ax4,1
	!by A4,1
	!by G4 + $60, $60

    +SETFQ fq2bbc

	!by E4,8
	+SETFQ fq2bca

	!by E4 + $60,$50
	+SETFQ fq2b91

	!by G4,6
	!by A4,6
	!by Ax4 + $60,$80
	!by C5 + $60,$50
	!by D5,4
	!by E5,4
	!by F5,4
	!by E5 + $60,$50
	!by F5,4
	!by E5,2
	!by F5,1
	!by E5,1
	!by D5,4
	!by C5 + $60,$60
	!by G4,4
	!by A4,4
	!by Ax4 + $60,$80
	!by F5 + $60,$80
	+ISET $1b,$c8			; rlsc
	!by G5 + $60, $e0
	+SETFQ fq2bd8
	!by G5,8
	+ISET $0d,$00			; fqflag
	+NOOP 0
    +NOOP 0
	
	; 01:40 rising notes used with sync

	+INSTR5 l2b8c
	+ISET $11,$00			; pmflags
	+ISET $17,$08			; pwh
	+TRANS C2
	+REPEAT $40
	+EXEC .inctrans
    !by C0,2
	+NEXT

	+INSTR5 l2b87
	+PLAY p2ca8

	+PLAYT Fx4,p2cb1
	+PLAY p2ca8
	+PLAYT G4,p2c9d
	+PLAYT Fx4,p2cb4

	+ISET $18,$41			; ctrl
	+SETFQ arp59c

	+PLAY p2cc1
	+PLAY p2cc1

	+INSTR5 l2b87
	+ISET $0d,$00			; fqflag
	+REPEAT 4
	+REPEAT 3
	+PLAYT F3,p2cb1
	+NEXT
	+PLAYT E3,p2cb1
	+NEXT

	+INSTR5 l2c44
	+ISET $18,$15			; ctrl: tri + gate + ringmod
    +SETPM pm2c2e
	+SETFQ fq2bf4
	+NOOP 0
	+TRANS 0
	+NOOP 0
	!by D4 + $60,$7d
	+CISET $1d,$b3			; ?
	+CISET $1e,$08			; ?
	+NOOP $c8
	+CISET $1c,$ff			; hrtime
	+NOOP $c8
	+CISET $1c,$64			; hrtime
	+RET

.inctrans
	inc v1_ntrans
	rts


	; set morsecode pitch

.morse2					; freq of morsecode part 2
	ldx #$38
	ldy #$1b			; $1b38
	bne +
.morse3					; freq of morsecode part 1
	ldx #$b1
	ldy #$19
+	stx $d400			; $19b1
	sty $d401
	rts

p2e3c
	+PLAY p2e3f

p2e3f
	!by 0,6
	!by 0,4
	!by 0,4
	!by 0,6
	!by 0,4
	!by 12,2
	!by 0,2
	!by 12,2
	!by 12,2
	+RET

p2e52
	+PLAY p2e68
	!by 0,2
	!by 0,4
	!by 0,2
	!by 0,2
	!by 0,2
	!by 0,1
	!by 0,1
	!by 0,1
	!by 0,1
	+RET

p2e68
    !by 0,2
	!by 0,2
	!by 0,2
	!by 0,1
	!by 0,1
	!by 0,2
	!by 0,2
	!by 0,1
    !by 0,1
	!by 0,2
	+RET

p2e7d
	+SETFQ arp59c
	+REPEAT 4
	+PLAY p2e8a
	+NEXT
	+RET

p2e87
	+PLAY p2e8a

p2e8a
	+ARP arp59c
	+PLAYT F3,p2e52
	+ARP arp47c
	+PLAY p2e52
	+ARP arp27c
	+PLAY p2e52
	+ARP arp38c
	+D2 E3,p2e52

p2ea4
	!by 0,1
	!by 0,1
    !by 0,4
	!by 0,4
	!by 0,4
	!by 0,1
	!by 0,1
	+RET

	+PLAY p2eb6

p2eb6
	+PLAY p2ea4

p2eb9
	!by 0,2
	!by 0,2
	!by 0,2
	!by 0,2
	!by 0,4
	!by 0,1
	!by 0,1
	!by 0,1
	!by 0,1
	+RET

; ============================================================================
;
;	TRACK 3 DATA
;
; ============================================================================

track3data
	!by C0 + $60,$32
	+NOOP 0
	+INSTR ins2b43
	+NOOP 0
	+REPEAT 2
	+PLAYT C3,p2e3c
	+PLAYT Gx2,p2e3c
	+PLAYT F2,p2e3c
	+NEXT

	+PLAYT C3,p2e3c
	+PLAY p2e7d
	
	; 1
	
	+ARP arp37c
	+PLAYT G4,p2eb6
	
	+ARP arp38c
	 +PLAY p2eb6
	 
	+ARP arp59c
	+PLAYT F4,p2eb6
	
	+ARP arp38c
	+PLAYT Fx4,p2eb6
	
	; 2
	
	+ARP arp37c
	+PLAYT G4,p2eb6
	
	+ARP arp38c
	+PLAY p2eb6
	
	+ARP arp59c
	+PLAYT F4,p2eb6
	
	+ARP arp368
	+PLAYT Fx4,p2eb6
	
	; 3
	
	+ARP arp37c
	+PLAYT G4,p2eb6
	
    +ARP arp38c
	+PLAY p2eb6
	
	+ARP arp59c
	+PLAYT F4,p2eb6
	
	+ARP arp48c
    +PLAYT Fx4,p2ea4
	
	+ARP arp368
	+PLAY p2eb9
	
	; 4
	
	+ARP arp37c
	+PLAYT G4,p2eb6
	+ARP arp38c
	+PLAY p2eb6
	+ARP arp59c
	+PLAYT F4,p2eb6
	+ARP arp58c
	+PLAYT G4,p2ea4
	+ARP arp38c
	+PLAYT Fx4,p2eb9
	+PLAY p2e87
	
	+INSTR ins2c2c
    +NOOP 0
	+NOOP 0
	+TRANS 0
	!by D4 + $60,$80
	!by C4 + $60,$80
	!by C4 + $60,$00
	!by D4 + $60,$80
    !by C4 + $60,$50
	+ISET $10,$00			; pmdelay
	!by Ax3,4
	!by A3,4
	!by Ax3,4
	!by C4 + $60,$50
	!by G3,4
	!by C4,4
	!by D4,4
	!by E4,12
	!by F4,4
	!by G4,4
	!by A4,4
	!by Ax4,4
	!by C5,4
	!by Ax4 + $60,$80
	!by A4 + $60,$80
	!by G4 + $60,$00
	!by D4 + $60,$80
	!by C4 + $60,$80
	!by C4 + $60,$80
	!by D4 + $60,$80
	+SETFQ fq2c02
    !by C4 + $60,$19
	+CISET $0d,$07			; fq4addh
	+NOOP $64
	+CISET $1d,$93			; fql
	+CISET $1e,$08			; fqh
	+NOOP $c8
	+CISET $1c,$ff			; hrtime
	+NOOP $c8
	+CISET $1c,$ff			; hrtime
	+RET

p2fc0
	!by F0,4
	!by F0,2
	!by F0,2
	!by F0,2
    !by F0,4
	!by F0,4
	!by C0,2
	!by Dx0,2
	!by F0,2
	!by Gx0,2
	!by G0,2
	!by F0,2
    !by Dx0,2
	+RET

p2fdb
	+PLAY p2fde

p2fde
	+PLAY p2ff4

p2fe1
	!by 0,4
	!by 0,1
	!by 0,1
	!by 0,2
	!by 0,2
	!by 12,2
	!by 0,2
	!by 0,2
	!by 0,2
	+RET

p2ff4
	!by 0,1
	!by 0,1
    !by 0,2
	!by 0,2
	!by 0,2
	!by 12,2
	!by 0,4
	+RET

p3003
	+PLAY p3006

p3006
	+PLAYT Ax2,p2fde
	+PLAYT F2,p2fde
	+PLAYT C3,p2fdb
	+RET

p3013
	+REPEAT 4
	!by F0,4
	!by C0,4
	+NEXT
	+RET

p301b
	+REPEAT 4
	!by C0,8
	+NEXT
	+RET
p3021
	+PLAYT G2,p2fde
	+PLAYT Dx3,p2fde
	+PLAYT Ax2,p2fde
	+RET

; ============================================================================
;
;	TRACK 2 DATA
;
; ============================================================================

track2data						; $302e
	!by C0 + $60,$32
	+INSTR ins2b43
	+NOOP 0
	+REPEAT 14
	+PLAYT G2,p2fc0
	+NEXT
	+INSTR5 l2b6a
	+PLAY p2fc0
	+SETPM pm2b60
	+ISET $18,$41				; ctrl
    !by F0,2
	!by F1,1
	!by F1,1
	!by F1,2
	!by F1,2
	!by F1,2
	!by F0,6
	!by F1,4
    !by F0,4
	!by F1,2
	!by F0,2
	!by E0,4
	
	+ISET $0d,$00				; fqflag
	+PLAY p3003
	+PLAY p3003
	+PLAYT G2,p2fdb
	+PLAYT F2,p2fde
	+PLAYT Fx2,p2fde
	+PLAYT G2,p2fdb
	+PLAYT Ax2,p2fde
	+PLAYT D3,p2fde
	+PLAY p3021

	+PLAYT D3,p2ff4
    +PLAYT Fx2,p2fe1
	+PLAY p3021
	+PLAYT C3,p2ff4
	+PLAYT D3,p2fe1
	+PLAY p3003
	+REPEAT 3
	+PLAYT F2,p3013
	+PLAYT C2,p3013
	+PLAYT G2,p3013
    +PLAY p3013
	+NEXT

	+INSTR5 l2b6f
	+SETFQ fq2c1e
	+ISET $11,$00				; pmflags
	+PLAYT Ax2,p301b
	+PLAYT F2,p301b
	+PLAYT C3,p301b
	+PLAYT C2,p301b
	+PLAYT Ax2,p301b
	+PLAYT F2,p301b
	+PLAYT Gx2,p301b
	+PLAYT Ax2,p301b
	+INSTR5 l2b74
    !by D0 + $60,$00
	+CISET $1c,$ff				; hrtime
	+NOOP $fe
	+CISET $1c,$ff				; hrtime
	+RET

initmusic
    lda #0
    sta v1_hrtime
    sta v2_hrtime
    sta v3_hrtime
    ldx #$17
-   sta $d400,x
    dex
    bpl -

	; init zeropage

    ldx #$f
-   lda zeropage,x
    sta $10,x
    dex
    bpl -

.restoreplay
    ldx #<playmusic
    ldy #>playmusic
    stx playl
    sty playh
    rts

	; morsecode player
	
l310b
	lda #0
l310e = * + 1
	beq l310f
l310f
	ldy v1_repstack
	lda .morsedata,y
	beq .restoreplay

	inc v1_repstack
	lsr
	tax
	lda l3194,x
	sta v1_repstack + 1
	and #3
	sta v1_repstack + 2
	ldx #$1c
	bne l3198

l312b
	asl v1_repstack + 1
	bcc l3138
	lda #6
	ldx #$49
l3134
	ldy #$21
	bne l316a
l3138
	lda #2
	ldx #$50
	bne l3134
	dec v1_repstack + 3
	bpl l319b
	dec v1_repstack + 2
	bpl l312b

	lda #6
	sta v1_repstack + 3
	ldx #$42
	bne l3198
	dec v1_repstack + 3
	bpl l319b
	bmi l310f
	dec v1_repstack + 3
	bpl l319b
	bmi l3164
	dec v1_repstack + 3
	bpl l319b
l3164
	lda #2
	ldx #$2f
	ldy #$20
l316a
	sta v1_repstack + 3
	bne l3195

	; MORSE CODE

.morse1
	lda #$a0
	sta SIDV1SR		; sr1
	ldx #$4b
	stx SIDV1FREQLO		; fq1l
	ldy #$22
	sty SIDV1FREQHI		; fq1h
	ldx #<l310b
	ldy #>l310b
	stx playl
	sty playh
	lda #0
	tax
	tay
	sta v1_repstack + 0
	sta SIDV1AD			; ad1
l3194 = * + 2
	sta v1_hrtime
l3195
	sty SIDV1CTRL		; ctrl1
l3198
	stx l310e
l319b
	jmp playmusic

	; morse code data
.morsedata
	!by $84,$92,$98,$98,$84,$82,$a4,$9c,$82,$88,$82,$ac,$92
    !by $88,$86,$9e,$98,$98,$92,$8a,$a4,$9a,$82,$a4,$a8,$92,$9c,$8e,$82
    !by $98,$ae,$82,$b2,$a8,$9e,$9c,$b2,$a0,$9e,$9a,$8c,$a4,$8a,$a8,$a6
    !by $a8,$8a,$ac,$8a,$ae,$82,$90,$92,$88,$00,$41,$83,$a3,$82,$00,$23
    !by $c2,$03,$10,$73,$a2,$43,$c1,$81,$e2,$63,$d3,$42,$02,$80,$22,$13
    !by $62,$93,$b3,$c3,$c5,$ce,$ff,$00,$00,$00,$00,$00,$00,$00,$00,$00
    !by $00,$00,$00

}
