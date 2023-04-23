
; Reversed by dmx87

	!to "Whittaker_David_Panther.sid",plain

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
SIDFCLO = $d415
SIDFCHI = $d416
SIDRESFILT = $d417
SIDMODEVOL = $d418
SIDPOTX = $d419
SIDPOTY = $d41a
SIDOSC3 = $d41b
SIDENV3 = $d41c

	* = $0000

	!text "PSID"
	!by $00,$02		; version 2
	!by $00,$7c		; data offset
	!by $00,$00		; Load (auto)
	!by >init,<init	; Init
	!by >play,<play	; Play
	!by $00,$01		; num. songs
	!by $00,$01		; first song
	!wo 0
	!wo 0
_t	!text "Panther"
	!fill 32 - (* - _t)
_a	!text "David Whittaker"
	!fill 32 - (* - _a)
_c	!text "1986 Mastertronic"
	!fill 32 - (* - _c)
	!wo $1400			; v2 flags
	!wo $0000			; Start page, page length (reloc)
	!wo $0000			; Reserved

	; auto load address
	!wo $9000

!pseudopc $9000 {

	* = $9000
	
VD	= $fa

VD_FLAGS	= $00
VD_PAT		= $01	; Pattern pointer
VD_PATH 	= $02
VD_B03		= $03
VD_TRACK	= $03	; Track pointer
VD_TRACKH 	= $04
VD_B05		= $05
VD_B07		= $07
VD_ARP2L	= $09
VD_ARP2H	= $0a
VD_ARP		= $0b	; Arp pointer
VD_ARPH		= $0c
VD_B0E		= $0e
VD_B0F		= $0f
VD_NOTC		= $10	; Note count
VD_NOTD		= $11	; Note duration
VD_NOTE		= $12	; Last note
VD_AD		= $13	; AD
VD_SR		= $14	; SR
VD_FQL		= $15	; Freq
VD_FQH		= $16
VD_PWL		= $17	; Pulse width
VD_PWH		= $18
VD_B19		= $19
VD_B1A		= $1A
VD_B1B		= $1B
VD_B1C		= $1C
VD_B1D		= $1D
VD_B1E		= $1E
VD_B1F		= $1f
VD_B20		= $20
VD_B21		= $21
VD_WAVE		= $22	; Waveform (without gate bit)
VD_CTRL		= $23	; Ctrl

	
init
    lda #0
    sta PlayFlag
    ldy #$23
    lda #0
_1  sta v1data,y
    sta v2data,y
    sta v3data,y
    dey
    bpl _1

    tay
    lda Track1
    sta v1data + VD_TRACK
    sta $f8
    lda Track1 + 1
    sta v1data + VD_TRACKH
    sta $f9

	; Load pattern ptr with first pattern
	
    lda ($f8),y
    sta v1data + VD_PAT
    iny
    lda ($f8),y
    sta v1data + VD_PATH

    dey

    lda Track2
    sta v2data + VD_TRACK
    sta $f8
    lda Track2 + 1
    sta v2data + VD_TRACKH
    sta $f9
    lda ($f8),y
    sta v2data + VD_PAT
    iny
    lda ($f8),y
    sta v2data + VD_PATH
    dey

    lda Track3
    sta v3data + VD_TRACK
    sta $f8
    lda Track3 + 1
    sta v3data + VD_TRACKH
    sta $f9
    lda ($f8),y
    sta v3data + VD_PAT
    iny
    lda ($f8),y
    sta v3data + VD_PATH

    iny
    sty v1data + VD_B05
    sty v2data + VD_B05
    sty v3data + VD_B05
    dey
    sty v1data + VD_NOTC
    sty v2data + VD_NOTC
    sty v3data + VD_NOTC
    dey
    sty v1data
    sty v2data
    sty v3data
    sty v1data + VD_B1D
    sty v2data + VD_B1D
    sty v3data + VD_B1D
    sty v1data + VD_B21
    sty v2data + VD_B21
    sty v3data + VD_B21
    jsr SIDreset

    lda #$0f
    sta ModeVol
    lda #1
    sta TempoCnt
    sta PlayFlag
    rts

	; Voice 1





v1data
	!by $00,$10,$99,$a6,$97
	!by $1a,$00,$00,$38,$00,$00,$00,$00,$00,$04,$01
    !by $01,$01,$5d,$40,$00,$00,$eb,$50,$05,$00,$00,$00,$00
    !by $00,$02,$08,$28,$01,$80,$80

	; Voice 2

v2data  !by $00		;00
		!by $db		;01
		!by $99		;02
		!by $12		;03
		!by $98		;04
		; 05 - 0F
		!by $18,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		; 10 - 12
		!by $01,$01,$18
		!by $08		; 13
		!by $10		; 14
		!by $64		; 15
		!by $04		; 16
		!by $28		; 17
		!by $05		;18
		!by $00,$06,$03,$09		; 19 - 1C
		!by $60,$02,$08,$28		; 1D - 20
		!by $01,$40				; 21 - 22
		!by $40					; 23

	; Voice 3

v3data  !by $00
		!by $b9
		!by $9a
		!by $96
		!by $98
		!by $18,$00,$52,$08,$73,$97,$74,$97,$0a,$00,$01
		!by $01,$01,$24
		!by $08
		!by $10
		!by $ba
		!by $08
		!by $dc
		!by $05
		!by $00,$0a,$05,$0f
		!by $60,$01,$08,$50
		!by $01,$40
		!by $40

PlayFlag
	!by $01

L_9117
    !by $14

L_9118
    !by $0a

L_9119
    !by $00
	
	!by $aa,$a0,$00,$8c,$16,$91,$a9,$fa,$18,$69,$06,$ca,$10,$fa,$aa
    !by $bd,$a0,$97,$8d,$ad,$90,$bd,$a1,$97,$8d,$ae,$90,$bd,$a2,$97,$8d
    !by $d1,$90,$bd,$a3,$97,$8d,$d2,$90,$bd,$a4,$97,$8d,$f5,$90,$bd,$a5
    !by $97,$8d,$f6,$90,$ee,$16,$91

noplay
    rts

	; -------------------------------------------------------------------------
	; Play
	; -------------------------------------------------------------------------

play
    cld
    lda PlayFlag
    beq noplay

    dec TempoCnt
    bne L_9182

    lda #<v1data
    sta VD
    lda #>v1data
    sta VD + 1
    jsr GetNote

    lda #<v2data
    sta VD
    lda #>v2data
    sta VD + 1
    jsr GetNote

    lda #<v3data
    sta VD
    lda #>v3data
    sta VD + 1
    jsr GetNote

SongTempo = * + 1
    lda #$07
    sta TempoCnt
L_9182
    lda #<v1data
    sta VD
    lda #>v1data
    sta VD + 1
    jsr SoundUpdate

    stx v1data + VD_FQL
    sty v1data + VD_FQH
    lda #<v2data
    sta VD
    lda #>v2data
    sta VD + 1
    jsr SoundUpdate

    stx v2data + VD_FQL
    sty v2data + VD_FQH
    lda #<v3data
    sta VD
    lda #>v3data
    sta VD + 1
    jsr SoundUpdate

    stx v3data + VD_FQL
    sty v3data + VD_FQH
    lda v1data + VD_FQL
    sta SIDV1FREQLO
    lda v1data + VD_FQH
    sta SIDV1FREQHI
    lda v1data + VD_PWL
    sta SIDV1PWLO
    lda v1data + VD_PWH
    sta SIDV1PWHI
    lda v1data + VD_AD
    sta SIDV1AD
    lda v1data + VD_SR
    sta SIDV1SR
    ldx v1data + VD_CTRL
    lda v1data + VD_B19
    beq _v2

    dec v1data + VD_B19
    stx SIDV1CTRL

_v2 inx
    stx SIDV1CTRL
    lda v2data + VD_FQL
    sta SIDV2FREQLO
    lda v2data + VD_FQH
    sta SIDV2FREQHI
    lda v2data + VD_PWL
    sta SIDV2PWLO
    lda v2data + VD_PWH
    sta SIDV2PWHI
    lda v2data + VD_AD
    sta SIDV2AD
    lda v2data + VD_SR
    sta SIDV2SR
    ldx v2data + VD_CTRL
    lda v2data + VD_B19
    beq _v3

    dec v2data + VD_B19
    stx SIDV2CTRL
	
_v3 inx
    stx SIDV2CTRL
    lda v3data + VD_FQL
    sta SIDV3FREQLO
    lda v3data + VD_FQH
    sta SIDV3FREQHI
    lda v3data + VD_PWL
    sta SIDV3PWLO
    lda v3data + VD_PWH
    sta SIDV3PWHI
    lda v3data + VD_AD
    sta SIDV3AD
    lda v3data + VD_SR
    sta SIDV3SR
    ldx v3data + VD_CTRL
    lda v3data + VD_B19
    beq _out

    dec v3data + VD_B19
    stx SIDV3CTRL
_out
    inx
    stx SIDV3CTRL
    lda ModeVol
    sta SIDMODEVOL
    rts

StopMusic
    lda #0
    sta PlayFlag
SIDreset
    ldx #$17
    lda #0
-  	sta $d400,x
    dex
    bpl -
	rts

TempoCnt
    !by 2

CommandTable
	!wo L_93FB			; $80
	!wo L_93CF			; $81
	!wo L_93D7			; $82
	!wo L_93DF			; $83
	!wo L_9363			; $84
	!wo L_935B			; $85
	!wo L_93C5			; $86
	!wo L_93BD			; $87
	!wo L_9304			; $88
	!wo L_939A			; $89
	!wo cmd_Noise			; $8a
	!wo cmd_Pulse			; $8b
	!wo cmd_Saw			; $8c
	!wo cmd_Tri			; $8d
	!wo L_93EF			; $8e
	!wo cmd_PulseHi		; $8f
	!wo L_9297			; $90
	!wo cmd_StopMusic		; $91
	!wo cmd_RingTri		; $92
	!wo cmd_SyncSquare	; $93

L_9297
    ldy #0
    lda ($f8),y
    pha
    iny
    lda ($f8),y
    pha
    iny
    lda ($f8),y
    pha
    lda $f8
    clc
    adc #3
    sta $f8
    bcc +
    inc $f9
	
+   ldy #VD_B20
    pla
    sta (VD),y			; $20
    dey
    pla
    sta (VD),y			; $1f
    dey
    pla
    sta (VD),y			; $1e
	
    ldy #VD_B21
    lda #$01
    sta (VD),y
    jmp NextPatValue

cmd_Noise
    lda #$80
    bne csetwave

cmd_Pulse
    lda #$40
    bne csetwave

cmd_Saw
    lda #$20
    bne csetwave

cmd_Tri
    lda #$10
    bne csetwave

cmd_RingTri
    lda #$14
    bne csetwave

cmd_SyncSquare
    lda #$42
	
csetwave
    ldy #VD_WAVE
    sta (VD),y
    jmp NextPatValue

cmd_PulseHi
    ldy #VD_PWL
    lda #0
    sta (VD),y
    ldy #0
    lda ($f8),y
    ldy #VD_PWH
    sta (VD),y
    inc $f8
    bne +
    inc $f9
+   ldy #VD_B21
    lda #0
    sta (VD),y
    jmp NextPatValue

cmd_StopMusic
    pla
    pla
    jmp StopMusic

L_9304
    ldy #VD_B03
	lda (VD),y
	sta $f8
	iny
	lda (VD),y
	sta $f9
	ldy #VD_B05
	lda (VD),y
	clc
	adc $f8
	sta $f8
	iny
	lda (VD),y
	adc $f9
	sta $f9
	dey
	lda (VD),y
	adc #$02
	sta (VD),y
	iny
	lda (VD),y
	adc #$00
	sta (VD),y
	ldy #$00
	lda ($f8),y
	iny
	ora ($f8),y
	bne L_934C
	
	ldy #VD_B03
	lda (VD),y
	sta $f8
	iny
	lda (VD),y
	sta $f9
	ldy #VD_B05
	lda #2
	sta (VD),y
	lda #0
	iny
	sta (VD),y
L_934C
	ldy #$00
	lda ($f8),y
	tax
	iny
	lda ($f8),y
	sta $f9
	stx $f8
	jmp NextPatValue
	
L_935B
	ldy #VD_FLAGS
	lda (VD),y
	ora #$20						; bit 6
	bne L_9369

L_9363
	ldy #VD_FLAGS
	lda (VD),y
	ora #$04						; bit 3
	
L_9369
	sta (VD),y
	lda #$00
	
	ldy #VD_B07
	sta (VD),y
	iny
	sta (VD),y					; B08 = B07
	tay
	lda ($f8),y
	pha
	iny
	lda ($f8),y
	pha
	iny
	lda ($f8),y
	ldy #VD_B0F
	sta (VD),y
	pla
	ldy #VD_B0E
	sta (VD),y
	dey
	pla
	sta (VD),y
	lda $f8
	clc
	adc #3
	sta $f8
	bcc +
	inc $f9
+	jmp NextPatValue

L_939A
	ldy #0
	lda ($f8),y
	ldy #VD_B1B
	sta (VD),y
	inc $f8
	bne +
	inc $f9
+	ldy #0
	lda ($f8),y
	
	ldy #VD_B1A
	sta (VD),y
	ldy #VD_B1C
	sta (VD),y
	inc $f8
	bne +
	inc $f9
+	jmp NextPatValue
	
L_93BD
	ldy #VD_FLAGS
	lda (VD),y
	ora #$80						; bit 8
	sta (VD),y
	
L_93C5
	ldy #VD_FLAGS
	lda (VD),y
	ora #$08						; bit 4
	sta (VD),y
	bne NextPatValue

L_93CF
	ldy #VD_B1D
	lda #0
	sta (VD),y
	beq NextPatValue

L_93D7
	ldy #VD_B1D
	lda #$40						; bit 7
	sta (VD),y
	bne NextPatValue

L_93DF
	ldy #0
	lda ($f8),y
	sta ModeVol
	inc $f8
	bne +
	inc $f9
+	jmp NextPatValue

L_93EF
	ldy #VD_FLAGS
	lda (VD),y
	ora #$02						; bit 2
	ora #$01						; + bit 1
	sta (VD),y
	bne NextPatValue
L_93FB
	jmp L_9431

	; -------------------------------------------------------------------------
	;
	; -------------------------------------------------------------------------


GetNote
    ldy #VD_NOTC
    lda (VD),y				; get note count
    sec
    sbc #1
    sta (VD),y
    bne pnotdone				; note not done

    ldy #VD_FLAGS
    sta (VD),y

    ldy #VD_PAT
    lda (VD),y
    sta $f8
    iny
    lda (VD),y
    sta $f9						; ($f8) = pattern

NextPatValue
    ldy #0
    lda ($f8),y					; val from pattern
    tax
    inc $f8						; pos++
    bne +
    inc $f9
+   txa
    bmi pspecial

	; Note
	
    ldy #VD_B19
    lda #1
    sta (VD),y
	
    txa
    ldy #VD_NOTE
    sta (VD),y				; $12 = note

L_9431
    ldy #VD_NOTD				; note duration
    lda (VD),y
	
    ldy #VD_NOTC				; reset counter
    sta (VD),y

    lda $f8						; store new pat ptr
    ldy #VD_PAT
    sta (VD),y
    lda $f9
    iny							; VD_PATH
    sta (VD),y
    rts

pnotdone
    ldy #VD_FLAGS
    lda (VD),y
    tax
    and #$08					; bit 4 set ?
    bne _b4
    rts

_b4 ldy #VD_NOTE				; yes
    txa
    bpl _sub

    lda (VD),y
    clc
    adc #1
    sta (VD),y
    rts

_sub
    lda (VD),y
    sec
    sbc #1
    sta (VD),y
    rts

	; Handle effect/command
	
pspecial
    cmp #$b8					; $80 - $b7 = command
    bcc pcommand

    adc #$1f					; $d8 - $e0 = note duration
    bcs pdur

    adc #$10					; $c8 - $e0 = adsr
    bcs padsr

    adc #$10					; $d8 - $e0
    bcc ptempo
	
	; Arpeggio
	
    asl
    tax
    lda ArpTable,x
    
	ldy #VD_ARP
    sta (VD),y
    
	ldy #VD_ARP2L
    sta (VD),y
    
	lda ArpTable + 1,x
	
    ldy #VD_ARPH
    sta (VD),y
	
    ldy #VD_ARP2H
    sta (VD),y
	
    ldy #VD_FLAGS
    lda #$10					; bit 5
    ora (VD),y
    sta (VD),y
    jmp NextPatValue

ptempo
    adc #$09
    sta SongTempo
    jmp NextPatValue

	; Duration
	
pdur
    adc #0
    ldy #VD_NOTD
    sta (VD),y
    jmp NextPatValue

	; ADSR
	
padsr
    ldy #0
    lda ($f8),y
	
    ldy #VD_AD
    sta (VD),y
	
    ldy #1
    lda ($f8),y
	
    ldy #VD_SR
    sta (VD),y
	
    lda $f8
    clc
    adc #2
    sta $f8
    bcc +
    inc $f9
+   jmp NextPatValue

	; Execute command
	
pcommand
    asl
    tax
    lda CommandTable,x
    sta _j + 1
    lda CommandTable + 1,x
    sta _j + 2
_j  jmp L_9297

	; -------------------------------------------------------------------------
	; SoundUpdate
	; -------------------------------------------------------------------------


SoundUpdate
    ldy #VD_NOTE
    lda (VD),y
    tax
    ldy #VD_FLAGS
    lda #$10
    and (VD),y					; bit 5 set ?
    beq _noarp

	; Bit 5: Arpeggio
	
    ldy #VD_ARP
    lda (VD),y
    sta $f8
    iny
    lda (VD),y
    sta $f9							; ($f8) = ARP1
	
    ldy #0
    lda ($f8),y						; Get arp value
    cmp #$54
    bcc _note

    ldy #VD_ARP2L					; Reset arp if >= $54
    lda (VD),y
    sta $f8
    iny
    lda (VD),y
    sta $f9
    ldy #0
    lda ($f8),y
	
_note
    sta _transpose
    inc $f8							; Arp pos++
    bne +
    inc $f9
+   lda $f8
    ldy #VD_ARP
    sta (VD),y
    lda $f9
    iny
    sta (VD),y
	
    txa
    clc
_transpose = * + 1
    adc #0
    tax
	
_noarp
    txa
    asl
    tax
    lda NoteFreqsL,x
    sta $f8
    lda NoteFreqsH,x
    sta $f9							; $f8, $f9 = freq
	
    ldy #VD_B1D
    lda (VD),y
    and #$40						; bit 7 set ?
    bne L_9537

    jmp L_95C0						; no

L_9537								; yes
    ldy #VD_B1A
    lda (VD),y
    asl
    sta L_9117
	
    ldy #VD_B1D
    lda (VD),y
    bpl L_9554

    ldy #VD_FLAGS
    lda (VD),y
    and #1							; bit 1 set ?
    beq L_9554

    ldy #VD_B1C						; yes
    lda (VD),y
    jmp L_9590

L_9554
    ldy #VD_B1D
    lda (VD),y
    and #$20						; bit 6?
    bne L_9573

    ldy #VD_B1C
    lda (VD),y
    ldy #VD_B1B
    sec
    sbc (VD),y
    bcs L_958C

    ldy #VD_B1D
    lda (VD),y
    ora #$20						; set bit 6
    sta (VD),y
    lda #0
    beq L_958C

L_9573
    ldy #VD_B1C
    lda (VD),y
    ldy #VD_B1B
    clc
    adc (VD),y
    cmp L_9117
    bcc L_958C

    ldy #VD_B1D
    lda (VD),y
    and #$df						; clear bit 6
    sta (VD),y
    lda L_9117
L_958C
    ldy #VD_B1C
    sta (VD),y
L_9590
    ldy #VD_B1A
    sec
    sbc (VD),y
    sta L_9118
    txa
    ldx #0
    bcs +
    dex
+   stx L_9119
    clc
    adc #$a0
    bcs _addfq

-   asl L_9118
    rol L_9119
    clc
    adc #$18
    bcc -

_addfq
    lda L_9118
    clc
    adc $f8
    sta $f8
    lda L_9119
    adc $f9
    sta $f9
	
L_95C0
    ldy #VD_FLAGS
    lda (VD),y
    eor #1						; flip bit 1
    sta (VD),y
    and #$24
    beq L_962C

    tax
    ldy #VD_B0F
    lda (VD),y
    sec
    sbc #1
    sta (VD),y
    bne L_962C

    lda #1
    sta (VD),y
    txa
    and #4
    bne L_9608

    ldy #VD_B0E
    lda (VD),y
    pha
    dey
    lda (VD),y					; B0D
    ldy #VD_B07
    clc
    adc (VD),y
    sta (VD),y
    iny
    pla
    adc (VD),y					; B08
    sta (VD),y
    dey
    lda $f8
    clc
    adc (VD),y					; B07
    sta $f8
    lda $f9
    iny							; B08
    adc (VD),y
    sta $f9
    jmp L_962C

L_9608
    ldy #VD_B0E
    lda (VD),y
    pha
    dey
    lda (VD),y
    ldy #VD_B07
    clc
    adc (VD),y
    sta (VD),y
    iny
    pla
    adc (VD),y					; B08
    sta (VD),y
    dey
    lda $f8
    sec
    sbc (VD),y					; B07
    sta $f8
    lda $f9
    iny
    sbc (VD),y					; B08
    sta $f9
	
L_962C
    ldy #VD_B21
    lda (VD),y
    beq L_9674

    bmi L_9654

    ldy #VD_B20
    lda (VD),y
    ldy #VD_PWL
    clc
    adc (VD),y
    sta (VD),y
    iny
    lda (VD),y				; B21
    adc #0
    sta (VD),y
    ldy #VD_B1F
    cmp (VD),y
    bne L_9674

    ldy #VD_B21
    lda #$81
    sta (VD),y
    bne L_9674

L_9654
    ldy #VD_PWL
    lda (VD),y
    ldy #VD_B20
    sec
    sbc (VD),y
    ldy #VD_PWL
    sta (VD),y
    iny
    lda (VD),y				; B21
    sbc #0
    sta (VD),y
    ldy #VD_B1E
    cmp (VD),y
    bne L_9674

    ldy #VD_B21
    lda #1
    sta (VD),y
L_9674
    ldy #VD_WAVE
    lda (VD),y
    iny
    sta (VD),y				; B22
    ldy #0
    lda (VD),y
    and #3
    cmp #3
    bne L_9691

    lda $f9
    adc #$30
    sta $f9
    ldy #VD_CTRL
    lda #$80
    sta (VD),y
L_9691
    ldx $f8
    ldy $f9
    rts

ModeVol
    !by $0f

NoteFreqsL
NoteFreqsH = * + 1
    !wo $0116,$0126,$0138,$014b,$0160,$0172,$0189,$01a1,$01bb,$01d6,$01f1,$020e
	!wo $022c,$024c,$0270,$0296,$02c0,$02e4,$0312,$0342,$0376,$03ac,$03e2,$041c
	!wo $0458,$0498,$04e0,$052c,$0580,$05c8,$0624,$0684,$06ec,$0758,$07c4,$0838
	!wo $08b0,$0930,$09c0,$0a58,$0b00,$0b90,$0c48,$0d08,$0dd8,$0eb0,$0f88,$1070
	!wo $1160,$1260,$1380,$14b0,$1600,$1720,$1890,$1a10,$1bb0,$1d60,$1f10,$20e0
	!wo $22c0,$24c0,$2700,$2960,$2c00,$2e40,$3120,$3420,$3760,$3ac0,$3e20,$41c0
	!wo $4580,$4980,$4e00,$52c0,$5800,$5c80,$6240,$6840,$6ec0,$7580,$7c40,$8380
	!wo $8b00,$9300,$9c00,$a580,$b000,$b900,$c480,$d080,$dd80,$eb00,$f880

ArpTable
	!wo L_976F
	!wo L_9773
	!wo L_9777
	!wo L_977C
	!wo L_9781
	!wo L_9785
	!wo L_9789
	!wo L_978D
	!wo L_9791
	!wo L_9794
	!wo L_9797
	!wo L_979A
	!wo L_979D
	
L_976F	!by $00,$03,$07,$88
L_9773	!by $00,$04,$07,$88
L_9777	!by $00,$03,$07,$0c,$88
L_977C	!by $00,$04,$07,$0c,$88
L_9781	!by $07,$0c,$0f,$88
L_9785	!by $07,$0c,$10,$88
L_9789	!by $03,$07,$0c,$88
L_978D	!by $04,$07,$0c,$88
L_9791	!by $00,$0c,$88
L_9794	!by $00,$04,$88
L_9797	!by $00,$03,$88
L_979A	!by $00,$05,$88
L_979D	!by $00,$07,$88

Track1	!wo Track1Seq
Track2	!wo Track2Seq
Track3	!wo Track3Seq

Track1Seq
	!wo L_9902
	!wo L_9909
	!wo L_9909
	!wo L_9909
	!wo L_9909
	!wo L_9909
	!wo L_9909
	!wo L_9909
	!wo L_9909
	!wo L_9909
	!wo L_9909
	!wo L_9909
	!wo L_9909
	!wo L_9934
	!wo L_9934
	!wo L_9934
	!wo L_9934
	!wo L_9934
	!wo L_996E
	!wo L_9934
	!wo L_996E
	!wo L_9934
	!wo L_996E
	!wo L_9934
	!wo L_996E
	!wo L_9909
	!wo L_9909
	!wo L_9909
	!wo L_9909
	!wo L_9934
	!wo L_996E
	!wo L_9934
	!wo L_996E
	!wo L_9934
	!wo L_996E
	!wo L_9934
	!wo L_996E
	!wo L_9909
	!wo L_9909
	!wo L_9909
	!wo L_9909
	!wo L_9934
	!wo L_996E
	!wo L_9934
	!wo L_996E
	!wo L_9934
	!wo L_996E
	!wo L_9934
	!wo L_996E
	!wo L_9909
	!wo L_9909
	!wo L_9909
	!wo L_9909
	!wo 0

Track2Seq
	!wo L_99A8
	!wo L_99CD
	!wo L_99CD
	!wo L_99CD
	!wo L_99CD
	!wo L_99CD
	!wo L_99CD
	!wo L_99CD
	!wo L_99CD
	!wo L_99CD
	!wo L_99CD
	!wo L_99CD
	!wo L_99CD
	!wo L_99CD
	!wo L_99CD
	!wo L_99CD
	!wo L_99CD
	!wo L_99CD
	!wo L_99EB
	!wo L_99CD
	!wo L_99EB
	!wo L_99CD
	!wo L_99EB
	!wo L_99CD
	!wo L_99EB
	!wo L_9A09
	!wo L_9A27
	!wo L_9A45
	!wo L_9A63
	!wo L_9A09
	!wo L_9A27
	!wo L_9A45
	!wo L_9A63
	!wo L_99CD
	!wo L_99EB
	!wo L_99CD
	!wo L_99EB
	!wo L_99CD
	!wo L_99EB
	!wo L_99CD
	!wo L_99EB
	!wo L_9A09
	!wo L_9A27
	!wo L_9A45
	!wo L_9A63
	!wo L_9A09
	!wo L_9A27
	!wo L_9A45
	!wo L_9A63
	!wo L_99CD
	!wo L_99EB
	!wo L_99CD
	!wo L_99EB
	!wo L_99CD
	!wo L_99EB
	!wo L_99CD
	!wo L_99EB
	!wo L_9A09
	!wo L_9A27
	!wo L_9A45
	!wo L_9A63
	!wo L_9A09
	!wo L_9A27
	!wo L_9A45
	!wo L_9A63
	!wo 0

Track3Seq
	!wo L_9A81
	!wo L_9909
	!wo L_9909
	!wo L_9909
	!wo L_9909
	!wo L_9AAB
	!wo L_9AAB
	!wo L_9AAB
	!wo L_9AAB
	!wo L_9AAB
	!wo L_9AAB
	!wo L_9AAB
	!wo L_9AAB
	!wo L_9AAB
	!wo L_9AAB
	!wo L_9AAB
	!wo L_9AAB
	!wo L_9AC9
	!wo L_9AEB
	!wo L_9AC9
	!wo L_9B0C
	!wo L_9AC9
	!wo L_9AEB
	!wo L_9AC9
	!wo L_9B0C
	!wo L_9B2D
	!wo L_9B4F
	!wo L_9B71
	!wo L_9B93
	!wo L_9B2D
	!wo L_9B4F
	!wo L_9B71
	!wo L_9BB5
	!wo L_9AC9
	!wo L_9AEB
	!wo L_9AC9
	!wo L_9B0C
	!wo L_9AC9
	!wo L_9AEB
	!wo L_9AC9
	!wo L_9B0C
	!wo L_9BD0
	!wo L_9C03
	!wo L_9C36
	!wo L_9C69
	!wo L_9BD0
	!wo L_9C03
	!wo L_9C36
	!wo L_9C9C
	!wo L_9CBD
	!wo L_9CBD
	!wo L_9CD2
	!wo L_9CD2
	!wo 0

PC_NOISE	= $8a
PC_PULSE	= $8b
	
	
L_9902
	!by $be,$d0,$00,$00,$ff,$80,$88
	
L_9909
	!by PC_NOISE,$d0,$40,$00,$81,$e0,$5d,$24,$51,$26,$3e,$39
    !by $8e,$30,$59,$5d,$24,$51,$26,$3e,$39,$8e,$30,$59,$5d,$24,$51,$26
    !by $3e,$39,$8e,$30,$59,$5d,$24,$51,$26,$3e,$39,$8e,$30,$59,$88

L_9934
	!by PC_PULSE
    !by $90,$02,$08,$28,$d0,$0a,$40,$e1,$18,$d0,$40,$00,PC_NOISE,$e0,$51,$54
    !by $e1,$d0,$09,$00,PC_NOISE,$84,$00,$04,$01,$8e,$3c,$d0,$0a,$40,PC_PULSE,$1f
    !by $18,$d0,$40,$00,PC_NOISE,$e0,$53,$56,$e1,$d0,$09,$00,PC_NOISE,$84,$00,$04
    !by $01,$8e,$30,$d0,$0a,$40,PC_PULSE,$13,$88
L_996E
	!by PC_PULSE,$90,$02,$08,$28,$d0,$0a
    !by $40,$e1,$18,$d0,$40,$00,PC_NOISE,$e0,$51,$54,$e1,$d0,$09,$00,PC_NOISE,$84
    !by $00,$04,$01,$8e,$3c,$d0,$0a,$40,PC_PULSE,$1d,$22,$d0,$40,$00,PC_NOISE,$e0
    !by $53,$56,$e1,$d0,$09,$00,PC_NOISE,$84,$00,$04,$01,$8e,$30,$d0,$0a,$40
    !by PC_PULSE,$1d,$88

L_99A8
	!by PC_PULSE,$90,$01,$08,$0a,$89,$01,$02,$82,$ff,$d0,$df,$00
    !by $18,$90,$02,$08,$0a,$d0,$0e,$00,$18,$18,$18,$18,$89,$04,$08,$d0
    !by $0c,$40,$ef,$18,$18,$18,$18
	!by $88
L_99CD
	!by PC_PULSE,$d0,$08,$10,$89,$03,$06,$82
    !by $90,$02,$08,$28,$e0,$18,$18,$18,$24,$18,$18,$24,$18,$18,$18,$18
    !by $24,$18,$18,$24,$24,$88
L_99EB
	!by PC_PULSE,$d0,$08,$10,$89,$03,$06,$82,$90,$02
    !by $08,$28,$e0,$18,$18,$18,$24,$18,$18,$24,$18,$16,$16,$16,$22,$16
    !by $16,$22,$22,$88
L_9A09
	!by PC_PULSE,$d0,$08,$10,$89,$03,$06,$82,$90,$02,$08,$28
    !by $e0,$1b,$1b,$1b,$27,$1b,$1b,$27,$1b,$1b,$1b,$1b,$27,$1b,$1b,$27
    !by $27,$88
L_9A27
	!by PC_PULSE,$d0,$08,$10,$89,$03,$06,$82,$90,$02,$08,$28,$e0,$19
    !by $19,$19,$25,$19,$19,$25,$19,$19,$19,$19,$25,$19,$19,$25,$25,$88
L_9A45
    !by PC_PULSE,$d0,$08,$10,$89,$01,$02,$82,$90,$02,$08,$28,$e0,$12,$12,$12
    !by $1e,$12,$12,$1e,$12,$12,$12,$12,$1e,$12,$12,$1e,$1e,$88
L_9A63
	!by PC_PULSE,$d0
    !by $08,$10,$89,$01,$02,$82,$90,$02,$08
	!by $28,$e0,$13,$13,$13,$1f,$13
    !by $13,$1f,$13,$13,$13,$13,$1f,$13,$13,$1f,$1f,$88
L_9A81
	!by $92,$90,$02,$08
    !by $14,$89,$7f,$fe,$82,$d0,$df,$00,$ff,$84,$80,$00,$01,$51,PC_PULSE,$d0
    !by $0d,$00,$81,$0c,$82,$92,$84,$14,$00,$0c,$4f,$84,$10,$00,$0c,$48
    !by $84,$0a,$00,$0c,$3c,$88
L_9AAB
	!by PC_PULSE,$d0,$08,$10,$89,$05,$0a,$82,$90,$01
    !by $08,$50,$e0,$24,$24,$2b,$2b,$29,$29,$2b,$2b,$24,$24,$2b,$2b,$27
    !by $27,$29,$29,$88
L_9AC9
	!by PC_PULSE,$90,$02,$07,$96,$82,$d0,$0a,$40,$e1,$89,$0a
    !by $14,$24,$89,$14,$28,$30,$38,$37,$89,$0a,$14,$24,$89,$14,$28,$c8
    !by $30,$c8,$38,$c8,$37,$88
L_9AEB
	!by PC_PULSE,$90,$02,$07,$96,$d0,$0a,$40,$e1,$89
    !by $0a,$14,$24,$89,$14,$28,$30,$38,$37,$89,$05,$0a,$22,$89,$0a,$14
    !by $c8,$2e,$c8,$3a,$c8,$3c,$88
L_9B0C
	!by PC_PULSE,$90,$02,$07,$96,$d0,$0a,$40,$e1
    !by $89,$0a,$14,$24,$89,$14,$28,$30,$38,$37,$89,$05,$0a,$22,$89,$0a
    !by $14,$c8,$2e,$c8,$3a,$c8,$37,$88
L_9B2D
	!by PC_PULSE,$90,$04,$08,$96,$82,$d0,$0a
    !by $40,$e1,$89,$0a,$14,$27,$89,$14,$28,$33,$3a,$38,$89,$0a,$14,$27
    !by $89,$14,$28,$c8,$33,$c8,$3a,$c8,$38,$88
L_9B4F
	!by PC_PULSE,$90,$04,$08,$96,$82
    !by $d0,$0a,$40,$e1,$89,$0a,$14,$25,$89,$14,$28,$31,$38,$36,$89,$0a
    !by $14,$25,$89,$14,$28,$c8,$31,$c8,$38,$c8,$36,$88
L_9B71
	!by PC_PULSE,$90,$04,$08
    !by $96,$82,$d0,$0a,$40,$e1,$89,$05,$0a,$1e,$89,$0a,$14,$2a,$31,$2a
    !by $89,$05,$0a,$1e,$89,$0a,$14,$c8,$2a,$c8,$31,$c8,$2a,$88
L_9B93
	!by PC_PULSE,$90
    !by $04,$08,$96,$82,$d0,$0a,$40,$e1,$89,$05,$0a,$1f,$89,$0a,$14,$2b
    !by $32,$2b,$89,$05,$0a,$1f,$89,$0a,$14,$c8,$2b,$c8,$32,$c8,$2b,$88
L_9BB5
    !by PC_PULSE,$90,$04,$08,$96,$82,$d0,$0a,$40,$e1,$89,$05,$0a,$1f,$89,$0a
    !by $14,$2b,$32,$2b,$32,$2b,$c8,$32,$c8,$33,$88
L_9BD0
	!by PC_PULSE,$90,$02,$08,$63
    !by $82,$d0,$08,$20,$e0,$89,$0a,$14,$27,$89,$14,$28,$33,$3a,$38,$89
    !by $0a,$14,$27,$89,$14,$28,$33,$3a,$38,$89,$0a,$14,$27,$89,$14,$28
    !by $33,$3a,$38,$89,$0a,$14,$27,$89,$14,$28,$33,$3a,$38,$88
L_9C03
	!by PC_PULSE,$90
    !by $02,$08,$63,$82,$d0,$08,$20,$e0,$89,$0a,$14,$25,$89,$14,$28,$31
    !by $38,$36,$89,$0a,$14,$25,$89,$14,$28,$31,$38,$36,$89,$0a,$14,$25
    !by $89,$14,$28,$31,$38,$36,$89,$0a,$14,$25,$89,$14,$28,$31,$38,$36
    !by $88
L_9C36
	!by PC_PULSE,$90,$02,$08,$63,$82,$d0,$08,$20,$e0,$89,$05,$0a,$1e,$89
    !by $0a,$14,$2a,$31,$2a,$89,$05,$0a,$1e,$89,$0a,$14,$2a,$31,$2a,$89
    !by $05,$0a,$1e,$89,$0a,$14,$2a,$31,$2a,$89,$05,$0a,$1e,$89,$0a,$14
    !by $2a,$31,$2a,$88
L_9C69
	!by PC_PULSE,$90,$02,$08,$63,$82,$d0,$08,$20,$e0,$89,$05
    !by $0a,$1f,$89,$0a,$14,$2b,$32,$2b,$89,$05,$0a,$1f,$89,$0a,$14,$2b
    !by $32,$2b,$89,$05,$0a,$1f,$89,$0a,$14,$2b,$32,$2b,$89,$05,$0a,$1f
    !by $89,$0a,$14,$2b,$32,$2b,$88
L_9C9C
	!by PC_PULSE,$90,$02,$08,$63,$82,$d0,$08,$20
    !by $e0,$89,$05,$0a,$1f,$1f,$89,$0a,$14,$2b,$2b,$32,$32,$2b,$2b,$32
    !by $32,$2b,$2b,$32,$32,$33,$33,$88
L_9CBD
	!by $8d,$89,$28,$50,$82,$d0,$0d,$70
    !by $f7,$c0,$3c,$e7,$c1,$3a,$f7,$c0,$3c,$e7,$c1,$3a,$88
L_9CD2
	!by $8d,$89,$28
    !by $50,$82,$d0,$0d,$70,$ef,$c1,$3f,$c1,$3d,$c1,$36,$84,$50,$00,$30
    !by $c1,$37,$88,$f0,$05,$fe,$00,$b0,$45,$20,$7f,$30,$0d,$f4,$ff,$fe
    !by $05,$b0,$1f,$30,$4d,$be,$4d,$b0,$45,$b0,$0c

}