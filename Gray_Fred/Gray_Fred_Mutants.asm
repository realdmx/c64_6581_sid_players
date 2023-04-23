
;	Mutants by Fred Gray

;	A work in progress

	!to "Gray_Fred_Mutants.sid",plain

	* = $0000

	!text "PSID"
	!be16 2							; version 2
	!be16 $7c						; data offset
	!be16 0							; Load (0 = auto)
	!be16 init						; Init
	!be16 play						; Play
	!be16 2							; num songs
	!be16 1							; first song
	!word 0
	!word 0
-	!text "Mutants"
	!fill 32 - (* - -)
-	!text "Fred Gray"
	!fill 32 - (* - -)
-	!text "1987 Ocean"
	!fill 32 - (* - -)
	!be16 $0014							; v2 flags
	!be16 0							; Start page, page length (reloc)
	!be16 0							; Reserved
	!word $e000
	
!pseudopc $e000 {

	* = $e000

V1FQL = $d400
V1FQH = $d401
V1PWL = $d402
V1PWH = $d403
V1CTRL = $d404
V1AD = $d405
V1SR = $d406
V2FQL = $d407
V2FRQH = $d408
V2PWL = $d409
V2PWH = $d40a
V2CTRL = $d40b
V2AD = $d40c
V2SR = $d40d
V3FQL = $d40e
V3FQH = $d40f
V3PWL = $d410
V3PWH = $d411
V3CTRL = $d412
V3AD = $d413
V3SR = $d414
FCLO = $d415
FCHI = $d416
RESFILT = $d417
MODEVOL = $d418
POTX = $d419
POTY = $d41a
OSC3 = $d41b
ENV3 = $d41c

    * = $e000

le000    !by $00
le001    !by $81
le002    !by $00
le003    !by $00
le004    !by $00
le005    !by $20,$00,$00,$00
le009    !by $00,$00,$00,$20,$00,$00,$00
le010    !by $00,$00,$00,$20,$00,$00,$00

play
    lda #$0f                         ; e017: !by $a9,$0f
    sta MODEVOL                      ; e019: !by $8d,$18,$d4
	
	; mainloop
	
    ldx #14				; x = voice offset
-	lda lec01,x
    sta V1SR,x
    jsr led24

    jsr lebdc

    jsr leb7f

    jsr lea14

    jsr leba1

    txa
    sec
    sbc #7
    tax
    bpl -

    jmp le03d                        ; e03a: !by $4c,$3d,$e0

le03d
    lda le000                        ; e03d: !by $ad,$00,$e0
    beq le04d                        ; e040: !by $f0,$0b

    sta le001                        ; e042: !by $8d,$01,$e0
    jsr le992                        ; e045: !by $20,$92,$e9

    lda #$00                         ; e048: !by $a9,$00
    sta le000                        ; e04a: !by $8d,$00,$e0
le04d
    lda le001                        ; e04d: !by $ad,$01,$e0
    bmi le06a                        ; e050: !by $30,$18

    sta le0be                        ; e052: !by $8d,$be,$e0
    ora #$80                         ; e055: !by $09,$80
    sta le001                        ; e057: !by $8d,$01,$e0
    lda #$00                         ; e05a: !by $a9,$00
    sta lecb5                        ; e05c: !by $8d,$b5,$ec
    sta lecb6                        ; e05f: !by $8d,$b6,$ec
    lda #$ff                         ; e062: !by $a9,$ff
    sta lecbd                        ; e064: !by $8d,$bd,$ec
    sta lecbe                        ; e067: !by $8d,$be,$ec
le06a
    lda le0be                        ; e06a: !by $ad,$be,$e0
    asl                              ; e06d: !by $0a
    clc                              ; e06e: !by $18
    adc #$7e                         ; e06f: !by $69,$7e
    sta le07c                        ; e071: !by $8d,$7c,$e0
    lda #$00                         ; e074: !by $a9,$00
    adc #$e0                         ; e076: !by $69,$e0
    sta le07d                        ; e078: !by $8d,$7d,$e0
le07c =  * + 1
le07d =  * + 2
    jmp ($e080)                      ; e07b: !by $6c,$80,$e0

	!wo le0bf
	!wo le0df
    !wo le126
	!wo le0df
	!wo le126
	!wo le157
	!wo le19e
	!wo le1d1
	!wo le1f5
	!wo le21a
    !wo le26d
	!wo le21a
    !wo le26d
	!wo le2a2
    !wo le303
    !wo le21a
    !wo le26d
	!wo le21a
    !wo le26d
	!wo le2a2
    !wo le157
	!wo le2e7
    !wo le2fd
	!wo le369
    !wo le36f
    !wo le378
    !wo le378
    !wo le3e2
    !wo le3e2
    !wo le41b
	!wo le41b
	!wo le451
le0be
	!by $01
	
le0bf
    rts

le0c0
    !by $00,$a0,$10,$a0,$10,$80,$80,$50
	!by $80,$20,$20,$20,$20,$60,$00,$20
    !by $20,$20,$20,$60,$18,$38,$16,$17
	!by $18,$00,$c0,$20,$20,$20,$98
	
le0df
	lda #9
	sta $d405
	sta $d40c
	lda #$0b
	sta $d413
	ldy #$10
	lda #$10
	ldx #8
	jsr le9fd
	lda #8
	jsr leb0a
	beq le0fd
	rts

le0fd
	lda le51f,y
	ldx #0
	jsr le9d9
	tya
	and #$0f
	tay
	lda le5c0,y
	ldx #$0c
	jsr le9c7
	lda le5e0,y
	ldx #$c
	jsr le9d0
	lda lecb6
	beq le123
	ldy #4
	jsr lead6
le123
	jmp leb20

le126
	lda #9
	sta $d413
	ldy #8
	lda #8
	ldx #$18
	jsr le9fd
	lda #8
	jsr leb0a
	beq le13c
	rts
	
le13c
	lda le5f0,y
	ldx #0
	jsr le9d0
	lda le5f0,y
	ldx #$18
	jsr le9d9
	lda le601,y
	ldx #$18
	jsr le9b1
	jmp leb20
	
le157
	lda #9
	sta $D405	; V1 AD
	sta $D413	; V3 AD
	lda #11
	sta $D40C	; V2 AD
	ldx #7
	jsr leb7f
	ldy #$20
	lda #8
	ldx #0
	jsr le9fd
	lda #8
	jsr leb0a
	beq le17a
	rts 
	
le17a
	tya 
	and #$3f
	tay 
	lda le651,y
	ldx #$f8
	jsr le9d9
	lda le611,y
	ldx #$18
	jsr le9b1
	lda le691,y
	ldx #12
	jsr le9d0
	ldy #4
	jsr lead6
	jmp leb20
	
le19e
	ldy #$20
	lda #8
	ldx #0
	jsr le9fd
	lda #8
	jsr leb0a
	beq le1af
	rts 
le1af
	tya 
	and #$3f
	tay 
	lda le731,y
	jsr le9d3
	tya 
	and #$1f
	tay 
	lda le6d1,y
	jsr le9b4
	lda le6f1,y
	jsr le9dc
	ldy #4
	jsr lead6
	jmp leb20
le1d1
	lda #13
	sta $d40c	; v2 ad
	lda #8
	jsr leb0a
	beq le1de
	rts 
le1de
	lda le771,y
	jsr le9b4
	lda le811,y
	jsr le9d3
	lda le7c1,y
	ldx #0
	jsr le9d9
	jmp leb20
le1f5
	lda #8
	jsr leb0a
	beq le1fd
	rts 
le1fd
	tya 
	and #$3f
	tay 
	lda le861,y
	jsr le9b4
	lda le8b2,y
	jsr le9d3
	tya 
	and #15
	tay 
	lda le8a2,y
	jsr le9dc
	jmp leb20
le21a
	lda #9
	sta $d40c
	sta $d405
	lda #11
	sta $d413
	ldy #$10
	lda #$10
	ldx #$28
	jsr le9fd
	lda #8
	jsr leb0a
	beq le238
	rts 
le238
	lda le0be
	cmp #$11
	bne le244
	lda #0
	sta lecbc
le244
	lda le8f2,y
	ldx #12
	jsr le9d9
	tya 
	and #15
	tay 
	lda le5c0,y
	ldx #12
	jsr le9c7
	lda le5e0,y
	ldx #12
	jsr le9d0
	lda lecb6
	beq le26a
	ldy #4
	jsr lead6
le26a
	jmp leb20
le26d
	ldy #$10
	lda #$10
	ldx #$28
	jsr le9fd
	lda #8
	jsr leb0a
	beq le27e
	rts 
le27e
	lda le8f2,y
	ldx #10
	jsr le9d9
	tya 
	and #15
	tay 
	lda le5c0,y
	ldx #10
	jsr le9c7
	lda le5e0,y
	ldx #10
	jsr le9d0
	ldy #4
	jsr lead6
	jmp leb20
le2a2
	lda #11
	sta $d40c	; v2 ad
	ldy #$10
	lda #$20
	ldx #$28
	jsr le9fd
	lda #8
	jsr leb0a
	beq le2b8
	rts 
le2b8
	lda le932,y
	ldx #0
	jsr le9d0
	lda lecb6
	cmp #$5a
	bcs le2d3
	tya 
	and #$1f
	tay 
	lda le8f2,y
	ldx #12
	jsr le9d9
le2d3
	tya 
	and #15
	tay 
	lda le5e0,y
	ldx #12
	jsr le9c7
	ldy #4
	jsr lead6
	jmp leb20
le2e7
	lda #$40
	sta $d404	; v1 ctl
	sta $d40b	; v2 ctl
	sta $d412	; v3 ctl
	lda #2
	jsr leb0a
	beq le2fa
	rts 
le2fa
	jmp leb20
le2fd
	lda #1
	sta le001
	rts 
le303
	lda #9
	sta $d40c	; v2 ad
	ldy #$10
	lda #8
	ldx #8
	jsr le9fd
	lda #8
	jsr leb0a
	beq le319
	rts 
le319
	tya 
	lsr 
	lsr 
	lsr 
	lsr 
	lsr 
	tax 
	lda le361,x
	sta lecbc
	tya 
	and #15
	tay 
	lda le5d0,y
	ldx #0
	jsr le9d0
	lda lecb6
	bpl le342
	lda le8f2,y
	ldx #12
	jsr le9d9
	jmp le34c
le342
	tya 
	bne le34c
	lda #8
	ldx #12
	jsr le9d9
le34c
	tya 
	and #15
	tay 
	lda le5e0,y
	ldx #12
	jsr le9c7
	ldy #4
	jsr lead6
	jmp leb20
	!by	$14
le361
	!by	$00, $00, $03, $05, $00, $fd, $03, $05
le369
	lda #1
	sta le001
	rts
	
le36f
	jsr le992	; resetaudio
	lda #0
	sta le001
	rts
	
	
le378
	lda #$60
	sta $d40c	; v2 ad
	lda #15
	sta $d405	; v1 ad
	lda #11
	sta $d413	; v3 ad
	ldy #8
	lda #$28
	ldx #$28
	jsr le9fd
	lda #10
	jsr leb0a
	beq le398
	rts 
	
	
le398
	tya 
	lsr 
	bcs le3b5
	tay 
	ldx #$18
	lda lecb6
	cmp #$fe
	beq le3ad
	lda le0be
	cmp #$1a
	bne le3af
le3ad
	ldx #$24
le3af
	lda le49f,y
	jsr le9b1
le3b5
	ldy lecb6
	tya 
	and #7
	tay 
	lda le497,y
	ldx #0
	jsr le9d9
	lda lecb6
	bpl le3cd
	tya 
	ora #8
	tay 
le3cd
	lda le457,y
	ldx #$18
	jsr le9d0
	tya 
	and #3
	bne le3df
	lda #$81	; noise on
	sta $d412	; v3 ctl
le3df
	jmp leb20
le3e2
	lda #9
	sta $d405
	sta $d40c
	lda #10
	jsr leb0a
	beq le3f2
	rts 
le3f2
	lda le467,y
	ldx #$24
	jsr le9b1
	tya 
	and #7
	tay 
	lda le497,y
	ldx #12
	jsr le9d0
	lda le497,y
	ldx #0
	jsr le9d9
	tya 
	and #3
	bne le418
	lda #$81	; noise on
	sta $d412	; v3 ctl
le418
	jmp leb20
le41b
	lda #15
	sta $d405
	lda #10
	jsr leb0a
	beq le428
	rts 
le428
	lda le487,y
	ldx #$18
	jsr le9b1
	tya 
	and #7
	tay 
	lda le497,y
	ldx #12
	jsr le9d0
	lda le497,y
	ldx #0
	jsr le9d9
	tya 
	and #3
	bne le44e
	lda #$81
	sta $d412
le44e
	jmp leb20

le451	
	lda #$19
	sta le001
	rts 

le457
	!by $0f,$15,$16,$15,$0f,$15,$16,$15,$0f,$15,$16,$19,$16,$15,$0f,$15
le467
	!by $4a,$00,$00,$00,$26,$28,$00,$00,$00,$4a,$4a,$4a,$26,$28,$00,$00
	!by $4a,$00,$00,$00,$26,$28,$00,$00,$00,$2d,$00,$2d,$6c,$4a,$00,$00
le487
	!by $00,$05,$06,$05,$06,$05,$09,$55,$06,$05,$59,$05,$00,$00,$00,$05
le497
	!by $03,$03,$0a,$03,$0f,$03,$09,$0a
le49f
	!by $0f,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0a
	!by $59,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01
	!by $03,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$43
	!by $25,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$25
	!by $26,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$26
	!by $28,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$28
	!by $2b,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$2b
	!by $2d,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0d
le51f
	!by $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	!by $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0f,$00
	!by $94,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	!by $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0f,$00
	!by $92,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	!by $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0f,$00
	!by $8d,$00,$00,$00,$00,$00,$00,$00,$0f,$00,$00,$00,$0f,$00,$00,$00
	!by $08,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	!by $20,$00,$00,$9e,$00,$00,$00,$00,$a2,$00,$00,$9e,$00,$00,$00,$00
	!by $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0d
	!by $00
	
le5c0
    !by $23,$00,$0f,$16,$0f,$17,$0f,$19,$0f,$16,$17,$0f,$14,$16,$23,$00
le5d0
    !by $0f,$14,$0f,$16,$0f,$17,$0f,$19,$0f,$16,$17,$0f,$14,$16,$0f,$12
le5e0
    !by $14,$17,$1b,$14,$17,$1e,$14,$17,$1b,$14,$17,$1e,$14,$17,$1b,$14
le5f0
    !by $03,$03,$00,$03,$00,$04,$06,$00,$00,$00,$00,$00,$00,$00,$00,$00
    !by $00
le601
	!by $43,$43,$00,$43,$00,$24,$26,$00,$00,$00,$00,$00,$00,$00,$00,$00
le611
	!by $48,$48,$00,$48,$00,$48,$48,$00,$00,$48,$48,$00,$48,$48,$48,$00
	!by $43,$43,$00,$43,$00,$24,$26,$00,$00,$26,$26,$26,$26,$26,$26,$00
	!by $26,$26,$00,$26,$00,$26,$26,$00,$00,$26,$26,$00,$26,$26,$26,$00
	!by $24,$24,$00,$24,$00,$26,$24,$00,$00,$00,$00,$00,$00,$00,$00,$00
	
le651
	!by $0f,$16,$00,$15,$00,$0f,$14,$00,$00,$12,$0d,$0e,$0f,$12,$0f
    !by $0f,$0a,$11,$00,$10,$00,$0a,$0f,$00,$00,$00,$00,$00,$00,$00,$00
    !by $0d,$0a,$11,$00,$10,$00,$0a,$0f,$00,$00,$0d,$0a,$0d,$08,$09,$0a
    !by $0d,$08,$09,$0a,$0d,$0a,$0d,$08,$00,$00,$00,$00,$00,$00,$00,$00
    !by $00

le691
	!by $14,$14,$00,$14,$00,$14,$14,$00
	!by $00,$14,$14,$14,$14,$14,$12,$00
	!by $0f,$0f,$00,$0f,$00,$10,$12,$00
	!by $00,$00,$00,$00,$00,$00,$00,$12
	!by $12,$12,$00,$12,$00,$12,$12,$00
	!by $00,$12,$12,$12,$12,$12,$10,$00
	!by $0d,$0d,$00,$0d,$00,$0f,$0d,$00
	!by $00,$00,$00,$00,$00,$00,$00,$0f
le6d1
	!by $2b,$2b,$00,$2b,$00,$1b,$4f,$00,$00,$4f,$4f,$4f,$4f,$4f,$4f,$4f
	!by $1d,$2d,$00,$2d,$00,$3d,$2d,$00,$00,$2d,$2d,$2d,$2d,$2d,$2c,$00

le6f1
	!by $0f,$0f,$00,$0f,$00,$11,$12,$00,$00,$00,$0f,$00,$11,$11,$12,$12
	!by $16,$14,$00,$14,$00,$12,$14,$00,$00,$00,$14,$00,$14,$14,$13,$00
	!by $0b,$00,$00,$0b,$00,$0d,$0f,$00,$00,$0f,$0f,$00,$00,$00,$00,$0f
	!by $0d,$00,$00,$0d,$00,$0b,$0d,$00,$00,$00,$00,$00,$0d,$00,$0c,$00
	
le731
	!by $1b,$00,$17,$14,$00,$14,$1e,$00,$1b,$00,$17,$14,$00,$00,$1b,$12
	!by $13,$14,$17,$19,$14,$1a,$1b,$1e,$14,$00,$00,$00,$00,$00,$00,$0f
	!by $1b,$00,$17,$14,$00,$14,$1e,$00,$1b,$00,$17,$14,$00,$00,$1b,$12
	!by $13,$14,$0d,$0e,$0f,$08,$0b,$0d,$08,$0e,$0f,$08,$0b,$00,$0d,$00
le771
	!by $4a,$4a,$00,$4a,$00,$4a,$4a,$00,$00,$4a,$4a,$00,$4a,$4a,$4a,$4a
	!by $26,$26,$00,$26,$00,$26,$26,$00,$00,$26,$26,$00,$26,$26,$26,$26
	!by $28,$28,$00,$28,$00,$28,$28,$00,$00,$28,$28,$00,$28,$28,$28,$28
	!by $45,$45,$00,$45,$00,$45,$45,$00,$00,$45,$45,$00,$45,$45,$45,$45
	!by $26,$26,$00,$26,$00,$26,$26,$00,$00,$26,$26,$00,$26,$26,$28,$00

le7c1	
	!by $0a,$0a,$00,$0a,$00,$0a,$0a,$00,$00,$0a,$0a,$00,$0a,$0a,$08,$00
	!by $06,$06,$00,$06,$00,$06,$06,$00,$00,$06,$06,$00,$06,$05,$06,$07
	!by $08,$08,$00,$08,$00,$08,$08,$00,$00,$08,$08,$00,$08,$08,$06,$00
	!by $00,$05,$00,$05,$00,$05,$05,$00,$00,$05,$05,$00,$05,$05,$05,$05
	!by $06,$06,$00,$06,$00,$06,$06,$00,$00,$06,$06,$00,$06,$06,$08,$00
	
le811
	!by $0a,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0a,$00,$0b,$00
	!by $0d,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0b,$00,$0c,$00
	!by $0f,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0b,$0c,$0f,$00
	!by $11,$00,$00,$00,$14,$00,$16,$14,$00,$00,$00,$00,$00,$00,$00,$00
	!by $00,$00,$00,$00,$0f,$11,$14,$16,$14,$00,$00,$00,$14,$16,$19,$1b
	
le861
	!by $21,$21,$00,$21,$00,$21,$21,$00,$00,$21,$21,$00,$21,$21,$21,$21
	!by $26,$26,$00,$26,$00,$26,$26,$00,$00,$26,$26,$00,$26,$26,$26,$26
	!by $2b,$2b,$00,$2b,$00,$2b,$2b,$00,$00,$2b,$2b,$00,$2b,$2b,$2b,$2b
	!by $2b,$2d,$00,$2d,$00,$2d,$2d,$00,$2b,$2d,$2d,$00,$2b,$2d,$2d,$00
	!by $00
	
le8a2
	!by $0d,$0d,$00,$0d,$00,$0d,$0d,$00,$00,$0d,$0d,$0d,$0d,$08,$0a,$12
le8b2
	!by $19,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$19,$1d,$20,$00
	!by $23,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$19,$1d,$20,$00
	!by $23,$00,$00,$00,$22,$00,$00,$00,$20,$00,$00,$00,$1e,$00,$00,$00
	!by $20,$00,$1b,$1d,$00,$00,$00,$00,$00,$00,$00,$00,$14,$16,$19,$1b
	
le8f2
	!by $16,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$16,$17,$19,$1b
	!by $16,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$16,$00,$1b,$00
	!by $14,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$14,$15,$17,$19
	!by $14,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$14,$00,$19,$00
	
le932
	!by $08,$94,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    !by $0f,$00,$08,$94,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    !by $0f,$00,$06,$92,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$12,$00
    !by $0f,$00,$06,$92,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    !by $0f,$00,$04,$90,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$10,$00
    !by $0f,$00,$01,$8d,$00,$00,$00,$00,$00,$00,$0f,$00,$00,$00,$12,$00
    !by $00,$00

le992
    lda #1	                         ; e992: !by $a9,$01
    sta le002                        ; e994: !by $8d,$02,$e0
    sta le009                        ; e997: !by $8d,$09,$e0
    sta le010                        ; e99a: !by $8d,$10,$e0
    ldx #$00                         ; e99d: !by $a2,$00
    txa                              ; e99f: !by $8a
    sta lecbc                        ; e9a0: !by $8d,$bc,$ec
-   sta V1FQL,x                      ; e9a3: !by $9d,$00,$d4
    inx                              ; e9a6: !by $e8
    cpx #$18                         ; e9a7: !by $e0,$18
    bne -

    lda #$0f                         ; e9ab: !by $a9,$0f
    sta MODEVOL                      ; e9ad: !by $8d,$18,$d4
    rts                              ; e9b0: !by $60

le9b1
	stx lec16
le9b4
	tax 
	beq le9c4
	and #$f0
	sta lebff
	lda #0
	sta lebfe
	txa 
	and #15
le9c4
	jmp le9ca
le9c7
	stx lec16
le9ca
	ldx #0
	jsr leb43
	rts 
le9d0
	stx lec1d
le9d3
	ldx #7
	jsr leb43
	rts 
le9d9
	stx lec24
le9dc
	ldx #14
	jsr leb43
	rts
	
le9e2
	lda lecb5
	cmp lecc3
	bne +

	lda lecb6
le9ed
-   lda lec25,y
    sta lebfb,x
    inx
    iny
    tya
    and #7
    cmp #7
    bne -
+   rts

le9fd
	sta lecbf
	stx lecc0
	ldx #0
	jsr le9e2
	ldy lecbf
	jsr le9e2
	ldy lecc0
	jmp le9e2

lea14
    inc lec10,x
    lda lec10,x
    cmp lebfe,x
    bcs lea24

    lda #$00
    jmp lea2e

lea24
    and #$0f
    clc
    adc lebff,x
    tay
    lda lea36,y
lea2e
    clc
    adc lec14,x
    sta lec15,x
    rts

lea36
    !by $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    !by $00,$1c,$14,$00,$1c,$14,$00,$1c,$14,$00,$1c,$14,$00,$1c,$14,$00
    !by $00,$1c,$10,$00,$1c,$10,$00,$1c,$10,$00,$1c,$10,$00,$1c,$10,$00
    !by $00,$1c,$08,$00,$1c,$08,$00,$1c,$08,$00,$1c,$08,$00,$1c,$08,$00
    !by $00,$1c,$0c,$00,$1c,$0c,$00,$1c,$0c,$00,$1c,$0c,$00,$1c,$0c,$00
    !by $01,$01,$01,$01,$ff,$fe,$fe,$ff,$01,$02,$02,$01,$ff,$ff,$ff,$ff
    !by $00,$0c,$18,$00,$0c,$18,$00,$0c,$18,$00,$0c,$18,$00,$0c,$18,$00
    !by $00,$0c,$30,$0c,$00,$0c,$30,$0c,$00,$0c,$30,$0c,$00,$0c,$30,$0c
    !by $30,$00,$1c,$00,$00,$01,$02,$01,$00,$ff,$fe,$ff,$00,$01,$00,$ff
    !by $30,$1c,$00,$01,$00,$ff,$00,$02,$00,$fe,$00,$01,$00,$ff,$00,$1c

lead6
	lda lecb6
	and #15
	tax 
	tya 
	and leaeb,x
	beq +
	lda #$80
	sta $d404
	inc $d404
+	rts 

leaeb
	!by $07,$00,$00,$00,$03,$00,$01,$01
	!by $03,$00,$00,$00,$03,$00,$05,$03
	
	lda $D41C
	clc 
	adc #$6F
	ldx #12
	stx $D413
	sta $D401
	rts 

leb0a
	tax
	dex
	stx lecc3
	ldy lecb6
	inc lecb5
	cmp lecb5
	bne +
	lda #0
	sta lecb5
+	rts

leb20
	ldx #$ff
	ldy le0be
	lda le0c0,y
	inc lecb6
	cmp lecb6
	bne leb35
	lda #0
	sta lecb6
leb35
	bne leb42
	txa
	bpl leb3f
	ldx le0be
	inx
	txa
leb3f
	sta le001
leb42
	rts

leb43
    sta lecbf                        ; eb43: !by $8d,$bf,$ec
    and #$ff                         ; eb46: !by $29,$ff
    bne leb4b                        ; eb48: !by $d0,$01

    rts                              ; eb4a: !by $60

leb4b
    clc                              ; eb4b: !by $18
    adc lec16,x                      ; eb4c: !by $7d,$16,$ec
    adc lecbc                        ; eb4f: !by $6d,$bc,$ec
    asl                              ; eb52: !by $0a
    asl                              ; eb53: !by $0a
    sta lec14,x                      ; eb54: !by $9d,$14,$ec
    sta lec15,x                      ; eb57: !by $9d,$15,$ec
    sta lec12,x                      ; eb5a: !by $9d,$12,$ec
    lda #0
    sta lec10,x                      ; eb5f: !by $9d,$10,$ec
    lda lebfc,x                      ; eb62: !by $bd,$fc,$eb
    beq leb6a                        ; eb65: !by $f0,$03

    sta lec13,x                      ; eb67: !by $9d,$13,$ec
leb6a
    lda lebfb,x                      ; eb6a: !by $bd,$fb,$eb
    sta V1CTRL,x                     ; eb6d: !by $9d,$04,$d4
    inc V1CTRL,x                     ; eb70: !by $fe,$04,$d4
    lda lecbf                        ; eb73: !by $ad,$bf,$ec
    bmi +                        ; eb76: !by $30,$06

    lda lec12,x                      ; eb78: !by $bd,$12,$ec
    sta lec11,x                      ; eb7b: !by $9d,$11,$ec
+	rts                              ; eb7e: !by $60

leb7f
    lda lec00,x                      ; eb7f: !by $bd,$00,$ec
    beq leba0                        ; eb82: !by $f0,$1c

    lda lec11,x                      ; eb84: !by $bd,$11,$ec
    cmp lec12,x                      ; eb87: !by $dd,$12,$ec
    beq leba0                        ; eb8a: !by $f0,$14

    bcs leb97                        ; eb8c: !by $b0,$09

    adc lec00,x                      ; eb8e: !by $7d,$00,$ec
    sta lec11,x                      ; eb91: !by $9d,$11,$ec
    jmp leb9a                        ; eb94: !by $4c,$9a,$eb

leb97
    sbc lec00,x                      ; eb97: !by $fd,$00,$ec
leb9a
    sta lec11,x                      ; eb9a: !by $9d,$11,$ec
    sta lec14,x                      ; eb9d: !by $9d,$14,$ec
leba0
    rts                              ; eba0: !by $60

leba1
    lda lec15,x                      ; eba1: !by $bd,$15,$ec
    ldy #$06                         ; eba4: !by $a0,$06
    sec                              ; eba6: !by $38
leba7
    dey                              ; eba7: !by $88
    sbc #$30                         ; eba8: !by $e9,$30
    bcs leba7                        ; ebaa: !by $b0,$fb

    clc                              ; ebac: !by $18
    adc #$30                         ; ebad: !by $69,$30
    sty lecbf                        ; ebaf: !by $8c,$bf,$ec
    asl                              ; ebb2: !by $0a
    tay                              ; ebb3: !by $a8
    lda lecc4,y                      ; ebb4: !by $b9,$c4,$ec
    sta lecb7                        ; ebb7: !by $8d,$b7,$ec
    iny                              ; ebba: !by $c8
    lda lecc4,y                      ; ebbb: !by $b9,$c4,$ec
    sta lecb8                        ; ebbe: !by $8d,$b8,$ec
    ldy lecbf                        ; ebc1: !by $ac,$bf,$ec
    beq lebcf                        ; ebc4: !by $f0,$09

lebc6
    lsr lecb8                        ; ebc6: !by $4e,$b8,$ec
    ror lecb7                        ; ebc9: !by $6e,$b7,$ec
    dey                              ; ebcc: !by $88
    bne lebc6                        ; ebcd: !by $d0,$f7

lebcf
    lda lecb7                        ; ebcf: !by $ad,$b7,$ec
    sta V1FQL,x                      ; ebd2: !by $9d,$00,$d4
    lda lecb8                        ; ebd5: !by $ad,$b8,$ec
    sta V1FQH,x                      ; ebd8: !by $9d,$01,$d4
    rts                              ; ebdb: !by $60

lebdc
    lda lec13,x                      ; ebdc: !by $bd,$13,$ec
    clc                              ; ebdf: !by $18
    adc lebfd,x                      ; ebe0: !by $7d,$fd,$eb
    sta lec13,x                      ; ebe3: !by $9d,$13,$ec
    asl                              ; ebe6: !by $0a
    asl                              ; ebe7: !by $0a
    asl                              ; ebe8: !by $0a
    ora lecbb                        ; ebe9: !by $0d,$bb,$ec
    sta V1PWL,x                      ; ebec: !by $9d,$02,$d4
    lda lec13,x                      ; ebef: !by $bd,$13,$ec
    lsr                              ; ebf2: !by $4a
    lsr                              ; ebf3: !by $4a
    lsr                              ; ebf4: !by $4a
    lsr                              ; ebf5: !by $4a
    lsr                              ; ebf6: !by $4a
    sta V1PWH,x                      ; ebf7: !by $9d,$03,$d4
    rts                              ; ebfa: !by $60

lebfb
    !by $10
lebfc
    !by $01

lebfd
    !by $d0

lebfe
    !by $00

lebff
    !by $80

lec00
    !by $02

lec01
    !by $00,$10,$01,$d0,$00,$80,$02
	!by $00,$40,$88,$0a,$0c,$50,$02
	!by $4c

lec10
    !by $04

lec11
    !by $bc

lec12
    !by $bc

lec13
    !by $41

lec14
    !by $bc

lec15
    !by $bc

lec16
    !by $0c,$04,$9c,$9c,$41,$9c,$9c
lec1d
	!by $0c,$7b,$04,$04,$87,$04,$05
	
lec24
	!by $00

lec25
    !by $40,$80,$02,$08,$50,$04,$2c,$00
	!by $40,$88,$0a,$0c,$50,$02,$4c,$00
    !by $10,$01,$d0,$00,$80,$02,$00,$00
	!by $80,$80,$02,$00,$20,$04,$00,$00
    !by $40,$01,$08,$00,$80,$00,$8c,$00
	!by $40,$01,$fe,$00,$90,$07,$40,$00
    !by $00,$00,$00,$00,$00,$00,$00,$00
	!by $10,$01,$d0,$00,$10,$16,$00,$00
    !by $80,$01,$f8,$00,$80,$2d,$00,$00
	!by $40,$01,$ff,$00,$30,$16,$00,$00
    !by $40,$80,$37,$00,$20,$07,$00,$00
	!by $14,$01,$81,$08,$50,$2d,$00,$00
    !by $00,$01,$03,$00,$00,$02,$00,$00
	!by $12,$01,$10,$00,$20,$02,$00,$00
    !by $40,$80,$08,$08,$50,$1d,$00,$00
	!by $10,$01,$fe,$00,$40,$0b,$00,$00
    !by $10,$01,$fe,$08,$48,$21,$00,$00
	!by $10,$01,$01,$00,$58,$2c,$00,$00

lecb5
    !by $04

lecb6
    !by $0f

lecb7
    !by $be

lecb8
    !by $3b,$00,$00

lecbb
    !by $3f

lecbc
    !by $00

lecbd
    !by $ff

lecbe
    !by $ff

lecbf
    !by $10
lecc0
	!by $08,$00,$00
	
lecc3
	!by $07

lecc4
    !by $97,$7e,$6e,$80,$4d,$82,$32,$84,$1e,$86,$11,$88,$0c,$8a,$0e,$8c
    !by $18,$8e,$29,$90,$41,$92,$62,$94,$8b,$96,$bb,$98,$f4,$9a,$35,$9d
    !by $7e,$9f,$d0,$a1,$2b,$a4,$8e,$a6,$fa,$a8,$6f,$ab,$ee,$ad,$75,$b0
    !by $06,$b3,$a1,$b5,$45,$b8,$f3,$ba,$ac,$bd,$6e,$c0,$3a,$c3,$11,$c6
    !by $f3,$c8,$df,$cb,$d6,$ce,$d8,$d1,$e6,$d4,$fe,$d7,$23,$db,$52,$de
    !by $8f,$e1,$d6,$e4,$2b,$e8,$8b,$eb,$f8,$ee,$72,$f2,$f9,$f5,$8c,$f9

led24
    lda le004,x                      ; ed24: !by $bd,$04,$e0
    beq led3e                        ; ed27: !by $f0,$15

    inc le005,x                      ; ed29: !by $fe,$05,$e0
    lda le005,x                      ; ed2c: !by $bd,$05,$e0
    and #$03                         ; ed2f: !by $29,$03
    bne led3e                        ; ed31: !by $d0,$0b

    dec le004,x                      ; ed33: !by $de,$04,$e0
    bne led3e                        ; ed36: !by $d0,$06

    lda le003,x                      ; ed38: !by $bd,$03,$e0
    sta le002,x                      ; ed3b: !by $9d,$02,$e0
led3e
    lda le002,x                      ; ed3e: !by $bd,$02,$e0
    beq led46                        ; ed41: !by $f0,$03

    jsr led47                        ; ed43: !by $20,$47,$ed

led46
    rts                              ; ed46: !by $60

led47
    asl                              ; ed47: !by $0a
    asl                              ; ed48: !by $0a
    asl                              ; ed49: !by $0a
    tay                              ; ed4a: !by $a8
    lda #$00                         ; ed4b: !by $a9,$00
    sta lec16,x                      ; ed4d: !by $9d,$16,$ec
    sta le002,x                      ; ed50: !by $9d,$02,$e0
    sty lecbf                        ; ed53: !by $8c,$bf,$ec
    lda led89,y                      ; ed56: !by $b9,$89,$ed
    tay                              ; ed59: !by $a8
    jsr le9ed                        ; ed5a: !by $20,$ed,$e9

    txa                              ; ed5d: !by $8a
    sec                              ; ed5e: !by $38
    sbc #$07                         ; ed5f: !by $e9,$07
    tax                              ; ed61: !by $aa
    ldy lecbf                        ; ed62: !by $ac,$bf,$ec
    iny                              ; ed65: !by $c8
    lda led89,y                      ; ed66: !by $b9,$89,$ed
    sta lec11,x                      ; ed69: !by $9d,$11,$ec
    iny                              ; ed6c: !by $c8
    lda led89,y                      ; ed6d: !by $b9,$89,$ed
    jsr leb43                        ; ed70: !by $20,$43,$eb

    iny                              ; ed73: !by $c8
    lda led89,y                      ; ed74: !by $b9,$89,$ed
    sta V1AD,x                       ; ed77: !by $9d,$05,$d4
    iny                              ; ed7a: !by $c8
    lda led89,y                      ; ed7b: !by $b9,$89,$ed
    sta le004,x                      ; ed7e: !by $9d,$04,$e0
    iny                              ; ed81: !by $c8
    lda led89,y                      ; ed82: !by $b9,$89,$ed
    sta le003,x                      ; ed85: !by $9d,$03,$e0
    rts                              ; ed88: !by $60

led89
    !by $00,$00,$80,$00,$00,$00,$00,$00,$30,$00,$81,$63,$01,$00,$00,$00
    !by $10,$04,$b7,$09,$06,$02,$00,$00,$18,$04,$b7,$6b,$a6,$01,$00,$00
    !by $38,$f4,$85,$09,$42,$01,$00,$00,$38,$04,$b7,$0b,$42,$01,$00,$00
    !by $40,$f4,$85,$0b,$42,$01,$00,$00,$48,$64,$8f,$0c,$3e,$01,$00,$00
    !by $50,$dc,$8f,$0b,$c8,$01,$00,$00,$50,$10,$8f,$09,$c8,$01,$00,$00
    !by $58,$10,$98,$0d,$c8,$01,$00,$00,$60,$10,$b6,$0b,$c8,$01,$00,$00
    !by $60,$d8,$84,$0b,$c8,$01,$00,$00,$58,$b0,$81,$0b,$c8,$01,$00,$00
    !by $10,$b0,$81,$0b,$06,$01,$00,$00,$10,$10,$81,$0b,$06,$0f,$00,$00
    !by $18,$04,$81,$0d,$c4,$01,$00,$00,$68,$48,$bd,$0c,$de,$01,$00,$00
    !by $68,$70,$a0,$0c,$de,$01,$00,$00,$68,$e8,$8b,$0c,$de,$01,$00,$00
    !by $60,$10,$84,$09,$ff,$01,$00,$00,$38,$d8,$84,$68,$19,$01,$00,$00
    !by $70,$10,$ac,$0b,$ff,$01,$00,$00,$58,$88,$bd,$0b,$ff,$01,$00,$00
    !by $18,$f4,$a9,$0b,$ff,$01,$00,$00,$10,$38,$bf,$09,$19,$01,$00,$00
    !by $38,$38,$bf,$89,$19,$01,$00,$00,$40,$38,$bf,$89,$19,$01,$00,$00
    !by $78,$d8,$83,$09,$19,$01,$00,$00,$10,$38,$8d,$69,$19,$01,$00,$00
    !by $80,$10,$ba,$08,$19,$01,$00,$00,$88,$d8,$83,$08,$19,$01,$00,$00
    !by $10,$38,$00,$ff,$00,$ff,$00,$ff,$00,$ff,$00,$ff,$00,$ff,$00,$ff
    !by $00,$ff,$00,$ff,$00,$ff,$00

init
    tax                              ; eea0: !by $aa
    lda leea8,x                      ; eea1: !by $bd,$a8,$ee
    sta le000                        ; eea4: !by $8d,$00,$e0
    rts                              ; eea7: !by $60

leea8
    !by $01,$19,$04,$00,$28,$31,$30,$30,$25,$20,$52,$49,$50,$29

}
