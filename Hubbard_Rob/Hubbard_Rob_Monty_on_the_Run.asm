
	!to "Hubbard_Rob_Monty_on_the_Run.sid",plain

	* = $0000

	!text "PSID"
	!byte $00,$02		; version 2
	!byte $00,$7c		; data offset
	!byte $00,$00		; Load (auto)
	!byte >init,<init	; Init
	!byte >play,<play	; Play
	!byte $00,$01		; num. songs
	!byte $00,$01		; first song
	!word 0
	!word 0
_t	!text "Monty on the Run"
	!fill 32 - (* - _t)
_a	!text "Rob Hubbard"
	!fill 32 - (* - _a)
_c	!text "1985 Gremlin Graphics"
	!fill 32 - (* - _c)
	!word $0000			; v2 flags
	!word $0000			; Start page, page length (reloc)
	!word $0000			; Reserved
	
	; auto load address
	!word $8000

!pseudopc $8000 {

;	-------------------------------------------------------
;	Rob Hubbard Music Driver
;	-------------------------------------------------------
;	Original disassembly and commenting by Anthony McSweeney
;	Port to ACME by dmx87
;	-------------------------------------------------------
;	This player was used (with small modifications) for his
;	first approx. 30 tunes
;	-------------------------------------------------------

init	
	jmp initmusic
play
	jmp playmusic

;	-------------------------------------------------------
;	Init
;	-------------------------------------------------------

initmusic
	lda #0				; song number (this only got one)
	ldy #0
	asl
	sta tempstore
	asl
	clc
	adc tempstore		; a = song num * 6
	tax

_1	lda songs,x
	sta currtrkhi,y		; set track pointers
	inx
	iny
	cpy #$06
	bne _1

	lda #$00			; SID reset	
	sta $d404
	sta $d40b
	sta $d412
	sta $d417

	lda #$0f			; max volume
	sta $d418

	lda #$40			; bit 6 = init music
	sta mstatus

	rts


;====================================
;music off

musicoff
	lda #$c0         	; music off
	sta mstatus
	rts

;	-------------------------------------------------------
;	Play
;	-------------------------------------------------------

playmusic
	inc counter
	
	bit mstatus      	; get status
	bmi _off			; neg? then music is off (bit 7, 8)
	bvc contplay		; bit 6 clear? play

	lda #$00			; init song
	sta counter
	
	ldx #2
-	sta posoffset,x		; set pattern pointers
	sta patoffset,x
	sta lengthleft,x	; clear length
	sta notenum,x		; clear note
	dex
	bpl -
	
	sta mstatus			; status = 0 (play)
	jmp contplay


;==========
;music is off (mstatus $80 or $c0)

_off
	bvc +				; bit 6 clear? exit
	
	lda #$00			; clear control regs
	sta $d404
	sta $d40b
	sta $d412
	
	lda #$0f			; max volume
	sta $d418
	
	lda #$80			; next time, go to end
	sta mstatus
	
+ 	jmp musicend


;==========
;music is playing (mstatus otherwise)

;	-------------------------------------------------------
;	Continue to play music
;	-------------------------------------------------------

contplay
	ldx #2				; voice
	
	dec speed
	bpl mainloop
	
	lda resetspd		; reset speed counter
	sta speed

mainloop
	lda regoffsets,x
	sta tmpregofst
	tay					; y = sid reg offset

; check whether a new note is needed

	lda speed
	cmp resetspd
	beq checknewnote	; speed was reset; get notedata
	jmp vibrato			; keep 

checknewnote

	lda currtrkhi,x
	sta $02
	lda currtrklo,x
	sta $03				; ($02) = current track ptr

	dec lengthleft,x	; previous note done?
	bmi getnewnote		; yes, get new

	jmp soundwork		; keep sounding


;==========
;notework
;a new note is needed. get the pattern number/cc from this position

getnewnote
	ldy posoffset,x		; y = pattern position
	lda ($02),y			; a = note data
	
	cmp #$ff			; a == $ff ? restart
	beq restart
	
	cmp #$fe			; a == $fe ? stop
	bne getnotedata
	
	jmp musicend

;cc of $ff restarts this track from the first position

restart
	lda #0		        ; reset position
	sta lengthleft,x
	sta posoffset,x
	sta patoffset,x
	jmp getnewnote		; and get new note

;	-------------------------------------------------------
;	getnotedata
;	-------------------------------------------------------
;	Get note data from current pattern + pos
;	Byte 1: Length of note (bits 0-4)
;	        No release (bit 5)
;	        Retrig off (bit 6)
;	        New instr/porta coming (bit 7)
;	Byte 2: Instr. number or porta speed
;	        if bit 7 on byte 1 set, else 0
;	Byte 3: Note number
;	-------------------------------------------------------

getnotedata
	tay
	lda patptl,y
	sta $04
	lda patpth,y
	sta $05				; ($04) = pattern ptr
	
	lda #0				; porta disable as default
	sta portaval,x
	
	ldy patoffset,x		; y = pattern position
	
	lda #$ff			; retrig on as default
	sta appendfl

	lda ($04),y			; get note duration
	sta savelnthcc,x
	sta templnthcc
	and #$1f			; keep bits 0-4 (value: 0-31)
	sta lengthleft,x

	bit templnthcc		; test for append (bit 6)
	bvs appendnote

	inc patoffset,x		; next column

	lda templnthcc		; 2nd byte needed?
	bpl getpitch

;2nd byte needed as 1st byte negative
;2nd byte is the instrument number(+ve)
;or portamento speed(-ve)

	iny
	lda ($04),y			; a = get instr/portamento
	bpl +				; is instrument

	sta portaval,x		; is portamento if negative
	jmp ++

+	sta instrnr,x

++	inc patoffset,x		; next column

; 3rd byte is the pitch of the note get the 'base frequency' here

getpitch
	iny
	lda ($04),y			; a = note
	sta notenum,x
	asl
	tay					; y = note * 2
	
	lda notefreqsl,y
	sta tempfreq
	lda notefreqsh,y
	
	ldy tmpregofst		; y = sid reg offset
	sta $d401,y			; freq hi
	sta savefreqhi,x
	
	lda tempfreq
	sta $d400,y			; freq lo
	sta savefreqlo,x
	jmp getinstrument

appendnote
	dec appendfl     ;clever eh?

;	-------------------------------------------------------
;	getinstrument
;	-------------------------------------------------------
;	Get instrument data
;	-------------------------------------------------------

getinstrument
	ldy tmpregofst		; y = sid voice offset
	lda instrnr,x		; x = instrument
	stx tempstore		; tempstore = instrument no.
	asl
	asl
	asl
	tax					; x = instrument * 8 (aka offset)

	lda instr+2,x		; (2) a = control value
	sta tempctrl
	
	lda instr+2,x
	and appendfl		; if append, disable gate bit
	sta $d404,y			; control reg

	lda instr+0,x		; (0) pulse width lo
	sta $d402,y

	lda instr+1,x		; (1) pulse width hi
	sta $d403,y

	lda instr+3,x		; (3) attack/decay
	sta $d405,y

	lda instr+4,x		; (4) sustain/release
	sta $d406,y

	ldx tempstore	    ; x = instrument no.
	lda tempctrl		; instrument control value
	sta voicectrl,x

	inc patoffset,x		; point to 4th byte
	
	ldy patoffset,x
	lda ($04),y			; a = value

	cmp #$ff			; end of pattern?
	bne +

	lda #$00			; reset pattern position
	sta patoffset,x
	inc posoffset,x

+	jmp loopcont


;	-------------------------------------------------------
;	soundwork
;	-------------------------------------------------------
;	Update instrument and effects
;	-------------------------------------------------------

soundwork
	ldy tmpregofst		; y = sid voice offset

	lda savelnthcc,x	
	and #$20			; bit 4 set = sustain
	bne vibrato

	lda lengthleft,x	; note done?
	bne vibrato

	lda voicectrl,x
	and #$fe			; start release by disabling gate bit
	sta $d404,y

	lda #$00
	sta $d405,y			; AD = 0
	sta $d406,y			; SR = 0

;	-------------------------------------------------------
;	Vibrato
;	-------------------------------------------------------
;	A clever little trick to create an oscillating value
;	-------------------------------------------------------

vibrato
	lda instrnr,x		; a = instrument no.
	asl
	asl
	asl
	tay					; y = instrument offset
	sty instnumby8
	
	lda instr+7,y		; (7) instrument fx
	sta instrfx
	
	lda instr+6,y		; (6) pulse speed
	sta pulsevalue
	
	lda instr+5,y		; (5) vibrato depth
	sta vibrdepth
	beq pulsework		; no vibrato here
	
	lda counter			; (this is clever!!)
	and #7				; counter 0-7
	cmp #4				; from 4+
	bcc +
	eor #7				; is reversed
+	sta oscilatval		; for sequence 01233210

	lda notenum,x		; a = note
	asl					; a *= 2
	tay					; y = a
	sec
	lda notefreqsl+2,y
	sbc notefreqsl,y
	sta tmpvdiflo		; diff = (note + 1) - note
	lda notefreqsh+2,y
	sbc notefreqsh,y
	
-	lsr					; divide diff by 2
	ror tmpvdiflo
	dec vibrdepth		; for each depth
	bpl -
	
	sta tmpvdifhi
	
	lda notefreqsl,y	; save note frequency
	sta tmpvfrqlo
	lda notefreqsh,y
	sta tmpvfrqhi
	
	lda savelnthcc,x
	and #$1f			; a = note duration
	cmp #8
	bcc +				; less than 8 ? no need for vibrato
	
	ldy oscilatval		; y = oscillator value
	
-	dey              	;depending on the osc
	bmi +            	;value, add the vibr
	clc              	;freq that many times
	lda tmpvfrqlo    	;to the base freq
	adc tmpvdiflo
	sta tmpvfrqlo
	lda tmpvfrqhi
	adc tmpvdifhi
	sta tmpvfrqhi
	jmp -

+	ldy tmpregofst		; store and load SID
	lda tmpvfrqlo
	sta $d400,y
	lda tmpvfrqhi
	sta $d401,y


;	-------------------------------------------------------
;	pulse-width
;	-------------------------------------------------------
;	Delay, speed
;	The pulse-speed value is also used in inc/dec depth
;	-------------------------------------------------------

pulsework
	lda pulsevalue		; a = pulse value
	beq portamento		; 0 = disabled
	
	ldy instnumby8		; y = instr offset
	
	and #$1f
	
	dec pulsedelay,x	; delayed?
	bpl portamento		; yes, skip it
	
	sta pulsedelay,x	; reset delay
	
	lda pulsevalue		; restrict pulse speed
	and #$e0         	; to $00, $20, $40, $60 ...
	sta pulsespeed
	
	lda pulsedir,x		; a = pulse direction
	bne pulsedown		; !0 = down
	
	lda pulsespeed		; 0 = up
	clc
	adc instr+0,y		; add speed
	pha					; <- pw hi
	lda instr+1,y
	adc #$00
	and #$0f
	pha					; <- pw lo
	cmp #$0e			; pulse max ?
	bne dumpulse
	inc pulsedir,x		; yes, flip direction
	jmp dumpulse

pulsedown
	sec
	lda instr+0,y
	sbc pulsespeed		; subtract speed
	pha					; <- pw hi
	lda instr+1,y
	sbc #$00
	and #$0f
	pha					; <- pw lo
	cmp #$08			; pulse min?
	bne dumpulse
	dec pulsedir,x		; yes, flip direction

dumpulse
	stx tempstore
	ldx tmpregofst
	pla					; -> pwlo
	sta instr+1,y
	sta $d403,x			; PWHI
	pla					; -> pwhi
	sta instr+0,y
	sta $d402,x			; PWLO
	ldx tempstore

;	-------------------------------------------------------
;	portamento (2nd byte negative)
;	-------------------------------------------------------

portamento
	ldy tmpregofst
	lda portaval,x		; get porta
	beq drums			; 0 = disabled
	
	and #$7e			; cap
	sta tempstore
	
	lda portaval,x		; bit 0 signals up/down
	and #$01
	beq _up
	
	sec					; down
	lda savefreqlo,x ;sub portaval from
	sbc tempstore    ;current frequency
	sta savefreqlo,x
	sta $d400,y
	lda savefreqhi,x
	sbc #$00         ;(word arithmetic)
	sta savefreqhi,x
	sta $d401,y
	jmp drums

_up clc              ;portamento up
	lda savefreqlo,x ;add portval to
	adc tempstore    ;current frequency
	sta savefreqlo,x
	sta $d400,y
	lda savefreqhi,x
	adc #$00
	sta savefreqhi,x
	sta $d401,y

;	-------------------------------------------------------
;	drums (bit 0 of instrfx)
;	-------------------------------------------------------
;	Drum timbre depends on ctrl register value for instrument
;	If ctrl is 0, it's all noise, otherwise ctrl waveform
;	and noise after 1st vblank. Fast attack/decay and a rapid
;	downwards frequency slide creates this sound
;	-------------------------------------------------------

drums
	lda instrfx
	and #$01			; bit 1 = drums
	beq skydive
	
	lda savefreqhi,x
	beq skydive			; freq is already at lowest
	
	lda lengthleft,x
	beq skydive			; note is done
	
	lda savelnthcc,x	; a = note info
	and #$1f			; a = note length (0-31)
	sec
	sbc #$01
	cmp lengthleft,x	; is it the first vblank ?
	ldy tmpregofst
	bcc _first

	lda savefreqhi,x	; not the first hit
	dec savefreqhi,x	; decrease freqhi
	sta $d401,y
	
	lda voicectrl,x		; if ctrlreg is 0 then it's all noise
	and #$fe			; otherwise, ctrl waveform
	bne _nf

_first
	lda savefreqhi,x ;noise is used for
	sta $d401,y      ;the first vbl also
	lda #$80         ;(set noise)

_nf
	sta $d404,y

;	-------------------------------------------------------
;	skydive (bit 1 of instrfx)
;	-------------------------------------------------------
;	The famous Hubbard signature effect
;	A long slide down from note to zero
;	-------------------------------------------------------

skydive
	lda instrfx      ;check if skydive
	and #$02         ;needed this instr
	beq octarp
	
	lda counter      ;every 2nd vbl
	and #$01
	beq octarp
	
	lda savefreqhi,x ;check if skydive
	beq octarp        ;already complete
	
	dec savefreqhi,x ;decr and save the
	ldy tmpregofst   ;high byte freq
	sta $d401,y

;	-------------------------------------------------------
;	arpeggio (bit 2 of instrfx)
;	-------------------------------------------------------
;	Simple octave arpeggio, also a signature sound
;	-------------------------------------------------------

octarp
	lda instrfx
	and #$04			; bit 2 = octave arpeggio
	beq loopcont
	
	lda counter			; 0 or 1
	and #$01
	beq +
	
	lda notenum,x		; a = note
	clc
	adc #$0c			; note + 12
	jmp ++

+	lda notenum,x    ;even, note

++	asl
	tay					; load freq
	lda notefreqsl,y
	sta tempfreq
	lda notefreqsh,y
	ldy tmpregofst
	sta $d401,y
	lda tempfreq
	sta $d400,y


;==========
;end of dbf loop

loopcont
	dex					; next channel
	bmi musicend
	jmp mainloop

musicend
  rts

;====================================
;frequenz data
;====================================

notefreqsl
notefreqsh = * + 1
 !wo $0116,$0127,$0138,$014b,$015f,$0173,$018a,$01a1,$01ba,$01d4,$01f0,$020e
 !wo $022d,$024e,$0271,$0296,$02bd,$02e7,$0313,$0342,$0374,$03a9,$03e0,$041b
 !wo $045a,$049b,$04e2,$052c,$057b,$05ce,$0627,$0685,$06e8,$0751,$07c1,$0837
 !wo $08b4,$0937,$09c4,$0a57,$0af5,$0b9c,$0c4e,$0d09,$0dd0,$0ea3,$0f82,$106e
 !wo $1168,$126e,$1388,$14af,$15eb,$1739,$189c,$1a13,$1ba1,$1d46,$1f04,$20dc
 !wo $22d0,$24dc,$2710,$295e,$2bd6,$2e72,$3138,$3426,$3742,$3a8c,$3e08,$41b8
 !wo $45a0,$49b8,$4e20,$52bc,$57ac,$5ce4,$6270,$684c,$6e84,$7518,$7c10,$8370
 !wo $8b40,$9370,$9c40,$a578,$af58,$b9c8,$c4e0,$d098,$dd08,$ea30,$f820,$fd2e


regoffsets !by $00,$07,$0e
tmpregofst !by $00
posoffset  !by $00,$00,$00
patoffset  !by $00,$00,$00
lengthleft !by $00,$00,$00
savelnthcc !by $00,$00,$00
voicectrl  !by $00,$00,$00
notenum    !by $00,$00,$00
instrnr    !by $00,$00,$00
appendfl   !by $00
templnthcc !by $00
tempfreq   !by $00
tempstore  !by $00
tempctrl   !by $00
vibrdepth  !by $00
pulsevalue !by $00
tmpvdiflo  !by $00
tmpvdifhi  !by $00
tmpvfrqlo  !by $00
tmpvfrqhi  !by $00
oscilatval !by $00
pulsedelay !by $00,$00,$00
pulsedir   !by $00,$00,$00
speed      !by $00
resetspd   !by $01
instnumby8 !by $00
mstatus    !by $c0
savefreqhi !by $00,$00,$00
savefreqlo !by $00,$00,$00
portaval   !by $00,$00,$00
instrfx    !by $00
pulsespeed !by $00
counter    !by $00
currtrkhi  !by $00,$00,$00
currtrklo  !by $00,$00,$00


;====================================
;monty on the run main theme
;====================================

songs =*
 !by <montymaintr1
 !by <montymaintr2
 !by <montymaintr3
 !by >montymaintr1
 !by >montymaintr2
 !by >montymaintr3


;====================================
;pointers to the patterns

;low pointers
patptl =*
 !by <ptn00
 !by <ptn01
 !by <ptn02
 !by <ptn03
 !by <ptn04
 !by <ptn05
 !by <ptn06
 !by <ptn07
 !by <ptn08
 !by <ptn09
 !by <ptn0a
 !by <ptn0b
 !by <ptn0c
 !by <ptn0d
 !by <ptn0e
 !by <ptn0f
 !by <ptn10
 !by <ptn11
 !by <ptn12
 !by <ptn13
 !by <ptn14
 !by <ptn15
 !by <ptn16
 !by <ptn17
 !by <ptn18
 !by <ptn19
 !by <ptn1a
 !by <ptn1b
 !by <ptn1c
 !by <ptn1d
 !by <ptn1e
 !by <ptn1f
 !by <ptn20
 !by <ptn21
 !by <ptn22
 !by <ptn23
 !by <ptn24
 !by <ptn25
 !by <ptn26
 !by <ptn27
 !by <ptn28
 !by <ptn29
 !by <ptn2a
 !by <ptn2b
 !by <ptn2c
 !by <ptn2d
 !by 0
 !by <ptn2f
 !by <ptn30
 !by <ptn31
 !by <ptn32
 !by <ptn33
 !by <ptn34
 !by <ptn35
 !by <ptn36
 !by <ptn37
 !by <ptn38
 !by <ptn39
 !by <ptn3a
 !by <ptn3b

;high pointers
patpth =*
 !by >ptn00
 !by >ptn01
 !by >ptn02
 !by >ptn03
 !by >ptn04
 !by >ptn05
 !by >ptn06
 !by >ptn07
 !by >ptn08
 !by >ptn09
 !by >ptn0a
 !by >ptn0b
 !by >ptn0c
 !by >ptn0d
 !by >ptn0e
 !by >ptn0f
 !by >ptn10
 !by >ptn11
 !by >ptn12
 !by >ptn13
 !by >ptn14
 !by >ptn15
 !by >ptn16
 !by >ptn17
 !by >ptn18
 !by >ptn19
 !by >ptn1a
 !by >ptn1b
 !by >ptn1c
 !by >ptn1d
 !by >ptn1e
 !by >ptn1f
 !by >ptn20
 !by >ptn21
 !by >ptn22
 !by >ptn23
 !by >ptn24
 !by >ptn25
 !by >ptn26
 !by >ptn27
 !by >ptn28
 !by >ptn29
 !by >ptn2a
 !by >ptn2b
 !by >ptn2c
 !by >ptn2d
 !by 0
 !by >ptn2f
 !by >ptn30
 !by >ptn31
 !by >ptn32
 !by >ptn33
 !by >ptn34
 !by >ptn35
 !by >ptn36
 !by >ptn37
 !by >ptn38
 !by >ptn39
 !by >ptn3a
 !by >ptn3b


;====================================
;tracks
;====================================

;track1
montymaintr1 =*
 !by $11,$14,$17,$1a,$00,$27,$00,$28
 !by $03,$05,$00,$27,$00,$28,$03,$05
 !by $07,$3a,$14,$17,$00,$27,$00,$28
 !by $2f,$30,$31,$31,$32,$33,$33,$34
 !by $34,$34,$34,$34,$34,$34,$34,$35
 !by $35,$35,$35,$35,$35,$36,$12,$37
 !by $38,$09,$2a,$09,$2b,$09,$0a,$09
 !by $2a,$09,$2b,$09,$0a,$0d,$0d,$0f
 !by $ff

;track2
montymaintr2 =*
 !by $12,$15,$18,$1b,$2d,$39,$39
 !by $39,$39,$39,$39,$2c,$39,$39,$39
 !by $39,$39,$39,$2c,$39,$39,$39,$01
 !by $01,$29,$29,$2c,$15,$18,$39,$39
 !by $39,$39,$39,$39,$39,$39,$39,$39
 !by $39,$39,$39,$39,$39,$39,$39,$39
 !by $39,$39,$39,$39,$39,$39,$39,$39
 !by $39,$39,$39,$39,$39,$01,$01,$01
 !by $29,$39,$39,$39,$01,$01,$01,$29
 !by $39,$39,$39,$39,$ff

;track3
montymaintr3 =*
 !by $13,$16,$19
 !by $1c,$02,$02,$1d,$1e,$02,$02,$1d
 !by $1f,$04,$04,$20,$20,$06,$02,$02
 !by $1d,$1e,$02,$02,$1d,$1f,$04,$04
 !by $20,$20,$06,$08,$08,$08,$08,$21
 !by $21,$21,$21,$22,$22,$22,$23,$22
 !by $24,$25,$3b,$26,$26,$26,$26,$26
 !by $26,$26,$26,$26,$26,$26,$26,$26
 !by $26,$26,$26,$02,$02,$1d,$1e,$02
 !by $02,$1d,$1f,$2f,$2f,$2f,$2f,$2f
 !by $2f,$2f,$2f,$2f,$2f,$2f,$2f,$2f
 !by $0b,$0b,$1d,$1d,$0b,$0b,$1d,$0b
 !by $0b,$0b,$0c,$0c,$1d,$1d,$1d,$10
 !by $0b,$0b,$1d,$1d,$0b,$0b,$1d,$0b
 !by $0b,$0b,$0c,$0c,$1d,$1d,$1d,$10
 !by $0b,$1d,$0b,$1d,$0b,$1d,$0b,$1d
 !by $0b,$0c,$1d,$0b,$0c,$23,$0b,$0b
 !by $ff


;====================================
;patterns
;====================================

ptn00 =*
 !by $83,$00,$37,$01,$3e,$01,$3e,$03
 !by $3d,$03,$3e,$03,$43,$03,$3e,$03
 !by $3d,$03,$3e,$03,$37,$01,$3e,$01
 !by $3e,$03,$3d,$03,$3e,$03,$43,$03
 !by $42,$03,$43,$03,$45,$03,$46,$01
 !by $48,$01,$46,$03,$45,$03,$43,$03
 !by $4b,$01,$4d,$01,$4b,$03,$4a,$03
 !by $48,$ff

ptn27 =*
 !by $1f,$4a,$ff

ptn28 =*
 !by $03,$46,$01,$48,$01,$46,$03,$45
 !by $03,$4a,$0f,$43,$ff

ptn03 =*
 !by $bf,$06
 !by $48,$07,$48,$01,$4b,$01,$4a,$01
 !by $4b,$01,$4a,$03,$4b,$03,$4d,$03
 !by $4b,$03,$4a,$3f,$48,$07,$48,$01
 !by $4b,$01,$4a,$01,$4b,$01,$4a,$03
 !by $4b,$03,$4d,$03,$4b,$03,$48,$3f
 !by $4c,$07,$4c,$01,$4f,$01,$4e,$01
 !by $4f,$01,$4e,$03,$4f,$03,$51,$03
 !by $4f,$03,$4e,$3f,$4c,$07,$4c,$01
 !by $4f,$01,$4e,$01,$4f,$01,$4e,$03
 !by $4f,$03,$51,$03,$4f,$03,$4c,$ff

ptn05 =*
 !by $83,$04,$26,$03,$29,$03,$28,$03
 !by $29,$03,$26,$03,$35,$03,$34,$03
 !by $32,$03,$2d,$03,$30,$03,$2f,$03
 !by $30,$03,$2d,$03,$3c,$03,$3b,$03
 !by $39,$03,$30,$03,$33,$03,$32,$03
 !by $33,$03,$30,$03,$3f,$03,$3e,$03
 !by $3c,$03,$46,$03,$45,$03,$43,$03
 !by $3a,$03,$39,$03,$37,$03,$2e,$03
 !by $2d,$03,$26,$03,$29,$03,$28,$03
 !by $29,$03,$26,$03,$35,$03,$34,$03
 !by $32,$03,$2d,$03,$30,$03,$2f,$03
 !by $30,$03,$2d,$03,$3c,$03,$3b,$03
 !by $39,$03,$30,$03,$33,$03,$32,$03
 !by $33,$03,$30,$03,$3f,$03,$3e,$03
 !by $3c,$03,$34,$03,$37,$03,$36,$03
 !by $37,$03,$34,$03,$37,$03,$3a,$03
 !by $3d

ptn3a =*
 !by $03,$3e,$07,$3e,$07,$3f,$07
 !by $3e,$03,$3c,$07,$3e,$57,$ff

ptn07 =*
 !by $8b
 !by $00,$3a,$01,$3a,$01,$3c,$03,$3d
 !by $03,$3f,$03,$3d,$03,$3c,$0b,$3a
 !by $03,$39,$07,$3a,$81,$06,$4b,$01
 !by $4d,$01,$4e,$01,$4d,$01,$4e,$01
 !by $4d,$05,$4b,$81,$00,$3a,$01,$3c
 !by $01,$3d,$03,$3f,$03,$3d,$03,$3c
 !by $03,$3a,$03,$39,$1b,$3a,$0b,$3b
 !by $01,$3b,$01,$3d,$03,$3e,$03,$40
 !by $03,$3e,$03,$3d,$0b,$3b,$03,$3a
 !by $07,$3b,$81,$06,$4c,$01,$4e,$01
 !by $4f,$01,$4e,$01,$4f,$01,$4e,$05
 !by $4c,$81,$00,$3b,$01,$3d,$01,$3e
 !by $03,$40,$03,$3e,$03,$3d,$03,$3b
 !by $03,$3a,$1b,$3b,$8b,$05,$35,$03
 !by $33,$07,$32,$03,$30,$03,$2f,$0b
 !by $30,$03,$32,$0f,$30,$0b,$35,$03
 !by $33,$07,$32,$03,$30,$03,$2f,$1f
 !by $30,$8b,$00,$3c,$01,$3c,$01,$3e
 !by $03,$3f,$03,$41,$03,$3f,$03,$3e
 !by $0b,$3d,$01,$3d,$01,$3f,$03,$40
 !by $03,$42,$03,$40,$03,$3f,$03,$3e
 !by $01,$3e,$01,$40,$03,$41,$03,$40
 !by $03,$3e,$03,$3d,$03,$3e,$03,$3c
 !by $03,$3a,$01,$3a,$01,$3c,$03,$3d
 !by $03,$3c,$03,$3a,$03,$39,$03,$3a
 !by $03,$3c,$ff

ptn09 =*
 !by $83,$00,$32,$01,$35,$01,$34,$03
 !by $32,$03,$35,$03,$34,$03,$32,$03
 !by $35,$01,$34,$01,$32,$03,$32,$03
 !by $3a,$03,$39,$03,$3a,$03,$32,$03
 !by $3a,$03,$39,$03,$3a,$ff

ptn2a =*
 !by $03,$34,$01,$37,$01,$35,$03,$34
 !by $03,$37,$03,$35,$03,$34,$03,$37
 !by $01,$35,$01,$34,$03,$34,$03,$3a
 !by $03,$39,$03,$3a,$03,$34,$03,$3a
 !by $03,$39,$03,$3a,$ff

ptn2b =*
 !by $03,$39,$03,$38,$03,$39,$03,$3a
 !by $03,$39,$03,$37,$03,$35,$03,$34
 !by $03,$35,$03,$34,$03,$35,$03,$37
 !by $03,$35,$03,$34,$03,$32,$03,$31
 !by $ff

ptn0a =*
 !by $03
 !by $37,$01,$3a,$01,$39,$03,$37,$03
 !by $3a,$03,$39,$03,$37,$03,$3a,$01
 !by $39,$01,$37,$03,$37,$03,$3e,$03
 !by $3d,$03,$3e,$03,$37,$03,$3e,$03
 !by $3d,$03,$3e,$03,$3d,$01,$40,$01
 !by $3e,$03,$3d,$03,$40,$01,$3e,$01
 !by $3d,$03,$40,$03,$3e,$03,$40,$03
 !by $40,$01,$43,$01,$41,$03,$40,$03
 !by $43,$01,$41,$01,$40,$03,$43,$03
 !by $41,$03,$43,$03,$43,$01,$46,$01
 !by $45,$03,$43,$03,$46,$01,$45,$01
 !by $43,$03,$46,$03,$45,$03,$43,$01
 !by $48,$01,$49,$01,$48,$01,$46,$01
 !by $45,$01,$46,$01,$45,$01,$43,$01
 !by $41,$01,$43,$01,$41,$01,$40,$01
 !by $3d,$01,$39,$01,$3b,$01,$3d,$ff

ptn0d =*
 !by $01,$3e,$01,$39,$01,$35,$01,$39
 !by $01,$3e,$01,$39,$01,$35,$01,$39
 !by $03,$3e,$01,$41,$01,$40,$03,$40
 !by $01,$3d,$01,$3e,$01,$40,$01,$3d
 !by $01,$39,$01,$3d,$01,$40,$01,$3d
 !by $01,$39,$01,$3d,$03,$40,$01,$43
 !by $01,$41,$03,$41,$01,$3e,$01,$40
 !by $01,$41,$01,$3e,$01,$39,$01,$3e
 !by $01,$41,$01,$3e,$01,$39,$01,$3e
 !by $03,$41,$01,$45,$01,$43,$03,$43
 !by $01,$40,$01,$41,$01,$43,$01,$40
 !by $01,$3d,$01,$40,$01,$43,$01,$40
 !by $01,$3d,$01,$40,$01,$46,$01,$43
 !by $01,$45,$01,$46,$01,$44,$01,$43
 !by $01,$40,$01,$3d,$ff

ptn0f =*
 !by $01,$3e,$01
 !by $39,$01,$35,$01,$39,$01,$3e,$01
 !by $39,$01,$35,$01,$39,$01,$3e,$01
 !by $39,$01,$35,$01,$39,$01,$3e,$01
 !by $39,$01,$35,$01,$39,$01,$3e,$01
 !by $3a,$01,$37,$01,$3a,$01,$3e,$01
 !by $3a,$01,$37,$01,$3a,$01,$3e,$01
 !by $3a,$01,$37,$01,$3a,$01,$3e,$01
 !by $3a,$01,$37,$01,$3a,$01,$40,$01
 !by $3d,$01,$39,$01,$3d,$01,$40,$01
 !by $3d,$01,$39,$01,$3d,$01,$40,$01
 !by $3d,$01,$39,$01,$3d,$01,$40,$01
 !by $3d,$01,$39,$01,$3d,$01,$41,$01
 !by $3e,$01,$39,$01,$3e,$01,$41,$01
 !by $3e,$01,$39,$01,$3e,$01,$41,$01
 !by $3e,$01,$39,$01,$3e,$01,$41,$01
 !by $3e,$01,$39,$01,$3e,$01,$43,$01
 !by $3e,$01,$3a,$01,$3e,$01,$43,$01
 !by $3e,$01,$3a,$01,$3e,$01,$43,$01
 !by $3e,$01,$3a,$01,$3e,$01,$43,$01
 !by $3e,$01,$3a,$01,$3e,$01,$43,$01
 !by $3f,$01,$3c,$01,$3f,$01,$43,$01
 !by $3f,$01,$3c,$01,$3f,$01,$43,$01
 !by $3f,$01,$3c,$01,$3f,$01,$43,$01
 !by $3f,$01,$3c,$01,$3f,$01,$45,$01
 !by $42,$01,$3c,$01,$42,$01,$45,$01
 !by $42,$01,$3c,$01,$42,$01,$48,$01
 !by $45,$01,$42,$01,$45,$01,$4b,$01
 !by $48,$01,$45,$01,$48,$01,$4b,$01
 !by $4a,$01,$48,$01,$4a,$01,$4b,$01
 !by $4a,$01,$48,$01,$4a,$01,$4b,$01
 !by $4a,$01,$48,$01,$4a,$01,$4c,$01
 !by $4e,$03,$4f,$ff

ptn11 =*
 !by $bf,$06,$56,$1f,$57,$1f,$56,$1f
 !by $5b,$1f,$56,$1f,$57,$1f,$56,$1f
 !by $4f,$ff

ptn12 =*
 !by $bf,$0c,$56,$7f,$7f,$7f,$7f,$7f
 !by $7f,$7f,$ff

ptn13 =*
 !by $bf,$08,$13,$3f,$13,$3f,$13,$3f
 !by $13,$3f,$13,$3f,$13,$3f,$13,$1f
 !by $13,$ff

ptn14 =*
 !by $97,$09,$2e,$03,$2e,$1b,$32,$03
 !by $32,$1b,$31,$03,$31,$1f,$34,$43
 !by $17,$32,$03,$32,$1b,$35,$03,$35
 !by $1b,$34,$03,$34,$0f,$37,$8f,$0a
 !by $37,$43,$ff

ptn15 =*
 !by $97,$09,$2b,$03,$2b,$1b,$2e,$03
 !by $2e,$1b,$2d,$03,$2d,$1f,$30,$43
 !by $17,$2e,$03,$2e,$1b,$32,$03,$32
 !by $1b,$31,$03,$31,$0f,$34,$8f,$0a
 !by $34,$43,$ff

ptn16 =*
 !by $0f,$1f,$0f,$1f,$0f,$1f,$0f,$1f
 !by $0f,$1f,$0f,$1f,$0f,$1f,$0f,$1f
 !by $0f,$1f,$0f,$1f,$0f,$1f,$0f,$1f
 !by $0f,$1f,$0f,$1f,$0f,$1f,$0f,$1f
 !by $ff

ptn17 =*
 !by $97,$09,$33,$03,$33,$1b,$37,$03
 !by $37,$1b,$36,$03,$36,$1f,$39,$43
 !by $17,$37,$03,$37,$1b,$3a,$03,$3a
 !by $1b,$39,$03,$39,$2f,$3c,$21,$3c
 !by $21,$3d,$21,$3e,$21,$3f,$21,$40
 !by $21,$41,$21,$42,$21,$43,$21,$44
 !by $01,$45,$ff

ptn18 =*
 !by $97,$09,$30,$03,$30,$1b,$33,$03
 !by $33,$1b,$32,$03,$32,$1f,$36,$43
 !by $17,$33,$03,$33,$1b,$37,$03,$37
 !by $1b,$36,$03,$36,$2f,$39,$21,$39
 !by $21,$3a,$21,$3b,$21,$3c,$21,$3d
 !by $21,$3e,$21,$3f,$21,$40,$21,$41
 !by $01,$42,$ff

ptn19 =*
 !by $0f,$1a,$0f,$1a,$0f,$1a,$0f,$1a
 !by $0f,$1a,$0f,$1a,$0f,$1a,$0f,$1a
 !by $0f,$1a,$0f,$1a,$0f,$1a,$0f,$1a
 !by $0f,$1a,$0f,$1a,$0f,$1a,$0f,$1a
 !by $ff

ptn1a =*
 !by $1f,$46,$bf,$0a,$46,$7f,$7f,$ff

ptn1b =*
 !by $1f,$43,$bf,$0a,$43,$7f,$ff

ptn1c =*
 !by $83,$02,$13,$03,$13,$03,$1e,$03
 !by $1f,$03,$13,$03,$13,$03,$1e,$03
 !by $1f,$03,$13,$03,$13,$03,$1e,$03
 !by $1f,$03,$13,$03,$13,$03,$1e,$03
 !by $1f,$03,$13,$03,$13,$03,$1e,$03
 !by $1f,$03,$13,$03,$13,$03,$1e,$03
 !by $1f,$03,$13,$03,$13,$03,$1e,$03
 !by $1f,$03,$13,$03,$13,$03,$1e,$03
 !by $1f,$ff

ptn29 =*
 !by $8f,$0b,$38,$4f,$ff

ptn2c =*
 !by $83,$0e,$32,$07,$32,$07,$2f,$07
 !by $2f,$03,$2b,$87,$0b,$46,$83,$0e
 !by $2c,$03,$2c,$8f,$0b,$32,$ff

ptn2d =*
 !by $43,$83,$0e,$32,$03,$32,$03,$2f
 !by $03,$2f,$03,$2c,$87,$0b,$38,$ff

ptn39 =*
 !by $83,$01
 !by $43,$01,$4f,$01,$5b,$87,$03,$2f
 !by $83,$01,$43,$01,$4f,$01,$5b,$87
 !by $03,$2f,$83,$01,$43,$01,$4f,$01
 !by $5b,$87,$03,$2f,$83,$01,$43,$01
 !by $4f,$01,$5b,$87,$03,$2f,$83,$01
 !by $43,$01,$4f,$01,$5b,$87,$03,$2f
 !by $83,$01,$43,$01,$4f,$01,$5b,$87
 !by $03,$2f

ptn01 =*
 !by $83,$01,$43,$01,$4f,$01,$5b,$87
 !by $03,$2f,$83,$01,$43,$01,$4f,$01
 !by $5b,$87,$03,$2f,$ff

ptn02 =*
 !by $83,$02,$13,$03,$13,$03,$1f,$03
 !by $1f,$03,$13,$03,$13,$03,$1f,$03
 !by $1f,$ff

ptn1d =*
 !by $03,$15,$03,$15,$03,$1f,$03,$21
 !by $03,$15,$03,$15,$03,$1f,$03,$21
 !by $ff

ptn1e =*
 !by $03,$1a,$03,$1a,$03,$1c,$03,$1c
 !by $03,$1d,$03,$1d,$03,$1e,$03,$1e
 !by $ff

ptn1f =*
 !by $03,$1a,$03,$1a,$03,$24,$03,$26
 !by $03,$13,$03,$13,$07,$1f,$ff

ptn04 =*
 !by $03,$18,$03,$18,$03,$24,$03,$24
 !by $03,$18,$03,$18,$03,$24,$03,$24
 !by $03,$20,$03,$20,$03,$2c,$03,$2c
 !by $03,$20,$03,$20,$03,$2c,$03,$2c
 !by $ff

ptn20 =*
 !by $03,$19,$03,$19,$03
 !by $25,$03,$25,$03,$19,$03,$19,$03
 !by $25,$03,$25,$03,$21,$03,$21,$03
 !by $2d,$03,$2d,$03,$21,$03,$21,$03
 !by $2d,$03,$2d,$ff

ptn06 =*
 !by $03,$1a,$03,$1a
 !by $03,$26,$03,$26,$03,$1a,$03,$1a
 !by $03,$26,$03,$26,$03,$15,$03,$15
 !by $03,$21,$03,$21,$03,$15,$03,$15
 !by $03,$21,$03,$21,$03,$18,$03,$18
 !by $03,$24,$03,$24,$03,$18,$03,$18
 !by $03,$24,$03,$24,$03,$1f,$03,$1f
 !by $03,$2b,$03,$2b,$03,$1f,$03,$1f
 !by $03,$2b,$03,$2b,$03,$1a,$03,$1a
 !by $03,$26,$03,$26,$03,$1a,$03,$1a
 !by $03,$26,$03,$26,$03,$15,$03,$15
 !by $03,$21,$03,$21,$03,$15,$03,$15
 !by $03,$21,$03,$21,$03,$18,$03,$18
 !by $03,$24,$03,$24,$03,$18,$03,$18
 !by $03,$24,$03,$24,$03,$1c,$03,$1c
 !by $03,$28,$03,$28,$03,$1c,$03,$1c
 !by $03,$28,$03,$28

ptn3b =*
 !by $83,$04,$36,$07
 !by $36,$07,$37,$07,$36,$03,$33,$07
 !by $32,$57,$ff

ptn08 =*
 !by $83,$02,$1b,$03,$1b,$03,$27,$03
 !by $27,$03,$1b,$03,$1b,$03,$27,$03
 !by $27,$ff

ptn21 =*
 !by $03,$1c,$03,$1c,$03,$28,$03,$28
 !by $03,$1c,$03,$1c,$03,$28,$03,$28
 !by $ff

ptn22 =*
 !by $03,$1d,$03,$1d,$03,$29,$03,$29
 !by $03,$1d,$03,$1d,$03,$29,$03,$29
 !by $ff

ptn23 =*
 !by $03,$18,$03,$18,$03,$24,$03,$24
 !by $03,$18,$03,$18,$03,$24,$03,$24
 !by $ff

ptn24 =*
 !by $03,$1e,$03,$1e,$03,$2a,$03,$2a
 !by $03,$1e,$03,$1e,$03,$2a,$03,$2a
 !by $ff

ptn25 =*
 !by $83,$05,$26,$01,$4a,$01,$34,$03
 !by $29,$03,$4c,$03,$4a,$03,$31,$03
 !by $4a,$03,$24,$03,$22,$01,$46,$01
 !by $30,$03,$25,$03,$48,$03,$46,$03
 !by $2d,$03,$46,$03,$24,$ff

ptn0b =*
 !by $83,$02,$1a,$03,$1a,$03,$26,$03
 !by $26,$03,$1a,$03,$1a,$03,$26,$03
 !by $26,$ff

ptn0c =*
 !by $03,$13,$03,$13,$03,$1d,$03,$1f
 !by $03,$13,$03,$13,$03,$1d,$03,$1f
 !by $ff

ptn26 =*
 !by $87,$02,$1a,$87,$03,$2f,$83,$02
 !by $26,$03,$26,$87,$03,$2f,$ff

ptn10 =*
 !by $07,$1a,$4f,$47,$ff

ptn0e =*
 !by $03,$1f,$03,$1f,$03,$24,$03,$26
 !by $07,$13,$47,$ff

ptn30 =*
 !by $bf,$0f,$32,$0f,$32,$8f,$90,$30
 !by $3f,$32,$13,$32,$03,$32,$03,$35
 !by $03,$37,$3f,$37,$0f,$37,$8f,$90
 !by $30,$3f,$32,$13,$32,$03,$2d,$03
 !by $30,$03,$32,$ff

ptn31 =*
 !by $0f,$32
 !by $af,$90,$35,$0f,$37,$a7,$99,$37
 !by $07,$35,$3f,$32,$13,$32,$03,$32
 !by $a3,$e8,$35,$03,$37,$0f,$35,$af
 !by $90,$37,$0f,$37,$a7,$99,$37,$07
 !by $35,$3f,$32,$13,$32,$03,$2d,$a3
 !by $e8,$30,$03,$32,$ff

ptn32 =*
 !by $07,$32,$03
 !by $39,$13,$3c,$a7,$9a,$37,$a7,$9b
 !by $38,$07,$37,$03,$35,$03,$32,$03
 !by $39,$1b,$3c,$a7,$9a,$37,$a7,$9b
 !by $38,$07,$37,$03,$35,$03,$32,$03
 !by $39,$03,$3c,$03,$3e,$03,$3c,$07
 !by $3e,$03,$3c,$03,$39,$a7,$9a,$37
 !by $a7,$9b,$38,$07,$37,$03,$35,$03
 !by $32,$af,$90,$3c,$1f,$3e,$43,$03
 !by $3e,$03,$3c,$03,$3e,$ff

ptn33 =*
 !by $03,$3e
 !by $03,$3e,$a3,$e8,$3c,$03,$3e,$03
 !by $3e,$03,$3e,$a3,$e8,$3c,$03,$3e
 !by $03,$3e,$03,$3e,$a3,$e8,$3c,$03
 !by $3e,$03,$3e,$03,$3e,$a3,$e8,$3c
 !by $03,$3e,$af,$91,$43,$1f,$41,$43
 !by $03,$3e,$03,$41,$03,$43,$03,$43
 !by $03,$43,$a3,$e8,$41,$03,$43,$03
 !by $43,$03,$43,$a3,$e8,$41,$03,$43
 !by $03,$45,$03,$48,$a3,$fd,$45,$03
 !by $44,$01,$43,$01,$41,$03,$3e,$03
 !by $3c,$03,$3e,$2f,$3e,$bf,$98,$3e
 !by $43,$03,$3e,$03,$3c,$03,$3e,$ff

ptn34 =*
 !by $03,$4a,$03,$4a,$a3,$f8,$48,$03
 !by $4a,$03,$4a,$03,$4a,$a3,$f8,$48
 !by $03,$4a,$ff

ptn35 =*
 !by $01,$51,$01,$54,$01
 !by $51,$01,$54,$01,$51,$01,$54,$01
 !by $51,$01,$54,$01,$51,$01,$54,$01
 !by $51,$01,$54,$01,$51,$01,$54,$01
 !by $51,$01,$54,$ff

ptn36 =*
 !by $01,$50,$01,$4f
 !by $01,$4d,$01,$4a,$01,$4f,$01,$4d
 !by $01,$4a,$01,$48,$01,$4a,$01,$48
 !by $01,$45,$01,$43,$01,$44,$01,$43
 !by $01,$41,$01,$3e,$01,$43,$01,$41
 !by $01,$3e,$01,$3c,$01,$3e,$01,$3c
 !by $01,$39,$01,$37,$01,$38,$01,$37
 !by $01,$35,$01,$32,$01,$37,$01,$35
 !by $01,$32,$01,$30,$ff

ptn37 =*
 !by $5f,$5f,$5f
 !by $47,$83,$0e,$32,$07,$32,$07,$2f
 !by $03,$2f,$07,$2f,$97,$0b,$3a,$5f
 !by $5f,$47,$8b,$0e,$32,$03,$32,$03
 !by $2f,$03,$2f,$47,$97,$0b,$3a,$5f
 !by $5f,$47,$83,$0e,$2f,$0b,$2f,$03
 !by $2f,$03,$2f,$87,$0b,$30,$17,$3a
 !by $5f,$8b,$0e,$32,$0b,$32,$0b,$2f
 !by $0b,$2f,$07,$2c,$07,$2c,$ff

ptn38 =*
 !by $87
 !by $0b,$34,$17,$3a,$5f,$5f,$84,$0e
 !by $32,$04,$32,$05,$32,$04,$2f,$04
 !by $2f,$05,$2f,$47,$97,$0b,$3a,$5f
 !by $5f,$84,$0e,$32,$04,$32,$05,$32
 !by $04,$2f,$04,$2f,$05,$2f,$ff

ptn2f =*
 !by $03,$1a,$03,$1a,$03
 !by $24,$03,$26,$03,$1a,$03,$1a,$03
 !by $18,$03,$19,$03,$1a,$03,$1a,$03
 !by $24,$03,$26,$03,$1a,$03,$1a,$03
 !by $18,$03,$19,$03,$18,$03,$18,$03
 !by $22,$03,$24,$03,$18,$03,$18,$03
 !by $16,$03,$17,$03,$18,$03,$18,$03
 !by $22,$03,$24,$03,$18,$03,$18,$03
 !by $16,$03,$17,$03,$13,$03,$13,$03
 !by $1d,$03,$1f,$03,$13,$03,$13,$03
 !by $1d,$03,$1e,$03,$13,$03,$13,$03
 !by $1d,$03,$1f,$03,$13,$03,$13,$03
 !by $1d,$03,$1e,$03,$1a,$03,$1a,$03
 !by $24,$03,$26,$03,$1a,$03,$1a,$03
 !by $18,$03,$19,$03,$1a,$03,$1a,$03
 !by $24,$03,$26,$03,$1a,$03,$1a,$03
 !by $18,$03,$19,$ff



;====================================
;instruments
;====================================

; Drum = $01
; Sky = $02
; Arp = $04


	; 0     1     2     3     4    5     6    7
	;  XXX,XXX,CTL,XXX,XXX,VIB,PUL,FX

instr =*
 !by $80,$09,$41,$48,$60,$03,$81,$00
 !by $00,$08,$81,$02,$08,$00,$00,$01
 !by $a0,$02,$41,$09,$80,$00,$00,$00
 !by $00,$02,$81,$09,$09,$00,$00,$05
 !by $00,$08,$41,$08,$50,$02,$00,$04
 !by $00,$01,$41,$3f,$c0,$02,$00,$00
 !by $00,$08,$41,$04,$40,$02,$00,$00
 !by $00,$08,$41,$09,$00,$02,$00,$00
 !by $00,$09,$41,$09,$70,$02,$5f,$04
 !by $00,$09,$41,$4a,$69,$02,$81,$00
 !by $00,$09,$41,$40,$6f,$00,$81,$02
 !by $80,$07,$81,$0a,$0a,$00,$00,$01
 !by $00,$09,$41,$3f,$ff,$01,$e7,$02		; first skydive
 !by $00,$08,$41,$90,$f0,$01,$e8,$02
 !by $00,$08,$41,$06,$0a,$00,$00,$01
 !by $00,$09,$41,$19,$70,$02,$a8,$00
 !by $00,$02,$41,$09,$90,$02,$00,$00
 !by $00,$00,$11,$0a,$fa,$00,$00,$05
 !by $00,$08,$41,$37,$40,$02,$00,$00
 !by $00,$08,$11,$07,$70,$02,$00,$00


}