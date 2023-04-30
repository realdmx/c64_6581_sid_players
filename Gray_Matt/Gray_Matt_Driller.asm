; Driller by Matt Gray 1997
; estracted from the sid in HVSC
; NOTE: the prg goes at VIC speed as I use this for misuring rastertime:
; activate the CIA for having the true speed.

; From SIDin #02, disassembled by Stefano Tognon

	!to "Gray_Matt_Driller.sid",plain
	
	* = $0000

	* = $0000

	!text "PSID"
	!be16 2							; version 2
	!be16 $7c						; data offset
	!be16 $1000						; Load (0 = auto)
	!be16 initsongs					; Init
	!be16 serviceMusic				; Play
	!be16 1							; num songs
	!be16 1							; first song
	!word 0
	!word 0
-	!text "Driller"
	!fill 32 - (* - -)
-	!text "Matt Gray"
	!fill 32 - (* - -)
-	!text "1987 Incentive Software"
	!fill 32 - (* - -)
	!be16 $0014						; v2 flags
	!be16 0							; Start page, page length (reloc)
	!be16 0							

!pseudopc $1000 {

	* = $1000


makeMusic =*

	lda songsNumber
	bne songNotZero
	sta $D418 ; Select volume and filter mode (to 0)
	rts
	
songNotZero:
	cmp #$AB ; current songs ?
	beq makeCurrent
	jmp initTr ; init the tracks
	
initMusic =*
	lda #$00 ; clear control register
	sta $D404 ; Voice 1: Control registers
	sta $D40B ; Voice 2: Control registers
	sta $D412 ; Voice 3: Control registers
	lda #$0F ; full volume
	sta $D418 ; Select volume and filter mode
	ldy #$00
	sty offsetTr1 ; set track 1 position to the beginning
	sty offsetTr2 ; set track 2 position to the beginning
	sty offsetTr3 ; set track 3 position to the beginning
	sty actNoteLength ; actual note length duration voice 1
	sty actNoteLength+$7 ; actual note length duration voice 2
	sty actNoteLength+$E ; actual note length duration voice 3
	sty ptnInd1 ; set pattern index to the beginning
	sty ptnInd2
	sty ptnInd3
	iny
	sty actSpeed ; actual cycle timer (speed of song)
	jmp decCycleTimer
	
makeCurrent:
	ldy instrInd,x ; index of instrument data
	lda instr1+7,y ; load instrument effect
	and #$04 ; is effect 4?
	beq noEffect4
	; change the control register for making a sound effect
controlEffect:
effect4:
	lda dur4Eff,x ; read duration of effect 4 switch
	beq applyNormalControl
	dec dur4Eff,x ; dec duration of effect 4 switch
	lda instr2+2,y ; control register
	sta $D404,x ; Voice 1: Control registers
	bne noEffect4
applyNormalControl:
	lda instr1+1,y ; control register
	sta $D404,x ; Voice 1: Control registers
noEffect4:
	lda actSpeed ; actual cycle timer (speed of song)
	bne noDelayEnd
	dec actNoteLength,x ; actual note length duration
	bmi readPattern ; jump if note is finished
noDelayEnd:
	jmp pulseTimbre
	
; init tracks with selected songs

initTr =*
	ldy songsNumber ; load song number (1, 2)
	lda ltr1,y ; set current track 1
	sta currentTr1+0 ; current track 1 position (base)
	lda htr1,y
	sta currentTr1+1 ; current track 1 position (base)
	lda ltr2,y ; set current track 2
	sta currentTr2+0 ; current track 2 position (base)
	lda htr2,y
	sta currentTr2+1 ; current track 2 position (base)
	lda ltr3,y ; set current track 3
	sta currentTr3+0 ; current track 3 position (base)
	lda htr3,y
	sta currentTr3+1 ; current track 3 position (base)
	lda songSpeed,y ; read the song speed
	sta speed ; cycle timer (speed of song)
	jmp initMusic
	
; decrement the cycle timer (speed of song)
	decCycleTimer =*
	cpx #$0E ; is voice 3 ?
	bne skipDec ; no, jump
	dec actSpeed ; actual cycle timer (speed of song)
	bpl skipDec ; jump if positive
	lda speed ; cycle timer (speed of song)
	sta actSpeed ; actual cycle timer (speed of song)
skipDec:
	lda #$AB ; current song
	sta songsNumber
	rts
; give in FD-FE the pattern pointer to a note
readPattern:
	lda currentTr1+0,x ; current track 1 position (base)
	sta $FB
	lda currentTr1+1,x ; current track 1 position (base)
	sta $FC ; FB-FC: track pattern pointer
	ldy offsetTr1,x ; read actual track position (offset)
	lda ($FB),y ; read actual track pattern pointer value
	tay
	lda patptl,y ; read pattern pointer low
	sta $FD
	lda patpth,y ; read pattern pointer high
	sta $FE ; FD-FE: pattern pointer
	lda #$FF
	sta controlSwitch ; on ADS
	lda #$00
	sta portaFlag,x ; no portamento
	sta arpeggioFlag,x ; no arpeggio
	sta vibratoFlag,x ; no vibrato
	; read the pattern value of note to play
readNextPat:
	ldy ptnInd,x ; load pattern index of this voice
	lda ($FD),y ; read a pattern value
	cmp #$FD ; note length
	bcc notFD ; jump if <$FD
	iny ; next pattern value index
	inc ptnInd,x ; next (saved) pattern value index
	lda ($FD),y ; read a pattern value (note duration)
	sta noteLength,x ; store note length duration
incPtnInd:
	inc ptnInd,x ; next (saved) patter value index
	bne readNextPat
notFD:
	cmp #$FB
	bcc notFB ; jump if <$FB
	cmp #$FB
	bne isFC ; jump if <>$FB
isFB:
	lda #$01 ; negative portamento
storePort:
	sta portaFlag,x ; store in portamento flag
	iny ; next pattern value index
	inc ptnInd,x ; next (saved) pattern value index
	lda ($FD),y ; read a pattern value
	sta portaVal,x ; store pattern value (portamento)
	lda #$00
	sta arpeggioFlag,x ; no arpeggio
	sta vibratoFlag,x ; no vibrato
	beq incPtnInd
isFC:
	lda #$02 ; positive portamento
	bne storePort ; store portamento value
notFB:
	cmp #$FA
	bcc newNote ; jump if <$FA
; select new instruments
	iny ; next pattern value index
	inc ptnInd,x ; next (saved) pattern value index
	lda ($FD),y ; read a pattern value (instrument index)
	asl
	asl
	asl ; index * 8
	sta instrInd,x ; index of instruments data
	tay
	lda instr1+0,y ; read Hi/Lo of pulsation amplitude
	pha
	and #$0F
	sta hi1Wave,x ; Wave form pulsation amplitude (hi byte)
	sta hi2Wave,x ; Wave form pulsation amplitude (hi byte)
	pla
	and #$F0
	sta lo1Wave,x ; Wave form pulsation amplitude (lo byte)
	sta lo2Wave,x ; Wave form pulsation amplitude (lo byte)
	jmp incPtnInd
newNote:
	sta rpNote,x ; store packet note (readed note to play)
	lda noteLength,x ; note length duration
	sta actNoteLength,x ; actual note length duration
	lda #$00
	sta cycleInt,x
	sta cycleEst,x
	lda #$02
	sta dur4Eff,x ; set duration of effect 4 switch
	ldy instrInd,x ; index of instrument data
	lda instr1+7,y ; instrument effect
	and #$02 ; is effect 2?
	beq noEffect2
effect2:
	lda lo2Wave,x ; Wave form pulsation amplitude (lo byte)
	sta lo1Wave,x ; Wave form pulsation amplitude (lo byte)
	lda hi2Wave,x ; Wave form pulsation amplitude (hi byte)
	sta hi1Wave,x ; Wave form pulsation amplitude (hi byte)
noEffect2:
	lda rpNote,x ; readed note to play
	bne notZero
	lda pNote,x ; note to play
	sta rpNote,x ; readed note to play
	lda #$00
	sta pNote,x ; note to play
	ldy instrInd,x ; index of instrument data
	dec controlSwitch
	bne setVoice
notZero:
	sta pNote,x ; note to play
	tay
	lda frequencyHi,y
	sta $D401,x ; Voice 1: Frequency control (hi byte)
	sta freqHi1Eff,x ; Voice 1: Frequency control (hi byte) for effect 1
	sta freqPortHi,x ; Voice 1: Frequency control (hi byte) for portamento
	lda frequencyLo,y
	sta $D400,x ; Voice 1: Frequency control (lo byte)
	sta freqLo1Eff,x ; Voice 1: Frequency control (lo byte)
	sta freqPortLo,x ; Voice 1: Frequency control (lo byte) for portamento
	ldy instrInd,x ; index of instrument data
	lda instr1+6,y ; control register (ADS off)
	sta $D404,x ; Voice 1: Control registers
setVoice:
	lda instr1+1,y ; control register
	and controlSwitch ; change ADS
	sta $D404,x ; Voice 1: Control registers
	lda instr1+2,y ; read A/D value
	sta $D405,x ; Generator 1: Attack/Decay
	lda instr1+3,y ; read S/R value
	sta $D406,x ; Generator 1: Sustain/Release
	lda lo1Wave,x ; Wave form pulsation amplitude (lo byte)
	sta $D402,x ; Voice 1: Wave form pulsation amplitude (lo byte)
	lda hi1Wave,x ; Wave form pulsation amplitude (hi byte)
	sta $D403,x ; Voice 1: Wave form pulsation amplitude (hi byte)
	inc ptnInd,x ; next (saved) pattern value index
	ldy ptnInd,x ; load pattern value index
	lda ($FD),y ; read a pattern value
	cmp #$FF ; end of pattern
	bne noEndPattern
endPattern:
	lda #$00
	sta ptnInd,x ; set pattern value index to the beginning
	inc offsetTr1,x ; next track position (offset)
	ldy offsetTr1,x
	lda ($FB),y ; read next track pattern pointer
	cmp #$FF ; end of pattern ?
	bne noEndTrack
endTrack:
	lda #$00
	sta offsetTr1,x ; set track position to the beginning
	beq noEndPattern
noEndTrack:
	cmp #$FE ; end of song (no repeat)
	bne noEndPattern
	lda #$00
	sta songsNumber ; nothing song to play
	rts
	
noEndPattern:
	lda pNote,x ; note to play
	beq pulseTimbre
	ldy instrInd,x ; index of instrument data
	lda portaFlag,x ; portamento flag
	bne skipSetPort
	lda instr2+4,y ; read portamento flag for the instrument
	beq skipPortaInstr
	sta portaFlag,x ; store portamento flag
	lda instr2+3,y ; read portamento value
	sta portaVal,x ; portamento value
skipSetPort:
	jmp testMakePortamento
	
skipPortaInstr:
	lda instr1+5,y ; hi/lo value for arpeggio
	beq skipArpeggioInit
	jmp arpeggioInit
	
skipArpeggioInit:
	sta arpeggioFlag,x
	lda instr2+0,y ; read oscillating frequency value
	beq noOscEffect
	jmp vibratoInit
	
noOscEffect:
	sta vibratoFlag,x ; no vibrato
	jmp decCycleTimer
	
;=================================
; pulse-width timbre routine
;=================================

pulseTimbre:
	lda instr1+4,y ; Wave amplitude inc/dec value
	sta ampVar ; store for late use
	beq afterWaveShifting
	lda pulseDirFlag,x
	bne decWave
	clc
	lda lo1Wave,x ; Wave form pulsation amplitude (lo byte)
	adc ampVar
	sta lo1Wave,x ; Wave form pulsation amplitude (lo byte)
	sta $D402,x ; Voice 1: Wave form pulsation amplitude (lo byte)
	lda hi1Wave,x ; Wave form pulsation amplitude (hi byte)
	adc #$00
	sta hi1Wave,x ; Wave form pulsation amplitude (hi byte)
	sta $D403,x ; Voice 1: Wave form pulsation amplitude (hi byte)
	clc
	cmp #$0E
	bcc afterWaveShifting
	inc pulseDirFlag,x
	bne afterWaveShifting
decWave:
	lda lo1Wave,x ; Wave form pulsation amplitude (lo byte)
	sec
	sbc ampVar
	sta lo1Wave,x ; Wave form pulsation amplitude (lo byte)
	sta $D402,x ; Voice 1: Wave form pulsation amplitude (lo byte)
	lda hi1Wave,x ; Wave form pulsation amplitude (hi byte)
	sbc #$00
	sta hi1Wave,x ; Wave form pulsation amplitude (hi byte)
	sta $D403,x ; Voice 1: Wave form pulsation amplitude (hi byte)
	clc
	cmp #$08
	bcs afterWaveShifting
	dec pulseDirFlag,x

afterWaveShifting:
	lda arpeggioFlag,x ; test for made arpeggio
	beq testVibratoEff

;=================================
; make the arpeggio
;=================================

makeArpeggio:
	lda lowInd,x ; low index for arpeggio table
	asl
	tay
	lda pointerLo,y ; low pointer of arpeggio table
	sta adcArp+1
	lda pointerHi,y ; high pointer of arpeggio table
	sta adcArp+2
	lda actualCycle,x
	cmp limitCycle,x
	bne noResetCylce
	lda #$00
	sta actualCycle,x
noResetCylce:
	tay
	lda rpNote,x ; readed note to play
	clc
adcArp:
	adc TableArp0,y ; add the tone to note to play
	tay ; calciulate the new freq. to use
	lda frequencyLo,y
	sta $D400,x ; Voice 1: Frequency control (lo byte)
	lda frequencyHi,y
	sta $D401,x ; Voice 1: Frequency control (hi byte)
	inc actualCycle,x
	jmp decCycleTimer
testVibratoEff:
	lda vibratoFlag,x ; load vibrato flag
	bne vibratoEffect ; is to performe vibrato?
	jmp testMakePortamento
	
;=================================
; make the vibrato
;=================================
vibratoEffect:
	lda countVibFlag,x
	beq sbcVib
	cmp #$03
	bcc adcVib
	sec
	lda freqPortLo,x ; Voice 1: Frequency control (lo byte) for portamento
	sbc oscilFreq,x ; frequency of oscillation
	sta freqPortLo,x ; Voice 1: Frequency control (lo byte) for portamento
	sta $D400,x ; Voice 1: Frequency control (lo byte)
	lda freqPortHi,x ; Voice 1: Frequency control (hi byte) for portamento
	sbc #$00
	sta freqPortHi,x ; Voice 1: Frequency control (hi byte) for portamento
	sta $D401,x ; Voice 1: Frequency control (hi byte)
	dec actualVibCount,x
	bne exitVib1
	lda vibCount,x ; read stored value of count
	sta actualVibCount,x ; set to actual
	inc countVibFlag,x
	lda countVibFlag,x
	cmp #$05
	bcc exitVib1
	lda #$01
	sta countVibFlag,x
exitVib1:
	jmp decCycleTimer

sbcVib:
	sec
	lda freqPortLo,x ; Voice 1: Frequency control (lo byte) for portamento
	sbc oscilFreq,x ; frequency of oscillation
	sta freqPortLo,x ; Voice 1: Frequency control (lo byte) for portamento
	sta $D400,x ; Voice 1: Frequency control (lo byte)
	lda freqPortHi,x ; Voice 1: Frequency control (hi byte) for portamento
	sbc #$00
	sta freqPortHi,x ; Voice 1: Frequency control (hi byte) for portamento
	sta $D401,x ; Voice 1: Frequency control (hi byte)
	dec actualVibCount,x
	bne exitVib2
	lda vibCount,x ; read stored count value
	sta actualVibCount,x
	inc countVibFlag,x
exitVib2:
	jmp decCycleTimer

adcVib:
	clc
	lda freqPortLo,x ; Voice 1: Frequency control (lo byte) for portamento
	adc oscilFreq,x ; frequency of oscillation
	sta freqPortLo,x ; Voice 1: Frequency control (lo byte) for portamento
	sta $D400,x ; Voice 1: Frequency control (lo byte)
	lda freqPortHi,x ; Voice 1: Frequency control (hi byte) for portamento
	adc #$00
	sta freqPortHi,x ; Voice 1: Frequency control (hi byte) for portamento
	sta $D401,x ; Voice 1: Frequency control (hi byte)
	dec actualVibCount,x
	bne noEffect1
	lda vibCount,x ; read stored count value
	sta actualVibCount,x
	inc countVibFlag,x
	bne noEffect1
	jmp decCycleTimer
	
testMakePortamento:
	lda portaFlag,x ; portamento flag
	beq testEffect1
	cmp #$01 ; negative portamento
	beq negativePortamento
	cmp #$02 ; positive portamento
	beq positivePortamento
	cmp #$03
	beq negativePortamentoHI
	clc
	lda freqPortHi,x ; Voice 1: Frequency control (hi byte) for portamento
	adc portaVal,x ; add portamento value
	sta freqPortHi,x ; Voice 1: Frequency control (hi byte) for portamento
	sta $D401,x ; Voice 1: Frequency control (hi byte)
	jmp testEffect1

negativePortamento:
	clc
	lda freqPortLo,x ; Voice 1: Frequency control (lo byte) for portamento
	sbc portaVal,x ; sub portamento value
	sta freqPortLo,x ; Voice 1: Frequency control (lo byte) for portamento
	sta $D400,x ; Voice 1: Frequency control (lo byte)
	lda freqPortHi,x ; Voice 1: Frequency control (hi byte) for portamento
	sbc #$00
	sta freqPortHi,x ; Voice 1: Frequency control (hi byte) for portamento
	sta $D401,x ; Voice 1: Frequency control (hi byte)
	jmp testEffect1

negativePortamentoHI:
	sec
	lda freqPortHi,x ; Voice 1: Frequency control (hi byte) for portamento
	sbc portaVal,x ; sub portamento value
	sta freqPortHi,x ; Voice 1: Frequency control (hi byte) for portamento
	sta $D401,x ; Voice 1: Frequency control (hi byte)
	jmp testEffect1
	
positivePortamento:
	clc
	lda freqPortLo,x ; Voice 1: Frequency control (lo byte) for portamento
	adc portaVal,x ; add portamento value
	sta freqPortLo,x ; Voice 1: Frequency control (lo byte) for portamento
	sta $D400,x ; Voice 1: Frequency control (lo byte)
	lda freqPortHi,x ; Voice 1: Frequency control (hi byte) for portamento
	adc #$00
	sta freqPortHi,x ; Voice 1: Frequency control (hi byte) for portamento
	sta $D401,x ; Voice 1: Frequency control (hi byte)
	
testEffect1:
	ldy instrInd,x ; index of instrument data
	lda instr1+7,y ; load instrument effect
	and #$01 ; is effect 1?
	beq noEffect1
	jmp effect1
noEffect1:
	jmp decCycleTimer
;==================================
; Voice 1
;
; vibrato flag: 1= on
;
; arpeggio flag: 1= on
;
; portaFlag:
; 1: neg. port. HiLo
; 2: pos. port. HiLo
; 3: neg. port. Hi
; x: pos. port. Hi
;
; pulseDirFlag: 1 dec, 0 inc
;==================================
vibratoFlag: ; vibrato flag (1=on)
	!by $00
arpeggioFlag: ; 1= effect on
	!by $00
portaFlag:
	!by $00 ; portamento flag (voice 1)
cycleInt: ; internal cycle for effect 1
	!by $00
cycleEst: ; external cycle for effect 1
	!by $00
ptnInd:
ptnInd1:
	!by $06 ; pattern value index (voice 1)
pulseDirFlag:
	!by $00
;==================================
; Voice 2:
;==================================
	!by $00
	!by $00
	!by $00 ; portamento flag (voice 2)
	!by $00
	!by $00
ptnInd2:
	!by $06 ; pattern value index (voice 2)
	!by $00
;==================================
; Voice 3:
;==================================
	!by $00
	!by $00
	!by $00 ; portamento flag
	!by $00
	!by $00
ptnInd3:
	!by $00 ; pattern value index (voice 3)
	!by $01
;==================================
; Voice 1:
;==================================
portaVal:
	!by $00 ; portamento value (voice 1)
r0CE4: ; not used
	!by $00
noteLength:
	!by $3F ; note length duration (voice 1)
instrInd:
	!by $08 ; index of instrument data (voice 1)
lo1Wave:
	!by $BB ; Wave form pulsation amplitude (lo byte)
lo2Wave:
	!by $90 ; Wave form pulsation amplitude (lo byte)
hi1Wave:
	!by $02 ; Wave form pulsation amplitude (hi byte)

;==================================
; Voice 2:
;==================================
	!by $00 ; portamento value (Voice 2)
	!by $00
	!by $3F ; note length duration (voice 2)
	!by $08 ; index of instrument data (voice 2)
	!by $BB ; Wave form pulsation amplitude (lo byte)
	!by $90 ; Wave form pulsation amplitude (lo byte)
	!by $02 ; Wave form pulsation amplitude (hi byte)

;==================================
; Voice 3:
;==================================
	!by $00 ; portamento value (voice 3)
	!by $00
	!by $3F ; note length duration (voice 3)
	!by $20 ; index of instrument data (voice 3)
	!by $F0 ; Wave form pulsation amplitude (lo byte)
	!by $90 ; Wave form pulsation amplitude (lo byte)
	!by $0C ; Wave form pulsation amplitude (hi byte)
	
;==================================
; Voice 1:
;==================================
hi2Wave:
	!by $00 ; Wave form pulsation amplitude (hi byte)
lowInd: ; low index for freq. instrument effect
	!by $00
currentTr1: ; current track 1 position (base)
	!by <drillerMainTr1
	!by >drillerMainTr1
offsetTr1: ; current track 1 position (offset)
	!by $00
r0CFD: ; not used
	!by $00
actNoteLength =*
	!by $3C ; actual note length duration
	
;==================================
; Voice2:
;==================================
	!by $00
	!by $00 ; low index for freq. instrument effect
currentTr2: ; current track 2 position (base)
	!by <drillerMainTr2
	!by >drillerMainTr2
offsetTr2: ; current track 2 position (offset)
	!by $00
	!by $00
	!by $3C ; actual note length duration
;==================================
; Voice 3:
;==================================
	!by $06
	!by $00 ; low index for freq. instrument effect
currentTr3: ; current track 3 position (base)
	!by <drillerMainTr3
	!by >drillerMainTr3
offsetTr3: ; current track 3 position (offset)
	!by $02
	!by $00
	!by $3C ; actual note length duration
r0D0D:
	!by $00
	!by $00
songsNumber =*
	!by $AB ; 0=nothing 1=first, $AB current ?
speed:
	!by $03 ; cycle timer (cylce for having 1 note tick)
ampVar: ; wave amplitude variation
	!by $A0
actSpeed:
	!by $00 ; actual cycle timer (for having 1 note tick)
controlSwitch: ; set ON/OFF the ADS
	!by $FE
;==================================
; Voice 1
;==================================
freqPortLo: ; frequency of portamento lo
	!by $47 ; Voice 1: Frequency control (lo byte)
freqLo1Eff: ; freq lo for instrument effect 1 ?
	!by $47 ; Voice 1: Frequency control (lo byte)
freqHi1Eff: ; freq hi for instrument effect 1
	!by $06 ; Voice 1: Frequency control (hi byte)
rpNote =*
	!by $1F ; Voice 1: readed note to play
freqPortHi: ; frequency of portamento Hi
	!by $06 ; Voice 1: Frequency control (hi byte)
limitCycle: ; limit of cycle for arpeggio
	!by $00
actualCycle: ; actual cycle for arpeggio
	!by $00
;==================================
; Voice 2
;==================================
!by $23 ; frequency of portamento lo
!by $23 ; freq lo for instrument effect 1 ?
!by $03 ; freq hi for instrument effect 1
!by $13 ; Voice 2: readed note to play
!by $03 ; frequency of portamento Hi
!by $00 ; limit of cycle for arpeggio
!by $00 ; actual cycle for arpeggio
;==================================
; Voice 3
;==================================
!by $00 ; frequency of portamento lo
!by $00 ; freq lo for instrument effect 1 ?
!by $00 ; freq hi for instrument effect 1
!by $00 ; Voice 3: readed note to play
!by $00 ; frequency of portamento Hi
!by $00
!by $00
;==================================
; Voice 1
;==================================
countVibFlag: ; counter flag: use for add/sbc of vibrato
	!by $00
oscilFreq: ; frequency of oscillator (for vibrato)
	!by $00
vibCount: ; value of vibrato count
	!by $00
actualVibCount: ; actual value of vibrato count intensity
	!by $00
r0D2D:
	!by $00
	!by $00
pNote =*
	!by $1F ; note to play
;==================================
; Voice 2
;==================================
	!by $00
	!by $00
	!by $00
	!by $00
	!by $00
	!by $00
	!by $13 ; note to play
;==================================
; Voice 3
;==================================
	!by $00
	!by $00
	!by $00
	!by $00
	!by $00
	!by $00
	!by $00 ; note to play
;==================================
; Voice 1
;==================================
dur4Eff:
	!by $02 ; duration of effect 4 switch
	!by $00
	!by $00
	!by $00
	!by $00
	!by $00
	!by $00
;==================================
; Voice 2
;==================================
	!by $02
	!by $00
	!by $00
	!by $00
	!by $00
	!by $00
	!by $00
;==================================
; Voice 3
;==================================
	!by $02
	!by $00
	!by $00
	!by $00
	!by $00
	!by $00
	!by $00
; freq. low
frequencyLo:
	!by $0C, $1C, $2D, $3E ; C-0 C#-0 D-0 D#-0
	!by $51, $66, $7B, $91
	!by $A9, $C3, $DD, $FA
	!by $18, $38, $5A, $7D
	!by $A3, $CC, $F6, $23
	!by $53, $86, $BB, $F4
	!by $30, $70, $B4, $FB
	!by $47, $98, $ED, $47
	!by $A7, $0C, $77, $E9
	!by $61, $E1, $68, $F7
	!by $8F, $30, $DA, $8F
	!by $4E, $18, $EF, $D2
	!by $C3, $C3, $D1, $EF
	!by $1F, $60, $B5, $1E
	!by $9C, $31, $DF, $A5
	!by $87, $86, $A2, $DF
	!by $3E, $C1, $6B, $3C
	!by $39, $63, $BE, $4B
	!by $0F, $0C, $45, $BF
	!by $7D, $83, $D6, $79
	!by $73, $C7, $7C, $97
	!by $1E, $18, $8B, $7E
	!by $FA, $06, $AC, $F3
	!by $E6, $8F, $F8, $2E
frequencyHi:
	!by $01, $01, $01, $01
	!by $01, $01, $01, $01
	!by $01, $01, $01, $01
	!by $02, $02, $02, $02
	!by $02, $02, $02, $03
	!by $03, $03, $03, $03
	!by $04, $04, $04, $04
	!by $05, $05, $05, $06
	!by $06, $07, $07, $07
	!by $08, $08, $09, $09
	!by $0A, $0B, $0B, $0C
	!by $0D, $0E, $0E, $0F
	!by $10, $11, $12, $13
	!by $15, $16, $17, $19
	!by $1A, $1C, $1D, $1F
	!by $21, $23, $25, $27
	!by $2A, $2C, $2F, $32
	!by $35, $38, $3B, $3F
	!by $43, $47, $4B, $4F
	!by $54, $59, $5E, $64
	!by $6A, $70, $77, $7E
	!by $86, $8E, $96, $9F
	!by $A8, $B3, $BD, $C8
	!by $D4, $E1, $EE, $FD
	
activeCiaIrq =*
	sei
	lda #<serviceIrq
	sta $0314 ; Vector: Hardware Interrupt (IRQ)
	lda #>serviceIrq
	sta $0315 ; Vector: Hardware Interrupt (IRQ)
	ldx #$00
	stx $DC0E ; Control register A of CIA #1
	inx
	stx $D01A ; IRQ mask register
	cli
	rts
serviceIrq =*
	lda #$01
	sta $D019 ; Interrupt indicator register
	lda #$82
	sta $D012 ; Reading/Writing IRQ balance value
	lda #$1B

;Definition of the used
;frequencies
	sta $D011 ; VIC control register
	lda #$01
	sta $D020 ; Border color
	jsr serviceMusic
	dec $D020 ; Border color
	jmp $EA31 ; Default hardware interrupt (IRQ)
serviceMusic =*
	ldx #$00 ; Voice 1
	jsr makeMusic
	ldx #$07 ; Voice 2
	jsr makeMusic
	ldx #$0E ; Voice 3
	jsr makeMusic
	rts
	
	!text "(C)1987 MATT GRAY"

	;=================================
	; Init the arpeggio routine
	;=================================
arpeggioInit:
	pha
	and #$0F
	sta lowInd,x
	pla
	and #$F0
	lsr
	lsr
	lsr
	lsr
	sta limitCycle,x
	lda #$00
	sta actualCycle,x
	lda #$01 ; made on arpeggio
	sta arpeggioFlag,x
	lda #$00
	sta vibratoFlag,x ; no vibrato
	jmp decCycleTimer
	
;=================================
; Init the vibrato routine
;=================================
vibratoInit:
	sta oscilFreq,x ; oscillating frequency value
	lda instr2+1,y ; read the count intensity for vibrato
	sta vibCount,x ; save count value
	sta actualVibCount,x
	lda #$00
	sta arpeggioFlag,x ; make arpeggio off
	sta countVibFlag,x
	lda #$01
	sta vibratoFlag,x ; make vibrato
	jmp decCycleTimer
;=================================
; instruments part 1
; 0: Hi/Lo of Wave form amplitude
; 1: Control register
; 2: A/D value
; 3: S/R value
; 4: Wave amplitude inc/dec value
; 5: hi/lo value for arpeggio
; 6: Control register
; 7: instrument effect
; 1: a frequency effect
; 2: a pulse wave effect
; 4: switch between waveform
;=================================
instr1 =*
	!by $00 ; Wave form amplitude (Hi/Lo)
	!by $81 ; Control: Noise, ADS on
	!by $0A ; A/D
	!by $00 ; S/R
	!by $00
	!by $00
	!by $80 ; Control: Noise, ADS off
	!by $01 ; instrument effect
	!by $90
	!by $41
	!by $FE
	!by $0D
	!by $25
	!by $00
; The first table of instruments definition
!by $40
!by $02
!by $00
!by $81
!by $FD
!by $00
!by $00
!by $00
!by $80
!by $00
!by $30
!by $41
!by $0E
!by $00
!by $30
!by $00
!by $40
!by $02
!by $96
!by $41
!by $0E
!by $00
!by $A0
!by $00
!by $40
!by $02
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $32
!by $41
!by $00
!by $40
!by $F0
!by $00
!by $40
!by $02
!by $00
!by $81
!by $08
!by $00
!by $00
!by $00
!by $80
!by $01
!by $00
!by $11
!by $0D
!by $00
!by $00
!by $00
!by $10
!by $00
!by $90
!by $41
!by $0E
!by $00
!by $25
!by $00
!by $40
!by $02
!by $2E
!by $43
!by $00
!by $60
!by $F5
!by $00
!by $40
!by $04
!by $70
!by $41
!by $0A
!by $00
!by $40
!by $00
!by $40
!by $02
!by $00
!by $15
!by $03
!by $00
!by $00
!by $20
!by $14
!by $04
!by $40
!by $41
!by $00
!by $90
!by $01
!by $00
!by $40
!by $00
!by $00
!by $15
!by $EE
!by $00
!by $00
!by $00
!by $14
!by $00
!by $98
!by $41
!by $09
!by $00
!by $00
!by $00
!by $40
!by $01
!by $21
!by $41
!by $0A
!by $00
!by $30
!by $00
!by $40
!by $06
!by $21
!by $41
!by $0A
!by $00
!by $30
!by $00
!by $40
!by $06
!by $31
!by $41
!by $0E
!by $00
!by $10
!by $00
!by $40
!by $02
!by $23
!by $41
!by $00
!by $A0
!by $50
!by $00
!by $40
!by $00
!by $91
!by $41
!by $0A
!by $00
!by $30
!by $00
!by $40
!by $06
!by $F1
!by $41
!by $0C
!by $00
!by $40
!by $00
!by $40
!by $06
;=================================
; instruments part 2
; 0: oscillating frequency value (for vibrato)
; 1: length of vibrato intensity (for vibrato)
; 2: Control register for effect 4
; 3: portamento value |
; 4: portamento flag (1=on) | portamento for the instrument
; 5: duration cycle for effect 1
; 6: not used
; 7: not used
;=================================
instr2 =*
!by $00
!by $00
!by $11 ; Control: ^, on ADS
!by $00 ; portamento value
!by $00 ; portamento flag
!by $03
!by $00 ; not used
!by $00 ; not used
!by $00
!by $00
!by $81
!by $00
!by $00
!by $00
!by $00
!by $00
!by $06
!by $50
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $30
!by $02
!by $81
!by $00
!by $00
!by $00
!by $00
!by $00
!by $40
!by $02
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $81
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $11
!by $41
!by $01
!by $01
!by $00
!by $00
!by $50
!by $02
;The second table for instruments definition
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $81
!by $00
!by $00
!by $00
!by $00
!by $00
!by $20
!by $02
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $81
!by $00
!by $00
!by $00
!by $00
!by $00
!by $40
!by $02
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $41
!by $F0
!by $01
!by $01
!by $00
!by $00
!by $10
!by $02
!by $43
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $A0
!by $02
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $60
!by $02
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $00
!by $43
!by $00
!by $00
!by $00
!by $00
!by $00
!by $0A
!by $02
!by $43
!by $00
!by $00
!by $00
!by $00
!by $00
effect1:
	lda freqHi1Eff,x ; Voice 1: Frequency control (hi byte) for effect 1
	beq noDec
	dec freqHi1Eff,x ; Voice 1: Frequency control (hi byte) for effect 1
noDec:
	lda cycleInt,x
	beq jmpTestEndCycle
	dec cycleInt,x
	lda #$81 ; noise + ADS
	sta $D404,x ; Voice 1: Control registers
	lda freqHi1Eff,x ; Voice 1: Frequency control (hi byte) for effect 1
	eor #$23
	sta $D401,x ; Voice 1: Frequency control (hi byte)
	jmp decCycleTimer
	
jmpTestEndCycle:
	jmp testEndCycle
	
changeFreq:
	lda freqPortHi,x ; Voice 1: Frequency control (hi byte) for portamento
	sta $D401,x ; Voice 1: Frequency control (hi byte)
	sta freqHi1Eff,x ; Voice 1: Frequency control (hi byte) for effect 1
	lda instr2+2,y ; control register
	sta $D404,x ; Voice 1: Control registers
	jmp decCycleTimer
testEndCycle:
	lda cycleEst,x
	cmp instr2+5,y ; duration cycle for effect 1
	beq resetCycle
	inc cycleInt,x
	inc cycleEst,x
	bne changeFreq
resetCycle:
	lda #$00
	sta cycleEst,x
	sta cycleInt,x
	beq changeFreq
songSpeed =* ; cycle for having note tick (for each songs)
!by $00
!by $03
!by $03
; $FF: repeat the track
; $FE: end of music
drillerMainTr1:
	!by $01, $01, $07, $09
	!by $09, $09, $01, $07
	!by $07, $0F, $0F, $0F
	!by $0F, $0F, $0F, $03
	!by $03, $0F, $0F, $13
; The definition of the
; patterns used by each
; traks are from here
!by $13, $0F, $13, $0F
!by $13, $0F, $13, $0F
!by $13, $0F, $0F, $0F
!by $0F, $0F, $0F, $0F
!by $0F, $0F, $0F, $0F
!by $0F, $0F, $0F, $0F
!by $0F, $0F, $0F, $0F
!by $0F, $0F, $0F, $0F
!by $0F, $0F, $1B, $1D
!by $1E, $0F, $1B, $1D
!by $1E, $0F, $1B, $1D
!by $1E, $12, $12, $12
!by $12, $24, $24, $21
!by $21, $24, $24, $21
!by $21, $24, $24, $21
!by $21, $24, $24, $21
!by $21, $24, $24, $21
!by $21, $24, $24, $21
!by $21, $24, $24, $21
!by $21, $24, $24, $21
!by $21, $24, $24, $21
!by $21, $24, $24, $21
!by $21, $08, $08, $28
!by $00, $00, $00, $00
!by $00, $FF

drillerMainTr2:
	!by $03, $03, $08, $0A
	!by $0D, $0D, $0D, $0D
	!by $08, $07, $0E, $0E
	!by $0E, $0E, $0E, $0E
	!by $0E, $0E, $05, $12
	!by $12, $12, $12, $14
	!by $15, $14, $15, $14
	!by $15, $14, $15, $08
	!by $17, $17, $17, $17
	!by $17, $17, $17, $17
	!by $17, $17, $17, $17
	!by $07, $07, $1F, $1F
	!by $1F, $1F, $07, $07
	!by $00, $00, $25, $25
	!by $26, $25, $27, $27
	!by $27, $27, $27, $27
	!by $27, $27, $06, $06
	!by $06, $06, $06, $06
	!by $06, $06, $06, $06
	!by $28, $00, $00, $00
	!by $00, $FF
	
drillerMainTr3:
	!by $00, $00, $00, $00
	!by $04, $06, $06, $0C
	!by $0B, $0C, $0B, $0C
	!by $0B, $06, $06, $06
	!by $06, $06, $06, $06
	!by $06, $06, $06, $06
	!by $06, $06, $06, $0F
	!by $0F, $10, $11, $0E
	!by $0E, $0E, $0E, $0E
	!by $0E, $0E, $0E, $0E
	!by $0E, $0E, $0E, $16
	!by $07, $07, $07, $18
	!by $19, $19, $1A, $1A
	!by $08, $08, $1C, $08
	!by $08, $23, $23, $22
	!by $22, $23, $23, $22
	!by $22, $23, $23, $22
	!by $22, $23, $23, $22
	!by $22, $23, $23, $22
	!by $22, $23, $23, $22
	!by $22, $23, $23, $22
	!by $22, $23, $23, $22
	!by $22, $23, $23, $22
	!by $22, $23, $23, $22
	!by $22, $07, $07, $0F
	!by $0F, $0F, $0F, $29
	!by $00, $00, $00, $00
	!by $FF
;=================================
; pattern data
;=================================
; format:
; 00 : release gate
; xx : note xx
; $FA nn : select instrument nn
; $FB mm : negative portamento (mm)
; $FC kk : positive portamento (kk)
; $FD kk : duration kk
; $FF : end of pattern

pnt00 =*
	!by $FD, $3F
	!by $FA, $04
	!by $00
	!by $FF
pnt01 =*
	!by $FA, $01
	!by $FD, $3F
	!by $23, $1F, $22, $1E
	!by $FF
pnt03 =*
	!by $FA, $01
	!by $FD, $3F
	!by $17, $13, $16, $12
	!by $FF
pnt02 =*
	!by $FD, $0F
	!by $FA, $04
	!by $00
	!by $FF
pnt04 =*
	!by $FA, $02
	!by $FD, $7F
	!by $25, $25
	!by $FF
pnt05 =*
	!by $FA, $0E
	!by $FD, $3F
	!by $2F, $2B, $2E
	!by $FC, $20
	!by $2A
	!by $FF
pnt06 =*
	!by $FA, $06
	!by $FD, $01
	!by $42, $3B, $3B, $42
	!by $3B, $3B, $43, $3B
	!by $42, $3B, $3B, $42
	!by $3B, $3B, $43, $3B
	!by $42, $3B, $3B, $42
	!by $3B, $3B, $43, $3B
	!by $42, $3B, $3B, $42
	!by $3B, $3B, $43, $3B
	!by $FF
pnt07 =*
	!by $FA, $01
	!by $FD, $7F
	!by $23
	!by $FF
pnt08 =*
	!by $FA, $01
	!by $FD, $7F
	!by $17, $00
	!by $FF
pnt09 =*
	!by $FA, $09
	!by $FD, $1F
	!by $17, $13, $12, $0F
	!by $FF
pnt0A =*
	!by $FA, $08
	!by $FD, $0F
	!by $3E, $39
	!by $FD, $1F
	!by $3B
	!by $FD, $0F
	!by $3D, $3B
	!by $FD, $1F
	!by $3A
	!by $FD, $7F
	!by $FB, $01
	!by $2F
	!by $FF
pnt0B =*
	!by $FA, $06
	!by $FD, $01
; All the patterns of
; notes start from here
	!by $3D, $36, $36, $3D
	!by $36, $36, $3E, $36
	!by $3D, $36, $36, $3D
	!by $36, $36, $3E, $36
	!by $3A, $33, $33, $3A
	!by $33, $33, $3B, $33
	!by $3A, $33, $33, $3A
	!by $33, $33, $3B, $33
	!by $FF
pnt0C =*
	!by $FA, $06
	!by $FD, $01
	!by $42, $3B, $3B, $42
	!by $3B, $3B, $43, $3B
	!by $42, $3B, $3B, $42
	!by $3B, $3B, $43, $3B
	!by $3E, $37, $37, $3E
	!by $37, $37, $3F, $37
	!by $3E, $37, $37, $3E
	!by $37, $37, $3F, $37
	!by $FF
pnt0D =*
	!by $FA, $0A
	!by $FD, $01
	!by $3B, $3A, $39, $38
	!by $39, $3A, $3B, $3A
	!by $39, $38, $39, $3A
	!by $3B, $3A, $39, $38
	!by $39, $3A, $3B, $3A
	!by $39, $38, $39, $3A
	!by $3B, $3A, $39, $38
	!by $39, $3A, $3B, $3A
	!by $FF
pnt0E =*
	!by $FA, $07
	!by $FD, $01
	!by $2D
	!by $FD, $03
	!by $2D
	!by $FD, $0D
	!by $2D
	!by $FD, $03
	!by $2D
	!by $FD, $07
	!by $FA, $00
	!by $2D
	!by $FA, $07
	!by $FD, $01
	!by $2D
	!by $FD, $03
	!by $2D
	!by $FD, $0D
	!by $2D
	!by $FD, $03
	!by $2D
	!by $FD, $07
	!by $FA, $00
	!by $2D
	!by $FF
pnt0F =*
	!by $FA, $0B
	!by $FD, $01
	!by $23, $23, $23, $23
	!by $23, $23, $23, $23
	!by $23, $23, $23, $23
	!by $23, $23, $23, $23
	!by $23, $23, $23, $23
	!by $23, $23, $23, $23
	!by $23, $23, $23, $23
	!by $23, $23, $23, $23
	!by $FF
pnt10 =*
	!by $FA, $0B
	!by $FD, $01
	!by $22, $22, $22, $22
	!by $22, $22, $22, $22
	!by $22, $22, $22, $22
	!by $22, $22, $22, $22
	!by $22, $22, $22, $22
	!by $22, $22, $22, $22
	!by $22, $22, $22, $22
	!by $22, $22, $22, $22
	!by $FF
pnt11 =*
	!by $FA, $0B
	!by $FD, $01
	!by $25, $25, $25, $25
	!by $25, $25, $25, $25
	!by $25, $25, $25, $25
	!by $25, $25, $25, $25
	!by $25, $25, $25, $25
	!by $25, $25, $25, $25
	!by $25, $25, $25, $25
	!by $25, $25, $25, $25
	!by $FF
pnt12 =*
	!by $FA, $0A
	!by $FD, $01
	!by $3B, $37, $36, $34
	!by $3B, $37, $36, $34
	!by $3B, $37, $36, $34
	!by $3B, $37, $36, $34
	!by $3B, $37, $36, $34
	!by $3B, $37, $36, $34
	!by $3B, $37, $36, $34
	!by $3B, $37, $36, $34
	!by $FF
pnt13 =*
	!by $FA, $0B
	!by $FD, $01
	!by $1F, $1F, $1F, $1F
	!by $1F, $1F, $1F, $1F
	!by $1F, $1F, $1F, $1F
	!by $1F, $1F, $1F, $1F
	!by $1F, $1F, $1F, $1F
	!by $1F, $1F, $1F, $1F
	!by $1F, $1F, $1F, $1F
	!by $1F, $1F, $1F, $1F
	!by $FF
pnt14 =*
	!by $FA, $06
	!by $FD, $01
	!by $3F, $3B, $36, $3F
	!by $3B, $36, $3F, $3B
	!by $3F, $3B, $36, $3F
	!by $3B, $36, $3F, $3B
	!by $3F, $3B, $36, $3F
	!by $3B, $36, $3F, $3B
	!by $3F, $3B, $36, $3F
	!by $3B, $36, $3F, $3B
	!by $FF
pnt15 =*
	!by $FA, $06
	!by $FD, $01
	!by $3E, $3B, $37, $3E
	!by $3B, $37, $3E, $3B
	!by $3E, $3B, $37, $3E
	!by $3B, $37, $3E, $3B
	!by $3E, $3B, $37, $3E
	!by $3B, $37, $3E, $3B
	!by $3E, $3B, $37, $3E
	!by $3B, $37, $3E, $3B
	!by $FF
pnt16 =*
	!by $FA, $0D
	!by $FD, $1F
	!by $37, $36, $39, $37
	!by $36, $2F, $2F, $32
	!by $FF
pnt17 =*
	!by $FA, $10
	!by $FD, $01
	!by $23, $23, $2A, $2A
	!by $28, $28, $2A, $2A
	!by $26, $26, $2A, $2A
	!by $28, $28, $2A, $2A
	!by $23, $23, $2A, $2A
	!by $28, $28, $2A, $2A
	!by $26, $26, $2A, $2A
	!by $28, $28, $2A, $2A
	!by $FF
pnt18 =*
	!by $FA, $13
	!by $FD, $07
	!by $FC, $37, $45
	!by $FD, $2F
	!by $47
	!by $FD, $07
	!by $FB, $7F, $47
	!by $FD, $37
	!by $42
	!by $FD, $07
	!by $FB, $80
	!by $42
	!by $FF
pnt19 =*
	!by $FA, $13
	!by $FD, $1F
	!by $3B
	!by $FD, $0F
	!by $39, $37
	!by $FD, $3F
	!by $36
	!by $FF
pnt1A =*
	!by $FA, $13
	!by $FD, $1F
	!by $34
	!by $FD, $0F
	!by $32, $31
	!by $FD, $3F
	!by $2F
	!by $FF
pnt1B =*
	!by $FA, $0B
	!by $FD, $01
	!by $1B, $1B, $1B, $1B
	!by $1B, $1B, $1B, $1B
	!by $1B, $1B, $1B, $1B
	!by $1B, $1B, $1B, $1B
	!by $1B, $1B, $1B, $1B
	!by $1B, $1B, $1B, $1B
	!by $1B, $1B, $1B, $1B
	!by $1B, $1B, $1B, $1B
	!by $FF
pnt1C =*
	!by $FA, $01
	!by $FD, $1F
	!by $3B
	!by $FD, $0F
	!by $3A, $36
	!by $FD, $2F
	!by $36
	!by $FD, $0F
	!by $38
	!by $FD, $1F
	!by $38, $2F, $31
	!by $FD, $0F
	!by $33, $34
	!by $FD, $7F
	!by $36, $36
	!by $FF
pnt1D =*
	!by $FA, $0B
	!by $FD, $01
	!by $1C, $1C, $1C, $1C
	!by $1C, $1C, $1C, $1C
	!by $1C, $1C, $1C, $1C
	!by $1C, $1C, $1C, $1C
	!by $1C, $1C, $1C, $1C
	!by $1C, $1C, $1C, $1C
	!by $1C, $1C, $1C, $1C
	!by $1C, $1C, $1C, $1C
	!by $FF
pnt1E =*
	!by $FA, $0B
	!by $FD, $01
	!by $1E, $1E, $1E, $1E
	!by $1E, $1E, $1E, $1E
	!by $1E, $1E, $1E, $1E
	!by $1E, $1E, $1E, $1E
	!by $1E, $1E, $1E, $1E
	!by $1E, $1E, $1E, $1E
	!by $1E, $1E, $1E, $1E
	!by $1E, $1E, $1E, $1E
	!by $FF
pnt1F =*
	!by $FA, $09
	!by $FD, $3F
	!by $23, $1B, $1C, $1E
	!by $FF
pnt20 =*
	!by $FA, $01
	!by $FD, $7F
	!by $17, $17
	!by $FF
	!by $21, $26
	!by $FD, $11
	!by $28
	!by $FF
pnt21 =*
	!by $FA, $15
	!by $FD, $01
	!by $1F, $1F
	!by $FD, $03
	!by $1F
	!by $FA, $0F
	!by $FD, $01
	!by $2E, $27
	!by $FA, $15
	!by $1F
	!by $FD, $03
	!by $1F
	!by $FD, $01
	!by $1F
	!by $FD, $03
	!by $1F
	!by $FD, $01
	!by $FA, $0F
	!by $2F
	!by $FA, $15
	!by $1A, $1D, $1F
	!by $FF
pnt22 =*
	!by $FA, $09
	!by $FD, $01
	!by $13, $13
	!by $FD, $03
	!by $13
	!by $FD, $01
	!by $FA, $00
	!by $2E, $27
	!by $FA, $09
	!by $13
	!by $FD, $03
	!by $13
	!by $FD, $01
	!by $13
	!by $FD, $03
	!by $13
	!by $FD, $01
	!by $13, $10, $11, $13
	!by $FF
pnt23 =*
	!by $FA, $09
	!by $FD, $01
	!by $17, $17
	!by $FD, $03
	!by $17
	!by $FD, $01
	!by $FA, $00
	!by $2E, $27
	!by $FA, $09
	!by $17
	!by $FD, $03
	!by $17
	!by $FD, $01
	!by $17
	!by $FD, $03
	!by $17
	!by $FD, $01
	!by $17, $12, $15, $17
	!by $FF
pnt24 =*
	!by $FA, $15
	!by $FD, $01
	!by $23, $23
	!by $FD, $03
	!by $23
	!by $FA, $0F
	!by $FD, $01
	!by $2E, $27
	!by $FA, $15
	!by $23
	!by $FD, $03
	!by $23
	!by $FD, $01
	!by $23
	!by $FD, $03
	!by $23
	!by $FD, $01
	!by $FA, $0F
	!by $2F
	!by $FA, $15
	!by $1E, $21, $23
	!by $FF
pnt25 =*
	!by $FA, $0A
	!by $FD, $39
	!by $47
	!by $FD, $01
	!by $46, $45, $44
	!by $FD, $39
	!by $43
	!by $FD, $01
	!by $44, $45, $46
	!by $FF
pnt26 =*
	!by $FA, $12
	!by $FD, $3F
	!by $3B, $43, $42, $3E
	!by $3B, $37, $36, $2F
	!by $FF
pnt27 =*
	!by $FA, $0C
	!by $FD, $01
	!by $31, $3D, $49, $3D
	!by $31, $3D, $49, $3D
	!by $FF
pnt28 =*
	!by $FA, $01
	!by $FD, $7F
	!by $17, $00, $00, $00
	!by $FF
pnt29 =*
	!by $FA, $01
	!by $FD, $7F
	!by $23, $00, $00, $00
	!by $FF
;=========================
; arpeggio pointers table
;=========================
pointerLo:
	!by <TableArp0
r157B:
pointerHi:
	!by >TableArp0
TableArp0:
	!by $00, $0C, $18
;pointers to the patterns
;low pointers
patptl =*
	!by <pnt00
	!by <pnt01
	!by <pnt02
	!by <pnt03
	!by <pnt04
	!by <pnt05
	!by <pnt06
	!by <pnt07
	!by <pnt08
	!by <pnt09
	!by <pnt0A
	!by <pnt0B
	!by <pnt0C
	!by <pnt0D
	!by <pnt0E
	!by <pnt0F
	!by <pnt10
	!by <pnt11
	!by <pnt12
	!by <pnt13
	!by <pnt14
; Here goes the definition
; of the arpeggio
	!by <pnt15
	!by <pnt16
	!by <pnt17
	!by <pnt18
	!by <pnt19
	!by <pnt1A
	!by <pnt1B
	!by <pnt1C
	!by <pnt1D
	!by <pnt1E
	!by <pnt1F
	!by <pnt20
	!by <pnt21
	!by <pnt22
	!by <pnt23
	!by <pnt24
	!by <pnt25
	!by <pnt26
	!by <pnt27
	!by <pnt28
	!by <pnt29
;high pointers
patpth =*
	!by >pnt00
	!by >pnt01
	!by >pnt02
	!by >pnt03
	!by >pnt04
	!by >pnt05
	!by >pnt06
	!by >pnt07
	!by >pnt08
	!by >pnt09
	!by >pnt0A
	!by >pnt0B
	!by >pnt0C
	!by >pnt0D
	!by >pnt0E
	!by >pnt0F
	!by >pnt10
	!by >pnt11
	!by >pnt12
	!by >pnt13
	!by >pnt14
	!by >pnt15
	!by >pnt16
	!by >pnt17
	!by >pnt18
	!by >pnt19
	!by >pnt1A
	!by >pnt1B
	!by >pnt1C
	!by >pnt1D
	!by >pnt1E
	!by >pnt1F
	!by >pnt20
	!by >pnt21
	!by >pnt22
	!by >pnt23
	!by >pnt24
	!by >pnt25
	!by >pnt26
	!by >pnt27
	!by >pnt28
	!by >pnt29
songs =*
ltr1:
	!by $00
	!by <drillerMainTr1
htr1:
	!by $00
	!by >drillerMainTr1
ltr2:
	!by $00
	!by <drillerMainTr2
htr2:
	!by $00
	!by >drillerMainTr2
ltr3:
	!by $00
	!by <drillerMainTr3
htr3:

;The tables of songs
;with pointers to the
;tracks for each voices

	!by $00
	!by >drillerMainTr3
	!by $00
	
initsongs =*
	lda #$01 ; first song
	sta songsNumber
	rts
	
}