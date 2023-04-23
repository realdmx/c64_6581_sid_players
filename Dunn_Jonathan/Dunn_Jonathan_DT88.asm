
; Converted to ACME by dmx87

	!to "Dunn_Jonathan_DT88.sid",plain

;----------------------------------------------------------------------------;
;																			 ;
;						  D.THOMPSON '88 Soundtrack							 ;
;																			 ;
;			Orchestration, blood, sweat & tears by Jonathan Dunn			 ;
;																			 ;
;				Oh wow (paroxism!) programming by Paul Hughes				 ;
;																			 ;
;	  (c) Copyright Ocean Software / Imagine Software / Paul Hughes	 1988	 ;
;																			 ;
; Ported to DASM 10/11/2004 by Paulie.										 ;
;----------------------------------------------------------------------------;

    * = $0000

	!text "PSID"
	!be16 2							; version 2
	!be16 $7c						; data offset
	!be16 0							; Load (0 = auto)
	!be16 INIT						; Init
	!be16 PLAY						; Play
	!be16 6							; num songs
	!be16 4							; first song
	!word 0
	!word 0
-	!text "Daley Thompson '88"
	!fill 32 - (* - -)
-	!text "Jonathan Dunn"
	!fill 32 - (* - -)
-	!text "1988 Ocean"
	!fill 32 - (* - -)
	!be16 $0014							; v2 flags
	!be16 0							; Start page, page length (reloc)
	!be16 0							; Reserved
	!word $1000

!pseudopc $1000 {

NUM_OF_TUNES   = 6
TOPLAY		   = 6-1
ALLOCATION	   = 5*1024+512
DRIVER_LEN	   = DRIVER_END-DRIVER_START
DATA_LEN	   = DATA_END-DATA_START
PROGRAM_LEN	   = PROG_END-DRIVER_START
TOGO		   = ALLOCATION-PROGRAM_LEN

BARS		   = $44				  ; Bars to skip
BARLENGTH	   = 96					  ; Beats per bar

;
; ZP - All zero page usage for speed, although only the PC's r=ire ZP
;

ZP			   = $20

IA			   = ZP
IX			   = ZP+1
IY			   = ZP+2
PC_A		   = ZP+4
PC_B		   = ZP+6
PC_C		   = ZP+8
READ_A		   = ZP+10
READ_B		   = ZP+12
READ_C		   = ZP+14
FROM		   = ZP+16

FRM			   = ZP+18				  ; USED BY TESTER
TO			   = ZP+20
TO2			   = ZP+22
SA			   = ZP+24
SX			   = ZP+25
SY			   = ZP+26

; ------------------------------------

AT			   = $80
FIN			   = 0

;
; To play the music	 " JSR REFRESH " every frame
;
; To select a tune " LDX #TUNE_NUMBER "
;				   " JSR TUNE		  "

;TUNE_NUMBER:- Load X with...

;			   0 = DISQUALIFIED
;			   1 = SUCCESS
;			   2 = FAIL
;			   3 = NEXT EVENT
;			   4 = TITLE MUSIC
;			   5 = QUALIFY

DRIVER_START   = *

;
; TUNE - Initialises a tune, entered with tune number in X
;

PLAY           JMP REFRESH
INIT           TAX                      ; tune number
TUNE		   LDY #0
			   LDA #0
CLR_VAR		   STA VAR_START,Y			; Reset all variables to zero
			   INY
			   CPY #VAR_END-VAR_START
			   BNE CLR_VAR

			   LDY #$16
CLRSID		   LDA #8					; Set SID test bit so there's no
			   STA SID,Y
			   LDA #0					; grotty clicking
			   STA SID,Y
			   DEY
			   BPL CLRSID
			   STY MODEBYTE_A			; Reset any non-zero variables ($FF)
			   STY MODEBYTE_B
			   STY MODEBYTE_C
			   LDY #1
			   STY DURCOUNT_A			; $01
			   STY DURCOUNT_B
			   STY DURCOUNT_C
			   STY DURATIONON_A
			   STY DURATIONON_B
			   STY DURATIONON_C
			   STY MFL_A
			   STY MFL_B
			   STY MFL_C
			   STY MOD_OFF_A
			   STY MOD_OFF_B
			   STY MOD_OFF_C

			   LDA TUNETABLE_A_L,X		; Initialise the addresses
			   STA PC_A
			   LDA TUNETABLE_A_H,X		; of the tunes into each
			   STA PC_A+1
			   LDA TUNETABLE_B_L,X		; individual program counter
			   STA PC_B
			   LDA TUNETABLE_B_H,X
			   STA PC_B+1
			   LDA TUNETABLE_C_L,X
			   STA PC_C
			   LDA TUNETABLE_C_H,X
			   STA PC_C+1

			   LDA #$0F					; Volume on
			   STA $D418

			   RTS						; Finito

;
; REFRESH - Called by interrupt, plays the music
;

REFRESH		   LDA MFL_A				; Check if channel A is playing
			   BEQ NOMUSIC_A
			   JSR MUSICA				; Play it

NOMUSIC_A	   LDA MFL_B				; Check if channel B is playing
			   BEQ NOMUSIC_B
			   JSR MUSICB				; Play it

NOMUSIC_B	   LDA MFL_C				; Check if channel C is playing
			   BEQ NOMUSIC_C
			   JSR MUSICC				; Play it

NOMUSIC_C	   LDA #READ_C				; Set up the channel C indirect
			   STA MOD_1+1				; address for the arpeggio

			   LDA #2					; Start modulation from channel C
			   STA VOICE_NUM

			   LDX VOICE_NUM

NEXT_VOICE	   LDA MOD_OFF_A,X			; See if modulators are r=ired
			   BNE NOPUL_A

			   LDA drumFLAG,X			; If a drum is playing don't do any
			   BNE NOPUL_A				; modulators

			   LDA ARPFLAG_A,X			; Is the arpeggio active on this
			   BEQ NOAR_A				; Voice

			   JSR DO_ARP_A				; Yes, process the arpeggio

NOAR_A		   LDA PULSEFLAG_A,X		; Is the pulse width active on this
			   BEQ NOPUL_A				; voice

			   JSR DOPULSE_A			; Yes, process the pulse width

NOPUL_A		   DEC MOD_1+1				; Modify the indirect arpeggio address
			   DEC MOD_1+1
			   DEX
			   STX VOICE_NUM			; Do the next voice
			   BPL NEXT_VOICE

NOPUL_C		   JMP DRUMMER				; Process DunnyDrums (tm)

;
; MUSICTEST - Tests to see if music is playing
;

MUSICTEST	   LDA MFL_A				; ORA all the flags together
			   ORA MFL_B				; if zero is the result then the
			   ORA MFL_C				; tune is complete
			   RTS
;
; MUSICA - Handles all music to channel A
;

MUSICA		   DEC DURCOUNT_A			; Note duration over ?
			   BEQ READBYTE_A			; Yup, get a new byte
			   LDA DURCOUNT_A
			   CMP #1
			   BNE NO_SHUT_A

			   STA MOD_OFF_A			; Shut off modulation for this frame

NO_SHUT_A	   DEC RELEASE_A			; Has the note release finished
			   BNE NO_RELEASE_A			; No, modulate channel A

			   LDA EFFECTFLAG_A			; Effect mode active ?
			   BEQ NOEFFECT_A			; No

			   LDX REAL_NOTE_A			; Get the real unmauled note
			   LDA LOWFREQ,X
			   STA FREQ_LOW_A			; expand out into a fr=ency
			   LDA HIFREQ,X
			   STA FREQ_HI_A

NOEFFECT_A	   LDA MODEBYTE_A			; Should the gate get set
			   AND #1
			   BEQ NO_RELEASE_A			; No way jose

			   LDA GATEOFF_A			; Set the gate bit
			   STA $D404

NO_RELEASE_A   LDX #0
			   JMP MODULATE				; Modulate channel A

READBYTE_A	   LDA SUSTAIN_A			; Reset the note's release period
			   STA RELEASE_A

			   LDY #0					; Shut down any drum on this channel
			   STY MOD_OFF_A
			   STY drumFLAG+0

			   LDA (PC_A),Y				; Get next byte from the data
			   BPL NOCONTROL_A

			   TAX
			   LDA JUMPVECL_A-128,X		; the control code vectors
			   STA JUMPVEC_A+1
			   LDA JUMPVECH_A-128,X
			   STA JUMPVEC_A+2
			   INY

JUMPVEC_A	   JMP $FFFF				; Self modified jump address


NOCONTROL_A	   CMP #REST				; Is it a rest
			   BNE NOREST_A

			   JMP REST_A				; Yes process a rest

NOREST_A	   CLC
				ADC TRANSVAL_A			 ; Add any transpose value to the base
			   STA NOTE_A				; note, and store it
			   STA REAL_NOTE_A

			   LDX EFFECTFLAG_A			; Is effect mode enabled
			   BEQ NOFX_A				; No, thank God !

			   LDA MODEBYTE_A			; Is it an emulated plucked note (!)
			   AND #8
			   BNE PLUCK				; Yessum boss

			   LDA EFFECT_NOTE_A		; Use the effect note for the freq
			   JMP NOFX_A

PLUCK		   LDA REAL_NOTE_A			; Otherwise get the base note and
		CLC
			   ADC EFFECT_NOTE_A		; add the effect note on

NOFX_A		   TAX						; Expand the note into a fr=ency
			   LDA LOWFREQ,X
			   STA FREQ_LOW_A			;  Maulable fr=encys
			   LDA HIFREQ,X
			   STA FREQ_HI_A

DOREST_A	   LDA PULSEFLAG_A			; Pulse mode enabled ?
			   BEQ NOPULSE_A			; No

			   LDA MODEBYTE_A			; Should the pulse be retriggered
			   AND #2
			   BEQ NOPULSE_A			; No

			   LDA PWL2_A				; Reset all pulse width variables
			   STA PWL_A
			   LDA PWH2_A
			   STA PWH_A

			   LDA #0
			   STA PULSEDIR_A			; Pulse direction
			   LDA CUP2_A				; Counts up
			   STA CUP_A
			   LDA CDOWN2_A				; Counts down
			   STA CDOWN_A

NOPULSE_A	   LDA VIBFLAG_A			; Is the vibrato active
			   BEQ NOV_A				; No

			   LDA VIBDELAY2_A			; Reset the vibrato delay
			   STA VIBDELAY_A
			   LDA VIBDEPTH_A			; Get the vibrato depth
			   LSR
			   STA VIBDEPTH2_A			; Divide it by 2 for other direction

			   LDA #0					; Reset the Vib direction variable
			   STA VIB_DIR_A

NOV_A		   LDA MODEBYTE_A			; Should the gate get set
			   AND #1
			   BEQ REST_A				; No

			   LDA GATE_A				; Set the gate bit
			   STA $D404

REST_A		   LDA DURATIONON_A			; Automatic duration mode ?
			   BNE PICK_UP_A			; No

			   LDA DURATION_A			; Reset the note duration
			   STA DURCOUNT_A

			   LDX GAPFLAG_A			; Gap mode active
			   BEQ NOGAP2_A				; no

		   SEC
			   SBC SUSTAIN_A			; Subtact the sustain from the
			   STA RELEASE_A			; duration to give a release value

NOGAP2_A	   INC PC_A					; Increment the music PC
			   BNE COMEOUT_A
			   INC PC_A+1

COMEOUT_A	   JMP NO_RELEASE_A			; Do the modulators for channel A

PICK_UP_A	   INY						; Manual duration mode
			   LDA (PC_A),Y
			   STA DURCOUNT_A			; Get the duration of the note

			   LDX GAPFLAG_A			; Gap mode active
			   BEQ NOGAP_A				; No

				SEC
			   SBC SUSTAIN_A			; Subtact the sustain from the
			   STA RELEASE_A			; duration to give a release value

NOGAP_A		   LDA PC_A					; Add 2 to the music PC
				CLC
			   ADC #2
			   STA PC_A
			   BCC SKIPIT_A
			   INC PC_A+1
SKIPIT_A	   JMP NO_RELEASE_A			; Do the modulators for channel A

;
; MODULATE - Handles all modulation r=irements for Channels A,B & C
;

MODULATE	   LDA MOD_OFF_A,X			; Check if modulators are r=ired
			   BNE NOVIB_A

			   LDA ARPFLAG_A,X			; Is arpeggio processing
			   BEQ NOARP_A
			   RTS						; Yes, don't bother with other stuff

NOARP_A		   LDA PORTFLAG_A,X			; Pitch bender on ?
			   BEQ NOBEND_A				; No

			   JSR DO_BEND_A			; Set up a pitch bend

NOBEND_A	   LDA VIBFLAG_A,X			; Is the vibrato active ?
			   BEQ NOVIB_A				; No

			   JSR DOVIB_A				; Process the vibrato

NOVIB_A		   JMP DUMP					; Dump the final fr=ency to SID

;
; DO.BEND.A - Processes all pitchbending to all channels
;

DO_BEND_A	   BMI SET_BEND_A			; Bendflag minus = init bend

			   DEC BENDLENGTH_A,X		; See if bend complete
			   BEQ BEND_DONE_A			; Exit if so

PROC_BEND_A	   LDA BEND_DIR_A,X			; Otherwise get bend direction
			   BMI BEND_DOWN_A			; Minus = downwards bend

			   LDA FREQ_LOW_A,X			; Get fr=ency and add the bend
				CLC
			   ADC BENDSTEP_A,X
			   STA FREQ_LOW_A,X			; additive
			   BCC NOCHANGE1_A
			   INC FREQ_HI_A,X
NOCHANGE1_A	   RTS						; Exit

BEND_DOWN_A	   LDA FREQ_LOW_A,X			; Get fr=ency and subtract the bend
		SEC
			   SBC BENDSTEP_A,X
			   STA FREQ_LOW_A,X			; subtractive
			   BCS NOCHANGE2_A
			   DEC FREQ_HI_A,X
NOCHANGE2_A	   RTS						; Exit

BEND_DONE_A	   LDA #0					; Terminate the pitchbend
			   STA PORTFLAG_A,X
			   RTS						; Exit

SET_BEND_A	   LDA #1					; Do bend but don't re-initialise
			   STA PORTFLAG_A,X

			   LDY WORD_OFFSET,X

			   LDA BEND_DIR_A,X			; Get the bend direction
			   BMI SET_DOWN_A

			   LDA FREQ_LOW_A,X			; Subract the offset from the base
		   SEC
			   SBC OFFSET_A,Y
			   STA FREQ_LOW_A,X			; fr=ency (bend up to a note)
			   LDA FREQ_HI_A,X
			   SBC OFFSET_A+1,Y
			   STA FREQ_HI_A,X
			   JMP PROC_BEND_A			; Process the bend

SET_DOWN_A	   LDA FREQ_LOW_A,X			; Add the offset from the base
		CLC
			   ADC OFFSET_A,Y
			   STA FREQ_LOW_A,X			; fr=ency (bend down to a note)
			   LDA FREQ_HI_A,X
			   ADC OFFSET_A+1,Y
			   STA FREQ_HI_A,X
			   JMP PROC_BEND_A			; Process the bend

;
; DOVIB_A - Suprisingly enuf this handles the vibrato
;

DOVIB_A		   LDA VIBDELAY_A,X			; Time to activate the vibrato
			   BEQ DELAYOFF_A
			   DEC VIBDELAY_A,X			; No, not yet
			   RTS

DELAYOFF_A	   DEC VIBDEPTH2_A,X		; Decrement the vib direction counter
			   BEQ CHANGEDIR_A			; Zero = change the direction

			   LDA VIB_DIR_A,X			; Get the vibrato direction
			   BNE VIB_DOWN_A

			   LDA FREQ_LOW_A,X			; Get the base fr=ency
		   CLC
			   ADC VIBRATE_A,X			; Add the vibrato add rate
			   STA FREQ_LOW_A,X
			   BCC NOADD_A
			   INC FREQ_HI_A,X
NOADD_A		   RTS						; Exit

VIB_DOWN_A	   LDA FREQ_LOW_A,X			; Get the base fr=ency
		SEC
			   SBC VIBRATE_A,X			; Subtract the vibrato subtract rate
			   STA FREQ_LOW_A,X
			   BCS NOADD2_A
			   DEC FREQ_HI_A,X
NOADD2_A	   RTS						; Exit

CHANGEDIR_A	   LDA VIB_DIR_A,X			; Flip the vibrato direction
			   EOR #1
			   STA VIB_DIR_A,X
			   LDA VIBDEPTH_A,X			; Reset the depth counter
			   STA VIBDEPTH2_A,X
			   JMP DELAYOFF_A			; Back to processing the vibrato

;
; TRANS_A - Constant transpose value initialise
;

TRANS_A		   LDA (PC_A),Y				; Pull in the transpose value
			   STA TRANSVAL_A			; Store it
			   LDA #2
			   JMP ADD_PC_A				; Bump the PC

;
; TRANS_B - Constant transpose value initialise
;

TRANS_B									; Ditto
			   LDA (PC_B),Y
			   STA TRANSVAL_B
			   LDA #2
			   JMP ADD_PC_B

;
; TRANS_C - Constant transpose value initialise
;

TRANS_C									; Ditto
			   LDA (PC_C),Y
			   STA TRANSVAL_C
			   LDA #2
			   JMP ADD_PC_C

;
; CT_A - Call a routine transposed
;

CT_A		   LDA (PC_A),Y				; Pull in the transpose value
			   STA TRANSVAL_A			; Store
			   INC PC_A					; Bump the PC
			   BNE SKPCT_A
			   INC PC_A+1
SKPCT_A		   JMP CALL_A				; Cheap n nasty go to the CALL routine

;
; CT_B - Call a routine transposed
;

CT_B									; Ditto
			   LDA (PC_B),Y
			   STA TRANSVAL_B
			   INC PC_B
			   BNE SKPCT_B
			   INC PC_B+1
SKPCT_B		   JMP CALL_B

;
; CT_C - Call a routine transposed
;

CT_C									; Ditto
			   LDA (PC_C),Y
			   STA TRANSVAL_C
			   INC PC_C
			   BNE SKPCT_C
			   INC PC_C+1
SKPCT_C		   JMP CALL_C

;
; LENGTH_A - Determines the duration of the notes
;

LENGTH_A	   LDA (PC_A),Y				; Pull in the constant duration value
			   STA DURATION_A
			   LDA #0
			   STA DURATIONON_A			; Signal this mode
			   LDA #2
			   JMP ADD_PC_A				; Bump the PC
;
; DO_ARP_A - Processes the arpeggiator on A,B & C
;

DO_ARP_A	   DEC NEXTDELAY_A,X		; Time to read the next byte yet ?
			   BEQ GO_ARP_A
			   RTS						; No

GO_ARP_A	   LDA NEXTDELAY2_A,X		; Reset the read period
			   STA NEXTDELAY_A,X

			   LDY ARPEGIND_A,X			; Get the index within the table
MOD_1		   LDA (READ_A),Y			; plus the indirect address
			   LDY SID_OFFSETS,X
		   CLC
			   ADC NOTE_A,X				; add this value to the base note
			   TAX
			   LDA LOWFREQ,X			; Expand out into a fr=ency
			   STA $D400,Y
			   LDA HIFREQ,X
			   STA $D401,Y
			   LDX VOICE_NUM
			   INC ARPEGIND_A,X			; Increment the index within the table
			   LDA ARPEGIND_A,X
			   CMP ARPEGLEN_A,X			; Reached the end of the table yet ?
			   BNE RESETARP_A
			   LDA #2					; Yes reset the index to 2
			   STA ARPEGIND_A,X
RESETARP_A	   RTS						; Exit

;
; DOPULSE_A - Process pwm on A,B & C
;

DOPULSE_A	   LDA PULSEDIR_A,X			; Get the pulse direction
			   BNE PULSEDOWN_A

PULSEUP_A	   DEC CUP_A,X				; Decrement the pulse up counter
			   BEQ NEWDIR_A				; zero, do the other direction

			   LDY SID_OFFSETS,X
			   LDA PWL_A,X				; Add the rate up to the
		   CLC
			   ADC RATEUP_A,X
			   STA PWL_A,X				; pulse width
			   STA $D402,Y
			   LDA PWH_A,X
			   ADC #0
			   STA PWH_A,X
			   STA $D403,Y
			   RTS						; Exit

PULSEDOWN_A	   DEC CDOWN_A,X			; Decrement the pulse down counter
			   BEQ NEWDIR2_A			; Zero, do the other direction

			   LDY SID_OFFSETS,X
			   LDA PWL_A,X				; Subtract the pulse down counter
		   SEC
			   SBC RATEUP_A,X
			   STA PWL_A,X				; to the pulse width
			   STA $D402,Y
			   LDA PWH_A,X
			   SBC #0
			   STA PWH_A,X
			   STA $D403,Y
			   RTS						; Exit

NEWDIR_A	   LDA CUP2_A,X				; Reset the count up
			   STA CUP_A,X
			   LDA #1					; Set the new direction
			   STA PULSEDIR_A,X
			   JMP PULSEDOWN_A			; Process it

NEWDIR2_A	   LDA CDOWN2_A,X			; Reset the count down
			   STA CDOWN_A,X
			   LDA #0					; Set the new direction
			   STA PULSEDIR_A,X
			   JMP PULSEUP_A			; Process it


;
; DUMP - Dumps fr=encies to SID channel A,B & C
;

DUMP		   LDY SID_OFFSETS,X
			   LDA FREQ_LOW_A,X			; Get the final mauled fr=ency
			   STA $D400,Y
			   LDA FREQ_HI_A,X			; and send it to SID
			   STA $D401,Y
			   RTS

;
; ADD_PC_A - Adds a value in A to PC_A
;

ADD_PC_A		CLC
				ADC PC_A				 ; Add PC to the value in the
			   STA PC_A
			   BCC SKP_A				; accumulator
			   INC PC_A+1
SKP_A		   JMP READBYTE_A			; and back to the s=encer

;
; DUR_ON_A - Acctivates manual duration mode
;

DUR_ON_A	   LDA #1					; Set the manual duration flag
			   STA DURATIONON_A
			   INC PC_A					; Bump PC
			   BNE SKIP_A
			   INC PC_A+1
SKIP_A		   JMP READBYTE_A			; Back to the s=encer

;
; PULSEON_A - Switches pulse width modulation on !
;

PULSEON_A
			LDA #0
			STA FROM+1				; Zero FROM+1
			LDA (PC_A),Y				; Get the pulse patch number
			ASL
			ASL
			CLC
			ADC (PC_A),Y				; *5
			ADC #<PWM_START
			STA FROM
			LDA FROM+1
			ADC #>PWM_START			 ; plus PWM table base address
			STA FROM+1

			DEY						; Y=0
			LDA (FROM),Y
			STA RATEUP_A				; Get the rate on the upward sweep

			INY						; Y=1

			STY PULSEFLAG_A			; Pulse enabled
			STY PULSEDIR_A			; Set the initial direction

			LDA (FROM),Y
			STA CUP_A				; Get the additive count
			STA CUP2_A
			INY
			LDA (FROM),Y
			STA CDOWN_A				; Get the subtractive count
			STA CDOWN2_A

			INY						; Y=2

			LDA (FROM),Y				; Initial pulse width LOW
			STA PWL2_A
			INY						; Y=3
			LDA (FROM),Y
			STA PWH2_A				; Initial pulse width HIGH

			LDA #2					; Bump PC
			JMP ADD_PC_A

;
; MUSICB - Handles all music to channel B
;

MUSICB
			DEC DURCOUNT_B
			BEQ READBYTE_B
			LDA DURCOUNT_B
			CMP #1
			BNE NO_SHUT_B

			STA MOD_OFF_B

NO_SHUT_B
			DEC RELEASE_B
			BNE NO_RELEASE_B

			LDA EFFECTFLAG_B
			BEQ NOEFFECT_B

			LDX REAL_NOTE_B
			LDA LOWFREQ,X
			STA FREQ_LOW_B
			LDA HIFREQ,X
			STA FREQ_HI_B

NOEFFECT_B	   LDA MODEBYTE_B
			   AND #1
			   BEQ NO_RELEASE_B
			   LDA GATEOFF_B
			   STA $D404+7
NO_RELEASE_B   LDX #1
			   JMP MODULATE				; Modulate channel B

READBYTE_B
			LDA SUSTAIN_B
			STA RELEASE_B

			LDY #0
			STY MOD_OFF_B
			STY drumFLAG+1
			LDA (PC_B),Y
			BPL NOCONTROL_B

			TAX
			LDA JUMPVECL_B-128,X
			STA JUMPVEC_B+1
			LDA JUMPVECH_B-128,X
			STA JUMPVEC_B+2
			INY

JUMPVEC_B	   JMP $FFFF


NOCONTROL_B	   CMP #REST
			   BNE NOREST_B
			   JMP REST_B
NOREST_B		CLC
		ADC TRANSVAL_B
			   STA NOTE_B
			   STA REAL_NOTE_B

			   LDX EFFECTFLAG_B
			   BEQ NOFX_B

			   LDA MODEBYTE_B
			   AND #8
			   BNE PLUCK_B

			   LDA EFFECT_NOTE_A
			   JMP NOFX_B

PLUCK_B		   LDA REAL_NOTE_B
		CLC
			   ADC EFFECT_NOTE_B

NOFX_B		   TAX
			   LDA LOWFREQ,X
			   STA FREQ_LOW_B			;  Maulable fr=encys
			   LDA HIFREQ,X
			   STA FREQ_HI_B

			   LDA PULSEFLAG_B
			   BEQ NOPULSE_B
			   LDA MODEBYTE_B
			   AND #2
			   BEQ NOPULSE_B

			   LDA PWL2_B
			   STA PWL_B
			   LDA PWH2_B
			   STA PWH_B
			   LDA #0
			   STA PULSEDIR_B
			   LDA CUP2_B
			   STA CUP_B
			   LDA CDOWN2_B
			   STA CDOWN_B

NOPULSE_B	   LDA VIBFLAG_B
			   BEQ NOV_B
			   LDA VIBDELAY2_B
			   STA VIBDELAY_B
			   LDA VIBDEPTH_B
			   LSR
			   STA VIBDEPTH2_B
			   LDA #0
			   STA VIB_DIR_B

NOV_B		   LDA MODEBYTE_B
			   AND #1
			   BEQ REST_B

			   LDA GATE_B
			   STA $D404+7

REST_B		   LDA DURATIONON_B
			   BNE PICK_UP_B

			   LDA DURATION_B
			   STA DURCOUNT_B

			   LDX GAPFLAG_B
			   BEQ NOGAP2_B

		   SEC
			   SBC SUSTAIN_B
			   STA RELEASE_B

NOGAP2_B	   INC PC_B
			   BNE COMEOUT_B
			   INC PC_B+1

COMEOUT_B	   JMP NO_RELEASE_B

PICK_UP_B	   INY
			   LDA (PC_B),Y
			   STA DURCOUNT_B

			   LDX GAPFLAG_B
			   BEQ NOGAP_B

		   SEC
			   SBC SUSTAIN_B
			   STA RELEASE_B

NOGAP_B		   LDA PC_B
		CLC
			   ADC #2
			   STA PC_B
			   BCC SKIPIT_B
			   INC PC_B+1
SKIPIT_B	   JMP NO_RELEASE_B

;
; ADD_PC_B - Adds a value in A to PC_B
;

ADD_PC_B		CLC
		ADC PC_B
			   STA PC_B
			   BCC SKP_B
			   INC PC_B+1
SKP_B		   JMP READBYTE_B

;
; LENGTH_B - Determines the duration of the notes
;

LENGTH_B	   LDA (PC_B),Y
			   STA DURATION_B
			   LDA #0
			   STA DURATIONON_B
			   LDA #2
			   JMP ADD_PC_B

;
; DUR_ON_B - Acctivates manual duration mode
;

DUR_ON_B	   LDA #1
			   STA DURATIONON_B
			   INC PC_B
			   BNE SKIP_B
			   INC PC_B+1
SKIP_B		   JMP READBYTE_B

;
; PULSEON_B - Switches pulse width modulation on !
;

PULSEON_B	   LDA #0
			   STA FROM+1
			   LDA (PC_B),Y
			   ASL
		   ASL
		   CLC
			   ADC (PC_B),Y
			   ADC #<PWM_START
			   STA FROM
			   LDA FROM+1
			   ADC #>PWM_START
			   STA FROM+1

			   DEY
			   LDA (FROM),Y
			   STA RATEUP_B
			   INY
			   STY PULSEFLAG_B
			   STY PULSEDIR_B
			   LDA (FROM),Y
			   STA CUP_B
			   STA CUP2_B
			   INY
			   LDA (FROM),Y
			   STA CDOWN_B
			   STA CDOWN2_B
			   INY
			   LDA (FROM),Y
			   STA PWL2_B
			   INY
			   LDA (FROM),Y
			   STA PWH2_B

			   LDA #2
			   JMP ADD_PC_B

;
; MUSICC - Handles all music to channel C
;

MUSICC		   DEC DURCOUNT_C
			   BEQ READBYTE_C
			   LDA DURCOUNT_C
			   CMP #1
			   BNE NO_SHUT_C

			   STA MOD_OFF_C

NO_SHUT_C	   DEC RELEASE_C
			   BNE NO_RELEASE_C

			   LDA EFFECTFLAG_C
			   BEQ NOEFFECT_C

			   LDX REAL_NOTE_C
			   LDA LOWFREQ,X
			   STA FREQ_LOW_C
			   LDA HIFREQ,X
			   STA FREQ_HI_C

NOEFFECT_C	   LDA MODEBYTE_C
			   AND #1
			   BEQ NO_RELEASE_C
			   LDA GATEOFF_C
			   STA $D404+14

NO_RELEASE_C   LDA drumFLAG+2			; No modulation during drums
			   BEQ DO_MODULATE
			   RTS

DO_MODULATE	   LDX #2
			   JMP MODULATE				; Modulate channel C

READBYTE_C	   LDA SUSTAIN_C
			   STA RELEASE_C

			   LDA drumFLAG+2
			   BEQ DONTOFF_C

			   LDA #0
			   STA drumFLAG+2
			   STA DRUMx+2
			   STA $D404+14

			   LDA SUSTEMP+2
			   STA $D406+14
			   LDA ADTEMP+2
			   STA $D405+14

DONTOFF_C	   LDY #0
			   STY MOD_OFF_C
			   LDA (PC_C),Y
			   BPL NOCONTROL_C			; Found a control byte

			   TAX
			   LDA JUMPVECL_C-128,X
			   STA JUMPVEC_C+1
			   LDA JUMPVECH_C-128,X
			   STA JUMPVEC_C+2
			   INY

JUMPVEC_C	   JMP $FFFF

NOCONTROL_C	   CMP #REST
			   BNE NOREST_C
			   JMP REST_C
NOREST_C		CLC
		ADC TRANSVAL_C
			   STA NOTE_C
			   STA REAL_NOTE_C

			   LDX EFFECTFLAG_C
			   BEQ NOFX_C

			   LDA MODEBYTE_C
			   AND #8
			   BNE PLUCK_C

			   LDA EFFECT_NOTE_C
			   JMP NOFX_C

PLUCK_C		   LDA REAL_NOTE_C
		CLC
			   ADC EFFECT_NOTE_C

NOFX_C		   TAX
			   LDA LOWFREQ,X
			   STA FREQ_LOW_C			;  Maulable fr=encys
			   LDA HIFREQ,X
			   STA FREQ_HI_C

DOREST_C	   LDA PULSEFLAG_C
			   BEQ NOPULSE_C
			   LDA MODEBYTE_C
			   AND #2
			   BEQ NOPULSE_C

			   LDA PWL2_C
			   STA PWL_C
			   LDA PWH2_C
			   STA PWH_C
			   LDA #0
			   STA PULSEDIR_C
			   LDA CUP2_C
			   STA CUP_C
			   LDA CDOWN2_C
			   STA CDOWN_C

NOPULSE_C	   LDA VIBFLAG_C
			   BEQ NOV_C
			   LDA VIBDELAY2_C
			   STA VIBDELAY_C
			   LDA VIBDEPTH_C
			   LSR
			   STA VIBDEPTH2_C
			   LDA #0
			   STA VIB_DIR_C

NOV_C		   LDA MODEBYTE_C
			   AND #1
			   BEQ REST_C
			   LDA GATE_C
			   STA $D404+14

REST_C		   LDA DURATIONON_C
			   BNE PICK_UP_C

			   LDA DURATION_C
			   STA DURCOUNT_C

			   LDX GAPFLAG_C
			   BEQ NOGAP2_C

		   SEC
			   SBC SUSTAIN_C
			   STA RELEASE_C

NOGAP2_C	   INC PC_C
			   BNE COMEOUT_C
			   INC PC_C+1

COMEOUT_C	   JMP NO_RELEASE_C

PICK_UP_C	   INY
			   LDA (PC_C),Y
			   STA DURCOUNT_C

			   LDX GAPFLAG_C
			   BEQ NOGAP_C

		   SEC
			   SBC SUSTAIN_C
			   STA RELEASE_C

NOGAP_C		   LDA PC_C
		CLC
			   ADC #2
			   STA PC_C
			   BCC SKIPIT_C
			   INC PC_C+1
SKIPIT_C	   JMP NO_RELEASE_C

;
; LENGTH_C - Determines the duration of the notes
;

LENGTH_C	   LDA (PC_C),Y
			   STA DURATION_C
			   LDA #0
			   STA DURATIONON_C
			   LDA #2
			   JMP ADD_PC_C

;
; DUR_ON_C - Acctivates manual duration mode
;

DUR_ON_C	   LDA #1
			   STA DURATIONON_C
			   INC PC_C
			   BNE SKIP_C
			   INC PC_C+1
SKIP_C		   JMP READBYTE_C


;
; ADD_PC_C - Adds a value in A to PC_C
;

ADD_PC_C		CLC
		ADC PC_C
			   STA PC_C
			   BCC SKP_C
			   INC PC_C+1
SKP_C		   JMP READBYTE_C

;
; PULSEON_C - Switches pulse width modulation on !
;

PULSEON_C	   LDA #0
			   STA FROM+1
			   LDA (PC_C),Y
			   ASL
		   ASL
		   CLC
			   ADC (PC_C),Y
			   ADC #<PWM_START
			   STA FROM
			   LDA FROM+1
			   ADC #>PWM_START
			   STA FROM+1

			   DEY
			   LDA (FROM),Y
			   STA RATEUP_C
			   INY
			   STY PULSEFLAG_C
			   STY PULSEDIR_C
			   LDA (FROM),Y
			   STA CUP_C
			   STA CUP2_C
			   INY
			   LDA (FROM),Y
			   STA CDOWN_C
			   STA CDOWN2_C
			   INY
			   LDA (FROM),Y
			   STA PWL2_C
			   INY
			   LDA (FROM),Y
			   STA PWH2_C

			   LDA #2
			   JMP ADD_PC_C

;
; Music control routines follow ......
;

;
; JUMP_A - Jumps to a new s=ence of notes
;

JUMP_A		   LDA (PC_A),Y
			   STA TEMP_PC				; Preserve the LOW byte new address
			   INY
			   LDA (PC_A),Y
			   STA PC_A+1				; New high byte into PC+1
			   LDA TEMP_PC
			   STA PC_A					; Low byte in PC

			   JMP READBYTE_A			; Back to s=encer

;
; CALL_A - Same as JSR, goes to a new s=ence and returns on finding a RET
;

CALL_A		   LDX SP_A					; Stack pointer, channel A
			   LDA (PC_A),Y
			   STA TEMP_PC				; TEMP_PC contains CALL address
			   INY
			   LDA (PC_A),Y
			   STA TEMP_PC+1

			   LDA PC_A
		   CLC
			   ADC #3
			   STA STACK_A_L,X			; Preserve current PC low byte
			   LDA PC_A+1
			   ADC #0
			   STA STACK_A_H,X			; Preserve current PC high byte

			   LDA TEMP_PC				; Set PC to CALL address
			   STA PC_A
			   LDA TEMP_PC+1
			   STA PC_A+1

			   INC SP_A					; Increment channel A stack pointer
			   JMP READBYTE_A

;
; RET_A	 - Returns the last stacked PC value
;

RET_A		   DEC SP_A					; Decrement channel A stack pointer
			   LDX SP_A
			   LDA STACK_A_L,X			; Retrieve Program counter low byte
			   STA PC_A
			   LDA STACK_A_H,X			; Retrieve Program counter high byte
			   STA PC_A+1
			   LDA #0
			   STA TRANSVAL_A			; Reset the transpose value
			   JMP READBYTE_A

;
; FOR_HANDLE_A - Handles the stacking of FOR instructions
;

FOR_HANDLE_A   LDX SP_A
			   LDA (PC_A),Y
			   STA FOR_COUNTS_A,X		; Store number of channel A FOR loops

			   LDA PC_A					; Bump PC, and stack it's new value
		   CLC
			   ADC #2
			   STA PC_A
			   STA STACK_A_L,X
			   BCC NOCARRY_A
			   INC PC_A+1

NOCARRY_A	   LDA PC_A+1
			   STA STACK_A_H,X
			   INC SP_A					; Bump channel A stack pointer
			   JMP READBYTE_A

;
; NEXT_HANDLE_A - Handles unstacking PC and decrementing the FOR counters
;

NEXT_HANDLE_A  LDX SP_A					; SP_A only gets stored in LAST_NEXT
			   DEX						; to preserve the FOR address
			   DEC FOR_COUNTS_A,X
			   BEQ LAST_NEXT_A
			   LDA STACK_A_L,X			; Get back old for address
			   STA PC_A
			   LDA STACK_A_H,X
			   STA PC_A+1
			   JMP READBYTE_A

LAST_NEXT_A	   STX SP_A					; Decremented SP_A is now stored
			   INC PC_A
			   BNE NOCARRY2_A
			   INC PC_A+1
NOCARRY2_A	   JMP READBYTE_A			; Back to s=encer

;
; JUMP_B - Jumps to a new s=ence of notes
;

JUMP_B									; Ditto
			   LDA (PC_B),Y
			   STA TEMP_PC
			   INY
			   LDA (PC_B),Y
			   STA PC_B+1
			   LDA TEMP_PC
			   STA PC_B
			   JMP READBYTE_B

;
; CALL_B - Same as JSR, goes to a new s=ence and returns on finding a RET
;

CALL_B		   LDX SP_B					; Ditto
			   LDA (PC_B),Y
			   STA TEMP_PC
			   INY
			   LDA (PC_B),Y
			   STA TEMP_PC+1

			   LDA PC_B
		   CLC
			   ADC #3
			   STA STACK_B_L,X
			   LDA PC_B+1
			   ADC #0
			   STA STACK_B_H,X

			   LDA TEMP_PC
			   STA PC_B
			   LDA TEMP_PC+1
			   STA PC_B+1

			   INC SP_B
			   JMP READBYTE_B

;
; RET_B	 - Returns the last stacked PC value
;

RET_B		   DEC SP_B					; Ditto
			   LDX SP_B
			   LDA STACK_B_L,X
			   STA PC_B
			   LDA STACK_B_H,X
			   STA PC_B+1
			   LDA #0
			   STA TRANSVAL_B
			   JMP READBYTE_B

;
; FOR_HANDLE_B - Handles the stacking of FOR instructions
;

FOR_HANDLE_B   LDX SP_B					; Ditto
			   LDA (PC_B),Y
			   STA FOR_COUNTS_B,X

			   LDA PC_B
		   CLC
			   ADC #2
			   STA PC_B
			   STA STACK_B_L,X
			   BCC NOCARRY_B
			   INC PC_B+1

NOCARRY_B	   LDA PC_B+1
			   STA STACK_B_H,X
			   INC SP_B
			   JMP READBYTE_B

;
; NEXT_HANDLE_B - Handles unstacking PC and decrementing the FOR counters
;

NEXT_HANDLE_B  LDX SP_B					; Ditto
			   DEX
			   DEC FOR_COUNTS_B,X
			   BEQ LAST_NEXT_B
			   LDA STACK_B_L,X
			   STA PC_B
			   LDA STACK_B_H,X
			   STA PC_B+1
			   JMP READBYTE_B

LAST_NEXT_B	   STX SP_B
			   INC PC_B
			   BNE NOCARRY2_B
			   INC PC_B+1
NOCARRY2_B	   JMP READBYTE_B

;
; JUMP_C - Jumps to a new s=ence of notes
;

JUMP_C									; Ditto
			   LDA (PC_C),Y
			   STA TEMP_PC
			   INY
			   LDA (PC_C),Y
			   STA PC_C+1
			   LDA TEMP_PC
			   STA PC_C
			   JMP READBYTE_C

;
; CALL_C - Same as JSR, goes to a new s=ence and returns on finding a RET
;

CALL_C		   LDX SP_C					; Ditto
			   LDA (PC_C),Y
			   STA TEMP_PC
			   INY
			   LDA (PC_C),Y
			   STA TEMP_PC+1

			   LDA PC_C
		   CLC
			   ADC #3
			   STA STACK_C_L,X
			   LDA PC_C+1
			   ADC #0
			   STA STACK_C_H,X

			   LDA TEMP_PC
			   STA PC_C
			   LDA TEMP_PC+1
			   STA PC_C+1

			   INC SP_C
			   JMP READBYTE_C

;
; RET_C	 - Returns the last stacked PC value
;

RET_C		   DEC SP_C					; Ditto
			   LDX SP_C
			   LDA STACK_C_L,X
			   STA PC_C
			   LDA STACK_C_H,X
			   STA PC_C+1
			   LDA #0
			   STA TRANSVAL_C
			   JMP READBYTE_C

;
; FOR_HANDLE_C - Handles the stacking of FOR instructions
;

FOR_HANDLE_C   LDX SP_C					; Ditto
			   LDA (PC_C),Y
			   STA FOR_COUNTS_C,X

			   LDA PC_C
		   CLC
			   ADC #2
			   STA PC_C
			   STA STACK_C_L,X
			   BCC NOCARRY_C
			   INC PC_C+1

NOCARRY_C	   LDA PC_C+1
			   STA STACK_C_H,X
			   INC SP_C
			   JMP READBYTE_C

;
; NEXT_HANDLE_C - Handles unstacking PC and decrementing the FOR counters
;

NEXT_HANDLE_C  LDX SP_C					; Ditto
			   DEX
			   DEC FOR_COUNTS_C,X
			   BEQ LAST_NEXT_C
			   LDA STACK_C_L,X
			   STA PC_C
			   LDA STACK_C_H,X
			   STA PC_C+1
			   JMP READBYTE_C

LAST_NEXT_C	   STX SP_C
			   INC PC_C
			   BNE NOCARRY2_C
			   INC PC_C+1
NOCARRY2_C	   JMP READBYTE_C



;
; VIBON_A - Acctivates the vibrato
;

VIBON_A		   LDA (PC_A),Y
			   STA VIBDELAY2_A			; Permenant vib delay
			   INY
			   LDA (PC_A),Y
			   STA VIBRATE_A			; additive/subtractive rate
			   INY
			   LDA (PC_A),Y
			   STA VIBDEPTH_A			; Variable vib depth
			   LSR
			   STA VIBDEPTH2_A			; Permenant vib depth
			   LDX #1
			   STX VIBFLAG_A			; Signal vibrato is on
			   DEX

			   STX VIB_DIR_A			; Set the initial vibrato direction
			   STX ARPFLAG_A			; Cancel arpeggio (if it is running)

			   LDA #4					; Bump the PC
			   JMP ADD_PC_A

;
; VIBOFF_A - Guess moron !
;

VIBOFF_A	   LDA #0					; Shut down the vibrato
			   STA VIBFLAG_A
			   INC PC_A					; Bump PC
			   BNE SKIP2_A
			   INC PC_A+1
SKIP2_A		   JMP READBYTE_A			; Back to the s=encer

;
; VIBON_B - Acctivates the vibrato
;

VIBON_B									; Ditto
			   LDA (PC_B),Y
			   STA VIBDELAY2_B			; Permenant vib delay
			   INY
			   LDA (PC_B),Y
			   STA VIBRATE_B
			   INY
			   LDA (PC_B),Y
			   STA VIBDEPTH_B
			   LSR
			   STA VIBDEPTH2_B			; "	 "	"  "  " depth
			   LDX #1
			   STX VIBFLAG_B
			   DEX
			   STX VIB_DIR_B
			   STX ARPFLAG_B

			   LDA #4
			   JMP ADD_PC_B

;
; VIBOFF_B - Guess moron !
;

VIBOFF_B	   LDA #0					; Ditto
			   STA VIBFLAG_B
			   INC PC_B
			   BNE SKIP2_B
			   INC PC_B+1
SKIP2_B		   JMP READBYTE_B

;
; VIBON_C - Acctivates the vibrato
;

VIBON_C									; Ditto
			   LDA (PC_C),Y
			   STA VIBDELAY2_C			; Permenant vib delay
			   INY
			   LDA (PC_C),Y
			   STA VIBRATE_C
			   INY
			   LDA (PC_C),Y
			   STA VIBDEPTH_C
			   LSR
			   STA VIBDEPTH2_C			; "	 "	"  "  " depth
			   LDX #1
			   STX VIBFLAG_C
			   DEX
			   STX VIB_DIR_C
			   STX ARPFLAG_C

			   LDA #4
			   JMP ADD_PC_C

;
; VIBOFF_C - Guess moron !
;

VIBOFF_C	   LDA #0					; Ditto
			   STA VIBFLAG_C
			   INC PC_C
			   BNE SKIP2_C
			   INC PC_C+1
SKIP2_C		   JMP READBYTE_C



;
; ARPON_A - Acctivates the arpeggiator
;

ARPON_A
			   LDA (PC_A),Y
			   STA READ_A				; Get the address of the arpeggio
			   INY
			   LDA (PC_A),Y				; table
			   STA READ_A+1

			   LDY #0
;  STY VIBFLAG_A
			   LDA (READ_A),Y
			   STA NEXTDELAY2_A			; Get the delay between reading bytes
			   INY
			   STY ARPFLAG_A			; Trigger the arpeggiator
			   STY NEXTDELAY_A
			   LDA (READ_A),Y
		   CLC
			   ADC #2
			   STA ARPEGLEN_A			; Set the length of the arpeggio table
			   INY
			   STY ARPEGIND_A			; Set the index into the table

			   LDA #3					; Bump the PC
			   JMP ADD_PC_A

;
; ARPOFF_A - No prizes dickhead !
;

ARPOFF_A	   LDA #0					; Shut down the arpeggiator
			   STA ARPFLAG_A
			   INC PC_A					; Bump the PC
			   BNE SKIP4_A
			   INC PC_A+1
SKIP4_A		   JMP READBYTE_A			; Back to the s=encer

;
; ARPON_B - Acctivates the arpeggiator
;

ARPON_B									; Ditto
			   LDA (PC_B),Y
			   STA READ_B
			   INY
			   LDA (PC_B),Y
			   STA READ_B+1

			   LDY #0
;	   STY VIBFLAG_B
			   LDA (READ_B),Y
			   STA NEXTDELAY2_B
			   INY
			   STY ARPFLAG_B
			   STY NEXTDELAY_B
			   LDA (READ_B),Y
		   CLC
			   ADC #2
			   STA ARPEGLEN_B
			   INY
			   STY ARPEGIND_B

			   LDA #3
			   JMP ADD_PC_B

;
; ARPOFF_B - No prizes dickhead !
;

ARPOFF_B	   LDA #0					; Ditto
			   STA ARPFLAG_B
			   INC PC_B
			   BNE SKIP4_B
			   INC PC_B+1
SKIP4_B		   JMP READBYTE_B

;
; ARPON_C - Acctivates the arpeggiator
;

ARPON_C									; Ditto
			   LDA (PC_C),Y
			   STA READ_C
			   INY
			   LDA (PC_C),Y
			   STA READ_C+1

			   LDY #0
;	  STY VIBFLAG_C
			   LDA (READ_C),Y
			   STA NEXTDELAY2_C
			   INY
			   STY ARPFLAG_C
			   STY NEXTDELAY_C
			   LDA (READ_C),Y
		   CLC
			   ADC #2
			   STA ARPEGLEN_C
			   INY
			   STY ARPEGIND_C

			   LDA #3
			   JMP ADD_PC_C

;
; ARPOFF_C - No prizes dickhead !
;

ARPOFF_C	   LDA #0					; Ditto
			   STA ARPFLAG_C
			   INC PC_C
			   BNE SKIP4_C
			   INC PC_C+1
SKIP4_C		   JMP READBYTE_C


;
; MODE_A - Sets triggering mode, i'll be buggered if i know what this does !
;

MODE_A		   LDA (PC_A),Y
			   BMI SETEM_A
			   EOR #$FF
			   AND MODEBYTE_A
			   STA MODEBYTE_A

			   LDA #2
			   JMP ADD_PC_A

SETEM_A		   ORA MODEBYTE_A
			   STA MODEBYTE_A
			   LDA #2
			   JMP ADD_PC_A

;
; MODE_B - Sets triggering mode
;

MODE_B		   LDA (PC_B),Y
			   BMI SETEM_B
			   EOR #$FF
			   AND MODEBYTE_B
			   STA MODEBYTE_B

			   LDA #2
			   JMP ADD_PC_B

SETEM_B		   ORA MODEBYTE_B
			   STA MODEBYTE_B
			   LDA #2
			   JMP ADD_PC_B

;
; MODE_C - Sets triggering mode
;

MODE_C		   LDA (PC_C),Y
			   BMI SETEM_C
			   EOR #$FF
			   AND MODEBYTE_C
			   STA MODEBYTE_C

			   LDA #2
			   JMP ADD_PC_C

SETEM_C		   ORA MODEBYTE_C
			   STA MODEBYTE_C
			   LDA #2
			   JMP ADD_PC_C

;
; PORT_A - Sends 1 byte of data to an offset of the SID chip
;

PORT_A		   LDA (PC_A),Y
			   STA TEMP_VAL				; Store byte to be send to SID
			   INY
			   LDA (PC_A),Y				; Get sid offset
			   TAX
			   LDA TEMP_VAL				; Send value to SID
			   STA $D400,X

			   LDA #3					; Bump the PC
			   JMP ADD_PC_A

PORT_B									; Ditto
			   LDA (PC_B),Y
			   STA TEMP_VAL
			   INY
			   LDA (PC_B),Y
			   TAX
			   LDA TEMP_VAL
			   STA $D400,X

			   LDA #3
			   JMP ADD_PC_B

PORT_C									; Ditto
			   LDA (PC_C),Y
			   STA TEMP_VAL
			   INY
			   LDA (PC_C),Y
			   TAX
			   LDA TEMP_VAL
			   STA $D400,X

			   LDA #3
			   JMP ADD_PC_C

;
; CODE_A - Jumps to a machine code segment
;

CODE_A		   LDA (PC_A),Y
			   STA JUMP_MOD_A+1			; Get the address of the code
			   INY
			   LDA (PC_A),Y				; to be JSR'd to
			   STA JUMP_MOD_A+2

JUMP_MOD_A	   JSR $AAAA				; Self modified

			   LDA #3					; Bump the PC
			   JMP ADD_PC_A

CODE_B									; Ditto
			   LDA (PC_B),Y
			   STA JUMP_MOD_B+1
			   INY
			   LDA (PC_B),Y
			   STA JUMP_MOD_B+2

JUMP_MOD_B	   JSR $AAAA				; Self modified

			   LDA #3
			   JMP ADD_PC_B


CODE_C									; Ditto
			   LDA (PC_C),Y
			   STA JUMP_MOD_C+1
			   INY
			   LDA (PC_C),Y
			   STA JUMP_MOD_C+2

JUMP_MOD_C	   JSR $AAAA				; Self modified

			   LDA #3
			   JMP ADD_PC_C

;
; EFFECT_A - Sets up effect mode
;

;EFFECT_A
;			   LDA (PC_A),Y
;			   STA EFFECT_NOTE_A
;			   STY EFFECTFLAG_A
;
;			   LDA #2
;			   JMP ADD_PC_A
;
;EFFECT_B
;			   LDA (PC_B),Y
;			   STA EFFECT_NOTE_B
;			   STY EFFECTFLAG_B
;
;			   LDA #2
;			   JMP ADD_PC_B

EFFECT_C	   LDA (PC_C),Y
			   STA EFFECT_NOTE_C
			   STY EFFECTFLAG_C

			   LDA #2
			   JMP ADD_PC_C

;
; BEND.ON.A - Acctivates the Pitchbender & sets up the pitchbend data
;

BEND_ON_A	   LDA #$81					; Bend on+minus = initialise bend
			   STA PORTFLAG_A
			   LDA (PC_A),Y
			   STA BEND_DIR_A			; Get the bend direction
			   INY
			   LDA (PC_A),Y
			   STA BENDSTEP_A			; Get the steps
			   INY
			   LDA (PC_A),Y
			   STA BENDLENGTH_A			; and the length
			   INY
			   LDA (PC_A),Y				; Bend offset (Rate x Duration)
			   STA OFFSET_A
			   INY
			   LDA (PC_A),Y
			   STA OFFSET_A+1

			   LDA #6					; Bump the PC
			   JMP ADD_PC_A

;
; BEND_ON_B - Acctivates the Pitchbender & sets up the pitchbend data
;

;BEND_ON_B		LDA #$81				 ;			 Bend on_ Minus = init bend
;			   STA PORTFLAG_B
;			   LDA (PC_B),Y
;			   STA BEND_DIR_B			;	  Pitchbend direction
;			   INY
;			   LDA (PC_B),Y
;			   STA BENDSTEP_B			;	  Bend steps
;			   INY
;			   LDA (PC_B),Y
;			   STA BENDLENGTH_B
;			   INY
;			   LDA (PC_B),Y				;		Bend offset (Rate x Duration)
;			   STA OFFSET_B
;			   INY
;			   LDA (PC_B),Y
;			   STA OFFSET_B+1
;
;			   LDA #6
;			   JMP ADD_PC_B

;
; BEND_ON_C - Acctivates the Pitchbender & sets up the pitchbend data
;

;BEND_ON_C		LDA #$81				 ;			 Bend on_ Minus = init bend
;			  STA PORTFLAG_C
;			  LDA (PC_C),Y
;			 STA BEND_DIR_C			  ;		Pitchbend direction
;			 INY
;			 LDA (PC_C),Y
;			 STA BENDSTEP_C			  ;		Bend steps
;			 INY
;			  LDA (PC_C),Y
;			  STA BENDLENGTH_C
;			 INY
;			  LDA (PC_C),Y			   ;	   Bend offset (Rate x Duration)
;			 STA OFFSET_C
;			 INY
;			 LDA (PC_C),Y
;			 STA OFFSET_C+1
;
;			 LDA #6
;			 JMP ADD_PC_C


;
; PATCH_A - Pokes sound patch into SID chip Channel A
;

PATCH_A		   LDA #0
			   STA FROM+1				; Zeroise FROM+1
			   LDA (PC_A),Y				; Get patch number
			   ASL
			   ASL
		   CLC
			   ADC (PC_A),Y				; *5
			   ADC #<PATCH_START
			   STA FROM
			   LDA FROM+1				; Plus patch base address
			   ADC #>PATCH_START
			   STA FROM+1

			   LDY #4
			   LDA (FROM),Y
			   STA GATEOFF_A			; Get the gateoff value
			   DEY
			   LDX #0
			   LDA (FROM),Y				; Minus = gap mode
			   BPL NO_GAP_A
			   INX
NO_GAP_A	   AND #$7F					; Get rid of bit 7
			   STA SUSTAIN_A
			   STX GAPFLAG_A			; Set the gap flag (nonzero)
			   DEY
			   LDA (FROM),Y				; Get SR period
			   STA $D406
			   STA SUSTEMP+0			; Preserve this
			   DEY
			   LDA (FROM),Y
			   STA $D405				; Get the AD period
			   STA ADTEMP+0				; Preserve this
			   DEY
			   LDA (FROM),Y
			   STA GATE_A				; Get the gateon value

			   LDA #2					; Bump the PC
			   JMP ADD_PC_A

;
; PATCH_B - Pokes sound patch into SID chip Channel B
;

PATCH_B		   LDA #0					; Ditto
			   STA FROM+1
			   LDA (PC_B),Y
			   ASL
			   ASL
		   CLC
			   ADC (PC_B),Y
			   ADC #<PATCH_START
			   STA FROM
			   LDA FROM+1
			   ADC #>PATCH_START
			   STA FROM+1

			   LDY #4
			   LDA (FROM),Y
			   STA GATEOFF_B
			   DEY
			   LDX #0
			   LDA (FROM),Y
			   BPL NO_GAP_B
			   INX
NO_GAP_B	   AND #$7F
			   STA SUSTAIN_B
			   STX GAPFLAG_B
			   DEY
			   LDA (FROM),Y
			   STA $D406+7
			   STA SUSTEMP+1
			   DEY
			   LDA (FROM),Y
			   STA $D405+7
			   STA ADTEMP+1
			   DEY
			   LDA (FROM),Y
			   STA GATE_B

			   LDA #2
			   JMP ADD_PC_B

;
; PATCH_C - Pokes sound patch into SID chip Channel C
;

PATCH_C		   LDA #0
			   STA FROM+1
			   LDA (PC_C),Y
			   ASL
			   ASL
		   CLC
			   ADC (PC_C),Y
			   ADC #<PATCH_START
			   STA FROM
			   LDA FROM+1
			   ADC #>PATCH_START
			   STA FROM+1

			   LDY #4
			   LDA (FROM),Y
			   STA GATEOFF_C
			   DEY
			   LDX #0
			   LDA (FROM),Y
			   BPL NO_GAP_C
			   INX
NO_GAP_C	   AND #$7F
			   STA SUSTAIN_C
			   STX GAPFLAG_C
			   DEY
			   LDA (FROM),Y
			   STA $D406+14
			   STA SUSTEMP+2
			   DEY
			   LDA (FROM),Y
			   STA $D405+14
			   STA ADTEMP+2
			   DEY
			   LDA (FROM),Y
			   STA GATE_C

			   LDA #2
			   JMP ADD_PC_C

;
; DRUM_C - Gets new drum table pointers (DunnyDrums tm)
;

DRUM_C		   LDA (PC_C),Y
			   STA DRUMadd
			   ASL
		   CLC
			   ADC DRUMadd
			   ASL
			   ADC DRUMadd
			   TAX
			   LDA DRUMTABLE,X			;GET LO & HI BYTE OF WVFORM TABLE
			   STA WAVES_L+2			;
			   LDA DRUMTABLE+1,X		;
			   STA WAVES_H+2			;
			   LDA DRUMTABLE+2,X		;"	 ""	   "  ""   " FR=ENCY TABLE
			   STA FREQS_L+2			;
			   LDA DRUMTABLE+3,X		;
			   STA FREQS_H+2			;
			   LDA DRUMTABLE+4,X		;GET LENGTH OF DRUM TABLE
			   STA DRUMlength+2			;
			   LDA DRUMTABLE+5,X		;GET VOLUME (SUTAIAN/RELEASE)
			   STA drumFLAG+2			;
			   STA $D406+14				;
			   LDA DRUMTABLE+6,X		;GET PULSE WIDTH
			   STA $D403+14
			   LDA #$00					;ATTACK/DECAY
			   STA $D405+14				;FOR DRUMSOUND

			   INY
			   LDA (PC_C),Y
			   STA DRUMtr+2

			   LDA DURATIONON_C
			   BNE GRAB_DUR_C

			   LDA DURATION_C
			   STA DURCOUNT_C

			   LDA #3
ADD_N_FALL_C	CLC
		ADC PC_C
			   STA PC_C
			   BCC NOLOW_C
			   INC PC_C+1
NOLOW_C		   RTS

GRAB_DUR_C	   INY
			   LDA (PC_C),Y
			   STA DURCOUNT_C

			   LDA #4
			   JMP ADD_N_FALL_C

;----------DRUM PROGRAM--------------------

DRUMMER		   LDX #2
NEXT_DRUM	   LDA drumFLAG,X
			   BEQ QUITDRUM
			   LDA DRUMx,X
			   CMP DRUMlength,X
			   BNE DRUMcont
			   LDA #0
			   STA $D417
			   STA DRUMx,X
			   STA drumFLAG,X
			   LDY SID_OFFSETS,X
;	 LDA SUSTEMP,X
;	 STA $D406,Y
;	 LDA ADTEMP,X
;	 STA $D405,Y
;	LDA #0
			   STA $D404,Y

QUITDRUM	   DEX
			   BPL NEXT_DRUM
			   RTS

DRUMcont	   STX TEMPX				; TEMPX+0 = Drum Number
			   STA TEMPX+1				; TEMPX+1 = Drum's position in table

			   LDA FREQS_L,X
			   STA DRUMFQ+1
			   LDA FREQS_H,X
			   STA DRUMFQ+2

			   LDA WAVES_L,X
			   STA DRUMWV+1
			   LDA WAVES_H,X
			   STA DRUMWV+2

			   LDY SID_OFFSETS,X
			   LDX TEMPX+1				;GET CURRENT VOICE

DRUMFQ		   LDA $DDDD,X				; LOAD FR=ENCY FROM TABLE  (S/M/C)

			   LDX TEMPX
		   CLC
			   ADC DRUMtr,X				; ADD TRANSPOSE BYTE
			   LDX TEMPX+1

			   STA $D401,Y				; STORE IN SID FRQ HI
DRUMWV		   LDA $DDDD,X				; LOAD WAVEFORM FROM TABLE	 (S/M/C)
			   STA $D404,Y				; STORE IN SID WAVEFORM

			   LDX TEMPX
			   INC DRUMx,X
			   DEX
			   BMI DRUMS_DONE

			   JMP NEXT_DRUM

DRUMS_DONE	   RTS

BITABLE		   !by 1,2,4

;
; END_A - Terminates the tune on that voice
;

END_A		   LDA #0
			   STA MFL_A
			   STA PORTFLAG_A
			   STA VIBFLAG_A
			   STA ARPFLAG_A
			   STA PULSEFLAG_A
			   RTS

END_B		   LDA #0
			   STA MFL_B
			   STA PORTFLAG_B
			   STA VIBFLAG_B
			   STA ARPFLAG_B
			   STA PULSEFLAG_B
			   RTS

END_C		   LDA #0
			   STA MFL_C
			   STA PORTFLAG_C
			   STA VIBFLAG_C
			   STA ARPFLAG_C
			   STA PULSEFLAG_C
			   RTS

DRUM_A
DRUM_B
BEND_ON_B
BEND_ON_C
EFFECT_A
EFFECT_B
NOTHING		   RTS

;
; FINITO
;

;
; Control tables...
;

JUMPVECL_A	   !by <JUMP_A,<CALL_A,<RET_A,<FOR_HANDLE_A,<NEXT_HANDLE_A
			   !by <BEND_ON_A,<PATCH_A,<LENGTH_A,<VIBON_A,<VIBOFF_A
			   !by <ARPON_A,<ARPOFF_A,<DUR_ON_A,<PULSEON_A,<NOTHING
			   !by <END_A,<TRANS_A,<CT_A,<NOTHING,<MODE_A,<DRUM_A
			   !by <PORT_A,<CODE_A,<EFFECT_A

JUMPVECH_A	   !by >JUMP_A,>CALL_A,>RET_A,>FOR_HANDLE_A,>NEXT_HANDLE_A
			   !by >BEND_ON_A,>PATCH_A,>LENGTH_A,>VIBON_A,>VIBOFF_A
			   !by >ARPON_A,>ARPOFF_A,>DUR_ON_A,>PULSEON_A,>NOTHING
			   !by >END_A,>TRANS_A,>CT_A,>NOTHING,>MODE_A,>DRUM_A
			   !by >PORT_A,>CODE_A,>EFFECT_A

JUMPVECL_B	   !by <JUMP_B,<CALL_B,<RET_B,<FOR_HANDLE_B,<NEXT_HANDLE_B
			   !by <BEND_ON_B,<PATCH_B,<LENGTH_B,<VIBON_B,<VIBOFF_B
			   !by <ARPON_B,<ARPOFF_B,<DUR_ON_B,<PULSEON_B,<NOTHING
			   !by <END_B,<TRANS_B,<CT_B,<NOTHING,<MODE_B,<DRUM_B
			   !by <PORT_B,<CODE_B,<EFFECT_B

JUMPVECH_B	   !by >JUMP_B,>CALL_B,>RET_B,>FOR_HANDLE_B,>NEXT_HANDLE_B
			   !by >BEND_ON_B,>PATCH_B,>LENGTH_B,>VIBON_B,>VIBOFF_B
			   !by >ARPON_B,>ARPOFF_B,>DUR_ON_B,>PULSEON_B,>NOTHING
			   !by >END_B,>TRANS_B,>CT_B,>NOTHING,>MODE_B,>DRUM_B
			   !by >PORT_B,>CODE_B,>EFFECT_B

JUMPVECL_C	   !by <JUMP_C,<CALL_C,<RET_C,<FOR_HANDLE_C,<NEXT_HANDLE_C
			   !by <BEND_ON_C,<PATCH_C,<LENGTH_C,<VIBON_C,<VIBOFF_C
			   !by <ARPON_C,<ARPOFF_C,<DUR_ON_C,<PULSEON_C,<NOTHING
			   !by <END_C,<TRANS_C,<CT_C,<NOTHING,<MODE_C,<DRUM_C
			   !by <PORT_C,<CODE_C,<EFFECT_C

JUMPVECH_C	   !by >JUMP_C,>CALL_C,>RET_C,>FOR_HANDLE_C,>NEXT_HANDLE_C
			   !by >BEND_ON_C,>PATCH_C,>LENGTH_C,>VIBON_C,>VIBOFF_C
			   !by >ARPON_C,>ARPOFF_C,>DUR_ON_C,>PULSEON_C,>NOTHING
			   !by >END_C,>TRANS_C,>CT_C,>NOTHING,>MODE_C,>DRUM_C
			   !by >PORT_C,>CODE_C,>EFFECT_C

STACKDEPTH	   = 5
STACK_A_L	   !fill STACKDEPTH,0
STACK_B_L	   !fill STACKDEPTH,0
STACK_C_L	   !fill STACKDEPTH,0
STACK_A_H	   !fill STACKDEPTH,0
STACK_B_H	   !fill STACKDEPTH,0
STACK_C_H	   !fill STACKDEPTH,0
FOR_COUNTS_A   !fill STACKDEPTH,0
FOR_COUNTS_B   !fill STACKDEPTH,0
FOR_COUNTS_C   !fill STACKDEPTH,0

SID_OFFSETS	   !by 0,7,14
WORD_OFFSET	   !by 0,2,4

LOWFREQ		   !by <N00,<N01,<N02,<N03,<N04,<N05,<N06,<N07,<N08,<N09
			   !by <N10,<N11,<N12,<N13,<N14,<N15,<N16,<N17,<N18,<N19
			   !by <N20,<N21,<N22,<N23,<N24,<N25,<N26,<N27,<N28,<N29
			   !by <N30,<N31,<N32,<N33,<N34,<N35,<N36,<N37,<N38,<N39
			   !by <N40,<N41,<N42,<N43,<N44,<N45,<N46,<N47,<N48,<N49
			   !by <N50,<N51,<N52,<N53,<N54,<N55,<N56,<N57,<N58,<N59
			   !by <N60,<N61,<N62,<N63,<N64,<N65,<N66,<N67,<N68,<N69
			   !by <N70,<N71,<N72,<N73,<N74,<N75,<N76,<N77,<N78,<N79
			   !by <N80,<N81,<N82,<N83,<N84,<N85,<N86,<N87,<N88,<N89
			   !by <N90,<N91,<N92,<N93,<0

HIFREQ		   !by >N00,>N01,>N02,>N03,>N04,>N05,>N06,>N07,>N08,>N09
			   !by >N10,>N11,>N12,>N13,>N14,>N15,>N16,>N17,>N18,>N19
			   !by >N20,>N21,>N22,>N23,>N24,>N25,>N26,>N27,>N28,>N29
			   !by >N30,>N31,>N32,>N33,>N34,>N35,>N36,>N37,>N38,>N39
			   !by >N40,>N41,>N42,>N43,>N44,>N45,>N46,>N47,>N48,>N49
			   !by >N50,>N51,>N52,>N53,>N54,>N55,>N56,>N57,>N58,>N59
			   !by >N60,>N61,>N62,>N63,>N64,>N65,>N66,>N67,>N68,>N69
			   !by >N70,>N71,>N72,>N73,>N74,>N75,>N76,>N77,>N78,>N79
			   !by >N80,>N81,>N82,>N83,>N84,>N85,>N86,>N87,>N88,>N89
			   !by >N90,>N91,>N92,>N93,>0

DRIVER_END	   = *

;
;------------------------------------------------------------------------------
; MUSIC DATA
;------------------------------------------------------------------------------
;

DATA_START	   = *

TUNETABLE_A_L  !by <DITTY1,<SUCCESS1,<FAIL1,<EVENT1,<TITLE1,<QUAL1
TUNETABLE_A_H  !by >DITTY1,>SUCCESS1,>FAIL2,>EVENT1,>TITLE1,>QUAL1

TUNETABLE_B_L  !by <DITTY2,<SUCCESS2,<FAIL2,<EVENT2,<TITLE2,<QUAL2
TUNETABLE_B_H  !by >DITTY2,>SUCCESS2,>FAIL2,>EVENT2,>TITLE2,>QUAL2

TUNETABLE_C_L  !by <DITTY3,<SUCCESS3,<FAIL3,<EVENT3,<TITLE3,<QUAL3
TUNETABLE_C_H  !by >DITTY3,>SUCCESS3,>FAIL3,>EVENT3,>TITLE3,>QUAL3

;-----------------------------------------------------------------------------
TEMPO		   = 5					  ;	 TEMPO CONTROL

SQ			   = TEMPO				  ;	 SEMI-QUAVER
QV			   = SQ*2				  ;	 QUAVER
CR			   = QV*2				  ;	 CROTCHET
DCR			   = CR+QV
MN			   = CR*2				  ;	 MINIM
SB			   = MN*2				  ;	 SEMI-BREVE
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
TEMPO2		   = 6					  ;	 TEMPO CONTROL

SQ2			   = TEMPO2				  ;	 SEMI-QUAVER
QV2			   = SQ2*2				  ;	 QUAVER
DQV2		   = SQ2+QV2			  ;	 QUAVER
CR2			   = QV2*2				  ;	 CROTCHET
DCR2		   = CR2+QV2
MN2			   = CR2*2				  ;	 MINIM
SB2			   = MN2*2				  ;	 SEMI-BREVE
;-----------------------------------------------------------------------------

UP			   = 1
DOWN		   = $80
GAP			   = $80
NO			   = 0
FILTIT		   = 1

REST		   = 94
JUMP		   = $80
CALL		   = $81
RET			   = $82
LOOP		   = $83
NEXT		   = $84
BEND		   = $85
PATCH		   = $86
LENGTH		   = $87
VIBON		   = $88
VIBOFF		   = $89
ARPON		   = $8A
ARPOFF		   = $8B
MANUAL		   = $8C
PWM			   = $8D
FILTER		   = $8E
END			   = $8F
TRANSPOSE	   = $90
CT			   = $91
FILTOFF		   = $92
MODE		   = $93
DRUM		   = $94
PORT		   = $95
CODE		   = $96
EFFECT		   = $97



;----DISQUALIFIED  JINGLE-----------------------------------------------------

DITTY2
			!by PATCH,1,PWM,1
			!by ARPON
			!wo ARP2
			!by LENGTH,SQ

			!by CALL
			!wo MEL1
			!by CT,-5
			!wo MEL1
			!by CT,-12
			!wo MEL1
			!by CT,-17
			!wo MEL1

			!by D4
			!by END

MEL1
			!by LOOP,2
			!by F4,G4,F4,D5
			!by NEXT
			!by RET
DITTY1
			!by PATCH,0,PWM,0,VIBON,0,7,6
			!by LOOP,2,D2,CR+QV,D2,CR+QV,D2,CR,NEXT
			!by END

DITTY3
		   !by LOOP,2
		   !by DRUM,0,2,SQ
		   !by DRUM,2,0,SQ
		   !by DRUM,2,0,SQ
		   !by DRUM,2,0,SQ
		   !by DRUM,1,0,QV
		   !by DRUM,2,0,SQ
		   !by DRUM,2,0,SQ
		   !by DRUM,0,2,SQ
		   !by DRUM,2,0,SQ
		   !by DRUM,0,2,SQ
		   !by DRUM,2,0,SQ
		   !by DRUM,1,0,QV
		   !by DRUM,2,0,SQ
		   !by DRUM,2,0,SQ
		   !by NEXT
		   !by DRUM,3,0,QV
		   !by END
ARP1	   !by 3,4,0,4,7,9
ARP2	   !by 1,3,24,12,0
;-----------------------------------------------------------------------------
;----------SUCCESS------------------------------------------------------------
;-----------------------------------------------------------------------------
SUCCESS1	!by REST,2
			!by CALL
			!wo FMEL1
			!by END
SUCCESS2
			!by CALL
			!wo FMEL1
			!by END

FMEL1
			!by PATCH,3,PWM,3
			!by ARPON
			!wo FARP1
			!by REST,CR2
			!by FS4,CR2
			!by ARPON
			!wo FARP2
			!by FS4,CR2
			!by ARPON
			!wo FARP1
			!by FS4,CR2
			!by ARPON
			!wo FARP2
			!by FS4,CR2
			!by ARPON
			!wo FARP1
			!by FS4,CR2
			!by ARPON
			!wo FARP2
			!by FS4,QV2
			!by ARPON
			!wo FARP3
			!by FS4,QV2+CR2
			!by RET
;-----------------------------------------------------------------------------
SUCCESS3
			!by PATCH,2,PWM,2,VIBON,0,7,6

			!by LOOP,2
			!by A2,QV2,A2,SQ2,A1,SQ2,DRUM,3,0,QV2,A2,QV2
			!by NEXT
			!by E2,QV2,E2,SQ2,E3,SQ2,DRUM,3,0,QV2,E2,QV2
			!by D2,QV2,D2,SQ2,D1,SQ2,DRUM,3,0,QV2
			!by END
FARP1		!by 2,4,0,3,7,12
FARP2		!by 2,4,0,3,8,12
FARP3		!by 2,4,0,3,10,12

;-----------------------------------------------------------------------------
;-------------FAIL------------------------------------------------------------
;-----------------------------------------------------------------------------
FAIL1
			!by PATCH,4,PWM,4
			!by VIBON,6,$20,6
			!by LENGTH,CR
			!by C5
			!by MODE,%00000010
			!by B4,A4,G4,F4,E4,D4,C4,B3
			!by CODE
			!wo TURNOFF
			!by END
FAIL2
			!by REST,QV
			!by PATCH,4,PWM,4
			!by VIBON,6,$40,6
			!by LENGTH,CR
			!by F4
			!by MODE,%00000010
			!by E4,D4,C4,B3,A3,G3,F3,REST
			!by END

FAIL3
			!by PATCH,5,ARPON
			!wo ARP2
			!by DRUM,4,-9,QV
			!by DRUM,4,-9,QV
			!by DRUM,4,-9,QV
			!by C5,QV
			!by DRUM,3,15,QV
			!by C4,QV,G5,QV
			!by DRUM,4,-9,CR
			!by DRUM,4,-9,QV
			!by DRUM,4,-9,QV
			!by E5,QV
			!by DRUM,3,15,QV
			!by C4,QV,G5,QV
			!by DRUM,4,-9,QV
			!by B4,CR

			!by END

;-----------------------------------------------------------------------------
;------NEXT EVENT-------------------------------------------------------------
;-----------------------------------------------------------------------------
EVENT1
			!by PATCH,6,PWM,5,VIBON,0,7,6
			!by CODE
			!wo RESETVOL
			!by LENGTH,QV2
			!by LOOP,9
CURRENTVOL
			!by PORT,1,$18
			!by A2
			!by CODE
			!wo FADEUP
			!by NEXT
			!by REST,REST
			!by CODE
			!wo TURNOFF
			!by END

EVENT2
			!by PATCH,7,PWM,6,ARPON
			!wo NARP1
			!by LENGTH,SB2+QV2
			!by A4
			!by END

EVENT3
			!by LOOP,16*2
			!by DRUM,2,-1,SQ2/2
			!by NEXT
			!by DRUM,3,0,QV2
			!by END
;-----------------------------------------------------------------------------
RESETVOL
			LDA #3
			STA CURRENTVOL+1
			RTS

FADEUP		INC CURRENTVOL+1
			RTS

NARP1	   !by 2,5,0,4,7,9,12

;-----------------------------------------------------------------------------
;-------TITLE MUSIC-----------------------------------------------------------
;-----------------------------------------------------------------------------

TITLE1
			!by PATCH,9,PWM,9
			!by VIBON,0,$80,6		 ;0
			!by LOOP,4				 ;
			!by CS5,SB2				 ;
			!by A4,MN2,FS4,MN2		 ;
			!by NEXT				 ;8
			!by A4,SB2*2			 ;10
			!by REST,SB2*2

			!by LOOP,2

			!by PATCH,13,PWM,11
			!by VIBON,0,$40,6
			!by CALL
			!wo TMEL1
			!by CALL
			!wo TMEL1

			!by PATCH,14,VIBON,8,$E0,6
			!by CALL
			!wo TMEL2
			!by CALL
			!wo TMEL2

			!by NEXT				 ;PLAY VERSE+CHORUS TWICE

			!by PATCH,15,PWM,12		 ;
			!by VIBON,8,$20,8		;		 MIDDLE 8
			!by LOOP,2				 ;		  --------
			!by FS4,MN2,E4,SB2
			!by CS4,QV2,D4,QV2,CS4,QV2,D4,QV2
			!by CS4,SB2*2
			!by NEXT
			!by REST,SB2

			!by PATCH,17,PWM,4
			!by ARPON
			!wo ARP2
			!by LENGTH,SQ2
			!by LOOP,4*2
			!by E4,FS4,A4,E4,FS4,B4,D4,FS4,A4,D4,E4,CS4,A4,E4,FS4,B4
			!by NEXT

			!by MANUAL
			!by PATCH,18,PWM,11
			!by VIBON,$10,$1A,8
			!by CT,-12
			!wo TMEL1
			!by CT,-12
			!wo TMEL1

			!by PATCH,14,VIBON,8,$E0,6
			!by CALL
			!wo TMEL2
			!by CALL
			!wo TMEL2

			!by LOOP,4,REST,SB2*2,NEXT;			LET BASS + BACK TAKE OVER

			!by PATCH,14,VIBON,8,$E0,6
			!by CODE
			!wo RESETFADE
			!by LOOP,8
FADEPOS
			!by PORT,$0F,$18
			!by E5,QV2,FS5,QV2,A5,QV2
			!by BEND,UP,$80,11
			!wo $80*11
			!by B5,CR2,A5,CR2+QV2+SB2
			!by CODE
			!wo TFADEAWAY
			!by NEXT
			!by PORT,$00,$18
			!by REST,SB*2
			!by REST,SB*2
			!by PORT,$0F,$18

			!by JUMP
			!wo TITLE1

TFADEAWAY	   LDA FADEPOS+1
			   CMP #1
			   BEQ MISSFADE
			   DEC FADEPOS+1
			   DEC FADEPOS+1
MISSFADE	   RTS

RESETFADE	   LDA #$0F
			   STA FADEPOS+1
			   RTS
;-----------------------------------------------------------------------------
TITLE2
			!by PATCH,10,PWM,3
			!by LOOP,5
			!by CALL
			!wo TBACK1
			!by NEXT
			!by PATCH,12
			!by LOOP,1+4+4+4+4
			!by CALL
			!wo TBACK1
			!by NEXT

			!by ARPON
			!wo TARP4	  ;		   MIDDLE 8
			!by PATCH,16				;		 ========
			!by LOOP,4				;
			!by CALL
			!wo TBACK2
			!by NEXT
			!by REST,SB2

			!by PATCH,12
			!by LOOP,4+4+4
			!by CALL
			!wo TBACK1
			!by NEXT

			!by ARPON
			!wo TARP4	  ;		   MIDDLE 8
			!by PATCH,16				;		 ========
			!by LOOP,4				;
			!by CALL
			!wo TBACK2
			!by NEXT

			!by PATCH,12				;
			!by LOOP,8				; REPEAT TO FADE
			!by CALL
			!wo TBACK1	   ;
			!by NEXT					;

			!by REST,SB*2
			!by REST,SB*2
			!by JUMP
			!wo TITLE2
;-----------------------------------------------------------------------------
TITLE3
			!by PATCH,9,PWM,9
			!by VIBON,0,$80,6		;0
			!by LOOP,4				;
			!by A4,SB2				;
			!by E4,MN2,D4,MN2		;
			!by NEXT					;8
			!by E4,SB2*2				;10
			!by PATCH,11,PWM,10
			!by EFFECT,C7,MODE,%00001000
			!by VIBON,0,7,6
			!by LOOP,1+4+4+4+4
			!by CALL
			!wo TBASS1
			!by NEXT

			!by LOOP,4				;
			!by CALL
			!wo TBASS2	   ;MIDDLE8
			!by CT,-2
			!wo TBASS2	  ;
			!by NEXT					;

			!by FS2,QV2,G2,QV2
			!by DRUM,3,0,CR2
			!by CODE
			!wo RESTOREPOS
DRUMPOS
			!by LOOP,16,DRUM,2,-9,SQ2/2,CODE
			!wo UPDRUM
			!by NEXT

			!by LOOP,4+4+4
			!by CALL
			!wo TBASS1
			!by NEXT

			!by LOOP,3
			!by CALL
			!wo TBASS3
			!by CT,-2
			!wo TBASS3
			!by NEXT
			!by CALL
			!wo TBASS3
			!by CALL
			!wo TBASS4
			!by DRUM,3,3,QV2
			!by DRUM,3,3,QV2
			!by LOOP,4
			!by DRUM,3,3,SQ2
			!by NEXT

			!by LOOP,8				;
			!by CALL
			!wo TBASS1	   ; REPEAT TO FADE
			!by NEXT					;

			!by CODE
			!wo EFFECTOFF_C

			!by REST,SB*2
			!by REST,SB*2
			!by JUMP
			!wo TITLE3

;-----------------------------------------------------------------------------
UPDRUM		   INC DRUMPOS+4
			   RTS
RESTOREPOS	   LDA #-9
			   STA DRUMPOS+4
			   RTS
EFFECTOFF_C	   LDA #0
			   STA EFFECTFLAG_C
			   RTS
EFFECTON_C	   LDA #1
			   STA EFFECTFLAG_C
			   RTS

;-----------------------------------------------------------------------------
TBACK1
			!by ARPON
			!wo TARP1
			!by REST,CR2
			!by FS4,CR2
			!by ARPON
			!wo TARP2
			!by FS4,CR2
			!by ARPON
			!wo TARP1
			!by FS4,CR2
			!by ARPON
			!wo TARP2
			!by FS4,CR2
			!by ARPON
			!wo TARP1
			!by FS4,CR2
			!by ARPON
			!wo TARP2
			!by FS4,QV2
			!by ARPON
			!wo TARP3
			!by FS4,QV2+CR2
			!by RET

TBACK2
			!by CS4,CR2,CS4,QV2,CS4,QV2,CS4,CR2,CS4,QV2,CS4,QV2
			!by CS4,QV2,REST,QV2,CS4,QV2,CS4,CR2,CS4,QV2,CS4,CR2
			!by RET

TBASS1
			!by A2,SQ2,A2,SQ2,A2,QV2,DRUM,3,0,QV2,A2,QV2
			!by A2,QV2,A2,SQ2,A1,SQ2,DRUM,3,3,QV2,DRUM,3,3,SQ2
			!by A2,SQ2
			!by E2,QV2,E3,SQ2,E2,SQ2,DRUM,3,0,QV2,E2,QV2
			!by D2,QV2,D3,SQ2,D2,SQ2,DRUM,3,3,QV2,DRUM,3,3,SQ2,D3,SQ2
			!by RET

TBASS2
			!by FS2,SQ2,FS2,SQ2,FS2,QV2,DRUM,3,0,QV2,FS2,QV2
			!by FS2,QV2,FS2,SQ2,FS1,SQ2,DRUM,3,3,QV2,DRUM,3,3,SQ2
			!by FS2,SQ2
			!by RET

TBASS3		   !by FS2,SQ2,FS2,SQ2,FS2,QV2,FS3,QV2,FS2,QV2
TBASS4		   !by FS2,QV2,FS2,SQ2,FS1,SQ2,FS2,QV2,FS3,SQ2
			   !by FS2,SQ2
			   !by RET


TMEL1
			!by FS5,MN2,E5,SB2
			!by CS5,QV2,D5,QV2,CS5,QV2,D5,QV2,CS5,QV2
			!by A4,CR2,E4,MN2+QV2+SB2
			!by RET

TMEL2
			!by E5,QV2,FS5,QV2,A5,QV2
			!by BEND,UP,$80,11
			!wo $80*11
			!by B5,CR2,A5,CR2+QV2+SB2

TMEL22
			!by E5,QV2,FS5,QV2,A5,QV2
			!by BEND,UP,$80,11
			!wo $80*11
			!by B5,CR2,A5,CR2+QV2,B5,CR2
			!by BEND,UP,$90,11
			!wo $90*11
			!by CS6,CR2,B5,QV2,A5,CR2+QV2
			!by RET

TARP1		!by 2,3,0,3,7
TARP2		!by 2,3,0,3,8
TARP3		!by 2,3,0,3,10
TARP4		!by 1,3,0,3,5

;-----------------------------------------------------------------------------
QUAL1
			!by PATCH,18,PWM,11
			!by VIBON,$04,$1A,6
			!by BEND,UP,$50,8
			!wo $50*8
			!by C5,DQV2,G4,DQV2,C4,QV2
			!by VIBON,$0E,$20,8
			!by BEND,UP,$50,11
			!wo $50*11
			!by DS4,MN2,F4,MN2,G4,SB2
			!by END
QUAL2
		   !by PATCH,18,PWM,11
		   !by VIBON,$0E,$20,8
		   !by REST,3
		   !by C5,DQV2,G4,DQV2,C4,QV2-3
		   !by AS4,MN2,A4,MN2,B4,SB2
		   !by CODE
		   !wo TURNOFF
		   !by END

QUAL3
			!by REST,MN2
			!by VIBON,0,7,6
			!by PWM,10
			!by EFFECT,C7,MODE,%00001000
			!by CALL
			!wo QBASS1
			!by CT,2
			!wo QBASS1
			!by CT,4
			!wo QBASS1
			!by CT,4
			!wo QBASS1
			!by END
QBASS1
			!by PATCH,11
			!by DS2,SQ2,DS3,SQ2
			!by PATCH,13
			!by CODE
			!wo EFFECTOFF_C
			!by DS5,SQ2,DS5,SQ2,DRUM,3,5,QV2
			!by CODE
			!wo EFFECTON_C
			!by PATCH,11,DS1,QV2
			!by RET
;---PATCH TABLE---------------------------------------------------------------
PATCH_START
			!by $51,$00,$C8,3,$40	;	00
			!by $01,$00,$88,2,$42	;	01
			!by $55,$00,$99,3,$40	;	02
			!by $41,$00,$88,QV2+GAP,$10; 03
			!by $55,$00,$A9,3,$40	;	04
			!by $81,$00,$A8,3,$10	;	05
			!by $41,$00,$99,3,$40	;	06
			!by $41,$00,$59,9+GAP,$40;	07
			!by $41,$00,$6A,6+GAP,$10;	08
			!by $11,$AA,$79,QV2+SQ2+GAP,$10; 09
			!by $41,$00,$89,QV2+GAP,$20; 10
			!by $81,$00,$99,2,$40	;	11
			!by $41,$00,$89,QV2+GAP,$10; 12
			!by $41,$00,$88,4+GAP,$14;	13
			!by $11,$00,$A8,4+GAP,$10;	14
			!by $41,$00,$69,6+GAP,$20;	15
			!by $41,$00,$69,5+GAP,$10;	16
			!by $41,$00,$48,3+GAP,$10;	17
			!by $41,$00,$68,4+GAP,$14;	18
;---PWM TABLE-----------------------------------------------------------------
PWM_START
			!by $80,$00,$02
			!wo $0F50;	 00
			!by $70,$10,$08
			!wo $0000;	 01
			!by $40,$05,$05
			!wo $0100;	 02
			!by $30,$00,$02
			!wo $0000;	 03
			!by $F0,$04,$04
			!wo $0800;	 04
			!by $50,$05,$05
			!wo $0200;	 05
			!by $08,$00,$02
			!wo $0000;	 06
			!by $08,$00,$02
			!wo $0000;	 07
			!by $4F,$04,$04
			!wo $0600;	 08
			!by $08,$03,$03
			!wo $0800;	 09
			!by $30,$04,$02
			!wo $0100;	 10
			!by $50,$06,$04
			!wo $0000;	 11
			!by $20,$08,$07
			!wo $0100;	 12
			!by $50,$10,$0E
			!wo $0000;	 13

;-----DRUM TABLE & DATA---------------------
DRUMTABLE
			!wo DRUM1V,DRUM1F
			!by 4,$D8,$08
			!wo DRUM2V,DRUM2F
			!by 13,$D9,$07
			!wo DRUM3V,DRUM3F
			!by 5,$95,$02
			!wo DRUM4V,DRUM4F
			!by 15,$F8,$02
			!wo DRUM3V,DRUM3F
			!by 5,$F5,$02
;-----BASS DRUM------
DRUM1V		   !by $41,$41,$10,$80
DRUM1F		   !by $0E,$06,$0F,$10
;-----SNARE----------
DRUM2V		   !by $41,$41,$40,$80,$80,$80,$10,$80,$10,$80,$80,$10,$80
DRUM2F		   !by $11,$0C,$08,$70,$70,$2F,$0C,$2E,$0C,$2D,$BC,$0C,$2B
;-----HI-HAT---------
DRUM3V		   !by $11,$11,$80,$80,$80
DRUM3F		   !by $10,$10,$70,$20,$30
;-----SNARE 2--------
DRUM4V		   !by $11,$11,$80,$10,$80,$10,$80,$10,$80,$10,$80,$10,$80,$10,$80
DRUM4F		   !by $10,$10,$70,$0B,$24,$0B,$23,$0B,$22,$0B,$21,$0B,$20,$0B,$20


TURNOFF		   LDA #8
			   STA $D404
			   STA $D404+7
			   STA $D404+14
			   LDA #0
			   STA $D404
			   STA $D404+7
			   STA $D404+14
			   RTS


DATA_END	   = *
;------------------------------------------------------------------------------
VAR_START
FILT_TOGGLE	   !by 0
FILT_TOGFLAG   !by 0
DURCOUNT_A	   !by 0
DURCOUNT_B	   !by 0
DURCOUNT_C	   !by 0
TEMP_PC		   !by 0,0					; 2 bytes
GATE_A		   !by 0
GATE_B		   !by 0
GATE_C		   !by 0
SP_A		   !by 0
SP_B		   !by 0
SP_C		   !by 0
PORTFLAG_A	   !by 0
PORTFLAG_B	   !by 0
PORTFLAG_C	   !by 0
BEND_DIR_A	   !by 0
BEND_DIR_B	   !by 0
BEND_DIR_C	   !by 0
BENDSTEP_A	   !by 0
BENDSTEP_B	   !by 0
BENDSTEP_C	   !by 0
FREQ_LOW_A	   !by 0
FREQ_LOW_B	   !by 0
FREQ_LOW_C	   !by 0
FREQ_HI_A	   !by 0
FREQ_HI_B	   !by 0
FREQ_HI_C	   !by 0
OFFSET_A	   !by 0,0					;  2 bytes
OFFSET_B	   !by 0,0					;  2 bytes
OFFSET_C	   !by 0,0					;  2 bytes
BENDLENGTH_A   !by 0
BENDLENGTH_B   !by 0
BENDLENGTH_C   !by 0
RELEASE_A	   !by 0
RELEASE_B	   !by 0
RELEASE_C	   !by 0
SUSTAIN_A	   !by 0
SUSTAIN_B	   !by 0
SUSTAIN_C	   !by 0
DURATION_A	   !by 0
DURATION_B	   !by 0
DURATION_C	   !by 0
VIBFLAG_A	   !by 0
VIBFLAG_B	   !by 0
VIBFLAG_C	   !by 0
VIBDELAY_A	   !by 0
VIBDELAY_B	   !by 0
VIBDELAY_C	   !by 0
VIBDELAY2_A	   !by 0
VIBDELAY2_B	   !by 0
VIBDELAY2_C	   !by 0
VIBDEPTH_A	   !by 0
VIBDEPTH_B	   !by 0
VIBDEPTH_C	   !by 0
VIBDEPTH2_A	   !by 0
VIBDEPTH2_B	   !by 0
VIBDEPTH2_C	   !by 0
VIBRATE_A	   !by 0
VIBRATE_B	   !by 0
VIBRATE_C	   !by 0
VIB_DIR_A	   !by 0
VIB_DIR_B	   !by 0
VIB_DIR_C	   !by 0
NEXTDELAY_A	   !by 0
NEXTDELAY_B	   !by 0
NEXTDELAY_C	   !by 0
NEXTDELAY2_A   !by 0
NEXTDELAY2_B   !by 0
NEXTDELAY2_C   !by 0
ARPEGLEN_A	   !by 0
ARPEGLEN_B	   !by 0
ARPEGLEN_C	   !by 0
ARPEGIND_A	   !by 0
ARPEGIND_B	   !by 0
ARPEGIND_C	   !by 0
ARPFLAG_A	   !by 0
ARPFLAG_B	   !by 0
ARPFLAG_C	   !by 0
GATEOFF_A	   !by 0
GATEOFF_B	   !by 0
GATEOFF_C	   !by 0
NOTE_A		   !by 0
NOTE_B		   !by 0
NOTE_C		   !by 0
DURATIONON_A   !by 0
DURATIONON_B   !by 0
DURATIONON_C   !by 0
PULSEDIR_A	   !by 0
PULSEDIR_B	   !by 0
PULSEDIR_C	   !by 0
RATEUP_A	   !by 0
RATEUP_B	   !by 0
RATEUP_C	   !by 0
RATEDOWN_A	   !by 0
RATEDOWN_B	   !by 0
RATEDOWN_C	   !by 0
CUP_A		   !by 0
CUP_B		   !by 0
CUP_C		   !by 0
CUP2_A		   !by 0
CUP2_B		   !by 0
CUP2_C		   !by 0
CDOWN_A		   !by 0
CDOWN_B		   !by 0
CDOWN_C		   !by 0
CDOWN2_A	   !by 0
CDOWN2_B	   !by 0
CDOWN2_C	   !by 0
PWL_A		   !by 0
PWL_B		   !by 0
PWL_C		   !by 0
PWL2_A		   !by 0
PWL2_B		   !by 0
PWL2_C		   !by 0
PWH_A		   !by 0
PWH_B		   !by 0
PWH_C		   !by 0
PWH2_A		   !by 0
PWH2_B		   !by 0
PWH2_C		   !by 0
PULSEFLAG_A	   !by 0
PULSEFLAG_B	   !by 0
PULSEFLAG_C	   !by 0
GAPFLAG_A	   !by 0
GAPFLAG_B	   !by 0
GAPFLAG_C	   !by 0
MFL_A		   !by 0
MFL_B		   !by 0
MFL_C		   !by 0
TRANSVAL_A	   !by 0
TRANSVAL_B	   !by 0
TRANSVAL_C	   !by 0
MODEBYTE_A	   !by 0
MODEBYTE_B	   !by 0
MODEBYTE_C	   !by 0
VOICE_NUM	   !by 0
DRUMadd		   !by 0
DRUMx		   !fill 3,0
drumFLAG	   !fill 3,0
DRUMtr		   !fill 3,0
DRUMlength	   !fill 3,0
WAVES_L		   !fill 3,0
WAVES_H		   !fill 3,0
FREQS_L		   !fill 3,0
FREQS_H		   !fill 3,0
VOLUMES		   !fill 3,0
RELEASE_TEMP   !by 0
ATTACK_TEMP	   !by 0
TEMP_VAL	   !by 0
EFFECTFLAG_A   !by 0
EFFECTFLAG_B   !by 0
EFFECTFLAG_C   !by 0
EFFECT_NOTE_A  !by 0
EFFECT_NOTE_B  !by 0
EFFECT_NOTE_C  !by 0
REAL_NOTE_A	   !by 0
REAL_NOTE_B	   !by 0
REAL_NOTE_C	   !by 0
TEMPX		   !by 0,0					; 2 Bytes
SUSTEMP		   !fill 3,0
ADTEMP		   !fill 3,0
MOD_OFF_A	   !by 0
MOD_OFF_B	   !by 0
MOD_OFF_C	   !by 0
VAR_END		   = *

PROG_END	   = *
;------------------------------------------------------------------------------

SID			   = $D400


; PAL Note fr=ency table

N00			   = 279
N01			   = 296
N02			   = 314
N03			   = 332
N04			   = 352
N05			   = 373
N06			   = 395
N07			   = 419
N08			   = 444
N09			   = 470
N10			   = 498
N11			   = 528
N12			   = 559
N13			   = 592
N14			   = 627
N15			   = 665
N16			   = 704
N17			   = 746
N18			   = 790
N19			   = 837
N20			   = 887
N21			   = 940
N22			   = 996
N23			   = 1055
N24			   = 1118
N25			   = 1184
N26			   = 1255
N27			   = 1330
N28			   = 1408
N29			   = 1492
N30			   = 1581
N31			   = 1675
N32			   = 1774
N33			   = 1880
N34			   = 1992
N35			   = 2110
N36			   = 2236
N37			   = 2369
N38			   = 2509
N39			   = 2659
N40			   = 2817
N41			   = 2984
N42			   = 3162
N43			   = 3350
N44			   = 3549
N45			   = 3760
N46			   = 3984
N47			   = 4220
N48			   = 4471
N49			   = 4737
N50			   = 5019
N51			   = 5317
N52			   = 5634
N53			   = 5969
N54			   = 6324
N55			   = 6700
N56			   = 7098
N57			   = 7520
N58			   = 7967
N59			   = 8441
N60			   = 8943
N61			   = 9475
N62			   = 10038
N63			   = 10635
N64			   = 11267
N65			   = 11937
N66			   = 12647
N67			   = 13399
N68			   = 14195
N69			   = 15040
N70			   = 15934
N71			   = 16881
N72			   = 17886
N73			   = 18949
N74			   = 20076
N75			   = 21270
N76			   = 22534
N77			   = 23875
N78			   = 25294
N79			   = 26798
N80			   = 28391
N81			   = 30080
N82			   = 31869
N83			   = 33764
N84			   = 35771
N85			   = 37898
N86			   = 40151
N87			   = 42540
N88			   = 45069
N89			   = 47749
N90			   = 50588
N91			   = 53596
N92			   = 56783
N93			   = 60160

C0			   = 0
CS0			   = 1
D0			   = 2
DS0			   = 3
E0			   = 4
F0			   = 5
FS0			   = 6
G0			   = 7
GS0			   = 8
A0			   = 9
AS0			   = 10
B0			   = 11
C1			   = 12
CS1			   = 13
D1			   = 14
DS1			   = 15
E1			   = 16
F1			   = 17
FS1			   = 18
G1			   = 19
GS1			   = 20
A1			   = 21
AS1			   = 22
B1			   = 23
C2			   = 24
CS2			   = 25
D2			   = 26
DS2			   = 27
E2			   = 28
F2			   = 29
FS2			   = 30
G2			   = 31
GS2			   = 32
A2			   = 33
AS2			   = 34
B2			   = 35
C3			   = 36
CS3			   = 37
D3			   = 38
DS3			   = 39
E3			   = 40
F3			   = 41
FS3			   = 42
G3			   = 43
GS3			   = 44
A3			   = 45
AS3			   = 46
B3			   = 47
C4			   = 48
CS4			   = 49
D4			   = 50
DS4			   = 51
E4			   = 52
F4			   = 53
FS4			   = 54
G4			   = 55
GS4			   = 56
A4			   = 57
AS4			   = 58
B4			   = 59
C5			   = 60
CS5			   = 61
D5			   = 62
DS5			   = 63
E5			   = 64
F5			   = 65
FS5			   = 66
G5			   = 67
GS5			   = 68
A5			   = 69
AS5			   = 70
B5			   = 71
C6			   = 72
CS6			   = 73
D6			   = 74
DS6			   = 75
E6			   = 76
F6			   = 77
FS6			   = 78
G6			   = 79
GS6			   = 80
A6			   = 81
AS6			   = 82
B6			   = 83
C7			   = 84
CS7			   = 85
D7			   = 86
DS7			   = 87
E7			   = 88
F7			   = 89
FS7			   = 90
G7			   = 91
GS7			   = 92
A7			   = 93
AS7			   = 94
B7			   = 95

}