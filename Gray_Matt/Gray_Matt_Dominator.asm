
	; Dominator tunes 4, 5 and 6
	; Player from MGEdit


	!to "Gray_Matt_Dominator.sid",plain

	* = $0000

	!text "PSID"
	!be16 2							; version 2
	!be16 $7c						; data offset
	!be16 $1000						; Load (0 = auto)
	!be16 INIT						; Init
	!be16 DRIVER					; Play
	!be16 3							; num songs
	!be16 1							; first song
	!word 0
	!word 0
-	!text "Dominator (4-6)"
	!fill 32 - (* - -)
-	!text "Matt Gray"
	!fill 32 - (* - -)
-	!text "1987 Matt Gray"
	!fill 32 - (* - -)
	!be16 $0014						; v2 flags
	!be16 0							; Start page, page length (reloc)
	!be16 0							; Reserved

!pseudopc $1000 {

;-----------------------------------------------------------------------------

                ;PLAYER V4.2
                ;(C)1987
                ;MATT GRAY
                ;This work is licensed
                ;under a Creative Commons 
                ;Attribution-NonCommercial 4.0 
                ;International License
				
STARTADD       =$1000
C0             =1
CS0            =2
D0             =3
DS0            =4
E0             =5
F0             =6
FS0            =7
G0             =8
GS0            =9
A0             =10
AS0            =11
B0             =12
C1             =13
CS1            =14
D1             =15
DS1            =16
E1             =17
F1             =18
FS1            =19
G1             =20
GS1            =21
A1             =22
AS1            =23
B1             =24
C2             =25
CS2            =26
D2             =27
DS2            =28
E2             =29
F2             =30
FS2            =31
G2             =32
GS2            =33
A2             =34
AS2            =35
B2             =36
C3             =37
CS3            =38
D3             =39
DS3            =40
E3             =41
F3             =42
FS3            =43
G3             =44
GS3            =45
A3             =46
AS3            =47
B3             =48
C4             =49
CS4            =50
D4             =51
DS4            =52
E4             =53
F4             =54
FS4            =55
G4             =56
GS4            =57
A4             =58
AS4            =59
B4             =60
C5             =61
CS5            =62
D5             =63
DS5            =64
E5             =65
F5             =66
FS5            =67
G5             =68
GS5            =69
A5             =70
AS5            =71
B5             =72
C6             =73
CS6            =74
D6             =75
DS6            =76
E6             =77
F6             =78
FS6            =79
G6             =80
GS6            =81
A6             =82
AS6            =83
B6             =84
C7             =85
CS7            =86
D7             =87
DS7            =88
E7             =89
F7             =90
FS7            =91
G7             =92
GS7            =93
A7             =94
AS7            =95
B7             =96
POINTS         =$FC
BARS           =$FE
V2LO           =V1LO+7
V2HI           =V1HI+7
V3LO           =V1LO+14
V3HI           =V1HI+14

                * = STARTADD
				
TN              !by   1
FADE            !by   0

INIT			TAX
				INX
				STX TN
				RTS

DRIVER          LDX #$00
                JSR MAIN
                LDX #$07
                JSR MAIN
                LDX #$0E
                JSR MAIN
                RTS
				
MAIN            LDA TN
                BNE PLAYMUSIC2
                STA $D418
                RTS 
PLAYMUSIC2      
                CMP #$AB
                BEQ MUSIC
                JMP SETPOINTS
SETCONT         LDA #0
                LDY #23
SIDLOOP         STA $D400,Y
                DEY 
                BPL SIDLOOP
                LDA #$0F
                STA $D418
                STA VOLUME
                LDY #0
                STY BARCOUNT
                STY BARCOUNT+7
                STY BARCOUNT+14
                STY V1DUR
                STY V1DUR+7
                STY V1DUR+14
                STY BEATCOUNT
                STY BEATCOUNT+7
                STY BEATCOUNT+14
                STY FADE
                INY 
                STY SPEED
                STY SPEED2
                JMP QUIT
MUSIC           
                LDA FADE
                BEQ OKMUSIC
                DEC VOLTIME
                BPL OKMUSIC
                LDA FADE
                STA VOLTIME
                DEC VOLUME
                BPL OKFADE
                LDA #0
                STA TN
                RTS 
OKFADE          LDA VOLUME
                STA $D418
OKMUSIC         LDY SOUND,X
                LDA VDATA+7,Y
                AND #4
                BEQ NOIMPLEX
                LDA IMPLEX,X
                BEQ NORMAL
                DEC IMPLEX,X
                LDA VDATA2+2,Y
                STA $D404,X
                BNE NOIMPLEX
NORMAL          LDA VDATA+1,Y
                STA $D404,X
NOIMPLEX        
                LDA VDATA+7,Y
                AND #$10
                BEQ NOHAT
                LDA HAT,X
                BEQ CANCELHAT
                DEC HAT,X
VAL             LDA #$50
                STA $D401,X
                LDA #$81
                STA $D404,X
                BNE NOHAT
CANCELHAT       LDA C1NHIGH,X
                STA $D401,X
                LDA VDATA+1,Y
                STA $D404,X
NOHAT           LDA SPEED
                BNE GOFX
                LDA #1
                STA HAT,X
DELAYS2         DEC V1DUR,X
                BMI MAINLOOP
GOFX            JMP CHECKFX
SETPOINTS       LDY TN
                LDA VOICE1L,Y
                STA V1LO
                LDA VOICE1H,Y
                STA V1HI
                LDA VOICE2L,Y
                STA V2LO
                LDA VOICE2H,Y
                STA V2HI
                LDA VOICE3L,Y
                STA V3LO
                LDA VOICE3H,Y
                STA V3HI
                LDA TDATA,Y
                STA TEMPOBYTE
                JMP SETCONT
QUIT            CPX #$0E
                BNE QUIT2
                DEC SPEED
                BPL QUIT2
                LDA TEMPOBYTE
                STA SPEED
QUIT2           
                LDA #$AB
                STA TN
QUIT3           RTS 
MAINLOOP        LDA V1LO,X
                STA POINTS
                LDA V1HI,X
                STA POINTS+1
AGAIN4          LDY BARCOUNT,X
                LDA (POINTS),Y
NOTEND2         TAY 
                LDA BARLO,Y
                STA BARS
                LDA BARHI,Y
                STA BARS+1
                LDA #$FF
                STA GATEBYTE
                LDA #0
                STA V1SLIDE,X
AGAIN           LDY BEATCOUNT,X
                LDA (BARS),Y
                BNE AGAIN3
                JMP PLAYNOTE
AGAIN3          CMP #$FD
                BCC SLIDE
                INY 
                INC BEATCOUNT,X
                LDA (BARS),Y
                JMP PLEXSETUP
REGET           INC BEATCOUNT,X
                BNE AGAIN
SLIDE           CMP #$FB
                BCC NEWVOICE
                CMP #$FB
                BNE SLIDEUP
                LDA #1
SLIDECONT       STA V1SLIDE,X
                INY 
                INC BEATCOUNT,X
                LDA (BARS),Y
                STA SLIDELO,X
                LDA #0
                STA V1PLEX,X
                STA V1VIB,X
                BEQ REGET
SLIDEUP         LDA #$02
                BNE SLIDECONT
NEWVOICE        CMP #$FA
                BCC VIBDELAY
                INY 
                INC BEATCOUNT,X
                LDA (BARS),Y
                ASL
                ASL
                ASL
                STA SOUND,X
                TAY 
                LDA VDATA+6,Y
                AND #$FE
                STA $D404,X
                LDA VDATA,Y
                PHA 
                AND #$0F
                STA V1PULSEHI,X
                STA PWH,X
                PLA 
                AND #$F0
                STA V1PULSELO,X
                STA PWL,X
                LDA VDATA2+6,Y
                PHA 
                AND #$0F
                STA PUCH,X
                PLA 
                AND #$F0
                ROL
                ROL
                ROL
                ROL
                STA PUCL,X
NOPR            LDA #0
                STA VDELAY,X
                STA V1VIB,X
                STA V1PLEX,X
                BEQ REGET
VIBDELAY        CMP #$F9
                BCC NOTEDUR
                INY 
                INC BEATCOUNT,X
                LDA (BARS),Y
                STA VDELAY,X
                JMP REGET
NOTEDUR         CMP #$70
                BCC PLAYNOTE
                SBC #$70
                STA NEWDUR,X
                JMP REGET
PLAYNOTE        BEQ NOBV
                CLC 
                ADC TP,X
NOBV            STA BARVALUE,X
                LDA NEWDUR,X
                STA V1DUR,X
                LDA #0
                STA DRUM2,X
                LDA #1
                STA IMPLEX,X
                LDA BARVALUE,X
                BEQ PLAYCONT2
                LDY SOUND,X
                LDA VDATA+7,Y
                AND #$02
                BEQ PLAYCONT
                LDA PWL,X
                STA V1PULSELO,X
                LDA PWH,X
                STA V1PULSEHI,X
PLAYCONT        LDA BARVALUE,X
                BNE NOREST
PLAYCONT2       LDA TEMP3,X
                STA BARVALUE,X
                LDA #$00
                STA TEMP3,X
                LDY SOUND,X
                DEC GATEBYTE
                BNE NOPITCH
NOREST          STA TEMP3,X
                TAY 
                LDA NTH,Y
                STA $D401,X
                STA V1HIFREQ,X
                STA C1NHIGH,X
                LDA NTL,Y
                STA $D400,X
                STA V1LOFREQ,X
                STA C1NLOW,X
                LDY SOUND,X
                LDA VDATA+6,Y
                STA $D404,X
                LDA VDATA+2,Y
                STA $D405,X
                LDA VDATA+3,Y
                STA $D406,X
                LDA V1PULSELO,X
                STA $D402,X
                LDA V1PULSEHI,X
                STA $D403,X
                LDA VDELAY,X
                STA VIBD,X
NOPITCH         
                LDA VDATA+1,Y
                AND GATEBYTE
                STA $D404,X
                INC BEATCOUNT,X
                LDY BEATCOUNT,X
                LDA (BARS),Y
                CMP #$FF
                BNE FXSETUP
                LDA #$00
                STA BEATCOUNT,X
                INC BARCOUNT,X
                LDY BARCOUNT,X
                LDA (POINTS),Y
                CMP #$FF
                BNE NOTEND
                LDA #$00
                STA BARCOUNT,X
                BEQ FXSETUP
NOTEND          CMP #$FE
                BNE FXSETUP
                LDA #$5F
                STA FADE
                INC BARCOUNT,X
FXSETUP         
                LDA TEMP3,X
                BEQ CHECKFX
                LDY SOUND,X
                LDA V1SLIDE,X
                BNE ALREADY
                LDA VDATA2+4,Y
                BEQ NOBEND
                STA V1SLIDE,X
                LDA VDATA2+3,Y
                STA SLIDELO,X
ALREADY         JMP SLIDECHECK
NOBEND          
               
VIBCHECK        LDA VDATA2,Y
                BEQ NOVIB
                JMP VIBSETUP
NOVIB           STA V1VIB,X
                JMP QUIT

CHECKFX         LDA VDATA+4,Y
                STA PTEMP
                BEQ PLEXCHECK
                LDA PMODDIR,X
                BNE PDOWN
                CLC 
                LDA V1PULSELO,X
                ADC PTEMP
                STA V1PULSELO,X
                STA $D402,X
                LDA V1PULSEHI,X
                ADC #$00
                STA V1PULSEHI,X
                STA $D403,X
                CLC 
                CMP PUCH,X
                BCC PLEXCHECK
                INC PMODDIR,X
                BNE PLEXCHECK
PDOWN           LDA V1PULSELO,X
                SEC 
                SBC PTEMP
                STA V1PULSELO,X
                STA $D402,X
                LDA V1PULSEHI,X
                SBC #$00
                STA V1PULSEHI,X
                STA $D403,X
                CLC 
                CMP PUCL,X
                BCS PLEXCHECK
                DEC PMODDIR,X

PLEXCHECK       LDA V1PLEX,X
                BEQ VIBUPDATE
                LDA PLEXTEMP,X
                ASL
                TAY 
                LDA PLEXLH,Y
                STA PLEXADD+1
                LDA PLEXLH+1,Y
                STA PLEXADD+2
                LDA PLEXC,X
                CMP PLEXCOUNT,X
                BNE PLEXCONT
                LDA #$00
                STA PLEXC,X
PLEXCONT        TAY 
                LDA BARVALUE,X
                CLC 
PLEXADD         ADC P0,Y
              
                TAY 
                LDA NTL,Y
                STA $D400,X
                LDA NTH,Y
                STA $D401,X
                INC PLEXC,X
                JMP QUIT
VIBUPDATE       
                LDA V1VIB,X
                BNE OKVIB1
                JMP SLIDECHECK
OKVIB1          LDA VIBD,X
                BEQ OKVIB
                DEC VIBD,X
                JMP SLIDECHECK
OKVIB           LDA VIBDIR,X
                BEQ VIBDOWN1
                CMP #$03
                BCC VIBUP
VIBDOWN         SEC 
                LDA C1NLOW,X
                SBC VIBSTEP,X
                STA C1NLOW,X
                STA $D400,X
                LDA C1NHIGH,X
                SBC #0
                STA C1NHIGH,X
                STA $D401,X
                DEC VIBTEMP,X
                BNE VIBEND1
                LDA VIBTIME,X
                STA VIBTEMP,X
                INC VIBDIR,X
                LDA VIBDIR,X
                CMP #$05
                BCC VIBEND1
                LDA #$01
                STA VIBDIR,X
VIBEND1         JMP QUIT

VIBDOWN1        SEC 
                LDA C1NLOW,X
                SBC VIBSTEP,X
                STA C1NLOW,X
                STA $D400,X
                LDA C1NHIGH,X
                SBC #0
                STA C1NHIGH,X
                STA $D401,X
                DEC VIBTEMP,X
                BNE VIBEND2
                LDA VIBTIME,X
                STA VIBTEMP,X
                INC VIBDIR,X
VIBEND2         JMP QUIT

VIBUP           CLC 
                LDA C1NLOW,X
                ADC VIBSTEP,X
                STA C1NLOW,X
                STA $D400,X
                LDA C1NHIGH,X
                ADC #0
                STA C1NHIGH,X
                STA $D401,X
                DEC VIBTEMP,X
                BNE NODRUMS
                LDA VIBTIME,X
                STA VIBTEMP,X
                INC VIBDIR,X
                BNE NODRUMS
                JMP QUIT
				
SLIDECHECK      LDA V1SLIDE,X
                BEQ NOMOREFX
                CMP #$01
                BEQ SLIDEDOWN2
                CMP #$02
                BEQ SLIDEUP2
                CMP #$03
                BEQ HIGHDOWN
                CLC 
                LDA C1NHIGH,X
                ADC SLIDELO,X
                STA C1NHIGH,X
                STA $D401,X
                JMP NOMOREFX
				
SLIDEDOWN2      CLC 
                LDA C1NLOW,X
                SBC SLIDELO,X
                STA C1NLOW,X
                STA $D400,X
                LDA C1NHIGH,X
                SBC #$00
                STA C1NHIGH,X
                STA $D401,X
                JMP NOMOREFX
				
HIGHDOWN        SEC 
                LDA C1NHIGH,X
                SBC SLIDELO,X
                STA C1NHIGH,X
                STA $D401,X
                JMP NOMOREFX
				
SLIDEUP2        CLC 
                LDA C1NLOW,X
                ADC SLIDELO,X
                STA C1NLOW,X
                STA $D400,X
                LDA C1NHIGH,X
                ADC #$00
                STA C1NHIGH,X
                STA $D401,X
NOMOREFX        LDY SOUND,X
                LDA VDATA+7,Y
                AND #1
                BEQ NODRUMS
                JMP DRUMMOD2
				
NODRUMS         JMP QUIT

V1VIB           !by   0
V1PLEX          !by   0
V1SLIDE         !by   0
PUCH            !by   0
PUCL            !by   0
BEATCOUNT       !by   0
PMODDIR         !by   0
                !by   0,0,0,0,0,0,0,0,0,0,0,0,0,0
SLIDELO         !by   0
FADEFLAG        !by   0
NEWDUR          !by   0
SOUND           !by   0
V1PULSELO       !by   0
PWL             !by   0
V1PULSEHI       !by   0
                !by   0,0,0,0,0,0,0,0,0,0,0,0,0,0
PWH             !by   0
PLEXTEMP        !by   0
V1LO            !by   0
V1HI            !by   0
BARCOUNT        !by   0
SEQNUMBER       !by   0
V1DUR           !by   0
                !by   0,0,0,0,0,0,0,0,0,0,0,0,0,0
TRACK           !by   0
PLAYFLAG        !by   0
TEMPOBYTE       !by   2
PTEMP           !by   0
SPEED           !by   0
GATEBYTE        !by   0
C1NLOW          !by   0
V1LOFREQ        !by   0
V1HIFREQ        !by   0
BARVALUE        !by   0
C1NHIGH         !by   0
PLEXCOUNT       !by   0
PLEXC           !by   0
                !by   0,0,0,0,0,0,0,0,0,0,0,0,0,0
VIBDIR          !by   0
VIBSTEP         !by   0
VIBTIME         !by   0
VIBTEMP         !by   0
VIBH            !by   0
VIBL            !by   0
TEMP3           !by   0
                !by   0,0,0,0,0,0,0,0,0,0,0,0,0,0
IMPLEX          !by   0
HAT             !by   0
VDELAY          !by   0
VIBD            !by   0
TP              !by   0
TWAVE           !by   0
DRUM2           !by   0
                !by   0,0,0,0,0,0,0,0,0,0,0,0,0,0
NTL             !by   12,28,45,62,81,102,123,145,169,195
                !by   221,250,24,56,90,125,163,204,246,35
                !by   83,134,187,244,48,112,180,251,71,152
                !by   237,71,167,12,119,233,97,225,104,247
                !by   143,48,218,143,78,24,239,210,195,195
                !by   209,239,31,96,181,30,156,49,223,165
                !by   135,134,162,223,62,193,107,60,57,99
                !by   190,75,15,12,69,191,125,131,214,121
                !by   115,199,124,151,30,24,139,126,250,6
                !by   172,243,230,143,248,46
NTH             !by   1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2
                !by   3,3,3,3,3,4,4,4,4,5,5,5,6,6,7,7,7
                !by   8,8,9,9,10,11,11,12,13,14,14,15,16,17,18
                !by   19,21,22,23,25,26,28,29,31,33,35,37,39,42
                !by   44,47,50,53,56,59,63,67,71,75,79,84,89,94
                !by   100,106,112,119,126,134,142,150,159,168
                !by   179,189,200,212,225,238,253
PLEXLH          !wo   P0,P1,P2,P3,P4,P5,P6
P0              !by   $07,$03,$00
P1              !by   $09,$05,$00
P2              !by   $08,$03,$00
P3              !by   $18,$0C,$00
P4              !by   $07,$05,$00
P5              !by   $07,$04,$00
P6              !by   $08,$05,$00

                ;DRUM TABLE
DTL             !by   DT&255,BT&255
DTH             !by   DT/256,BT/256
DT              !by   $81,$30,$11,$02,$41,$04
                !by   $80,$30,$80,$15,$80,$20,$80,$10
                !by   $80,$20,$80,$20,$80,$10,$80,$20,$FF
BT              !by   $81,$30,$41,$03,$40,$03,$80,$20
                !by   $80,$10,$80,$20,$80,$10,$80,$20,$FF

;-----------------------------------------------------------------------------
                !text   "(C)1988 MG"
;-----------------------------------------------------------------------------

VOLUME          !by   0
VOLTIME         !by   0
TEM2            !by   0
TEM3            !by   5
SPEED2          !by   0
PLEXSETUP       PHA 
                AND #$0F
                STA PLEXTEMP,X
                PLA 
                AND #$F0
                LSR
                LSR
                LSR
                LSR
                STA PLEXCOUNT,X
                LDA #$00
                STA PLEXC,X
                LDA #1
                STA V1PLEX,X
                LDA #0
                STA V1VIB,X
                JMP REGET
VIBSETUP        STA VIBSTEP,X
                LDA VDATA2+1,Y
                STA VIBTIME,X
                STA VIBTEMP,X
                LDA #0
                ;STA V1PLEX,X
                STA VIBDIR,X
                LDA #1
                STA V1VIB,X
                JMP QUIT
VDATA           !by   $87,$11,$00,$E6,$00,$00,$10,$01
                !by   $31,$41,$00,$ED,$15,$00,$40,$02
                !by   $00,$15,$0F,$00,$00,$00,$14,$00
                !by   $71,$41,$00,$8C,$30,$00,$40,$02
                !by   $F1,$41,$0F,$00,$20,$00,$40,$12
                !by   $00,$00,$00,$00,$00,$00,$00,$00
                !by   $00,$11,$00,$A0,$00,$00,$10,$00
                !by   $87,$81,$00,$E8,$00,$00,$80,$01
                !by   $20,$21,$00,$AD,$00,$00,$20,$00
                !by   $44,$41,$00,$7C,$C0,$00,$40,$02
                !by   $00,$80,$00,$A0,$00,$00,$10,$10
                !by   $C0,$41,$00,$9C,$25,$00,$40,$02
                !by   $C0,$41,$00,$9C,$25,$00,$40,$00
                !by   $00,$11,$0F,$00,$00,$00,$10,$00
                !by   $00,$11,$0F,$00,$00,$00,$10,$00
                !by   $F0,$41,$0B,$00,$30,$00,$40,$02
                !by   $31,$41,$00,$8C,$A0,$00,$40,$02
                !by   $00,$21,$00,$8C,$00,$00,$20,$00
				
VDATA2          !by   $00,$00,$81,$00,$00,$01,$8E,$00
                !by   $00,$00,$81,$00,$00,$00,$8E,$00
                !by   $00,$00,$81,$00,$00,$00,$8E,$00
                !by   $00,$00,$81,$00,$00,$00,$46,$00
                !by   $00,$00,$81,$00,$00,$00,$33,$00
                !by   $00,$00,$00,$00,$00,$00,$8E,$00
                !by   $00,$00,$81,$00,$00,$00,$8E,$00
                !by   $00,$00,$41,$00,$00,$00,$8E,$00
                !by   $00,$00,$81,$00,$00,$00,$8E,$00
                !by   $90,$02,$81,$00,$00,$00,$35,$00
                !by   $00,$00,$81,$00,$00,$00,$8E,$00
                !by   $90,$02,$81,$00,$00,$00,$27,$00
                !by   $90,$02,$81,$00,$00,$00,$27,$00
                !by   $00,$00,$81,$B3,$03,$00,$8E,$00
                !by   $FF,$08,$81,$00,$00,$00,$86,$00
                !by   $00,$00,$81,$00,$00,$00,$8C,$00
                !by   $80,$02,$81,$00,$00,$00,$8C,$00
                !by   $90,$02,$81,$00,$00,$00,$8C,$00
				
BARLO           !by   T0&255,T1&255,T2&255,T3&255,T4&255,T5&255
                !by   T6&255,T7&255,T8&255,T9&255,T10&255
                !by   T11&255,T12&255,T13&255,T14&255,T15&255
                !by   T16&255,T17&255,T18&255,T19&255,T20&255,T21&255
                !by   T22&255,T23&255,T24&255,T25&255,T26&255
                !by   T27&255,T28&255,T29&255,T30&255,T31&255
                !by   T32&255,T33&255
                !by   T34&255,T35&255,T36&255,T37&255
                !by   T38&255,T39&255
                !by   T40&255,T41&255,T42&255,T43&255
                !by   T44&255,T45&255
                !by   T46&255,T47&255,T48&255
                !by   T49&255,T50&255,T51&255
                !by   T52&255,T53&255,T54&255,T55&255,T56&255

BARHI           !by   T0/256,T1/256,T2/256,T3/256,T4/256,T5/256
                !by   T6/256,T7/256,T8/256,T9/256,T10/256
                !by   T11/256,T12/256,T13/256,T14/256,T15/256
                !by   T16/256,T17/256,T18/256,T19/256,T20/256,T21/256
                !by   T22/256,T23/256,T24/256,T25/256,T26/256
                !by   T27/256,T28/256,T29/256,T30/256,T31/256
                !by   T32/256,T33/256
                !by   T34/256,T35/256,T36/256,T37/256
                !by   T38/256,T39/256
                !by   T40/256,T41/256,T42/256,T43/256
                !by   T44/256,T45/256
                !by   T46/256,T47/256,T48/256
                !by   T49/256,T50/256,T51/256
                !by   T52/256,T53/256,T54/256,T55/256,T56/256
				
VOICE1L         !by   0,TUNE1&255,OVER1&255,FIN1&255
VOICE1H         !by   0,TUNE1/256,OVER1/256,FIN1/256
VOICE2L         !by   0,TUNE2&255,OVER2&255,FIN2&255
VOICE2H         !by   0,TUNE2/256,OVER2/256,FIN2/256
VOICE3L         !by   0,TUNE3&255,OVER3&255,FIN3&255
VOICE3H         !by   0,TUNE3/256,OVER3/256,FIN3/256

DRUMMOD2        LDA POINTS
                PHA 
                LDA POINTS+1
                PHA 
                LDA VDATA2+5,Y
                TAY 
                LDA DTL,Y
                STA POINTS
                LDA DTH,Y
                STA POINTS+1
                LDY DRUM2,X
                LDA (POINTS),Y
                BPL DSTAGE2
                CMP #$FF
                BEQ DEND
DSTAGE3         STA $D404,X
                INY 
                INC DRUM2,X
                LDA (POINTS),Y
                STA $D401,X
                INY 
                INC DRUM2,X
                BNE DEND
DSTAGE2         STA TWAVE,X
                INY 
                INC DRUM2,X
                SEC 
                LDA C1NHIGH,X
                SBC (POINTS),Y
                STA C1NHIGH,X
                INY 
                INC DRUM2,X
DEND2           LDA TWAVE,X
                STA $D404,X
                LDA C1NHIGH,X
                STA $D401,X
DEND            PLA 
                STA POINTS+1
                PLA 
                STA POINTS
                JMP QUIT
				
TDATA           !by   0,5,3,4

TUNE1           !by   5,5,7,7,7,7,14,14,14,14,14,14,14,17
                !by   15,15,15,15,15,15,15,20,16,18,16,18,16,18,16,19
                !by   16,18,16,18,16,18,16,18
                !by   16,18,16,19,16,18,16,18
                !by   16,18,16,18,16,18,16,18,16,18,16,22
                !by   24,24,24,24,24,24,24,24,27,27,27,27,27,27,27,27
                !by   16,18,16,18,16,18,16,22,27,27,27,27,27,27,27,27
                !by   27,27,27,27,27,27,27,22
                !by   27,27,27,27,27,27,27,22
                !by   27,27,27,27,27,27,27,22
                !by   27,27,27,27,27,27,27,22
                !by   16,18,16,18,16,18,16,18
                !by   15,34,15,34,15,34,15,22
                !by   15,34,15,34,15,34,15,22
                !by   15,34,15,34,15,34,15,22,$FF

TUNE2           !by   6,10,10
                !by   11,11,11,11,12,12,12,12,11,11,11,11,13,13,13,13
                !by   11,11,11,11,12,12,12,12,11,11,11,11,13,13,13,13
                !by   11,11,11,11,12,12,12,12,11,11,11,11,13,13,13,13
                !by   11,11,11,11,12,12,12,12,11,11,11,11,13,13,13,13
                !by   11,11,11,11,12,12,12,12,11,11,11,11,13,13,13,13
                !by   11,11,11,11,12,12,12,12,11,11,11,11,13,13,13,13
                !by   5,10,10,1,3,1,4,1,3,1,4,1,3,1,4,1,3,1,4
                !by   1,3,1,4,1,3,1,4,1,3,1,4,1,3,1,4,1,3,1,4,1,3,21
                !by   11,11,11,11,12,12,12,12,11,11,11,11,13,13,13,13
                !by   11,11,11,11,12,12,12,12,11,11,11,11,13,13,13,13
                !by   11,11,11,11,12,12,12,12,11,11,11,11,13,13,13,13
                !by   26,26,30,30,30,30,33,33,35,36,38,39,37,37,37,37
                !by   40,40,41,41,42,42,43,43
                !by   40,40,41,41,42,42,43,43,44
                !by   40,40,41,41,42,42,43,43
                !by   40,40,41,41,42,42,43,43,44,44,$FF

TUNE3           !by   8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,9,8,9,8,9,8,9
                !by   8,9,8,9,8,9,8,9,8,8,8,8,8,8,8,8,1,3,1,4,1,3,1,4
                !by   1,3,1,4,1,3,1,4,1,3,1,4,1,3,1,4
                !by   1,3,1,4,1,3,1,4,1,3,1,4,1,3,1,4
                !by   1,3,1,4,1,3,1,4,1,3,1,4,1,3,1,4,25,23
                !by   1,3,1,4,1,3,1,4,1,3,1,4,1,3,1,4
                !by   28,29,28,29,28,29,28,29,28,29,28,29,28,29,28,29
                !by   28,31,32,29,28,31,32,29
                !by   28,31,32,29,28,31,32,29
                !by   28,31,32,29,28,31,32,29
                !by   28,31,32,29,28,31,32,29
                !by   28,31,32,29,28,31,32,29
                !by   28,31,32,29,28,31,32,29
                !by   28,31,32,29,28,31,32,29
                !by   28,31,32,29,28,31,32,29
                !by   45,45,45,45,45,45,45,45,$FF

T0              !by   $AF,$FA,$05,$00,$FF
T1              !by   $FA,$04,$71,D2,D2,$70,F2,D2,F2,$71,G2,G2,$70,G2
                !by   $FF
T3              !by   $71,A2,F2,$FF
T6              !by   $FA,$02,$EF,$FC,$0A,AS3,$FB,$0A,AS4,$FF
T2              !by   $7F,$FA,$05,$00,$FF
T4              !by   $71,A1,C2,$FF
T5              !by   $FA,$01,$AF,D1,0,$FF
T7              !by   $FA,$01,$7F,D1,G1,D1,G1,$FF
T8              !by   $FA,$04,$71,D2,D2,D2,$70,D2,D2,$71,D2,D2,D2,$70
                !by   D2,D2,$FF
T9              !by   $FA,$04,$71,G2,G2,G2,$70,G2,G2,$71,G2,G2,G2,$70
                !by   G2,G2,$FF
T10             !by   $FA,$03,$FD,$30,$77,D5,0
                !by   $FD,$31,D5,0,$FD,$30,D5,0,$FD,$32,B4,0,$FF
T11             !by   $FA,$06,$FD,$33,$70,A4,F4,D4,F4,$FF
T12             !by   $FA,$06,$FD,$33,$70,B4,G4,D4,G4,$FF
T13             !by   $FA,$06,$FD,$33,$70,G4,D4,B3,D4,$FF
T14             !by   $FA,$00,$73,C4,C4,C4,C4,$FF
T15             !by   $FA,$00,$73,C4,$FA,$07,D4,$FA,$00,C4,$FA,$07,D4
                !by   $FF
T16             !by   $FA,$00,$71,C4,$FA,$06,$FD,$34,D6
                !by   $FA,$07,D4,$FA,$06,$FD,$34,D6
                !by   $FA,$00,C4,$FA,$06,$FD,$34,D6
                !by   $FA,$07,D4,$FA,$06,$FD,$34,D6,$FF
T17             !by   $FA,$00,$73,C4,C4,$71,C4,$FA,$07,$71,D4,D4,$70
                !by   D4,D4,$FF
T18             !by   $FA,$00,$71,C4,$FA,$06,$FD,$30,D6
                !by   $FA,$07,D4,$FA,$06,$FD,$30,D6
                !by   $FA,$00,C4,$FA,$06,$FD,$30,D6
                !by   $FA,$07,D4,$FA,$06,$FD,$30,D6,$FF
T19             !by   $FA,$00,$71,C4,$FA,$06,$FD,$30,D6
                !by   $FA,$07,D4,$FA,$06,$FD,$30,D6
                !by   $FA,$00,C4,$FA,$06,$FD,$30,D6
                !by   $FA,$07,$70,D4,D4,D4,D4,$FF
T20             !by   $FA,$00,$73,C4,$FA,$07,D4,$FA,$00,$71,C4
                !by   $FA,$07,$70,D4,D4,$71,D4,$70,D4,D4,$FF
T21             !by   $FA,$04,$71,D2,D2,$70,D2,D2,D2,$71,F2,F2
                !by   $70,F2,$71,G2,G2,$FF
T22             !by   $FA,$07,$71,D4,D4,$70,D4,D4,D4,$71,D4,D4
                !by   $70,D4,$71,D4,D4,$FF
T23             !by   $FA,$04,$71,D3,D3,$70,D3,D3,D3,$71,F3,F3
                !by   $70,F3,$71,G3,G3,$FF
T24             !by   $FA,$00,$71,C4,C4,$70,C4,C4,C4,$71,C4,C4,$70,C4
                !by   $71,C4,C4,$FF
T25             !by   $FA,$02,$EF,$FC,$0A,AS4,$DF,$FB,$0A,AS5,$FF
T26             !by   $FA,$09,$F9,$08,$72,A4,D5,F5,A5,$71,F5,A5
                !by   $72,B5,G5,D5,B4,$71,A4,G4,$FF
T27             !by   $FA,$00,$71,C4,C4
                !by   $FA,$07,$71,D4
                !by   $FA,$00,$70,C4,$71,C4,C4,$70,C4
                !by   $FA,$07,$71,D4
                !by   $FA,$00,C4,$FF
T28             !by   $FA,$04,$70,D2,D2,D2,D2,D3,D3,D2,D2,D3,D3
                !by   D2,D2,C3,B2,C3,D3,$FF
T29             !by   $FA,$04,$70,G2,G2,G2,G2,G3,G3,G2,G2,G3,G3
                !by   G2,G2,F3,E3,F3,G3,$FF
T30             !by   $FA,$01,$7F,D2,G2,$FF
T31             !by   $FA,$04,$70,F2,F2,F2,F2,F3,F3,F2,F2,F3,F3
                !by   F2,F2,DS3,D3,DS3,F3,$FF
T32             !by   $FA,$04,$70,C2,C2,C2,C2,C3,C3,C2,C2,C3,C3
                !by   C2,C2,AS2,A2,AS2,C3,$FF
T33             !by   $FA,$03,$77,$FD,$30,D5,0,$FD,$31,C5,0,$FD,$35,C5
                !by   0,$FD,$32,B4,0,$FF
T34             !by   $FA,$00,$73,C4,$FA,$07,D4,$FA,$00,C4,$72,$FA,$07
                !by   D4,$70,D4,$FF
T35             !by   $FA,$0B,$F9,$0A,$75,D4,E4,$73,F4
                !by   $75,A4,G4,$73,F4
                !by   $75,E4,F4,$73,G4
                !by   $77,D4,0,$FF
T36             !by   $FA,$0B,$F9,$0A,$75,D4,E4,$73,F4
                !by   $75,A4,G4,$73,F4
                !by   $75,D5,C5,$73,B4
                !by   $77,B4,0,$FF
T37             !by   $FA,$0C,$F9,$0A,$77,C4,$FC,$0B,C4
                !by   D4,$FB,$0B,D4,$FF
T38             !by   $FA,$0B,$F9,$0A,$75,D5,C5,$73,D5
                !by   $75,F5,D5,$73,F5
                !by   $75,G5,F5,$73,E5
                !by   $77,D5,0,$FF
T39             !by   $FA,$0B,$F9,$0A,$75,D5,C5,$73,D5
                !by   $75,F5,D5,$73,F5
                !by   $75,C6,B5,$73,G5
                !by   $7B,G5,$73,$FB,$90,G5,$FF
T40             !by   $FA,$0B,$FD,$33,$71,F4,D4,A3,D4,$FF
T41             !by   $FA,$0B,$FD,$33,$71,F4,C4,A3,C4,$FF
T42             !by   $FA,$0B,$FD,$33,$71,E4,C4,G3,C4,$FF
T43             !by   $FA,$0B,$FD,$33,$71,D4,B3,G3,B3,$FF
T44             !by   $FA,$0D,$AF,D5,$FA,$0E,D5,$FF
T45             !by   $FA,$0A,$70,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,$FF
OVER1           !by   46,$FE,0,0
OVER2           !by   47,0,0
OVER3           !by   48,0,0
T46             !by   $FA,$01,$77,C1,G1,DS1,AS1,$70,F1,$EE,0,$FF
T47             !by   $FA,$0B,$77,$FD,$36,G5,$FD,$34,G5,$FD,$32,G5
                !by   $FD,$31,F5,$FD,$35,$77,F5,$97,0,$FF
T48             
                !by   $FA,$09,$77,$FD,$36,G4,$FD,$34,G4,$FD,$32,G4
                !by   $FD,$31,F4,$FD,$35,$77,F4,$E7,0,$FF
FIN1            !by   49,49,49,49,49,49,56,56,49,49,$FE,49,49,49,49,49
                !by   49,49
FIN2            !by   50,51,52,53,$FF
FIN3            !by   56,54,54,55,$FE,54,56
T49             !by   $FA,$00,$73,A3,$FA,$07,$71,D4
                !by   $FA,$00,$73,A3,$71,A3,$FA,$07,$73,D4
                !by   $FA,$00,$72,A3,$70,A3,$71,$FA,$07,D4
                !by   $FA,$00,$73,A3,$71,A3,$FA,$07,D4,D4,$FF
T50             
                !by   $FA,$0F,$71,C2,C3,C3,C2,C3,C2,G2,AS2,$FF
T51             !by   AS1,AS2,AS2,AS1,AS2,AS1,F1,G1,$FF
T52             !by   DS2,DS3,DS3,DS2,DS3,DS2,AS1,C2,$FF
T53             !by   F2,F3,F3,F2,F3,F2,C2,D2,$FF
T54             !by   $FA,$10,$F9,$0A,$75,C5,D5,$73,DS5
                !by   $75,F5,DS5,$73,D5
                !by   $75,DS5,D5,$73,AS4
                !by   $77,AS4,A4
                !by   $75,C5,D5,$73,DS5
                !by   $75,F5,DS5,$73,D5
                !by   $75,DS5,D5,$73,AS4
                !by   $77,C5,0,$FF
T55             !by   $FA,$11,$F9,$10,$75,C5,D5,$73,DS5
                !by   $75,F5,DS5,$73,D5
                !by   $75,DS5,D5,$73,AS4
                !by   $77,AS4,A4
                !by   $75,C5,D5,$73,DS5
                !by   $75,F5,DS5,$73,D5
                !by   $75,DS5,D5,$73,AS4
                !by   $77,C5,$77,0,$FF
T56             !by   $FA,$06,$7F,$FD,$36,G4,$FD,$31,F4
                !by   $FD,$32,G4,$FD,$35,F4,$FF
E               !by   0

}
