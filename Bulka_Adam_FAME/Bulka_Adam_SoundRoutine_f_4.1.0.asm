
	; Converted from TurboAssembler to ACME by dmx87


	!to "Bulka_Adam_SoundRoutine_f_4.1.0.sid",plain

	* = $0000
	
	!text "PSID"
	!be16 2							; version 2
	!be16 $7c						; data offset
	!be16 $1000							; Load (0 = auto)
	!be16 musicset0					; Init
	!be16 music						; Play
	!be16 1							; num songs
	!be16 1							; first song
	!word 0
	!word 0
-	!text "Sound Routine f 4.1.0"
	!fill 32 - (* - -)
-	!text "Adam Bulka"
	!fill 32 - (* - -)
-	!text "1988 FAME"
	!fill 32 - (* - -)
	!be16 $0014							; v2 flags
	!be16 0							; Start page, page length (reloc)
	!be16 0							; Reserved

!pseudopc $1000 {


;0@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@.
;]       SOUND ROUTINE f 4.1.0         ]
;]                                     ]
;]       WRITTEN BY ADAM BULKA         ]
;]                                     ]
;]                                     ]
;]         WORKING VERSION I           ]
;]                                     ]
;] DATE:  27.07.1988   2.29 pm         ]
;-@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@=



         *= $1000

ze0      = $50
ze1      = $51
ze2      = $52
ze3      = $53

ze4      = $54
ze5      = $55


sid      = $d400
volume   = $0f


bd       = $21
sd       = $29
cd       = $1a


;*************************
;    NOTEN DEFINITION



c0       = 0
ch0      = 1
d0       = 2
dh0      = 3
e0       = 4
f0       = 5
fh0      = 6
g0       = 7
gh0      = 8
a0       = 9
ah0      = 10
h0       = 11

c1       = 12
ch1      = 13
d1       = 14
dh1      = 15
e1       = 16
f1       = 17
fh1      = 18
g1       = 19
gh1      = 20
a1       = 21
ah1      = 22
h1       = 23

c2       = 24
ch2      = 25
d2       = 26
dh2      = 27
e2       = 28
f2       = 29
fh2      = 30
g2       = 31
gh2      = 32
a2       = 33
ah2      = 34
h2       = 35

c3       = 36
ch3      = 37
d3       = 38
dh3      = 39
e3       = 40
f3       = 41
fh3      = 42
g3       = 43
gh3      = 44
a3       = 45
ah3      = 46
h3       = 47

c4       = 48
ch4      = 49
d4       = 50
dh4      = 51
e4       = 52
f4       = 53
fh4      = 54
g4       = 55
gh4      = 56
a4       = 57
ah4      = 58
h4       = 59

c5       = 60
ch5      = 61
d5       = 62
dh5      = 63
e5       = 64
f5       = 65
fh5      = 66
g5       = 67
gh5      = 68
a5       = 69
ah5      = 70
h5       = 71

c6       = 72
ch6      = 73
d6       = 74
dh6      = 75
e6       = 76
f6       = 77
fh6      = 78
g6       = 79
gh6      = 80
a6       = 81
ah6      = 82
h6       = 83

c7       = 84
ch7      = 85
d7       = 86
dh7      = 87
e7       = 88
f7       = 89
fh7      = 90
g7       = 91
gh7      = 92
a7       = 93
ah7      = 94
h7       = 95

musicset0

         tay

         ldx #29*3+3-1
         lda #0
lopp2    sta d400,x
         dex
         bpl lopp2

         ldx #$18
         lda #0
lopp3    sta sid,x
         dex
         bpl lopp3



         tya
         asl
         sta ze0
         asl
         clc
         adc ze0
         tax
         lda speedtab,y
         sta speed2+1
         ldy #0
lopp1    lda trackslohi,x
         sta tracklo,y
         inx
         iny
         cpy #6
         bcc lopp1
         lda #$80
         sta beginflag
         rts


aa35
         cmp #$ff
         bne aa42
         iny
         lda (ze0),y
         sta tracklo,x
         iny
         lda (ze0),y
         sta trackhi,x
         jmp newtrack

aa42
         cmp #$a0
         bcs aa36
         and #%00011111
         sta noteadc,x
aa39     iny
         jmp aa37
aa36
         cmp #$c0
         bcs aa38
         and #%00011111
         sta voiceadc,x
         bpl aa39
aa38
         and #%00111111
         sta repeat,x
         iny
         jmp aa37


newtrack
         lda tracklo,x
         sta ze0
         lda trackhi,x
         sta ze1
         lda #0
         tay
         sta steppointer,x

         lda repeat,x
         beq nixrepeat

         dec repeat,x
         jmp aa34

nixrepeat
aa37     lda (ze0),y
         bmi aa35
         asl
         sta aktustep,x
         iny
         tya
         clc
         adc tracklo,x
         sta tracklo,x
         bcc aa34
         inc trackhi,x
         jmp aa34


aa23
         jmp wibrato


music
         ldx #2
music1
         bit beginflag
         bmi newtrack

         lda speed1
         bne aa23

         dec releasecount,x
         bpl aa31

         ldy voicenow,x
         lda wavetab-8,y
         bmi aa31

         lda #$fe
         sta waveand,x

aa31
         dec speedcount,x
         bpl aa23
aa34
         ldy aktustep,x
         lda steps,y
         sta ze0
         lda steps+1,y
         sta ze1
         ldy steppointer,x
aa26     lda (ze0),y
         bmi aa24

         cmp #$60
         bcs exarp1
         clc
         adc noteadc,x
         sta notenow,x
         jmp note1

exarp1
         and #%00011111
         sta exarpeggio,x
         iny
         jmp aa26

aa24
         cmp #$ff
         bne weiter1
         jmp newtrack
weiter1
         cmp #$a0
         bcs aa25
         and #%00011111
         sta speedwert,x
         iny
         jmp aa26


aa25

         cmp #$c0
         bcs aa27
         adc voiceadc,x
         and #%00011111
         asl
         asl
         asl
         sta voicenow,x
         iny
         sty ze2
         tay
         lda fpbyte-8,y
         and #%11110000
         beq aa45

         stx filterflag+1
         lsr
         lsr
         lsr
         lsr
         sta filternow+1
         tay
         lda filterfrq-1,y
         sta ffrq+1
         lda filtermode-1,y
         and #%11110000
         ora #volume
         sta filter+1
         lda filtermode-1,y
         asl
         asl
         asl
         asl
         ora filterbyte,x
         jmp aa46


aa45
         lda d417+1
         and filterand,x
aa46     sta d417+1
         ldy ze2
         jmp aa26


aa27     cmp #$e0
         bcs aa28

         and #%00011111
         sta speedcount,x
         jmp aa29


aa28
         and #%00011111
         sta speedwert,x

         iny
         lda (ze0),y
         ora #%10000000
         sta portacount,x
         iny
         lda (ze0),y
         clc
         adc noteadc,x
         sta byte1,x
         sta notenow,x
         iny
         lda (ze0),y
         clc
         adc noteadc,x
         sta byte2,x


note1
         sty ze2
         ldy voicenow,x

         lda puls-8,y
         sta pulslohi,x

         lda release-8,y
         bpl aa40

         ldy voicebyte,x
         lda #0
         sta sid+5,y
         sta sid+6,y
aa40
         lda speedwert,x
         sta speedcount,x
         lda #0
         sta wibpoint,x
         sta wibcount,x
         sta wavepoint,x
         sta pulscount,x
         sta pulspoint,x
         sta adcwert,x

filterflag
         cpx #$ff
         bne nofilter

         sta filtercount
         sta filterpoint+1
ffrq
         lda #$ff
         sta d416+1

nofilter

         lda #$ff
         sta waveand,x

         ldy notenow,x
         lda low,y
         sta d400,x
         lda high,y
         sta d401,x


         ldy voicenow,x

         lda wavetab-8,y
         bmi aa32

         asl
         asl
         lda release-8,y
         and #%01111111
         bcs aa41

         cmp speedwert,x
         bcc aa33
         lda #1


aa41
         sta ze3
         lda speedwert,x
         sec
         sbc ze3
aa33
         sta releasecount,x

aa32
         ldy ze2


aa29     iny
         tya
         sta steppointer,x

counter1
         dex
         bmi aa43
         jmp music1
aa43
         dec speed1
         bpl aa44
speed2
         lda #2
         sta speed1
aa44
         lda #0
         sta beginflag

filter

         lda #volume+$10
         sta sid+24
d417     lda #0
         sta sid+23
filternow
         ldx #0
         beq aa48

         dec filtercount
         bpl d416

filterpoint
         lda #0
         cmp filterup-1,x
         bcc aa49

         lda filterag-1,x
aa49
         tay

         lda ftablo-1,x
         sta ze0
         lda ftabhi-1,x
         sta ze1

         lda (ze0),y
         sta filtercount
         iny
         lda (ze0),y
         sta fadcwert+1
         iny
         tya
         sta filterpoint+1

d416
         lda #$ff
         clc
fadcwert adc #0
         sta sid+22
         sta d416+1
aa48
         rts

stat1
         !by 0

setportapara
         stx aa50+1
         and #%01111111
         sta portacount,x
         sta ze2
         lda #0
         sta ze3


         lda byte2,x
         sta notenow,x
         cmp byte1,x
         bcs aa51

         sta ze0
         ldy byte1,x
         ldx ze0

         lda #$ff
         sta stat1
         jmp aa52

aa51

         tay
         lda byte1,x
         tax
         lda #0
         sta stat1


aa52
         lda low,y
         sec
         sbc low,x
         sta ze0
         lda high,y
         sbc high,x
         sta ze1


div16

         lda #0     ; rest = 0
         sta ze4
         sta ze5
         ldx #16    ; 16 bits

divl1    asl ze0    ; 1.op shiften
         rol ze1

         rol ze4    ; rest shiften
         rol ze5

         sec        ; rest > 2.op
         lda ze4
         sbc ze2
         tay
         lda ze5
         sbc ze3
         bcc divj1  ; nein !
                    ; sonst:
         sty ze4    ; rest=rest-2.op
         sta ze5
         inc ze0    ; faktor + 1

divj1    dex
         bne divl1


aa50
         ldx #0

         lda ze0
         eor stat1
         sta byte1,x
         lda ze1
         eor stat1
         sta byte2,x
         jmp pulswib



porta
         bmi setportapara

         dec portacount,x

         lda d400,x
         clc
         adc byte1,x
         sta d400,x
         lda d401,x
         adc byte2,x
         sta d401,x
         jmp pulswib



aa6
                    ;arpeggio
         sty ze3
         clc
         adc notenow,x
         tay
         lda low,y
         sta d400,x
         lda high,y
         sta d401,x
         ldy ze3
aaa5     jmp aa5

tabarp
         bvs aa6
                  ;werte direkt in sid
aa7      sta d401,x
         lda #0
         sta d400,x
         beq aaa5

sense
         ldy voicebyte,x
         lda #8
         sta sid+4,y
         lda #0
         sta sid+5,y
         sta sid+6,y
         jmp counter1



wibrato
         ldy voicenow,x
         beq sense

         lda portacount,x
         bne porta

         lda exarpeggio,x
         beq nixarpeggio

         tay
         lda arplo-1,y
         sta tablo
         lda arphi-1,y
         sta tabhi
         lda arpup-1,y
         sta wibup
         lda arpag-1,y
         sta wibag

         lda #$c0
         sta ze2
         ldy #0
         beq arpeggio

nixarpeggio
         lda wibbyte-8,y
         beq aaa0
         sta ze2
         bpl aa1
nupara   and #%00011111
         tay
arpeggio lda tablo,y
         sta ze0
         lda tabhi,y
         sta ze1
         lda wibpoint,x
         cmp wibup,y
         bcc aa2
         lda wibag,y
aa2
         tay
aa22     lda (ze0),y
         bit ze2
         bmi tabarp
         sta byte1,x
         sta ze3
         and #%00000111
         sta wibcount,x
         iny
         lda (ze0),y
         asl ze3
         bit ze3
         bcc aa3
         bvs aa21
         bpl aaa6
         jmp aa7

aa21
         sta adcwert,x
         iny
         jmp aa22

aaa6
         jmp aa6

aa3
         sta byte2,x
         lsr
         lsr
         lsr
         lsr
         and #%00000111
         sta adccount,x
aa5
         iny
         tya
         sta wibpoint,x
         bit ze2
         bpl backpara
aaa0     jmp pulswib


aa1
         dec wibcount,x
         bmi nupara
backpara
         lda byte1,x
         bmi aaa0
         lsr
         lsr
         lsr
         and #%00000111
         sta ze2
         lda byte2,x
         lsr
         lsr
         lsr
         and #%00001111
         clc
         adc notenow,x
         tay
         lda low,y
         sbc low-1,y
         sta ze3
         lda high,y
         sbc high-1,y
         clc
         adc adcwert,x
aa9      dec ze2
         bmi aa8
         lsr
         ror ze3
         jmp aa9
aa8
         sta ze2
         lda byte1,x
         and #%01000000
         beq substract

         lda d400,x
         clc
         adc ze3
         sta d400,x
         lda d401,x
         adc ze2
         sta d401,x
         jmp aa10
substract
         lda d400,x
         sec
         sbc ze3
         sta d400,x
         lda d401,x
         sbc ze2
         sta d401,x
aa10
         dec adccount,x
         bpl aa11
         lda byte2,x
         lsr
         lsr
         lsr
         lsr
         and #%00000111
         sta adccount,x
         bpl aa14
aa11
         bne pulswib
aa14     lda byte2,x
         bpl aa13
         inc adcwert,x
         jmp pulswib
aa13
         dec adcwert,x


pulswib
         ldy voicenow,x
         lda fpbyte-8,y
         and #%00001111
         beq wavewib1
         tay
         dec pulscount,x
         bpl aa17

         lda ptablo-1,y
         sta ze0
         lda ptabhi-1,y
         sta ze1
         lda pulspoint,x
         cmp pulsup-1,y
         bcc aa16
         lda pulsag-1,y
aa16
         tay
         lda (ze0),y
         sta pulscount,x
         iny
         lda (ze0),y
         sta padcwert,x
         iny
         tya
         sta pulspoint,x


aa17
         lda pulslohi,x
         clc
         adc padcwert,x
         adc #0
         sta pulslohi,x



wavewib
         ldy voicenow,x
wavewib1 lda wave-8,y
         sta ze2
         lda wavetab-8,y
         and #%00011111
         beq aa19
         tay
         lda wtablo-1,y
         sta ze0
         lda wtabhi-1,y
         sta ze1
         lda wavepoint,x
         cmp ze2
         bcc aa18
         lda waveag-1,y
aa18
         tay
         inc wavepoint,x
         lda (ze0),y
         jmp aa20
aa19
         lda ze2
aa20
         and waveand,x
         sta d404,x



setsid
         ldy voicebyte,x
         lda d400,x
         sta sid,y
         lda d401,x
         sta sid+1,y
         lda pulslohi,x
         sta sid+2,y
         sta sid+3,y
         lda d404,x
         sta sid+4,y
         stx ze2
         lda voicenow,x
         tax
         lda ad-8,x
         sta sid+5,y
         lda sr-8,x
         sta sid+6,y
         ldx ze2
         jmp counter1




;***************************************
;              VOICES
ad

sr       = ad+1

puls     = ad+2

wave     = ad+3

wibbyte  = ad+4      ;Vibrato Pointer
                     ;Betriebsart
                     ;$80=tab/$c0=arp
fpbyte   = ad+5      ;Lownibble=Puls Po.
                     ;Highnibble=Fil Po.
wavetab  = ad+6

release  = ad+7


;1 bd
         !by $00,$d8,$08,$0c
         !by $c1,$00,$81,$80

;2 sd  ;GEFILTERT
         !by $00,$f8,$08,$0f
         !by $c2,$00,$82,$80

;3 bd II
         !by $00,$c7,$08,$03
         !by $c3,$00,$81,$80

;4 main I
         !by $01,$98,$e7,$41
         !by $04,$01,$40,$03

;5 bass I
         !by $01,$ef,$05,$02
         !by $05,$12,$43,$81

;6 bass II
         !by $00,$7f,$04,$02
         !by $05,$10,$43,$81

;7 floete I
         !by $00,$f8,$00,$11
         !by $04,$00,$40,$03

;8 main II
         !by $01,$98,$a0,$04
         !by $04,$01,$44,$03

;9 arpeggio I
         !by $01,$3a,$08,$41
         !by $c6,$01,$40,$06

;0a arpeggio-short I
         !by $01,$69,$08,$41
         !by $c7,$00,$00,$80

;0b short I
         !by $01,$99,$07,$41
         !by $00,$02,$00,$80

;0c main III
         !by $01,$99,$e7,$41
         !by $08,$01,$40,$04

;0d sd  ;Nicht Gefiltert
         !by $00,$f8,$08,$0f
         !by $c2,$00,$82,$80

;0e arpeggio I
         !by $01,$7a,$08,$41
         !by $c6,$01,$40,$06

;$0f short !!
         !by $01,$7f,$05,$02
         !by $05,$02,$43,$81


;$10 main II lange halten
         !by $01,$9d,$a0,$04
         !by $04,$01,$44,$01





;**********************
;     VIBpointer

tablo
         !by 0
         !by <tab1
         !by <tab2
         !by <tab3
         !by <tab4
         !by <tab5
         !by <tab6
         !by <tab7
         !by <tab8
tabhi
         !by 0
         !by >tab1
         !by >tab2
         !by >tab3
         !by >tab4
         !by >tab5
         !by >tab6
         !by >tab7
         !by >tab8

wibup
         !by 0
         !by $0b
         !by $10
         !by $0a
         !by $0a
         !by $0a
         !by $03
         !by $03
         !by $08

wibag
         !by 0
         !by $0a
         !by $0f
         !by $09
         !by $06
         !by $06
         !by $00
         !by $00
         !by $04


;**********************
;    PULSpointer

ptablo
         !by <ptab1
         !by <ptab2

ptabhi
         !by >ptab1
         !by >ptab2

pulsup
         !by $08
         !by $08
pulsag

         !by $04
         !by $04

;**********************
;     WAVEpointer

wtablo
         !by <wtab1
         !by <wtab2
         !by <wtab3
         !by <wtab4
         !by <wtab5

wtabhi
         !by >wtab1
         !by >wtab2
         !by >wtab3
         !by >wtab4
         !by >wtab5

waveag
         !by $0b
         !by $0e
         !by $01
         !by $03
         !by $03



;**********************
;    FILTERpointer

ftablo
         !by <ftab1
         !by <ftab2

ftabhi
         !by >ftab1
         !by >ftab2

filterup
         !by $0c
         !by $02

filterag
         !by $0a
         !by $00

filtermode
         !by $1f
         !by $1f

filterfrq
         !by $e0
         !by $8e ;$a8

;***********************
;EXTENDEDARPEGGIOpointer

arplo
         !by <arp1
         !by <arp2
         !by <arp3
         !by <arp4
arphi
         !by >arp1
         !by >arp2
         !by >arp3
         !by >arp4
arpup
         !by $03
         !by $03
         !by $03
         !by $03
arpag
         !by $00
         !by $00
         !by $00
         !by $00

;**********************
;ARPTABS

arp1
         !by $00,$03,$07
arp2
         !by $00,$03,$08
arp3
         !by $00,$04,$07
arp4
         !by $00,$04,$08



;***************************************
;        LOW & HIGH-FREQUENCES

low
         !by $16,$27,$38,$4b,$5f,$73
         !by $8a,$a1,$ba,$d4,$f0,$0e
         !by $2d,$4e,$71,$96,$bd,$e7
         !by $13,$42,$74,$a9,$e0,$1b
         !by $5a,$9b,$e2,$2c,$7b,$ce
         !by $27,$85,$e8,$51,$c1,$37
         !by $b4,$37,$c4,$57,$f5,$9c
         !by $4e,$09,$d0,$a3,$82,$6e
         !by $68,$6e,$88,$af,$eb,$39
         !by $9c,$13,$5e,$46,$04,$dc
         !by $d0,$dc,$10,$5e,$d6,$72
         !by $38,$26,$42,$8c,$08,$b8
         !by $a0,$b8,$20,$bc,$ac,$e4
         !by $70,$4c,$84,$18,$10,$70
         !by $40,$70,$40,$78,$58,$c8
         !by $e0,$98,$08,$30,$20


high
         !by 1,1,1,1,1,254
         !by 1,1,1,1,1,2
         !by 2,2,2,2,2,2
         !by 3,3,3,3,3,4
         !by 4,4,4,5,5,5
         !by 6,6,6,7,7,8
         !by 8,9,9,10,10,11
         !by 12,13,13,14,15,16
         !by 17,18,19,20,21,23
         !by 24,26,27,29,31,32
         !by 34,36,39,41,43,46
         !by 49,52,55,58,62,65
         !by 69,73,78,82,87,92
         !by 98,104,110,117,124,131
         !by 139,147,156,165,175,185
         !by 196,208,221,234,248




;***************************************
;            VARIABLEN

voicebyte !by 0,7,14

filterbyte !by 1,2,4

filterand
         !by %11111110
         !by %11111101
         !by %11111011



d400     !by 0,0,0

d401     !by 0,0,0

notenow  !by 0,0,0

voicenow !by 0,0,0

wibcount !by 0,0,0

wibpoint !by 0,0,0

adccount !by 0,0,0

adcwert  !by 0,0,0

byte1    !by 0,0,0

byte2    !by 0,0,0 ;10

pulslohi !by 0,0,0

padcwert !by 0,0,0

pulspoint !by 0,0,0

pulscount !by 0,0,0

wavepoint !by 0,0,0

d404     !by 0,0,0

waveand  !by 0,0,0

releasecount !by 0,0,0


tracklo  !by 0,0,0

trackhi  !by 0,0,0

repeat   !by 0,0,0

aktustep !by 0,0,0

steppointer !by 0,0,0


noteadc  !by 0,0,0

voiceadc !by 0,0,0


speedcount !by 0,0,0

speedwert !by 0,0,0

portacount !by 0,0,0

exarpeggio !by 0,0,0  ;29


filtercount !by 0


beginflag !by 0

speed1   !by 0



;*****ENDE DES VARIABLEN-SPEICHERS*****



;************************
;     STEP-POINTER

steps
         !wo s0,s1,s2,s3,s4
         !wo s5,s6,s7,s8,s9
         !wo s10,s11,s12,s13
         !wo s14,s15,s16,s17


;************************
;     TRACK-POINTER
trackslohi

         !by <track1,<track2,<track3
         !by >track1,>track2,>track3

speedtab
         !by 2

;************************


ftab2
         !by $7f,$00
wtab1
         !by $81,$41,$40,$40,$40,$40
         !by $40,$40,$40,$40,$40,$08


tab1
         !by $26,$0b,$08,$05,$03,$01
         !by $00,$ff,$fe,$fd,$fc




tab2
         !by $20,$07,$05,$28
         !by $26,$05,$28,$2c
         !by $05,$28,$2a,$05
         !by $28,$16,$14


wtab2
         !by $81,$11,$40,$80
         !by $80,$10,$80,$80
         !by $10,$80,$80,$10
         !by $80,$80,$80,$00



tab3
         !by $25,$10,$0c,$08
         !by $05,$02,$ff,$fd
         !by $fb,$fa


ptab1
         !by $3a,$20
         !by $0f,$df
         !by $07,$20,$07,$df


wtab4
         !by $21,$21,$21,$41


tab4
         !by $05+%10000000,$00
tab8
         !by $07+%10000000,$00

         !by $01+%01010000
         !by %11110000
         !by $03+%00010000
         !by %11110000
         !by $03+%01010000
         !by %11110000



tab5
         !by $00+%11000000,$e8
         !by $06+%10000000,$00

         !by $01+%01000000
         !by %11111111
         !by $03+%00000000
         !by %11111111
         !by $03+%01000000
         !by %11111111




ptab2
         !by $02,$01,$01,$80
         !by $0a,$60,$0a,$9f



ftab1
         !by $01,$00,$00,$a0
         !by $01,$f0,$05,$f8
         !by $00,$f0,$7f,$00


wtab3
         !by $81,$41


tab6
         !by $00,$07,$04

tab7
         !by $00,$18,$0c


wtab5
         !by $21,$21,$41

;************************
;        STEPS

s0
         !by $df,$ff


s1       !by $a5,$83,e2,$81,e2,e2
         !by $a2,sd
         !by $a5,$83,dh2
         !by e2,$81,e2,e2,e2
         !by $ff

s2
         !by $8b,$a1,bd,$83,bd
         !by $8f,bd
         !by $85,bd,$81,bd,$c3,$83,bd
         !by $8f,bd
         !by $ff



s3
         !by $81,$a1,bd
         !by $af,e5,d5,h4,e4,g4
         !by $a1,bd,$af,d5
         !by $a1,bd,$af
         !by e4,d4,h3,e3,g3
         !by h3,d4
         !by $81,$a1,bd
         !by $af,e5,d5,$a1,bd
         !by $af,e4,g4
         !by $a1,bd,$af,d5
         !by $a1,bd,$af
         !by e4,d4,h3,e3,g3
         !by h3,d4



         !by $81,$a1,bd
         !by $af,e5,d5,h4,e4,g4
         !by $a1,bd,$af,d5
         !by $a1,bd,$af
         !by e4,d4,h3,e3,g3
         !by h3,d4
         !by $81,$a1,bd
         !by $af,e5,d5,$a1,bd
         !by $af,e4,g4
         !by $a1,bd,$af,d5
         !by $a1,bd,$af
         !by e4,d4,$89,h3
         !by $ff





s5
         !by $ab,$61,$81
         !by e4,e4,e5,e4
         !by e5,e4,e5,e4
         !by e5,e5,e4,e5
         !by e4,e5,e4,e4

         !by $62,$81
         !by e5,e5,e4,e5
         !by e4,e5,e4,e5
         !by $63,e4,e4,e5,e4
         !by e5,e4,e5,e5

         !by $ab,$61,$81
         !by e4,e4,e5,e4
         !by e5,e4,e5,e4
         !by e5,e5,e4,e5
         !by e4,e5,e4,e4

         !by $62,$81
         !by e5,e5,e4,e5
         !by e4,e5,e4,e5
         !by $64,e4,e4,e5,e4
         !by e5,e4,e5,e5


         !by $60,$ff


s6
         !by $a2,sd,$a5,h1,d2,dh2
         !by $ff
s7
         !by $83,$a2,sd,$81,$a5,h1
         !by dh2
         !by $ff

s8
         !by $a2,sd,$a5,h1
         !by $a2,sd,sd
         !by $ff
s9
         !by $83,h1,$81,d2,dh2
         !by $ff


s10
         !by $a8,$81
         !by h4,e5,$87,g5
         !by $81,fh5,g5,$85,$b0,fh5
         !by e5,$83,d5
         !by $a8,$81,h4,e5
         !by $87,g5,$81
         !by fh5
         !by g5,$85,$b0,h5,a5,$83,g5

         !by $a8,$81,h4,e5,$87,g5
         !by $81,fh5,g5,$85,$b0,fh5
         !by e5,$83,d5
         !by $b0,e5,$db
         !by $ff



s11
         !by $83,$a2,sd,sd,$87,sd
         !by $83,sd,sd,$87,sd
         !by $83,sd,sd,sd,sd
         !by $81,sd,sd,sd,sd,sd,sd,sd
         !by sd,$ff


s12
         !by $a5,$85,e2,$81,e3,$81,$a2
         !by sd,$a5,$81,e3
         !by $83,e2,$81,e3,e3,$83,e2
         !by $81,$a2,sd,$a5
         !by $81,e3,$81,e3,e3
         !by $a5,$85,e2,$81,e3,$81,$a2
         !by sd,$a5,$81,e3
         !by $83,e2,$81,g2,g2,$83,g2
         !by $81,$a2,sd,$a5
         !by $81,a2,$81,a2,a2
         !by $a5,$85,g2,$81,g3,$a2,sd
         !by $a5,$81,g2,$83,g3
         !by $a5,$81,g3,g2,f3,g3
         !by $81,$a2,sd,$a5,$81
         !by f3,d3,c3
         !by $a5,$85,g2,$81,g3,$a2,sd
         !by $a5,$81,g2,$83,g3
         !by $a5,$81,g3,g3,g2,f3
         !by $81,$a2,sd,$a5,$81
         !by g3,g2,f3
         !by $a5,$81,d2,d2,d2,d3,$a2
         !by sd,$a5,d2,d3,d2
         !by $85,d2,$81,d3,$a2,sd
         !by $a5,$81,d3,a2,f2
         !by $a5,$81,d2,$83,d2
         !by $81,d3,$a2
         !by sd,$a5,d2,d3,d2
         !by $85,d3,$81,f3,$a2,sd
         !by $a5,$81,f3,d3,c3
         !by $a5,$81,e2,e2,$83,e2
         !by $a2,$81,sd,$a5,$83,e3
         !by e3,$81,h2,g2,e2,$a2,sd
         !by $a5,h2,g2,e3
         !by $a5,$87,e2,$81,$a2,sd
         !by $81,$a5,h1,e2,fh2
         !by $85,e3,$81,e3,$a2,sd,sd
         !by sd,sd,$ff

s4
         !by $a0,$df,$ff


s13
         !by $61,$ab
         !by $81,e5,e5,$83,e5,$85,e5
         !by $62,$81,e5
         !by $ff
s14
         !by $61,$ab
         !by $81,g5,g5,$83,g5,$85,g5
         !by $62,$81,g5
         !by $ff

s15
         !by $61,$ab
         !by $81,d5,d5,$83,d5,$85,d5
         !by $62,$81,d5
         !by $ff

s16
         !by $63,$ab
         !by $81,e5,e5,$83,e5,$85,e5
         !by $64,$81,e5
         !by $ff
s17
         !by $81,$a1,bd
         !by $af,e5,d5,h4,e4,g4
         !by $a1,bd,$af,d5
         !by $a1,bd,$af
         !by e4,d4,h3,e3,g3
         !by h3,d4
         !by $81,$a1,bd
         !by $af,e5,d5,$a1,bd
         !by $af,e4,g4
         !by $a1,bd,$af,d5
         !by $a1,bd,$af
         !by e4,d4,h3,e3,g3
         !by h3,d4
         !by $81,$a1,bd
         !by $af,g5,f5,d5,g4,ah4
         !by $a1,bd,$af,f5
         !by $a1,bd,$af
         !by g4,f4,d4,g3,ah3
         !by d4,g4
         !by $81,$a1,bd
         !by $af,g5,f5,$a1,bd
         !by $af,g4,ah4
         !by $a1,bd,$af,g5
         !by $a1,bd,$af
         !by g4,f4,d4,g3,ah3
         !by d4,f4
         !by $81,$a1,bd
         !by $af,d5,c5,a4,d4,f4
         !by $a1,bd,$af,c5
         !by $a1,bd,$af
         !by d4,c4,a3,d3,f3
         !by a3,c4
         !by $81,$a1,bd
         !by $af,d5,c5,$a1,bd
         !by $af,d4,f4
         !by $a1,bd,$af,c5
         !by $a1,bd,$af
         !by d4,c4,a3,d3,f3
         !by a3,c4
         !by $81,$a1,bd
         !by $af,e5,d5,h4,e4,g4
         !by $a1,bd,$af,d5
         !by $a1,bd,$af
         !by e4,d4,h3,e3,g3
         !by h3,d4
         !by $81,$a1,bd
         !by $af,e5,d5,$a1,bd
         !by $af,e4,g4
         !by $a1,bd,$af,d5
         !by $a1,bd,$af
         !by e4,d4,h3,e3,g3
         !by h3,d4

         !by $ff


track1
         !by $bf+2,2

         !by 3,3,3,3,3,3,3,11

         !by 17,17


         !by 0
         !by $ff
         !wo track1

track2
         !by 1,6,1,8
         !by 1,6,1,7
         !by 1,6,1,8
         !by 1,6,1,9

         !by 1,6,1,7
         !by 1,6,1,9
         !by 1,6,1,8

         !by 1,6,1,7
         !by 1,6,1,8
         !by 1,6,1,9
         !by 1,6,1,7

         !by 1,6,1,9
         !by 1,6,1,8
         !by 1,6,1,7
         !by 1,6,1,8
         !by 1,6,1,9

         !by 4,4
         !by 12,12
         ;!by 0


         !by $ff
         !wo track2


track3
         !by $bf+8,0
         !by 10,10

         !by 5,5,5,5

         !by 0,0
         !by $c3,13,$c3,14,$c3,15
         !by $c3,16
         !by $c3,13,$c3,14,$c3,15
         !by $c3,16

         !by $ff
         !wo track3

}