

	; A test tune by Jeroen Kimmel
	
	; Converted from TurboAssembler to ACME by dmx87
	
	; Something is missing here, there's a bass but inaudible


	!to "Kimmel_Jeroen_Test.sid",plain
	
	* = $0000

	!text "PSID"
	!be16 2							; version 2
	!be16 $7c						; data offset
	!be16 $1000						; Load (0 = auto)
	!be16 init						; Init
	!be16 play						; Play
	!be16 1						; num songs
	!be16 1							; first song
	!word 0
	!word 0
-	!text "Red's Player"
	!fill 32 - (* - -)
-	!text "Jeroen Kimmel (Red)"
	!fill 32 - (* - -)
-	!text "1989 Jeroen Kimmel"
	!fill 32 - (* - -)
	!word 0							; v2 flags
	!word 0							; Start page, page length (reloc)
	!word 0							; Reserved

!pseudopc $1000 {	

;0@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@.
;]Red's music player (C) 1989 J.Kimmel]
;-@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@=

           *= $1000   ; start of player

fd400      = $d400
fd401      = $d401
fd402      = $d402
fd403      = $d403
fd404      = $d404
fd405      = $d405
fd406      = $d406
fd416      = $d416
fd417      = $d417
afb        = $fb
afc        = afb+1
tp         = $ff
vol        = $d418

c1         = $0b
cis1       = $0c
d1         = $0d
dis1       = $0e
e1         = $0f
f1         = $10
fis1       = $11
g1         = $12
gis1       = $13
a1         = $14
ais1       = $15
b1         = $16
c2         = $17
cis2       = $18
d2         = $19
dis2       = $1a
e2         = $1b
f2         = $1c
fis2       = $1d
g2         = $1e
gis2       = $1f
a2         = $20
ais2       = $21
b2         = $22
c3         = $23
cis3       = $24
d3         = $25
dis3       = $26
e3         = $27
f3         = $28
fis3       = $29
g3         = $2a
gis3       = $2b
a3         = $2c
ais3       = $2d
b3         = $2e
c4         = $2f
cis4       = $30
d4         = $31
dis4       = $32
e4         = $33
f4         = $34
fis4       = $35
g4         = $36
gis4       = $37
a4         = $38
ais4       = $39
b4         = $3a
c5         = $3b
cis5       = $3c
d5         = $3d
dis5       = $3e
e5         = $3f
f5         = $40
fis5       = $41
g5         = $42
gis5       = $43
a5         = $44
ais5       = $45
b5         = $46
c6         = $47
cis6       = $48
d6         = $49
dis6       = $4a
e6         = $4b
f6         = $4c
fis6       = $4d
g6         = $4e
gis6       = $4f
a6         = $50
ais6       = $51
b6         = $52
c7         = $53

;
; @@@@ Program @@@@
;

stemselect !by $00
snelheid   !by $00
volbyte    !by $00

; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

glidesp    !by $00
userb1     !by $00
wave       !by $00
stemadd    !by $00
facco      !by $00
flagreg    !by $00
pfsweep    !by $00

           !by $00,$00,$00
           !by $00,$00,$00
           !by $00

           !by $00,$00,$00
           !by $00,$00,$00
           !by $00

; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

blockp     !by $00
counter    !by $00
stemno     !by $00
blockteller !by $00
st1pntr1   !by $00
st1pntr2   !by $00
vcode      !by $03


           !by $00
           !by $00
           !by $00
           !by $00
st2pntr1   !by $00
st2pntr2   !by $00
           !by $02


           !by $00
           !by $00
           !by $00
           !by $00
st3pntr1   !by $00
st3pntr2   !by $00
           !by $04

; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

acco       !by $00
lglide     !by $00
hglide     !by $00
d4000      !by $00
d4001      !by $00
pulwidthlo !by $00
pulwidthhi !by $00

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

; 0@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@.
; ] Last vary's, if not used crunch ]
; -@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@=

lad1       !by $00
had1       !by $00
filtersp   !by $00
tel        !by $00
vibsubd400 !by $00
vibsubd401 !by $00
stemtel    !by $00

lad2       !by $00
had2       !by $00
           !by $00
           !by $00
           !by $00
           !by $00
           !by $00

lad3       !by $00
had3       !by $00
           !by $00
           !by $00
           !by $00
           !by $00
           !by $00

; @@@      @@@@@@@@@

d4416      !by $00
filtstand  !by $00
lock       !by $00

; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

frtabhi    !by $01,$01,$01,$01
           !by $01,$01,$01,$01
           !by $01,$01,$01,$02
           !by $02,$02,$02,$02
           !by $02,$02,$03,$03
           !by $03,$03,$03,$04
           !by $04,$04,$04,$05
           !by $05,$05,$06,$06
           !by $07,$07,$07,$08
           !by $08,$09,$09,$0a
           !by $0b,$0b,$0c,$0d
           !by $0e,$0e,$0f,$10
           !by $11,$12,$13,$15
           !by $16,$17,$19,$1a
           !by $1c,$1d,$1f,$21
           !by $23,$25,$27,$2a
           !by $2c,$2f,$32,$35
           !by $38,$3b,$3f,$43
           !by $47,$4b,$4f,$54
           !by $59,$5e,$64,$6a
           !by $70,$77,$7e,$86
           !by $8e,$96,$9f,$a8
           !by $b3,$bd,$c8,$d4
           !by $e1,$ee,$fd


frtablow   !by $1c,$2d,$3e,$51
           !by $66,$7b,$91,$a9
           !by $c3,$dd,$fa,$18
           !by $38,$5a,$7d,$a3
           !by $cc,$f6,$23,$53
           !by $86,$bb,$f4,$30
           !by $7a,$b4,$fb,$47
           !by $98,$ed,$47,$a7
           !by $0c,$77,$e9,$61
           !by $e1,$68,$f7,$8f
           !by $30,$da,$8f,$4e
           !by $18,$ef,$d2,$c3
           !by $c3,$d1,$ef,$1f
           !by $60,$b5,$1e,$9c
           !by $31,$df,$a5,$87
           !by $86,$a2,$df,$3e
           !by $c1,$6b,$3c,$39
           !by $63,$be,$4b,$0f
           !by $0c,$45,$bf,$7d
           !by $83,$d6,$79,$73
           !by $c7,$7c,$97,$1e
           !by $18,$8b,$7e,$fa
           !by $06,$ac,$f3,$e6
           !by $8f,$f8,$2e

;          @@@@@@@@@@@@@@@@@@@@@

vibtab1    !by $00,$30,$60,$90
           !by $c0,$f0,$f0,$c0
           !by $90,$60,$30,$00

vibtab2    !by $00,$10,$20,$30
           !by $20,$10,$00,$10
           !by $20,$30,$20,$10

;          @@@@@@@@@@@@@@@@@@@@@

actab      !by $00,$00,$00,$00;1
           !by $00,$00,$00,$00
           !by $00,$00,$00,$00

           !by $00,$03,$07,$00;2
           !by $03,$07,$00,$03
           !by $07,$00,$03,$07

           !by $00,$04,$07,$00;3
           !by $04,$07,$00,$04
           !by $07,$00,$04,$07

;          @@@@@@@@@@@@@@@@@@@@@
digifreq
           !by 0,$55,$0a,$44
           !by 0,$55,$10,$44

;          @@@@@@@@@@@@@@@@@@@@@
digiwave
           !by $00,$81,$11,$81
           !by $00,$81,$11,$81

;          @@@@@@@@@@@@@@@@@@@@@

wavesw     !by $41,$81

;          @@@@@@@@@@@@@@@@@@@@@

setfreq    tya
           sta stemno,x
putfreq    clc
           adc stemadd,x
           tay
           lda frtabhi,y
           sta d4001,x
           lda frtablow,y
           sta d4000,x
           rts

newnoot    lda st1pntr1,x
           sta afb
           lda st1pntr2,x
           sta afc
           lda blockteller,x
           asl
           tay
           iny
           lda (afb),y
           cmp #$00          ; repeat ?
           bne loop3

           lda #$00
           sta blockteller,x

           ldy #$01
           lda (afb),y
           clv
           bvc goon

loop3      cmp #$01          ; end ?
           bne goon

           lda #$00          ; music off
           sta stemselect
           sta $d418
           rts

goon       cmp #$ff
           bne nomore
           lda st1pntr1,x
           sec
           sbc #$01
           sta afb
           lda (afb),y
           sta stemadd,x
           inc blockteller,x
           jmp newnoot

nomore     pha
           dey
           lda (afb),y
           sta afb
           pla
           sta afc

           lda blockp,x
           tay
           clc
           adc #$02
           sta blockp,x

           lda (afb),y
           cmp #$00
           bne getadsr

           lda #$00
           sta blockp,x
           inc blockteller,x
           jmp newnoot

getadsr    sta stemno,x
           iny
           lda (afb),y
           pha
           and #$1f
           sta userb1,x
           pla
           and #$e0
           lsr
           lsr
           tay
           lda lad1,x
           sta afb
           lda lad1+1,x
           sta afc
           lda #$00
           sta fd404,x
           sta fd402,x
           sta pulwidthlo,x
           sta lock
           sta stemtel,x
           lda filtstand
           sta d4416
           lda (afb),y
           iny
           sta fd403,x
           sta pulwidthhi,x
           and #$f0
           sta glidesp,x
           lda (afb),y
           iny
           sta wave,x
           lda (afb),y
           iny
           sta fd405,x
           lda (afb),y
           iny
           sta fd406,x
           and #$0f
           asl
           asl
           asl
           asl
           sta filtstand,x
           lda (afb),y
           iny
           sta pfsweep,x
           and #$0f
           sta filtersp,x
           lda (afb),y
           iny
           sta facco,x
           lda (afb),y
           iny
           sta acco,x
           lda (afb),y
           iny
           sta flagreg,x
           ldy stemno,x
           jsr setfreq
           lda d4001,x
           sta hglide,x
           lda d4000,x
           sta lglide,x
           lda wave,x
           sta fd404,x
           rts

fasttime

; 0@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@.
; ]            FASTTIMERS            ]
; -@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@=

; @@@@@@@@@ Accoorden @@@@@@@@@@

           lda facco,x
           beq pulse

           lda tel,x
           clc
           adc facco,x
           tay
           lda stemno,x
           clc
           adc actab,y
           jsr putfreq

; @@@@@@@@@ Pulsesweep @@@@@@@@@

pulse
           lda pulwidthlo,x
           clc
           adc pfsweep,x
           sta pulwidthlo,x
           sta fd402,x
           bcc nextos
           inc pulwidthhi,x
           lda pulwidthhi,x
           sta fd403,x
nextos

; @@@@@@@@@ Waveswitch @@@@@@@@@

           lda flagreg,x
           rol
           bcc begin

           lda stemtel,x
           pha
           lsr
           bcc oneven

           pla
           tay
           lda wavesw
           sta fd404,x
           jmp begin

oneven     pla
           tay
           lda wavesw+1
           sta fd404,x

begin

; @@@@@@@@@ Glide @@@@@@@@@@@

           lda flagreg,x
           lsr
           pha
           bcs downglide

           lda d4000,x
           clc
           adc glidesp,x
           sta d4000,x
           bcc next
           inc d4001,x
           jmp next

downglide
           lda d4000,x
           sec
           sbc glidesp,x
           sta d4000,x
           bcs next
           dec d4001,x

; @@@@@@@@@ Filtersweep @@@@@@@@

next       lda filtersp,x
           beq endisimo

           lda lock
           bne endisimo

           lda filtersp,x
           and #$08
           bne ones

conti      pla
           lsr
           pha
           bcs downfilt
           lda d4416
           clc
           adc filtersp,x
           adc filtersp,x
           sta d4416
           jmp end
downfilt   lda d4416
           sec
           sbc filtersp,x
           sbc filtersp,x
           sta d4416
           jmp end

ones       pla
           lsr
           pha
           bcs downfil
           lda d4416
           clc
           adc filtersp,x
           bcs lockset
           sta d4416
           jmp end

downfil    lda d4416
           sec
           sbc filtersp,x
           bcc lockset
           sta d4416
end        pla
           jmp finito

lockset    lda #$77
           sta lock
           jmp end

endisimo   pla
           lsr

; @@@@@@@@@ Vibrato @@@@@@@@@@

finito     pha
           inc tel,x
           lda tel,x
           cmp #$0c
           bne geennul
           lda #$00
           sta tel,x

geennul    inc stemtel,x
           lda #$1a      ; vibrato-delay
           cmp stemtel,x
           bcs noway

           tay
           pla
           lsr
           bcc nextvibo

           pha

           lda stemno,x
           tay
           jsr putfreq

           lda tel,x
           tay
           lda d4000,x
           clc
           adc vibtab1,y
           sta d4000,x
           bcc noinc
           inc d4001,x
noinc      pla

nextvibo
           lsr
           bcc gotodigi

           pha
           lda stemno,x
           tay
           jsr putfreq

           lda tel,x
           tay

           lda d4000,x
           clc
           adc vibtab2,y
           sta d4000,x
           bcc got
           inc d4001,x
got        pla
           jmp gotodigi

noway      pla
           lsr
           lsr

gotodigi
           pha

           lda #$04           ; digitaal
           cmp stemtel,x
           beq setreal
           bcc eind

           pla
           lsr
           bcc ddd2

           lda stemtel,x
           tay
           lda digiwave,y
           sta fd404,x
           lda digifreq,y
           sta fd401,x
           rts

ddd2
           lsr
           bcc ddd3

           lda stemtel,x
           tay
           lda digiwave+4,y
           sta fd404,x
           lda digifreq+4,y
           sta fd401,x
           rts

ddd3       lsr
           bcc fin

           lda stemtel,x
           clc
           pha
           adc #$80
           cmp #$82
           beq setreal
           bpl eind

           pla
           lda #$81
           sta fd404,x
           lda #$ff
           sta fd401,x
           rts

setreal    lda wave,x
           sta fd404,x
           lda d4001,x
           sta fd401,x
           pla
           rts

eind       pla

fin
;          0@@@@@@@@@@@@.
;          ]   finish   ]
;          -@@@@@@@@@@@@=

           lda d4000,x
           sta fd400,x
           lda d4001,x
           sta fd401,x
           lda d4416
           sta fd416
           rts

play       inc counter
           ldx #$00
again      lda stemselect
           and vcode,x
           beq nxtvoice
           jsr fasttime
           lda counter
           cmp snelheid
           bne nxtvoice
           dec userb1,x
           bpl nxtvoice
           jsr newnoot
nxtvoice   txa
           clc
           adc #$07
           tax
           cpx #$15
           bne again

           lda counter
           cmp snelheid
           bne nietop0
           lda #$00
           sta counter
nietop0    rts

initrout   ldy #$00
           lda (afb),y
           sta st1pntr1
           iny
           lda (afb),y
           sta st1pntr2
           iny
           lda (afb),y
           sta st2pntr1
           iny
           lda (afb),y
           sta st2pntr2
           iny
           lda (afb),y
           sta st3pntr1
           iny
           lda (afb),y
           sta st3pntr2
           ldx #$00
clearthem  lda #$00
           sta userb1,x
           sta blockteller,x
           sta blockp,x
           txa
           clc
           adc #$07
           tax
           cpx #$15
           bne clearthem
           ldx #$17
           lda #$00
           sta counter
loop1      sta fd400,x
           dex
           bne loop1

           lda volbyte
           sta vol
           lda #$f1        ; filter !!!
           sta fd417
           rts

; 0@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@.
; ]          ADSR-tabellen        ]
; -@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@=


adsr1      !by $00,$41,$08,$30
           !by $00,0,0,%01000000

           !by $00,$41,$08,$30
           !by $00,0,0,%00010000

           !by $01,$41,$08,$30
           !by $20,0,0,%01000000

           !by $01,$41,$08,$30
           !by $20,0,0,%00010000


;          @@@@@@@@@@@@@@@@@@@@@

adsr2      !by $03,$41,$bc,$40
           !by $10,0,0,%01001000

           !by $00,$00,$00,$00
           !by $00,0,0,%00000000

           !by $00,$00,$00,$00
           !by $00,0,0,%00000000

;          @@@@@@@@@@@@@@@@@@@@@

adsr3      !by $00,$11,$09,$00
           !by $00,0,0,%00000000

           !by $00,$00,$00,$00
           !by $00,0,0,%00000000

           !by $00,$00,$00,$00
           !by $00,0,0,%00000000


;0@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@.
;]             Music              ]
;-@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@=


stuk1      !by c1,$03,c1,$01
           !by c1,$01,dis1,$23
           !by c1,$01,f1,$03
           !by c1,$01,g1,$01
           !by c1,$01,c2,$21
           !by c1,$01,g1,$21
           !by dis1,$01

           !by 0,0

stuk3      !by c1,$03,c1,$01
           !by c1,$01,dis1,$23
           !by c1,$01,f1,$03
           !by c1,$01,g1,$01
           !by c1,$01,c2,$61
           !by c1,$41,g1,$61
           !by dis1,$41

           !by 0,0

stuk2      !by c1,$43,c1,$41
           !by c1,$41,dis1,$63
           !by c1,$41,f1,$43
           !by c1,$41,g1,$41
           !by c1,$41,c2,$61
           !by c1,$41,g1,$61
           !by dis1,$41

           !by 0,0

hft        !by c5,$1f,b4,$0f
           !by a4,$0f,e4,$1f
           !by c1,$3f

           !by c5,$1f,b4,$0f
           !by e4,$0f,f4,$1f
           !by c1,$3f

           !by d5,$1f,c5,$0f
           !by b4,$0f,b4,$0f
           !by a4,$1f

           !by e4,$03,f4,$03
           !by g4,$03,a4,$07
           !by b4,$07,c4,$07
           !by d4,$0a

           !by c1,$3f

           !by 0,0

; 0@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@.
; ]           Music Program        ]
; -@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@=

stem1
           !by $09,tp
           !wo stuk1
           !by 0,0

stem2
           !wo hft
           !by $02,tp
           !wo hft
           !by 0,0

stem3
           !by $09,tp
           !wo stuk1,stuk3
           !wo stuk2,stuk2
           !by $0e,tp
           !wo stuk2,stuk2
           !by $10,tp
           !wo stuk2
           !by $0e,tp
           !wo stuk2
           !by $09,tp
           !wo stuk2,stuk2

           !by 0,0

; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

watiswat   !wo stem1,stem2,stem3

; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;

init	   lda #$03
           sta snelheid
           lda #$6f
           sta volbyte
           lda #$03
           sta stemselect
           lda #<adsr1
           sta lad1
           lda #>adsr1
           sta had1
           lda #<adsr2
           sta lad2
           lda #>adsr2
           sta had2
           lda #<adsr3
           sta lad3
           lda #>adsr3
           sta had3
           lda #<watiswat
           sta afb
           lda #>watiswat
           sta afc
           jmp initrout



}