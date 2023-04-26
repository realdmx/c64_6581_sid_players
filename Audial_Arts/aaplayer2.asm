
	; Converted from TurboAssembler to ACME by dmx87


	!to "Audial_Arts_v2.sid",plain

	* = $0000
	
	!text "PSID"
	!be16 2							; version 2
	!be16 $7c						; data offset
	!be16 $1000							; Load (0 = auto)
	!be16 reset						; Init
	!be16 play						; Play
	!be16 2							; num songs
	!be16 1							; first song
	!word 0
	!word 0
-	!text "Audial Arts v2"
	!fill 32 - (* - -)
-	!text "Francois Prijt"
	!fill 32 - (* - -)
-	!text "1991 Audial Arts"
	!fill 32 - (* - -)
	!be16 $0014							; v2 flags
	!be16 0							; Start page, page length (reloc)
	!be16 0			

!pseudopc $1000 {

;---------------------------------------
; zong player v2.0 (c) Francois Prijt!
;       Audial Arts / Flash inc.
;---------------------------------------
;---------------------------------------
         *= $1000
;---------------------------------------
reset    jmp goreset
play     jmp player
stop     jmp turnoff
         bit einde
;---------------------------------------
         !text "player by fp/-aa- "
;---------------------------------------
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
b0       = 11
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
b1       = 23
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
b2       = 35
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
b3       = 47
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
b4       = 59
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
b5       = 71
c6       = 72
ch6      = 73
d6       = 74
dh6      = 75
e6       = 76
z6       = 77
fh6      = 78
g6       = 79
gh6      = 80
a6       = 81
ah6      = 82
b6       = 83
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
b7       = 95
;---------------------------------------
loftab   !by $0c,$1c,$2d,$3e,$51,$66
         !by $7b,$91,$a9,$c3,$dd,$fa
         !by $18,$38,$5a,$7d,$a3,$cc
         !by $f6,$23,$53,$86,$bb,$f4
         !by $30,$70,$b4,$fb,$47,$98
         !by $ed,$47,$a7,$0c,$77,$e9
         !by $61,$e1,$68,$f7,$8f,$30
         !by $da,$8f,$4e,$18,$ef,$d2
         !by $c3,$c3,$d1,$ef,$1f,$60
         !by $b5,$1e,$9c,$31,$df,$a5
         !by $87,$86,$a2,$df,$3e,$c1
         !by $6b,$3c,$39,$63,$be,$4b
         !by $0f,$0c,$45,$bf,$7d,$83
         !by $d6,$79,$73,$c7,$7c,$97
         !by $1e,$18,$8b,$7e,$fa,$06
         !by $ac,$f3,$e6,$8f,$f8,$2e
;---------------------------------------
hiftab   !by $01,$01,$01,$01,$01,$01
         !by $01,$01,$01,$01,$01,$01
         !by $02,$02,$02,$02,$02,$02
         !by $02,$03,$03,$03,$03,$03
         !by $04,$04,$04,$04,$05,$05
         !by $05,$06,$06,$07,$07,$07
         !by $08,$08,$09,$09,$0a,$0b
         !by $0b,$0c,$0d,$0e,$0e,$0f
         !by $10,$11,$12,$13,$15,$16
         !by $17,$19,$1a,$1c,$1d,$1f
         !by $21,$23,$25,$27,$2a,$2c
         !by $2f,$32,$35,$38,$3b,$3f
         !by $43,$47,$4b,$4f,$54,$59
         !by $5e,$64,$6a,$70,$77,$7e
         !by $86,$8e,$96,$9f,$a8,$b3
         !by $bd,$c8,$d4,$e1,$ee,$fd
;---------------------------------------
zero     = $fa   ;plaats zeropage adres.
;---------------------------------------
zp       = zero
zp2      = zero+2
xval     = zero+4
tval     = zero+5
;---------------------------------------
sndpnt   !by 0,7,14
trackl   !by 0,0,0
trackh   !by 0,0,0
trkpos   !by 0,0,0
blkpos   !by 0,0,0
blkrep   !by 0,0,0
incsnd   !by 0,0,0
incnte   !by 0,0,0
cursnd   !by 0,0,0
cursnd2  !by 0,0,0
nteval   !by 0,0,0
nteval2  !by 0,0,0
durval   !by 0,0,0
durval2  !by 0,0,0
gldval   !by 0,0,0
gldval2  !by 0,0,0
gldnte   !by 0,0,0
gldnte2  !by 0,0,0
lopls    !by 0,0,0
hipls    !by 0,0,0
udpls    !by 0,0,0
alpls    !by 0,0,0
ahpls    !by 0,0,0
inpls    !by 0,0,0
tlpls    !by 0,0,0
arpcnt   !by 0,0,0
losid    !by 0,0,0
hisid    !by 0,0,0
logid    !by 0,0,0
higid    !by 0,0,0
voice    !by 0,0,0
time     !by 0,0,0
udfilt   !by 0
fon      !by 1,2,4
foff     !by 254,253,251
;---------------------------------------
turnoff  ldy #0
         sty player+1
         rts
;---------------------------------------
goreset  sta $fa
         asl
         adc $fa
         tax
         ldy #$ff
         sty xval
         iny
         sty player+1
docop    lda slo,x
         sta trackl,y
         lda shi,x
         sta trackh,y
         lda #0
         sta voice,y
         sta trkpos,y
         sta incsnd,y
         sta incnte,y
         sta blkpos,y
         sta time,y
         sta blkrep,y
         inx
         iny
         cpy #3
         bne docop
         sta rsn+1
         sta bps+1
         sta vfi+1
         sta frq+1
         tax
doloop   jsr getvals
         jsr noteon
         inx
         cpx #3
         bne doloop
         ldx #$ff
         stx player+1
         rts
;---------------------------------------
player   ldx #0
         bmi doplay
         stx $d404
         stx $d404+7
         stx $d404+14
         rts
;---------------------------------------
doplay   inx
vol      lda #$0f
bps      ora #0
         sta $d418
rsn      lda #0
vfi      ora #0
         sta $d417
frq      lda #0
         sta $d416
loopx    stx xval
         lda voice,x
         bne goeff
         ldy time,x
         dey
         cpy xval
         beq getvals
         iny
         tya
         cmp durval,x
         bne goeff
         jsr noteon
goeff    jmp effect
getvals  lda trackl,x
         sta zp
         lda trackh,x
         sta zp+1
read1    ldy trkpos,x
readt    lda (zp),y
         sta tval
         cmp #$40
         bcc blok
         cmp #$fe
         bne notoff
         lda #0
         sta voice,x
         rts
notoff   cmp #$ff
         bne notnew
         iny
         lda (zp),y
         sta trkpos,x
         tay
         jmp readt
notnew   tay
         and #224
         cmp #$40
         bne notrep
         dey
         tya
         and #63
         sta blkrep,x
         inc trkpos,x
         jmp read1
notrep   cmp #$80
         bne notinc1
yepinc   tya
         and #31
         sta incnte,x
         inc trkpos,x
         jmp read1
notinc1  cmp #$a0
         beq yepinc
notinc   cmp #$c0
         bne notsnda
         tya
         and #31
         sta incsnd,x
         inc trkpos,x
         jmp read1
notsnda  cmp #$e0
         bne blok
         sty tval
         lda #32
         sec
         sbc tval
         sta incsnd,x
         inc trkpos,x
         jmp read1
blok     ldy tval
         lda blo,y
         sta zp2
         lda bhi,y
         sta zp2+1
blok1    ldy blkpos,x
hblok    lda (zp2),y
         sta tval
         tay
         cmp #$60
         bcc donote
         cmp #$ff
         bne notendb
         lda #0
         sta blkpos,x
         lda blkrep,x
         beq newblk
         dec blkrep,x
         jmp blok1
newblk   inc trkpos,x
         jmp read1
notendb  and #224
         cmp #$c0
         bne glidc
         tya
         and #31
         clc
         adc incsnd,x
         and #31
         sta cursnd2,x
         inc blkpos,x
         jmp blok1
glidc    cmp #$e0
         bne error
         tya
         and #1
         beq gdir
         lda #$80
gdir     sta tval
         ldy blkpos,x
         iny
         lda (zp2),y
         clc
         adc incnte,x
         ora tval
         sta gldnte2,x
         iny
         lda (zp2),y
         sta durval2,x
         iny
         lda (zp2),y
         sta gldval2,x
         iny
         tya
         sta blkpos,x
         lda xval
         bpl effect
         rts
error    inc $d020
donote   tya
         clc
         adc incnte,x
         sta nteval2,x
         ldy blkpos,x
         iny
         lda (zp2),y
         sta durval2,x
         iny
         tya
         sta blkpos,x
         lda #0
         sta gldval2,x
         lda xval
         bpl effect
         rts
;---------------------------------------
noteon   lda nteval2,x
         sta nteval,x
         lda cursnd2,x
         sta cursnd,x
         lda durval2,x
         sta durval,x
         lda gldval2,x
         sta gldval,x
         beq zeros
         lda gldnte2,x
         sta gldnte,x
         lda #0
zeros    sta tlpls,x
         sta udpls,x
         sta ahpls,x
         sta arpcnt,x
         sta time,x
         ldy sndpnt,x
         sta $d404,y
         sta $d405,y
         sta $d406,y
dorts    rts
;---------------------------------------
effect   ldy cursnd,x
         lda #0
         sta zp+1
         tya
         clc
         asl
         asl
         asl
         asl
         clc
         adc #<sound
         bcc noth
         inc zp+1
noth     sta zp
         tya
         clc
         lsr
         lsr
         lsr
         lsr
         lsr
         lsr
         clc
         adc #>sound
         adc zp+1
         sta zp+1
;---------------------------------------
         lda time,x
         bne jobs
         ldy #5
         lda (zp),y
         tay
         and #240
         sta lopls,x
         tya
         and #15
         sta hipls,x
         ldy #7
         lda (zp),y
         sta alpls,x
         iny
         lda (zp),y
         sta inpls,x
;---------------------------------------
jobs     ldy #$0f
         lda (zp),y
         and #4
         beq not4
         lda #0
         beq yep4
not4     lda nteval,x
yep4     sta setfreq+1
         ldy #$0a
         lda (zp),y
         bne dofilt
         lda vfi+1
         and foff,x
         sta vfi+1
         jmp jobs2
dofilt   and #2
         sta udfilt
         lda vfi+1
         ora fon,x
         sta vfi+1
         lda time,x
         bne notinf
         iny
         lda (zp),y
         sta tval
         and #15
         clc
         asl
         asl
         asl
         asl
         sta rsn+1
         lda tval
         and #240
         sta bps+1
         iny
         lda (zp),y
         sta frq+1
notinf   ldy #$0d
         lda udfilt
         bne downf
         lda (zp),y
         and #15
         clc
         asl
         asl
         asl
         asl
         sta max+1
         iny
         lda frq+1
         clc
         adc (zp),y
max      cmp #0
         bcs jobs2
         sta frq+1
         jmp jobs2
downf    lda (zp),y
         and #240
         sta mix+1
         iny
         lda frq+1
         sec
         sbc (zp),y
mix      cmp #0
         bcc jobs2
endfil   sta frq+1
;---------------------------------------
jobs2    ldy #4
         lda time,x
         cmp (zp),y
         bmi arp
         lda alpls,x
         beq arp
         lda udpls,x
         bne down
         lda lopls,x
         clc
         adc alpls,x
         sta lopls,x
         lda hipls,x
         adc #0
         clc
         adc ahpls,x
         sta hipls,x
         jmp endipl
down     ldy #7
         lda lopls,x
         sec
         sbc alpls,x
         sta lopls,x
         lda hipls,x
         sbc #0
         sec
         sbc ahpls,x
         sta hipls,x
endipl   ldy #6
         lda (zp),y
         cmp tlpls,x
         bne plscn
         lda #$ff
         sta tlpls,x
         lda udpls,x
         eor #$ff
         sta udpls,x
plscn    inc tlpls,x
         lda inpls,x
         beq arp
         clc
         adc alpls,x
         bcc endpls
         inc ahpls,x
endpls   sta alpls,x
arp      ldy #9
         lda (zp),y
         bne doarp
         jmp tock
doarp    sta tval
;---------------------------------------
         pha
chkarp   and #240
         beq noarp
         clc
         lsr
         lsr
         lsr
         lsr
         tay
         lda arplo,y
         sta aread+1
         lda arphi,y
         sta aread+2
         ldy arpcnt,x
aread    lda $ffff,y
         bpl noenda
         cmp #$fe
         beq noarp
         cmp #$ff
         bne noenda
         lda #0
         sta arpcnt,x
         tay
         jmp aread
noenda   clc
         adc setfreq+1
         sta setfreq+1
noarp    pla
;---------------------------------------
         and #15
         beq freqz2
         tay
         lda wvelo,y
         sta wread+1
         lda wvehi,y
         sta wread+2
         ldy arpcnt,x
wread    lda $ffff,y
         cmp #$ff
         beq tock
         inc arpcnt,x
         cmp #$c0
         bcc dwave
         and #$3f
         sta arpcnt,x
         lda tval
         pha
         jmp chkarp
freqz2   inc arpcnt,x
;---------------------------------------
tock     lda time,x
         cmp #2
         bcs freqz
         ldy #15
         lda (zp),y
         sta tval
         and #1
         beq notock
         lda #$5f
         sta setfreq+1
         lda #$81
         jmp dwave
notock   lda tval
         and #2
         beq freqz
         lda #1
         jmp dwave
freqz    ldy #3
         lda (zp),y
         clc
         adc #3
         sta woff+1
         ldy #0
         lda time,x
woff     cmp #0
         bcs wave2
         lda (zp),y
         and #240
         lsr
         lsr
         lsr
         lsr
         tay
         lda wavepr,y
         ora #1
         jmp dwave
wave2    lda (zp),y
         and #15
         tay
         lda wavepr,y
dwave    sta wave+1
         ldy #1
         lda (zp),y
         sta atdc+1
         iny
         lda (zp),y
         sta ssre+1
;---------------------------------------
         lda gldval,x
         beq setfreq
         lda time,x
         beq setfreq
         lda gldnte,x
         pha
         and #127
         tay
         lda gldval,x
         sta tval
         pla
         and #128
         bne downgl
         lda losid,x
         clc
         adc tval
         bcc notov
         inc hisid,x
notov    sta losid,x
         lda hisid,x
         cmp hiftab,y
         bcc dosnds
         lda losid,x
         cmp loftab,y
         bcc dosnds
         jmp endgld2
downgl   lda losid,x
         sec
         sbc tval
         bcs notov2
         dec hisid,x
notov2   sta losid,x
         lda hisid,x
         cmp hiftab,y
         bcs dosnds
         lda losid,x
         cmp loftab,y
         bcs dosnds
endgld2  lda #0
endgld   sta gldval,x
         lda gldnte,x
         and #127
         sta nteval,x
         sta setfreq+1
;---------------------------------------
setfreq  ldy #0
         lda loftab,y
         sta losid,x
         lda hiftab,y
         sta hisid,x
;---------------------------------------
dosnds   ldy sndpnt,x
atdc     lda #0
         sta $d405,y
ssre     lda #0
         sta $d406,y
wave     lda #0
         sta $d404,y
         lda lopls,x
         sta $d402,y
         lda hipls,x
         sta $d403,y
         lda losid,x
         sta $d400,y
         lda hisid,x
         sta $d401,y
;---------------------------------------
nextv    inc time,x
         inx
         cpx #3
         beq endplay
         jmp loopx
endplay  rts
;---------------------------------------
;sound instelling (pak handleiding!)
;---------------------------------------
wavepr   !by 0,$10,$20,$40,$80,$50
         !by $12,$14,$44,$14,$20,$14
         !by $10,$14,$20,$24
;---------------------------------------
sound    !by 0,0,240,0,0,0,0,0 ;00
         !by 0,0,0,0,0,0,0,0

         !by $33,$01,$ec,$00   ;01
         !by $00,$03,$04,$60
         !by $00,$00,$03,$1f
         !by $c0,$4c,$18,$00

         !by $00,$00,$e9,$00   ;02
         !by $00,$08,$20,$e0
         !by $00,$11,$03,$1f
         !by $c0,$2c,$12,$04

         !by $00,$07,$e7,$00   ;03
         !by $00,$08,$20,$40
         !by $00,$22,$00,$00
         !by $00,$00,$00,$00

         !by $22,$08,$99,$5a   ;04
         !by $00,$07,$08,$a4
         !by $00,$40,$00,$00
         !by $00,$00,$00,$00

         !by $22,$08,$99,$5a   ;05
         !by $00,$07,$08,$a0
         !by $00,$50,$00,$00
         !by $00,$00,$00,$00

         !by $22,$08,$99,$5a   ;06
         !by $00,$07,$08,$a0
         !by $00,$60,$00,$00
         !by $00,$00,$00,$00

         !by $22,$08,$99,$5a   ;07
         !by $00,$07,$08,$a0
         !by $00,$70,$00,$00
         !by $00,$00,$00,$00

         !by $33,$01,$ad,$00   ;08
         !by $02,$38,$0c,$50
         !by $00,$00,$00,$00
         !by $00,$00,$00,$01

         !by $66,$01,$c7,$00   ;09
         !by $00,$00,$00,$00
         !by $00,$00,$00,$00
         !by $00,$00,$00,$00

         !by $33,$08,$99,$00   ;0a
         !by $00,$0b,$08,$40
         !by $00,$80,$00,$00
         !by $00,$00,$00,$00

         !by $11,$00,$d9,$00   ;0b
         !by $00,$08,$20,$e0
         !by $00,$11,$00,$00
         !by $00,$00,$00,$00

         !by $33,$01,$9d,$00   ;0c
         !by $04,$08,$06,$27
         !by $00,$00,$00,$00
         !by $00,$00,$00,$01

         !by $22,$08,$99,$5a   ;0d
         !by $00,$07,$08,$a0
         !by $00,$a0,$00,$00
         !by $00,$00,$00,$00

         !by $22,$08,$99,$5a   ;0e
         !by $00,$07,$08,$a0
         !by $00,$b0,$00,$00
         !by $00,$00,$00,$00

         !by $33,$00,$9c,$01   ;0f
         !by $00,$03,$04,$70
         !by $00,$00,$01,$3f
         !by $30,$2c,$0c,$02
;---------------------------------------
;arpregio tabel
;---------------------------------------
arplo    !by 0,<ap1,<ap2,<ap3,<ap4
         !by <ap5,<ap6,<ap7,<ap8,<ap9
         !by <ap10,<ap11,<ap12,<ap13
         !by <ap14,<ap15
;---------------------------------------
arphi    !by 0,>ap1,>ap2,>ap3,>ap4
         !by >ap5,>ap6,>ap7,>ap8,>ap9
         !by >ap10,>ap11,>ap12,>ap13
         !by >ap14,>ap15
;---------------------------------------
;arpreggio (via tabel initialiseren!)
;---------------------------------------
ap1      !by $2b,$29,$44,$46,$44,$2b
         !by $44,$46

ap2      !by $5e,$21,$1f,$1c,$18,$13
         !by $10,$0c

ap3      !by 14,14,19,19,22,22,$ff

ap4      !by 12,15,19,$ff

ap5      !by 10,14,17,$ff

ap6      !by 12,17,22,$ff

ap7      !by 12,17,21,$ff

ap8      !by 15,19,24,$ff

ap9      !by 0,12,12,12,0,0,$ff

ap10     !by 14,19,22,$ff

ap11     !by 17,22,24,$ff

ap12     !by 0,0,0,0,2,2,2,2,5,5,9,9
         !by 9,$ff

ap13     !by 0,0,0,0,3,3,3,5,5,9,9,9
         !by $ff

ap14     !by 0,0,9,7,4,$ff

ap15     !by 0,0,12,7,4,$ff
;---------------------------------------
;golfvorm pattern tabel(voor drums ect)
;---------------------------------------
wvelo    !by 0,<wv1,<wv2,<wv3
;---------------------------------------
wvehi    !by 0,>wv1,>wv2,>wv3
;---------------------------------------
;golfvorm patterns(via tabel init!)
;---------------------------------------
wv1      !by $41,$41,$80,$80,$80,$10
         !by $80,$c4

wv2      !by $81,$41,$40,$40,$40,$40
         !by $40,$40,$ff

wv3      !by $01,$41,$80,$80,$80,$40
         !by $80,$c4
;---------------------------------------
;songs definieren
;---------------------------------------
slo      !by <t11,<t12,<t13 ;songlo1
         !by <t21,<t22,<t23 ;songlo2
;---------------------------------------
shi      !by >t11,>t12,>t13 ;songhi1
         !by >t21,>t22,>t23 ;songhi2
;---------------------------------------
;block definieren
;---------------------------------------
blo      !by <t0,<t1,<t2,<t3,<t4,<t5
         !by <t6,<t7,<t8,<t9,<t0a
         !by <t0b,<t0c,<t0d,<t0e,<t0f
         !by <t10,<t20
;---------------------------------------
bhi      !by >t0,>t1,>t2,>t3,>t4,>t5
         !by >t6,>t7,>t8,>t9,>t0a
         !by >t0b,>t0c,>t0d,>t0e,>t0f
         !by >t10,>t20
;---------------------------------------
;tracks song1
;---------------------------------------
t11      !by $c0
         !by $86,$49,$0c,$86,$07,$48
         !by $0d,$88,$07,$82,$0e,$11
         !by $ff,$04
;---------------------------------------
t12      !by $c0
         !by $92,$09,$0a,$92,$01,$90
         !by $01,$8e,$01,$8d,$01,$8e
         !by $01,$90,$01,$8e,$01,$90
         !by $01,$92,$01,$90,$01,$8e
         !by $01,$8d,$01,$8e,$01,$90
         !by $01,$8e,$01,$90,$01,$94
         !by $01,$92,$01,$90,$01,$8f
         !by $01,$90,$01,$92,$01,$90
         !by $01,$92,$01,$90,$01,$95
         !by $01,$91,$01,$93,$01,$90
         !by $01,$95,$01,$91,$01,$93
         !by $01,$ff,$04
;---------------------------------------
t13      !by $c0
         !by $86,$08,$0b,$86,$02,$02
         !by $03,$03,$04,$04,$05,$05
         !by $04,$04,$03,$03,$04,$04
         !by $03,$06,$02,$02,$03,$03
         !by $04,$04,$05,$05,$04,$04
         !by $03,$03,$04,$04,$03,$06
         !by $88,$02,$02,$03,$03,$04
         !by $04,$05,$05,$04,$04,$03
         !by $03,$04,$04,$03,$06,$84
         !by $02,$02,$0f,$0f,$87,$03
         !by $03,$89,$10,$03,$84,$02
         !by $02,$0f,$0f,$87,$03,$03
         !by $89,$10,$03,$ff,$04
;---------------------------------------
;tracks song2
;---------------------------------------
t21      !by $86,$c0,$02,$02,$03,$03
         !by $04,$04,$05,$05
         !by $ff,$00
;---------------------------------------
t22      !by $86,$c0,$00
         !by $ff,$00
;---------------------------------------
t23      !by $84,$c3,$0e,$86,$c0
         !by $44,$01
         !by $ff,$03
;---------------------------------------
;block memory
;---------------------------------------
t0       !by $c0,$00,$60,$ff

t1       !by $c1,$0c,$12,$18,$06,$c2
         !by $00,$0c,$c1,$0a,$06,$0a
         !by $06,$0c,$0c,$0c,$0c,$c2
         !by $00,$0c,$c1,$0a,$0c,$c1
         !by $0c,$12,$18,$06,$c2,$00
         !by $0c,$c1,$0a,$06,$0a,$06
         !by $0c,$0c,$0c,$0c,$c2,$00
         !by $0c,$c1,$0a,$0c,$ff

t2       !by $c3,$00,$0c,$c4,$24,$0c
         !by $24,$06,$24,$0c,$24,$06
         !by $c3,$00,$06,$c4,$24,$06
         !by $24,$0c,$24,$06,$24,$0c
         !by $24,$06,$ff

t3       !by $c3,$00,$0c,$c5,$24,$0c
         !by $24,$06,$24,$0c,$24,$06
         !by $c3,$00,$06,$c5,$24,$06
         !by $24,$0c,$24,$06,$24,$0c
         !by $24,$06,$ff

t4       !by $c3,$00,$0c,$c5,$22,$0c
         !by $22,$06,$22,$0c,$22,$06
         !by $c3,$00,$06,$c5,$22,$06
         !by $22,$0c,$22,$06,$22,$0c
         !by $22,$06,$ff

t5       !by $c3,$00,$0c,$c4,$1f,$0c
         !by $1f,$06,$1f,$0c,$1f,$06
         !by $c3,$00,$06,$c4,$1f,$06
         !by $1f,$0c,$1f,$06,$1f,$0c
         !by $1f,$06,$ff

t6       !by $c3,$00,$0c,$c5,$24,$0c
         !by $24,$06,$24,$0c,$24,$06
         !by $c3,$00,$06,$c6,$24,$06
         !by $24,$0c,$c7,$24,$06,$24
         !by $0c,$c4,$24,$06,$ff

t7       !by $c0,$00,$18,$c8,$30,$0c
         !by $32,$0c,$33,$18,$32,$0c
         !by $30,$84,$30,$0c,$32,$0c
         !by $33,$18,$35,$0c,$30,$84
         !by $30,$0c,$32,$0c,$33,$18
         !by $2e,$0c,$2c,$3c,$2e,$0c
         !by $30,$24,$30,$18,$2e,$18
         !by $2e,$18,$2c,$0c,$2b,$84
         !by $30,$0c,$32,$0c,$33,$18
         !by $2e,$0c,$2c,$3c,$2e,$0c
         !by $30,$24,$33,$18,$32,$18
         !by $32,$18,$30,$0c,$32,$84
         !by $30,$0c,$32,$0c,$33,$18
         !by $2e,$0c,$2c,$3c,$2e,$0c
         !by $30,$24,$37,$18,$35,$18
         !by $35,$18,$33,$0c,$32,$48
         !by $2e,$0c,$30,$18,$ff

t8       !by $c4,$24,$60,$24
         !by $60,$c5,$24,$60,$24,$60
         !by $22,$60,$22,$60,$c4,$1f
         !by $60,$1f,$60,$c5,$22,$60
         !by $22,$60,$24,$60,$24,$60
         !by $22,$60,$22,$60,$24,$60
         !by $24,$60,$ff

t9       !by $c1,$0c,$54,$0a
         !by $0c,$0c,$48,$0a,$0c,$0c
         !by $0c,$0a,$54,$08,$0c,$0a
         !by $3c,$08,$0c,$0a,$0c,$08
         !by $0c,$08,$54,$07,$0c,$08
         !by $3c,$07,$0c,$08,$06,$07
         !by $12,$07,$54,$05,$0c,$07
         !by $3c,$07,$0c,$05,$0c,$07
         !by $0c,$08,$54,$07,$0c,$08
         !by $48,$07,$0c,$08,$0c,$0a
         !by $54,$08,$0c,$0a,$3c,$08
         !by $0c,$08,$0c,$0a,$0c,$08
         !by $54,$07,$0c,$08,$3c,$07
         !by $0c,$08,$0c,$09,$0c,$0a
         !by $54,$08,$0c,$0a,$3c,$0a
         !by $0c,$0f,$0c,$0e,$0c,$ff

t0a      !by $c1,$0c,$6c,$18,$12,$0a
         !by $12,$c2,$00,$0c
         !by $00,$0c,$00,$06,$00,$06
         !by $00,$06,$00,$06,$ff

t0b      !by $c4,$24,$84,$c4,$24,$18
         !by $c7,$24,$18,$ca
         !by $24,$0c,$ff

t0c      !by $c3,$00,$18,$00,$18,$00
         !by $18,$00,$18,$00,$18,$00
         !by $18,$00,$18,$cb,$00,$0c
         !by $00,$0c,$ff

t0d      !by $c0,$00,$0d,$c9,$40,$05
         !by $41,$06,$c0,$00,$0d,$c9
         !by $42,$05,$43,$06,$c0,$00
         !by $0d,$c9,$44,$05,$45,$06
         !by $c0,$00,$0d,$c9,$46,$05
         !by $47,$06,$c0,$00,$0d,$c9
         !by $48,$05,$47,$06,$c0,$00
         !by $0d,$c9,$46,$05,$45,$06
         !by $c0,$00,$0d,$c9,$44,$05
         !by $43,$06,$c0,$00,$0d,$c9
         !by $42,$05,$41,$06,$ff

t0e      !by $c0,$00,$18,$cc,$32,$0c
         !by $35,$0c,$37,$18,$39,$0c
         !by $37,$18,$35,$18,$32,$18
         !by $35,$24,$3a,$18,$39,$18
         !by $37,$18,$35,$0c,$37,$6c
         !by $3a,$18,$39,$18,$37,$18
         !by $35,$0c,$37,$48,$39,$0c
         !by $3a,$0c,$3c,$3c,$3a,$18
         !by $39,$0c,$35,$6c,$ff

t0f      !by $c3,$00,$0c,$cd,$22,$0c
         !by $22,$06,$22,$0c,$22,$06
         !by $c3,$00,$06,$cd,$22,$06
         !by $22,$0c,$22,$06,$22,$0c
         !by $22,$06,$ff

t10      !by $c3,$00,$0c,$ce,$1d,$0c
         !by $1d,$06,$1d,$0c,$1d,$06
         !by $c3,$00,$06,$ce,$1d,$06
         !by $1d,$0c,$1d,$06,$1d,$0c
         !by $1d,$06,$ff

t20      !by $c0,$00,$18,$cc,$32,$0c
         !by $35,$0c,$37,$18,$39,$0c
         !by $37,$18,$35,$18,$32,$18
         !by $35,$24,$3a,$18,$39,$18
         !by $37,$18,$35,$0c,$37,$6c
         !by $3a,$18,$39,$18,$37,$18
         !by $35,$0c,$37,$48,$39,$0c
         !by $3a,$0c,$3c,$3c,$3e,$18
         !by $3f,$0c,$41,$6c,$ff
;---------------------------------------
einde    !by 0

}