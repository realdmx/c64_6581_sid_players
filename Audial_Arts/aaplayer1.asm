
	; Converted from TurboAssembler to ACME by dmx87

	!to "Audial_Arts_v1.sid",plain

	* = $0000
	
	!text "PSID"
	!be16 2							; version 2
	!be16 $7c						; data offset
	!be16 $1000							; Load (0 = auto)
	!be16 reset						; Init
	!be16 play						; Play
	!be16 1							; num songs
	!be16 1							; first song
	!word 0
	!word 0
-	!text "Audial Arts v1"
	!fill 32 - (* - -)
-	!text "Francois Prijt"
	!fill 32 - (* - -)
-	!text "1991 Audial Arts"
	!fill 32 - (* - -)
	!be16 $0014							; v2 flags
	!be16 0							; Start page, page length (reloc)
	!be16 0							; Reserved

!pseudopc $1000 {


;---------------------------------------
;  (c) copyright FRANCOIS PRIJT / -AA-
;     Audial Arts / Flash Inc. 1991
;       >>  zong player v1.0  <<
;---------------------------------------
		 
;---------------------------------------
         *= $1000
;---------------------------------------
reset    jmp goreset
play     jmp player
stop     jmp turnoff
demo     jmp irqplay
         bit einde
;---------------------------------------
         !text "player by fp/-aa-(c) "
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
zero     = $fa   ;plaats zeropage adres.
;---------------------------------------
zp       = zero
zp2      = zero+2
xval     = zero+4
tval     = zero+5
varia    = $4a08 ;geheugen adres variab.
;---------------------------------------
trackl   = varia
trackh   = varia+3
trkpos   = varia+(3*2)
blkpos   = varia+(3*3)
blkrep   = varia+(3*4)
incsnd   = varia+(3*5)
incnte   = varia+(3*6)
cursnd   = varia+(3*7)
cursnd2  = varia+(3*8)
nteval   = varia+(3*9)
nteval2  = varia+(3*10)
durval   = varia+(3*11)
durval2  = varia+(3*12)
gldval   = varia+(3*13)
gldval2  = varia+(3*14)
gldnte   = varia+(3*15)
gldnte2  = varia+(3*16)
lopls    = varia+(3*17)
hipls    = varia+(3*18)
udpls    = varia+(3*19)
alpls    = varia+(3*20)
ahpls    = varia+(3*21)
inpls    = varia+(3*22)
tlpls    = varia+(3*23)
arpcnt   = varia+(3*24)
losid    = varia+(3*25)
hisid    = varia+(3*26)
logid    = varia+(3*27)
higid    = varia+(3*28)
time     = varia+(3*29)
udfilt   = varia+(3*30)
fltadc   = varia+(3*30)+1
vfi      = varia+(3*30)+2
frq      = varia+(3*30)+3
fltcnt   = varia+(3*30)+4
fltspd   = varia+(3*30)+5
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
         sta $d416
         sta $d417
         sta vfi
         tax
doloop   jsr getvals
         jsr noteon
         inx
         cpx #3
         bne doloop
         lda #$ff
         !by $0c    ;3 byte nop
turnoff  lda #0
ploff    sta player+1
         and #15
         sta $d418
         lda #1
         sta fltspd
         sta fltcnt
         rts
;---------------------------------------
player   ldx #0
         bmi dox
         stx $d404
         stx $d404+7
         stx $d404+14
         rts
;---------------------------------------
dox      inx
loopx    stx xval
         ldy time,x
         bne nt0
nt0      tya
         dey
         cpy xval
         beq getvals
         cmp durval,x
         bne goeff
         jmp noteon
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
notoff   cmp #$ff
         bne notnew
         iny
         lda (zp),y
         sta trkpos,x
         tay
         lda (zp),y
         sta tval
         cmp #$40
         bcc blok
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
         tya
donote   clc
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
goend    rts
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
         sta wave+1
         ldy sndpnt,x
         sta $d404,y
       ; sta $d405,y
       ; sta $d406,y
dorts    lda xval
         bmi goend
;---------------------------------------
effect   ldy cursnd,x
         lda losnd,y
         sta zp
         lda hisnd,y
         sta zp+1
         lda sndifrq,y
         sta ifrq+1
;---------------------------------------
jobs     ldy #$0f
         lda (zp),y
         and #4
         bne not4
         lda nteval,x
         !by $0c     ;3 byte nop
not4     lda #0
         clc
ifrq     adc #0
         sta setfreq+1
oksnd    lda time,x
         beq dozeros
         jmp noinit
dozeros  ldy #5
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
         ldy #10
         lda (zp),y
         beq fltoff
         and #2
         sta udfilt
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
vol      ora #15
         sta $d418
         iny
         lda (zp),y
         sta frq
         iny
         lda (zp),y
         sta minmaxf+1
         iny
         lda (zp),y
         sta fltadc
         lda vfi
         ora fon,x
         bne rsn
fltoff   lda vfi
         and foff,x
rsn      ora #0
setf     sta $d417
         sta vfi
         ldy #15
         lda (zp),y
         sta tval
         and #1
         beq notock1
         lda #$5f
         sta setfreq+1
         lda #$81
         !by $0c     ;3 byte nop
haper1   lda #1
         bne dwave1
notock1  lda tval
         and #2
         bne haper1
         ldy #0
         lda (zp),y
         and #240
         lsr
         lsr
         lsr
         lsr
         tay
         lda wavepr,y
         ora #1
dwave1   sta wave+1
         jmp setfreq
;---------------------------------------
noinit   lda alpls,x
         beq arp
         sta tval
         ldy #4
         lda time,x
         cmp (zp),y
         bmi arp
         lda udpls,x
         bne down
         lda lopls,x
         clc
         adc tval
         sta lopls,x
         lda hipls,x
         adc #0
         clc
         adc ahpls,x
         jmp endipl
down     lda lopls,x
         sec
         sbc tval
         sta lopls,x
         lda hipls,x
         sbc #0
         sec
         sbc ahpls,x
endipl   sta hipls,x
         ldy #6
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
         !by $0c     ;3 byte nop
haper    lda #1
         bne dwave
notock   lda tval
         and #2
         bne haper
freqz    ldy #3
         tya
         clc
         adc (zp),y
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
         bne dwave
wave2    lda (zp),y
         and #15
         tay
         lda wavepr,y
dwave    sta wave+1
;---------------------------------------
glid     lda gldval,x
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
         ldy #2
         lda (zp),y
         pha
         dey
         lda (zp),y
;---------------------------------------
dosnds   ldy sndpnt,x
         sta $d405,y
         pla
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
         beq filtjob
         jmp loopx
filtjob  lda fltadc
         beq end
         dec fltcnt
         beq dofilt
         rts
dofilt   lda fltspd
         sta fltcnt
minmaxf  ldy #0
         lda udfilt
         bne downf
         tya
         and #15
         clc
         asl
         asl
         asl
         asl
         sta max+1
         lda frq
         clc
         adc fltadc
max      cmp #0
         bcs end
         jmp endfil
downf    tya
         and #240
         sta mix+1
         lda frq
         sec
         sbc fltadc
mix      cmp #0
         bcc end
endfil   sta frq
         sta $d416
end      rts

;---------------------------------------
irqplay  sei
         lda #<irqd
         sta $0314
         lda #>irqd
         sta $0315
         lda #1
         sta $dc0d
         sta $d01a
         sta $d019
         lda #$1b
         sta $d011
         lda #$42
         sta $d012
         lda song
         jsr reset
         cli
         rts
;---------------------------------------
irqd     jsr play
         inc $d019
         jmp $ea31
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
wavepr   !by 0,$10,$20,$40,$80,$50
         !by $12,$14,$44,$14,$20,$14
         !by $10,$14,$20,$24

fon      !by 1,2,4
foff     !by 254,253,251

sndpnt   !by 0,7,14
;---------------------------------------
;sound tabel(low-hi waardes)
;---------------------------------------
losnd    !by <s0,<s1,<s2,<s3,<s4,<s5
         !by <s6,<s7,<s8,<s9,<s0a
         !by <s0b,<s0c,<s0d,<s0e,<s0f
         !by <s10,<s11,<s12,<s13,<s14
         !by <s15
;---------
hisnd    !by >s0,>s1,>s2,>s3,>s4,>s5
         !by >s6,>s7,>s8,>s9,>s0a
         !by >s0b,>s0c,>s0d,>s0e,>s0f
         !by >s10,>s11,>s12,>s13,>s14
         !by >s15
;---------------------------------------
;sounds
;---------------------------------------
s0       !by 0,0,0,0,0,0,0,0 ;c0
         !by 0,0,0,0,0,0,0,0

s1       !by $23,$01,$fa,$00 ;c1
         !by $00,$01,$20,$40
         !by $00,$00,$00,$19
         !by $a0,$4d,$08,$00

s2       !by $00,$0f,$e7,$00 ;c2
         !by $00,$08,$04,$40
         !by $00,$22,$00,$00
         !by $00,$00,$00,$04

s3       !by $00,$0f,$e8,$00 ;c3
         !by $00,$86,$0f,$40
         !by $00,$33,$00,$00
         !by $00,$00,$00,$04

s4       !by $00,$0f,$e7,$00 ;c4
         !by $00,$86,$04,$40
         !by $00,$34,$00,$00
         !by $00,$00,$00,$04

s5       !by $33,$01,$b9,$00 ;c5
         !by $00,$04,$04,$40
         !by $00,$90,$00,$00
         !by $00,$00,$00,$00

s6       !by $33,$01,$d9,$00 ;c6
         !by $00,$0c,$04,$40
         !by $00,$f0,$00,$00
         !by $00,$00,$00,$01

s7       !by $33,$01,$ba,$00 ;c7
         !by $08,$4c,$20,$78
         !by $00,$00,$00,$00
         !by $00,$00,$00,$01

s8       !by $33,$0c,$b7,$00 ;c8
         !by $00,$04,$04,$40
         !by $00,$c0,$00,$00
         !by $00,$00,$00,$00

s9       !by $44,$04,$96,$00 ;c9
         !by $00,$00,$00,$00
         !by $00,$00,$00,$6f
         !by $c0,$48,$0f,$00

s0a      !by $76,$00,$ed,$00 ;ca
         !by $00,$00,$00,$00
         !by $00,$90,$00,$00
         !by $00,$00,$00,$00

s0b      !by $33,$01,$ca,$00 ;cb
         !by $00,$06,$20,$20
         !by $00,$50,$03,$3f
         !by $90,$28,$01,$00

s0c      !by $44,$00,$7f,$00 ;cc
         !by $00,$00,$00,$00
         !by $00,$00,$01,$3c
         !by $30,$4a,$02,$00

s0d      !by $44,$03,$7d,$18 ;cd
         !by $00,$00,$08,$00
         !by $00,$00,$03,$32
         !by $a0,$4a,$01,$00

s0e      !by $33,$08,$8a,$00 ;ce
         !by $00,$0b,$06,$20
         !by $00,$90,$00,$00
         !by $00,$00,$00,$01

s0f      !by $33,$00,$ec,$01 ;cf
         !by $00,$03,$04,$70
         !by $00,$60,$01,$3f
         !by $30,$2c,$0c,$02

s10      !by $33,$06,$ac,$00 ;d0
         !by $00,$04,$10,$70
         !by $00,$90,$00,$1f
         !by $60,$6c,$04,$01

s11      !by $33,$06,$6a,$00 ;d1
         !by $00,$04,$04,$20
         !by $00,$f0,$00,$00
         !by $00,$00,$00,$01

s12      !by $33,$06,$ea,$00 ;d2
         !by $00,$03,$06,$40
         !by $00,$00,$01,$1f
         !by $30,$2a,$0c,$01

s13      !by $00,$00,$f8,$00 ;d3
         !by $00,$84,$20,$04
         !by $00,$22,$03,$1f
         !by $80,$48,$04,$04

s14      !by $00,$00,$f9,$00 ;d4
         !by $00,$84,$04,$40
         !by $00,$33,$01,$3f
         !by $48,$4c,$05,$04

s15      !by $33,$0c,$ea,$00 ;d5
         !by $00,$01,$04,$60
         !by $00,$00,$03,$1c
         !by $80,$3c,$04,$02
;---------------------------------------
;chane the hight of the sound.
;---------------------------------------
sndifrq  !by $00,$00,$0f,$1c ;$00-
         !by $1a,$00,$00,$00
         !by $00,$00,$00,$00
         !by $00,$00,$00,$00 ;-$0f

         !by $00,$00,$00,$0c ;$10-
         !by $10,$00,$00,$00
         !by $00,$00,$00,$00
         !by $00,$00,$00,$00 ;-$1f
;---------------------------------------
;arpregio numbers
;---------------------------------------
arplo    !by 0,<ap1,<ap2,<ap3,<ap4
         !by <ap5,<ap6,<ap7,<ap8,<ap9
         !by <ap10,<ap11,<ap12,<ap13
         !by <ap14,<ap15
;---------
arphi    !by 0,>ap1,>ap2,>ap3,>ap4
         !by >ap5,>ap6,>ap7,>ap8,>ap9
         !by >ap10,>ap11,>ap12,>ap13
         !by >ap14,>ap15
;---------------------------------------
;arpreggio (place it with ^^^!!)
;---------------------------------------
ap1      !by $2b,$29,$44,$46,$44,$2b
         !by $44,$46

ap2      !by $5f,$1e,$1c,$18,$14,$10
         !by $0e,$0c

ap3      !by $32,$0f,$0d,$32,$32

ap4      !by 0,0,0,0,7,7,5,5,5,3,3
         !by 3,$ff

ap5      !by 0,12,7,3,$ff

ap6      !by 0,12,8,5,$ff

ap7      !by 0,12,9,4,$ff

ap8      !by 0,0,12,8,3,$ff

ap9      !by 0,0,0,0,12,12,12,12
         !by 24,24,24,24,12,12,12,12
         !by $ff

ap10     !by 0,0,0,0,3,3,3,3,8,8,8
         !by 10,10,10,$ff

ap11     !by 0,0,0,0,2,2,2,2,5,5
         !by 9,9,9,$ff

ap12     !by 0,0,12,7,4,$ff

ap13     !by 0,0,9,5,3,$ff

ap14     !by 0,0,0,0,3,3,3,3,5,5,5
         !by 9,9,9,$ff

ap15     !by 0,0,0,0,12,12,12,12,$ff
;---------------------------------------
;golfvorm pattern tabel(for drums ect)
;---------------------------------------
wvelo    !by 0,<wv1,<wv2,<wv3,<wv4
;---------
wvehi    !by 0,>wv1,>wv2,>wv3,>wv4
;---------------------------------------
;golfvorm patterns(via tabel ^^^^!)
;---------------------------------------
wv1      !by $41,$41,$80,$80,$80,$10
         !by $80,$c4

wv2      !by $81,$41,$40,$40,$40,$40
         !by $40,$40,$ff

wv3      !by $81,$40,$40,$80,$80,$c1

wv4      !by $81,$41,$41,$80,$80,$c4
;---------------------------------------
;songs definition
;---------------------------------------
slo      !by <t11,<t12,<t13 ;songlo1
         !by <t21,<t22,<t23 ;songlo2
;---------
shi      !by >t11,>t12,>t13 ;songhi1
         !by >t21,>t22,>t23 ;songhi2
;---------------------------------------
;block definition
;---------------------------------------
blo      !by <t0,<t1,<t2,<t3,<t4,<t5
         !by <t6,<t7,<t8
;---------
bhi      !by >t0,>t1,>t2,>t3,>t4,>t5
         !by >t6,>t7,>t8
;---------------------------------------
;tracks song1
;---------------------------------------
;-track 1
t11      !by $e0,$98,$43,$05
         !by $98,$43,$06
         !by $98,$43,$05
         !by $80,$42,$07
         !by $ff,$00
;-track 2
t12      !by $e0,$80,$44,$01,$44,$00
         !by $80,$44,$01,$44,$00
         !by $80,$44,$01,$44,$00
         !by $80,$44,$01
         !by $ff,$00
;-track 3
t13      !by $e0,$80,$50,$02,$44,$00
         !by $ea,$80,$43,$06
         !by $e9,$98,$43,$05
         !by $ea,$80,$42,$06
         !by $ff,$00
;---------------------------------------
;tracks song2
;---------------------------------------
t21      !by $c0,$94,$02
         !by $ff,$00

t22      !by $cf,$80,$03,$82,$03
         !by $ff,$00

t23      !by $e0,$98,$01
         !by $ff,$00
;---------------------------------------
;keyboard controller
;---------------------------------------
keyon    !by 0  ;keyboard on(1)off(0)
sndset   !by $15;sound nr.#$
curnte   !by c3 ;height note
voicen   !by 0  ;wich voice (0-2)
song     !by 0  ;play song nr #
;---------------------------------------
;music-blocks
;---------------------------------------
t0       !by $c0,c0,$60,$ff
;---------------------------------------
t1       !by $c2
         !by c0,$18
         !by $c3
         !by c0,$18
         !by $c2
         !by c0,$12
         !by c0,$06
         !by $c3
         !by c0,$0c
         !by $c2
         !by c0,$0c

         !by $c2
         !by c0,$06
         !by c0,$06
         !by c0,$0c
         !by $c3
         !by c0,$18
         !by $c2
         !by c0,$12
         !by c0,$06
         !by $c3
         !by c0,$0c
         !by $c2
         !by c0,$0c
         !by $ff
;---------------------------------------
t2       !by $c9
         !by c6,$06
         !by c6,$06
         !by c6,$06
         !by c6,$06

         !by c6,$06
         !by c6,$06
         !by c6,$06
         !by c6,$06
         !by $ff
;---------------------------------------
t3       !by $c6
         !by c2,$0c
         !by c2,$0c
         !by c2,$06
         !by c2,$06
         !by c2,$0c

         !by g2,$0c
         !by g2,$0c
         !by g2,$06
         !by g2,$06
         !by g2,$0c

         !by f2,$0c
         !by f2,$0c
         !by f2,$06
         !by f2,$06
         !by f2,$0c

         !by e2,$0c
         !by e2,$0c
         !by e2,$06
         !by e2,$06
         !by e2,$0c
         !by $ff

;---------------------------------------
t4       !by $c7
         !by c2,$0c
         !by c2,$0c
         !by c2,$0c
         !by c2,$0c

         !by b1,$0c
         !by b1,$0c
         !by b1,$0c
         !by b1,$0c

         !by a1,$0c
         !by a1,$0c
         !by a1,$0c
         !by a1,$0c

         !by g1,$0c
         !by g1,$0c
         !by g1,$0c
         !by g1,$0c
         !by $ff

;---------------------------------------
t5       !by $cf

         !by b1,$18
         !by b1,$0c
         !by b1,$24
         !by b1,$18

         !by e2,$18
         !by e2,$0c
         !by e2,$24
         !by e2,$18

         !by b1,$18
         !by b1,$0c
         !by b1,$24
         !by b1,$18

         !by d2,$18
         !by d2,$0c
         !by d2,$24
         !by d2,$18
         !by $ff
;---------------------------------------
t6       !by $cb
         !by b1,$18
         !by b1,$0c
         !by b1,$24
         !by b1,$18

         !by d2,$18
         !by d2,$0c
         !by d2,$24
         !by d2,$18

         !by a1,$18
         !by a1,$0c
         !by a1,$24
         !by a1,$18

         !by c2,$18
         !by c2,$0c
         !by c2,$24
         !by c2,$18
         !by $ff

;---------------------------------------
t7       !by $d2
         !by a3,$18
         !by a3,$0c
         !by a3,$24
         !by a3,$18

         !by b3,$18
         !by c4,$0c
         !by d4,$24
         !by e4,$18

         !by d4,$18
         !by c4,$0c
         !by b3,$24+$18

         !by d4,$18
         !by c4,$0c
         !by b3,$24+$18

         !by $ff

;---------------------------------------
t8       !by $ca
         !by c3,$c8
         !by $e0,c5,$b8,$50
         !by c5,$c8
         !by $e1,c3,$b8,$50
         !by $ff
;---------------------------------------
einde    !by 0
;---------------------------------------

}