
	; Converted from TurboAssembler to ACME by dmx87

	!to "Bjerregaard_J_Myth.sid",plain

	* = $0000
	
	!text "PSID"
	!be16 2							; version 2
	!be16 $7c						; data offset
	!be16 $1000							; Load (0 = auto)
	!be16 setmus						; Init
	!be16 play						; Play
	!be16 2							; num songs
	!be16 1							; first song
	!word 0
	!word 0
-	!text "Myth"
	!fill 32 - (* - -)
-	!text "Johannes Bjerregaard"
	!fill 32 - (* - -)
-	!text "1989 Johannes Bjerregaard"
	!fill 32 - (* - -)
	!be16 $0014							; v2 flags
	!be16 0							; Start page, page length (reloc)
	!be16 0							; Reserved

!pseudopc $1000 {

;-------------------------------
zp       = $fc;zp+1 used as well
;-------------------------------
         *= $1000

         ;this is the player...

         jmp setmus;init(.a=music no)
         jmp musoff;stops playing tune
         jmp play;jsr in irq-interrupt

rtime	!by 0,0,0

faderate !by 0;store fade rate(1-127)
                ;here.....

play

muson    = *+1
         lda #0
         bne playit
         rts

novoices = *+1
playit   ldx #2

         dec tempocnt

nextv
		lda $d012
        sta rtime,x

         lda tempocnt
         bpl dope

temposet = *+1

         lda #2
         sta tempocnt
         dec len
         dec len+1
         dec len+2
         jmp dope

setnow
         ldy envptr,x
         lda set1+2,y
         and #$0f
         lsr
         lda #1
         sta vibdir,x;vibrato direction

         lda #0
         sta vibrate,x
         sta vibratehi,x
         sta notsetyet,x

         sta vibbase,x
         sta vibbasehi,x

         sta pwtimes,x
         sta ftms,x

         ldy note,x

         lda fqdatlo,y
         sta glitmp2,x
         lda fqdathi,y
         sta glitmp2hi,x

         ldy endnote,x

         lda fqdatlo,y
         sta glitmp,x
         lda fqdathi,y
         sta glitmphi,x

         lda glitmp,x
         sec
         sbc glitmp2,x
         sta glitmp,x

         lda glitmphi,x
         sbc glitmp2hi,x
         sta glitmphi,x

         jmp zd
dope
         lda notsetyet,x
         bne setnow

         lda len,x
         bpl donot

         lda tempocnt
         beq jsetvals

         cmp #1
         beq readvals

donot    jmp compute
jsetvals
         jmp setvals
;-------------------------
readvals
         txa
         asl
         tay

         lda musstart,y
         sta zp
         lda musstart+1,y
         sta zp+1

         ldy seqno,x

reread   lda (zp),y;read sequence no.

         bpl less

         cmp #$ff
         bne notrep

         ldy #0
fjfjf
         sty seqno
         sty seqno+1
         sty seqno+2
         jmp reread

notrep   cmp #$fe
         bne notend
;-----------------------
musoff   lda #0
         sta muson
         sta $d404
         sta $d404+7
         sta $d404+14
         rts
;-----------------------
notend
         and #$7f
         sec
         sbc #$40
         sta transp,x
         iny
         inc seqno,x
         jmp reread
;-------------------------
less     tay         ;read sequence data

         lda #0
         sta glide,x
         lda envptr,x
         sta envset,x

         lda loseq,y
         sta zp
         lda hiseq,y
         sta zp+1

         ldy seqptr,x

         lda #$f6
         sta gate,x

reread2  lda (zp),y

         bmi notarp2
         cmp #$60
         bcs notarp3
         jmp nott
notarp3
         and #$1f
         sta arpselect,x
         iny
         jmp reread2
notarp2
         cmp #$fe
         bne notsus
         iny
         lda (zp),y
         sta sr,x
         iny
         jmp reread2
notsus
         cmp #$fc;tie next two notes
         bne notarp

         lda #$f7
         sta gate,x
         iny
         jmp reread2

notarp   cmp #$e0
         bcc notpause

         and #$1f
         sta len,x
         iny
         tya
         sta seqptr,x

         lda (zp),y
         cmp #$ff
         bne jcmp2

         inc seqno,x
         lda #0
         sta seqptr,x
jcmp2
         jmp setsid

notpause cmp #$c0
         bcc notlen

         and #$1f
         sta setlen,x
         sta nlen,x
         iny
         jmp reread2
notlen
         pha
         lda setlen,x
         sta nlen,x
         pla

         cmp #$a0
         bcc notglide

         and #$1f
         sta temp

         lda #0
         sta gltemp2,x

         lda temp
         asl
         asl
         asl
         rol gltemp2,x
         asl
         rol gltemp2,x
         asl
         rol gltemp2,x
         ora #1
         sta glide,x

         iny
         lda (zp),y
         clc
         adc transp,x
         sta endnote,x

         iny
         jmp reread2

notglide ;this gotta be $80-$9f

         and #$1f
         asl
         asl
         asl
         sta envset,x
         sty temp

         tay
         lda set1+4,y
         and #$1f
         sta arpselect,x

         lda set1+1,y
         sta sr,x

         ldy temp
         iny
         jmp reread2

read2    iny
         lda (zp),y

nott     clc
         adc transp,x
         sta nxnote,x

         iny
         tya
         sta seqptr,x

         lda (zp),y
         cmp #$ff
         bne whi

         lda #0
         sta seqptr,x
         inc seqno,x
whi
         lda gate,x
         and #1
         bne bingo

         ldy envptr,x
         lda set1+4,y
         bpl bingo

         lda #0
         ldy d4point,x
         sta $d405,y
         sta $d406,y

bingo
         jmp zd
;----------------------
setvals

         lda nxnote,x
         sta note,x

         lda envset,x
         tay
         sta envptr,x

         lda nlen,x
         sta len,x

         lda gate,x
         and #$01
         beq burp

         jmp compute
burp
         lda #1
         sta notsetyet,x

         lda #0
         sta glidebase,x
         sta glidebasehi,x

         lda set1,y
         sta ad,x

         ldy envptr,x

         lda #1;byte 0 = repeat point
         sta vibcounter,x;vib(delay)

         lda set1+3,y;pulse
         tay
         lda p0,y
         sta pwpreset,x
         iny
         iny
         tya
         sta pwpcounter,x

         ldy envptr,x
         lda set1+5,y
         tay
         lda f0,y
         sta fipreset,x
         iny
         iny
         iny
         tya
         sta fipcounter,x

         ldy envptr,x;waves
         lda set1+6,y
         tay
         lda w0,y
         sta wfpreset,x
         iny
         tya
         sta wfpcounter,x

         ldy arpselect,x
         lda noadrlist,y
         tay
         lda n0,y
         sta nopreset,x
         iny
         tya
         sta nopcounter,x

         ldy envptr,x
         lda set1+7,y
         and #$f0
         lsr
         lsr
         sta delaycheck,x

         lda set1+2,y
         and #$f0
         lsr
         lsr
         lsr
         sta regdelay,x

         lda #$f7
         sta gate,x

         lda set1+3,y
         tay
         iny
         lda p0,y

         pha
         and #$0f
         sta hipw,x
         pla
         and #$f0
         sta lopw,x

         ldy envptr,x
         lda set1+5,y
         tay

         lda f0+1,y
         beq filtoff

         sta ctof

         lda f0+2,y
         sta d418

         lda d417
         ora font,x
         sta d417
         jmp guf

filtoff  lda d417
         and fofft,x
         sta d417
guf
         jmp zd
;---------------------
compute

         ldy envptr,x
         lda regdelay,x

         cmp vibcounter,x
         bcc vibr
notvib   jmp novibr
vibr
         ldy tempnote,x

         lda fqdatlo+1,y
         sec
         sbc fqdatlo,y
         sta zp
         lda fqdathi+1,y
         sbc fqdathi,y
         sta zp+1

         ldy envptr,x
         lda set1+7,y
         and #$0f
         tay ;vibrato rate

         lda zp+1

rollover dey
         bmi enough

         lsr
         ror zp
         jmp rollover

enough   sta zp+1
         ldy envptr,x

         lda set1+2,y
         and #$0f
         lsr
         cmp vibdir,x
         bcs vibdown

         lda vibbase,x
         clc
         adc vibrate,x
         sta vibbase,x
         lda vibbasehi,x
         adc vibratehi,x
         sta vibbasehi,x
         jmp setrate

vibdown  lda vibbase,x
         sec
         sbc vibrate,x
         sta vibbase,x
         lda vibbasehi,x
         sbc vibratehi,x
         sta vibbasehi,x

setrate  lda set1+7,y
         and #$f0
         lsr
         lsr
         cmp vibcounter,x
         bcc dirinc
         lda zp
         clc
         adc vibrate,x
         sta vibrate,x
         lda vibratehi,x
         adc zp+1
         sta vibratehi,x

dirinc   inc vibdir,x
         lda set1+2,y
         and #$0f
         cmp vibdir,x
         bcs novibr
         lda #0
         sta vibdir,x
novibr
;------------------------
         ldy pwpcounter,x

         lda pwtimes,x
         bne storesame

reread4  lda p0,y

         cmp #$ff
         bne valueok

         lda pwpreset,x
         sta pwpcounter,x
         tay
         lda p0,y
         jmp valueok

storesame
         lda pwvalue,x

valueok  sta pwvalue,x

         bmi add

         asl
         sta temp
         lda lopw,x
         sec
         sbc temp
         sta lopw,x

         lda hipw,x
         sbc #0
         sta hipw,x

gettimes lda pwtimes,x
         bne decpwt

         iny
         lda p0,y
         sta pwtimes,x

         inc pwpcounter,x
         inc pwpcounter,x

         jmp nomorepw

add      asl
         clc
         adc lopw,x
         sta lopw,x

         lda hipw,x
         adc #0
         sta hipw,x

         jmp gettimes

decpwt   dec pwtimes,x
nomorepw
;-----------------------
         ldy fipcounter,x

         lda ftms,x
         bne readsame
rrread
         lda f0,y
         cmp #$ff
         bne addsubok

         lda fipreset,x
         sta fipcounter,x
         tay
         lda f0,y
         jmp addsubok

readsame lda fdta,x
addsubok sta fdta,x

         lda ctof
         clc
         adc fdta,x
         sta ctof

         lda ftms,x
         bne notimeset

         iny
         lda f0,y
         sta ftms,x

         inc fipcounter,x
         inc fipcounter,x

notimeset dec ftms,x
nofiltchange
;-------------------------
         ;glide

         lda glide,x
         bne glok
         jmp zd
glok
         lda endnote,x
         cmp note,x
         bcs glideup

         lda glidebase,x
         sec
         sbc glide,x
         sta glidebase,x
         lda glidebasehi,x
         sbc gltemp2,x
         sta glidebasehi,x

         lda glidebase,x
         sec
         sbc glitmp,x
         lda glidebasehi,x
         sbc glitmphi,x
         bcs zd

glidend  lda #0
         sta glide,x
         sta glidebase,x
         sta glidebasehi,x
         lda endnote,x
         sta note,x
         jmp zd
glideup
         lda glidebase,x
         clc
         adc glide,x
         sta glidebase,x

         lda glidebasehi,x
         adc gltemp2,x
         sta glidebasehi,x

         lda glitmp,x
         sec
         sbc glidebase,x
         lda glitmphi,x
         sbc glidebasehi,x
         bcs zd
         jmp glidend
zd
;---------------------
         ldy wfpcounter,x

         lda w0,y
         cmp #$ff
         bne waveform

         lda wfpreset,x
         sta wfpcounter,x
         tay
         lda w0,y

waveform sta wfm,x
         inc wfpcounter,x
nochang
;------------------------
         ldy nopcounter,x

         lda n0,y
         cmp #$ff
         bne noteok

         lda nopreset,x
         sta nopcounter,x
         tay

         lda n0,y

noteok   cmp #$80
         and #$7f
         bcs steady

         clc
         adc note,x
         ldy note,x
         beq nosetvib
steady
         sta tempnote,x
         tay
         lda glide,x
         bne nosetvib

         lda fqdatlo,y
         clc
         adc vibbase,x
         sta lofq,x

         lda fqdathi,y
         adc vibbasehi,x
         sta hifq,x
         jmp jgege
nosetvib
         lda fqdatlo,y
         sta lofq,x
         lda fqdathi,y
         sta hifq,x
jgege
         inc nopcounter,x
z3
         lda vibcounter,x
         bmi setsid
         inc vibcounter,x
;--------------------
setsid   ldy d4point,x

         lda lofq,x
         clc
         adc glidebase,x
         sta $d400,y

         lda hifq,x
         adc glidebasehi,x
         sta $d401,y


         lda ad,x
         sta $d405,y
         lda sr,x
         sta $d406,y

         lda wfm,x
         and gate,x
         sta $d404,y

         lda wfm,x
         and #8
         beq zpulse

         sta $d403,y
         lda #0
         sta $d402,y
         jmp z2

zpulse
         lda lopw,x
         sta $d402,y

         lda hipw,x
         sta $d403,y

z2
         lda $d012
         sec
         sbc rtime,x
         sta rtime,x

         dex
         bmi setvol
         jmp nextv
setvol
         lda d417
         sta $d417

         lda ctof
         sta $d416

         lda faderate
         beq storeit

         dec fadecounter
         bpl storeit

         lda volume
         beq storeit

         lda faderate
         sta fadecounter

         dec volume

storeit  lda d418
         ora volume
         sta $d418
         lda volume
         bne outahere
         jmp musoff

outahere
         rts
;------------------------
setmus
         asl
         asl
         asl
         tax

         lda start,x
         sta novoices

         lda start+1,x
         sta temposet

         ldy #0
         sty faderate
loop     lda start+2,x
         sta musstart,y
         inx
         lda #0
         sta gate,y
         sta transp,y
         sta seqno,y
         sta glide,y
         sta notsetyet,y
         iny
         cpy #6
         bne loop

         lda #$0f
         sta $d404
         sta $d404+7
         sta $d404+14

         sty muson
         sta volume

         lda #2
         sta tempocnt
         lda #$f7
         sta gate
         sta gate+1
         sta gate+2

         lda #$ff
         sta len
         sta len+1
         sta len+2

         rts
;------------------
start    !by 2,2;intro tune
         !wo v0,v1,v2

         !by 2,2;ending tune
         !wo v3,v4,v5
;------------------
loseq
         !by <s00,<s01,<s02,<s03
         !by <s04,<s05,<s06,<s07
         !by <s08,<s09,<s0a,<s0b
         !by <s0c,<s0d,<s0e,<s0f
         !by <s10,<s11,<s12,<s13
         !by <s14,<s15,<s16,<s17
         !by <s18,<s19,<s1a,<s1b
         !by <s1c,<s1d,<s1e,<s1f
         !by <s20,<s21,<s22,<s23
         !by <s24,<s25,<s26,<s27
         !by <s28,<s29,<s2a,<s2b
         !by <s2c,<s2d,<s2e,<s2f
         !by <s30,<s31,<s32,<s33
         !by <s34,<s35,<s36,<s37
         !by <s38,<s39,<s3a,<s3b
         !by <s3c
hiseq
         !by >s00,>s01,>s02,>s03
         !by >s04,>s05,>s06,>s07
         !by >s08,>s09,>s0a,>s0b
         !by >s0c,>s0d,>s0e,>s0f
         !by >s10,>s11,>s12,>s13
         !by >s14,>s15,>s16,>s17
         !by >s18,>s19,>s1a,>s1b
         !by >s1c,>s1d,>s1e,>s1f
         !by >s20,>s21,>s22,>s23
         !by >s24,>s25,>s26,>s27
         !by >s28,>s29,>s2a,>s2b
         !by >s2c,>s2d,>s2e,>s2f
         !by >s30,>s31,>s32,>s33
         !by >s34,>s35,>s36,>s37
         !by >s38,>s39,>s3a,>s3b
         !by >s3c
noadrlist
         !by 0,n1-n0,n2-n0,n3-n0
         !by n4-n0,n5-n0,n6-n0
         !by n7-n0,n8-n0,n9-n0
         !by na-n0,nb-n0,nc-n0
         !by nd-n0,ne-n0,nf-n0
fqdatlo
         !by $00,$27,$39,$4c,$5f,$74
         !by $8a,$a2,$bb,$d5,$f1,$0f
         !by $2e,$4f,$72,$98,$bf,$e9
         !by $15,$44,$76,$ab,$e3,$1e
         !by $5c,$9f,$e5,$30,$7f,$d2
         !by $2b,$89,$ed,$56,$c6,$3c
         !by $b9,$3e,$cb,$60,$fe,$a5
         !by $57,$13,$da,$ac,$8c,$79
         !by $73,$7d,$96,$c1,$fd,$4b
         !by $ae,$26,$b4,$59,$18,$f2
         !by $e7,$fa,$2d,$82,$fa,$97
         !by $5c,$4c,$68,$b3,$31,$e4
         !by $cf,$f5,$5b,$04,$f4,$2f
         !by $b9,$98,$d0,$67,$62,$c8
         !by $9e,$eb,$b7,$09,$e8,$5e
         !by $73,$31,$a1,$cf,$c5,$ff
fqdathi
         !by $00,$01,$01,$01,$01,$01
         !by $01,$01,$01,$01,$01,$02
         !by $02,$02,$02,$02,$02,$02
         !by $03,$03,$03,$03,$03,$04
         !by $04,$04,$04,$05,$05,$05
         !by $06,$06,$06,$07,$07,$08
         !by $08,$09,$09,$0a,$0a,$0b
         !by $0c,$0d,$0d,$0e,$0f,$10
         !by $11,$12,$13,$14,$15,$17
         !by $18,$1a,$1b,$1d,$1f,$20
         !by $22,$24,$27,$29,$2b,$2e
         !by $31,$34,$37,$3a,$3e,$41
         !by $45,$49,$4e,$53,$57,$5d
         !by $62,$68,$6e,$75,$7c,$83
         !by $8b,$93,$9c,$a6,$af,$ba
         !by $c5,$d1,$dd,$ea,$f8,$ff
;--------------------
          ;*** title ***
;--------------------
s00      !by $ef,$ef,$ef,$ef,$ff

s01
         !by $c3,$82,$26
         !by $c1,$84,$3c,$48
         !by $c3,$85,$2d
         !by $82,$26,$26
         !by $c1,$84,$3a,$3c
         !by $85,$2d
         !by $c1,$84,$3c,$41,$43,$ff
s02
         !by $c5,$80,$60,$30
         !by $30,$c5,$30,$fe,$39,$c1
         !by $30,$fe,$79
         !by $c5,$61,$30,$60,$30
         !by $62,$30,$30
         !by $c5,$63,$30
         !by $fe,$39,$30
         !by $81,$c1
         !by $fe,$95,$48
         !by $fe,$95,$43
         !by $fe,$35,$48
         !by $fe,$35,$43,$ff
s03
         !by $c1,$83
         !by $18,$c3,$18
         !by $c1,$24,$00,$18,$c3,$18
         !by $e3
         !by $c1,$18,$22,$00,$16
         !by $22,$1b
         !by $c1,$18,$c3,$18
         !by $c1,$24,$00,$18,$c3,$18
         !by $c1,$16,$c3,$18,$c1,$16
         !by $22,$1d,$1f,$13,$ff

s04
         !by $c3,$82,$26
         !by $c1,$84,$3c,$48
         !by $c3,$85,$2d
         !by $82,$26,$85,$2d,$2d
         !by $c1,$2d,$2d,$c3,$2d,$ff
s05
         !by $c1,$81
         !by $fe,$a6,$3f,$3c
         !by $fe,$36,$3f
         !by $fe,$a6,$3f,$3c
         !by $fe,$36,$3f
         !by $fe,$a6,$3f,$3c
         !by $fe,$36,$3f
         !by $fe,$a6,$3f,$3c
         !by $fe,$36,$3f
         !by $fe,$a6,$3e,$fc,$3a
         !by $fe,$36,$3e,$fc,$3a

         !by $fe,$a6,$3f,$38
         !by $fe,$36,$3f
         !by $fe,$a6,$3f,$38
         !by $fe,$36,$3f
         !by $fe,$a6,$3f,$38
         !by $fe,$36,$3f
         !by $fe,$a6,$3f,$38
         !by $fe,$36,$3f
         !by $fe,$a6,$3e,$fc,$35
         !by $fe,$36,$3e,$fc,$35
         !by $ff
s06
         !by $c1,$83
         !by $18,$c3,$18
         !by $c1,$24,$00,$18,$c3,$18
         !by $e3
         !by $c1,$18,$22,$00,$16
         !by $22,$1b

         !by $c1,$83
         !by $18,$c3,$18
         !by $c1,$24,$00,$18,$c3,$18
         !by $86,$c1,$2a,$c3,$27
         !by $c1,$25,$c5,$83
         !by $a4,$01,$26,$c1,$00,$ff
s07
         !by $c1,$83
         !by $18,$c3,$18
         !by $c1,$24,$00,$18,$c3,$18
         !by $e3
         !by $c1,$18,$22,$00,$16
         !by $22,$1b

         !by $c1,$83
         !by $18,$c3,$18
         !by $c1,$24,$00,$18,$c3,$18
         !by $86,$c1,$2a,$c3,$27
         !by $c1,$25,$c1,$27,$27
         !by $c3,$24,$ff
s08
         !by $ef,$ef,$ef,$e7
         !by $81,$c1
         !by $fe,$95,$48,$43
         !by $fe,$35,$48,$43
         !by $fe,$15,$48,$43,$eb

         !by $ef,$ef
         !by $fe,$25,$48,$43
         !by $fe,$45,$48,$43
         !by $fe,$95,$48,$43
         !by $fe,$25,$48,$43,$ff
s09
         !by $c5,$80,$63,$2f
         !by $2f,$c5,$2f,$fe,$39,$c1
         !by $2f,$fe,$79
         !by $c5,$68,$2f,$63,$2f
         !by $68,$2b,$2b
         !by $c5,$69,$2b
         !by $fe,$39,$2b
         !by $81,$c1
         !by $fe,$95,$43,$42
         !by $fe,$35,$43,$42,$ff
s0a
         !by $c1,$8a,$18,$c3,$83,$18
         !by $c1,$24
         !by $89,$2d,$83,$18,$c3
         !by $8a,$18,$00
         !by $c1,$83,$18,$24
         !by $89,$2d
         !by $83,$16,$22,$1b
         !by $c1,$8a,$16,$c3,$83,$18
         !by $c1,$24
         !by $89,$2d,$83,$16,$c3
         !by $8a,$18,$00
         !by $c1,$83,$22,$24
         !by $89,$2d
         !by $83,$22,$1d,$1f,$ff

s0b
         !by $c1,$8a,$18,$c3,$83,$18
         !by $c1,$24
         !by $89,$2d,$83,$18,$c3
         !by $8a,$18,$00
         !by $c1,$83,$18,$24
         !by $89,$2d
         !by $83,$16,$22,$1b
         !by $c1,$8a,$16,$c3,$83,$18
         !by $c1,$24
         !by $89,$2d,$83,$16,$c3
         !by $8a,$18,$00
         !by $c1,$83,$22,$24
         !by $c3,$89,$2d
         !by $c1,$2d,$2d,$ff

s0c
         !by $8b,$c5
         !by $a4,$38,$37,$38
         !by $c9,$8c,$37,$e5
         !by $8b,$c1,$35,$33
         !by $c3,$a4,$35,$33,$33
         !by $32,$c7,$8c,$30,$8b,$c3
         !by $32
         !by $33,$37
         !by $c5,$a4,$38,$37,$38
         !by $c9,$8c,$37,$e5
         !by $c1,$8b,$35,$33
         !by $c3,$a4,$3c,$3a,$3a
         !by $c1,$37,$35,$c7,$8c,$33
         !by $8b,$c3,$35,$c1,$30,$32
         !by $33,$37
         !by $c7,$a4,$3a,$38,$c1,$38
         !by $37,$c9,$8c,$38,$e9
         !by $c5,$8b,$a4,$3a,$38,$c1
         !by $fc,$3c,$38,$37,$c7,$8c
         !by $38,$e3,$8b
         !by $c1,$30,$32,$33,$37
         !by $cb,$a4,$38,$37,$cb
         !by $8c,$38,$8b
         !by $c7,$3a,$a4,$3b,$3a
         !by $37,$c3,$35,$33,$c7,$8c
         !by $32
         !by $ff
s0d
         !by $c1,$8a,$1a,$c3,$83,$1a
         !by $c1,$26
         !by $89,$2d,$83,$1a,$c3
         !by $8a,$1a,$00
         !by $c1,$83,$1a,$26
         !by $89,$2d
         !by $83,$16,$24,$22
         !by $c1,$8a,$13,$c3,$83,$13
         !by $c1,$1f
         !by $89,$2d,$83,$1f,$c3
         !by $8a,$13,$13
         !by $c1,$83,$1d,$1f
         !by $c3,$89,$2d
         !by $c1,$2d,$2d,$ff
s0e
         !by $c5,$80
         !by $63,$30,$30,$c7,$60,$30
         !by $c5,$30,$30
         !by $c5,$63,$30,$30,$c7,$60
         !by $30,$c5,$63,$30,$60,$30

         !by $c5,$69
         !by $33,$33,$c7,$35,$c5,$35
         !by $35
         !by $63,$37,$37,$c7,$69,$35
         !by $c5,$35,$35

         !by $c5,$60,$35,$35,$c7,$35
         !by $c5,$35,$35
         !by $c5,$60,$35,$35,$c7,$35
         !by $c5,$68,$35,$60,$35

         !by $c5,$60,$35,$35,$c7,$35
         !by $c5,$35,$35
         !by $61,$30,$30
         !by $c7,$69,$32,$c5,$32,$32
         !by $ff
s0f
         !by $c5,$8b,$30,$32,$c3,$33
         !by $c5,$8c,$35,$8b,$33
         !by $c3,$35
         !by $c5,$a4,$37,$35,$8c,$3a
         !by $c3,$8b,$a4,$37,$35,$c5
         !by $35,$8c,$33,$c3,$8b,$32
         !by $df,$30,$ff
s10
         !by $c5,$8a,$24,$24,$c3,$24
         !by $c5,$22,$22,$c3,$22
         !by $c5,$20,$20,$c3,$20
         !by $c5,$22,$1f,$c3,$1b
         !by $cf,$14
         !by $c1,$89,$2d,$2d,$c3,$2d
         !by $c1,$2d,$2d,$2d,$2d,$ff
s11
         !by $80,$63,$c5
         !by $37,$37,$c3,$37
         !by $c5,$69,$35,$35
         !by $c3,$35
         !by $c5,$33,$33,$c3,$33
         !by $c5,$63,$32,$32,$c3,$32
         !by $cf,$69,$33,$fc
         !by $a2,$24,$33,$ff

s12
         !by $c5,$8b,$30,$32,$c3,$33
         !by $c5,$8c,$35,$8b,$33
         !by $c3,$35
         !by $c5,$a4,$37,$35,$8c,$3a
         !by $c3,$8b,$a4,$3f,$3e,$c5
         !by $3e,$8c,$3a,$c3,$8b,$37
         !by $df,$a3,$3c,$3a,$ff
s13
         !by $e1,$c1,$6d
         !by $fe,$45,$33,$e1
         !by $fe,$25,$33,$e1
         !by $80,$30,$32,$33
         !by $3a,$e1,$fe,$39,$3a,$e1
         !by $80,$33,$32,$33,$30
         !by $e1,$fe,$39,$30,$e1
         !by $fe,$29,$30,$e1
         !by $80,$30,$32,$33
         !by $3a,$fe,$39,$33
         !by $80,$3a,$fe,$29,$33
         !by $80,$33,$32,$33,$30
         !by $e1,$fe,$39,$fe,$29,$30
         !by $e3,$30,$f7

         !by $d1,$8d,$a4,$00,$3f
         !by $c3,$80,$fe,$79,$63,$37
         !by $c1,$69,$35,$64,$3a,$37
         !by $6d,$fe,$75,$33,$ff
s14
         !by $c1,$83
         !by $18,$c3,$18
         !by $c1,$24,$00,$18,$c3,$18
         !by $e3
         !by $c1,$18,$22,$16,$13,$22
         !by $1f
         !by $14,$c3,$14
         !by $c1,$20,$00,$14,$c3,$20
         !by $c1,$14,$c3,$16,$c1,$22
         !by $16,$13,$16,$1f,$ff

s15
         !by $c5,$8b,$38,$38
         !by $c7,$a4,$3a,$38
         !by $c3,$38,$37,$35
         !by $c5,$a3,$38,$37,$38
         !by $c7,$8c,$3a
         !by $c3,$8b,$38,$37,$35
         !by $c5,$a3,$37,$35,$37
         !by $c3,$38,$c5,$37,$c0
         !by $fc,$38,$fc,$37
         !by $c3,$33,$c5,$8c,$30,$e1
         !by $c1
         !by $80,$48,$fe,$39,$48
         !by $80,$46,$fe,$39,$46
         !by $80,$44,$fe,$39,$44
         !by $80,$43,$41,$3f,$fe,$39
         !by $3f
         !by $80,$3e,$fe,$39,$3e
         !by $80,$3c,$fe,$39,$3c,$ff
s16
         !by $c5,$8b,$38,$38
         !by $c7,$a4,$3a,$38
         !by $c3,$38,$37,$35
         !by $c5,$a3,$38,$37,$38
         !by $c7,$8c,$3a
         !by $c3,$8b,$38,$37,$35
         !by $c5,$a3,$37,$35,$37
         !by $c3,$38,$c5,$37,$c0
         !by $fc,$38,$fc,$37
         !by $c3,$35,$c7,$8c,$37,$eb
         !by $c1
         !by $80,$43,$fe,$39,$43
         !by $80,$41,$fe,$39,$41
         !by $80,$3e,$3b
         !by $80,$37,$fe,$39,$37,$ff
s17
         !by $c3
         !by $82,$26
         !by $80,$63,$30
         !by $85,$2d
         !by $80,$63,$30
         !by $82,$26
         !by $80,$63,$30
         !by $85,$2d
         !by $80,$63,$30

         !by $82,$26
         !by $80,$69,$38
         !by $85,$2d
         !by $80,$69,$38
         !by $82,$26
         !by $80,$69,$38
         !by $85,$2d
         !by $80,$69,$38

         !by $82,$26
         !by $80,$63,$37
         !by $85,$2d
         !by $80,$63,$37
         !by $82,$26
         !by $80,$63,$37
         !by $85,$2d
         !by $80,$63,$37

         !by $82,$26
         !by $80,$68,$37
         !by $85,$2d
         !by $80,$68,$37
         !by $82,$26
         !by $80,$68,$37
         !by $85,$2d
         !by $80,$68,$37

         !by $ff

s18
         !by $c3
         !by $82,$26
         !by $80,$63,$30
         !by $85,$2d
         !by $80,$63,$30
         !by $82,$26
         !by $80,$63,$30
         !by $85,$2d
         !by $80,$63,$30

         !by $82,$26
         !by $80,$69,$38
         !by $85,$2d
         !by $80,$69,$38
         !by $82,$26
         !by $80,$69,$38
         !by $85,$2d
         !by $80,$69,$38

         !by $82,$26
         !by $80,$68,$37
         !by $85,$2d
         !by $80,$68,$37
         !by $82,$26
         !by $80,$68,$37
         !by $85,$2d
         !by $80,$68,$37

         !by $82,$26
         !by $80,$69,$32
         !by $85,$2d
         !by $80,$69,$32
         !by $82,$26
         !by $80,$69,$32
         !by $85,$2d
         !by $80,$69,$32
         !by $ff
s19
         !by $c3,$83,$18,$c1,$18
         !by $c3,$24,$c1,$18,$c3,$18
         !by $18,$c1,$18,$c3,$24
         !by $c1,$13,$16,$13
         !by $ff
s1a
         !by $c3,$83,$18,$c1,$18
         !by $c3,$24,$c1,$18,$c3,$18
         !by $18,$c1,$18,$c3,$24
         !by $c1,$13,$17,$13
         !by $ff
s1b
         !by $c3
         !by $82,$26
         !by $80,$68,$37
         !by $85,$2d
         !by $80,$68,$37
         !by $82,$26
         !by $80,$63,$37
         !by $85,$2d
         !by $80,$63,$37

         !by $82,$26
         !by $80,$61,$3c
         !by $85,$2d
         !by $80,$61,$3c
         !by $82,$26
         !by $80,$60,$3c
         !by $85,$2d
         !by $c1,$80,$60,$3c
         !by $85,$2d

         !by $c3,$82,$26
         !by $80,$69,$33
         !by $85,$2d
         !by $80,$69,$33
         !by $82,$26
         !by $80,$69,$35
         !by $85,$2d
         !by $80,$69,$35

         !by $82,$26
         !by $80,$63,$37
         !by $85,$2d
         !by $80,$63,$37
         !by $85,$26
         !by $80,$69,$35
         !by $85,$c1,$2d,$2d
         !by $80,$69,$35,$85,$2d
         !by $ff
s1c
         !by $8e,$c3,$3c
         !by $8f,$d3,$a4,$3c,$3a
         !by $8e,$c1,$3a,$c0,$fc,$3c
         !by $fc,$3a,$c3,$37
         !by $c3,$3e
         !by $8f,$d3,$a4,$3e,$3c
         !by $8e,$c1,$3a,$c0,$fc,$3c
         !by $fc,$3a,$c3,$37

         !by $c5,$a3,$3f,$3e,$3e
         !by $c3,$3f,$c5,$a5,$41,$3f
         !by $3f,$c3,$a4,$3e,$3c
         !by $8f,$cf,$3c,$e7
         !by $8e,$c1,$3c,$3e,$3f,$43

         !by $46,$fc,$41,$fc,$43
         !by $48,$fc,$41,$fc,$43
         !by $46,$fc,$41,$fc,$43
         !by $48,$fc,$41,$fc,$43
         !by $46,$fc,$41,$fc,$43
         !by $48

         !by $4a,$fc,$41,$fc,$46
         !by $4b,$fc,$41,$fc,$46
         !by $4a,$fc,$41,$fc,$46
         !by $4b,$fc,$41,$fc,$46
         !by $4a,$fc,$41,$fc,$4a
         !by $4b

         !by $4d,$fc,$4a
         !by $fc,$4d,$fc,$4f
         !by $52,$fc,$50
         !by $fc,$4f,$51
         !by $fc,$50,$fc,$4f
         !by $fc,$4d,$fc,$4b
         !by $fc,$4a,$fc,$48
         !by $4d,$fc,$4b
         !by $fc,$4a,$fc,$48
         !by $fc,$44,$47
         !by $fc,$46,$fc,$44
         !by $fc,$43,$fc,$41
         !by $42,$fc,$40
         !by $fc,$3f,$fc,$3e
         !by $fc,$3c,$fc,$3b
         !by $fc,$3a,$fc,$38

         !by $c5,$a4,$37,$35
         !by $c0,$fc,$35,$fc,$33
         !by $d3,$8f,$32,$8e,$c0
         !by $32,$fc,$33,$fc,$37
         !by $fc,$3a

         !by $fc,$3c,$fc,$3a
         !by $fc,$3a,$fc,$35
         !by $fc,$37,$fc,$35
         !by $fc,$33,$fc,$30
         !by $fc,$36,$fc,$35
         !by $fc,$33,$fc,$30
         !by $fc,$2e,$fc,$2b
         !by $fc,$29,$fc,$27

         !by $c5,$26,$c1,$fc,$27,$c3
         !by $fc,$26
         !by $d3,$24

         !by $ff
s1d
         !by $c5,$26,$c1,$fc,$27,$c3
         !by $fc,$26,$cb,$24
         !by $c7,$27
         !by $c3,$a3,$2b,$29

         !by $c1
         !by $fc,$2c,$c9,$8f,$2b,$ff
s1e
s1f
s20
s21
s22
s23
s24
s25
s26
s27
s28
s29
s2a
s2b
s2c
s2d
s2e
s2f
s30
s31
s32
s33
s34
s35
s36
s37
s38
s39
s3a
s3b
s3c
;--------------------------
         ;**** tune #1 ****
v1
         !by $c0,$01,$01,$01,$04
         !by $c0,$01,$01,$01,$04
         !by $c4,$01,$01,$01,$04
         !by $c0,$01,$01,$01,$04
         !by $c4,$01,$01,$01,$04
         !by $cc,$02,$02,$09,$09
         !by $c0,$0c,$0c,$0f
         !by $c0,$01,$01,$01,$04
         !by $c0,$01,$01,$01,$04
         !by $c0,$01,$01,$01,$04
         !by $15,$16,$15,$16
         !by $12
v5
         !by $1c,$1d,$0f,$ff
v0
         !by $c0,$08
         !by $c0,$02,$02
         !by $09,$09
         !by $c0,5,5,$c4,5,5
         !by $c0,$05,$05,$c4,$05,$05
         !by $c0,$0e,$0e,$11
         !by $c0,$13,$13
         !by $c0,$08
         !by $17,$18,$17,$18
         !by $11
v3
         !by $1b,$1b,$1b,$11,$ff
v2
         !by $c0,$03,$06
         !by $c0,$03,$07
         !by $c4,$03,$06
         !by $c0,$03,$07
         !by $c4,$03,$06
         !by $c0,$0a,$0b
         !by $c4,$0a,$0b
         !by $c0,$0a,$bc,$0b
         !by $c5,$0a,$c0,$0d
         !by $c0,$0a,$bc,$0b
         !by $c5,$0a,$c0,$0d
         !by $10
         !by $c0,$03,$14,$03,$14
         !by $c0,$03,$07
         !by $c5,$19,$c1,$1a
         !by $c0,$19,$19
         !by $c5,$19,$c1,$1a
         !by $c2,$19,$bb,$1a
         !by $c5,$19,$c1,$1a
         !by $c0,$19,$19
         !by $c5,$19,$c1,$1a
         !by $c2,$19,$bb,$1a
         !by $c0,$10

v4
         !by $c0,$19,$19
         !by $bc,$1a,$1a
         !by $c0,$19,$19
         !by $bc,$1a,$bb,$19
         !by $c0,$19,$19
         !by $bc,$1a,$1a
         !by $c0,$10
         !by $ff





;------------------------
         ;**** tune #2 ****
v6
v7
v8         ;unused
;--------------------
set1
         ;0+1 : attack/decay
         ;2   : vib delay/speed
         ;3   : pulse block pointer
         ;4   : notes program# selector
         ;4   : bit 7 shutup notes
         ;5   : filter block pointer
         ;6   : waves block pointer
         ;7   : vib steady/rate

;chord
         !by $02,$79,$19,p0-p0
         !by $84,f0-f0,w0-w0,$00;0
;boring 41-sound
         !by $00,$36,$00,p0-p0
         !by $84,f0-f0,w1-w0,$00;1
;bazedrum
         !by $08,$66,$00,p1-p0
         !by $05,f0-f0,w2-w0,$00;2
;baze
         !by $0a,$a3,$00,p2-p0
         !by $04,f1-f0,w3-w0,$00;3
;plonk
         !by $04,$56,$17,p0-p0
         !by $0b,f0-f0,w4-w0,$14;4
;shortsnare
         !by $09,$b8,$00,p1-p0
         !by $06,f0-f0,w5-w0,$00;5
;tom
         !by $06,$27,$00,p1-p0
         !by $07,f2-f0,w6-w0,$00;6
;guitar
         !by $01,$a9,$00,p3-p0
         !by $04,f0-f0,w1-w0,$00;7
;filterbazedrum
         !by $08,$66,$00,p1-p0
         !by $05,f2-f0,w2-w0,$00;8
;filtershortsnare
         !by $09,$b8,$00,p1-p0
         !by $06,f2-f0,w5-w0,$00;9
;filterbazedrum+baze
         !by $0a,$a7,$00,p2-p0
         !by $0c,f1-f0,w7-w0,$00;a
;soft guitar wo vib
         !by $06,$79,$d7,p4-p0
         !by $84,f0-f0,w8-w0,$f5;b
;soft with vib
         !by $06,$79,$37,p4-p0
         !by $84,f0-f0,w8-w0,$23;c
;zzzzooooow f.x.
         !by $bf,$e9,$00,p0-p0
         !by $04,f0-f0,w9-w0,$00;d
;wierd solo guitar
         !by $06,$89,$00,p5-p0
         !by $04,f0-f0,wa-w0,$00;e
;same with vib
         !by $06,$89,$39,p5-p0
         !by $04,f0-f0,wa-w0,$23;f
;
         !by $00,$00,$00,$00
         !by $00,$01,$00,$00;10
;
         !by $00,$4a,$00,$09
         !by $00,$01,$06,$00;11
;
         !by $00,$8a,$19,$00
         !by $11,$71,$0c,$02;12
;
         !by $00,$4a,$39,$05
         !by $00,$61,$06,$01;13
;
         !by $03,$69,$97,$09
         !by $0b,$71,$06,$05;14
;
         !by $09,$89,$00,$00
         !by $03,$01,$09,$00;15
;--------------------
          ;waveform program sequences
          ;byte 0 = repeat point
          ;$ff=repeat/$fe=keep last

w0       ;chord
         !by w0+8-w0
         !by $01,$41,$41,$41
         !by $41,$41,$41,$21,$ff

w1       ;echo
         !by w1+5-w0
         !by $01,$01,$41,$11,$11,$ff


w2       ;bazedrum
         !by w2+$0a-w0
         !by $01,$81,$41,$11,$40
         !by $40,$40,$40,$40,$80,$ff

w3       ;baze
         !by w3+3-w0
         !by $01,$51,$41,$ff
w4
         ;plonk
         !by w4+3-w0
         !by $01,$81,$15,$14,$ff
w5
         ;snaredrum
         !by w5+5-w0
         !by $01,$81,$11,$49,$80
         !by $10,$ff
w6
         !by w6+6-w0
         !by $01,$81,$41,$41,$41
         !by $40,$ff
w7
         ;filterbazedrum+baze
         !by w7+$06-w0
         !by $01,$81,$49,$11,$49
         !by $41,$ff
w8
         !by w8+5-w0
         !by $49,$49,$49,$49,$41,$ff
w9
         !by w9+1-w0
         !by $43,$43,$ff
wa
         !by wa+6-w0
         !by $01,$4b,$43,$41,$41
         !by $41,$ff
wb
wc
;-------------
n0
         !by n0+1-n0
         !by $07,$03,$00,$07,$ff
n1
         !by n1+1-n0
         !by $05,$02,$00,$05,$ff
n2
         !by n2+1-n0
         !by $0a,$07,$00,$0a,$ff
n3
         !by n3+1-n0
         !by $08,$05,$00,$08,$ff
n4
         !by n4+1-n0
         !by $00,$ff
n5
         ;bazedrum
         !by n5+10-n0
         !by $01,$ca
         !by $aa,$b1,$a6,$a2
         !by $9f,$9c,$9e,$df,$da,$ff
n6
         ;snaredrum
         !by n6+5-n0
         !by $01,$c6
         !by $b1,$ad,$c4,$ac,$ff
n7
         ;tom
         !by n7+7-n0
         !by $08,$05,$03,$02,$01
         !by $01,$00,$ff
n8
         !by n8+1-n0
         !by $07,$05,$00,$07,$ff
n9
         !by n9+1-n0
         !by $09,$05,$00,$09,$ff
na
         !by na+1-n0
         !by $00,$ff
nb
         !by nb+3-n0
         !by $ca,$ca,$00,$ff
nc
         ;filterbazedrum-baze
         !by nc+6-n0
         !by $00,$ca
         !by $aa,$b1,$a6,$00,$ff
nd
         !by nd+1-n0
         !by $09,$04,$00,$09,$ff
ne
nf
ede
efe
;-----------------
       ;pulse width program sequences
       ;byte 0 = repeat point
       ;byte 1 = lo/hi pulse width
       ;$ff=repeat/$fe=keep last

p0
         !by p0+2-p0
         !by $08
         !by $18,$01
         !by $10,$01
         !by $18,$02
         !by $20,$03
         !by $18,$02
         !by $10,$01
         !by $08,$ff
         !by $ff

p1       ;skwear polls waive
         !by p1+2-p0
         !by $08,$00,$7f,$ff

p2       ;filterbaze
         !by p2+8-p0
         !by $e2
         !by $83,$01
         !by $87,$01
         !by $8a,$01
         !by $90,$0a
         !by $70,$0a,$ff

p3       ;
         !by p3+$10-p0
         !by $08
         !by $22,$01
         !by $10,$01
         !by $08,$01
         !by $03,$01
         !by $93,$01
         !by $a3,$01
         !by $b3,$03
         !by $c3,$09
         !by $43,$09
         !by $ff
p4
         !by p4+$10-p0
         !by $a0
         !by $30,$02
         !by $18,$02
         !by $0c,$02
         !by $06,$01
         !by $86,$01
         !by $40,$0a
         !by $20,$0a
         !by $10,$7f,$ff
p5
         !by p5+8-p0
         !by $4f
         !by $60,$02
         !by $40,$02
         !by $20,$02
         !by $b0,$01
         !by $20,$01
         !by $ff
p6
p7
p8
p9
;-----------------
       ;filter program sequences
         ;byte 0=repeat point
         ;byte 1=cutoff trigger value
         ;byte 2=filter pass (0=off)

f0
         ;no filter
         !by f0+3-f0
         !by $00,$00,$00,$00
         !by $ff,$ff

f1       ;filterbass
         !by f1+5-f0
         !by $90,$10,$d0,$03
         !by $00,$7f,$ff

f2
         !by f2+3-f0
         !by $ff,$10,$00,$7f
         !by $ff
f3
f4
;----------------

tempocnt !by 0
setlen   !by 0,0,0
transp   !by 0,0,0
nonew    !by 0,0,0
seqno    !by 0,0,0
seqptr   !by 0,0,0
glide    !by 0,0,0
nlen     !by 0,0,0
temp     !by 0
endnote  !by 0,0,0
envptr   !by 0,0,0
vibcounter !by 0,0,0
fipcounter !by 0,0,0
fipreset !by 0,0,0
wfpcounter !by 0,0,0
wfpreset !by 0,0,0
nopcounter !by 0,0,0
nopreset !by 0,0,0
pwpcounter !by 0,0,0
pwpreset !by 0,0,0
vibdir   !by 0,0,0
wfm      !by 0,0,0
gate     !by 0,0,0
len      !by 0,0,0
lofq     !by 0,0,0
hifq     !by 0,0,0
tempnote !by 0,0,0
vibrate  !by 0,0,0
vibratehi !by 0,0,0
note     !by 0,0,0
lopw     !by 0,0,0
hipw     !by 0,0,0
pwtimes  !by 0,0,0
pwvalue  !by 0,0,0
ftms     !by 0,0,0
fdta     !by 0,0,0
ctof     !by 0
d418     !by 0
d417     !by 0
arpselect !by 0,0,0
musstart !by 0,0,0,0,0,0
fadecounter !by 0
volume   !by 0
vibbase  !by 0,0,0
vibbasehi !by 0,0,0
glidebase !by 0,0,0
glidebasehi !by 0,0,0
gltemp2  !by 0,0,0
delaycheck !by 0,0,0
regdelay !by 0,0,0
ad       !by 0,0,0
glitmp   !by 0,0,0
glitmphi !by 0,0,0
glitmp2  !by 0,0,0
glitmp2hi !by 0,0,0
envset   !by 0,0,0
nxnote   !by 0,0,0
notsetyet !by 0,0,0
sr       !by 0,0,0
;------------------------
d4point  !by 0,7,14
font     !by $f1,$f2,$f4
fofft    !by $fe,$fd,$fb

}