
;	This is the original sourcecode of Dutch Breeze


    !to "Ouwehand_Reyn_Dutch_Breeze.sid",plain

	* = 0
	    
	!text	"PSID"			; PSID
	!be16	2				; version
	!be16	$7c				; offset
	!be16	0				; load address auto
	!be16	Init			; init address
	!be16	Play			; play address
	!be16	1				; nsongs
	!be16	1				; starting song
	!be32	0				; song speed (VBL)
-   !text 	"Dutch Breeze"
	!fill 32-(*--),0
.a	!text 	"Reyn Ouwehand"
	!fill 32-(*-.a),0
.c	!text 	"1990 MON"
	!fill 32-(*-.c),0

	!be16	0				; flags
	!be16	0				; reloc
	!be16	0				; v3+4
	
	!wo $1000               ; load address

!pseudopc $1000 {

; ZEROPAGE

zp  = $04
beginzp = zp
zplo = zp
zphi = zp+1
tx  = zp+2
manus = zp+3
arpcount = zp+4
arpcounor = zp+7
drmcounor = zp+10
drmcount = zp+13
endzp = zp+16

			*= $1000

ex_begin
Play        jmp songplay
            jmp songquit
Init        jmp songinit

fadelen     !by 0
datstart    !wo ex_begin

	;		SONGINIT
songinit    ldy #0
            sty test
            sta vermeerder
            asl
            asl
            asl
vermeerder = *+1
            adc #0
            sta seqtabpoint
            sta seqtabpoint+1
            sta seqtabpoint+2
            tax
            lda seqtab+6,x
            sta speedorig
            lda #0
            sta speedcop
            lda seqtab+7,x
            sta speed2
            lda seqtab+8,x
            sta tottrans
            lda #$10
            sta band
            lda #%11110001
            sta d417
            lda #$0f
            sta volume
            tya
            ldx #endzp-beginzp
filzp       sta beginzp-1,x
            dex
            bne filzp
            ldx #endva-beginva
filzp2      sta beginva-1,x
            dex
            bne filzp2
            sta fadelen
            ldx #$14
filsid      sta $d400,x
            dex
            bpl filsid
            stx test
            rts

d4point     !by 0,7,14
speedorig   !by 0
seqtabpoint !by 0,0,0
speedcop    !by 0

        ; VARIABLES
		
beginva
gatebit     !by 0,0,0
instcopy    !by 0,0,0
counter     !by 0,0,0
testcoun    !by 0,0,0
fx0sto      !by 0
fx1sto      !by 0
fx2sto      !by 0
fx3sto      !by 0
thisarp     !by 0,0,0
arprep      !by 0,0,0
d400        !by 0,0,0
d401        !by 0,0,0
d402        !by 0,0,0
d403        !by 0,0,0
d405        !by 0,0,0
d406        !by 0,0,0
vibhand1    !by 0,0,0
vibhand2    !by 0,0,0
vibhand3    !by 0,0,0
filter      !by 0
fitel       !by 0
fitabcou    !by 0
putabcou    !by 0,0,0
pucount     !by 0,0,0
pucounup    !by 0,0,0
pulstest    !by 0,0,0
pause       !by 0,0,0
hinotsto    !by 0,0,0
hinotst2    !by 0,0,0
wabetest    !by 0,0,0
tempnote    !by 0,0,0
tempglid    !by 0,0,0
temp        !by 0,0,0
vibcount    !by 0,0,0
conbyte     !by 0,0,0
dubdec      !by 0
d404        !by 0,0,0
fadecoun    !by 0,0,0
pulstes2    !by 0,0,0
seqcount    !by 0,0,0
tonetrp     !by 0,0,0
voicetrp    !by 0,0,0
repstep     !by 0,0,0
stepcoun    !by 0,0,0
instr       !by 0,0,0
lengorig    !by 0,0,0
lengcopy    !by 0,0,0
heenter     !by 0,0,0
pitch       !by 0,0,0
endva

lonote
lonote2 = *+1
    !by $1c,$ff,$00,$52,$66,$7b
    !by $92,$aa,$c3,$de,$fa,$18
    !by $38,$5a,$7e,$a3,$cc,$f6
    !by $23,$53,$86,$bb,$f4,$30
    !by $70,$b4,$fb,$47,$97,$ec
    !by $47,$a6,$0b,$77,$e8,$60
    !by $e0,$67,$f6,$8e,$2f,$d9
    !by $8d,$4c,$17,$ed,$d0,$c1
    !by $c0,$ce,$ec,$1c,$5d,$b2
    !by $1a,$98,$2d,$da,$a0,$82
    !by $80,$9c,$d9,$38,$ba,$63
    !by $34,$31,$5a,$b4,$41,$04
    !by $00,$39,$b2,$6f,$74,$c6
    !by $69,$61,$b5,$68,$82,$08
    !by $00,$72,$64,$de,$e9,$8c
    !by $d1,$c2,$69,$d1,$04
hinote
hinote2 = *+1
    !by 1,$fc,$c4,1,1,1,1,1,1,1,1,2
    !by 2,2,2,2,2,2,3,3,3,3,3,4
    !by 4,4,4,5,5,5,6,6,7,7,7,8
    !by 8,9,9,10,11,11
    !by 12,13,14,14,15,16
    !by 17,18,19,21,22,23
    !by $19,$1a,$1c,$1d,$1f,$21
    !by $23,$25,$27,$2a,$2c,$2f
    !by $32,$35,$38,$3b,$3f,$43
    !by $47,$4b,$4f,$54,$59,$5e
    !by $64,$6a,$70,$77,$7e,$86
    !by $8e,$96,$9f,$a8,$b2,$bd
    !by $c8,$d4,$e1,$ee,$fd

z   = ex_begin

seqence
            !wo s00-z, s01-z, s02-z, s03-z
            !wo s04-z, s05-z, s06-z, s07-z
            !wo s08-z, s09-z, s0a-z, s0b-z
            !wo s0c-z, s0d-z, s0e-z, s0f-z
            !wo s10-z, s11-z, s12-z, s13-z
            !wo s14-z, s15-z, s16-z, s17-z
            !wo s18-z, s19-z, s1a-z, s1b-z
            !wo s1c-z, s1d-z, s1e-z, s1f-z
            !wo s20-z, s21-z, s22-z, s23-z
            !wo s24-z, s25-z, s26-z, s27-z
            !wo s28-z, s29-z, s2a-z, s2b-z
            !wo s2c-z, s2d-z, s2e-z, s2f-z
            !wo s30-z, s31-z, s32-z, s33-z
            !wo s34-z, s35-z, s36-z, s37-z
            !wo s38-z, s39-z, s3a-z, s3b-z
            !wo s3c-z, s3d-z, s3e-z, s3f-z

songquit    lda #0
            sta $d404
            sta $d404+7
            sta $d404+14
            sta test
            rts

;	SONGPLAY

songplay
test = * + 1
            lda #0
            bmi hahasong
            rts

hahasong    lda fadelen
            beq nofade
            dec fadecoun
            bpl nofade
            sta fadecoun
            lda volume
            beq songquit
            dec volume
nofade
voice = * + 1
            ldx #2
            dec dubdec
            bpl dubdo
speed2 = * + 1
            lda #6
            sta dubdec
            bne playonce
dubdo       dec speedcop
            bpl playonce
            lda speedorig
            sta speedcop
playonce    stx tx
            lda speedorig
            cmp speedcop
            beq startpoint
            jmp onlyfx

startpoint  dec lengcopy,x
            bmi startpoint2
            jmp onlyfx2

startpoint2 txa
            asl
            adc seqtabpoint,x
            tax
            lda seqtab,x
            clc
            adc datstart
            sta seqlo
            lda seqtab+1,x
            adc datstart+1
            sta seqhi

            ldx tx

	        ;	SEQUENCE HANDLE

takeseq2    ldy seqcount,x
seqlo = *+1
seqhi = *+2
takeseq     lda $ffff,y                 ; get sequence value
            cmp #$fe
            bne songquit2
            jmp songquit                ; $fe = end of song

songquit2   cmp #$ff
            bne checkseq
            lda seqtabpoint,x
            cmp #8
            bcs dubbelinc
            clc
            adc #9                      ; to next step
            sta seqtabpoint,x
dubbelinc   lda #0
            sta lengcopy,x
            sta stepcoun,x
            sta seqcount,x
            jmp startpoint2

checkseq    cmp #$5f
            bcc takestep
            cmp #$6f
            bcs voictr
            sbc #$5f
            sta voicetrp,x
            inc seqcount,x
            bne takeseq2
voictr      cmp #$80
            bcc tonetrans
            sbc #$80
            clc
tottrans = *+1
            adc #0
            sta tonetrp,x
            inc seqcount,x
            bne takeseq2
tonetrans   sec                     ;remove!!!
            sbc #$70
            sta repstep,x
            inc seqcount,x
            bne takeseq2
takestep    asl
            tay
            lda seqence,y
            clc
            adc datstart
            sta zplo
            lda seqence+1,y
            adc datstart+1
            sta zphi
	
	        ;	STEP-HANDLE
	
takestep2   lda #0
            sta tempglid,x
            sta pause,x
            lda #1
            sta conbyte,x
            ldy stepcoun,x
            lda (zplo),y
            sta manus
            cmp #$60
            bcs tochdoen
            jmp thenote
tochdoen    cmp #$ff
            bne nextbyte

            lda #$00
            sta stepcoun,x
            lda repstep,x
            beq nextseq
            dec repstep,x
            bpl nextrep
nextseq     inc seqcount,x
            jmp takeseq2
nextrep     jmp takestep2
nextbyte    cmp #$fe
            bne filterset
            iny
            lda (zplo),y
            sta d417
            iny
            lda (zplo),y
            sta manus
filterset   cmp #$fd
            bne glideset
fise        iny
            lda (zplo),y
            sta tempglid,x
            iny
            lda (zplo),y
            sta manus
            iny
            lda (zplo),y
            clc
            adc tonetrp,x
            sta tempnote,x
            jmp thenote

glideset    lda #0
            sta tempglid,x
            lda manus
            cmp #$fc
            bne autofade
            iny
            lda (zplo),y
            sta fadelen
            iny
            lda (zplo),y
            sta manus
autofade    cmp #$fb
            bne connect
            lda #0
            sta conbyte,x
            iny
            lda (zplo),y
            sta manus

connect     lda manus
            cmp #$e0
            bcc pausecom
            sbc #$e1
            sta lengcopy,x
            lda #0
            sta d404,x
            sta d405,x
            sta d406,x
            lda #1
            sta pause,x
            iny
            tya
            sta stepcoun,x
            jmp nextvoice

pausecom    lda manus
            cmp #$c0
            bcc instrument
            sbc #$c0
            clc
            adc voicetrp,x
            sta instr,x
            iny
            lda (zplo),y
            cmp #$fd
            bne fise4
            jmp fise
fise4
            sta manus
instrument
            cmp #$80
            bcc tonelenght
            sbc #$81
            sta lengorig,x
fise3       iny
            lda (zplo),y
            cmp #$fd
            bne fise2
            jmp fise

fise2
            cmp #$80
            bcc fise3a
            sbc #$80
            clc
            adc lengorig,x
            sta lengorig,x
            bne fise3
fise3a      sta manus
tonelenght
            cmp #$60
            bcc thenote
            sbc #$60
            sta thisarp,x
            iny
            lda (zplo),y
            sta manus
thenote
            lda lengorig,x
            sta lengcopy,x
            lda #0
            sta counter,x
            sta vibhand1,x
            sta vibhand2,x
            sta vibhand3,x
            iny
            tya
            sta stepcoun,x

            lda manus
            clc
            adc tonetrp,x
            sta pitch,x
            tay
            lda lonote,y
            sta d400,x
            lda hinote,y
            sta d401,x
            sta hinotsto,x
            sta hinotst2,x

            inc counter,x
            lda #$ff
            sta gatebit,x
            lda instr,x
            asl
            asl
            asl
            sta instcopy,x
            tay
            lda attdec,y
            sta d405,x
            lda susrel,y
            sta d406,x
            lda conbyte,x
            beq pulher
            lda pulstes2,x
            bmi pulher
            lda pulsehi,y
            and #$0f
            sta d403,x
            lda pulsehi,y
            and #$f0
            sta d402,x
pulher      lda fx0,y
            and #8
            bne nows
            lda conbyte,x
            beq nows
            lda fx3,y
            and #$f0
            beq nows
            lda #1
            sta d404,x
            jmp nextvoice

nows        lda waveform,y
            sta d404,x
            jmp nextvoice

onlyfx2     lda lengcopy,x
            beq gateout
            cmp lengorig,x
            beq setgate2
            ldy instcopy,x
            lda fx0,y
            lsr
            lsr
            lsr
            lsr
            cmp #$0f
            beq gateout
            cmp lengcopy,x
            bcs gateout
setgate2    lda #$ff
            bne gwb
gateout
            lda testcoun,x
            beq onlyfx
            lda conbyte,x
            beq onlyfx
            lda #$fe
gwb         sta gatebit,x
onlyfx
            lda pause,x
            beq nopause
            jmp nextvoice
nopause
            inc counter,x
            lda counter,x
            cmp #3
            bcs last100
firstwo     lda #0
last100     sta testcoun,x

            ldy instcopy,x
            lda fx0,y
            sta fx0sto
            lda fx1,y
            sta fx1sto
            lda fx2,y
            sta fx2sto
            lda fx3,y
            sta fx3sto
            and #$08
            beq afkappen
            lda lengcopy,x
            bne afkappen
            lda conbyte,x
            beq afkappen
            lda speedcop
            cmp #1
            bne afkappen
            sta d406,x
afkappen

        	;	GLIDE

            lda tempglid,x
            bne doglide
            jmp noglide2
doglide
            ldy tempnote,x
            lda lonote,y
            sta zplo
            lda hinote,y
            sta zphi

            lda #0
            sta manus

            lda #$07
            ldy tempglid,x
            dey
looping     dey
            bmi nomoreofthat

            asl
            rol manus
            jmp looping

nomoreofthat
            sta temp

            lda pitch,x
            cmp tempnote,x
            bcc glideup

            lda d400,x
            sbc temp
            sta d400,x
            lda d401,x
            sbc manus
            sta d401,x

            lda d400,x
            sec
            sbc zplo
            lda d401,x
            sbc zphi
            bcs noglide

glidend     lda #0
            sta tempglid,x

            lda tempnote,x
            sta pitch,x
            lda zplo
            sta d400,x
            lda zphi
            sta d401,x
            sta hinotsto,x
            jmp noglide2

glideup     lda d400,x
            adc temp
            sta d400,x
            lda d401,x
            adc manus
            sta d401,x

            lda zplo
            sec
            sbc d400,x
            lda zphi
            sbc d401,x
            bcs noglide
            jmp glidend
noglide     jmp vibrafoetsie

noglide2

	        ;	ARP
			
            lda fx0sto
            and #$08
            beq onlyarp
            lda fx3sto
            lsr
            lsr
            lsr
            lsr
            tay
            dey
            lda starttabel,y
            clc
            adc #totalarps
            tay
            bne drumarp
onlyarp     lda fx3sto
            and #$04
            beq arpies
            ldy thisarp,x
drumarp     lda ar,y
            tay
            lda testcoun,x
            bne arpbegin
            sty arpcount,x
            sty arpcounor,x
            sta arprep,x
arpbegin
            lda ar0,y
            sta manus
            dec arprep,x
            bpl arpies
            lsr
            lsr
            lsr
            lsr
            and #$07
            sta arprep,x
arbe2       inc arpcount,x
            ldy arpcount,x
            lda ar0,y
            cmp #$ff
            bne setarppi
            ldy arpcounor,x
            lda ar0,y
            and #$0f
            clc
            adc arpcounor,x
            sta arpcount,x
            jmp arbe2
setarppi
            cmp #$fe
            bne sear2
            dec arpcount,x
            jmp arpies

sear2       ldy tempglid,x
            bne arpies

            bit manus
            bpl sar3
            sta d401,x
            sta d400,x
            jmp arpo
sar3        cmp #$00
            bmi sar2
            clc
            adc pitch,x
sar2        and #$7f
            tay
            lda lonote,y
            sta d400,x
            lda hinote,y
            sta d401,x
arpo        sta hinotsto,x
arpies

            ;	VIBRATO

            lda fx1sto
            bne vibraatje
vifo        jmp vibrafoetsie

vibraatje
            lda testcoun,x
            bne overvib
            lda #0
            sta vibhand2,x
            sta vibhand1,x
            sta vibcount,x
            lda fx1sto
            and #$07
            lsr
            adc #0
            sta vibhand3,x
            jmp vibrafoetsie
overvib
            lda tempglid,x
            bne vifo

            ldy pitch,x
            lda lonote,y
            sec
            sbc lonote-1,y
            sta zplo
            lda hinote,y
            sbc hinote-1,y
            sta zphi

            lda fx1sto
            and #$70
            lsr
            lsr
            lsr
            lsr
            tay
divide      dey
            bmi div2
            lsr zphi
            ror zplo
            jmp divide

div2        lda fx1sto
            bpl div3
            lda counter,x
            cmp #vibuntil
            bcs div5
            inc vibcount,x
div5        ldy #1
div4        lda zplo
            adc vibcount,x
            sta zplo
            lda zphi
            adc #0
            sta zphi
            dey
            bpl div4
div3        lda fx2sto
            lsr
            lsr
            lsr
            lsr
            sta checkvib
            lda lengorig,x
            sec
            sbc lengcopy,x
checkvib = *+1
            cmp #0
            bcc vibrafoetsie

            dec vibhand3,x
            bpl vibup
            inc vibhand3,x
            dec vibhand1,x
            bpl vibtel
            lda fx1sto
            and #$07
            sta vibhand1,x
            lda vibhand2,x
            eor #1
            sta vibhand2,x
vibtel      lda vibhand2,x
            bne vibdown
vibup
            lda d400,x
            clc
            adc zplo
            sta d400,x
            lda hinotsto,x
            adc zphi
            sta d401,x
            jmp vibrafo2

vibdown
            lda d400,x
            sec
            sbc zplo
            sta d400,x
            lda hinotsto,x
            sbc zphi
            sta d401,x
vibrafo2
            sta hinotsto,x
vibrafoetsie

	        ;		FILTER
	
            ldx tx
            lda fx0sto
            and #$07
            bne fm2a
            jmp fm2
fm2a
            stx filwhat
            tay
            dey
            lda filbyt,y
            tay
            lda testcoun,x
            bne nogwel2
            lda fi1,y
            and #$f0
            sta band
            lda #1
            sta fitel
            sta filtest
            iny
            lda fi1,y
            iny
            sty fitabcou
            jmp fme
nogwel2
filtest = *+1
            lda #0
            bmi frutsmaarraak

            dec fitel
            bne erover2

nopu4
            ldy fitabcou
            lda fi1,y
            cmp #$ff
            bne nopu2
            sta filtest
            jmp frutsmaarraak
nopu2
            sta fitel
            iny
            lda fi1,y
            sta filcountup
            iny
            sty fitabcou
erover2
            lda filter
            clc
filcountup = *+1
            adc #0
            jmp fme
fm2
filwhat = *+1
            lda #0
            cmp tx
            bne frutsmaarraak

            lda #$10
            sta band
            lda #$ff
fme         sta filter
            sta $d416
frutsmaarraak

	        ;	PULSE

            ldx tx
            lda fx2sto
            and #$0f                    ; fx2 bit 7 = pulse
            bne dopulsecount
            jmp pulsestore              ; not set, skip

dopulsecount
            tay
            dey
            lda pultablo,y
            sta zplo
            lda pultabhi,y
            sta zphi                    ; zplo = pulse table

            ldy #0
            lda (zplo),y
            and #$0f
            sta pulsehitop              ; pwh = a & f
            lda (zplo),y
            lsr
            lsr
            lsr
            lsr
            sta pulselotop              ; pwl = a >> 8

            lda testcoun,x              ; frame < 2
            bne pulseinit               ; no, continue
            iny
            lda (zplo),y
            sta pulstes2,x
            bmi pir2

pulseini2   lda (zplo),y
            and #$7f
            sta pulstest,x

pir2        lda #2
            sta putabcou,x
            lda #1
            sta pucount,x
            jmp pulsestore

pulseinit   lda pulstest,x
            bmi erover

            dec pucount,x
            bne erover

nopu3a      ldy putabcou,x
            lda (zplo),y
            cmp #$ff
            bne nopu
            lda pulstest,x
            eor #$a0
            sta pulstest,x
            jmp erover

nopu        cmp #$fe
            bne nopu2a
            lda #puls_2nd
            sta d403,x
            sta d402,x
            iny
            lda (zplo),y
nopu2a      sta pucount,x
            iny
            lda (zplo),y
            sta pucounup,x
            iny
            tya
            sta putabcou,x

erover      lda pulstest,x              ; get direction
            lsr
            bcc neertellen

            lda d402,x                  ; pulse up
            clc
            adc pucounup,x
            sta d402,x
            lda d403,x
            adc #0
            sta d403,x
pulsehitop = * + 1
            cmp #0
            bcc pulsestore
            dec pulstest,x
            jmp pulsestore

neertellen  lda d402,x                  ; pulse down
            sec
            sbc pucounup,x
            sta d402,x
            lda d403,x
            sbc #0
            sta d403,x
pulselotop = *+1
            cmp #0
            bcs pulsestore
            inc pulstest,x
pulsestore

        	;	SPACE
			
            lda fx3sto
            and #2                      ; bit 2 = space/gap
            beq nospacey

            lda lengorig,x
            cmp #spacecheck
            bcc spacecon

            lda lengcopy,x
            cmp #spacewait
            bcs spacecon

            lda counter,x
            and #1
            beq spacecon

            lda hinotst2,x
            beq nospacey
            dec hinotst2,x
            bne spacec2
spacecon
            lda hinotsto,x
spacec2
            sta d401,x
nospacey

            ; WAVEBEGIN

            lda fx0sto              
            and #8                      ; bit 3 = drum wave
            bne nowavebegin             ; is drum, skip wave

            lda conbyte,x
            bne xxv
            jmp drumie
xxv
            lda fx3sto
            and #$f0
            beq nowavebegin

            lsr
            lsr
            lsr
            lsr
            tay
            dey
            lda startlen,y
            cmp counter,x
            bcc nv3

            lda #1
            sta wabetest,x
            lda starttabel,y
            bpl nohiset
            lda #noisehitone
            sta d401,x
            lda #$81
nohiset     jmp nve

nv3         lda wabetest,x
            beq nowavebegin
            dec wabetest,x
            lda hinotsto,x
            sta d401,x
            ldy instcopy,x
            lda waveform,y
nve         sta d404,x

nowavebegin

	        ;	DRUMWAVE

            lda fx0sto
            and #8                  ; is drum wave?
            beq drumie              ; no, skip

            lda fx3sto
            lsr
            lsr
            lsr
            lsr
            tay
            dey
            ldx startlen,y
            ldy drmtab,x
            ldx tx
            lda testcoun,x
            bne drmbegin
            sty drmcount,x
            sty drmcounor,x

drmbegin    inc drmcount,x
            ldy drmcount,x
            lda dw0,y
            cmp #$ff
            bne setdrumi
            ldy drmcounor,x
            lda dw0,y
            and #$0f
            clc
            adc drmcounor,x
            sta drmcount,x
            jmp drmbegin

setdrumi    cmp #$fe
            bne +
            dec drmcount,x
            jmp drumie
+           sta d404,x
drumie

nextvoice   ldx tx
            ldy d4point,x           ; y = sid offset
            lda d406,x
            sta $d406,y
            lda d405,x
            sta $d405,y
            lda d404,x
            and gatebit,x
            sta $d404,y
            lda d403,x
            sta $d403,y
            lda d402,x
            sta $d402,y
            lda fx3sto
            and #1                  ; check detune flag
            beq +                   ; no detune
            lda d400,x
            clc
            adc #detune
            jmp ++
+           lda d400,x
            clc
++          sta $d400,y
            lda d401,x
            adc #0
            sta $d401,y
            dex
            bmi setsid
            jmp playonce
setsid

volume = * + 1
            lda #0
band = * + 1
            ora #0
            sta $d418
d417 = * + 1
            lda #0
            sta $d417
            rts

			; Song data
			
vibuntil = $2c
detune = $50
noisehitone = $fc
spacecheck = $00
spacewait = $30
totalarps = 25
puls_2nd = $0d

c0  = $00
cf0 = $01
d0  = $02
df0 = $03
e0  = $04
f0  = $05
ff0 = $06
g0  = $07
gf0 = $08
a0  = $09
af0 = $0a
b0  = $0b
c1  = $0c
cf1 = $0d
d1  = $0e
df1 = $0f
e1  = $10
f1  = $11
ff1 = $12
g1  = $13
gf1 = $14
a1  = $15
af1 = $16
b1  = $17
c2  = $18
cf2 = $19
d2  = $1a
df2 = $1b
e2  = $1c
f2  = $1d
ff2 = $1e
g2  = $1f
gf2 = $20
a2  = $21
af2 = $22
b2  = $23
c3  = $24
cf3 = $25
d3  = $26
df3 = $27
e3  = $28
f3  = $29
ff3 = $2a
g3  = $2b
gf3 = $2c
a3  = $2d
af3 = $2e
b3  = $2f
c4  = $30
cf4 = $31
d4  = $32
df4 = $33
e4  = $34
f4  = $35
ff4 = $36
g4  = $37
gf4 = $38
a4  = $39
af4 = $3a
b4  = $3b
c5  = $3c
cf5 = $3d
d5  = $3e
df5 = $3f
e5  = $40
f5  = $41
ff5 = $42
g5  = $43
gf5 = $44
a5  = $45
af5 = $46
b5  = $47
c6  = $48
cf6 = $49
d6  = $4a
df6 = $4b
e6  = $4c
f6  = $4d
ff6 = $4e
g6  = $4f
gf6 = $50
a6  = $51
af6 = $52
b6  = $53

	!text "MUSIC BY REYN OUWEHAND '90 "

seqtab
    !wo seq0a-z,seq0b-z,seq0c-z
    !by $02,$00,$fe
    !wo seq0at-z,seq0bt-z,seq0ct-z
    !by 0,0,0

pultablo
    !by <p1,<p2,<p3,<p4,<p5,<p6,<p7
    !by <p8
pultabhi
    !by >p1,>p2,>p3,>p4,>p5,>p6,>p7
    !by >p8
	
filbyt
	!by 0,fi2-fi1,fi3-fi1,fi4-fi1
    !by fi5-fi1,fi6-fi1,fi7-fi1
	
ar  !by 0,ar1-ar0,ar2-ar0
    !by ar3-ar0,ar4-ar0,ar5-ar0
    !by ar6-ar0,ar7-ar0,ar8-ar0
    !by ar9-ar0,ara-ar0,arb-ar0
    !by arc-ar0,ard-ar0,are-ar0
    !by arf-ar0,ar10-ar0,ar11-ar0
    !by ar12-ar0,ar13-ar0,ar14-ar0
    !by ar15-ar0,ar16-ar0,ar17-ar0
    !by ar18-ar0
    !by dt0-ar0,dt1-ar0,dt2-ar0
    !by dt3-ar0,dt4-ar0,dt5-ar0
    !by dt6-ar0,dt7-ar0,dt8-ar0
    !by dt9-ar0,dta-ar0

drmtab
	!by 0,dw1-dw0,dw2-dw0,dw3-dw0
    !by dw4-dw0,dw5-dw0,dw6-dw0
    !by dw7-dw0,dw8-dw0,dw9-dw0
    !by dwa-dw0

p1
    !by $8b,$01
    !by $06,$30
    !by $01,$50
    !by $ff
p2
    !by $ef,$00
    !by $01,$08
    !by $ff
p3
    !by $ab,$00
    !by $01,$30
    !by $ff
p4
    !by $8b,$01
    !by $03,$90
    !by $02,$60
    !by $01,$20
    !by $ff
p5
    !by $ad,$00
    !by $06,$70
    !by $01,$55
    !by $ff
p6
    !by $9c,$01
    !by $05,$f0
    !by $01,$60
    !by $ff
p7
p8

fi1
    !by $10,$40
    !by $1a,$ff
    !by $ff
fi2
    !by $10,$20
    !by $07,$10
    !by $07,$f0
    !by $ff
fi3
    !by $20,$90
    !by $18,$02
    !by $5a,$fe
    !by $ff
fi4
    !by $10,$58
    !by $01,$50
    !by $03,$d0
    !by $08,$ff
    !by $20,$02
    !by $ff
fi5
    !by $20,$0a
    !by $90,$01
    !by $ff
fi6
    !by $30,$b0
    !by $05,$e0
    !by $ff
fi7
    !by $10,$30
    !by $02,$30
    !by $20,$fc
    !by $ff

ar0 !by $10,$00,$04,$09,$ff
ar1 !by $10,$00,$05,$09,$ff
ar2 !by $10,$00,$03,$08,$ff
ar3 !by $10,$00,$03,$07,$ff
ar4 !by $10,$00,$03,$08,$ff
ar5 !by $10,$00,$05,$08,$ff
ar6
ar7
ar8
ar9
ara
arb
arc
ard
are
arf
ar10
ar11
ar12
ar13
ar14
ar15
ar16
ar17
ar18

dt0
    !by $83
    !by $33,$0f,$0c,$e0,$08,$ff
dt1
    !by $85
    !by $34,$14,$0e,$36,$0a
    !by $34,$36,$ff
dt2
    !by $85
    !by $30,10,8,6,3,6,7,$ff
dt3
    !by $81
    !by $30,$38,$34,$30,$2c
    !by $2a,$2c,$30,$34,$38,$3a
    !by $ff
dt4
    !by $00
    !by $81,7,5,4,3,2,1,0,$fe
dt5
    !by $84
    !by $fc,$1c,$fc,$18,$fc,$fd,$fe
dt6
    !by $83
    !by $34,$14,$0e
    !by $34,$36,$ff
dt7
    !by $80
    !by $e0,10,8,6,5,4,3,2,1,$fe

dt8
    !by $8f
    !by $30,$0e,$0c,$0a,$2c
    !by $2a,$28,$26,$24,$22,$20,$1e
    !by $1c,$1b,$1a,$18,$16,$14,$12
    !by $ff
dt9
dta

dw0
    !by $83
    !by $81,$41,$41,$80,$40,$ff
dw1
    !by $80
    !by $81,$11,$41,$80,$40,$80,$fe
dw2
    !by $80
    !by $81,$41,$41,$41,$41,$80,$fe
dw3
    !by $80
    !by $81,$43,$42,$fe
dw4
    !by $00
    !by $81,$11,$10,$fe
dw5
    !by $80
    !by $81,$11,$80,$10,$80,$fe
dw6
    !by $80
    !by $81,$11,$41,$80,$fe
dw7
    !by $80
    !by $81,$11,$11,$11,$11,$11,$11
    !by $10,$fe
dw8
    !by $80
    !by $81,$41,$41,$41,$80,$fe
dw9
dwa

pulsehi = * + 0
waveform = * + 1
attdec = * + 2
susrel = * + 3
fx0 = * + 4
fx1 = * + 5
fx2 = * + 6
fx3 = * + 7

;---------------------------------
;kick
    !by $00,$89,$0e,$f8
    !by $08,$00,$00,$c0      ;00
;snare
    !by $08,$89,$02,$89
    !by $08,$00,$00,$20      ;01
;41-
    !by $0a,$41,$02,$44
    !by $00,$00,$00,$30      ;02
;41-(echo)
    !by $0a,$41,$01,$24
    !by $00,$00,$00,$30      ;03
;41-arp light
    !by $0d,$41,$00,$3a
    !by $20,$00,$02,$04      ;04
;bass
    !by $0d,$41,$00,$54
    !by $00,$00,$01,$30      ;05
;big kick
    !by $08,$89,$03,$89
    !by $09,$00,$00,$50      ;06
;longbass
    !by $0d,$41,$00,$59
    !by $00,$00,$01,$30      ;07
;leeg
    !by $00,$80,$00,$00
    !by $00,$00,$00,$00      ;08
;tom
    !by $08,$89,$0d,$e6
    !by $08,$00,$00,$70      ;09
;81-tok
    !by $08,$81,$07,$b4
    !by $08,$00,$00,$90      ;0a
;81-space
    !by $00,$81,$00,$8e
    !by $03,$00,$00,$00      ;0b
;arp 41 (short)
    !by $0c,$41,$04,$55
    !by $00,$00,$03,$04      ;0c
;arp 41 (long)
    !by $0c,$41,$04,$5c
    !by $f0,$00,$03,$04      ;0d
;snare vol2
    !by $08,$89,$08,$c8
    !by $08,$00,$00,$a0      ;0e
;snare vol3
    !by $08,$89,$00,$67
    !by $08,$00,$00,$a0      ;0f
;41-arp light
    !by $ff,$41,$c0,$3a
    !by $70,$00,$02,$04      ;10
;41-melo
    !by $8c,$41,$01,$54
    !by $00,$23,$54,$b0      ;11
;41-melo (fade)
    !by $8c,$41,$01,$5b
    !by $e0,$23,$54,$b0      ;12
;bass (filter)
    !by $09,$41,$05,$d6
    !by $04,$23,$25,$40      ;13
;15-bongo
    !by $00,$15,$00,$15
    !by $f0,$00,$00,$40      ;14
;snare big
    !by $08,$89,$07,$c9
    !by $08,$00,$00,$10      ;15
;81-space
    !by $00,$81,$00,$8a
    !by $65,$00,$00,$00      ;16
;41-melo (2)
    !by $0d,$41,$03,$58
    !by $00,$23,$21,$30      ;17
;51-arp
    !by $00,$51,$00,$83
    !by $06,$00,$00,$04      ;18
;51-norm
    !by $00,$51,$00,$53
    !by $00,$33,$00,$00      ;19
;bass (filter) 2
    !by $0a,$41,$04,$c8
    !by $04,$23,$26,$30      ;1a

startlen
    !by $00,$01,$02,$02       ;1-4
    !by $02,$03,$04,$03
    !by $05,$06,$08,$07
    !by $08
starttabel
    !by $00,$01,$81,$19       ;1-4
    !by $02,$03,$04,$11
    !by $05,$06,$41,$07
    !by $08

seq0a
    !by $82
    !by 6
    !by 4
    !by 3,3
seq0at
    !by $82
    !by $03,$03
    !by $09,$09
    !by $03,$03
    !by $73,$0b
    !by $03,$03
    !by $00
    !by $03,$03
    !by $09,$09
    !by $83,$03,$03
    !by $09,$09
    !by $03,$03
    !by $00
    !by $ff
;---------------------------------------
seq0b
    !by $82
    !by 7
    !by $7f,$01
    !by $77,$01
seq0bt
    !by $82
    !by 8
    !by $0a
    !by $08
    !by $0c,$0c
    !by $08
    !by $00
    !by $08
    !by $0a
    !by $83
    !by $7f,$01
    !by $0a
    !by $08
    !by $00
    !by $ff
;---------------------------------------
seq0c
    !by $82,$77,$02
seq0ct
    !by $82
    !by $77,$05,$73,$05
    !by $87,$77,$05
    !by $82,$73,$05
    !by $00
    !by $77,$05
    !by $83
    !by $73,$02
    !by $73,$05
    !by $73,$05
    !by $00
    !by $ff

s01
    !by $c0,$82,0
    !by $c5,c2,c3,c2
    !by $c0,0
    !by $c5,c3,c2,c3
    !by $ff

s02
    !by $c2,$82,af4,c5,$c3,af4
    !by $c2,c5
    !by $c3,g4
    !by $c2,g4,af4,c5
    !by f4
    !by $c3,c5
    !by $c2,df4
    !by $c3,f4
    !by $c2,af4
    !by $c3,df4
    !by $c2,g4
    !by $c3,af4
    !by $c2,$82,af4,c5,$c3,af4
    !by $c2,c5
    !by $c3,g4
    !by $c2,g4,af4,c5
    !by f4
    !by $c3,c5
    !by $c2,df5
    !by $c3,f4
    !by $c2,d5
    !by $c3,df5
    !by $c2,af4
    !by $c3,d5
    !by $ff
	
s03
    !by $fe,$f1
    !by $da
    !by $86,c2,$82,c2
    !by $ce,$84,0
    !by $da,af1
    !by c2,$82,af1,c2
    !by $ce,$84,0
    !by $da,$82,df1,e1
    !by $86,f1,$82,f1
    !by $ce,$84,0
    !by $da,df1
    !by f1,$82,df2,f2
    !by $ce,$82,0
    !by $da,$82,df2,c2,af1
    !by $86,c2,$82,c2
    !by $ce,$84,0
    !by $da,af1
    !by c2,$82,af1,c2
    !by $ce,$84,0
    !by $da,$82,f1,g1
    !by $86,f1,$82,f1
    !by $ce,$84,0
    !by $da,f1
    !by $81,f2,df2,c2,df2,$82,f2,f1
    !by $ce,$82,0
    !by $da,$82
    !by g2,af2
    !by df3
    !by $ff
	
s04
    !by $fe,$f1
    !by $cb,$a0,$a0,c6
    !by $d6,$a0,$90,c6
    !by $ce,$82,0,$84,0,0
    !by $82,0,0,0
    !by $ff
	
s05
    !by $c0,$82,0
    !by $c2,c5,$c3,af4
    !by $c2,c5
    !by $c0,0
    !by $c2,g4,af4,c5
    !by $c0,0
    !by $c3,c5
    !by $c2,df4
    !by $c3,f4
    !by $c0,0
    !by $c3,df4
    !by $c2,g4
    !by $c3,af4
    !by $c0,$82,0,$c2,c5,$c3,af4
    !by $c2,c5
    !by $c0,0
    !by $c2,g4,af4,c5
    !by $c0,0
    !by $c3,c5
    !by $c2,df5
    !by $c3,f4
    !by $c0,0
    !by $c3,df5
    !by $c2,af4
    !by $c3,d5
    !by $ff
	
s06
    !by $fe,$f1
    !by $c6,$9c,c5,$84,c5
    !by $c6,$90,c5,$84,c5
    !by $d5,$8c,0
    !by $c6,$9c,c5,$84,c5
    !by $c6,$90,c5,$84,c5
    !by $d5,$88,0
    !by $ce,$82,0,0
    !by $ff
s07
    !by $c7,$a0,c2,f1,c2,f1
    !by $ff
s08
    !by $cd,$9c,$60,df4
    !by $a0,$61,df4
    !by $cc,$84,$62,d4
    !by $82,d4
    !by $84,$60,df4,df4
    !by $e2
    !by $d5,$8a,0
    !by $ce,$82,0
    !by $e4
    !by $cc
    !by $84,$63,c4,$82,c4,$84,$62,c4
    !by $84,c4
    !by $ca,$82,0,0,$e2,0,0,$e2,0,0,0
    !by $ce,0,0
    !by $cd,$9c,$60,df4
    !by $a0,$61,df4
    !by $cc,$84,$62,d4
    !by $82,d4
    !by $84,$60,df4,df4
    !by $e2
    !by $d5,$8a,0
    !by $ce,$82,0
    !by $e4
    !by $cc
    !by $84,$63,c5,$82,c5,$84,$62,c5
    !by $84,c5
    !by $cf,$82,0,0,$e2,$ca,0,0,$e2,0
    !by 0
    !by $ce,0,0,0
    !by $ff
s09
    !by $da
    !by $86,gf1,$82,gf1
    !by $ce,$84,0
    !by $da,g1
    !by gf1,$82,g1,gf1
    !by $ce,$84,0
    !by $da,$82,df1,e1
    !by $86,f1,$82,f1
    !by $ce,$84,0
    !by $da,df1
    !by f1,$82,df2,f2
    !by $ce,$82,0
    !by $da,$82,df2,c2,af1
    !by $86,c2,$82,c2
    !by $ce,$84,0
    !by $da,af1
    !by c2,$82,af1,c2
    !by $ce,$84,0
    !by $da,$82,f1,g1
    !by $86,c2,$82,c2
    !by $ce,$84,0
    !by $da,c2
    !by $84,af1,$82,c2,g1
    !by $ce,$82,0
    !by $da,$82
    !by f2,ff2
    !by g2
    !by $ff
s0a
    !by $d1,$90,g4
    !by $84,c5,$e4,$84,g4
    !by $94,f4
    !by $84,g4,$e4,$84,gf4
    !by $d2,$9c,$90,g4
    !by $e4
    !by $d1,$82,c2,c3,c4,c2,c3,c4,c2
    !by c3
    !by $ce,$82,0,0
    !by $d1,$90,g4
    !by $84,c5,$e4,$84,g4
    !by $94,f4
    !by $84,f5,$e4,$84,df5
    !by $d2,$9c,$88,c5
    !by $c9,$82,c4
    !by $84,a3,g3
    !by $82,f3
    !by $84,f3
    !by $82,g3
    !by $84,f3,df3
    !by $82,c3
    !by $84,c3
    !by $ff
s0b
    !by $da
    !by $86,cf2,$82,cf2
    !by $ce,$84,0
    !by $da,c2
    !by cf2,$82,c2,cf2
    !by $ce,$84,0
    !by $da,$82,c2,b1
    !by $86,af1,$82,af1
    !by $ce,$84,0
    !by $da,gf1
    !by af1,$82,gf1,gf2
    !by $ce,$82,0
    !by $da,$82,gf1,f1,df1
    !by $86,f1,$82,f1
    !by $ce,$84,0
    !by $da,df1
    !by f1,$82,f1,f2
    !by $ce,$84,0
    !by $da,$82,f1,df1
    !by $86,f1,$82,f2
    !by $ce,$84,0
    !by $da,df1
    !by $84,f1,$82,f1,f2
    !by $ce,$82,0
    !by $da,$82
    !by f1,g1
    !by af1
    !by $ff
s0c
    !by $d7,$90,f4,$84,gf4,$e4
    !by af4,$88,gf4
    !by $8c,f4
    !by $84,df4,c4,gf4
    !by $94,f4
    !by $e4
    !by $cc,$82,$65
    !by c5,$e2,c5,$e2,c5,$e2
    !by $86,$63,c5
    !by $82,$65,c5,$e4
    !by $82,$63,c5,$e2
    !by $65,c5,$e2
    !by $82,$63,c5
    !by $84,$65,c5
    !by $e6
    !by $d7,$90,f4,$84,gf4,$e4
    !by af4,$88,gf4
    !by $8c,f4
    !by $84,df5,c5,df5
    !by $94,f5
    !by $e4
    !by $cc,$82,$65
    !by c5,$e2,c5,$e2,c5,$e2
    !by $86,$63,c5
    !by $82,$65,c5,$e4
    !by $82,$63,c5,$e2
    !by $65,c5,$e2
    !by $82,$63,c5
    !by $84,$65,c5
    !by $e6
    !by $ff
s0d
s0e
s0f
s10
s11
s12
s13
s14
s15
s16
s17
s18
s19
s1a
s1b
s1c
s1d
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
s3d
s3e
s3f
s00
    !by $f0
    !by $ff





}