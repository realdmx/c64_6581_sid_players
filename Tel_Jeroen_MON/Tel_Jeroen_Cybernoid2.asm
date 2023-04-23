
; Cybernoid II
; Music by Jeroen Tel

; MUSICFILE V01-07-1988
; Programmed by Charles Deenen
; The most advanced music player ever written for the commodore 64!

	!to "Tel_Jeroen_Cybernoid2.sid",plain

RELOC	= $1000

SID_UNK  = %00000000
SID_PAL  = %00000100
SID_NTSC = %00001000
SID_BOTH = %00001100

SID_UNK  = %00000000
SID_6581 = %00010000
SID_8580 = %00100000
SID_ANY  = %00110000

	* = $0000

	!text "PSID"
	!be16 2							; version 2
	!be16 $7c						; data offset
	!be16 RELOC     				; Load (0 = auto)
	!be16 init						; Init
	!be16 play						; Play
	!be16 2							; num songs
	!be16 1							; first song
	!word 0
	!word 0
-	!text "Cybernoid II"
	!fill 32 - (* - -)
-	!text "Jeroen Tel"
	!fill 32 - (* - -)
-	!text "1988 Hewson"
	!fill 32 - (* - -)
	!be16 SID_PAL | SID_6581		; v2 flags
	!word 0							; Start page, page length (reloc)
	!word 0							; Reserved
	
!pseudopc $1000 {

	* = $1000


volume          = $0f
vibtotzover     = $30
pulserunspeed   = $63
dubvoice        = $0c
noisehitone     = $fa
pulsearpwait    = $01
gitarwait       = $0f
spacelength     = $00
spacewait       = $60
wavearpwait     = 2

; zero page

fx1sto          = $40
fx2sto          = $41
fx3sto          = $42
tabbytsto       = $43
zer0fillo       = $44
zer0filhi       = $45
zp3             = $46
zp4             = $47
wax             = $48
voicesto        = $49
vibrasto        = $4a
zodat3          = $4b
vibreallo       = $4c
vibrealhi       = $4d
temphino        = $4e
templono        = $4f
glideslo        = $50
glideshi        = $51
denom           = $52

init            jmp song
                jmp songout
play            jmp playirq

speedbyte       !by 0
d4point         !by 0,7,14

tabcount        !by $00,$00,$00
begcount        !by $00,$00,$00
nootcount       !by $00,$00,$00
nootleng        !by $00,$00,$00
wavesto         !by $00,$00,$00
noothoogt       !by $00,$00,$00
noho            !by $00,$00,$00
wavecount       !by $00,$00,$00
hinotesto       !by $00,$00,$00
hinotesto2      !by $00,$00,$00
lonotesto       !by $00,$00,$00
glidetest       !by $00,$00,$00
glidetest2      !by $00,$00,$00
pulsestolo      !by $00,$00,$00
pulsehisto      !by $00,$00,$00
pulsehitemp     !by $00,$00,$00
pulsecountup    !by $00
counter2        !by $00,$00,$00
toneadd         !by $00,$00,$00
vibstore1       !by $00,$00,$00
vibstore2       !by $00,$00,$00
vibstore3       !by $00,$00,$00
tonearpcounter  !by $00,$00,$00
arpieoklo       !by $00,$00,$00
arpieokhi       !by $00,$00,$00
st2             !by $00
st              !by $00
filter          !by $00,$00,$00
filtercount     !by $00,$00,$00
pulsetest       !by $00,$00,$00
wivofilter      !by $00
speedsto        !by $00
filwhat         !by $00
repeatsto       !by $00,$00,$00
stod404         !by $00,$00,$00
newnote         !by $00,$00,$00
strfiltest      !by $00
tempglide       !by $00,$00,$00
glidedelay      !by $00,$00,$00
strafil         !by $00
d400            !by $00,$00,$00
d401            !by $00,$00,$00
voiceinc        !by $00,$00,$00
byteand         !by $00,$00,$00
pulseruntest    !by $00,$00,$00
pulserunlo      !by $00,$00,$00
pulserunhi      !by $00,$00,$00
vibcounter      !by $00,$00,$00
d402            !by $00,$00,$00
d403            !by $00,$08,$08
tabelend

ok2     lda #0

        ldx #(tabelend - tabcount) - 1
-       sta tabcount - 1,x
        dex
        bne -
        dex
        stx counter2
        stx counter2 + 1
        stx counter2 + 2

        ldx #2
-       sta tabcount,x
        sta begcount,x
        sta nootcount,x
        sta noho,x
        dex
        bpl -

        sta testbyte
        rts

	    ; song

song    tax                     ; x = song number
        lda #1
        sta testbyte
        sta pulseruntest
        sta pulseruntest + 1
        sta pulseruntest + 2
        lda snelheid,x
        sta speedbyte
        txa
        sta multipli
        asl
multipli = * + 1
        adc #0
        asl
        adc #5
        tax

        ldy #5
-       lda seqtabel,x
        sta seqloclo,y
        dex
        dey
        bpl -

        sty $d416
        iny
        sty $d417
        lda #$10 + volume
        sta $d418
        jsr ok2

uitzet  lda #0
        ldx #$15
-       sta $d400,x
        dex
        bpl -
        rts

songout lda #1
        sta testbyte
        bne uitzet

playirq
testbyte = * + 1
        lda #0
        beq +
        rts

+       lda speedbyte
        ldx #2
        dec speedsto
        bpl startplayer

        sta speedsto

        ; ------set tracklo+hi-z------

startplayer
        stx wax
        inc counter2,x
        ldy d4point,x
        sty voicesto
        lda speedsto
        cmp speedbyte
        beq hy

        jmp h11

hy      lda seqloclo,x
        sta yoa1
        lda seqlochi,x
        sta yoa2
        dec nootcount,x
        bmi h2

        jmp h10

        ; -------take step from track--

h2      ldy tabcount,x
yoa1 = * + 1
yoa2 = * + 2
        lda $ffff,y
        cmp #$fe
        beq songout
        cmp #$ff
        bne h3

        lda #0
        sta nootcount,x
        sta tabcount,x
        sta begcount,x
        jmp h2

        ; -------add tone--------

h3      sta st2
        cmp #$40
        bcc h3f
        cmp #$80
        bcc h3a

        and #%00011111                  ; $1f
        sta toneadd,x
        inc tabcount,x
        jmp h2

        ; --------set steplo+hi--------

h3a     lda st2
        cmp #$60
        bcc h3c

        and #%00001111                  ; $0f
        sta voiceinc,x
        inc tabcount,x
        jmp h2

h3c     and #%00111111                  ; $3f
        sta repeatsto,x
        inc tabcount,x
        jmp h2

        lda st2
h3f     asl
        tay
        lda sequence,y
        sta zp3
        lda sequence + 1,y
        sta zp4

        ; --------load note---------

        lda #0
        sta glidetest,x
        sta glidetest2,x
        sta counter2,x
        sta vibcounter,x

        ldy begcount,x
        lda (zp3),y
        sta tabbytsto
        cmp #$f0
        bcc startnewnote

        lda tabbytsto
        and #1
        bne dofilset

        lda #1
        sta newnote,x
        inc begcount,x
        iny
        lda (zp3),y
        sta tabbytsto
        bne skip

dofilset
        inc begcount,x
        iny
        lda (zp3),y
        sta $d417                        ; resfilt
        jsr verhoogtest

startnewnote
        lda #0
        sta newnote,x

        ; --------glideset---------

skip    lda tabbytsto
        cmp #$e0
        bcc noglideset

        lda #1
        sta glidetest,x
        inc begcount,x
        iny
        lda (zp3),y
        sta glidedelay,x
        inc begcount,x
        inc begcount,x
        iny
        iny
        lda (zp3),y
        clc
        adc toneadd,x
        sta tempglide,x
        dey
        lda (zp3),y
        sta tabbytsto
        bne nolengset

noglideset
        lda tabbytsto
        cmp #$c0
        bcc novoiceset

        and #$1f
        clc
        adc voiceinc,x
        sta wavecount,x
        jsr verhoogtest

novoiceset
        lda tabbytsto
        cmp #$80
        bcc arpset

        and #$3f
        sec
        sbc #1
        sta nootleng,x
        jsr verhoogtest

        cmp #$e0
        beq skip
        cmp #$80
        bcc arpset

        and #$7f
        clc
        adc nootleng,x
        sta nootleng,x
        jsr verhoogtest

        cmp #$80
        bcc arpset

        jmp skip

arpset  cmp #$70
        bcc nolengset

        and #$0f
        sty denom
        tay
        lda arplo,y
        ldx wax
        sta arpieoklo,x
        lda arphi,y
        sta arpieokhi,x
        ldy denom
        jsr verhoogtest

nolengset
        lda nootleng,x
        sta nootcount,x
        lda #0
        sta tonearpcounter,x
        lda tabbytsto
        clc
        adc toneadd,x
        sta noho,x
        tay

        ; ------------- set adsr values ----------

        lda lonote,y
        sta d400,x
        pha
        sta lonotesto,x
        lda hinote,y
        sta d401,x
        sta hinotesto,x
        sta hinotesto2,x
        ldy voicesto
        sta $d401,y
        pla
        sta $d400,y

        lda newnote,x
        bne snnn

        lda wavecount,x
        asl
        asl
        asl
        tax
        lda attdec,x
        sta $d405,y
        lda susrel,x
        sta $d406,y
        lda filcount,x
        pha
        lda pulsehi,x
        pha
        lda waveform,x
        ldx wax
        sta wavesto,x
        sta stod404,x
        lda #0
        sta d402,x
        sta pulsestolo,x
        pla
        sta pulsehitemp,x
        and #$0f
        sta d403,x
        sta pulsehisto,x
        lda #1
        sta pulsetest,x
        pla
        sta filtercount,x

snnn    inc begcount,x
        ldy begcount,x
        lda (zp3),y
        cmp #$ff
        bne h10b

nextjmp lda #0
        sta begcount,x
        lda repeatsto,x
        beq nj1

        dec repeatsto,x
        bpl h10b

nj1     inc tabcount,x
h10b    lda #$ff
        sta byteand,x
        jmp nextvoice

h10     lda nootcount,x
        beq gwaitout

        lda wavecount,x
        asl
        asl
        asl
        tay
        lda filcount,y
        and #$f0
        lsr
        lsr
        cmp nootcount,x
        bcs gwaitout

        lda #$ff
        bne gwb

verhoogtest
        inc begcount,x
        iny
        lda (zp3),y
        cmp #$ff
        beq nextjmp

        sta tabbytsto
        rts

gwaitout
        lda #$fe
gwb     sta byteand,x
h11     lda pulsehitemp,x
        and #$10
        beq gwo2

        lda nootcount,x
        bne gwo2

        lda speedsto
        cmp #1
        bne gwo2

        lda #$02
        sta $d406,y                 ; sr

gwo2    lda wavecount,x
        asl
        asl
        asl
        tay
        lda fx1,y
        sta fx1sto
        lda fx2,y
        sta fx2sto
        lda noho,x
        sta noothoogt,x
        lda fx3,y
        sta fx3sto
        and #$10
        beq javib

        jmp b17

        ; Tonearpeggio

javib   lda fx3sto
        and #$04
        beq javib2

        ldx wax
        lda arpieoklo,x
        sta arpieoklo1
        sta arpieoklo2
        lda arpieokhi,x
        sta arpieokhi1
        sta arpieokhi2
        dec tonearpcounter,x
        bpl hallo

arpieoklo1 = * + 1
arpieokhi1 = * + 2
        lda arp0
        sta tonearpcounter,x
hallo   ldx wax
        ldy tonearpcounter,x
        iny
arpieoklo2 = * + 1
arpieokhi2 = * + 2
        lda arp0,y
        clc
        adc noho,x
        sta noothoogt,x
        tay
        lda lonote,y
        sta d400,x
        lda hinote,y
        sta d401,x

        ; Vibrato routine

javib2  lda fx1sto
        beq b17jmp2

        lda glidetest2,x
        bne b17jmp2

        ldy wavecount,x
        ldx vibtabwait,y
        stx vibwait
        lda fx1sto
        and #$0f
        sta vibrasto
        lda fx1sto
        and #%01110000                  ; $70
        lsr
        lsr
        lsr
        lsr
        ldx wax
        sta vibstore1,x
        ldy #$bc                    ; adc abs
        lda fx1sto
        bpl doityes

        ldy #$7d                    ; ldy abs
doityes sty doitnot
        lda vibcounter,x
        cmp #vibtotzover
        bcs frag

        inc vibcounter,x
frag    ldy noothoogt,x
        lda lonote2,y
        sec
        sbc lonote,y
        sta templono
        lda hinote2,y
        sbc hinote,y
doitnot ldy vibcounter,x
        sta temphino
reducesize
        dec vibrasto
        bmi redout

        lsr temphino
        ror templono
        jmp reducesize

b17jmp2 jmp b17

redout  lda vibstore2,x
        bpl w1

        dec vibstore3,x
        bne nextsect

        inc vibstore2,x
        bpl nextsect

w1      inc vibstore3,x
        lda vibstore1,x
        cmp vibstore3,x
        bcs nextsect

        sta vibstore3,x
        dec vibstore2,x
        dec vibstore3,x
nextsect
        ldy noothoogt,x
        lda lonote,y
        sta vibreallo
        lda hinote,y
        sta vibrealhi
        lda vibstore1,x
        lsr
        tay
subval  dey
        bmi endsv

        lda counter2,x
vibwait = * + 1
        cmp #0
        bcc endav

        lda vibreallo
        sec
        sbc templono
        sta vibreallo
        lda vibrealhi
        sbc temphino
        sta vibrealhi
        jmp subval

endsv   ldy vibstore3,x
addval  dey
        bmi endav

        clc
        lda vibreallo
        adc templono
        sta vibreallo
        lda vibrealhi
        adc temphino
        sta vibrealhi
        jmp addval

endav   ldx wax
        lda vibreallo
        sta d400,x
        sta lonotesto,x
        lda vibrealhi
        sta d401,x
        sta hinotesto,x

        ; Tone glide

b17     ldx wax
        lda glidetest,x
        bne rara

glideoutjmp
        jmp glideout

glideoutjmp2
        jmp glideout2

rara    lda glidedelay,x
        and #$f0
        lsr
        lsr
        lsr
        lsr
        sta glen
        sta bran
        lda glidedelay,x
        and #$0f
        sec
        sbc #1
        clc
        adc nootcount,x
        cmp nootleng,x
        bcs glideoutjmp

        pha
        lda #1
        sta glidetest2,x
        pla
glen = * + 1
        adc #0
        cmp nootleng,x
        bcc glideoutjmp2

        ldy noho,x
        lda tempglide,x
        tax
        sec
        lda lonote,y
        sbc lonote,x
        sta glideslo
        lda hinote,y
        sbc hinote,x
        sta glideshi
        ldx #$38
        ldy #$e9
        bcs eg

        ldx #$18            ; clc
        ldy #$69            ; adc
        eor #$ff
        sta glideshi
        lda glideslo
        eor #$ff
        sta glideslo
        inc glideslo
        bne eg

        inc glideshi
eg      sty updown1
        sty updown2
        stx glisscarry

        ldy speedbyte
        lda #0
        clc
bran = * + 1
-       adc #0
        dey
        bpl -

        sta denom
        clc
        asl glideslo
        rol glideshi
        ldx #$0f
        lda #0
nekstbit
        rol glideslo
        rol glideshi
        rol
        bcs notoff

        cmp denom
        bcc leave

notoff  sbc denom
        sec
leave   dex
        bne nekstbit

        rol glideslo
        rol glideshi
        asl
        cmp denom
        bcc noway
        inc glideslo
        bne noway
        inc glideshi
noway   lda glideslo
        sta udlo
        lda glideshi
        sta udhi
        ldx wax
        lda lonotesto,x
glisscarry
        clc
udlo = * + 1
updown1 adc #0
        sta lonotesto,x
        sta d400,x
        lda hinotesto,x
udhi = * + 1
updown2 adc #0
        sta hinotesto,x
        sta d401,x
        jmp glideout

pst     jmp pulsestore

glideout2
        lda tempglide,x
        sta noho,x
        tay
        lda lonote,y
        sta lonotesto,x
        sta d400,x
        lda hinote,y
        sta hinotesto,x
        sta d401,x
        lda #0
        sta glidetest,x
        sta glidetest2,x
glideout

        ; pulsegedoe

        lda fx2sto
        and #$07
        beq pst

        and #%00000111              ; $07
        asl
        asl
        asl
        sbc #$07
        tay
        lda pulsetabel,y
        pha
        and #$80
        beq noprep

        lda #1
noprep  sta purepbyte
        pla
        and #$0f
        sta pulsecountlo
        iny
        lda pulsetabel,y
        sta pulsecounthi
        iny
        lda pulsetabel,y
        and #$7f
        cmp counter2,x
        bcc go6

        jmp go5

go6     iny
        iny
        lda pulsetabel,y
        and #$7f
        cmp counter2,x
        bcc go2

        jmp go5

go2     iny
        iny
        lda pulsetabel,y
        and #$7f
        cmp counter2,x
        bcc go3

go5     lda pulsetabel,y
        and #$80
        beq goo1

        lda #0
        sta pulsetest,x
goo1    iny
        lda pulsetabel,y
        sta pulsecountup
        jmp go4

go3     lda fx2sto
        and #$f0
        sta pulsecountup

go4     lda pulsetest,x
        bne pusw1

        lda pulsestolo,x
        sec
        sbc pulsecountup
        sta pulsestolo,x
        lda pulsehisto,x
        sbc #0
        sta pulsehisto,x
pulsecountlo = * + 1
        cmp #1
        bcs pulsestore

        lda #1
        bne pulseshit

pusw1   lda pulsestolo,x
        clc
        adc pulsecountup
        sta pulsestolo,x
        lda pulsehisto,x
        adc #0
        sta pulsehisto,x
pulsecounthi = * + 1
        cmp #$0e
        bcc pulsestore

purepbyte = * + 1
        lda #0
        beq ppt

        sta pulsestolo,x
        lda pulsecountlo
        sta pulsehisto,x
        lda #1
        bne pulseshit

ppt     lda #0
pulseshit
        sta pulsetest,x
pulsestore
        ldx wax
        lda pulsestolo,x
        sta d402,x
        lda pulsehisto,x
        sta d403,x

        ; Wavearpeggio

wavetry lda fx3sto
        and #$40
        beq pulsearpplay

        ldx wax
        lda counter2,x
        cmp #wavearpwait
        bcc pulsearpplay
        and #3
        tax
        lda wavearp,x
        cmp #$81
        bne rdw
        ldx wax
        lda #$e0
        sta d400,x
        sta d401,x
        lda #$81
rdw     ldx wax
        sta stod404,x

        ; Pulsearpeggio

pulsearpplay
        lda fx3sto
        and #$08
        beq sweep

        ldx wax
        lda counter2,x
        cmp #pulsearpwait
        bcc sweep

        lda counter2,x
        and #7
        tax
        lda pulsearp,x
        ldx wax
        sta d403,x

        ; Tonesweep up
sweep   lda fx3sto
        and #$20
        beq filterklooi

        ldx wax
        ; lda nootcount,x
        ; cmp #4
        ; bcs filterklooi
        lda hinotesto,x
        sec
        sbc #1
        sta hinotesto,x
        sta d401,x

    ; Filterklooi

filterklooi
        lda fx3sto
        and #1
        beq fm2

        ldx wax
        stx filwhat
        lda filtercount,x
        and #%00000111      ; $07
        asl
        tax
        lda filterbytes,x
        sta trulo
        lda filterbytes + 1,x
        sta truhi
trulo = * + 1
        lda #<fb0
        sta zer0fillo
truhi = * + 1
        lda #>fb0
        sta zer0filhi

        ldy #5
        lda (zer0fillo),y
        sta $d418

        ldx wax
        lda counter2,x
        ldy #9
        cmp (zer0fillo),y
        bcc filfur3

        ldy #4
        lda (zer0fillo),y
        jmp fme

filfur3 dey
        cmp (zer0fillo),y
        bcs filfur1

        cpy #6
        bne filfur3

        ldy #0
        lda (zer0fillo),y
        jmp fme

        jmp frutsmaarraak

filfur1 dey
        dey
        dey
        dey
        dey
        lda filter,x
        clc
        adc (zer0fillo),y
        jmp fme

fm2     ldx wax
        cpx filwhat
        bne frutsmaarraak

        lda fx2sto
        and #$08
        bne frutsmaarraak

        lda #$10 + volume
        sta $d418
        lda #$80
fme     sta filter,x
        sta $d416

frutsmaarraak

        ; Strangefilter

        lda fx2sto
        and #8
        beq pulserun

        lda counter2 + 2
        and #1
        beq stfilout

        lda strfiltest
        beq fillup
filldown
        lda strafilter
        sec
        sbc filtermega + 3
        sta strafilter
        cmp filtermega + 1
        bcs stfilout
        lda #0
        sta strfiltest
        beq stfilout
fillup
        lda strafilter
        clc
        adc filtermega + 3
        sta strafilter
        cmp filtermega + 2
        bcc stfilout
        lda #1
        sta strfiltest
stfilout
        ldx wax
        lda counter2 + 2
        cmp #2
        bcs stfo2

        lda #$40
        sta strafil

stfo2   lda strafilter
        clc
        adc strafil
        sta $d416
        lda strafil
        cmp #2
        bcc pulserun

        lda strafil
        sec
        sbc aftrekspeed
        sta strafil

        ; Pulserun

pulserun
        ldx wax
        lda fx3sto
        and #$02
        beq pulserunout

        lda pulseruntest,x
        beq pulsebegin

        lda pulsehisto,x
        sta pulserunhi,x
        lda #0
        sta pulseruntest,x
        sta pulserunlo,x

pulsebegin
        ldy voicesto
        lda pulserunlo,x
        clc
        adc #pulserunspeed
        sta pulserunlo,x
        sta d402,x
        bcc pulserunsto

        inc pulserunhi,x
        lda pulserunhi,x
        cmp #$0f
        bne pulserunsto
        eor #$08
        sta pulserunhi,x
pulserunsto
        lda pulserunhi,x
        sta d403,x
        jmp jeroenshit

pulserunout
        lda #1
        sta pulseruntest,x

        ; Double voices

jeroenshit
        ldx wax
        lda filtercount,x
        and #$08
        beq space

        lda d400,x
        clc
        adc #dubvoice
        sta d400,x
        lda d401,x
        adc #0
        sta d401,x

        ; Space effect

space   ldx wax
        lda filtercount,x
        and #0
        beq b19a

        lda nootleng,x
        and #$7f
        cmp #spacelength
        bcc b19a

        lda nootcount,x
        cmp #spacewait
        bcs b19a

        lda counter2,x
        and #1
        beq b19a

        lda hinotesto2,x
        beq b19a
        dec hinotesto2,x
        sta d401,x

b19a

        ; Drumroutine

        lda fx3sto
        and #$10
        beq noti

        lda fx1sto
        and #$0f
        asl
        asl
        tax
        lda drumtabel,x
        sta dwalo
        sta drummylen + 1
        lda drumtabel + 1,x
        sta dwahi
        sta drummylen + 2
        lda drumtabel + 2,x
        sta dtalo
        lda drumtabel + 3,x
        sta dtahi
drummylen
        lda $ffff
        sta dl
        ldx wax
        lda counter2,x
dl = * + 1
        cmp #$0f
        bcs drfu

        tay
dwalo = * + 1
dwahi = * + 2
        lda $ffff,y
        sta stod404,x
        and #1
        beq nd1

        lda #$ff
        bmi nd2

nd1     lda #$fe
nd2     sta byteand,x
        dey
dtalo = * + 1
dtahi = * + 2
        lda $ffff,y
        sta st
        ldy voicesto
        lda fx1sto
        and #$10
        beq drfu2

        ldx wax
        lda noothoogt,x
        clc
        adc st
        jmp cdac

drfu2   ldx wax
        lda st
        sta d401,x
        lda #0
        sta d400,x
drfu    jmp nextvoice

        ; noisetik

noti    lda fx3sto
        and #$80
        beq nextvoice

        ldy wavecount,x
        lda counter2,x
        cmp startlen,y
        bcs nv3

        lda starttabel,y
        cmp #$7f
        bcc nohiset

        lda #noisehitone
        sta d401,x
        lda #$81
nohiset jmp nve

nv3     lda startlen,y
        clc
        adc #2
        sta stle2
        lda counter2,x
stle2 = * + 1
        cmp #$00
        bcs nextvoice

        lda lonotesto,x
        sta d400,x
        lda hinotesto,x
        sta d401,x
        lda wavesto,x
nve     sta stod404,x

nextvoice
        ldx wax
        ldy voicesto

        lda stod404,x
        and byteand,x
        sta $d404,y
        lda d400,x
        sta $d400,y
        lda d401,x
        sta $d401,y
        lda d402,x
        sta $d402,y
        lda d403,x
        sta $d403,y
        dex
        bmi playout

        jmp startplayer

playout rts

cdac    tay
        ldx wax
        lda lonote,y
        sta d400,x
        lda hinote,y
        sta d401,x
        jmp nextvoice

lonote
lonote2 = * + 1
        !by $c3,$dd,$fa,$18,$38,$5a,$7d,$a3,$cc,$f6,$23,$53
        !by $86,$bb,$e0,$30,$70,$b4,$fb,$47,$98,$ed,$47,$a7
        !by $0c,$77,$e9,$61,$e1,$68,$f7,$8f,$30,$da,$8f,$4e
        !by $18,$ef,$d2,$c3,$c3,$d1,$ef,$1f,$60,$b5,$1e,$9c
        !by $31,$df,$a5,$87,$86,$a2,$df,$3e,$c1,$6b,$3c,$39
        !by $63,$be,$4b,$0f,$0c,$45,$bf,$7d,$83,$d6,$79,$73
        !by $c7,$7c,$97,$1e,$18,$8b,$7e,$fa,$06,$ac,$f3,$e6
        !by $8f,$f8,$2e

hinote
hinote2 = * + 1
        !by $01,$01,$01,$02,$02,$02,$02,$02,$02,$02,$03,$03
        !by $03,$03,$03,$04,$04,$04,$04,$05,$05,$05,$06,$06
        !by $07,$07,$07,$08,$08,$09,$09,$0a,$0b,$0b,$0c,$0d
        !by $0e,$0e,$0f,$10,$11,$12,$13,$15,$16,$17,$19,$1a
        !by $1c,$1d,$1f,$21,$23,$25,$27,$2a,$2c,$2f,$32,$35
        !by $38,$3b,$3f,$43,$47,$4b,$4f,$54,$59,$5e,$64,$6a
        !by $70,$77,$7e,$86,$8e,$96,$9f,$a8,$b3,$bd,$c8,$d4
        !by $e1,$ee,$fd

        ; tabellen

snelheid
        !by 2, 2

seqtabel
        !by <seq0a,<seq0b,<seq0c        ; song 1
        !by >seq0a,>seq0b,>seq0c

        !by <seq1a,<seq1b,<seq1c        ; song 2
        !by >seq1a,>seq1b,>seq1c

seqloclo
        !by 0,0,0
seqlochi
        !by 0,0,0

sequence
        !wo st00,st01,st02,st03,st04
        !wo st05,st06,st07,st08,st09
        !wo st0a,st0b,st0c,st0d,st0e
        !wo st0f,st10,st11,st12,st13
        !wo st14,st15,st16,st17,st18
        !wo st19,st1a,st1b,st1c,st1d
        !wo st1e,st1f,st20

wavearp !by $80,$10,$80,$10

pulsearp
        !by $00,$00,$00,$00
        !by $00,$00,$00,$00

drumtabel
        !wo dwa0,dto0
        !wo dwa1,dto1
        !wo dwa2,dto2

dwa0    !by $0b                         ; length 11
        !by $81,$81,$20,$20
        !by $10,$10,$10,$10
        !by $10,$10,$10,$10

dto0    !by $34,$0a,$08,$06
        !by $04,$05,$06,$05
        !by $04,$05,$06,$05

dwa1    !by $0b
        !by $81,$41,$80,$40
        !by $80,$80,$80,$80
        !by $10,$10,$10,$10

dto1    !by $3c,$0e,$00,$0c
        !by $38,$3c,$38,$3c
        !by $0d,$0d,$0d,$0d

dwa2
dto2

;   ---------------------------------------------------------

arplo   !by <arp0,<arp1,<arp2,<arp3,<arp4,<arp5,<arp6,<arp7
arphi   !by >arp0,>arp1,>arp2,>arp3,>arp4,>arp5,>arp6,>arp7

arp0    !by $02,$00,$03,$07
arp1    !by $02,$00,$04,$07
arp2    !by $02,$00,$05,$08
arp3    !by $02,$00,$05,$09
arp4    !by $02,$00,$03,$08
arp5    !by $02,$00,$04,$09
arp6    !by $17
        !by $e9,$ea,$eb,$ec,$ed,$ee,$ef,$f0
        !by $f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8
        !by $f9,$fa,$fb,$fc,$fd,$fe,$ff,$00
arp7    !by $02,$00,$03,$06

;   ---------------------------------------------------------

filterbytes
        !wo fb0,fb1,fb2,fb3

fp0     = $10 + volume
fb0     !by $a0,$fe,$02,$ff,$80
        !by fp0,$02,$22,$42,$62

fp1     = $30 + volume
fb1     !by $a0,$fe,$01,$ff,$62
        !by fp1,$02,$22,$62,$a2

fp2     = $20 + volume
fb2     !by $50,$05,$fc,$01,$80
        !by fp2,$02,$10,$20,$40

fp3     = $70 + volume
fb3     !by $a0,$ff,$f0,$ff,$40
        !by fp3,$02,$22,$23,$53

filtermega
strafilter
        !by $00,$08,$94,$01
aftrekspeed
        !by $00

;   ---------------------------------------------------------

pulsetabel
        !by $04,$0c,$02,$30
        !by $04,$30,$06,$30 ; 1

        !by $04,$0c,$04,$80
        !by $08,$60,$0c,$40 ; 2

        !by $01,$0f,$02,$66
        !by $08,$42,$0c,$28 ; 3

        !by $07,$0d,$03,$70
        !by $06,$60,$09,$50 ; 4

pulsehi = * + 0
waveform = * + 1
attdec = * + 2
susrel = * + 3
filcount = * + 4
fx1 = * + 5
fx2 = * + 6
fx3 = * + 7
        !by $00,$00,$00,$00,$00,$00,$00,$00 ; leeg
        !by $05,$41,$08,$89,$00,$53,$64,$80 ; bass
        !by $08,$11,$00,$a8,$f0,$00,$00,$10 ; bassdrm
        !by $08,$11,$00,$a9,$f0,$01,$00,$10 ; snardrm
        !by $04,$21,$00,$c8,$01,$00,$81,$85 ; arp+filt
        !by $0c,$41,$00,$b8,$00,$52,$22,$01 ; melo-1
        !by $06,$41,$00,$e8,$02,$53,$74,$01 ; melo-2
        !by $00,$17,$00,$88,$f0,$00,$00,$80 ; $17zooi
        !by $04,$21,$00,$88,$00,$00,$81,$84 ; arp-filt
        !by $00,$55,$00,$b8,$00,$00,$00,$20 ; $55
        !by $03,$41,$07,$69,$00,$00,$51,$80 ; bassfilt
        !by $05,$41,$00,$b8,$03,$53,$83,$01 ; melo-3
        !by $01,$41,$00,$89,$f0,$00,$00,$02 ; tingel
        !by $06,$41,$00,$c8,$08,$00,$41,$00 ; omhoogt
        !by $07,$81,$08,$fd,$f0,$00,$00,$00 ; $81lang
        !by $05,$41,$00,$b8,$03,$c5,$24,$01 ; solo
        !by $0c,$41,$00,$b8,$00,$c6,$22,$00 ; melo-1
        !by $07,$41,$08,$89,$00,$00,$31,$00 ; basso
        !by $05,$41,$00,$a7,$00,$c5,$22,$00 ; solo2

vibtabwait
        !by $00,$0c,$00,$00 ; 00-03
        !by $00,$18,$0c,$00 ; 04-07
        !by $00,$00,$00,$18 ; 08-0b
        !by $00,$00,$00,$00 ; 0c-0f
        !by $00,$00,$00,$00 ; 10-13

startlen
        !by $02,$02,$02,$02 ; 00-03
        !by $05,$02,$02,$02 ; 04-07
        !by $06,$02,$02,$02 ; 08-0b

starttabel
        !by $81,$81,$81,$81 ; 00-03
        !by $41,$11,$11,$81 ; 04-07
        !by $41,$81,$81,$81 ; 08-0b

        ; ------ song 1: title ------

seq0a
    !by $84,$01
    !by $06,$07,$06,$07
    !by $09,$07,$09,$07
    !by $06,$07,$06,$07
    !by $06,$07,$06,$07
    !by $09,$07,$09,$8b,$06
    !by $90,$09,$07,$09,$8b,$06
    !by $90,$06,$07,$06,$07
    !by $09,$07,$09,$07
    !by $84,$47,$12
    !by $84,$16,$8e,$16,$89,$16
    !by $84,$16,$16,$8e,$16
    !by $89,$16,$84,$16
    !by $97,$1c
    !by $84,$12,$8c,$12,$8e,$12
    !by $90,$12,$84,$12,$8c,$12
    !by $89,$12,$84,$12
    !by $16,$8e,$16,$89,$16
    !by $84,$16
    !by $97,$1c
    !by $84,$16,$8c,$16,$8e,$16
    !by $90,$16,$84,$16,$8c,$16
    !by $89,$16,$84,$16
    !by $90,$16,$8e,$16,$89,$16
    !by $90,$16,$16,$8e,$16
    !by $89,$16,$90,$16
    !by $1f,$20
    !by $ff

seq0b
    !by $84,$02,$02
    !by $0f,$0f,$11,$11
    !by $02
    !by $90,$14
    !by $1a,$1a
    !by $1c
    !by $15,$1a,$1c,$15,$1a
    !by $1b,$21 ; ($1d in orig)
    !by $43,$1d
    !by $ff

seq0c
    !by $60
    !by $90,$5d,$03,$04,$4f,$05
    !by $0b,$0c,$0d,$0d
    !by $0b,$0c,$0d,$0d
    !by $0b,$0c,$0d,$0d
    !by $0b,$0c,$0d,$0d
    !by $0e,$0c,$0d,$0d
    !by $0e,$0c,$95,$0b,$10
    !by $90,$0e,$0c,$0d,$0d
    !by $0e,$0c,$95,$0b,$10
    !by $90,$0b,$0c,$0d,$0d
    !by $0b,$0c,$0d,$0d
    !by $0e,$0c,$0d,$0d
    !by $0e,$0c,$0d,$0d
    !by $43,$13
    !by $8b,$17,$17,$89,$18,$18
    !by $84,$17,$17,$8b,$43,$17
    !by $89,$18,$18
    !by $84,$17,$17,$8b,$17,$17
    !by $90,$03,$03,$04
    !by $8b,$17,$17,$87,$18,$18
    !by $89,$18,$18,$8b,$43,$17
    !by $87,$18,$18,$84,$17,$17,$8b,$43,$17,$89,$18,$18,$84,$17,$17,$8b,$17,$17
    !by $90,$03,$03,$04,$8b,$17,$17,$87,$18,$18,$89,$18,$18,$8b,$43,$17
    !by $87,$18,$18,$84,$17,$17,$8b,$43,$17,$89,$18,$18,$84,$17,$17,$8b
    !by $43,$17
    !by $89,$18,$18
    !by $84,$17,$17,$8b,$17,$17
    !by $63
    !by $90,$20,$1f
    !by $ff

st01    ; bas1
    !by $c1,$a0,$a0,$e0,$40,$0c,$18,$e0,$40,$0a,$16,$e0,$40,$0c,$18,$e0
    !by $40,$0a,$16,$e0,$40,$08,$14,$e0,$40,$0a,$16,$e0,$40,$08,$14,$e0
    !by $40,$0a,$16,$ff

st02    ; arp1
    !by $f1,$82,$c4,$a0,$72,$2b,$73,$27,$88,$72,$2b,$84
    !by $73,$29,$b4,$29,$a0,$72,$2b,$73,$27,$88,$70,$30,$84,$71,$2e,$b4
    !by $2e,$8c,$20,$74,$24,$73,$27,$71,$2c,$88,$74,$30,$73,$33,$74,$32
    !by $84,$71,$2e,$b4,$73,$29,$8c,$73,$27,$71,$2c,$88,$73,$29,$84,$71
    !by $2c,$88,$73,$29,$8c,$27,$88,$74,$26,$84,$70,$24,$71,$22,$20,$94
    !by $22,$c9,$88,$3c,$3a,$38,$36,$ff

st03    ; bassdreum
    !by $c2,$88,$08,$14,$ff

st04    ; snardreun
    !by $c3,$84,$14
    !by $14,$14,$14,$82,$14,$84,$14,$14,$82,$14,$14,$14,$ff

st05    ; ritm + short
    !by $c2,$84,$14
    !by $c7,$82,$2c,$2c,$c3,$84,$14,$c7,$82,$2c,$2c,$c2,$84,$14,$c7,$82
    !by $2c,$2c,$c3,$84,$14,$c7,$82,$2c,$2c,$ff

st06    ; bas2
    !by $c1,$84,$0c,$0c,$82,$18
    !by $0a,$0b,$84,$0c,$82,$0c,$84,$0c,$82,$18,$07,$0a,$07,$84,$0c,$0c
    !by $82,$18,$0a,$0b,$84,$0c,$82,$0c,$84,$0c,$82,$18,$0c,$0b,$17,$ff

st07
    !by $84,$0a,$0a,$82,$16,$07,$05,$84,$0a,$82,$0a,$84,$0a,$82,$16,$05
    !by $07,$05,$84,$0a,$0a,$82,$16,$07,$05,$84,$0a,$82,$0a,$84,$0a,$82
    !by $16,$0a,$05,$07,$ff

st09
    !by $c1,$84,$08,$08,$82,$14,$05,$03,$84,$08,$82
    !by $08,$84,$08,$82,$14,$03,$05,$03,$84,$08,$08,$82,$14,$05,$03,$84
    !by $08,$82,$08,$84,$08,$82,$14,$08,$09,$15,$ff

st0b
    !by $c2,$84,$14,$c8,$82
    !by $72,$1f,$1f,$c3,$84,$14,$c8,$82,$1f,$1f,$c2,$84,$14,$c8,$82,$1f
    !by $1f,$c3,$84,$14,$c8,$82,$1f,$1f,$ff

st0c
    !by $c2,$84,$14,$c8,$82,$71,$20
    !by $20,$c3,$84,$14,$c8,$82,$20,$20,$c2,$84,$14,$c8,$82,$20,$20,$c3
    !by $84,$14,$c8,$82,$20,$20,$ff

st0d
    !by $c2,$84,$14,$c8,$82,$73,$1d,$1d,$c3
    !by $84,$14,$c8,$82,$1d,$1d,$c2,$84,$14,$c8,$82,$1d,$1d,$c3,$84,$14
    !by $c8,$82,$1d,$1d,$ff

st0e
    !by $c2,$84,$14,$c8,$82,$73,$1b,$1b,$c3,$84,$14
    !by $c8,$82,$1b,$1b,$c2,$84,$14,$c8,$82,$1b,$1b,$c3,$84,$14,$c8,$82
    !by $1b,$1b,$ff

st0f
    !by $f1,$f2,$c5,$94,$24,$84,$24,$26,$88,$e0,$30,$26,$27
    !by $84,$26,$24,$88,$1f,$24,$e0,$14,$24,$22,$22,$b4,$22,$94,$24,$84
    !by $24,$26,$88,$e0,$14,$27,$26,$84,$26,$24,$1f,$2b,$88,$30,$a4,$a0
    !by $e0,$14,$30,$2e,$ff

st10
    !by $c2,$84,$14,$c8,$82,$74,$1e,$1e,$c3,$84,$14
    !by $c8,$82,$1e,$1e,$c2,$84,$14,$c8,$82,$1e,$1e,$c3,$08,$08,$08,$08
    !by $ff

st11
    !by $c6,$84,$30,$2e,$2c,$8c,$27,$84,$30,$2e,$2c,$8c,$27,$84,$30
    !by $32,$33,$35,$8c,$e0,$30,$32,$33,$32,$84,$2e,$a4,$29,$88,$27,$84
    !by $26,$8c,$24,$88,$27,$2c,$30,$84,$32,$88,$33,$a4,$e0,$20,$30,$32
    !by $c9,$a0,$4f,$ff

st12
    !by $c1,$84,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c
    !by $0c,$0c,$0c,$0c,$0c,$0c,$ff

st13
    !by $cc,$82,$2b,$24,$1f,$2b,$24,$1f,$2b
    !by $24,$1f,$2b,$24,$1f,$2b,$24,$1f,$2b,$29,$24,$1f,$29,$24,$1f,$29
    !by $24,$1f,$29,$24,$1f,$29,$24,$1f,$29,$27,$24,$1f,$27,$24,$1f,$27
    !by $24,$1f,$27,$24,$1f,$27,$24,$1f,$27,$29,$24,$1d,$29,$24,$1d,$29
    !by $24,$1d,$29,$24,$1d,$29,$24,$1d,$29,$ff

st14
    !by $f1,$f2,$c5,$a0,$18,$86
    !by $e0,$30,$1a,$1b,$1a,$18,$e0,$30,$1a,$1b,$84,$1a,$18,$a0,$a0,$13
    !by $a0,$18,$86,$e0,$30,$1a,$1b,$1a,$18,$e0,$20,$1c,$1d,$84,$1f,$20
    !by $a0,$1f,$c9,$48,$c5,$a0,$18,$86,$e0,$30,$1a,$1b,$1a,$18,$e0,$30
    !by $1a,$1b,$84,$1a,$18,$a0,$a0,$13,$a0,$18,$86,$e0,$30,$1a,$1b,$1a
    !by $18,$e0,$20,$1d,$1f,$84,$22,$24,$a0,$e0,$20,$22,$24,$c9,$90,$38
    !by $ce,$30,$ff

st15
    !by $f1,$f2,$cb,$90,$24,$a0,$e0,$20,$29,$2b,$84,$2b,$29
    !by $88,$27,$26,$84,$27,$b4,$24,$8c,$22,$e0,$20,$27,$29,$88,$27,$84
    !by $e0,$10,$27,$29,$2b,$2c,$88,$2e,$84,$30,$29,$2e,$a0,$a0,$e0,$20
    !by $29,$2b,$90,$24,$a0,$e0,$20,$29,$2b,$84,$27,$26,$88,$22,$24,$84
    !by $1b,$a4,$20,$84,$24,$27,$88,$2b,$e0,$20,$2b,$2c,$84,$2b,$8c,$2c
    !by $88,$24,$84,$e0,$20,$2b,$2c,$2b,$88,$29,$84,$22,$26,$27,$a4,$a0
    !by $e0,$20,$22,$24,$ff

st16
    !by $c1,$84,$0c,$0c,$18,$0c,$0c,$0c,$82,$18,$84
    !by $0c,$82,$0c,$84,$0c,$0c,$18,$0c,$0c,$0c,$82,$18,$84,$0c,$82,$18
    !by $ff

st17
    !by $c2,$82,$14,$c8,$84,$72,$24,$82,$24,$c3,$84,$14,$c8,$82,$24
    !by $24,$c2,$14,$c8,$24,$84,$24,$c3,$14,$c8,$82,$24,$24,$ff

st18
    !by $c2,$82
    !by $14,$c8,$84,$73,$24,$82,$24,$c3,$84,$14,$c8,$82,$24,$24,$c2,$14
    !by $c8,$24,$84,$24,$c3,$14,$c8,$82,$24,$24,$ff

st19
    !by $cd,$a0,$18,$90,$1b
    !by $88,$1d,$84,$1f,$82,$22,$24,$ff

st1a
    !by $cb,$84,$0c,$27,$24,$1f,$26,$24
    !by $1f,$27,$0c,$27,$24,$1f,$26,$24,$1f,$27,$0a,$26,$22,$1d,$24,$22
    !by $1d,$26,$0a,$22,$24,$26,$e0,$01,$26,$27,$26,$22,$1d,$11,$24,$20
    !by $1d,$26,$20,$1d,$24,$11,$24,$20,$1d,$26,$20,$1d,$27,$0c,$27,$24
    !by $1f,$26,$24,$1f,$27,$0c,$24,$1f,$1b,$1a,$16,$13,$18,$ff

st1b
    !by $cb,$a0
    !by $90,$24,$88,$26,$27,$a0,$a0,$26,$a0,$90,$24,$88,$26,$27,$82,$24
    !by $1f,$9c,$18,$ff

st1c
    !by $f1,$03,$cb,$a0,$a0,$18,$ff

st1d
    !by $f1,$d5 ; orig $75
    !by $ca,$84,$0c
    !by $82,$00,$82,$0c,$c3,$84,$08,$ca,$82,$0a,$84,$0b,$82,$0c,$82,$0c
    !by $82,$18,$c3,$84,$08,$ca,$82,$13,$82,$0c,$84,$0a,$82,$0b,$82,$0c
    !by $c3,$84,$08,$ca,$84,$0a,$82,$16,$84,$0b,$82,$17,$c3,$84,$08,$ca
    !by $82,$00,$82,$07,$ff

;st1e
    ; !by $f1,$d5,$ca,$84,$0c,$82,$00,$82,$0c,$c3,$84
    ; !by $08,$ca,$82,$0a,$84,$0b,$82,$0c,$82,$0c,$82,$18,$c3,$84,$08,$ca
    ; !by $82,$13,$82,$0c,$ff

st1f
    !by $cf,$94,$2e,$84,$2b,$2e,$2b,$2e,$88,$30,$8c
    !by $e0,$30,$32,$33,$82,$29,$2a,$84,$2b,$2e,$e0,$01,$29,$2a,$29,$27
    !by $22,$24,$27,$29,$82,$2a,$2b,$29,$2a,$2b,$2e,$30,$92,$e0,$88,$2e
    !by $01,$84,$30,$2e,$30,$82,$2e,$86,$e0,$20,$32,$33,$84,$32,$82,$2e
    !by $84,$30,$e0,$20,$2d,$2e,$82,$2b,$2a,$29,$27,$29,$1f,$22,$24,$84
    !by $27,$8a,$e0,$20,$22,$24,$84,$27,$82,$29,$27,$29,$2a,$29,$2a,$84
    !by $e0,$20,$2a,$2b,$82,$2e,$2a,$2b,$2e,$84,$30,$9d,$e0,$20,$2e,$30
    !by $ff

st20
    !by $c0,$83,$00
    !by $ff

    ; ---------------------- song 2: game over -------------------------

seq1a
    !by $8c,$08
    !by $fe

seq1b
    !by $8c,$0a
    !by $ff
seq1c
    !by $8c,$1e
    !by $ff

st08
    !by $f1,$03
    !by $c5,$8c,$24,$82,$26,$27,$84,$26,$24,$1f,$1b,$88,$1a,$13,$86,$17
    !by $18,$84,$1a,$d0,$a0,$a0,$e0,$8f,$18,$24
    !by $ff

st0a
    !by $c5,$8c,$1b,$82,$1d
    !by $1f,$84,$1d,$1b,$18,$13,$88,$11,$0f,$86,$0e,$0f,$84,$11,$d0,$a0
    !by $a0,$e0,$8f,$0f,$1b
    !by $ff

st1e
    !by $d1,$88,$0c,$0c,$84,$18,$0c,$05,$88,$07
    !by $84,$07,$07,$07,$13,$07,$07,$07,$a0,$a0,$0c
    !by $ff
st00
    !by $c0,$a0,$00
    !by $ff

}
