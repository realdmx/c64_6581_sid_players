
	; Armada
	; By Reyn Ouwehand
	; Player by Charles Deenen
	; 1989 Scoop Designs

	; Converted to ACME by dmx87

	!to "Ouwehand_Reyn_Armada.sid",plain

	* = 0
	    
	!text	"PSID"			; PSID
	!be16	2				; version
	!be16	$7c				; offset
	!be16	0				; load address auto
	!be16	Init			; init address
	!be16	Play			; play address
	!be16	6				; nsongs
	!be16	6				; starting song
	!be32	0				; song speed (VBL)
-   !text 	"Armada"
	!fill 32-(*--),0
-	!text 	"Reyn Ouwehand"
	!fill 32-(*--),0
-	!text 	"1989 Scoop Designs"
	!fill 32-(*--),0

	!be16	0				; flags
	!be16	0				; reloc
	!be16	0				; v3+4
	
	!wo $1000               ; load address
	
!pseudopc $1000 {

; 0@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@.
; ]      --MUSICFILE V01-07-1988     ]
; ]  Programmed by Charles Deenen    ]
; +@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@3
; ]  the most advanced music player  ]
; ]ever written for the commodore 64!]
; +@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@3
; ]   Scoop Designs L.T.D. in 1989   ]
; +@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@3
; ]Special version for :Reyn Ouwehand]
; -@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@=

volume   = $0f
songetje = 1
numvoice = 7
dubvoice = $10
noisehitone = $fa
pulsearpwait = $01
gitarwait = $0f
spacelength = $00
spacewait = $60
wavearpwait = 3


;0@@@@@@@@@@@@@@@@@@@@@@@@@@@.
;]Benodigde ZER0PAGE adressen]
;-@@@@@@@@@@@@@@@@@@@@@@@@@@@=

fx1sto   = $61
fx2sto   = $62
fx3sto   = $63
tabbytsto = $64
zer0fillo = $65; 2 bytes
zp3      = $67
zp4      = $68
wax      = $69

voicesto = $6a
vibrasto = $6b
zpdat3   = $6c
vibreallo = $6d
vibrealhi = $6e
temphino = $6f
templono = $70



c0       = $00
cf0      = $01
d0       = $02
df0      = $03
e0       = $04
f0       = $05
ff0      = $06
g0       = $07
gf0      = $08
a0       = $09
af0      = $0a
b0       = $0b
c1       = $0c
cf1      = $0d
d1       = $0e
df1      = $0f
e1       = $10
f1       = $11
ff1      = $12
g1       = $13
gf1      = $14
a1       = $15
af1      = $16
b1       = $17
c2       = $18
cf2      = $19
d2       = $1a
df2      = $1b
e2       = $1c
f2       = $1d
ff2      = $1e
g2       = $1f
gf2      = $20
a2       = $21
af2      = $22
b2       = $23
c3       = $24
cf3      = $25
d3       = $26
df3      = $27
e3       = $28
f3       = $29
ff3      = $2a
g3       = $2b
gf3      = $2c
a3       = $2d
af3      = $2e
b3       = $2f
c4       = $30
cf4      = $31
d4       = $32
df4      = $33
e4       = $34
f4       = $35
ff4      = $36
g4       = $37
gf4      = $38
a4       = $39
af4      = $3a
b4       = $3b
c5       = $3c
cf5      = $3d
d5       = $3e
df5      = $3f
e5       = $40
f5       = $41
ff5      = $42
g5       = $43
gf5      = $44
a5       = $45
af5      = $46
b5       = $47
c6       = $48
cf6      = $49
d6       = $4a
df6      = $4b
e6       = $4c
f6       = $4d
ff6      = $4e
g6       = $4f
gf6      = $50
a6       = $51
af6      = $52
b6       = $53
c7       = $54
cf7      = $55
d7       = $56
df7      = $57
e7       = $58
f7       = $59
ff7      = $5a
g7       = $5b
gf7      = $5c
a7       = $5d
af7      = $5e
b7       = $5f



         *= $1000

         ; ldx #$06
         ; jsr song
; opium    jsr playirq
         ; jsr trug
; irri     lda $d019
         ; sta $d019
         ; and #$01
         ; bne opium
         ; jmp irri

; nek      rts
; trug     jsr $ffe4
         ; cmp #$20
         ; bne nek
         ; jmp $9000

;--------------Player-------------------

Init     tax
		 jmp song
         jmp songout
Play     jmp playirq

speedbyte !by 0

d4point  !by 0,7,14      ; $d4xx-point
tabcount !by 0,0,0       ; tabelcount
begcount !by 0,0,0       ; begincount
nootcount !by 0,0,0      ; nootcount
nootleng !by 0,0,0       ; nootlenght
wavesto  !by 0,0,0       ; waveforms
noothoogt !by 0,0,0      ; noothoogte
noho     !by 0,0,0       ; tempnoothoo
wavecount !by 0,0,0      ;waveformcoun
hinotesto !by 0,0,0
hinotesto2 !by 0,0,0
lonotesto !by 0,0,0
glidetest !by 0,0,0
glidetest2 !by 0,0,0
pulselosto !by 0,0,0
pulsehisto !by 0,0,0
pulsehitemp !by 0,0,0
pulsecountup !by 0
counter2 !by 0,0,0
toneadd  !by 0,0,0
loadpoint !by 0
vibstore1 !by 0,0,0
vibstore2 !by 0,0,0
vibstore3 !by 0,0,0
tonearpcounter !by 0,0,0
arpieoklo !by 0,0,0
arpieokhi !by 0,0,0
st2      !by 0
st       !by 0
filter   !by 0,0,0
filtercount !by 0,0,0
pulsetest !by 0,0,0
wivofilter !by 0
speedsto !by 0     ;speed store
testbyte !by 1     ;if 1-> music stop
filwhat  !by 0
repeatsto !by 0,0,0
stod404  !by 0,0,0
newnote  !by 0,0,0
strfiltest !by 0
tempglide !by 0,0,0
glidedelay !by 0,0,0
denom    !by 0
strafil  !by 0
d400     !by 0,0,0
d401     !by 0,0,0
voiceinc !by 0,0,0
byteand  !by 0,0,0
pulseruntest !by 0,0,0
pulserunlo !by 0,0,0
pulserunhi !by 0,0,0
d402     !by 0,0,0
d403     !by 0,0,0
tabelend !by 0

ok2
         lda #0
         ldx #tabelend-tabcount
is1      sta tabcount,x
         dex
         bne is1

ok       lda #$ff
         sta counter2
         sta counter2+1
         sta counter2+2
         lda #0
         ldx #$02
b24      sta tabcount,x
         sta begcount,x
         sta nootcount,x
         sta noho,x
         dex
         bpl b24
         sta testbyte
         rts

song
         lda #2
         sta testbyte
         lda seqtabello,x
         sta yo1
         lda seqtabelhi,x
         sta yo2
         ldy #5
yo1      = *+1
yo2      = *+2
so1      lda $ffff,y
         sta seqloclo,y
         dey
         bpl so1
         sty $d416
         iny
         sty $d417
         iny
         sty pulseruntest
         sty pulseruntest+1
         sty pulseruntest+2
         lda snelheid,x
         sta speedbyte
         lda nootafbreek,x
         sta stopnote
         jsr ok2
uitzet
         ldx #$17
so2      lda #1
         sta $d400,x
         lda #0
         sta $d400,x
         dex
         bpl so2
         lda #$10+volume
         sta $d418
         rts

         !text "music by reyn ouwehan"
         !text "d from scoop designs "

songout
         lda #2
         sta testbyte
         jmp uitzet

 ;0@@@@@@@@@@@@@@@@@@.
 ;]aha! a samplerouty]
 ;-@@@@@@@@@@@@@@@@@@=



playirq
         lda testbyte
         cmp #2
         beq y
         cmp #1
         bne ok3
         jmp ok
y        rts
ok3
         inc counter2
         inc counter2+1
         inc counter2+2

         ldx #2
         dec speedsto
         bpl startplayer

         lda speedbyte
         sta speedsto

         ;-------set tracklo+hi-z------
startplayer
         lda #numvoice
         and playit,x
         bne playyes
         jmp endit
playit   !by 1,2,4

playyes
         stx wax
         lda d4point,x
         sta voicesto
         tay
         lda speedsto
         cmp speedbyte
         bne h1
         lda seqloclo,x
         sta yoa1
         lda seqlochi,x
         sta yoa2
         dec nootcount,x
         bmi h2
         jmp h10
h1       jmp h11

         ;--------take step from track--
h2
         ldy tabcount,x
yoa1     = *+1
yoa2     = *+2
         lda $ffff,y
         cmp #$fe
         beq songout
         cmp #$ff
         bne h3
         lda #$00
         sta nootcount,x
         sta tabcount,x
         sta begcount,x
         jmp h2

         ;-------------add tone---------
h3       sta st2

         cmp #$40
         bcc h3f

         cmp #$80
         bcc h3a
         and #%00011111
         sta toneadd,x
         inc tabcount,x
         jmp h2

         ;------------set steplo+hi-----
h3a
         lda st2
         cmp #$60
         bcc h3c
         and #%00001111
         sta voiceinc,x
         inc tabcount,x
         jmp h2
h3c
         and #%00111111
         sta repeatsto,x
         inc tabcount,x
         jmp h2

         lda st2
h3f
         asl
         tay
         lda seqence,y
         sta zp3
         lda seqence+1,y
         sta zp4
         ;------------load note---------
         lda #$00
         sta glidetest,x
         sta glidetest2,x
         ldy begcount,x
         sta counter2,x
junk     lda (zp3),y
         sta tabbytsto
junk2    and #$f0
         cmp #$f0
         bne startnewnote
         lda tabbytsto
         and #1
         bne dofilset
         lda #1
         sta newnote,x
         inc begcount,x
         iny
         lda (zp3),y
         sta tabbytsto
         jmp nolengset
dofilset
         jsr verhoogtest
         sta $d417
         jsr verhoogtest
startnewnote
         lda #0
         sta newnote,x
    ;@@@@@@@glideset@@@@@@@@@@
         lda tabbytsto
         and #$f0
         cmp #$e0
         bne noglideset
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
noglideset
         lda tabbytsto
         and #$e0
         cmp #$c0
         bne novoiceset
         lda tabbytsto
         and #$1f
         beq novoiceinc
         clc
         adc voiceinc,x
novoiceinc
         sta wavecount,x
         jsr verhoogtest
novoiceset
         lda tabbytsto
         and #$f0
         cmp #$70
         bne noarpset
         lda tabbytsto
         and #%00001111
         sty denom
         tay
         lda arplo,y
         ldx wax
         sta arpieoklo,x
         lda arphi,y
         sta arpieokhi,x
         ldy denom
         jsr verhoogtest
noarpset
         lda tabbytsto
         and #$c0
         cmp #$80
         bne nolengset
         lda tabbytsto
         and #$3f
         sec
         sbc #1
         sta nootleng,x
         jsr verhoogtest
         and #$c0
         cmp #$80
         bne gojunk
         lda tabbytsto
         and #$3f
         clc
         adc nootleng,x
         sta nootleng,x
         jsr verhoogtest
gojunk
         lda tabbytsto
         jmp junk2


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
         ;-------------set adsr values--
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
         stx loadpoint
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
         sta pulselosto,x
         pla
         sta pulsehitemp,x
         and #$0f
         sta d403,x
         sta pulsehisto,x
         lda #1
         sta pulsetest,x
         pla
         sta filtercount,x

snnn     inc begcount,x
         ldy begcount,x
         lda (zp3),y
         cmp #$ff
         bne h10b
nextjmp
         lda #$00
         sta begcount,x
         lda repeatsto,x
         beq nj1
         dec repeatsto,x
         bpl h10b
nj1
         inc tabcount,x
h10b
         lda #$ff
         sta byteand,x
         jmp nextvoice

h10
         lda nootcount,x
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
gwb
         sta byteand,x
h11
         lda pulsehitemp,x
         and #$10
         beq gwo2

         lda nootcount,x
         bne gwo2
         lda speedsto
stopnote = *+1
         cmp #0
         bne gwo2
         lda #$02
         sta $d406,y
gwo2

         lda wavecount,x
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


javib
       ;0@@@@@@@@@@@@.
       ;]Tonearpeggio]
       ;-@@@@@@@@@@@@=

         lda fx3sto
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

arpieoklo1 = *+1
arpieokhi1 = *+2
         lda arp0
         sta tonearpcounter,x
hallo

         ldx wax
         ldy tonearpcounter,x
         iny
arpieoklo2 = *+1
arpieokhi2 = *+2
         lda arp0,y
         clc
         adc noho,x
         sta noothoogt,x
         tay
         lda lonote,y
         sta d400,x
         lda hinote,y
         sta d401,x

       ;0@@@@@@@@@@@@@@@.
       ;]Vibrato routine]
       ;-@@@@@@@@@@@@@@@=
javib2
         lda fx1sto
         beq b17jmp2

         lda glidetest2,x
         bne b17jmp2

         ldy wavecount,x
         ldx vibwaittab,y
         stx vibwait
         lda fx1sto
         and #$0f
         sta vibrasto
         lda fx1sto
         and #%01110000
         lsr
         lsr
         lsr
         lsr
         ldx wax
         sta vibstore1,x
         ldy #$bc     ;adc abs
         lda fx1sto
         bpl doityes
         ldy #$7d     ;ldy abs
doityes
         sty doitnot

         ldy noothoogt,x
         lda lonote2,y
         sec
         sbc lonote,y
         sta templono

         lda hinote2,y
         sbc hinote,y
doitnot  adc counter2,x
         sta temphino
reducesize
         dec vibrasto
         bmi redout
         lsr temphino
         ror templono
         jmp reducesize

b17jmp2  jmp b17

redout
         lda vibstore2,x
         bpl w1
         dec vibstore3,x
         bne nextsect
         inc vibstore2,x
         bpl nextsect
w1       inc vibstore3,x
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
subval   dey
         bmi endsv

         lda counter2,x
vibwait  = *+1
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
       ;-----------------

endsv    ldy vibstore3,x
addval   dey
         bmi endav
         lda vibreallo
         clc
         adc templono
         sta vibreallo
         lda vibrealhi
         adc temphino
         sta vibrealhi
         jmp addval

endav
         ldx wax
         lda vibreallo
         sta d400,x
         sta lonotesto,x
         lda vibrealhi
         sta d401,x
         sta hinotesto,x

       ;0@@@@@@@@@@.
       ;]Tone glide]
       ;-@@@@@@@@@@=
b17

         ldx wax
         lda glidetest,x
         bne rara
glideoutjmp
         jmp glideout
glideoutjmp2
         jmp glideout2

rara
         lda glidedelay,x
         and #$f0
       ; lsr a
       ; lsr a
         lsr
         lsr
         sta glen
         sta bran

         lda glidedelay,x
         and #$0f
         asl
         asl
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
glen     = *+1
         adc #0
         cmp nootleng,x
         bcc glideoutjmp2

         ldy noho,x
         lda tempglide,x
         tax
         sec
         lda lonote,y
         sbc lonote,x
         sta udlo
         lda hinote,y
         sbc hinote,x
         sta udhi

         ldx #$38
         ldy #$e9
         bcs eg

         ldx #$18  ;clc
         ldy #$69  ;adc

         eor #$ff
         sta udhi

         lda udlo
         eor #$ff
         sta udlo

         inc udlo
         bne eg
         inc udhi

eg
         sty updown1
         sty updown2
         stx glisscarry

rightone ldy speedbyte
         lda #0
         clc
bran     = *+1
dfff     adc #0
         dey
         bpl dfff
         sta denom
         clc

         ldx #16
         lda #0
nekstbit rol udlo
         rol udhi
         rol
         bcs notoff
         cmp denom
         bcc leave
notoff   sbc denom
         sec
leave    dex
         bne nekstbit

         rol udlo
         rol udhi
         asl
         cmp denom
         bcc noway
         inc udlo
         bne noway
         inc udhi
noway

doglide
         ldx wax
         lda lonotesto,x
glisscarry clc
udlo     = *+1
updown1  adc #0
         sta lonotesto,x
         sta d400,x
         lda hinotesto,x
udhi     = *+1
updown2  adc #0
         sta hinotesto,x
         sta d401,x
         jmp glideout
pst      jmp pulsestore

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

         ;0@@@@@@@@@@.
         ;]pulsegedoe]
         ;-@@@@@@@@@@=

         lda fx2sto
         and #$07
         beq pst
         and #%00000111
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
noprep
         sta purepbyte
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

go6      iny
         iny
         lda pulsetabel,y
         and #$7f
         cmp counter2,x
         bcc go2
         jmp go5

go2      iny
         iny
         lda pulsetabel,y
         and #$7f
         cmp counter2,x
         bcc go3
go5
         lda pulsetabel,y
         and #$80
         beq goo1
         lda #0
         sta pulsetest,x
goo1
         iny
         lda pulsetabel,y
         sta pulsecountup
         jmp go4
go3
         lda fx2sto
         and #%11110000
         sta pulsecountup
go4
         lda pulsetest,x
         bne pusw1

         lda pulselosto,x
         sec
         sbc pulsecountup
         sta pulselosto,x
         lda pulsehisto,x
         sbc #0
         sta pulsehisto,x
pulsecountlo = *+1
         cmp #$01
         bcs pulsestore
         lda #1
         bne pulseshit
pusw1
         lda pulselosto,x
         clc
         adc pulsecountup
         sta pulselosto,x
         lda pulsehisto,x
         adc #0
         sta pulsehisto,x
pulsecounthi = *+1
         cmp #$0e
         bcc pulsestore
purepbyte = *+1
         lda #0
         beq ppt
         sta pulselosto,x
         lda pulsecountlo
         sta pulsehisto,x
         lda #1
         bne pulseshit
ppt
         lda #0
pulseshit
         sta pulsetest,x
pulsestore
         ldx wax
         lda pulselosto,x
         sta d402,x
         lda pulsehisto,x
         sta d403,x

       ;0@@@@@@@@@@@@.
       ;]Wavearpeggio]
       ;-@@@@@@@@@@@@=

wavetry  lda fx3sto
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
rdw
         ldx wax
         sta stod404,x

       ;0@@@@@@@@@@@@@.
       ;]Pulsearpeggio]
       ;-@@@@@@@@@@@@@=

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

       ;0@@@@@@@@@@@@.
       ;]Tonesweep up]
       ;-@@@@@@@@@@@@=
sweep
         lda fx3sto
         and #$20
         beq filterklooi
         ldx wax
        ;lda nootcount,x
        ;cmp #4
        ;bcs filterklooi
         lda hinotesto,x
         sec
         sbc #$81
         sta hinotesto,x
         sta d401,x

       ;0@@@@@@@@@@@.
       ;]filterklooi]
       ;-@@@@@@@@@@@=

filterklooi
         lda fx3sto
         and #1
         beq fm2

         ldx wax
         stx filwhat
         lda filtercount,x
         and #%00000011
         asl
         tax
         lda filterbytes,x
         sta trulo
         lda filterbytes+1,x
         sta truhi
trulo    = *+1
         lda #<fb0
         sta zer0fillo
truhi    = *+1
         lda #>fb0
         sta zer0fillo+1

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
filfur3
         dey
         cmp (zer0fillo),y
         bcs filfur1
         cpy #6
         bne filfur3
         ldy #0
         lda (zer0fillo),y
         jmp fme

frmara   jmp frutsmaarraak

filfur1
         dey
         dey
         dey
         dey
         dey
         lda filter,x
         clc
         adc (zer0fillo),y
         jmp fme
fm2
         ldx wax
         cpx filwhat
         bne frutsmaarraak

         lda fx2sto
         and #$08
         bne frutsmaarraak

         lda #$ff
fme      sta filter,x
         sta $d416

frutsmaarraak

       ;0@@@@@@@@@@@@@.
       ;]strangefilter]
       ;-@@@@@@@@@@@@@=

         lda fx2sto
         and #$08
         beq pulserun

         lda counter2+2
         and #1
         beq stfilout

         lda strfiltest
         beq fillup
filldown
         lda strafilter
         sec
         sbc filtermega+3
         sta strafilter
         cmp filtermega+1
         bcs stfilout
         lda #0
         sta strfiltest
         beq stfilout
fillup
         lda strafilter
         clc
         adc filtermega+3
         sta strafilter
         cmp filtermega+2
         bcc stfilout
         lda #1
         sta strfiltest
stfilout
         ldx wax
         lda counter2+2
         cmp #2
         bcs stfo2

         lda #$40
         sta strafil
stfo2
         lda strafilter
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
pulserun
       ;0@@@@@@@@.
       ;]pulserun]
       ;-@@@@@@@@=
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
         adc #4
         sta pulserunlo,x
         sta d402,x
         bcc pulserunsto
         inc pulserunhi,x
         lda pulserunhi,x
pulserunsto
         lda pulserunhi,x
         sta d403,x
         jmp jeroenshit

pulserunout
         lda #1
         sta pulseruntest,x

       ;0@@@@@@@@@@@@@.
       ;]double voices]
       ;-@@@@@@@@@@@@@=

jeroenshit
         ldx wax
         lda filtercount,x
         and #8
         beq space

         lda d400,x
         clc
         adc #dubvoice
         sta d400,x
         lda d401,x
         adc #0
         sta d401,x

       ;0@@@@@@@@@@@@.
       ;]space effect]
       ;-@@@@@@@@@@@@=

space    ldx wax
         lda filtercount,x
         and #4
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

       ;0@@@@@@@@@@@.
       ;]Drumroutine]
       ;-@@@@@@@@@@@=

b19a     lda fx3sto
         and #$10
         beq noti
         lda fx1sto
         and #%00001111
         asl
         asl
         tax
         lda drumtabel,x
         sta dwalo
         sta drummylen+1
         lda drumtabel+1,x
         sta dwahi
         sta drummylen+2
         lda drumtabel+2,x
         sta dtalo
         lda drumtabel+3,x
         sta dtahi
drummylen
         lda $ffff
         sta dl+1

         ldx wax
         lda counter2,x
dl       cmp #15
         bcs drfu
         tay
dwalo    = *+1
dwahi    = *+2
         lda dwa1,y
         sta stod404,x
         and #1
         beq nd1
         lda #$ff
         bmi nd2
nd1
         lda #$fe
nd2
         sta byteand,x

         dey
dtalo    = *+1
dtahi    = *+2
         lda dto1,y
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
drfu2
         ldx wax
         lda st
         sta d401,x
         lda #0
         sta d400,x
drfu     jmp nextvoice

         ;0@@@@@@@@.
         ;]noisetik]
         ;-@@@@@@@@=
noti
         lda fx3sto
         and #$80
         beq b23

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
nohiset
         jmp nve
nv3
         lda startlen,y
         clc
         adc #2
         sta stle2
         lda counter2,x
stle2    = *+1
         cmp #0
         bcs b23
         lda lonotesto,x
         sta d400,x
         lda hinotesto,x
         sta d401,x
         lda wavesto,x
nve
         sta stod404,x
b23
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
endit
         dex
         bmi playout
         jmp startplayer
playout
         rts

cdac
         tay
         ldx wax
         lda lonote,y
         sta d400,x
         lda hinote,y
         sta d401,x
         jmp nextvoice


       ;0@@@@@@@@@@@.
       ;]Hi-Lo tabel]
       ;-@@@@@@@@@@@=

lonote
lonote2  = *+1
         !by 28,45,62,81,102,123
         !by 145,169,195,221,250,24
         !by 56,90,125,163,204,246
         !by 35,83,134,187,224,48
         !by 112,180,251,71,152,237
         !by 71,167,12,119,233,97
         !by 225,104,247,143,48,218
         !by 143,78,24,239,210,195
         !by 195,209,239,31,96,181,30
         !by 156,49,223,165,135,134
         !by 162,223,62,193,107,60,57
         !by 99,190,75,15,12,69,191
         !by 125,131,214,121,115,199
         !by 124,151,30,24,139,126
         !by 250,6,172,243,230,143
         !by 248,46

hinote
hinote2  = *+1

         !by 1,1,1,1,1,1,1,1,1,1,1
         !by 2,2,2,2,2,2,2,3,3,3,3,3
         !by 4,4,4,4,5,5,5,6,6,7,7,7
         !by 8,8,9,9,10,11,11,12,13
         !by 14,14,15,16,17,18,19,21
         !by 22,23,25,26,28,29,31,33
         !by 35,37,39,42,44,47,50,53
         !by 56,59,63,67,71,75,79,84
         !by 89,94,100,106,112,119,126
         !by 134,142,150,159,168,179
         !by 189,200,212,225,238,253


       ;0@@@@@@@@.
       ;]tabellen]
       ;-@@@@@@@@=

snelheid !by 2    ; * 0 *
         !by 1
         !by 2
         !by 3
         !by 2
         !by 2
         !by 1

nootafbreek

         !by 1    ; * 0 *
         !by 1
         !by 1
         !by 1
         !by 1
         !by 1
         !by 1

;------------Number-of-tunes------------

seqtabello !by <seqtab0,<seqtab1
         !by <seqtab2,<seqtab3
         !by <seqtab4,<seqtab5
         !by <seqtab6

seqtabelhi !by >seqtab0,>seqtab1
         !by >seqtab2,>seqtab3
         !by >seqtab4,>seqtab5
         !by >seqtab6

;---------------------------------------

seqtab0  !by <seq0a,<seq0b,<seq0c
         !by >seq0a,>seq0b,>seq0c

seqtab1  !by <seq1a,<seq1b,<seq1c
         !by >seq1a,>seq1b,>seq1c

seqtab2  !by <seq2a,<seq2b,<seq2c
         !by >seq2a,>seq2b,>seq2c

seqtab3  !by <seq3a,<seq3b,<seq3c
         !by >seq3a,>seq3b,>seq3c

seqtab4  !by <seq4a,<seq4b,<seq4c
         !by >seq4a,>seq4b,>seq4c

seqtab5  !by <seq5a,<seq5b,<seq5c
         !by >seq5a,>seq5b,>seq5c

seqtab6  !by <seq6a,<seq6b,<seq6c
         !by >seq6a,>seq6b,>seq6c

seqloclo !by <seq0a,<seq0b,<seq0c
seqlochi !by >seq0a,>seq0b,>seq0c


seqence  !wo st00,st01,st02,st03,st04
         !wo st05,st06,st07,st08,st09
         !wo st0a,st0b,st0c,st0d,st0e
         !wo st0f,st10,st11,st12,st13
         !wo st14,st15,st16,st17,st18
         !wo st19,st1a,st1b,st1c,st1d
         !wo st1e,st1f,st20,st21,st22
         !wo st23,st24,st25,st26,st27
         !wo st28,st29,st2a,st2b,st2c


;---------------------------------------
wavearp
         !by $20,$20,$20,$20
;---------------------------------------
pulsearp
         !by $00,$00,$00,$00
         !by $00,$00,$00,$00
;---------------------------------------
drumtabel
         !wo dwa0,dto0
         !wo dwa1,dto1
         !wo dwa2,dto2
         !wo dwa3,dto3
         !wo dwa4,dto4
         !wo dwa5,dto5
         !wo dwa6,dto6
         !wo dwa7,dto7
         !wo dwa8,dto8

dwa1
         !by $0b ;_ lengte
         !by $81,$41,$11,$80
         !by $80,$80,$80,$80
         !by $80,$80,$80,$80
dto1
         !by $60,$0c,$0d,$70
         !by $70,$80,$80,$70
         !by $70,$80,$80,$70
dwa0
         !by $07
         !by $81,$11,$40,$80
         !by $10,$10,$10,$10
dto0
         !by $30,$1c,$20,$30
         !by $00,$00,$00,$00

dwa2
         !by $0b
         !by $81,$41,$40,$40
         !by $40,$40,$40,$40
         !by $40,$40,$40,$40
dto2
         !by $50,$0a,$08,$06
         !by $05,$04,$03,$02
         !by $02,$01,$00,$00
dwa3
dto3
dwa4
dto4
dto5
dwa5
dto6
dwa6
dwa7
dto7
dwa8
dto8


;---------------------------------------
arplo    !by <arp0,<arp1,<arp2
         !by <arp3,<arp4,<arp5
         !by <arp6
         !by <arp7
         !by <arp8
         !by <arp9

arphi    !by >arp0,>arp1,>arp2
         !by >arp3,>arp4,>arp5
         !by >arp6
         !by >arp7
         !by >arp8
         !by >arp9

arp0     !by $02,$00,$03,$07
arp1     !by $02,$00,$04,$07
arp2     !by $02,$00,$05,$08
arp3     !by $02,$00,$05,$09
arp4     !by $02,$00,$03,$08
arp5     !by $02,$00,$04,$09
arp6     !by $02,$00,$03,$09
arp7     !by $02,$00,$06,$09
arp8     !by $02,$00,$02,$05
arp9

;---------------------------------------


filterbytes !wo fb0,fb1,fb2


fp0      = $10+volume
           ;^
 ;fil nibb@@=
fb0
         !by $90,$f4,$f4,$f4,$30
         !by fp0,$02,$04,$06,$08

fp1      = $20+volume
           ;^
 ;fil nibb@@=
fb1
         !by $20,$01,$01,$01,$f0
         !by fp1,$02,$9e,$9f,$c0

fp2      = $10+volume
           ;^
 ;fil nibb@@=
fb2
         !by $c0,$fc,$fc,$fc,$40
         !by fp2,$02,$04,$08,$20


;---------------------------------------
filtermega
strafilter
         !by $00,$10,$a0,$01
aftrekspeed !by $00

;---------------------------------------
pulsetabel

         !by $07,$0d,$03,$70
         !by $06,$60,$09,$50   ;1

         !by $08,$0e,$07,$f0
         !by $08,$50,$09,$50   ;2

         !by $08,$0e,$7d,$10
         !by $7e,$10,$7f,$10   ;3

         !by $07,$09,$7d,$60
         !by $7e,$60,$7f,$60   ;4

         !by $07,$0d,$03,$30
         !by $06,$40,$09,$50   ;5

         !by $09,$0e,$06,$90
         !by $09,$80,$0c,$70   ;6


;---------------------------------------

pulsehi  = *+0
waveform = *+1
attdec   = *+2
susrel   = *+3
filcount = *+4
fx1      = *+5
fx2      = *+6
fx3      = *+7

;niks
         !by $00,$00,$00,$00
         !by $00,$00,$00,$00 ;*set 0*
;bassdrum
         !by $08,$11,$07,$07
         !by $f0,$02,$00,$10 ;*set 1*
;snaredrum
         !by $08,$11,$09,$08
         !by $f0,$01,$00,$10 ;*set 2*
;51-kort
         !by $06,$51,$00,$a9
         !by $f0,$53,$74,$00 ;*set 3*
;bass
         !by $05,$41,$00,$ab
         !by $00,$53,$69,$00 ;*set 4*
;arp
         !by $15,$41,$00,$69
         !by $10,$00,$41,$84 ;*set 5*
;arp-$08
         !by $15,$41,$00,$4a
         !by $08,$00,$41,$84 ;*set 6*
;bass-release
         !by $04,$41,$00,$aa
         !by $f1,$53,$51,$01 ;*set 7*
;loopbass
         !by $02,$41,$00,$89
         !by $00,$53,$65,$01 ;*set 8*
;17-tik
         !by $00,$17,$00,$89
         !by $f0,$00,$00,$00 ;*set 9*
;lead
         !by $03,$41,$00,$77
         !by $00,$53,$12,$80 ;*set a*
;leadlong
         !by $05,$41,$0a,$05
         !by $00,$53,$88,$00 ;*set b*
;lead-heavy
         !by $04,$41,$00,$7a
         !by $00,$53,$12,$00 ;*set c*
;le-he-rel
         !by $04,$41,$00,$79
         !by $58,$42,$12,$00 ;*set d*
;bass-rel
         !by $02,$41,$00,$aa
         !by $52,$42,$82,$01 ;*set e*
;bass met fil
         !by $02,$41,$00,$a9
         !by $02,$42,$82,$01 ;*set f*
;leadlong
         !by $0b,$41,$00,$67
         !by $00,$53,$13,$00 ;*set10*
;lead-heavy(2)
         !by $04,$41,$00,$7a
         !by $08,$53,$12,$00 ;*set11*
;le-he-rel(2)
         !by $04,$41,$00,$79
         !by $50,$42,$12,$00 ;*set12*
;21-ding
         !by $00,$21,$08,$8c
         !by $00,$53,$00,$80 ;*set13*
;loopbass+vib
         !by $02,$41,$00,$b7
         !by $00,$41,$65,$01 ;*set14*
;mel-arp
         !by $05,$41,$00,$57
         !by $00,$00,$31,$84 ;*set15*
;11-tok
         !by $00,$11,$05,$05
         !by $00,$00,$00,$80 ;*set16*
;big-git
         !by $07,$41,$00,$a6
         !by $00,$42,$56,$80 ;*set17*
;big-git2
         !by $07,$41,$00,$a6
         !by $00,$41,$56,$80 ;*set18*

vibwaittab
         !by $00,$00,$00,$00 ;00-03
         !by $0c,$0c,$00,$0c ;04-07
         !by $00,$00,$0c,$00 ;08-0b
         !by $0c,$12,$00,$08 ;0c-0f
         !by $0c,$0c,$12,$16 ;10-13
         !by $00,$00,$00,$0c ;14-17
         !by $1c,$00,$00,$00 ;18-1b
startlen
         !by $02,$01,$02,$02;00-03
         !by $02,$02,$02,$02 ;04-07
         !by $02,$02,$16,$02 ;08-0b
         !by $02,$02,$02,$02 ;0c-0f
         !by $02,$02,$02,$02 ;10-13
         !by $02,$02,$02,$0a ;14-17
         !by $0a,$02,$02,$02 ;18-1b
starttabel
         !by $81,$81,$81,$81 ;00-03
         !by $81,$11,$11,$81 ;04-07
         !by $81,$81,$43,$81 ;08-0b
         !by $81,$81,$81,$81 ;0c-0f
         !by $81,$81,$81,$11 ;10-13
         !by $81,$11,$81,$43 ;14-17
         !by $43,$81,$81,$81 ;18-1b

;---------------sequences---------------

;        -=>    get ready    <=-

seq0a    !by $80,$03,$ff

seq0b    !by $80,$01,$8c,$01,$04,$fe

seq0c    !by $80,$02,$8c,$02,$94,$04
         !by $fe

;-----------------steps-----------------

st00     !by $c0,$a0,c4,$ff

st01     !by $d0,$88,c2,c3,g3
         !by $ff

st02     !by $c0,$84,g4,$d0,$88
         !by g2,f3,$84,c4,$ff

st03     !by $f1,%11110001
         !by $c8,$8c,c2,e2,f2,g2
         !by $a0,c2,$ff

st04     !by $d0,$a0,e3,$ff

;--------------sequences----------------

;      -=> level completed <=-

seq1a    !by $80,$41,$05,$08,$09,$ff

seq1b    !by $80,$45,$06,$0a,$ff

seq1c    !by $8c,$07,$fe

;---------------steps-------------------

st05     !by $f1,%11110001
         !by $c8,$84,c2,c2,c2,c3,c2
         !by $88,c2,$84,c2,c2,c3,c2
         !by c2,$ff

st06     !by $c1,$84,c2
         !by $d6,c4,$c1,c2
         !by $c2,c2
         !by $d6,c5,$c1,c5
         !by $ff


st07     !by $d0,$98,c3,$90,g3,$84,g2
         !by g2
         !by $88,f3,e3,c3,$90,c3
         !by $84,af2,c3,$90
         !by af2,$84,af2,af2,$88
         !by a2,g2,f2,$a8,g2,$ff

st08     !by $f1,%11110001
         !by $c8,$84,df2,df2,df2
         !by df3,df2
         !by $88,f2,$84,f2,f2,f3,f2
         !by f2,$ff

st09     !by $f1,%11110001
         !by $c8,$84,c2,c2,c2,c3,c2
         !by $b0,c2,$ff

st0a     !by $c1,$84,c2
         !by $c5,$71,c5,c5,c5
         !by c5,c5,c5
         !by $c1,c2,c2,$c2,c2,$ff

;---------------sequences---------------

;        -=>  hall of fame  <=-

seq2a    !by $80,$0b
         !by $80,$0e,$ff
seq2b    !by $80,$0c
         !by $80,$4d,$0f,$00,$ff
seq2c    !by $80,$0d
         !by $80,$10,$00,$ff
;-----------------steps-----------------

st0b     !by $f1,%11110001
         !by $c8,$88,c2
         !by $c2,c2
         !by $c8,$84,c3,g2
         !by $c2,$84,c2
         !by $c8,$8c,e2,$88
         !by $c2,c2
         !by $c8,$84,e3,b2
         !by $c2,$84,e2
         !by $c8,$8c,f2,$88
         !by $c2,c2
         !by $c8,$84,f3,c3
         !by $c2,$84,f2
         !by $c8,$8c,g2,$88
         !by $c2,c2
         !by $c8,$84,g3,d3
         !by $c2,$84,g2
         !by $c8,$8c,c2,$88
         !by $c2,c2
         !by $c8,$84,c3,g2
         !by $c2,$84,c2
         !by $c8,$8c,e2,$88
         !by $c2,c2
         !by $c8,$84,e3,b2
         !by $c2,$84,e2
         !by $c8,$8c,f2,$88
         !by $c2,c2
         !by $c8,$84,f3,c3
         !by $c2,$84,f2
         !by $c8,$8c,ff2,$88
         !by $c2,c2
         !by $c8,$84,ff3,df3
         !by $c2,$84,ff2
         !by $c8,$8c,g2,$88
         !by $c2,c2
         !by $c8,$84,g3,d3
         !by $c2,$84,g2
         !by $c8,$8c,a2,$88
         !by $c2,c2
         !by $c8,$84,a3,e3
         !by $c2,$84,a2
         !by $c8,$8c,f2,$88
         !by $c2,c2
         !by $c8,$84,g2,g3
         !by $c2,$84,g2
         !by $c8,g2
         !by $88,c2
         !by $c2,c2
         !by $c8,$84
         !by c3,g2,$88
         !by $c2,c2
         !by $ff

st0c     !by $c1,$88,c2,$84
         !by $c5,$71,c4,c4
         !by $c1,$88,c2
         !by $c5,$71,c4
         !by $c1,$88,c2,$84
         !by $c5,$72,b3,b3
         !by $c1,$88,c2
         !by $c5,$72,b3
         !by $c1,$88,c2,$84
         !by $c5,$73,c4,c4
         !by $c1,$88,c2
         !by $c5,$73,c4
         !by $c1,$88,c2,$84
         !by $c5,$73,d4,d4
         !by $c1,$88,c2
         !by $c5,$73,d4
         !by $c1,$88,c2,$84
         !by $c5,$71,c4,c4
         !by $c1,$88,c2
         !by $c5,$71,c4
         !by $c1,$88,c2,$84
         !by $c5,$72,b3,b3
         !by $c1,$88,c2
         !by $c5,$72,b3
         !by $c1,$88,c2,$84
         !by $c5,$73,c4,c4
         !by $c1,$88,c2
         !by $c5,$73,c4
         !by $c1,$88,c2,$84
         !by $c5,$77,c4,c4
         !by $c1,$88,c2
         !by $c5,$77,c4
         !by $c1,$88,c2,$84
         !by $c5,$73,d4,d4
         !by $c1,$88,c2
         !by $c5,$73,d4
         !by $c1,$88,c2,$84
         !by $c5,$72,e4,e4
         !by $c1,$88,c2
         !by $c5,$72,e4
         !by $c1,$88,c2,$84
         !by $c5,$73,c4,c4
         !by $c1,$88,c2
         !by $c5,$73,d4
         !by $c1,$88,c2,$84
         !by $c5,$71,c4,c4
         !by $c1,$88,c2
         !by $c5,$71,c4
         !by $ff

st0d     !by $cb,$98,e5,$84,c5,$98
         !by g4,$84,g4,a4,c5,$88,d5
         !by d5,c5,$84,d5,c5,$81,df5
         !by $83,e5,$84,d5,c5,$94,d5
         !by $81,d5,df5
         !by $96,e5,$84,c5,$98
         !by g4,$84,g4,a4,c5,$88,d5
         !by d5,c5,$84,d5,c5,$81,df5
         !by $83,e5,$84,d5,c5,$88,d5
         !by $84,c5,d5,c5,$81
         !by d5,df5,$82,e5,$84,g5,$81
         !by d5,df5,$82,e5,$84,g5,$81
         !by d5,df5,$82,e5,$84,g5,$81
         !by d5,df5,$82,e5,$84,g5
         !by $84,a5,$88,g5,e5,$84
         !by d5,c5,a4,$88,d5,d5,c5
         !by $84,a4,$a0,c5
         !by $c2,$84,c2,$ff

st0e     !by $f1,%11110001
         !by $c8,$84
         !by f2,f2,f3,f2,f2,f3
         !by f2,f3
         !by g2,g2,g3,g2,g2,g3
         !by g2,g3
         !by f2,f2,f3,f2,f2,f3
         !by f2,f3
         !by g2,g2,g3,g2,g2,g3
         !by g2,g3
         !by gf2,gf2,gf3,gf2,gf2,gf3
         !by gf2,gf3
         !by af2,af2,af3,af2,af2,af3
         !by af2,af3
         !by c2,c2,c3,c2,c2,c3
         !by c2,c3
         !by $86,c2,e2,$84,g2,$86,c3
         !by g2,$84,e2,$ff

st0f     !by $c1,$84,c2,$82
         !by $d8,c4,c5
         !by $c2,$84,c2,$82
         !by $d8,c4,c5,$ff

st10     !by $d0,$8c,a3,$84,a3
         !by $86,a3,b3,$84,c4,$88,d4
         !by $84,c4,b3,g3,e3,g3
         !by $90,a3,$84,a3,$88,e4,$84
         !by d4,e4,d4,c4,b3,$94,g3
         !by $8c,c4,$82,c4,c4,$86,c4
         !by af3,$84,gf3,$8c
         !by d4,$82,d4,d4,$86,d4
         !by c4,$84,d4,$a0,e4
         !by $ff

;---------------sequences---------------

;        -=>   game over   <=-

seq3a    !by $81,$11,$fe

seq3b    !by $81,$12,$fe

seq3c    !by $81,$13,$ff

;----------------steps------------------

st11     !by $f1,%11110001
         !by $c8,$8c,c3,$84,g2
         !by $8c,gf2,$84,df2
         !by $8c,f2
         !by $84,df2
         !by $88,g2,g1
         !by $a0,c2,$ff

st12     !by $c5,$75
         !by $88,df4
         !by $74,d4
         !by $75,df4
         !by $74,d4,$84
         !by $75,df4
         !by $74,d4
         !by $75,df4
         !by $74,d4,$88
         !by $74,g4
         !by $73,f4
         !by $a0,$75
         !by df4,$ff

st13     !by $c9
         !by $88,c5
         !by c5
         !by c5
         !by c5,$84
         !by c5
         !by c5
         !by c5
         !by c5,$88
         !by c5
         !by c5
         !by $a0
         !by c5,$ff

;------------sequences------------------

;       -=> in-game tune 1 <=-

seq4a    !by $81,$43,$15,$8d,$43,$15
         !by $81,$47,$17,$ff

seq4b    !by $81,$4f,$18
         !by $4f,$14,$14,$14,$ff

seq4c    !by $81,$42,$16,$19,$ff

;-------------steps---------------------

st14     !by $c1,$82,c2
         !by $d6,c4,c5,c4
         !by $c2,$c2
         !by $d6,c5,c4,c5,$ff
st18     !by $c1,$82,c2
         !by $d6,c4,c5,c4
         !by $c1,$c2
         !by $d6,c5,c4,c5,$ff

st15     !by $f1,%11110001
         !by $c8,$a0,c2
         !by $ff

st16     !by $c5,$70,$84
         !by c5,c5,$86
         !by $78,c5
         !by $70,$84
         !by c5,c5,$82,c5
         !by $78,$84
         !by c5,$82,c5,c5
         !by $70,$84,c5,c5,$86
         !by $78,c5,$84
         !by $74,c5,c5,$82,c5,$84
         !by $71,af4,$82,af4
         !by af4,$ff

st17     !by $f1,%11110001
         !by $c8,$84,c2,c2,$86
         !by af1,$84,c2,c2,$82,c2
         !by $84,af1,$82,c2,af1
         !by $84,gf1,gf1,$86,gf1
         !by $84,af1,$82,af1,$84
         !by g1,af1,b1,$ff

st19     !by $c5,$70,$84
         !by c5,c5,$86
         !by $78,c5
         !by $70,$84
         !by c5,c5,$82,c5
         !by $78,$84
         !by c5,$82,c5,c5
         !by $70,$84,c5,c5,$86
         !by $78,c5,$84
         !by $75,df5,df5,$82,df5,$84
         !by $74,d5,$82,d5
         !by d5,$ff
;-------------Start-Sequences-----------

seq5a    !by $80,$1a,$82,$1a,$ff

seq5b    !by $80,$47,$1e,$82,$1b,$1b
         !by $80,$43,$20,$82,$43,$20
         !by $ff

seq5c    !by $80,$1c,$82,$1c,$80
         !by $1d,$82,$1d,$ff



;----------------Start-Steps------------


st1a     !by $f1,%11110001
         !by $c8,$84,d2,d2,d3,$82,d2
         !by $84,c2,c2,$82,c2,$84,c3
         !by $82,c2,c2
         !by $c8,$84,g1,g1,g2
         !by $82,g1
         !by $84,g1,g1,$82,a1,$84,a2
         !by $82,a1,c2
         !by $c8,$84,d2,d2,d3,$82,d2
         !by $84,c2,c2,$82,c2,$84,c3
         !by $82,c2,c2
         !by $c8,$84,af1,af1,af2
         !by $82,af1,$84
         !by $c8,a2,$c8,$82,g2,a2,g2
         !by f2,d2,a1,c2
         !by $c8,$84,d2,d2,d3,$82,d2
         !by $84,c2,c2,$82,c2,$84,c3
         !by $82,c2,c2
         !by $c8,$84,g1,g1,g2
         !by $82,g1
         !by $84,g1,g1,$82,a1,$84,a2
         !by $82,a1,c2
         !by $c8,$84,d2,d2,d3,$82,d2
         !by $84,c2,c2,$82,c2,$84,c3
         !by $82,c2,c2
         !by $c8,$84,af2,af2,af3
         !by $82,af2,$84
         !by $c8,a2,$c8,$82,g2,a2,g2
         !by f2,d2,a1,c2,$ff

st1b     !by $c1,$84,c2,$82,$d6,d4,d5
         !by $c2,$84,c2,$82,$c1,c2,$d6
         !by d4,d5
         !by d4,d5,d4,$c2,$84,c2,$82
         !by $d6,d4,d5
         !by $c1,$84,c2,$82,$d6,d4,d5
         !by $c2,$84,c2,$82,$c1,c2,$d6
         !by d4,d5
         !by d4,d5,d4,$c2,$84,c2,$82
         !by $d6,d5,$c2,c2
         !by $c1,$84,c2,$82,$d6,d4,d5
         !by $c2,$84,c2,$82,$c1,c2,$d6
         !by d4,d5
         !by d4,d5,d4,$c2,$84,c2,$82
         !by $d6,d4,d5
         !by $c1,$84,c2,$82,$d6,d4,d5
         !by $c2,$84,c2,$82,$c1,c2,$d6
         !by d4,d5
         !by d4,d5,d4,$c2,$84,c2,$82
         !by c2,c2,$ff


st1c     !by $c5,$72,$84,a4,a4,a4,$82
         !by a4,$73,$8e,g4,$84,g4,$82
         !by g4,$84,f4,$8a,f4,$88,f4
         !by $88,g4
         !by $c5,$72,$84,a4,a4,a4,$82
         !by a4,$73,$8e,g4,$71,$88,f4
         !by $73,$84,f4,$88,f4,$90,f4
         !by $c5,$72,$84,a4,a4,a4,$82
         !by a4,$73,$8e,g4,$84,g4,$82
         !by g4,$84,f4,$8a,f4,$88,f4
         !by $88,g4
         !by $c5,$72,$84,a4,a4,a4,$88
         !by $71,c5,$86,af4,$73,g4
         !by $90,f4,$c9,$82,f5,d5,af4
         !by d5,af4,f4,af4,f4
         !by $ff


st1d     !by $d0,$88,d4,$84,a4,$88
         !by g4,f4,e4,$84,c4,a3,$88,d4
         !by $84,d4,d4,e4,f4,e4,d4
         !by $88,e4,c4,a3,$84,c4,d4
         !by $94,d4
         !by $d0,$88,d4,$84,a4,$88
         !by g4,c5,a4,$84,c5,d5,$88,d5
         !by $84,d5,d5,e5,f5,g5,a5
         !by $88,g5,$86,f5,e5,$a0,d5
         !by $ff


st1e     !by $c1,$84,c2,$82,$d6,d4,d5
         !by $c1,$84,c2,$82,$c1,c2,$d6
         !by d4,d5
         !by d4,d5,d4,$c1,$84,c2,$82
         !by $d6,d4,d5,$ff

st1f

st20     !by $c1,$84,c1
         !by $c5,$72,$82,a4,a4
         !by $c2,$84,c1,$c1,$82,c1
         !by $c5,$73,$84,g4,$82,g4,g4
         !by g4
         !by $c2,$84,c1
         !by $c5,$73,$82,g4,g4
         !by $c1,$84,c1
         !by $c5,$73,$82,f4,f4
         !by $c2,$84,c1,$c1,$82,c1
         !by $c5,$73,$84,f4,$82,f4,f4
         !by f4
         !by $c2,$84,c1
         !by $c5,$73,$82,g4,g4
         !by $ff

;------------sequences------------------

;        -=>  title  tune  <=-

seq6a    !by $80,$4f,$21,$4f,$25,$ff

seq6b    !by $80,$4f,$23,$4f,$22
         !by $4f,$24,$4f,$24,$ff

seq6c    !by $80,$00,$ff

;---------------------------------------

st21     !by $f1,%11110001
         !by $c8,$84,c2,c2,c2,c2,c2
         !by c2,c2,c2,c2,c2,c2,c2
         !by $ff

st22     !by $c1,$88,c2,$84,c2
         !by $c1,$88,c2,$c1,$84
         !by c2,$ff


st23     !by $c0,$8c,c2,c2,$ff

st24     !by $c1,$88,c2,$84,c2
         !by $c2,$88,c2,$c1,$84
         !by c2,$ff

st25     !by $f1,%11110001
         !by $c8,$84
         !by c2,c2,c2,c3,c2,c2,c2,c3
         !by c2,c3,c2,c2,$ff
st26
st27
st28
st29
st2a
st2b
st2c

}