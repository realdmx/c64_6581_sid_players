
;	The classic Commando Title music by Rob Hubbard

	!to "Hubbard_Rob_Commando_Title.sid",plain

ADDR    = $1000

	* = 0 ; psid header
	
	!text "PSID"
	!be16 2,$7c
	!be16 0 ; load addr = auto
	!be16 init
	!be16 play
	!be16 1,1 ; num songs, start song
	!be32 0 ; song speeds
- 	!text "Commando (Title)"
	!fill 32-(*--),0
- 	!text "Rob Hubbard"
	!fill 32-(*--),0
- 	!text "1985 Elite"
	!fill 32-(*--),0
	!be16 0,0,0 ; flags reloc v3+v4
	!wo ADDR ; load address

!pseudopc ADDR {

TRACK   = $f0
;       = $f1
PATT    = $f2
;       = $f3

        * = ADDR

init    jmp _init
play    jmp _play

_play   inc frc
        bit status
        bmi musicoff
        bvc musicon
        lda #0                  ; init
        sta frc
        ldx #2
-       sta posoff,x
        sta patoff,x
        sta notec,x
        sta note,x
        dex
        bpl -
        sta status
        jmp musicon

musicoff
        bvc +
        lda #0                  ; music off
        sta $d404               ; v1ctl
        sta $d40b               ; v2ctl
        sta $d412               ; v3ctl
        lda #$f
        sta $d418               ; sidmode
        lda #$80
        sta status
+       rts

musicon ldx #2
        dec tempoc
        bpl mainloop
        lda tempo
        sta tempoc
mainloop
        lda sid,x
        sta sidoff
        tay
        lda tempoc
        cmp tempo
        bne +
        lda trackph,x
        sta TRACK
        lda trackpl,x
        sta TRACK + 1
        dec notec,x
        bmi steptrack
        jmp soundup

+       jmp vibrato

steptrack
        ldy posoff,x
        lda (TRACK),y
        cmp #$ff                ; ff = restart
        beq restart
        cmp #$fe
        bne getnote
		lda #$c0                ; fe = stop
		sta status
        rts

restart lda #0
        sta notec,x
        sta posoff,x
        sta patoff,x
        jmp steptrack

getnote tay
        lda patpl,y
        sta PATT
        lda patph,y
        sta PATT + 1
        lda #0
        sta pbend,x             ; pitchbend off
        ldy patoff,x
        lda #$ff                ; gate bit set
        sta gate

        lda (PATT),y
        sta notelen,x
        sta t3
        and #$1f
        sta notec,x
        bit t3
        bvs legato              ; bit 6 = legato / tie note
        inc patoff,x
        lda t3
        bpl getfreq
        iny
        lda (PATT),y            ; instr or pitch bend
        bpl +
        sta pbend,x
        jmp ++

+       sta instr,x

++      inc patoff,x

getfreq iny
        lda (PATT),y            ; note
        sta note,x
        asl
        tay
        lda freqsl,y
        sta t2
        lda freqsh,y
        ldy sidoff
        sta $d401,y             ; fqh
        sta nfqh,x
        lda t2
        sta $d400,y             ; fql
        sta nfql,x
        jmp getinstr

legato  dec gate                ; gate = $fe (gate off)

getinstr
        ldy sidoff
        lda instr,x        
        stx t1
        asl
        asl
        asl
        tax
        lda ins_ctrl,x          ; ctl
        sta tmpctrl
        lda ins_ctrl,x
        and gate
        sta $d404,y             ; ctl
        lda ins_pwl,x
        sta $d402,y             ; pwl
        lda ins_pwh,x
        sta $d403,y             ; pwh
        lda ins_ad,x
        sta $d405,y             ; ad
        lda ins_sr,x
        sta $d406,y             ; sr
+       ldx t1
        lda tmpctrl
        sta ctrl,x
        inc patoff,x
        ldy patoff,x
        lda (PATT),y
        cmp #$ff                ; end of pattern?
        bne +
        lda #0                  ; back to top
        sta patoff,x
        inc posoff,x
+       jmp next

soundup ldy sidoff
        lda notelen,x
        and #$20                ; bit 5 = sustain
        bne vibrato
        lda notec,x
        bne vibrato

        lda ctrl,x              ; hard reset
        and #$fe                ; gate off
        sta $d404,y             ; ctl
        lda #0
        sta $d405,y             ; ad
        sta $d406,y             ; sr

vibrato lda instr,x
        asl
        asl
        asl
        tay
        sty instroff
        lda ins_fx,y
        sta fx
        lda ins_pulse,y
        sta pulse
        lda ins_vib,y
        sta vib
        beq pwlmod

        lda frc                 ; vibrato
        and #7                  ; 0...1...2...3
        cmp #4
        bcc +
        eor #7                  ; 3...2...1...0
+       sta vosc
        lda note,x
        asl
        tay
        sec
        lda freqsl_2,y
        sbc freqsl,y
        sta vdifl
        lda freqsh_2,y
        sbc freqsh,y
-       lsr
        ror vdifl
        dec vib
        bpl -
        sta vdifh
        lda freqsl,y
        sta vfql
        lda freqsh,y
        sta vfqh
        lda notelen,x
        and #$1f
        cmp #6                  ; if notelen < 6 no vib
        bcc +

        ldy vosc
-       dey
        bmi +
        clc
        lda vfql
        adc vdifl
        sta vfql
        lda vfqh
        adc vdifh
        sta vfqh
        jmp -

+       ldy sidoff
        lda vfql
        sta $d400,y             ; v1fql
        lda vfqh
        sta $d401,y             ; v1fqh
        
pwlmod  lda fx
        and #8                  ; bit 3 = pwl modulate
        beq pulseup

        ldy instroff
        lda ins_pwl,y
        adc pulse
        sta ins_pwl,y
        ldy sidoff
        sta $d402,y             ; v1pwl
        jmp pitchbend

pulseup lda pulse
        beq pitchbend

        ldy instroff
        and #$1f
        dec pdelay,x
        bpl pitchbend

        sta pdelay,x
        lda pulse
        and #$e0
        sta prate
        lda pdir,x              ; which direction
        bne +
        lda prate
        clc
        adc ins_pwl,y
        pha
        lda ins_pwh,y
        adc #0
        and #$0f
        pha
        cmp #$0e                ; max $0e00
        bne pup
        inc pdir,x
        jmp pup

+       sec                     ; down
        lda ins_pwl,y
        sbc prate
        pha
        lda ins_pwh,y
        sbc #0
        and #$0f
        pha
        cmp #8
        bne pup
        dec pdir,x
pup     stx t1                ; up
        ldx sidoff
        pla
        sta ins_pwh,y
        sta $d403,x             ; pwh
        pla
        sta ins_pwl,y
        sta $d402,x             ; pwl
        ldx t1

pitchbend
        ldy sidoff
        lda pbend,x
        beq drum
        
        and #$7e
        sta t1
        lda pbend,x
        and #1                  ; bit 1 = bend up
        beq pbup    
        sec
        lda nfql,x
        sbc t1
        sta nfql,x
        sta $d400,y             ; fql
        lda nfqh,x
        sbc #0
        sta nfqh,x
        sta $d401,y             ; fqh
        jmp drum

pbup    clc
        lda nfql,x
        adc t1
        sta nfql,x
        sta $d400,y             ; fql
        lda nfqh,x
        adc #0
        sta nfqh,x
        sta $d401,y             ; fqh

drum    lda fx
        and #1                  ; fx bit 1 = drum
        beq skydive

        lda nfqh,x
        beq skydive
        lda notec,x
        beq skydive

        lda notelen,x
        and #$1f
        sec
        sbc #1
        cmp notec,x             ; first frame?
        ldy sidoff
        bcc +
        lda nfqh,x
        dec nfqh,x
        sta $d401,y             ; fqh
        lda ctrl,x
        and #$fe                ; gate off
        bne ++
+       lda nfqh,x
        sta $d401,y             ; fqh
        lda #$80                ; first frame = noise
++      sta $d404,y             ; ctl

skydive lda fx
        and #2                  ; fx bit 2 = skydive
        beq arpeggio

        lda notelen,x           ; a signature hubbard sound
        and #$1f
        cmp #3
        bcc arpeggio

        lda frc
        and #1
        beq arpeggio
        lda nfqh,x
        beq arpeggio
        inc nfqh,x
        inc nfqh,x
        ldy sidoff
        sta $d401,y             ; fqh

arpeggio
        lda fx
        and #4                  ; fx bit 3 = arp
        beq next

        lda frc
        and #1
        beq +
        lda note,x
        clc
        adc #12                 ; note += 12
        jmp ++

+       lda note,x
++      asl
        tay
        lda freqsl,y
        sta t2
        lda freqsh,y
        ldy sidoff
        sta $d401,y             ; fqh
        lda t2
        sta $d400,y             ; fql

next    dex
        bmi +
        jmp mainloop
+       rts

        ; a = song

_init   ldy #0
        tax
        lda songtempos,x
        sta tempo
        txa
        asl
        sta t1
        asl
        clc
        adc t1
        tax
-       lda songptrs,x
        sta trackph,y
        inx
        iny
        cpy #6
        bne -
        lda #0
        sta $d404               ; v1ctl
        sta $d40b               ; v2ctl
        sta $d412               ; v3ctl
        lda #$f
        sta $d418               ; sidmode
        lda #%01000000          ; bit 6 = music ready
        sta status
        rts

freqsh = * + 1
freqsl = *
freqsh_2 = * + 3
freqsl_2 = * + 4
        !wo $0116,$0127,$0138,$014b,$015f,$0173,$018a,$01a1,$01ba,$01d4,$01f0,$020e ; c0-h0
        !wo $022d,$024e,$0271,$0296,$02bd,$02e7,$0313,$0342,$0374,$03a9,$03e0,$041b ; c1-h1
        !wo $045a,$049b,$04e2,$052c,$057b,$05ce,$0627,$0685,$06e8,$0751,$07c1,$0837 ; c2-h2
        !wo $08b4,$0937,$09c4,$0a57,$0af5,$0b9c,$0c4e,$0d09,$0dd0,$0ea3,$0f82,$106e ; c3-h3
        !wo $1168,$126e,$1388,$14af,$15eb,$1739,$189c,$1a13,$1ba1,$1d46,$1f04,$20dc ; c4-h4
        !wo $22d0,$24dc,$2710,$295e,$2bd6,$2e72,$3138,$3426,$3742,$3a8c,$3e08,$41b8 ; c5-h5
        !wo $45a0,$49b8,$4e20,$52bc,$57ac,$5ce4,$6270,$684c,$6e84,$7518,$7c10,$8370 ; c6-h6
        !wo $8b40,$9370,$9c40,$a578,$af58,$b9c8,$c4e0,$d098,$dd08,$ea30,$f820,$fd2e ; c7-h7

posoff  	!h 00 00 00     ; 14a2
patoff	    !h 00 00 00     ; 14a5
notec   	!h 00 00 00     ; 14a8
notelen 	!h 00 00 00     ; 14ab
ctrl    	!h 00 00 00     ; 14ae
note    	!h 00 00 00     ; 14b1
instr   	!h 00 00 00     ; 14b4
pbend       !h 00 00 00
pdelay      !h 00 00 00
pdir        !h 00 00 00
nfqh        !h 00 00 00
nfql        !h 00 00 00
trackph     !h 00 00 00
trackpl     !h 00 00 00
gate    	!h 00
vib         !h 00
pulse       !h 00
status      !h 00
fx          !h 00
t1  	    !h 00
t2  	    !h 00
t3  	    !h 00
tmpctrl 	!h 00
vdifl       !h 00
vdifh       !h 00
vfql        !h 00
vfqh        !h 00
vosc        !h 00
tempoc      !h 00
tempo       !h 00
instroff    !h 00
prate       !h 00
frc         !h 00
sidoff  	!h 00
sid     	!h 00 07 0e

; Commando Title by Rob Hubbard

        !source "defs.asm"

        * = $2000

_songdata

songptrs
        !by <song1t1,<song1t2,<song1t3
        !by >song1t1,>song1t2,>song1t3

songtempos
        !by 2               ; tempo for each song

patpl   !by <p00,<p01,<p02,<p03,<p04,<p05,<p06,<p07
        !by <p08,<p09,<p0a,<p0b,<p0c,<p0d,<p0e,<p0f
        !by <p10,<p11,<p12,<p13,<p14,<p15,<p16,<p17
        !by <p18,<p19,<p1a,<p1b,<p1c,<p1d,<p1e,<p1f

patph   !by >p00,>p01,>p02,>p03,>p04,>p05,>p06,>p07
        !by >p08,>p09,>p0a,>p0b,>p0c,>p0d,>p0e,>p0f
        !by >p10,>p11,>p12,>p13,>p14,>p15,>p16,>p17
        !by >p18,>p19,>p1a,>p1b,>p1c,>p1d,>p1e,>p1f

song1t1 !h 13 13 13 13 07 07 09 0c 0c 10 10 10 10 0f 0f 11
        !h 11 12 17 17 17 17 17 17 17 17 10 10 10 10 17 17
        !h 17 17 10 10 17 17 1a 1b 1c 1c 1c 1c 1d 1d 1d 1d
        !h 1e 1e 1e 1e 0f 17 17 1f 10 10 17 11 17 12 17 1f
        !h ff

song1t2 !h 08 08 08 0a 08 0a 08 08 08 13 13 14 14 14 14 15
        !h 15 16 16 18 18 18 18 18 18 18 18 13 13 18 18 18
        !h 18 13 18 18 13 13 13 13 13 13 13 13 13 13 14 14
        !h 14 14 14 14 18 18 1f 13 18 15 18 16 16 18 1f ff

        ; bass, drums
song1t3 !h 01 01 02 03
        !h 01 01 02 03
        !h 01 01 02 03
        !h 04 04 05 06
        !h 01 01 02 03
        !h 04 04 05 06
        !h 01 0b 03 01
        !h 01 01 02 03
        !h 01 01 02 03 01 01 0b 0b 0d 0d 0e 0e 0d 0d 0e 0e
        !h 0b 0b 0b 0b 03 03 03 03 19 19 19 19 19 19 19 19
        !h 01 01 0b 0b 19 19 19 19 01 0b 19 19 01 01 0b 0b
        !h 01 01 0b 0b 01 01 0b 0b 01 01 0b 0b 01 01 0b 0b
        !h 0d 0d 0e 0e 0d 0d 0e 0e 0d 0d 0e 0e 19 19 1f 01
        !h 0b 19 0b 0b 19 03 03 03 03 19 1f ff

p00     !h 5f ff

p07     !h 81 03 32 81 00 39 03 39 03 39 03 39 07 39 05 39
        !h 03 39 01 40 03 40 03 40 03 40 07 40 87 0c 2c 87
        !h 00 41 07 40 07 41 07 40 41 01 3b 03 3b 03 3b 03
        !h 3b 07 3b 87 0c 2c 81 03 32 81 00 3c 03 3c 03 3c
        !h 03 3c 07 3c 05 3c 03 3c 01 43 03 43 03 43 03 43
        !h 07 43 87 0c 2c 87 00 44 07 43 07 44 07 43 41 01
        !h 3e 03 3e 03 3e 03 3e 07 3e 83 0c 2f 01 2c 01 2c
        !h ff

p08     !h 81 04 68 01 68 01 68 01 68 83 01 34 03 34 05 35
        !h 05 34 03 32 81 04 68 01 68 01 68 01 68 83 01 34
        !h 03 34 07 34 47 81 04 68 01 68 01 68 01 68 83 01
        !h 34 03 34 05 35 05 34 03 32 41 81 01 34 03 34 03
        !h 34 03 34 07 34 47 ff

p0a     !h 81 04 68 01 68 01 68 01 68 83 01 37 03 37 05 38
        !h 05 37 03 35 81 04 68 01 68 01 68 01 68 83 01 37
        !h 03 37 07 37 47 81 04 68 01 68 01 68 01 68 83 01
        !h 37 03 37 05 38 05 37 03 35 41 81 01 37 03 37 03
        !h 37 03 37 07 37 47 ff

p09     !h 83 03 32 03 32 83 00 39 03 39 01 39 01 39 03 39
        !h 03 3b 03 3c 01 3e 01 3e 03 3e 03 3e 03 3e 07 3e
        !h 83 0c 2c 81 00 3e 01 40 01 41 01 41 03 40 03 3e
        !h 03 3c 03 3b 03 39 07 38 81 03 32 81 00 39 03 39
        !h 03 39 03 3b 07 39 87 0c 2c ff

p0c     !h 81 05 3c 03 3b 01 3a 03 39 01 3c 03 3b 01 3a 03
        !h 39 01 3c 03 3b 01 3a 03 39 01 3c 03 3b 01 3a 03
        !h 39 01 3c 03 3b 01 39 03 41 03 40 01 41 03 40 01
        !h 3f 03 3e 01 41 03 40 01 3f 03 3e 03 41 03 40 01
        !h 3b 03 3a 01 39 03 38 01 3b 03 3a 01 39 03 38 03
        !h 3c 03 3b ff

p0f     !h 8b 06 42 a3 cf 42 07 40 03 3d 03 3b a3 bf 3b 03
        !h 3a 03 3a 83 0c 2c 03 2c 81 06 3a 01 3a 03 3b 03
        !h 3d 0b 3d 03 3d 05 40 05 3d a3 a8 3b 07 3d 8f d1
        !h 3d 83 0c 2c 03 2c ff

p10     !h 81 06 3c 03 3b 01 3a 03 39 01 3c 03 3b 01 3a 03
        !h 39 03 3c 03 3e ff

p11     !h 87 06 3e 83 0c 2c 81 06 3e 01 3e 05 40 05 3e a3
        !h a8 3c 07 3e 83 0c 2f 0b 2c 03 2f 03 2c ff

p12     !h 87 06 40 83 0c 2c 81 06 40 01 40 05 42 05 40 a3
        !h a8 3e 07 40 83 0c 2f 0b 2c 03 2f 03 2c 87 06 40
        !h 83 0c 2c 81 06 40 01 40 05 42 05 40 a3 a8 3e 05
        !h 40 05 42 03 44 05 42 05 44 03 45 ff

p13     !h 83 07 58 03 51 83 01 39 03 39 05 39 05 39 05 37
        !h 01 39 03 39 03 39 03 37 01 39 01 37 03 39 83 07
        !h 58 03 51 ff

p14     !h 83 07 55 03 4e 83 01 31 03 31 05 31 05 31 05 2f
        !h 01 31 03 31 03 31 03 2f 01 31 01 2f 03 31 83 07
        !h 55 03 4e ff

p15     !h 83 07 5d 03 56 83 01 32 03 32 05 32 05 32 05 30
        !h 01 32 03 32 03 32 03 30 01 32 01 30 03 32 83 07
        !h 5d 03 56 ff

p16     !h 83 07 5f 03 58 83 01 34 03 34 05 34 05 34 05 32
        !h 01 34 03 34 03 34 03 32 01 34 01 32 03 34 83 07
        !h 5f 03 58 ff

p17     !h 81 05 46 01 46 01 46 01 46 01 46 01 46 01 44 01
        !h 46 01 46 01 44 03 46 01 46 01 46 01 44 01 44 ff

p18     !h 81 05 43 01 43 01 43 01 43 01 43 01 43 01 41 01
        !h 43 01 43 01 41 03 43 01 43 01 43 01 41 01 41 ff

p19     !h 81 05 27 01 27 01 27 01 27 83 0c 2c 81 05 25 03
        !h 27 01 25 01 27 01 27 83 0c 2f 03 2c ff

p1a     !h a7 06 37 a7 a8 37 17 39 03 37 03 39 03 3e 03 3c
        !h 07 39 27 3c a7 aa 3c 17 3e 03 3e 03 43 03 42 03
        !h 3e 07 39 27 37 a7 90 37 17 39 a7 a9 3f 03 3e 03
        !h 3c 07 39 27 3e a7 a9 3e 17 3c 03 3e 03 40 03 43
        !h 03 42 03 43 03 45 27 43 a7 b4 43 07 45 01 45 03
        !h 45 01 45 01 45 03 45 01 43 03 45 01 43 03 42 01
        !h 43 03 42 03 40 03 3e 01 3e 03 3e 01 3c 03 3e 01
        !h 3c 03 3b 01 3c 03 3b 03 39 03 37 01 39 03 39 01
        !h 37 03 39 01 3b 03 3c 01 3e 03 40 03 42 03 43 ff

p1b     !h 27 47 a7 b1 47 17 45 03 43 03 45 01 48 01 48 03
        !h 45 01 4a 01 4a 03 48 27 4c a7 d1 4c 1f 4a 41 01
        !h 4c 01 4c 01 40 01 4d 01 40 01 48 01 4a ff

p1c     !h 01 4c 01 4c 03 40 03 4a 01 40 03 48 01 40 03 47
        !h 01 48 01 40 01 48 01 4a ff

p1d     !h 01 4c 01 4c 03 40 03 4b 01 40 03 49 01 40 03 47
        !h 01 49 01 40 01 49 01 4b ff

p1e     !h 01 49 01 49 03 3d 03 47 01 3d 03 46 01 3d 03 44
        !h 01 42 01 3d 01 47 01 49 ff

p1f     !h 87 07 68 4f 83 0c 2c 03 2c ff

        ; bass

p01     +INSTR 2,5
        !by A1
        !by 1, A2
        +INSTR 3,3
        !by As3
        +INSTR 2,3
        !by A1
        !by 7, A1
        +INSTR 3,3
        !by As3
        +INSTR 2,1
        !by G2
        !by 1, A2
        !h ff

        ;!h 85 02 15 01 21 83 03 2e 83 02 15 07 15 83 03 2e
        ;!h 81 02 1f 01 21 ff

p02     !h 85 02 16 01 22 83 03 2e 83 02 16 07 16 83 03 2e
        !h 81 02 21 01 22 ff

p03     !h 85 02 10 01 1c 83 03 2e 83 02 10 07 10 83 03 2e
        !h 81 02 1a 01 1c ff

p04     !h 85 02 18 01 24 83 03 2e 83 02 18 07 18 83 03 2e
        !h 81 02 22 01 24 ff

p05     !h 85 02 19 01 25 83 03 2e 83 02 19 07 19 83 03 2e
        !h 81 02 24 01 25 ff

p06     !h 85 02 13 01 1f 83 03 2e 83 02 13 07 13 83 03 2e
        !h 81 02 1c 01 1c ff

p0b     !h 85 02 1a 01 26 83 03 2e 83 02 1a 07 1a 83 03 2e
        !h 81 02 24 01 26 ff

p0d     !h 85 02 12 01 1e 83 03 2e 83 02 12 07 12 83 03 2e
        !h 81 02 1c 01 1e ff

p0e     !h 85 02 19 01 25 83 03 2e 83 02 19 07 19 83 03 2e
        !h 81 02 23 01 25 ff

        ; instruments

ins_pwl = *
ins_pwh = * + 1
ins_ctrl = * + 2
ins_ad = * + 3
ins_sr = * + 4
ins_vib = * + 5
ins_pulse = * + 6
ins_fx = * + 7

        !h c0 0a 41 29 5f 02 e0 00
        !h 80 01 41 06 4b 00 00 05
        !h 52 01 41 09 9f 00 16 08  ; bass
        !h 00 02 81 0a 09 00 00 05  ; snare IARP | IDRUM
        !h 00 02 43 0f c4 00 00 03
        !h da 08 41 05 a9 00 02 0d
        !h e0 0a 41 38 7a 02 e0 00
        !h 80 01 15 0d fb 01 00 05
        !h b4 08 41 49 5b 02 03 08
        !h 00 08 21 04 6f 03 00 05
        !h 7f 03 41 09 6b 02 01 0d
        !h 00 02 43 07 09 01 00 01
        !h 00 08 41 09 0a 00 00 01

} ; end
