
	; Converted from TurboAssembler to ACME by dmx87

	!to "Deenen_Charles_SFX_Player.sid",plain

* = $0000

	!text "PSID"
	!be16 2							; version 2
	!be16 $7c						; data offset
	!be16 $1000						; Load (0 = auto)
	!be16 restart					; Init
	!be16 plloop					; Play
	!be16 6							; num songs
	!be16 1							; first song
	!word 0
	!word 0
-	!text "SFX Player"
	!fill 32 - (* - -)
-	!text "Charles Deenen"
	!fill 32 - (* - -)
-	!text "1989 Scoop"
	!fill 32 - (* - -)
	!word 0							; v2 flags
	!word 0							; Start page, page length (reloc)
	!word 0							; Reserved
		
!pseudopc $1000 {


;--------------------------------------;
; M.O.N. SFXplayer (C) Scoop ltd. 1989 ;
;--------------------------------------;
;
; Probeer toets "1" tot "6" eens!
;
; Als je bij een block ipv 'end',
; 'repeat' zet, dan blijft het block
; herhaald worden! (zoals je wou!)
;
; Effect VIJF gebruikt 'n nieuwe truukje
; waarmee de duur van 'n toon van buiten
; de routine geregeld kan worden, dmv 'n
; poke! (+/- regelt dan dit adres)
;
; Terwijl je effect VIJF laat lopen,
; (of is het draaien?) kun je effect #6
; zich tegelijkertijd laten manifesteren
; dan effect #5!! (truukje mijnerzijds!)
;
; Voorlopig is dit de FINAL version!


zp       = $fb
effectnr = 0

; statements:
end      = $00
arpend   = $00
wfend    = $08
tabend   = $ff
endsong  = $ff
dur      = $80
snd      = $e0
vol      = $ef
up       = $00
down     = $80
freq     = $80
arp      = $00
niks     = $00
pulse    = $00
filt     = $00
res      = $00
drump    = $e0
arpp     = $c0
pause    = $7c
filtoff  = $7d
filter   = $7e
glide    = $7f
repeat   = $7b
durcontrol = $7a

c0       = 1
cc0      = 2
d0       = 3
dd0      = 4
e0       = 5
f0       = 6
ff0      = 7
g0       = 8
gg0      = 9
a0       = 10
aa0      = 11
b0       = 12
c1       = 13
cc1      = 14
d1       = 15
dd1      = 16
e1       = 17
f1       = 18
ff1      = 19
g1       = 20
gg1      = 21
a1       = 22
aa1      = 23
b1       = 24
c2       = 25
cc2      = 26
d2       = 27
dd2      = 28
e2       = 29
f2       = 30
ff2      = 31
g2       = 32
gg2      = 33
a2       = 34
aa2      = 35
b2       = 36
c3       = 37
cc3      = 38
d3       = 39
dd3      = 40
e3       = 41
f3       = 42
ff3      = 43
g3       = 44
gg3      = 45
a3       = 46
aa3      = 47
b3       = 48
c4       = 49
cc4      = 50
d4       = 51
dd4      = 52
e4       = 53
f4       = 54
ff4      = 55
g4       = 56
gg4      = 57
a4       = 58
aa4      = 59
b4       = 60
c5       = 61
cc5      = 62
d5       = 63
dd5      = 64
e5       = 65
f5       = 66
ff5      = 67
g5       = 68
gg5      = 69
a5       = 70
aa5      = 71
b5       = 72
c6       = 73
cc6      = 74
d6       = 75
dd6      = 76
e6       = 77
f6       = 78
ff6      = 70
g6       = 80
gg6      = 81
a6       = 82
aa6      = 83
b6       = 84
c7       = 85
cc7      = 86
d7       = 87
dd7      = 88
e7       = 89
f7       = 90
ff7      = 91
g7       = 92
gg7      = 93
a7       = 94
aa7      = 95
b7       = 96

;---------------------------------------
ezp      = $40

         *= $1000
restart  jmp reset
plloop   jmp irqloop
stopmus
         lda #0
         sta play
         lda #8
         sta $d404
         sta $d404+7
         sta $d404+14
         rts
lobyte
         !by $16,$27,$39,$4b,$5f,$74
         !by $8a,$a1,$ba,$d4,$f0,$0e
         !by $2d,$4e,$71,$96,$be,$e7
         !by $14,$42,$74,$a9,$e0,$1b
         !by $5a,$9c,$e2,$2d,$7b,$cf
         !by $27,$85,$e8,$51,$c1,$37
         !by $b4,$38,$c4,$59,$f7,$9e
         !by $4e,$0a,$c8,$a2,$81,$6d
         !by $67,$70,$89,$b2,$ed,$3b
         !by $9d,$14,$a0,$45,$03,$db
         !by $cf,$e1,$12,$65,$db,$76
         !by $3a,$27,$41,$8a,$05,$b5
         !by $9d,$c1,$24,$c9,$b6,$ed
         !by $73,$4e,$82,$14,$0a,$6a
         !by $3b,$82,$48,$93,$6b,$da
         !by $e7,$9c,$04,$28,$14

hibyte
         !by $01,$01,$01,$01,$01,$01
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
         !by $45,$49,$4e,$52,$57,$5c
         !by $62,$68,$6e,$75,$7c,$83
         !by $8b,$93,$9c,$a5,$af,$b9
         !by $c4,$d0,$dd,$ea,$f8

;   !by a,a,a,speed  (a=stepnr, 0?=>
;                            no effect!)
effectst !by 1,2,3,3
         !by 4,5,2,3
         !by 6,7,6,2
         !by 8,9,0,2
         !by 10,255,255,2
         !by 255,11,0,3

;--------------------------------------;
contdur  !by 0
reset
         asl
         asl
         tay
         lda effectst,y
         cmp #$ff
         bne doit
         jmp nm
doit     sta efnr
         ldx #0
         jsr clear
nm       lda effectst+1,y
         cmp #$ff
         bne doit1
         jmp nm1
doit1    sta efnr+1
         ldx #1
         jsr clear
nm1      lda effectst+2,y
         cmp #$ff
         bne doit2
         jmp nm2
doit2    sta efnr+2
         ldx #2
         jsr clear
nm2      lda effectst+3,y
         sta overallspeed
         sta oversp
         lda #filtpasst+15
         sta play
         rts

rectab   !by $00,$07,$0e

clear
         lda #0
         sta blockix,x
         sta logl,x
         sta higl,x
         sta duration,x
         sta durtel,x
         sta soundnr,x
         sta breb,x
         sta wfix,x
         lda rectab,x
         tax
         lda #0
         sta $d400,x
         sta $d401,x
         sta $d402,x
         sta $d403,x
         sta $d404,x
         sta $d405,x
         sta $d406,x
         rts

;--------------------------------------;
efnr     !by 0,0,0
blockix  !by 0,0,0
drix     !by 0,0,0
logl     !by 0,0,0
higl     !by 0,0,0
pwud     !by 0,0,0
duration !by 0,0,0
durtel   !by 0,0,0
vix      !by 0,0,0
soundnr  !by 0,0,0
breb     !by 0,0,0
sp       !by 0,0,0
glb      !by 0,0,0
frr      !by 0,0,0
pur      !by 0,0,0
wnf      !by 0,0,0
wfix     !by 0,0,0
cuto     !by 0
fiwa     !by 0
chendgll !by 0
chendglh !by 0
cof      !by 0
oversp   !by 0
;---------------------------------------
irqloop
         lda #$bd    ; lda ,x
         dec oversp
         bpl noid
overallspeed = *+1
         lda #0
         sta oversp
         lda #$fe    ; inc
noid     sta pis

play     = *+1
         lda #1
         sta $d418
         bne letplay
         rts
letplay
         ldx #$02
goon3
         ldy efnr,x
         bne dovoi
         jmp eoro
dovoi    lda blockadl-1,y
         sta zp
         lda blockadh-1,y
         sta zp+1
rrbb     ldy blockix,x
         lda (zp),y
         bpl goon5
         iny
         inc blockix,x
         cmp #snd
         bcs chosfx
         and #$7f
         sta duration,x
         jmp rrbb+3
chosfx
         asl
         asl
         asl
         sta soundnr,x
         jmp rrbb+3
fiof
         lda #0
         sta wnf,x
         inc blockix,x
         jmp rrbb
goon5
         cmp #durcontrol
         bcc goon7
         bne chgl
         lda contdur
         sta duration,x
         inc blockix,x
         iny
         jmp rrbb+3

ppp      lda #0
         sta breb,x
         jmp setin
chgl
         cmp #pause
         beq ppp
         cmp #filtoff
         beq fiof
         cmp #filter
         bne glider
         lda blockix,x
         clc
         adc #4
         sta blockix,x
         lda #1
         sta wnf,x
         iny
         lda (zp),y
         sta cuto
         iny
         lda (zp),y
         sta fiwa
         iny
         lda (zp),y
         sta flength
         iny
         jmp rrbb+3

glider   iny
         lda (zp),y
         sta nodrum+1
         iny
         sty sy1+1
         iny
         lda (zp),y
         clc
         tay
         lda hibyte-1,y
         sta chendglh
         lda lobyte-1,y
         sta chendgll
sy1      ldy #0
         lda (zp),y
         ldy breb,x
         bne setin
         tay
         lda hibyte-1,y
         sta higl,x
         lda lobyte-1,y
         sta logl,x
         lda #0
         jmp dpgf
goon7
         clc
         sta arpad+1
         tay
         lda hibyte-1,y
         sta freqhi+1
         lda lobyte-1,y
         sta freqlo+1
         lda breb,x
         bne setin
dpgf     sta drix,x
         sta sp,x
         sta glb,x
         sta pur,x
         sta wfix,x
         lda #$ff
         sta breb,x
         sta vix,x
         sta pwud,x
         sta frr,x
setin
         sta cont0+1

         ldy soundnr,x
         lda wfadsr,y
         sta zzkp+1
         lda wfadsr+1,y
         sta ad+1
         lda wfadsr+2,y
         sta sr+1
         lda wfadsr+3,y
         asl
         asl
         sta pl3+1
         lda wfadsr+4,y
         sta nodp+1
         lda wfadsr+5,y
         asl
         asl
         sta vibyte+1
pis
         inc durtel,x
         lda durtel,x
         cmp duration,x
         bmi pl2
         sta gooh+1
         lda nodrum+1
         beq nogg
         lda blockix,x
         clc
         adc #3
         sta blockix,x
         lda #0
nogg     sta durtel,x
         sta breb,x
         inc blockix,x
         ldy blockix,x
         lda (zp),y
         beq stopper
         cmp #repeat
         bne gooh
         lda #0
         sta blockix,x
         jmp gooh

stopper  sta efnr,x

gooh     lda #0

pl2      cmp #0
         bcc pl3
klkleur2 lda #0
         sta wavef+1

;----------------------------pulseeffect
pl3
         ldy #0
         beq zzkp

         lda sp,x
         clc
         adc pudata-4,y
         sta sp,x

         lda pur,x
         bne nnn
         lda pudata+2-4,y
         clc
         adc pwud,x
         sta pwud,x
         cmp pudata+1-4,y
         bcc goodw
         inc pur,x
         jmp goodw
nnn
         lda pwud,x
         sec
         sbc pudata+2-4,y
         sta pwud,x
         bcs goodw
         dec pur,x
goodw
         clc
         adc pudata+3-4,y
         adc sp,x
dopu
         sta zp
         asl
         asl
         asl
         asl
         ldy rectab,x
         sta $d402,y
         lda zp
         lsr
         lsr
         lsr
         lsr
         sta $d403,y
zzkp
;------------------------------waveforms
         ldy #0
         lda waveformsl,y
         sta zp
         lda waveformsh,y
         sta zp+1
         ldy wfix,x
         lda (zp),y
         bne notend
         iny
         lda (zp),y
         sta wfix,x
         tay
         lda (zp),y
notend
         sta wavef+1
         inc wfix,x

;------------------------------freq-poke

nodp     ldy #0
         beq nodrum
         bpl charpeggio
drumnr   lda dradl-$81,y
         sta zp
         lda dradh-$81,y
         sta zp+1
         ldy drix,x
         lda (zp),y
         bne dodwf
         iny
         lda (zp),y
         sta drix,x
         tay
         lda (zp),y
dodwf    sta freqhi+1
         lda #0
         sta freqlo+1
         inc drix,x
         jmp nodrum

;-------------------------------arpeggio
charpeggio
         lda arpadl-1,y
         sta zp
         lda arpadh-1,y
         sta zp+1
         ldy drix,x
         lda (zp),y
         bne neoat   ; 0? => arpend
         tay
         sta drix,x
         lda (zp),y
neoat    clc
arpad    adc #0
         tay
         inc drix,x
         lda lobyte-2,y
         sta freqlo+1
         lda hibyte-2,y
         sta freqhi+1
         jmp noglide

;----------------------------------glide
nodrum
         lda #0
         beq noglide
         bmi gdown
         asl
         clc
         adc logl,x
         sta logl,x
         sta freqlo+1
         lda higl,x
         adc #0
         sta higl,x
         sta freqhi+1
         cmp chendglh
         bcc noglide
         lda freqlo+1
         cmp chendgll
         bcc noglide
         jmp contb
gdown
         asl
         sta trek1+1
         lda logl,x
         sec
trek1    sbc #0
         sta logl,x
         sta freqlo+1
         lda higl,x
         sbc #0
         sta higl,x
         sta freqhi+1
         cmp chendglh
         bpl noglide
         lda freqlo+1
         cmp chendgll
         bpl noglide
contb
         lda chendgll
         sta freqlo+1
         lda chendglh
         sta freqhi+1
         lda blockix,x
         clc
         adc #3
         sta blockix,x

;--------------------------------vibrato
noglide
         lda #0
         sta nodrum+1
         sta lfip+1
         sta hfip+1
vibyte   ldy #0
         beq cont0
         lda durtel,x
         cmp vidata-4,y ; 1 = 1e vibr.
         bcc cont0
         cmp vidata+1-4,y
         bcs cont0

         lda vidata+3-4,y
         clc
         adc vix,x
         and #$7f
         sta vix,x
         clc
         adc vidata+2-4,y
         and #$7f
         tay
         lda sinus,y
         ldy vix,x
         sec
         sbc sinus,y
         sta lfip+1
         lda #0
         sbc #0
         sta hfip+1

;---------------------------------output
cont0
         ldy #0
         beq eoro
         ldy rectab,x
lfip     lda #0
         clc
freqlo   adc #0
         sta $d400,y
hfip     lda #0
freqhi   adc #0
         sta $d401,y
wavef    lda #0
         sta $d404,y
ad       lda #0
         sta $d405,y
sr       lda #0
         sta $d406,y
eoro
         dex
         bmi pnv
         jmp goon3
pnv
;---------------------------filtereffect
sres     lda #resonance
         ldx wnf+2
         beq nf1
         ora #4
         ldy #2
nf1      ldx wnf+1
         beq nf2
         ora #2
         ldy #1
nf2      ldx wnf
         beq nf3
         ora #1
         ldy #0
nf3
         cmp #resonance
         sta $d417
         beq exfif-1
         sty exfif+1

         ldy fiwa
         bne exfif
         lda cuto
         sta $d416
         rts
exfif
         ldx #0
         lda frr,x
         beq nmfr
         lda #0
         sta frr,x
         tya
         and #$f0
         sta cof
flength  = *+1
         lda #0
         sta tlr
         lda #1
         sta tlr2
nmfr
tlr      = *+1
         lda #0
         beq eof
         dec tlr
         dec tlr2
tlr2     = *+1
         lda #0
         bne eof
         tya
         and #$0f
         sta tlr2

         lda cuto
         clc
         adc cof
         sta cof
         sta $d416
eof      rts


;---------------------------------------
blockadl
         !by <block01,<block02
         !by <block03,<block04
         !by <block05,<block06
         !by <block07,<block08
         !by <block09,<block10
         !by <block11,<block12
         !by <block13,<block14
         !by <block15,<block16

blockadh
         !by >block01,>block02
         !by >block03,>block04
         !by >block05,>block06
         !by >block07,>block08
         !by >block09,>block10
         !by >block11,>block12
         !by >block13,>block14
         !by >block15,>block16

;---------------------------------------
srb
sinus
         !by $00,$00,$00,$01,$01,$02
         !by $04,$05,$07,$0a,$0c,$0f
         !by $12,$15,$19,$1d,$21,$25
         !by $29,$2e,$33,$38,$3d,$43
         !by $48,$4e,$54,$5a,$60,$66
         !by $6c,$72,$79,$7f,$85,$8b
         !by $91,$98,$9e,$a4,$aa,$b0
         !by $b5,$bb,$c0,$c6,$cb,$d0
         !by $d4,$d9,$dd,$e1,$e5,$e9
         !by $ec,$ef,$f2,$f5,$f7,$f9
         !by $fb,$fc,$fd,$fe,$ff,$ff
         !by $ff,$fe,$fe,$fd,$fb,$fa
         !by $f8,$f6,$f3,$f1,$ed,$ea
         !by $e7,$e3,$df,$db,$d6,$d1
         !by $cd,$c7,$c2,$bd,$b7,$b2
         !by $ac,$a6,$a0,$9a,$94,$8d
         !by $87,$81
         !by $7b,$73,$6d,$67,$61,$5b
         !by $55,$4f,$49,$43,$3e,$39
         !by $34,$2f,$2a,$25,$21,$1d
         !by $19,$16,$12,$0f,$0c,$0a
         !by $08,$06,$04,$03,$01,$01
         !by tabend,sinus-srb

;--------------------------------------;
;------------ARPEGGIOS-----------------;
;--------------------------------------;
arpadl   !by <arp1
arpadh   !by >arp1

arp1     !by c0,c1,arpend

;--------------------------------------;
;-------------FREQ-TABS----------------;
;--------------------------------------;
dradl    !by <freq1
dradh    !by >freq1

freq1    !by $14,$8e,$14,end,1

;---------------------------------------

;pulse:slidespeed,top,speed,bottom
;                        (or:pulsewidth)

pudata   !by $12,$30,$14,$00   ;1
         !by $00,$00,$00,$80   ;6

;vibrato: delay,length,amplitude,speed

vidata
         !by 2,255,$fe,$0a   ;1
         !by 1,255,$30,$25   ;2
         !by 1,12,$20,$03    ;3

;---------------------------------------
waveformsl
         !by <wf0,<wf1,<wf2,<wf3,<wf4
waveformsh
         !by >wf0,>wf1,>wf2,>wf3,>wf4

wf0      !by $85,$85,$47,$46,end,3
wf1      !by $81,$11,$81,$11,$80,$10
         !by $80,end,4
wf2      !by $85,$85,$85,$85,$85,$86
         !by $84,$80,end,5
wf3      !by $87,$27,$17,$27,$26,$16
         !by end,4
wf4      !by $13,$53,$51,$50,end,3


;--------------------------------------;
;------------SOUNDSETTINGS-------------;
;--------------------------------------;
;ring mod./synchro.-tabel:   (rm:4/s:2)
;         rm/syn.    with
;   voice    1         3
;            2         1
;            3         2

; filters:

filtpasst = $10 ; $10/$20/$40:lo/band/hi
resonance = $f0

; waveformt,ad,sr,pulsesetting
; freqtab,vibratosetting,nvt,nvt

wfadsr
         !by 0,$03,$f9,1          ;0
         !by niks,1,0,0

         !by 1,$04,$c9,2          ;1
         !by niks,niks,0,0

         !by 0,$05,$a9,1          ;2
         !by arp + 1,niks,0,0

         !by 2,$2f,$ea,niks       ;3
         !by niks,2,0,0

         !by 2,$2f,$ea,niks       ;4
         !by freq,1,2,0,0

         !by 3,$0f,$f7,niks       ;5
         !by niks,niks,0,0

         !by 0,$04,$fa,1          ;6
         !by niks,2,0,0

         !by 3,$0f,$fc,1          ;7
         !by niks,3,0,0

         !by 4,$0a,$fc,1          ;8
         !by niks,1,0,0


;--------------------------------------;
;---------------Blocks-----------------;
;--------------------------------------;
; voor een 'end' of 'repeat' moet een
; noot of een 'pause' staan!!!

block01
         !by snd + 1,dur + 8,c4,end

block02
         !by snd + 0,dur + 8,glide
         !by down+$70,c4,c2,end
block03
         !by snd + 2,dur + 8,c4,end
block04
         !by filter,$78,$02,$30
         !by snd + 3,dur + 2,c6,dur + 20
         !by c4,end
block05
         !by snd + 4,dur + 23,c2,end
block06
         !by snd + 5,dur + 95,glide
         !by down+$10,c5,c0,end
block07
         !by snd + 6,dur + 4,c4
         !by dur + 4,c4,dur + 6,c4,end
block08
         !by snd + 2,dur + 2,c3,e3,c4,e3
         !by dur + 8,c4,end
block09
         !by snd + 7,dur + 20,c4,end
block10
         !by filter,$fe,$32,$60
         !by snd + 4,durcontrol
         !by c3,repeat
block11
         !by snd + 8,dur + 10,c3,end
block12
block13
block14
block15
block16

}