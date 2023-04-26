
	; A very short testing tune
	; Converted to ACME by dmx87

	!to "Deenen_Charles_MONPlayer.sid",plain

	* = $0000

	!text "PSID"
	!be16 2							; version 2
	!be16 $7c						; data offset
	!be16 $1000						; Load (0 = auto)
	!be16 restart					; Init
	!be16 plloop					; Play
	!be16 1							; num songs
	!be16 1							; first song
	!word 0
	!word 0
-	!text "Test Tune"
	!fill 32 - (* - -)
-	!text "Charles Deenen"
	!fill 32 - (* - -)
-	!text "1988 Maniacs of Noise"
	!fill 32 - (* - -)
	!word 0							; v2 flags
	!word 0							; Start page, page length (reloc)
	!word 0							; Reserved
		
!pseudopc $1000 {

;0@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@.
;]    Maniacs of Noise music player    ]
;]   (C) Scoop designs ltd. / m.o.n.   ]
;-@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@=
songnummer = 0
;---------------------------------------
zp       = $fb    ; vector
;---------------------------------------
; statements:
;---------------------------------------
end      = $00
arpend   = $00
drend    = $ff
wfend    = $08
tabend   = $ff
endsong  = $ff
rep      = $bf
dur      = $80
snd      = $e0
vol      = $ef
up       = $00
down     = $80
trs      = $7f
drum     = $80
arp      = $00
larp     = $40
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
stop     = $7b
;---------------------------------------
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
; s        = dur.1
; e        = dur.2
; es       = dur.3
; q        = dur.4
; qs       = dur.5
; qe       = dur.6
; qes      = dur.7
; h        = dur.8
; hs       = dur.9
; he       = dur.10
; hes      = dur.11
; hq       = dur.12
; hqs      = dur.13
; hqe      = dur.14
; hqes     = dur.15
; w        = dur.16
; ws       = dur.17
; we       = dur.18
; wes      = dur.19
; wq       = dur.20
; wqs      = dur.21
; wqe      = dur.22
; wqes     = dur.23
; wh       = dur.24
; whs      = dur.25
; whe      = dur.26
; whes     = dur.27
; whq      = dur.28
; whqs     = dur.29
; whqe     = dur.30
; whqes    = dur.31
; ww       = dur.32
; wws      = dur.33
; wwe      = dur.34
; wwes     = dur.35
; wwq      = dur.36
; wwqs     = dur.37
; wwqe     = dur.38
; wwqes    = dur.39
; wwh      = dur.40
; wwhs     = dur.41
; wwhe     = dur.42
; wwhes    = dur.43
; wwhq     = dur.44
; wwhqs    = dur.45
; wwhqe    = dur.46
; wwhqes   = dur.47
; www      = dur.48
; wwws     = dur.49
; wwwe     = dur.50
; wwwes    = dur.51
; wwwq     = dur.52
; wwwqs    = dur.53
; wwwqe    = dur.54
; wwwqes   = dur.55
; wwwh     = dur.56
; wwwhs    = dur.57
; wwwhe    = dur.58
; wwwhes   = dur.59
; wwwhq    = dur.60
; wwwhqs   = dur.61
; wwwhqe   = dur.62
; wwwhqes  = dur.63
; wwww     = dur.64
; wwwws    = dur.65
; wwwwe    = dur.66
; wwwwes   = dur.67
; wwwwq    = dur.68
; wwwwqs   = dur.69
; wwwwqe   = dur.70
; wwwwqes  = dur.71
; wwwwh    = dur.72
; wwwwhs   = dur.73
; wwwwhe   = dur.74
; wwwwhes  = dur.75
; wwwwhq   = dur.76
; wwwwhqs  = dur.77
; wwwwhqe  = dur.78
; wwwwhqes = dur.79
; wwwww    = dur.80
; wwwwws   = dur.81
; wwwwwe   = dur.82
; wwwwwes  = dur.83
; wwwwwq   = dur.84
; wwwwwqs  = dur.85
; wwwwwqe  = dur.86
; wwwwwqes = dur.87
; wwwwwh   = dur.88
; wwwwwhs  = dur.89
; wwwwwhe  = dur.90
; wwwwwhes = dur.91
; wwwwwhq  = dur.92
; wwwwwhqs = dur.93
; wwwwwhqe = dur.94
; wwwwwhqes = dur.95
; wwwwww   = dur.96
;---------------------------------------
ezp      = $40
record   = $41
recpos   = $43
recdur   = $44

;--------------------------------------;
         *= $1000
;---------------------------------------
noot     !by 0
transp   !by 0
sound    !by 0


restart  jmp reset
plloop   jmp irqloop
stopmus  jmp kickit
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

mlen     !by $00,$00,$00
resetto  !by $00,$00,$00


songadrl !by <track1,<track2,<track3

songadrh !by >track1,>track2,>track3

songlen  !by track2-track1
         !by track3-track2
         !by em-track3

songres  !by reset1-track1
         !by reset2-track2
         !by reset3-track3

songspd  !by 3

;--------------------------------------;
reset
         pha
         sta sra1+1
         asl
         clc
sra1     adc #0
         tay
         jsr kickit
         ldx #0
setsd    lda songadrl,y
         sta mtabl,x
         lda songadrh,y
         sta mtabh,x
         lda songlen,y
         sta mlen,x
         lda songres,y
         sta resetto,x
         iny
         inx
         cpx #3
         bne setsd
         pla
         tay
         lda songspd,y
         sta overallspeed
         sta oversp
         lda #filtpasst+15
         sta play
         ldx #2
ffr      jsr readtrack
         dex
         bpl ffr
         rts
kickit
         lda #0
         sta play
         sta eorb
         ldx #irqloop-voi1x-1
         lda #0
clab     sta voi1x,x
         sta $d400,x
         dex
         bpl clab
         rts

;--------------------------------------;
readtrack
         lda mtabl,x
         sta zp
         lda mtabh,x
         sta zp+1
zkipvl   ldy voi1x,x
         lda (zp),y
         bpl setstepn
         iny
         cmp #rep
         bcc gotrs
         cmp #vol
         bcc gorep
         cmp #endsong
         beq eso
         and #$0f
         ora #filtpasst
         sta play
         jmp zkipvl+3
gotrs
         and #$3f
         sta nnh,x
         jmp zkipvl+3

eso      lda #0
         sta play
         pla
         pla
         rts
gorep
         and #$3f
         sta repc,x
         lda (zp),y
setstepn sta blocknr,x
         tya
         sta voi1x,x
         rts

mtabl    !by $00,$00,$00
mtabh    !by $00,$00,$00
voi1x    !by 0,0,0
nnh      !by 0,0,0
blockix  !by 0,0,0
drix     !by 0,0,0
logl     !by 0,0,0
higl     !by 0,0,0
repc     !by 0,0,0
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
blocknr  !by 0,0,0
wnf      !by 0,0,0
loopadd  !by 0,0,0
stopb    !by 0,0,0
eorb     !by 0
cuto     !by 0
chendgll !by 0
chendglh !by 0
cof      !by 0
oversp   !by 0
voicex   !by 0




;--------------------------------------;
irqloop
         lda #$bd    ; lda ,x
         dec oversp
         bpl noid
overallspeed = *+1
         lda #0
         sta oversp
         lda #$fe    ; inc
noid     sta pis

         lda play    ;;;
         bne play+1  ;
         lda noot    ;
         beq play+1  ;
         ldx #$60    ;
         stx eoro    ;
         jmp plnoo   ;;;

play     = *+1
         lda #1
         sta $d418
         bne letplay
         rts
letplay

         lda #$ca    ; dex   ;;
         sta eoro    ;;;;;;;;;;

         lda noot        ;;;;
         beq doo         ;
plnoo                    ;
         clc             ;
         adc transp      ;
         sta arpad+1     ;
         tay             ;
         lda pis         ;
         pha             ;
         cmp #$bd        ;
         beq zkipvh      ;
         inc durtel+2    ;
         inc recdur      ;
         lda durtel+2    ;
         cmp #95         ;
         bne zkipvh      ;
         lda #90         ;
         sta durtel+2    ;
zkipvh                   ;
         lda hibyte-1,y  ;
         sta freqhi+1    ;
         lda lobyte-1,y  ;
         sta freqlo+1    ;
         lda eorb        ;
         eor #$ff        ;
         sta eorb        ;
         ldx #$02        ;
         lda #$60        ;
         sta pis         ;
         jsr setin       ;
         pla             ;
         sta pis         ;
         lda #0          ;
         sta wnf+2       ;
         lda #$0e        ;
         sta voicex      ;
         lda durtel+2    ;
         jsr pl2         ;
         lda play        ;
         bne trug        ;
         jsr pnv         ;;;;
trug     rts

doo      ldx #$02
         lda eorb
         eor #$ff
         sta eorb
goon3
         lda rectab,x
         bpl dv
         jmp eoro
dv
         sta voicex
         ldy blocknr,x
         lda blockadl,y
         sta zp
         lda blockadh,y
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
         cmp #stop
         bcc goon7
         bne chgl
         lda #1
         sta stopb,x
         jmp setin

pp       lda #0
         sta breb,x
         jmp setin
chgl
         cmp #pause
         beq pp
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
         adc nnh,x
         tay
         lda hibyte-1,y
         sta chendglh
         lda lobyte-1,y
         sta chendgll
sy1      ldy #0
         lda (zp),y
         ldy breb,x
         bne setin
         clc
         adc nnh,x
         tay
         lda hibyte-1,y
         sta higl,x
         lda lobyte-1,y
         sta logl,x
         lda #0
         jmp dpgf

goon7
         clc
         adc nnh,x
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
         sta loopadd,x
         sta stopb,x
         lda #$ff
         sta breb,x
         sta vix,x
         sta pwud,x
         sta frr,x
setin
         sta cont0+1

         ldy soundnr,x
         lda waveform1,y
         sta wavef+1
         lda waveform2,y
         sta klkleur2+1
         lda attackdecay,y
         sta ad+1
         lda sustainrelease,y
         sta sr+1
         lda gateonlength,y
         sta pl2+1
         lda pulsefx,y
         asl
         asl
         adc pulsefx,y
         sta pl3+1
         lda arpdrum,y
         sta nodp+1
         lda vibratofx,y
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
         bne gooh
         sta blockix,x
         dec repc,x
         bpl vhir+3
         inc repc,x
vhir     inc voi1x,x
         lda voi1x,x
         cmp mlen,x
         bne gooh-3
         lda resetto,x
         sta voi1x,x
         jsr readtrack
gooh     lda #0

pl2      cmp #0
         bcc chstop
klkleur2 ldy #0
         sty wavef+1
;---------------------------------------
chstop
         ldy stopb,x
         beq pl3
         ldy voicex
         lda #8
         sta $d404,y
         jmp eoro

;----------------------------pulseeffect
pl3
         ldy #0
         beq zzkp
         cmp #1
         bne notfi
         lda pudata+1-5,y
         sta pwud,x

notfi    lda sp,x
         clc
         adc pudata-5,y
         sta sp,x

         lda pur,x
         bne nnn
         lda pudata+3-5,y
         beq finape
         clc
         adc pwud,x
         sta pwud,x
         cmp pudata+2-5,y
         bcc goodw
         inc pur,x
         jmp goodw

finape   lda pudata+1-5,y
         jmp goodw
nnn
         lda pwud,x
         sec
         sbc pudata+3-5,y
         sta pwud,x
         cmp pudata+1-5,y
         bcs goodw
         dec pur,x
         lda loopadd,x
         clc
         adc pudata+4-5,y
         sta loopadd,x
         lda pwud,x
goodw
         clc
         adc sp,x
         adc loopadd,x
dopu
         sta zp
         asl
         asl
         asl
         asl
         ldy voicex
         sta $d402,y
         lda zp
         lsr
         lsr
         lsr
         lsr
         sta $d403,y
zzkp
;----------------------------------drums
         lda wavef+1
         cmp #$e0
         bcc nodp
         and #$9f
         tay
         jmp drumnr
nodp     ldy #0
         beq prca
         bpl charpeggio
drumnr
         lda dradl-$81,y
         sta zp
         lda dradh-$81,y
         sta zp+1
         ldy drix,x
         lda (zp),y
         bpl dodwf
         cmp #drend
         beq enddrum
         and #$7f
         clc
         adc freqhi+1
dodwf    inc drix,x
enddrum
         sta freqhi+1
         tya
         clc
         adc #wft1-drum1
         tay
         lda (zp),y
         sta wavef+1
         jmp nodrum

;-------------------------------arpeggio
prca
         cmp #$c0       ;a=waveform
         bcc nodrum
         and #$1f
         tay
         lda klkleur2+1
         ora #$10
         sta wavef+1

charpeggio
         tya
         and #$40
         sta straa+1
         tya
         and #$3f
         tay
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
         lda lobyte-2,y
         sta freqlo+1
         lda hibyte-2,y
         sta freqhi+1

straa    lda #0
         beq aw
         lda eorb
         beq noglide
aw       inc drix,x
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
         bcs noglide
         lda freqlo+1
         cmp chendgll
         bcs noglide
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
         cmp vidata-4,y
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
         ldy voicex
lfip     lda #0
         clc
freqlo   adc #0
         sta $d400,y
hfip     lda #0
freqhi   adc #0
         sta $d401,y
ad       lda #0
         sta $d405,y
sr       lda #0
         sta $d406,y
wavef    lda #0
         sta $d404,y
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
         sta $d417
         cmp #resonance
         beq exfif-1
         sty exfif+1

fiwa     = *+1
         ldy #0
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
blockadl !by <block00
         !by <block01,<block02
         !by <block03,<block04
         !by <block05,<block06
         !by <block07,<block08
         !by <block09,<block10
         !by <block11,<block12
         !by <block13,<block14
         !by <block15,<block16

blockadh !by >block00
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
arpadl   !by <arp1,<arp2,<arp3,<arp4
         !by <arp5,<arp6,<arp7
arpadh   !by >arp1,>arp2,>arp3,>arp4
         !by >arp5,>arp6,>arp7


arp1     !by c0,c1,arpend

arp2     !by g0,c0,e0,arpend

arp3     !by g0,aa0,dd0,arpend

arp4     !by g0,aa0,c0,dd0,arpend

arp5     !by c0,dd0,g0
         !by arpend

arp6     !by c0,f0,a0,arpend

arp7     !by c0,g0,b0,arpend

;--------------------------------------;
;--------------DRUMS-------------------;
;--------------------------------------;
dradl    !by <drum1,<drum2,<drum3
         !by <drum4
dradh    !by >drum1,>drum2,>drum3
         !by >drum4

drum1
         !by $8e,$14,$8e,$14,$8e,$14
         !by $8e,$14,$8e,$14,$8e,$14
         !by $8e,$14,$8e,$14,$8e,$14
         !by $8e,$14,$8e,$14,$8e,$14
         !by $8e,$14,$8e,$14,$8e,$14
         !by $8e,$14,$8e,$14,$8e
         !by drend

drum2    !by $b8,$b8,$8e,$8c,$b8,$0d
         !by $38,$34,$38,$34,$38,$8d
         !by $0d,$0d,$0d,drend

drum3
         !by $60,$0c,$0d,$70,$70,$80
         !by $80,$70,$70,$80,$80,$70
         !by drend
drum4
         !by $50,$0a,$08,$06
         !by $05,$04,$03,$02
         !by $02,$01,$00,$00,drend



wft1     !by $81,$11,$81,$11,$80,$10
         !by $80,$10,$80,$10,$80,$10
         !by $80,$10,$80,$10,$80,$10
         !by $80,$10,$80,$10,$80,$10
         !by $80,$10,$80,$10,$80,$10
         !by $80,$10,$80,$10,$80
         !by wfend

wft2     !by $81,$81,$11,$81,$80,$80
         !by $80,$80,$80,$80,$10,$80
         !by $10,$80,$10,wfend
wft3
         !by $81,$41,$11,$80,$80,$80
         !by $80,$80,$80,$80,$80,$80
         !by wfend
wft4
         !by $81,$41,$40,$40
         !by $40,$40,$40,$40
         !by $40,$40,$40,$40,wfend

;---------------------------------------

;pulse:slidespeed,bottom,top,speed,loop+
;             (or:pulsewidth)

pudata   !by $02,$10,$30,$04,$f4 ;1
         !by $00,$04,$48,$02,$00 ;2
         !by $ff,$30,$45,$04,$20 ;3
         !by $04,$40,$7f,$02,$00 ;4
         !by $03,$70,$88,$01,$fe ;5
         !by $00,$80,$00,$00,$00 ;6
         !by $00,$20,$00,$00,$00 ;7
         !by $00,$30,$00,$00,$00 ;8

;vibrato: delay,length,amplitude,speed

vidata
         !by 2,255,$fe,$0a     ;1
         !by 0,255,$14,$f0     ;2
         !by 0,255,$04,$0c     ;3
         !by 5,255,$14,$0c     ;4
         !by 2,255,$10,$10     ;5
         !by 1,255,$06,$10     ;6
         !by 1,255,$10,$14     ;7
         !by 3,255,$04,$18     ;8

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


waveform1 = *
waveform2 = *+1
attackdecay = *+2
sustainrelease = *+3
gateonlength = *+4
pulsefx  = *+5
arpdrum  = *+6
vibratofx = *+7


         !by $00,$00,$00,$f5      ;0
         !by 0,6,drum + 3,niks

         !by $41,$40,$03,$fc      ;1
         !by 2,1,niks,1

         !by $41,$40,$02,$f9      ;2
         !by 2,1,niks,1

         !by $41,$40,$09,$fb      ;3
         !by 3,2,niks,2

         !by $41,$40,$32,$ec      ;4
         !by 3,1,arp + 6,3

         !by $41,$40,$32,$ec      ;5
         !by 3,1,arp + 7,3

         !by $11,$40,$03,$ea      ;6
         !by 2,3,niks,5

         !by $41,$40,$03,$ec      ;7
         !by 2,1,arp + 2,6

         !by $41,$40,$11,$cc      ;8
         !by 3,4,niks,7

         !by $41,$20,$09,$5d      ;9
         !by 5,1,niks,8

         !by drump,2,$40,$09,$ce  ;10
         !by 2,5,niks,niks

         !by $41,$40,$02,$c8      ;11
         !by 2,7,larp + 5,niks

         !by $00,$00,$00,$f3      ;12
         !by 0,6,drum + 4,niks

         !by $00,$00,$00,$fa      ;13
         !by 2,6,drum + 1,niks


;--------------------------------------;
;------------MUZIEK-DATA---------------;
;--------------------------------------;
;filter: stem 1 voor 2 voor 3.

       ; !by $00,$07,$0e
rectab   !by $00,$07,$8e ; $8x:disable


track1
reset1
         !by 0
track2
reset2
         !by trs+c1,1

track3
reset3

em
;--------------------------------------;
;---------------Blocks-----------------;
;--------------------------------------;
; voor een 'end'-byte moet altijd een
; noot,pause of hold staan!!!

block00
         !by snd + 4
         !by dur + 9,c4,c4,dur + 6,c4
         !by snd + 5
         !by dur + 9,c4,c4,dur + 6,c4
         !by end
block01
         !by snd + 3,dur + 18,c4
         !by dur + 3,c4
         !by dur + 12,d4
         !by dur + 3,d4
         !by dur + 3,c4
         !by dur + 3,d4
         !by dur + 3,c4
         !by dur + 3,a3
         !by dur + 18,c4
         !by dur + 3,c4
         !by dur + 27,d4
         !by end
block02
block03
block04
block05
block06
block07
block08
block09
block10
block11
block12
block13
block14
block15
block16

}