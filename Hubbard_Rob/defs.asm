
C0  = $00
Cs0 = $01
D0  = $02
Ds0 = $03
E0  = $04
F0  = $05
Fs0 = $06
G0  = $07
Gs0 = $08
A0  = $09
As0 = $0a
H0  = $0b

C1  = $0c
Cs1 = $0d
D1  = $0e
Ds1 = $0f
E1  = $10
F1  = $11
Fs1 = $12
G1  = $13
Gs1 = $14
A1  = $15
As1 = $16
H1  = $17

C2  = $18
Cs2 = $19
D2  = $1a
Ds2 = $1b
E2  = $1c
F2  = $1d
Fs2 = $1e
G2  = $1f
Gs2 = $20
A2  = $21
As2 = $22
H2  = $23

C3  = $24
Cs3 = $25
D3  = $26
Ds3 = $27
E3  = $28
F3  = $29
Fs3 = $2a
G3  = $2b
Gs3 = $2c
A3  = $2d
As3 = $2e
H3  = $2f

C4  = $30
Cs4 = $31
D4  = $32
Ds4 = $33
E4  = $34
F4  = $35
Fs4 = $36
G4  = $37
Gs4 = $38
A4  = $39
As4 = $3a
H4  = $3b

C5  = $3c
Cs5 = $3d
D5  = $3e
Ds5 = $3f
E5  = $40
F5  = $41
Fs5 = $42
G5  = $43
Gs5 = $44
A5  = $45
As5 = $46
H5  = $47

C6  = $48
Cs6 = $49
D6  = $4a
Ds6 = $4b
E6  = $4c
F6  = $4d
Fs6 = $4e
G6  = $4f
Gs6 = $50
A6  = $51
As6 = $52
H6  = $53

C7  = $54
Cs7 = $55
D7  = $56
Ds7 = $57
E7  = $58
F7  = $59
Fs7 = $5a
G7  = $5b
Gs7 = $5c
A7  = $5d
As7 = $5e
H7  = $5f

; instrument i,notelen
!macro INSTR .i,.l {
    !by $80 | .l, .i
}

INSTR = $80
PBU = $81
PBD = $80
LEGATO = $20


; instr fx flags

IDRUM = $01         ; drum noise + freq fall
ISKY  = $02         ; use for drums + toms
IARP  = $04         ; octave arp, combine with ISKY for classic effect
IPWL  = $08         ; pulse width lo modulation instead of pulse

