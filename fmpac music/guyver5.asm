; This player supports PSG#3 for sound effects.
; This player does not support SCC and assumes FM present, 9 channels not supported.

; Todo: Check frequency tables

;%include "z80r800.inc"
;%macro align %n
;  %if $ & (%1-1)
;    %defb %1 - ($ & (%1-1))
;  %endif
;%endmacro



R_INITFM:
        di

	ld	hl,Work			; clear work area
	ld	de,Work+1
	ld	bc,CHANNELBYTES*10-1
	ld	[hl],0
	ldir

	xor	a
        ld      [FMSlot],a              ; reset slotnr

        ld      b,4
.pri_l: push    bc
        ld      a,4
        sub     b
        ld      c,a
        ld      hl,EXPTBL
        add     a,l
        ld      l,a
        ld      a,[hl]
        add     a,a
        jr      nc,.notExp

        ld      b,4                     ; slot is expanded
.exp_l: push    bc
        ld      a,24h
        sub     b
        rlca    
        rlca    
        or      c
        ld      [SearchTemp],a

        call    .SearchFM

        ld      a,[FMSlot]

        or      a
        pop     bc
        jr      nz,.end
        djnz    .exp_l
.nextpri:
        pop     bc
        djnz    .pri_l
        ret

.notExp:                                ; slot is not expanded
        ld      a,c
        ld      [SearchTemp],a

        call    .SearchFM

        ld      a,[FMSlot]

        or      a
        jp      z,.nextpri
.end:   pop     bc
        ret     

.SearchFM:
        ld      b,8
        ld      hl,.TxtAPRL
        ld      de,4018h
        call    .Compare
        ret     nc

        ld      b,4
        ld      hl,.TxtOPLL
        ld      de,401Ch
        call    .Compare
        ret     c

        ld      hl,7FF6h
        push    af
        push    hl
        call    RDSLT
        or      1
        ld      e,a
        pop     hl
        pop     af
        call    WRSLT
        or      a
        ret     

.Compare:
	ld      a,[SearchTemp]
        ld      c,a
.loop:  push    bc
        push    hl
        push    de
        ld      a,c
        ex      de,hl
        call    RDSLT
        pop     de
        pop     hl
        pop     bc
        cp      [hl]
        scf     
        ret     nz

        inc     hl
        inc     de
        djnz    .loop
        ld      a,c
        ld      [FMSlot],a
        or      a
        ret     

.TxtAPRL: db     "APRL"
.TxtOPLL: db     "OPLL"


; In: HL = Address of SFX data
R_SPLAY:
        push	hl			; Save Sequence Start Addresses

        ld      de,WorkSFX+14
        ld      hl,DefaultWork14_19
        ld      bc,18
        ldir

; Initialise sequence and pattern pointers
	pop	hl
        ld      [WorkSFX+SEQPTR],hl

        xor	a
	ld      [WorkSFX+PLYFLG],a
        ld      e,[hl]
        inc     hl
        ld      d,[hl]
        inc     hl
        ld      [WorkSFX+PATPTR],de
        ld      a,[hl]
        ld      [WorkSFX+PATLP],a

        ld      a,1
        ld      [WorkSFX+LENCTR],a           ; reset length counter

	ld	a,[Playing]
	or	a
	ret	nz
	ld	a,-1
        ld      [Playing],a
        ret

; In: A = No. of Repeats (0 = infinite)
;     HL = Address of BGM data
; Out: A = Playing flag (0 = not playing, 255 = playing)
R_MPLAY:
        ld      [Loop],a
        ld      [BGMAddr],hl

        call    R_MSTOP

        ld      a,1
        ld      [LoopCount],a

InitBGM:
        ld      b,6                     ; set default FM instrument
        ld      hl,Work+FMINST
        ld      a,10*16
.FMinit:
        ld      [hl],a
        ld      de,CHANNELBYTES
        add     hl,de
        djnz    .FMinit

        ld      hl,.data                ; Initalize FM drums and frequencies
        ld      b,7
.loop:  ld      a,[hl]
        inc     hl
        ld      e,[hl]
        inc     hl
        call    WriteFM
        djnz    .loop
        ld      hl,WorkDrum+DRM1
        ld      de,WorkDrum+DRM1+1
        ld      bc,4
        ld      [hl],0
        ldir

        ld      a,2
        ld      de,WorkPSG+ENV
.envinit:
        ld      hl,DefaultEnv
        ld      bc,12
        ldir

        ld      hl,CHANNELBYTES-12
        add     hl,de
        ex      de,hl
        dec     a
        jr      nz,.envinit

        ld      a,9
        ld      de,Work+14
.wrkinit:
        ld      hl,DefaultWork14_19
        ld      bc,6
        ldir

        ld      hl,CHANNELBYTES-6
        add     hl,de
        ex      de,hl
        dec     a
        jr      nz,.wrkinit

        call    InitSequences

	ld	a,[Playing]
	or	a
	ret	nz
	ld	a,-1
        ld      [Playing],a
        ret

.data:  db      0Eh,20h,16h,54h,17h,54h,18h,54h,26h,08h,27h,08h,28h,04h

DefaultWork14_19:
        db      0,1,1,0,0,0
DefaultEnv:
        db      15,1,15,1,15,15,1,0,8,0,0,0


; Initialise sequence and pattern pointers
InitSequences:
        ld      b,9
        ld      ix,Work
        ld      hl,[BGMAddr]            ; Load Sequence Start Addresses
.chninit:
        ld      e,[hl]
        inc     hl
        ld      d,[hl]
        inc     hl
        ld      [ix+SEQPTR],e
        ld      [ix+SEQPTR+1],d
        push    hl

        ex      de,hl
        ld      a,h
        or      l
        ld      a,1
        jr      z,.noplay               ; if hl == 0 channel is not playing
        xor     a
.noplay:ld      [ix+PLYFLG],a
        ld      e,[hl]
        inc     hl
        ld      d,[hl]
        inc     hl
        ld      [ix+PATPTR],e
        ld      [ix+PATPTR+1],d
        ld      a,[hl]
        ld      [ix+PATLP],a

        ld      [ix+LENCTR],1           ; reset length counter

        pop     hl
        ld      de,CHANNELBYTES
        add     ix,de
        djnz    .chninit
        ret

; Initialise sequence and pattern pointers
; Skip first pattern (instrument definition)
LoopSequences:
        ld      b,9
        ld      ix,Work
        ld      hl,[BGMAddr]            ; Load Sequence Start Addresses
.chninit:
        ld      e,[hl]
        inc     hl
        ld      d,[hl]
        inc     hl

        ld      a,d                     ; Skip first pattern
        or      e
        jr      z,.skip
        inc     de
        inc     de
        inc     de
.skip:
        ld      [ix+SEQPTR],e
        ld      [ix+SEQPTR+1],d
        push    hl

        ex      de,hl
        ld      a,h
        or      l
        ld      a,1
        jr      z,.noplay               ; if hl == 0 channel is not playing
        xor     a
.noplay:ld      [ix+PLYFLG],a
        ld      e,[hl]
        inc     hl
        ld      d,[hl]
        inc     hl
        ld      [ix+PATPTR],e
        ld      [ix+PATPTR+1],d
        ld      a,[hl]
        ld      [ix+PATLP],a

        ld      [ix+LENCTR],1           ; reset length counter

        pop     hl
        ld      de,CHANNELBYTES
        add     ix,de
        djnz    .chninit
        ret


R_PAUREP:
        ld      a,[Playing]
        add     a,a                     ; paused flag in carry
        ret     z                       ; return if not playing
        ccf                             ; complement paused flag
        rra                             ; rotate back paused flag
        ld      [Playing],a
        rlca
        jp      c,UpdateVolumes         ; jump if playing

        ld      a,8                     ; zero volume for PSG#1-2
        ld      e,0
        call    WritePSG
        inc     a
        call    WritePSG

        ld      ix,Work
        ld      c,30h

        ld      b,6
.FMlp:  call    MixVolInstr
        or      00001111b
        ld      e,a
        ld      a,c
        call    WriteFM
        ld      de,CHANNELBYTES
        add     ix,de
        inc     c
        djnz    .FMlp

        ld      a,36h
        ld      e,255
        ld      b,3
.DRUMlp:call    WriteFM
        inc     a
        djnz    .DRUMlp

	ret


; Set Master Volume from table (DE)
R_SETMVO:
        ld      hl,Work+MSTVOL
        ld      bc,CHANNELBYTES
	ld	a,10
.loop:	ex	af,af'
	ld	a,[de]
	inc	de
	ld      [hl],a
        add     hl,bc
        ex	af,af'
        dec	a
        jp	nz,.loop

        ld      a,[Playing]             ; return if not playing
        rlca
        jp      c,UpdateVolumes
        ret


; Get Master Volume to table (DE)
R_GETMVO:
        ld      hl,Work+MSTVOL
        ld      bc,CHANNELBYTES
        ld	a,10
.loop:  ex	af,af'
	ld	a,[hl]
	add     hl,bc
	ld	[de],a
	inc	de
	ex	af,af'
	dec	a
        jr	nz,.loop
        ret


UpdateVolumes:
        ld      h,6
        ld      ix,Work
        ld      l,30h
.FMlp:  call    MixVolInstr
        ld      e,a
        ld      a,l
        call    WriteFM
        ld      bc,CHANNELBYTES
        add     ix,bc
        inc     l
        dec     h
        jr      nz,.FMlp

        call    UpdateDrumVolumes

	ld      ix,WorkPSG

        ld      h,3
        ld      l,8
.PSGlp: call    CalcVolPSG
        ld      e,a
        ld      a,l
        call    WritePSG
        ld      bc,CHANNELBYTES
        add     ix,bc
        inc     l
        dec     h
        jr      nz,.PSGlp

        ret


; WriteFM
;
; In:   A = register
;       E = value
; Chg:  A'
;
; Writes value E to FM register A

WriteFM:
;/;.x:     equ     $+1
;        jp      0
;\;
.Z80:   push    af
        out     [7Ch],a
        ld      a,e
        out     [7Dh],a
        nop
        nop
        pop     af
        ret
;/;
;.R800:  push    bc
;        push    af
;        out     [7Ch],a
;        ld      b,2                     ; wait 2 timerticks
;        call    .wait
;        ld      a,e
;        out     [7Dh],a
;        ld      b,7                     ; wait 7 timerticks
;        call    .wait
;        pop     af
;        pop     bc
;        ret    

;.wait:  in      a,[E6h]                 ; Wait B timer ticks
;        ld      c,a
;.wtlp:  in      a,[E6h]
;        sub     c
;        cp      b
;        jr      c,.wtlp
;        ret     
;\;

; WritePSG
;
; In:   A = register
;       E = value
; Chg:  A'
;
; Writes value E to PSG register A

WritePSG:
        out     [$A0],a
        ex      af,af'
        ld      a,e
        out     [$A1],a
        ex      af,af'
        ret     


R_MSTOP:
        xor     a
        ld      [Playing],a
        ld      [LoopCount],a

        ld      b,10
        ld      ix,Work
        ld      de,CHANNELBYTES
        ld      a,1
.disable:
        ld      [ix+PLYFLG],a		; disable all channels
        add     ix,de
        djnz    .disable

        ld      b,7                     ; reset PSG R#0-6
        ld      e,a
.PSGlp: call    WritePSG
        inc     a
        djnz    .PSGlp

        ld      a,7                     ; reset PSG R#7 (disable tone/noise)
        ld      e,$BF
        call    WritePSG

        ld      e,b                     ; reset PSG R#8-13
        ld      b,6
        ld      a,8
.PSGlp2:
        call    WritePSG
        inc     a
        djnz    .PSGlp2

        xor     a
        ld      e,a
        ld      b,4
        call    WriteMultiFM
        ld      a,4
        ld      e,255
        ld      b,4
        call    WriteMultiFM
        ld      a,10h
        ld      e,0
        call    Write9FM
        ld      a,20h
        ld      e,0
        call    Write9FM
        ld      a,0Eh
        ld      e,0
        call    WriteFM
        ld      a,30h
        ld      e,255
        call    Write9FM
        ld      a,0Eh
        ld      e,20h
        call    WriteFM 
        ld      a,36h
        ld      e,255
        ld      b,3
        call    WriteMultiFM
        ret

Write9FM:
        ld      b,9
WriteMultiFM:
.loop:  call    WriteFM
        inc     a
        djnz    .loop
        ret     


R_MINTER:
        ld      a,[Playing]
        add     a,a
        ret     z                       ; return if not playing
        ret     nc                      ; return if paused

        ld      ix,WorkPSG
        ld      a,7
.PSGlp: ld      [CurrChn],a

        ld      a,[ix+PLYFLG]
        dec	a
        jr	z,.skipPSGchn

        exx                             ; load pattern pointer
        ld      l,[ix+PATPTR]
        ld      h,[ix+PATPTR+1]
        exx
	call    DoPattern
        exx                             ; save new pattern pointer
        ld      [ix+PATPTR],l
        ld      [ix+PATPTR+1],h
        exx
.skipPSGchn:

        ld      bc,CHANNELBYTES
        add     ix,bc

        ld      a,[CurrChn]
        inc     a
        cp      10
        jr      c,.PSGlp

        xor     a
        ld      [CurrChn],a
        ld      ix,Work

        ld      b,6
.FMlp:  push    bc

        ld      a,[ix+PLYFLG]
        dec	a
        jr	z,.skipFMchn

        exx                             ; load pattern pointer
        ld      l,[ix+PATPTR]
        ld      h,[ix+PATPTR+1]
        exx
        call    DoPattern
        exx                             ; save new pattern pointer
        ld      [ix+PATPTR],l
        ld      [ix+PATPTR+1],h
        exx
.skipFMchn:

        ld      bc,CHANNELBYTES
        add     ix,bc
        ld      hl,CurrChn
        inc     [hl]
        pop     bc
        djnz    .FMlp

        exx                             ; load pattern pointer
        ld      l,[ix+PATPTR]
        ld      h,[ix+PATPTR+1]
        exx

        ld      a,[ix+PLYFLG]
        dec	a
        call    nz,DoDrumPattern

        exx                             ; save new pattern pointer
        ld      [ix+PATPTR],l
        ld      [ix+PATPTR+1],h
        exx

        ld      hl,Work+PLYFLG          ; check if any channels are playing
        ld      b,10
.loop:  ld      a,[hl]                  ; return as soon as a playing channel
        or      a                       ; is found
        ret     z

        ld      de,CHANNELBYTES
        add     hl,de
        djnz    .loop

        ld      hl,LoopCount
        inc     [hl]

        ld      hl,Loop
        ld      a,[hl]
        or      a
        jp      z,LoopSequences
        dec     [hl]                    ; decrease no. of loops
        jp      nz,LoopSequences        ; loop if still loops left

        jp      R_MSTOP                 ; stop bgm


CalcVolFM:
        add     a,[ix+MSTVOL]
        cp      15+1
        ret     c
        ld      a,15
        ret     


CalcVolPSG:
        ld      a,[ix+CURVOL]
        call    .base
        ld      b,a
        ld      a,15
        sub     b
        ld      b,a
        ld      a,[ix+ENV8]
        sub     b
        ret     nc

        xor     a
        ret

.base:  sub     [ix+MSTVOL]             ; calculate basevolume from
        ret     nc                      ; master volume
        xor     a
        ret     


; Write current frequencies to FM/PSG
WriteFrqs:
        res     0,d

        ld      a,[CurrChn]
        cp      7
        jr      c,.FM

.PSG:   sub     7
        add     a,a
        ld      e,[ix+CURFRQ]
        call    WritePSG
        inc     a
        ld      e,[ix+CURFRQ+1]
        jp      WritePSG

.FM:    ld      a,[ix+SETTNG]           ; mix Frequency and other bits
        and     30h                     ; get KeyOn/Sustain bits
        or      [ix+CURFRQ+1]           ; mix Frequency MSB
        ld      h,a

        ld      e,[ix+CURFRQ]           ; get Frequency LSB
        ld      a,[CurrChn]
        add     a,10h
        call    WriteFM
        add     a,10h
        ld      e,h
        jp      WriteFM


WriteVolume:
        res     1,d

        ld      a,[CurrChn]
        cp      7
        jr      c,.FM

.PSG:   call    CalcVolPSG
        ld      e,a
        ld      a,[CurrChn]
        inc     a
        jp      WritePSG

.FM:    call    MixVolInstr
        ld      e,a
        ld      a,[CurrChn]
        add	a,30h
        jp	WriteFM


; handle End Of Pattern
DoEOP:  ld      a,[ix+PATLP]
        dec     a
        jr      z,.noloop

        ld      [ix+PATLP],a
        ld      l,[ix+SEQPTR]
        ld      h,[ix+SEQPTR+1]

        ld      e,[hl]                  ; read pattern pointer
        inc     hl
        ld      d,[hl]

        push    de                      ; write new pattern pointer
        exx
        pop     hl
        exx

        scf
        ret

.noloop:
        ld      l,[ix+SEQPTR]           ; increase sequence pointer
        ld      h,[ix+SEQPTR+1]
        inc     hl
        inc     hl
        inc     hl
        ld      [ix+SEQPTR],l
        ld      [ix+SEQPTR+1],h

        ld      e,[hl]                  ; read pattern pointer
        inc     hl
        ld      d,[hl]
        inc     hl                      ; read loop count

        ld      a,e
        or      d
        jr      z,.stop

        ld      a,[hl]
        ld      [ix+PATLP],a

        push    de                      ; write new pattern pointer
        exx
        pop     hl
        exx
        scf
        ret

.stop:  ld      [ix+PLYFLG],-1          ; stop this channel if it is 0
        or      a
        ret


; Mix volume and instrument for FM register
MixVolInstr:
        ld      a,[ix+CURVOL]           ; get current volume
        call    CalcVolFM               ; adjust
        or      [ix+FMINST]             ; mix current fm instrument
        ret

; Mix Tone and Noise parameters for PSG R#7
MixToneNoise:
        ld      a,7                     ; setup R#7 for read
        out     [$A0],a

        ld	a,[WorkPSG+CHANNELBYTES*2+ENV5]
        add	a,a
        ld	e,a
        ld      a,[WorkPSG+CHANNELBYTES+ENV5]
	or	e
        add     a,a
	ld	hl,WorkPSG+ENV5
        or      [hl]
        ld      e,a

        in      a,[$A2]                 ; read R#7
        and     11000000b
        or      e
        ld      e,a
        ld      a,7
        jp      WritePSG


UpdateDrumVolumes:
        ld      bc,[WorkDrum+DRM1]
        ld      h,36h
        call    l6ade
        ld      bc,[WorkDrum+DRM2]
        inc     h
        call    l6ade
        ld      bc,[WorkDrum+DRM4]
        inc     h
l6ade:  ld      a,b
        call    CalcVolFM
        add     a,a
        add     a,a
        add     a,a
        add     a,a
        ld      l,a
        ld      a,c
        call    CalcVolFM
        or      l
        ld      e,a
        ld      a,h
        jp      WriteFM


DoDrumPattern:
        dec     [ix+LENCTR]
        ret     nz
; fallthrough

DoDrumCmd:
        exx                             ; Read a byte from the pattern data
        ld      a,[hl]
        inc     hl
        exx

        cp      $ff
        jr      z,EOPDrum
        cp      $c0
        jr      z,SetRegisterDrum
        bit     7,a
        jp      nz,SetDrumVolumes

        push    af
        ld      e,20h
        ld      a,0eh
        call    WriteFM
        pop     af
        and     1fh
        or      20h
        ld      e,a
        ld      a,0eh
        call    WriteFM

        exx                     ; Read length
        ld      a,[hl]
        inc     hl
        exx
        ld      [ix+LENCTR],a
        ret


EOPDrum:call    DoEOP
        jp      c,DoDrumCmd             ; continue if still playing
        ret


SetRegisterDrum:
        exx                             ; Read a byte from the pattern data
        ld      a,[hl]
        inc     hl
        exx

        ld      b,a

        exx                             ; Read a byte from the pattern data
        ld      a,[hl]
        inc     hl
        exx

        ld      e,a
        ld      a,b
        call    WriteFM
        jp      DoDrumCmd


SetDrumVolumes:
        ld      b,a

        exx                             ; Read a byte from the pattern data
        ld      a,[hl]
        inc     hl
        exx

        bit     4,b
        jr      z,.l6baa
        ld      [WorkDrum+DRM1],a
.l6baa: bit     3,b
        jr      z,.l6bb1
        ld      [WorkDrum+DRM2],a
.l6bb1: bit     2,b
        jr      z,.l6bb8
        ld      [WorkDrum+DRM5],a
.l6bb8: bit     1,b
        jr      z,.l6bbf
        ld      [WorkDrum+DRM4],a
.l6bbf: bit     0,b
        jr      z,.l6bc6
        ld      [WorkDrum+DRM3],a
.l6bc6: call    UpdateDrumVolumes
        jp      DoDrumCmd


DoPattern:
        exx
        ld      a,[hl]
        exx
        cp      $FF
        call    z,DoEOP

        ld      d,[ix+SETTNG]           ; load ix+SETTNG in D
        bit     0,d
        call    nz,WriteFrqs

        bit     1,d
        call    nz,WriteVolume

        ld      a,[ix+LENCTR]           ; final interrupt?
        dec     a
        jr      nz,.end

        bit     6,d                     ; skip if legato is on
        jr      nz,.end

        res     4,d                     ; reset keyon bit
        ld      a,[CurrChn]
        cp      7
        jr      nc,.noFM

        ld      a,d                     ; Output Key Off
        and     30h                     ; get KeyOn/Sustain bits
        or      [ix+CURFRQ+1]           ; mix Frequency MSB
        ld      e,a
        ld      a,[CurrChn]
        add     a,20h
        call    WriteFM
        jp      .end

.noFM:  ld      a,[ix+ENV3H]            ; Envelope Key Off
        ld      [ix+ENV7],a
        ld      [ix+ENV6],3

.end:   ld      [ix+SETTNG],d           ; save ix+SETTNG

        dec     [ix+LENCTR]
        jp      nz,DoFX

        ld      a,[ix+PLYFLG]
        or      a
        jp      nz,DoFX

;fallthrough

DoCmd:  exx                             ; Read a byte from the pattern data
        ld      a,[hl]
        inc     hl
        exx

        cp      60h
        jp      c,DoNote
        cp      $ff
        jr      z,EOP
        cp      70h
        jp      c,DoVolume
        cp      80h
        jp      c,DoFMInstrChg
        add     a,a
        ld      e,a
        ld      d,0
        ld      hl,Cmd8x
        add     hl,de
        ld      e,[hl]
        inc     hl
        ld      d,[hl]
        ex      de,hl
        jp      [hl]

Cmd8x:  dw      SetSustainOff,SetSustainOn,SetRegister,DoInstrChg
        dw      SetLegatoOff,SetLegatoOn,SetFXStep,SetDetune
        dw      SetPortamento,SetVibratoDepth


EOP:    call    DoEOP
        jp      c,DoCmd                 ; continue if still playing
;fallthrough

DoFX:   ld      a,[CurrChn]
        cp      7
        call    nc,DoEnvelope

	dec	[ix+FXSTEP]		; decrease FXSTEP and reload if zero
	ret	nz
	ld	a,[ix+FXRELD]
	ld	[ix+FXSTEP],a

        ld      a,[ix+VIBDEP]		; Check Vibrato
        or      a
	call    nz,DoVibrato
        ld      a,[ix+PORSPD]           ; Check Portamento
        or      a
        jp      nz,DoPortamento
	ret


DoVibrato:
        ld      a,[ix+VIBVAL]
        neg     
        ld      [ix+VIBVAL],a
        ld      e,a                     ; sign extend
        rlca    
        sbc     a,a
        ld      d,a
        ld      l,[ix+TGTFRQ]
        ld      h,[ix+TGTFRQ+1]
        add     hl,de
        ld      [ix+CURFRQ],l
        ld      [ix+CURFRQ+1],h

        set     0,[ix+SETTNG]
        ret


DoPortamento:
        ld      e,[ix+CURFRQ]
        ld      d,[ix+CURFRQ+1]
        ld      l,[ix+TGTFRQ]
        ld      h,[ix+TGTFRQ+1]

        push    hl                      ; cp hl,de
        or      a
        sbc     hl,de
        pop     hl

        ret	z
        jp      c,.down

        ld      l,[ix+PORSPD]
        ld      h,0
        add     hl,de

        ld      a,[CurrChn]
        cp      7
        jr      nc,.noFM

        push    hl                      ; adjust for octave boundary
        ld      a,h
        and     1
        ld      h,a
        ld      de,0156h
        or      a
        sbc     hl,de
        pop     hl
        jr      c,.noFM
        ld      a,h
        and     00001110b
        add     a,2
        ld      h,a
        ld      l,$AB

.noFM:  ld      e,[ix+TGTFRQ]
        ld      d,[ix+TGTFRQ+1]

        push    hl                      ; cp hl,de
        or      a
        sbc     hl,de
        pop     hl

        jr      c,.end
        jp      .attarget

.down:  ld      l,[ix+PORSPD]
        ld      h,0
        ex      de,hl

        or      a
        sbc     hl,de

        ld      a,[CurrChn]
        cp      7
        jr      nc,.noFM_down

        push    hl                      ; adjust for octave boundary
        ld      a,h
        and     1
        ld      h,a
        ld      de,00ABh
        or      a
        sbc     hl,de
        pop     hl
        jr      nc,.noFM_down
        ld      a,h
        and     00001110b
        dec	a
        ld      h,a
        ld      l,55h

.noFM_down:
        ld      e,[ix+TGTFRQ]
        ld      d,[ix+TGTFRQ+1]

        push    hl                      ; cp hl,de
        or      a
        sbc     hl,de
        pop     hl

        jr      nc,.end

.attarget:
        ex      de,hl

.end:   ld      [ix+CURFRQ],l
        ld      [ix+CURFRQ+1],h

        set     0,[ix+SETTNG]
        ret


DoEnvelope:
        dec     [ix+ENV7]
        ret     nz

        ld      a,[ix+ENV6]
        or      a
        jp      nz,.checkdecay

        ; Attack
        ld      a,[ix+ENV8]
        add     a,[ix+ENV0L]
        cp      15
        jr      nc,.godecay
        ld      [ix+ENV8],a

        ld      a,[ix+ENV0H]    ; Reload Attack Rate
        ld      [ix+ENV7],a

        set     1,[ix+SETTNG]
        ret

.godecay:
        ld      a,[ix+ENV1H]    ; Go to Decay State
        ld      [ix+ENV7],a
        ld      [ix+ENV6],1

        ld      [ix+ENV8],15
        set     1,[ix+SETTNG]
        ret

.checkdecay:
        dec     a
        jp      nz,.checksustain

.decay: ld      a,[ix+ENV1H]
        ld      [ix+ENV7],a

        set     1,[ix+SETTNG]

        ld      a,[ix+ENV8]
        sub     [ix+ENV1L]
        jr      c,.gosustain		; Enter sustain (probably level 0)

        ld      [ix+ENV8],a

        cp      [ix+ENV2]
        ret     nc

.gosustain:
        ld      [ix+ENV6],2
        ld      a,[ix+ENV2]
        ld      [ix+ENV8],a
        ret

.checksustain:
        dec     a
        ret     z                       ; Return if Sustain

.release:
        ld      a,[ix+ENV3H]
        ld      [ix+ENV7],a

        set     1,[ix+SETTNG]

        ld      a,[ix+ENV8]
        sub     [ix+ENV3L]
        jr      nc,.end

        ld      [ix+ENV6],2             ; Enter Sustain state with volume 0
        ld      [ix+ENV8],0
        ret

.end:   ld      [ix+ENV8],a
        ret



DoNote: or      a
        jp      z,.rest

        add     a,a
        ld      l,a
        ld      h,0

        ld      a,[CurrChn]
        cp      7
        jp      nc,.noFM

        ld      de,FMFrqTab-2
        add     hl,de
        ld      c,[hl]
        inc     l			; aligned
        ld      b,[hl]

        ld	de,FMVibDetTab
        ld	l,[ix+VIBDEP]
        ld	h,0
        add	hl,de
        ld      a,[hl]
        ld      [ix+VIBVAL],a
        ld      l,[ix+DETUNE]
        ld	h,0
        add	hl,de
        ld      l,[hl]
        ld      h,0
        add     hl,bc
        ld      [ix+TGTFRQ],l
        ld      [ix+TGTFRQ+1],h

        set     4,[ix+SETTNG]

        exx                             ; Read length
        ld      a,[hl]
        inc     hl
        exx
        ld      [ix+LENCTR],a

        ld      a,[ix+PORSPD]
        or      a
        jp      nz,DoFX

        ld      [ix+CURFRQ],l                   ; no portamento
        ld      [ix+CURFRQ+1],h                 ; (TGTFRQ in HL)

        set     0,[ix+SETTNG]
        jp      DoFX

.noFM:  add     hl,hl
        ld      de,PSGFrqTab-4
        add     hl,de
        ld      c,[hl]
        inc     hl
        ld      b,[hl]
        inc     hl
        ld      e,[hl]
        ld      d,0

        ld      a,[ix+VIBDEP]
        or      a
        jr      z,.novibrato
        push    de
        ld      hl,0
        call    CalcDepth
        ld      [ix+VIBVAL],a
        pop     de
.novibrato:
        ld      a,[ix+DETUNE]
        or      a
        ld      hl,0
        jr      z,.nodetune
        call    CalcDepth
        ld      l,a
        ld      h,0
.nodetune:
        add     hl,bc
        ld      [ix+TGTFRQ],l
        ld      [ix+TGTFRQ+1],h

        bit     4,[ix+SETTNG]                   ; skip if key on (legato)
        jr      nz,.legato

        set     4,[ix+SETTNG]

        ld      a,[ix+ENV0H]                    ; Key On
        ld      [ix+ENV7],a                     ; Load Attack Rate
        ld      [ix+ENV6],0                     ; Set Attack State
        ld      [ix+ENV8],0
.legato:

        exx                             ; Read length
        ld      a,[hl]
        inc     hl
        exx
        ld      [ix+LENCTR],a

        ld      a,[ix+PORSPD]
        or      a
        jp      nz,DoFX

        ld      [ix+CURFRQ],l                   ; no portamento
        ld      [ix+CURFRQ+1],h                 ; (TGTFRQ in HL)

        set     0,[ix+SETTNG]
        jp      DoFX

.rest:  res     4,[ix+SETTNG]

        ld      a,[ix+ENV3H]            ; Envelope Key Off
        ld      [ix+ENV7],a
        ld      [ix+ENV6],3

        exx                             ; Read length
        ld      a,[hl]
        inc     hl
        exx
        ld      [ix+LENCTR],a

        jp      DoFX



; Calculate depth for Vibrato / Detune
; In: DE = difference between current and next highest note
;      A = depth value
; Out: H = H * DE / 256
CalcDepth:
        add     a,a
        jr      nc,.nadd0
        add     hl,de
.nadd0: add     hl,hl
        add     a,a
        jr      nc,.nadd1
        add     hl,de
.nadd1: add     hl,hl
        add     a,a
        jr      nc,.nadd2
        add     hl,de
.nadd2: add     hl,hl
        add     a,a
        jr      nc,.nadd3
        add     hl,de
.nadd3: add     hl,hl
        add     a,a
        jr      nc,.nadd4
        add     hl,de
.nadd4: add     hl,hl
        add     a,a
        jr      nc,.nadd5
        add     hl,de
.nadd5: add     hl,hl
        add     a,a
        jr      nc,.nadd6
        add     hl,de
.nadd6: add     hl,hl
        add     a,a
        jr      nc,.end
        add     hl,de

.end:   ld      a,h
        or      a
        ret     nz

        inc     a
        ret     


DoVolume:
        and     15
        ld      [ix+CURVOL],a

        set     1,[ix+SETTNG]
        jp      DoCmd

; Change FM instrument
DoFMInstrChg:
        add     a,a                     ; store instrument in highnybble
        add     a,a
        add     a,a
        add     a,a
        ld      [ix+FMINST],a

        call    MixVolInstr
        ld      e,a
        ld      a,[CurrChn]
        add     a,30h
        call    WriteFM
        jp      DoCmd


SetSustainOff:
        res     5,[ix+SETTNG]
        jp      DoCmd

SetSustainOn:
        set     5,[ix+SETTNG]
        jp      DoCmd


; Change instrument for PSG and FM Custom Instrument
DoInstrChg:
        exx                             ; Read a byte from the pattern data
        ld      a,[hl]
        inc     hl
        exx

        ld      e,a

        exx                             ; Read a byte from the pattern data
        ld      a,[hl]
        inc     hl
        exx

        ld      d,a
        ex      de,hl
        ld      a,[CurrChn]
        cp      7
        jr      nc,.PSG

        xor     a
        ld      b,8
.FMlp:  ld      e,[hl]
        call    WriteFM
        inc     a
        inc     hl
        djnz    .FMlp
        jp      DoCmd

.PSG:   ld      e,ixl
        ld      d,ixh

        ex      de,hl
        ld      bc,ENV
        add     hl,bc
        ex      de,hl

        ldi | ldi | ldi | ldi
        ldi | ldi | ldi | ldi
        ldi

        call    MixToneNoise

        ld      a,[ix+ENV5]
        and     8
        jp      nz,DoCmd
        ld      e,[ix+ENV4]
        ld      a,6
        call    WritePSG
        jp      DoCmd


SetLegatoOff:
        res     6,[ix+SETTNG]
        jp      DoCmd

SetLegatoOn:
        set     6,[ix+SETTNG]
        jp      DoCmd


SetDetune:
        exx                             ; Read a byte from the pattern data
        ld      a,[hl]
        inc     hl
        exx

        ld      [ix+DETUNE],a
        jp      DoCmd


SetPortamento:
        exx                             ; Read a byte from the pattern data
        ld      a,[hl]
        inc     hl
        exx

        ld      [ix+PORSPD],a
        ld      [ix+VIBDEP],0           ; disable vibrato
        ld      l,[ix+TGTFRQ]
        ld      h,[ix+TGTFRQ+1]
        ld      [ix+CURFRQ],l
        ld      [ix+CURFRQ+1],h
        jp      DoCmd

SetVibratoDepth:
        exx                             ; Read a byte from the pattern data
        ld      a,[hl]
        inc     hl
        exx

        ld      [ix+VIBDEP],a
        ld      [ix+PORSPD],0           ; disable portamento
        jp      DoCmd


SetFXStep:
        exx                             ; Read a byte from the pattern data
        ld      a,[hl]
        inc     hl
        exx

        ld      [ix+FXRELD],a
        ld      [ix+FXSTEP],a
        jp      DoCmd

; Read register, read data, and output to PSG or FM.
SetRegister:
        exx                             ; Read a byte from the pattern data
        ld      a,[hl]
        inc     hl
        exx

        ld      b,a

        exx                             ; Read a byte from the pattern data
        ld      a,[hl]
        inc     hl
        exx

        ld      e,a
        ld      a,[CurrChn]
        cp      7
        ld      a,b
        jr      nc,.PSG
        call    WriteFM
        jp      DoCmd

.PSG:   call    WritePSG
        jp      DoCmd


;	align	2

ds 2 - ($ AND (2-1))

FMVibDetTab:
        db      0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
        db      1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
        db      1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2
        db      2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3
        db      3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4
        db      4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5
        db      5,5,5,5,5,5,5,5,5,5,5,5,6,6,6,6
        db      6,6,6,6,6,6,6,6,6,6,6,6,6,6,7,7
        db      7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
        db      8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
        db      8,8,9,9,9,9,9,9,9,9,9,9,9,9,9,9
        db      9,9,9,9,10,10,10,10,10,10,10,10,10,10,10,10
        db      10,10,10,10,10,10,11,11,11,11,11,11,11,11,11,11
        db      11,11,11,11,11,11,11,11,12,12,12,12,12,12,12,12
        db      12,12,12,12,12,12,12,12,12,12,13,13,13,13,13,13
        db      13,13,13,13,13,13,13,13,13,13,13,13,14,14,14,14

FMFrqTab:
        dw      0ABh,0B6h,0C0h,0CCh,0D8h,0E5h   ; O1
        dw      0F2h,101h,110h,120h,132h,144h
        dw      2ABh,2B6h,2C0h,2CCh,2D8h,2E5h   ; O2
        dw      2F2h,301h,310h,320h,332h,344h
        dw      4ABh,4B6h,4C0h,4CCh,4D8h,4E5h   ; O3
        dw      4F2h,501h,510h,520h,532h,544h
        dw      6ABh,6B6h,6C0h,6CCh,6D8h,6E5h   ; O4
        dw      6F2h,701h,710h,720h,732h,744h
        dw      8ABh,8B6h,8C0h,8CCh,8D8h,8E5h   ; O5
        dw      8F2h,901h,910h,920h,932h,944h
        dw      $AAB,$AB6,$AC0,$ACC,$AD8,$AE5   ; O6
        dw      $AF2,$B01,$B10,$B20,$B32,$B44
        dw      $CAB,$CB6,$CC0,$CCC,$CD8,$CE5   ; O7
        dw      $CF2,$D01,$D10,$D20,$D32,$D44
        dw      $EAB,$EB6,$EC0,$ECC,$ED8,$EE5   ; O8
        dw      $EF2,$F01,$F10,$F20,$F32


PSGFrqTab:
   ;    freq,diff to next note
   dw   $D5D,$0C1,$C9C,$0B5,$BE7,$0AB,$B3C,$0A1,$A9B,$099,$A02,$08F ; O1
   dw   $973,$088,$8EB,$080,$86B,$079,$7F2,$072,$780,$066,$71A,$06B
   dw   $6AF,$061,$64E,$05A,$5F4,$056,$59E,$050,$54E,$04D,$501,$047 ; O2
   dw   $4BA,$044,$476,$040,$436,$03D,$3F9,$039,$3C0,$033,$38D,$036
   dw   $357,$030,$327,$02D,$2FA,$02B,$2CF,$028,$2A7,$026,$281,$024 ; O3
   dw   $25D,$022,$23B,$020,$21B,$01E,$1FD,$01D,$1E0,$019,$1C7,$01B
   dw   $1AC,$018,$194,$017,$17D,$015,$168,$015,$153,$013,$140,$012 ; O4
   dw   $12E,$011,$11D,$010,$10D,$00F,$0FE,$00E,$0F0,$00D,$0E3,$00D
   dw   $0D6,$00C,$0CA,$00C,$0BE,$00A,$0B4,$00A,$0AA,$00A,$0A0,$009 ; O5
   dw   $097,$008,$08F,$008,$087,$008,$07F,$007,$078,$006,$072,$007
   dw   $06B,$006,$065,$006,$05F,$005,$05A,$005,$055,$005,$050,$004 ; O6
   dw   $04C,$005,$047,$004,$043,$003,$040,$004,$03C,$003,$039,$004
   dw   $035,$003,$032,$002,$030,$003,$02D,$003,$02A,$002,$028,$002 ; O7
   dw   $026,$002,$024,$002,$022,$002,$020,$002,$01E,$002,$01C,$001
   dw   $01B,$002,$019,$001,$018,$002,$016,$001,$015,$001,$014,$001 ; O8
   dw   $013,$001,$012,$001,$011,$001,$010,$001,$00F,$001
   ; note 00Eh (B8) is not used



;*** WORK AREA
CHANNELBYTES:   equ 32

Work:           equ     $D100                   ; rb 6 * CHANNELBYTES ; FM work area
WorkDrum:       equ     Work + 6 * CHANNELBYTES ; rb 1 * CHANNELBYTES ; Drum work area
WorkPSG:        equ     Work + 7 * CHANNELBYTES ; rb 2 * CHANNELBYTES ; PSG work area
WorkSFX:        equ     Work + 9 * CHANNELBYTES ; rb 1 * CHANNELBYTES ; SFX work area

;Work Area:
SEQPTR: equ     0       ; word  Address within Sequence
PATPTR: equ     2       ; word  Address within Current Pattern
PATLP:  equ     4       ; byte  Pattern Loop counter
PLYFLG: equ     5       ; byte  0 = channel is playing, -1 = channel stopped playing, 1 = channel disabled
MSTVOL: equ     6       ; byte  Master Volume
LENCTR: equ     7       ; byte  Length Counter, decreases until 0, when a new event starts
CURFRQ: equ     8       ; word  Current Frequency
TGTFRQ: equ     10      ; word  Target Frequency
CURVOL: equ     12      ; byte  Current Volume
VIBVAL: equ     13      ; byte  Signed Corrected Vibrato Depth

SETTNG: equ     14      ; byte  Settings        (default: 0)
;                               bit 0: 1 = Write Frequency
;                               bit 1: 1 = Write Volume
;                               bit 2-3: -unused-
;                               bit 4: Key on/off
;                               bit 5: FM Sustain on/off
;                               bit 6: Legato on/off
;                               bit 7: -unused-
FXRELD: equ     15      ; byte  Reload value for FXSTEP (default: 0)
FXSTEP: equ     16      ; byte  Vibrato/Portamento Step Counter (default: 0)
VIBDEP: equ     17      ; byte  Vibrato Depth (default: 0)
PORSPD: equ     18      ; byte  Portamento Speed (default: 0)
DETUNE: equ     19      ; byte  Current Detune value (default: 0)

;Envelope Work Area:
; Instrument Definitions
ENV:    equ     20      ; 12 bytes

                        ; hhhhllll              (default: 0001 1111)
ENV0L:  equ     ENV+0
ENV0H:  equ     ENV+1   ; Attack Rate (1 = fast, 15 = slow)

                        ; aaaabbbb              (default: 0001 1111)
ENV1L:  equ     ENV+2
ENV1H:  equ     ENV+3   ; Decay Rate

ENV2:   equ     ENV+4   ; Sustain Level         (default: 0000 1111)

                        ; aaaabbbb              (default: 0001 1111)
ENV3L:  equ     ENV+5
ENV3H:  equ     ENV+6   ; Release Rate

ENV4:   equ     ENV+7   ; noise frequency       (default: 0)    (PSG ONLY)

ENV5:   equ     ENV+8   ; note/noise select     (default: 8)    (PSG ONLY)

; Work:
ENV6:   equ     ENV+9   ; Envelope State                (default: 0)
                        ; 0 = Attack
                        ; 1 = Decay
                        ; 2 = Sustain
                        ; 3 = Release

ENV7:   equ     ENV+10  ; Envelope Counter              (default: 0)

ENV8:   equ     ENV+11  ; Envelope Volume               (default: 0)

; Drum Volumes
DRM1:   equ     ENV+0   ; (default: 0)
DRM2:   equ     ENV+1   ; (default: 0)
DRM3:   equ     ENV+2   ; (default: 0)
DRM4:   equ     ENV+3   ; (default: 0)
DRM5:   equ     ENV+4   ; (default: 0)

; FM Instrument
FMINST: equ     ENV     ; byte  bit 4-7: Current FM Instrument  (default: 10)
