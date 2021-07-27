		fname	"streetfighter.rom"
	org		$4000
Streetfighter:
;
; Streetfighter - ROM version
;
; Written by: TNI Team
;
	dw		"AB",init,0,0,0,0,0,0
;
; this is one-time only... can be overwritten with game stuff after it's done
;
memInit:	
	phase	$c000
;
initMem:	
	call	whereAmI	            ;Slot of this ROM
	ld		(romSlot),a
	ld		hl,$0000
	call	findRam		            ;Slot of RAM in page 0
	ld		(page0ram),a
	ld		hl,$4000
	call	findRam		            ;Slot of RAM in page 1
	ld		(page1ram),a
	ld		hl,$8000
	call	findRam		            ;Slot of RAM in page 2
	ld		(page2ram),a
	
	;ld	a,(page2ram)
	and		$03
	add		a,a
	add		a,a
	ld		b,a
	ld		a,(page1ram)
	and		$03
	or		b
	add		a,a
	add		a,a
	ld		b,a
	ld		a,(page0ram)
	and		$03
	or		b
	ld		b,a
	call	$138
	and		$c0
	or		b
	ld		b,a
	ld		(slot.ram),a
	
	and		$f3
	ld		c,a
	ld		a,(romSlot)
	and		$03
	add		a,a
	add		a,a
	or		c
	ld		(slot.page1rom),a
	
	ld		a,b
	and		$cf
	ld		c,a
	ld		a,(romSlot)
	and		$03
	add		a,a
	add		a,a
	add		a,a
	add		a,a
	or		c
	ld		(slot.page2rom),a
	
	ld		a,b
	and		$c3
	ld		c,a
	ld		a,(romSlot)
	and		$03
	ld		b,a
	add		a,a
	add		a,a
	or		b
	add		a,a
	add		a,a
	or		c
	ld		(slot.page12rom),a
	
	ld		a,(romSlot)
	ld		h,$80
	call	$24

	;Initialise Fm-Pac
  ld    a,musicreplayerblock  ;set music replayer in page 2, block3+4, at $8000
  ld    ($9000),a
  call  R_INITFM              ;initializes FM-PAC and work area
  ld    a,2
  ld    ($9000),a
	
	ld		a,(page1ram)
	ld		h,$40
	call	$24
	
	ld		a,(romSlot)
	ld		h,$40
	call	$24
	
	ld		b,3
	ld		de,.enaRam
	ld		hl,$3ffd
	push	hl
.ramOn:		
	push	bc
	push	de
	push	hl
	ld		a,(de)
	ld		e,a
	ld		a,(page0ram)
	call	$14
	pop		hl
	pop		de
	pop		bc
	inc		de
	inc		hl
	djnz	.ramOn
	ld		a,(page0ram)
	push	af
	pop		iy
	pop		ix
	ld		(tempStack),sp
	jp		$1c

.done:		
	ld		sp,(tempStack)
	ret

.enaRam:	
	jp		.done

searchSlot:		  db	0
searchAddress:	dw	0
tempStack:		  dw	0
page0ram:		    db	0
page1ram:		    db	0
page2ram:		    db	0
romSlot:		    db	0
;
; Out: A = slot of this ROM (E000SSPP)
;
whereAmI:	
	call	$138
	rrca
	rrca
	and		$03
	ld		c,a
	ld		b,0
	ld		hl,$fcc1
	add		hl,bc
	or		(hl)
	ld		c,a
	inc		hl
	inc		hl
	inc		hl
	inc		hl
	ld		a,(hl)
	and		$0c
	or		c
	ret
;
; In: HL = Address in page to search RAM
; Out: A = RAM slot of the page
;
findRam:	
	ld		bc,4 *256+ 0
	ld		(searchAddress),hl
	ld		hl,$fcc1
.primary.loop:	
	push	bc
	push	hl
	ld		a,(hl)
	bit		7,a
	jr		nz,.secondary
	ld		a,c
	call	.check
.primary.next:	
	pop		hl
	pop		bc
	ld		a,(searchSlot)
	ret		c
	inc		hl
	inc		c
	djnz	.primary.loop
	ld		a,-1			            ;should normally never occur
	ld		(searchSlot),a
	ret

.secondary:	
	and		$80
	or		c
	ld		b,4
.sec.loop:	
	push	bc
	call	.check
	pop		bc
	jr		c,.primary.next
	add		a,4
	djnz	.sec.loop
	jr		.primary.next

.check:		
	ld		(searchSlot),a
	call	.read
	ld		b,a
	cpl
	ld		c,a
	call	.write
	call	.read
	cp		c
	jr		nz,.noram
	cpl
	call	.write
	call	.read
	cp		b
	jr		nz,.noram
	ld		a,(searchSlot)
	scf
	ret
.noram:		
	ld		a,(searchSlot)
	or		a
	ret

.read:		
	push	bc
	push	hl
	ld		a,(searchSlot)
	ld		hl,(searchAddress)
	call	$0c
	pop		hl
	pop		bc
	ret

.write:		
	push	bc
	push	hl
	ld		e,a
	ld		a,(searchSlot)
	ld		hl,(searchAddress)
	call	$14
	pop		hl
	pop		bc
	ret

initMem.length:	equ	$-initMem
		dephase
;
; end of one-time only code...
;

;
init:		
;readslot, check if manbow2 is in slot 2
;  ld    a,slot
;  ld    hl,adres om te lezen
;  call  $0c

  xor   a
	ld		($f3e9),a	            ;foreground color 
	ld		($f3ea),a	            ;background color 
	ld		($f3eb),a	            ;border color

	ld 		a,5                   ;switch to screen 5
	call 	$5f

	ld		a,(VDP_8+1)
	and		%0111 1101	          ;set 60 hertz
	or		%1000 0000	          ;screen height 212
	ld		(VDP_8+1),a
	di
	out		($99),a
	ld		a,9+128
	ei
	out		($99),a

;screenmode transparancy
	ld		a,(vdp_8)
	or		32				            ;transparant mode off
;	and		223				            ;tranparant mode on
	ld		(vdp_8),a
	di
	out		($99),a
	ld		a,8+128
	ei
	out		($99),a

;SpriteInitialize:
	ld		a,(vdp_0+1)
	or		2			                ;sprites 16*16
  ld    (VDP_0+1),a
	di
	out		($99),a
	ld		a,1+128
	ei
	out		($99),a
;/SpriteInitialize:

	di
	im		1
	ld		bc,initMem.length
	ld		de,initMem
	ld		hl,memInit
	ldir
	call	initMem

	xor		a			                ;init blocks
	ld		(memblocks.1),a
	ld		($5000),a
	inc		a
	ld		(memblocks.2),a
	ld		($7000),a
	inc		a
	ld		(memblocks.3),a
	ld		($9000),a
	inc		a
	ld		(memblocks.4),a
	ld		($b000),a

; load BIOS / engine , load startup
	ld		hl,tempisr
	ld		de,$38
	ld		bc,6
	ldir

	ld		hl,engine
	ld		de,engaddr
	ld		bc,enginelength	      ;load engine (this part of the engine goes to page 0)
	ldir

	ld		hl,enginepage3
	ld		de,enginepage3addr
	ld		bc,enginepage3length  ;load enginepage3
	ldir

	ld		a,(slot.page12rom)	  ;all RAM except page 1+2
	out		($a8),a	
  
  ;set loader block and go to loader
	ld		a,loaderblock
	call	block34			          ;at address $8000 / page 2
  jp    loader.address        ;set loader in page 2 and jp to it

tempisr:	                    ;set temp ISR
	push	af
	in		a,($99)               ;check and acknowledge vblank int (ei0 is set)
	pop		af
	ei	
	ret

; 
; block 00 - 02 engine 
;	
enginepage3:
	include	"enginepage3.asm"	

; 
; block 00 - 02 engine 
;	
engine:
	include	"engine.asm"	

;
;fill remainder of blocks 0-2
;
	ds		$a000-$,$ff		

;
; block $03 - $04
;
loaderblock:  equ $03
phase	$8000
loaderstart:
	include "loader.asm"
loaderend:
	ds		$c000-$,$ff
dephase

;
; block $05
;
musicreplayerblock:           equ $05
phase	$8000
  include "..\fmpac music\guyver5.asm"
	ds		$a000-$,$ff
dephase

;
; block $06
;
scoreboardblock:              equ $06
phase	$4000
scoreboard:
  incbin "..\grapx\scoreboard\scoreboard.SC5",7,12*128  ;skip header
	ds		$6000-$,$ff
dephase

;
; block $07 - $08
;
samples1block:          equ $07
phase	$0000
SoftAttacksample:
  incbin "..\sampleplayer\SoftAttack.raw"
SoftAttacksampleLenght:       equ $-SoftAttacksample
HardAttacksample:
  incbin "..\sampleplayer\HardAttack.raw"
HardAttacksampleLenght:       equ $-HardAttacksample
SoftPunchHitsample:
  incbin "..\sampleplayer\SoftPunchHit.raw"
SoftPunchHitsampleLenght:     equ $-SoftPunchHitsample
HardPunchHitsample:
  incbin "..\sampleplayer\HardPunchHit.raw"
HardPunchHitsampleLenght:     equ $-HardPunchHitsample
SoftKickHitsample:
  incbin "..\sampleplayer\SoftKickHit.raw"
SoftKickHitsampleLenght:      equ $-SoftKickHitsample
Malethrowsample:
  incbin "..\sampleplayer\Malethrow.raw"
MalethrowsampleLenght:        equ $-Malethrowsample
	ds		$4000-$,$ff
dephase

;
; block $09 - $0a
;
samples2block:          equ $09
phase	$0000
Femalethrowsample:
  incbin "..\sampleplayer\Femalethrow.raw"
FemalethrowsampleLenght:      equ $-Femalethrowsample
HardKickHitsample:
  incbin "..\sampleplayer\HardKickHit.raw"
HardKickHitsampleLenght:      equ $-HardKickHitsample
Defendsample:
  incbin "..\sampleplayer\Defend.raw"
DefendsampleLenght:           equ $-Defendsample
MaleDiessample:
  incbin "..\sampleplayer\ryu dies.raw"
MaleDiessampleLenght:         equ $-MaleDiessample
Landingsample:
  incbin "..\sampleplayer\Landing.raw"
LandingsampleLenght:          equ $-Landingsample
	ds		$4000-$,$ff
dephase

;
; block $0b - $0c
;
samples3block:          equ $0b
phase	$0000
FemaleDiessample:
  incbin "..\sampleplayer\chunli dies.raw"
FemaleDiessampleLenght:       equ $-FemaleDiessample
KnockDownsample:
  incbin "..\sampleplayer\KnockDown.raw"
KnockDownsampleLenght:        equ $-KnockDownsample
onesample:
  incbin "..\sampleplayer\one.raw"
onesampleLenght:              equ $-onesample
twosample:
  incbin "..\sampleplayer\two.raw"
twosampleLenght:              equ $-twosample
	ds		$4000-$,$ff
dephase


;
; block $0d - $0e
;
samples4block:          equ $0d
phase	$0000
finalsample:
  incbin "..\sampleplayer\final.raw"
finalsampleLenght:            equ $-finalsample
roundsample:
  incbin "..\sampleplayer\round.raw"
roundsampleLenght:            equ $-roundsample
perfectsample:
  incbin "..\sampleplayer\perfect.raw"
perfectsampleLenght:          equ $-perfectsample
yousample:
  incbin "..\sampleplayer\you.raw"
yousampleLenght:              equ $-yousample
	ds		$4000-$,$ff
dephase

;
; block $0f
;
samples5block:          equ $0f
phase	$0000
losesample:
  incbin "..\sampleplayer\lose.raw"
losesampleLenght:             equ $-losesample
fightsample:
  incbin "..\sampleplayer\fight.raw"
fightsampleLenght:            equ $-fightsample
	ds		$2000-$,$ff
dephase

;
; block $10
;
samples6block:          equ $10
phase	$0000
winsample:
  incbin "..\sampleplayer\win.raw"
winsampleLenght:              equ $-winsample
threesample:
  incbin "..\sampleplayer\three.raw"
threesampleLenght:            equ $-threesample

	ds		$2000-$,$ff
dephase


;
; block $11 - $13
;
unlockedcharacterfacesblock:  equ $11
phase	$0000
  incbin "..\grapx\character select\unlocked character faces.SC8",7,96*256
	ds		$6000-$,$ff
dephase

;
; block $14 - $15
;
lockedcharacterfacesblock:    equ $14
phase	$0000
  incbin "..\grapx\character select\locked character faces.SC8",7,56*256
	ds		$4000-$,$ff
dephase

;
; block $16
;
Capcomlogoblock:              equ $16
phase	$4000
  incbin "..\fmpac music\capcomlo.tmf"
CapcomlogoAddress:
  incbin "..\grapx\capcom logo\capcom logo.SC5",7,40*128  ;skip header
Capcomlogopalette:
  incbin "..\grapx\capcom logo\capcom logo palette.plt"
	ds		$6000-$,$ff
dephase



;
; block $17 - $1b
;
worldmapblock:                equ $17
phase	$0000
  incbin "..\grapx\character select\world map.SC8",7,141*256
	ds		$a000-$,$ff
dephase

;
; block $1c - $1d
;
selectstageblock:             equ $1c
phase	$0000
  incbin "..\grapx\character select\selectstage.SC8",7,44*256
	ds		$4000-$,$ff
dephase

;
; block $1e
;
StartFightTuneblock:          equ $1e
phase	$0000
  incbin "..\fmpac music\startfig.tmf"
	ds		$2000-$,$ff
dephase

;
; block $1f - $21
;
Defeatedcharacterfacesblock:  equ $1f
phase	$0000
  incbin "..\grapx\character select\defeated character faces.SC8",7,84*256
	ds		$6000-$,$ff
dephase

;
; block $22
;
Continueblock:                equ $22
phase	$4000
  incbin "..\fmpac music\continue.tmf"
ContinueTilesAddress:
  incbin "..\grapx\continue\continue tiles.SC5",7,14*128
  incbin "..\grapx\continue\continue.plt",0,32
	ds		$6000-$,$ff
dephase

;
; block $23 - $24
;
Congratulationsblock:         equ $23
phase	$4000
  incbin "..\fmpac music\gameover.tmf"
CongratulationsTilesAddress:
  incbin "..\grapx\congratulations\congratulations.SC5",7,28*128
  incbin "..\grapx\congratulations\congratulations.plt",0,32
	ds		$8000-$,$ff
dephase

;
; block $25
;
chselectmusicblock:           equ $25
phase	$0000
  incbin "..\fmpac music\chselect.tmf"
	ds		$2000-$,$ff
dephase

;
; block $26 - $27
;
streetfighterlogoblock:       equ $26
phase	$0000
  incbin "..\grapx\main menu\street fighter logo.SC5",7,112*128
	ds		$4000-$,$ff
dephase

;
; block $28 - $29
;
mainmenuoptionsblock:         equ $28
phase	$4000
  incbin "..\grapx\main menu\arcade and vs.SC5",7,29*128
  incbin "..\grapx\main menu\trials and option.SC5",7,29*128
mainmenuselectionicongraphics:
  incbin "..\grapx\main menu\main menu selection icons.SC5",7,32*128
	ds		$8000-$,$ff
dephase



;
; block $2a - $35
;
Streetfighterlogoaniblock:    equ $2a
phase	$4000
;logo1pck:
;  incbin "..\grapx\streetfighter logo\logo01.raw.pck"
logo2pck:
  incbin "..\grapx\streetfighter logo\logo02.raw.pck"
logo3pck:
  incbin "..\grapx\streetfighter logo\logo03.raw.pck"
logo4pck:
  incbin "..\grapx\streetfighter logo\logo04.raw.pck"
logo5pck:
  incbin "..\grapx\streetfighter logo\logo05.raw.pck"
logo6pck:
  incbin "..\grapx\streetfighter logo\logo06.raw.pck"
logo7pck:
  incbin "..\grapx\streetfighter logo\logo07.raw.pck"
logo8pck:
  incbin "..\grapx\streetfighter logo\logo08.raw.pck"
logo9pck:
  incbin "..\grapx\streetfighter logo\logo09.raw.pck"
	ds		$1c000-$,$ff
dephase







;
; block $36 - $37
; block $38 - $39
; block $3a - $3b
; block $3c - $3d
;
ryuframelistblock:            equ $36
phase	$8000
  include "..\grapx\ryu\spritesryuPage0\frames.lst" 
	ds		$c000-$,$ff
dephase
phase	$8000
  include "..\grapx\ryu\spritesryuPage1\frames.lst" 
	ds		$c000-$,$ff
dephase
phase	$8000
  include "..\grapx\ryu\spritesryuPage2\frames.lst" 
	ds		$c000-$,$ff
dephase
phase	$8000
  include "..\grapx\ryu\spritesryuPage3\frames.lst" 
	ds		$c000-$,$ff
dephase

;
; block $3e - $3f
; block $40 - $41
; block $42 - $43
; block $44 - $45
;
ryuspritedatablock:           equ $3e
phase	$0000
  incbin "..\grapx\ryu\spritesryuPage0\frames.dat"
	ds		$4000-$,$ff
dephase
phase	$0000
  incbin "..\grapx\ryu\spritesryuPage1\frames.dat"
	ds		$4000-$,$ff
dephase
phase	$0000
  incbin "..\grapx\ryu\spritesryuPage2\frames.dat"
	ds		$4000-$,$ff
dephase
phase	$0000
  incbin "..\grapx\ryu\spritesryuPage3\frames.dat"
	ds		$4000-$,$ff
dephase

;
; block $46
;
ryucountrysampleblock:        equ $46
phase	$0000
ryucountrysample:
  incbin "..\sampleplayer\japan.raw"
ryucountrysampleLenght:       equ $-ryucountrysample
	ds		$2000-$,$ff
dephase

;
; block $47
;
ryuspecial1sampleblock:       equ $47
phase	$0000
ryuspecial1sample:
  incbin "..\sampleplayer\hadouken.raw"
ryuspecial1sampleLenght:      equ $-ryuspecial1sample
	ds		$2000-$,$ff
dephase

;
; block $48
;
ryuspecial2sampleblock:       equ $48
phase	$0000
ryuspecial2sample:
  incbin "..\sampleplayer\shoryuken.raw"
ryuspecial2sampleLenght:      equ $-ryuspecial2sample
	ds		$2000-$,$ff
dephase

;
; block $49
;
ryuspecial3sampleblock:       equ $49
phase	$0000
ryuspecial3sample:
  incbin "..\sampleplayer\hurricane kick ryu.raw"
ryuspecial3sampleLenght:      equ $-ryuspecial3sample
	ds		$2000-$,$ff
dephase

;
; block $4a - $4d
;
backgroundryublock:           equ $4a
phase	$0000
  incbin "..\grapx\ryu\background\ryu background.SC5",7,212*128 ;skip header
backgroundryupalette:
  incbin "..\grapx\ryu\background\ryu background palette.plt",0,32
scoreboardfaceryu:
  incbin "..\grapx\ryu\scoreboardface\scoreboardfaceryu.dat"
ryupalette:
  incbin "..\grapx\ryu\ryu palette\ryu palette.plt",0,6   ;only 3 unique colors per player
ryualtpalette:
  incbin "..\grapx\ryu\ryu palette\ryu altpalette.plt",0,6   ;only 3 unique colors per player
	ds		$8000-$,$ff
dephase

;
; block $4e
;
ryucharacterfaceblock:        equ $4e
ryuactiontablesblock:         equ $4e
phase	$4000
  incbin "..\grapx\character select\character faces\ryucharacterface.dat"
  include "actiontables\ryuactiontables.asm"
	ds		$6000-$,$ff
dephase











;
; block $4f
;
ryumusicblock:                equ $4f
phase	$0000
  incbin "..\fmpac music\ryu.tmf"
	ds		$2000-$,$ff
dephase




;
; block $50 - $51
; block $52 - $53
; block $54 - $55
; block $56 - $57
;
chunliframelistblock:        equ $50
phase	$8000
  include "..\grapx\chunli\spriteschunliPage0\frames.lst" 
	ds		$c000-$,$ff
dephase
phase	$8000
  include "..\grapx\chunli\spriteschunliPage1\frames.lst" 
	ds		$c000-$,$ff
dephase
phase	$8000
  include "..\grapx\chunli\spriteschunliPage2\frames.lst" 
	ds		$c000-$,$ff
dephase
phase	$8000
  include "..\grapx\chunli\spriteschunliPage3\frames.lst" 
	ds		$c000-$,$ff
dephase

;
; block $58 - $59
; block $5a - $5b
; block $5c - $5d
; block $5e - $5f
;
chunlispritedatablock:       equ $58
phase	$0000
  incbin "..\grapx\chunli\spriteschunliPage0\frames.dat"
	ds		$4000-$,$ff
dephase
phase	$0000
  incbin "..\grapx\chunli\spriteschunliPage1\frames.dat"
	ds		$4000-$,$ff
dephase
phase	$0000
  incbin "..\grapx\chunli\spriteschunliPage2\frames.dat"
	ds		$4000-$,$ff
dephase
phase	$0000
  incbin "..\grapx\chunli\spriteschunliPage3\frames.dat"
	ds		$4000-$,$ff
dephase

;
; block $60
;
chunlicountrysampleblock:     equ $60
phase	$0000
chunlicountrysample:
  incbin "..\sampleplayer\china.raw"
chunlicountrysampleLenght:    equ $-chunlicountrysample
	ds		$2000-$,$ff
dephase

;
; block $61
;
chunlispecial1sampleblock:    equ $61
phase	$0000
chunlispecial1sample:
  incbin "..\sampleplayer\kikouken.raw"
chunlispecial1sampleLenght:   equ $-chunlispecial1sample
	ds		$2000-$,$ff
dephase

;
; block $62
;
chunlispecial2sampleblock:    equ $62
phase	$0000
chunlispecial2sample:
  incbin "..\sampleplayer\lightning kick.raw"
chunlispecial2sampleLenght:   equ $-chunlispecial2sample
	ds		$2000-$,$ff
dephase

;
; block $63
;
chunlispecial3sampleblock:    equ $63
phase	$0000
chunlispecial3sample:
  incbin "..\sampleplayer\spinning bird kick.raw"
chunlispecial3sampleLenght:   equ $-chunlispecial3sample
	ds		$2000-$,$ff
dephase

;
; block $64 - $67
;
backgroundchunliblock:        equ $64
phase	$0000
  incbin "..\grapx\chunli\background\chunli background.SC5",7,212*128 ;skip header
backgroundchunlipalette:
  incbin "..\grapx\chunli\background\chunli background palette.plt",0,32
scoreboardfacechunli:
  incbin "..\grapx\chunli\scoreboardface\scoreboardfacechunli.dat"
chunlipalette:
  incbin "..\grapx\chunli\chunli palette\chunli palette.plt",0,6   ;only 3 unique colors per player
chunlialtpalette:
  incbin "..\grapx\chunli\chunli palette\chunli altpalette.plt",0,6   ;only 3 unique colors per player
	
	
	ds		$8000-$,$ff
dephase

;
; block $68
;
chunlicharacterfaceblock:     equ $68
chunliactiontablesblock:      equ $68
phase	$4000
  incbin "..\grapx\character select\character faces\chunlicharacterface.dat"
  include "actiontables\chunliactiontables.asm"
	ds		$6000-$,$ff
dephase




;
; block $69
;
chunlimusicblock:             equ $69
phase	$0000
  incbin "..\fmpac music\chunli.tmf"
	ds		$2000-$,$ff
dephase









;
; block $6a - $6b
; block $6c - $6d
; block $6e - $6f
; block $70 - $71
;
dhalsimframelistblock:        equ $6a
phase	$8000
  include "..\grapx\dhalsim\spritesdhalsimPage0\frames.lst" 
	ds		$c000-$,$ff
dephase
phase	$8000
  include "..\grapx\dhalsim\spritesdhalsimPage1\frames.lst" 
	ds		$c000-$,$ff
dephase
phase	$8000
  include "..\grapx\dhalsim\spritesdhalsimPage2\frames.lst" 
	ds		$c000-$,$ff
dephase
phase	$8000
  include "..\grapx\dhalsim\spritesdhalsimPage3\frames.lst" 
	ds		$c000-$,$ff
dephase

;
; block $72 - $73
; block $74 - $75
; block $76 - $77
; block $78 - $79
;
dhalsimspritedatablock:       equ $72
phase	$0000
  incbin "..\grapx\dhalsim\spritesdhalsimPage0\frames.dat"
	ds		$4000-$,$ff
dephase
phase	$0000
  incbin "..\grapx\dhalsim\spritesdhalsimPage1\frames.dat"
	ds		$4000-$,$ff
dephase
phase	$0000
  incbin "..\grapx\dhalsim\spritesdhalsimPage2\frames.dat"
	ds		$4000-$,$ff
dephase
phase	$0000
  incbin "..\grapx\dhalsim\spritesdhalsimPage3\frames.dat"
	ds		$4000-$,$ff
dephase

;
; block $7a
;
dhalsimcountrysampleblock:    equ $7a
phase	$0000
dhalsimcountrysample:
  incbin "..\sampleplayer\india.raw"
dhalsimcountrysampleLenght:   equ $-dhalsimcountrysample
	ds		$2000-$,$ff
dephase

;
; block $7b
;
dhalsimspecial1sampleblock:   equ $7b
phase	$0000
dhalsimspecial1sample:
  incbin "..\sampleplayer\joga fire.raw"
dhalsimspecial1sampleLenght:   equ $-dhalsimspecial1sample
	ds		$2000-$,$ff
dephase

;
; block $7c
;
dhalsimspecial2sampleblock:   equ $7c
phase	$0000
dhalsimspecial2sample:
  incbin "..\sampleplayer\joga flame.raw"
dhalsimspecial2sampleLenght:   equ $-dhalsimspecial2sample
	ds		$2000-$,$ff
dephase

;
; block $7d
;
dhalsimspecial3sampleblock:   equ $7d
phase	$0000
dhalsimspecial3sample:
  incbin "..\sampleplayer\spinning bird kick.raw"
dhalsimspecial3sampleLenght:   equ $-dhalsimspecial3sample
	ds		$2000-$,$ff
dephase

;
; block $7e - $81
;
backgrounddhalsimblock:       equ $7e
phase	$0000
  incbin "..\grapx\dhalsim\background\dhalsim background.SC5",7,212*128 ;skip header
backgrounddhalsimpalette:
  incbin "..\grapx\dhalsim\background\dhalsim background palette.plt",0,32
scoreboardfacedhalsim:
  incbin "..\grapx\dhalsim\scoreboardface\scoreboardfacedhalsim.dat"
dhalsimpalette:
  incbin "..\grapx\dhalsim\dhalsim palette\dhalsim palette.plt",0,6   ;only 3 unique colors per player
dhalsimaltpalette:
  incbin "..\grapx\dhalsim\dhalsim palette\dhalsim altpalette.plt",0,6   ;only 3 unique colors per player
	ds		$8000-$,$ff
dephase

;
; block $82
;
dhalsimcharacterfaceblock:    equ $82
dhalsimactiontablesblock:     equ $82
phase	$4000
  incbin "..\grapx\character select\character faces\dhalsimcharacterface.dat"
  include "actiontables\dhalsimactiontables.asm"
	ds		$6000-$,$ff
dephase

;
; block $83
;
dhalsimmusicblock:            equ $83
phase	$0000
  incbin "..\fmpac music\dhalsim.tmf"
	ds		$2000-$,$ff
dephase











;
; block $84 - $85
;
Samples7block:                equ $84
phase	$0000
menuselectsample:
  incbin "..\sampleplayer\menuselect.raw"
menuselectsampleLenght:       equ $-menuselectsample
countdownsample:
  incbin "..\sampleplayer\countdown.raw"
countdownsampleLenght:        equ $-countdownsample
	ds		$4000-$,$ff
dephase

;
; block $86 - $87
;
Samples8block:                equ $86
phase	$0000
planesample:
  incbin "..\sampleplayer\plane.raw"
planetsampleLenght:           equ $-planesample
	ds		$4000-$,$ff
dephase

;
; block $88 - $89
;
Samples9block:                equ $88
phase	$0000
characterselectedsample:
  incbin "..\sampleplayer\character selected.raw"
characterselectedsampleLenght:equ $-characterselectedsample
	ds		$4000-$,$ff
dephase

;
; block $8a - $8b
;
Samples10block:               equ $8a
phase	$0000
cursormovesample:
  incbin "..\sampleplayer\cursor move.raw"
cursormovesampleLenght:       equ $-cursormovesample
	ds		$4000-$,$ff
dephase


















LenghtIdleAction:             equ RyuActions.RightIdleFrame-RyuActions.LeftIdleFrame
LenghtBendAction:             equ RyuActions.RightBendFrame-RyuActions.LeftBendFrame
LenghtWalkAction:             equ RyuActions.LeftWalkRightFrame-RyuActions.LeftWalkLeftFrame
LenghtHorSpeedWalkTable:      equ RyuActions.HorSpeedWalkFastTable-RyuActions.HorSpeedWalkSlowTable
LenghtJumpAnimationTable:     equ RyuActions.LeftJumpLeftStartframe-RyuActions.LeftJumpStraightStartframe
LenghtJumpTable:              equ RyuActions.HorSpeedJumpSlowTable-RyuActions.jumptable
LenghtHorSpeedJumpTable:      equ RyuActions.HorSpeedJumpFastTable-RyuActions.HorSpeedJumpSlowTable
LenghtStandSoftPunchTable:    equ RyuActions.StandSoftPunchRightFrame-RyuActions.StandSoftPunchLeftFrame
LenghtSitHardKickTable:       equ RyuActions.SitHardKickRightFrame-RyuActions.SitHardKickLeftFrame
LenghtDefendTable:            equ RyuActions.RightStandDefendFrame-RyuActions.LeftStandDefendFrame
LenghtStandHitTable:          equ RyuActions.RightStandHitFrame-RyuActions.LeftStandHitFrame
LenghtHeavilyHitTable:        equ RyuActions.HeavilyHitRightFrame-RyuActions.HeavilyHitLeftFrame
LenghtKnockDownRecoverTable:  equ RyuActions.KnockDownRecoverRightFrame-RyuActions.KnockDownRecoverLeftFrame
LenghtTossTable:              equ RyuActions.TossRightFrame-RyuActions.TossLeftFrame
LenghtVictoryTable:           equ RyuActions.VictoryRightFrame-RyuActions.VictoryLeftFrame
LenghtSpecialMovementTable:   equ RyuActions.Special2MovementTablePointer-RyuActions.Special1MovementTablePointer
LenghtSpecialVariableTable:   equ RyuActions.VariableTableSpecial2-RyuActions.VariableTableSpecial1
LenghtSpecialFrameTable:      equ RyuActions.Special1RightFrame-RyuActions.Special1LeftFrame
LenghtGettingTossedFrameTable:equ RyuActions.GettingTossedRightFrame-RyuActions.GettingTossedLeftFrame
LenghtDamageTabel:            equ RyuActions.endDamageTabel-RyuActions.DamageTabel
LenghtProjectileTabel:        equ RyuActions.ProjectileRightFrame-RyuActions.ProjectileLeftFrame
LenghtProjectileEndTabel:     equ RyuActions.ProjectileRightEndFrame-RyuActions.ProjectileLeftEndFrame
LenghtPlayerDiedTabel:        equ RyuActions.DiedRightFrame-RyuActions.DiedLeftFrame




totallenght:	Equ	$-Streetfighter
;	ds		$80000-totallenght