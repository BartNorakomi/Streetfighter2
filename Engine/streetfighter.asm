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

  xor   a                     ;center screen
  di
  out   ($99),a
  ld    a,18+128
  ei
  out   ($99),a

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

guilespecial2sampleblock:   equ $07
guilespecial2sample:
HardAttacksample:
  incbin "..\sampleplayer\HardAttack.raw"
HardAttacksampleLenght:       equ $-HardAttacksample
guilespecial2sampleLenght:   equ $-guilespecial2sample

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
  incbin "..\sampleplayer\male dies.raw"
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
  incbin "..\sampleplayer\female dies.raw"
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
; block $2a - $2e
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
	ds		$e000-$,$ff
dephase

;
; block $2f - $30
;
Samples7block:                equ $2f
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
; block $31 - $32
;
Samples8block:                equ $31
phase	$0000
planesample:
  incbin "..\sampleplayer\plane.raw"
planetsampleLenght:           equ $-planesample
	ds		$4000-$,$ff
dephase

;
; block $33 - $34
;
Samples9block:                equ $33
phase	$0000
characterselectedsample:
  incbin "..\sampleplayer\character selected.raw"
characterselectedsampleLenght:equ $-characterselectedsample
	ds		$4000-$,$ff
dephase

;
; block $35 - $36
;
Samples10block:               equ $35
phase	$0000
cursormovesample:
  incbin "..\sampleplayer\cursor move.raw"
cursormovesampleLenght:       equ $-cursormovesample
	ds		$4000-$,$ff
dephase






 ds 9 * $2000












;
; block $40 - $41
; block $42 - $43
; block $44 - $45
; block $46 - $47
;
ryuframelistblock:            equ $40
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
; block $48 - $49
; block $4a - $4b
; block $4c - $4d
; block $4e - $4f
;
ryuspritedatablock:           equ $48
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
; block $50
;
ryucountrysampleblock:        equ $50
ehondacountrysampleblock:     equ $50
phase	$0000
ryucountrysample:
ehondacountrysample:
  incbin "..\sampleplayer\countries\japan.raw"
ryucountrysampleLenght:       equ $-ryucountrysample
ehondacountrysampleLenght:   equ $-ehondacountrysample
	ds		$2000-$,$ff
dephase

;
; block $51
;
ryuspecial1sampleblock:       equ $51
phase	$0000
ryuspecial1sample:
  incbin "..\sampleplayer\ryu\hadouken.raw"
ryuspecial1sampleLenght:      equ $-ryuspecial1sample
	ds		$2000-$,$ff
dephase

;
; block $52
;
ryuspecial2sampleblock:       equ $52
phase	$0000
ryuspecial2sample:
  incbin "..\sampleplayer\ryu\shoryuken.raw"
ryuspecial2sampleLenght:      equ $-ryuspecial2sample
	ds		$2000-$,$ff
dephase

;
; block $53
;
ryuspecial3sampleblock:       equ $53
phase	$0000
ryuspecial3sample:
  incbin "..\sampleplayer\ryu\hurricane kick ryu.raw"
ryuspecial3sampleLenght:      equ $-ryuspecial3sample
	ds		$2000-$,$ff
dephase

;
; block $54 - $57
;
backgroundryublock:           equ $54
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
; block $58
;
ryucharacterfaceblock:        equ $58
ryuactiontablesblock:         equ $58
phase	$4000
  incbin "..\grapx\character select\character faces\ryucharacterface.dat"
  include "actiontables\ryuactiontables.asm"
	ds		$6000-$,$ff
dephase

;
; block $59
;
ryumusicblock:                equ $59
phase	$0000
  incbin "..\fmpac music\ryu.tmf"
	ds		$2000-$,$ff
dephase




;
; block $5a - $5b
; block $5c - $5d
; block $5e - $5f
; block $60 - $61
;
chunliframelistblock:        equ $5a
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
; block $62 - $63
; block $64 - $65
; block $66 - $67
; block $68 - $69
;
chunlispritedatablock:       equ $62
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
; block $6a
;
chunlicountrysampleblock:     equ $6a
phase	$0000
chunlicountrysample:
  incbin "..\sampleplayer\countries\china.raw"
chunlicountrysampleLenght:    equ $-chunlicountrysample
	ds		$2000-$,$ff
dephase

;
; block $6b
;
chunlispecial1sampleblock:    equ $6b
phase	$0000
chunlispecial1sample:
  incbin "..\sampleplayer\chunli\kikouken.raw"
chunlispecial1sampleLenght:   equ $-chunlispecial1sample
	ds		$2000-$,$ff
dephase

;
; block $6c
;
chunlispecial2sampleblock:    equ $6c
phase	$0000
chunlispecial2sample:
  incbin "..\sampleplayer\chunli\lightning kick.raw"
chunlispecial2sampleLenght:   equ $-chunlispecial2sample
	ds		$2000-$,$ff
dephase

;
; block $6d
;
chunlispecial3sampleblock:    equ $6d
phase	$0000
chunlispecial3sample:
  incbin "..\sampleplayer\chunli\spinning bird kick.raw"
chunlispecial3sampleLenght:   equ $-chunlispecial3sample
	ds		$2000-$,$ff
dephase

;
; block $6e - $71
;
backgroundchunliblock:        equ $6e
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
; block $72
;
chunlicharacterfaceblock:     equ $72
chunliactiontablesblock:      equ $72
phase	$4000
  incbin "..\grapx\character select\character faces\chunlicharacterface.dat"
  include "actiontables\chunliactiontables.asm"
	ds		$6000-$,$ff
dephase

;
; block $73
;
chunlimusicblock:             equ $73
phase	$0000
  incbin "..\fmpac music\chunli.tmf"
	ds		$2000-$,$ff
dephase









;
; block $74 - $75
; block $76 - $77
; block $78 - $79
; block $7a - $7b
;
dhalsimframelistblock:        equ $74
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
; block $7c - $7d
; block $7e - $7f
; block $80 - $81
; block $82 - $83
;
dhalsimspritedatablock:       equ $7c
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
; block $84
;
dhalsimcountrysampleblock:    equ $84
phase	$0000
dhalsimcountrysample:
  incbin "..\sampleplayer\countries\india.raw"
dhalsimcountrysampleLenght:   equ $-dhalsimcountrysample
	ds		$2000-$,$ff
dephase

;
; block $85
;
dhalsimspecial1sampleblock:   equ $85
phase	$0000
dhalsimspecial1sample:
  incbin "..\sampleplayer\dhalsim\joga fire.raw"
dhalsimspecial1sampleLenght:   equ $-dhalsimspecial1sample
	ds		$2000-$,$ff
dephase

;
; block $86
;
dhalsimspecial2sampleblock:   equ $86
phase	$0000
dhalsimspecial2sample:
  incbin "..\sampleplayer\dhalsim\joga flame.raw"
dhalsimspecial2sampleLenght:   equ $-dhalsimspecial2sample
	ds		$2000-$,$ff
dephase

;
; block $87
;
dhalsimspecial3sampleblock:   equ $87
phase	$0000
dhalsimspecial3sample:
  incbin "..\sampleplayer\dhalsim\joga flame.raw"
dhalsimspecial3sampleLenght:   equ $-dhalsimspecial3sample
	ds		$2000-$,$ff
dephase

;
; block $88 - $8b
;
backgrounddhalsimblock:       equ $88
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
; block $8c
;
dhalsimcharacterfaceblock:    equ $8c
dhalsimactiontablesblock:     equ $8c
phase	$4000
  incbin "..\grapx\character select\character faces\dhalsimcharacterface.dat"
  include "actiontables\dhalsimactiontables.asm"
	ds		$6000-$,$ff
dephase

;
; block $8d
;
dhalsimmusicblock:            equ $8d
phase	$0000
  incbin "..\fmpac music\dhalsim.tmf"
	ds		$2000-$,$ff
dephase








;
; block $8e - $8f
; block $90 - $91
; block $92 - $93
; block $94 - $95
;
guileframelistblock:        equ $8e
phase	$8000
  include "..\grapx\guile\spritesguilePage0\frames.lst" 
	ds		$c000-$,$ff
dephase
phase	$8000
  include "..\grapx\guile\spritesguilePage1\frames.lst" 
	ds		$c000-$,$ff
dephase
phase	$8000
  include "..\grapx\guile\spritesguilePage2\frames.lst" 
	ds		$c000-$,$ff
dephase
phase	$8000
  include "..\grapx\guile\spritesguilePage3\frames.lst" 
	ds		$c000-$,$ff
dephase

;
; block $96 - $97
; block $98 - $99
; block $9a - $9b
; block $9c - $9d
;
guilespritedatablock:       equ $96
phase	$0000
  incbin "..\grapx\guile\spritesguilePage0\frames.dat"
	ds		$4000-$,$ff
dephase
phase	$0000
  incbin "..\grapx\guile\spritesguilePage1\frames.dat"
	ds		$4000-$,$ff
dephase
phase	$0000
  incbin "..\grapx\guile\spritesguilePage2\frames.dat"
	ds		$4000-$,$ff
dephase
phase	$0000
  incbin "..\grapx\guile\spritesguilePage3\frames.dat"
	ds		$4000-$,$ff
dephase

;
; block $9e
;
guilecountrysampleblock:    equ $9e
phase	$0000
guilecountrysample:
  incbin "..\sampleplayer\countries\usa.raw"
guilecountrysampleLenght:   equ $-guilecountrysample
	ds		$2000-$,$ff
dephase

;
; block $9f
;
guilespecial1sampleblock:   equ $9f
phase	$0000
guilespecial1sample:
  incbin "..\sampleplayer\guile\sonic boom.raw"
guilespecial1sampleLenght:   equ $-guilespecial1sample
	ds		$4000-$,$ff
dephase

;
; block $a1
;
guilespecial3sampleblock:   equ $a1
phase	$0000
guilespecial3sample:
  incbin "..\sampleplayer\countries\usa.raw"
guilespecial3sampleLenght:   equ $-guilespecial3sample
	ds		$2000-$,$ff
dephase

;
; block $a2 - $a5
;
backgroundguileblock:       equ $a2
phase	$0000
  incbin "..\grapx\guile\background\guile background.SC5",7,212*128 ;skip header
backgroundguilepalette:
  incbin "..\grapx\guile\background\guile background palette.plt",0,32
scoreboardfaceguile:
  incbin "..\grapx\guile\scoreboardface\scoreboardfaceguile.dat"
guilepalette:
  incbin "..\grapx\guile\guile palette\guile palette.plt",0,6   ;only 3 unique colors per player
guilealtpalette:
  incbin "..\grapx\guile\guile palette\guile altpalette.plt",0,6   ;only 3 unique colors per player
	ds		$8000-$,$ff
dephase

;
; block $a6
;
guilecharacterfaceblock:    equ $a6
guileactiontablesblock:     equ $a6
phase	$4000
  incbin "..\grapx\character select\character faces\guilecharacterface.dat"
  include "actiontables\guileactiontables.asm"
	ds		$6000-$,$ff
dephase

;
; block $a7
;
guilemusicblock:            equ $a7
phase	$0000
  incbin "..\fmpac music\guile.tmf"
	ds		$2000-$,$ff
dephase













;
; block $a8 - $a9
; block $aa - $ab
; block $ac - $ad
; block $ae - $af
;
ehondaframelistblock:        equ $a8
phase	$8000
  include "..\grapx\ehonda\spritesehondaPage0\frames.lst" 
	ds		$c000-$,$ff
dephase
phase	$8000
  include "..\grapx\ehonda\spritesehondaPage1\frames.lst" 
	ds		$c000-$,$ff
dephase
phase	$8000
  include "..\grapx\ehonda\spritesehondaPage2\frames.lst" 
	ds		$c000-$,$ff
dephase
phase	$8000
  include "..\grapx\ehonda\spritesehondaPage3\frames.lst" 
	ds		$c000-$,$ff
dephase

;
; block $b0 - $b1
; block $b2 - $b3
; block $b4 - $b5
; block $b6 - $b7
;
ehondaspritedatablock:       equ $b0
phase	$0000
  incbin "..\grapx\ehonda\spritesehondaPage0\frames.dat"
	ds		$4000-$,$ff
dephase
phase	$0000
  incbin "..\grapx\ehonda\spritesehondaPage1\frames.dat"
	ds		$4000-$,$ff
dephase
phase	$0000
  incbin "..\grapx\ehonda\spritesehondaPage2\frames.dat"
	ds		$4000-$,$ff
dephase
phase	$0000
  incbin "..\grapx\ehonda\spritesehondaPage3\frames.dat"
	ds		$4000-$,$ff
dephase

;
; block $b8
;
;ehondacountrysampleblock:    equ $b8
;phase	$0000
;ehondacountrysample:
;  incbin "..\sampleplayer\japan.raw"
;ehondacountrysampleLenght:   equ $-ehondacountrysample
;	ds		$2000-$,$ff
;dephase

ds $2000

;
; block $b9
;
ehondaspecial1sampleblock:   equ $b9
phase	$0000
ehondaspecial1sample:
  incbin "..\sampleplayer\ehonda\sumo smash.raw"
ehondaspecial1sampleLenght:   equ $-ehondaspecial1sample
	ds		$2000-$,$ff
dephase

;
; block $ba
;
ehondaspecial2sampleblock:   equ $ba
phase	$0000
ehondaspecial2sample:
  incbin "..\sampleplayer\ehonda\hundred hand slap.raw"
ehondaspecial2sampleLenght:   equ $-ehondaspecial2sample
	ds		$2000-$,$ff
dephase

;
; block $bb
;
ehondaspecial3sampleblock:   equ $bb
phase	$0000
ehondaspecial3sample:
  incbin "..\sampleplayer\ehonda\sumo headbutt.raw"
ehondaspecial3sampleLenght:   equ $-ehondaspecial3sample
	ds		$2000-$,$ff
dephase

;
; block $bc - $bf
;
backgroundehondablock:       equ $bc
phase	$0000
  incbin "..\grapx\ehonda\background\ehonda background.SC5",7,212*128 ;skip header
backgroundehondapalette:
  incbin "..\grapx\ehonda\background\ehonda background palette.plt",0,32
scoreboardfaceehonda:
  incbin "..\grapx\ehonda\scoreboardface\scoreboardfaceehonda.dat"
ehondapalette:
  incbin "..\grapx\ehonda\ehonda palette\ehonda palette.plt",0,6   ;only 3 unique colors per player
ehondaaltpalette:
  incbin "..\grapx\ehonda\ehonda palette\ehonda altpalette.plt",0,6   ;only 3 unique colors per player
	ds		$8000-$,$ff
dephase

;
; block $c0
;
ehondacharacterfaceblock:    equ $c0
ehondaactiontablesblock:     equ $c0
phase	$4000
  incbin "..\grapx\character select\character faces\ehondacharacterface.dat"
  include "actiontables\ehondaactiontables.asm"
	ds		$6000-$,$ff
dephase

;
; block $c1
;
ehondamusicblock:            equ $c1
phase	$0000
  incbin "..\fmpac music\ehonda.tmf"
	ds		$2000-$,$ff
dephase









;
; block $c2 - $c3
; block $c4 - $c5
; block $c6 - $c7
; block $c8 - $c9
;
cammyframelistblock:        equ $c2
phase	$8000
  include "..\grapx\cammy\spritescammyPage0\frames.lst" 
	ds		$c000-$,$ff
dephase
phase	$8000
  include "..\grapx\cammy\spritescammyPage1\frames.lst" 
	ds		$c000-$,$ff
dephase
phase	$8000
  include "..\grapx\cammy\spritescammyPage2\frames.lst" 
	ds		$c000-$,$ff
dephase
phase	$8000
  include "..\grapx\cammy\spritescammyPage3\frames.lst" 
	ds		$c000-$,$ff
dephase

;
; block $ca - $cb
; block $cc - $cd
; block $ce - $cf
; block $d0 - $d1
;
cammyspritedatablock:       equ $ca
phase	$0000
  incbin "..\grapx\cammy\spritescammyPage0\frames.dat"
	ds		$4000-$,$ff
dephase
phase	$0000
  incbin "..\grapx\cammy\spritescammyPage1\frames.dat"
	ds		$4000-$,$ff
dephase
phase	$0000
  incbin "..\grapx\cammy\spritescammyPage2\frames.dat"
	ds		$4000-$,$ff
dephase
phase	$0000
  incbin "..\grapx\cammy\spritescammyPage3\frames.dat"
	ds		$4000-$,$ff
dephase

;
; block $d2
;
cammycountrysampleblock:    equ $d2
phase	$0000
cammycountrysample:
  incbin "..\sampleplayer\countries\england.raw"
cammycountrysampleLenght:   equ $-cammycountrysample
	ds		$2000-$,$ff
dephase


;
; block $d3
;
cammyspecial1sampleblock:   equ $d3
phase	$0000
cammyspecial1sample:
  incbin "..\sampleplayer\cammy\spiral arrow.raw"
cammyspecial1sampleLenght:   equ $-cammyspecial1sample
	ds		$2000-$,$ff
dephase

;
; block $d4
;
cammyspecial2sampleblock:   equ $d4
phase	$0000
cammyspecial2sample:
  incbin "..\sampleplayer\cammy\axel spin knuckle.raw"
cammyspecial2sampleLenght:   equ $-cammyspecial2sample
	ds		$2000-$,$ff
dephase

;
; block $d5
;
cammyspecial3sampleblock:   equ $d5
phase	$0000
cammyspecial3sample:
  incbin "..\sampleplayer\cammy\cannon spike.raw"
cammyspecial3sampleLenght:   equ $-cammyspecial3sample
	ds		$2000-$,$ff
dephase

;
; block $d6 - $d9
;
backgroundcammyblock:       equ $d6
phase	$0000
  incbin "..\grapx\cammy\background\cammy background.SC5",7,212*128 ;skip header
backgroundcammypalette:
  incbin "..\grapx\cammy\background\cammy background palette.plt",0,32
scoreboardfacecammy:
  incbin "..\grapx\cammy\scoreboardface\scoreboardfacecammy.dat"
cammypalette:
  incbin "..\grapx\cammy\cammy palette\cammy palette.plt",0,6   ;only 3 unique colors per player
cammyaltpalette:
  incbin "..\grapx\cammy\cammy palette\cammy altpalette.plt",0,6   ;only 3 unique colors per player
	ds		$8000-$,$ff
dephase

;
; block $da
;
cammycharacterfaceblock:    equ $da
cammyactiontablesblock:     equ $da
phase	$4000
  incbin "..\grapx\character select\character faces\cammycharacterface.dat"
  include "actiontables\cammyactiontables.asm"
	ds		$6000-$,$ff
dephase

;
; block $db
;
cammymusicblock:            equ $db
phase	$0000
  incbin "..\fmpac music\cammy.tmf"
	ds		$2000-$,$ff
dephase







;
; block $dc - $dd
; block $de - $df
; block $e0 - $e1
; block $e2 - $e3
;
vegaframelistblock:        equ $dc
phase	$8000
  include "..\grapx\vega\spritesvegaPage0\frames.lst" 
	ds		$c000-$,$ff
dephase
phase	$8000
  include "..\grapx\vega\spritesvegaPage1\frames.lst" 
	ds		$c000-$,$ff
dephase
phase	$8000
  include "..\grapx\vega\spritesvegaPage2\frames.lst" 
	ds		$c000-$,$ff
dephase
phase	$8000
  include "..\grapx\vega\spritesvegaPage3\frames.lst" 
	ds		$c000-$,$ff
dephase

;
; block $e4 - $e5
; block $e6 - $e7
; block $e8 - $e9
; block $ea - $eb
;
vegaspritedatablock:       equ $e4
phase	$0000
  incbin "..\grapx\vega\spritesvegaPage0\frames.dat"
	ds		$4000-$,$ff
dephase
phase	$0000
  incbin "..\grapx\vega\spritesvegaPage1\frames.dat"
	ds		$4000-$,$ff
dephase
phase	$0000
  incbin "..\grapx\vega\spritesvegaPage2\frames.dat"
	ds		$4000-$,$ff
dephase
phase	$0000
  incbin "..\grapx\vega\spritesvegaPage3\frames.dat"
	ds		$4000-$,$ff
dephase

;
; block $ec
;
vegacountrysampleblock:    equ $ec
phase	$0000
vegacountrysample:
  incbin "..\sampleplayer\countries\spain.raw"
vegacountrysampleLenght:   equ $-vegacountrysample
	ds		$2000-$,$ff
dephase


;
; block $ed
;
vegaspecial1sampleblock:   equ $ed
phase	$0000
vegaspecial1sample:
  incbin "..\sampleplayer\vega\spiral arrow.raw"
vegaspecial1sampleLenght:   equ $-vegaspecial1sample
	ds		$2000-$,$ff
dephase

;
; block $ee
;
vegaspecial2sampleblock:   equ $ee
phase	$0000
vegaspecial2sample:
  incbin "..\sampleplayer\vega\axel spin knuckle.raw"
vegaspecial2sampleLenght:   equ $-vegaspecial2sample
	ds		$2000-$,$ff
dephase

;
; block $ef
;
vegaspecial3sampleblock:   equ $ef
phase	$0000
vegaspecial3sample:
  incbin "..\sampleplayer\vega\cannon spike.raw"
vegaspecial3sampleLenght:   equ $-vegaspecial3sample
	ds		$2000-$,$ff
dephase

;
; block $f0 - $f3
;
backgroundvegablock:       equ $f0
phase	$0000
  incbin "..\grapx\vega\background\vega background.SC5",7,212*128 ;skip header
backgroundvegapalette:
  incbin "..\grapx\vega\background\vega background palette.plt",0,32
scoreboardfacevega:
  incbin "..\grapx\vega\scoreboardface\scoreboardfacevega.dat"
vegapalette:
  incbin "..\grapx\vega\vega palette\vega palette.plt",0,6   ;only 3 unique colors per player
vegaaltpalette:
  incbin "..\grapx\vega\vega palette\vega altpalette.plt",0,6   ;only 3 unique colors per player
	ds		$8000-$,$ff
dephase

;
; block $f4
;
vegacharacterfaceblock:    equ $f4
vegaactiontablesblock:     equ $f4
phase	$4000
  incbin "..\grapx\character select\character faces\vegacharacterface.dat"
  include "actiontables\vegaactiontables.asm"
	ds		$6000-$,$ff
dephase

;
; block $f5
;
vegamusicblock:            equ $f5
phase	$0000
  incbin "..\fmpac music\vega.tmf"
	ds		$2000-$,$ff
dephase









;a combi-write to $5000 and $7000 (old situation) should be changed to a single write to $6000 
;<BiFiMSX> a combi-write to $9000 and $b000 (old situation) should be changed to a single write to $7000 

























  include "actiontables\actiontableslenghts.asm"





totallenght:	Equ	$-Streetfighter
;	ds		$80000-totallenght