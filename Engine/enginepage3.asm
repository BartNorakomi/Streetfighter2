phase	enginepage3addr
;*************************************************************
; met $7f, $bf en $ff in $9000 wordt de SCC voorgeschakeld
; ascii8 blokken:
; $5000 -> $6000 -> 
; $7000 -> $6800 -> 
; $9000 -> $7000 -> 
; $b000 -> $7800 -> 
;*************************************************************
block1:		
	di
	ld		(memblocks.1),a
	ld		($5000),a
	ei
	ret

block2:		
	di
	ld		(memblocks.2),a
	ld		($7000),a
	ei
	ret

block3:		
	di
	ld		(memblocks.3),a
	ld		($9000),a
	ei
	ret

block4:		
	di
	ld		(memblocks.4),a
	ld		($b000),a
	ei
	ret

block12:	
	di
	ld		(memblocks.1),a
	ld		($5000),a
	inc		a
	ld		(memblocks.2),a
	ld		($7000),a
	ei
	ret

block23:	
	di
	ld		(memblocks.2),a
	ld		($7000),a
	inc		a
	ld		(memblocks.3),a
	ld		($9000),a
	ei
	ret

block34:	
	di
	ld		(memblocks.3),a
	ld		($9000),a
	inc		a
	ld		(memblocks.4),a
	ld		($b000),a
	ei
	ret

block123:	
	di
	ld		(memblocks.1),a
	ld		($5000),a
	inc		a
	ld		(memblocks.2),a
	ld		($7000),a
	inc		a
	ld		(memblocks.3),a
	ld		($9000),a
	ei
	ret

block234:	
	di
	ld		(memblocks.2),a
	ld		($7000),a
	inc		a
	ld		(memblocks.3),a
	ld		($9000),a
	inc		a
	ld		(memblocks.4),a
	ld		($b000),a
	ei
	ret

block1234:	di
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
	ei
	ret

outix128:	
	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	
	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	
outix96:	
	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	
outix80:	
	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi
outix64:	
	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	
	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	
outix32:	
	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	outi	
outix16:	
	outi	outi	outi	outi	outi	outi	outi	outi
outix8:	
	outi	outi	outi	outi	outi	outi	outi	outi	
	ret

;Port range	Description
;#AA	bits 0-3: Row select
;#A9 (read)	Row read (inverted)

;	in	a,(#AA)
;	and	#F0		;only change bits 0-3
;	or	b		;take row number from B
;	out	(#AA),a
;	in	a,(#A9)		;read row into A

;      bit 7   bit 6   bit 5   bit 4   bit 3   bit 2   bit 1   bit 0
;row 0	7 &     6 ^     5 %     4 $     3 #     2 @     1 !     0 )
;row 1	; :	    ] }	    [ {	    \ ¦	    = +	    - _	    9 (	    8 *
;row 2	B	      A	      DEAD	  / ?	    . >	    , <	    ` ~	    ' "
;row 3	J	      I	      H	      G	      F	      E	      D	      C
;row 4	R	      Q	      P	      O	      N	      M	      L	      K
;row 5	Z	      Y	      X	      W	      V	      U	      T	      S
;row 6	F3	    F2	    F1	    CODE	  CAPS	  GRAPH	  CTRL	  SHIFT
;row 7	RET	    SELECT	BS	    STOP	  TAB	    ESC	    F5	    F4
;row 8	right   down    up      left    DEL	    INS	    HOME	  SPACE
;row 9	NUM4	  NUM3	  NUM2	  NUM1	  NUM0	  NUM/	  NUM+	  NUM*
;row 10 NUM.	  NUM,	  NUM-	  NUM9	  NUM8	  NUM7	  NUM6	  NUM5


HandleP2Ai:
  ld    a,r
  ld    c,a

	ld		hl,ControlsP2
	ld		a,(hl)
	xor		c
	and		c
	ld		(NewPrControlsP2),a
	ld		(hl),c
  ret


;
; bit 7     6     5     4     3     2     1     0
;		  softk softp hardk hardp right left  down  up  (action)
;		  /     .     '     ;     right left  down  up  (ControlsP1)
;		  S     A     W     Q     G     D     V     R   (ControlsP2)
;		  1     2                 7     8     9     0   (TestControls)
;
PopulateControls:	
  call  SetTestControls
  call  Player1Controls
;  call  Player2Controls
;  ret

;Player2Controls:

  ;check if 1v1 or arcade is played
  ld    a,(mainmenuoptionselected?)
  cp    1                     ;arcade is pressed
  jr    z,HandleP2Ai

  in    a,($aa)
  and   $f0                   ;only change bits 0-3
  ld    b,a                   ;row select copy
  ld    c,0                   ;Controls pressed

  ;check up
  or    4                     ;row 4, check R (up)
  out   ($aa),a
  in    a,($a9)               ;read row (inverted)
  bit   7,a
  jp    nz,.UpNotPressed
  set   0,c                   ;up is now pressed
  .UpNotPressed:
  ;/check up

  ;check down
  ld    a,b
  or    5                     ;row 5, check V (down)
  out   ($aa),a
  in    a,($a9)               ;read row (inverted)
  bit   3,a
  jp    nz,.DownNotPressed
  set   1,c                   ;down is now pressed
  .DownNotPressed:  
  ;/check down

  ;check left
  ld    a,b
  or    3                     ;row 3, check D (left)
  out   ($aa),a
  in    a,($a9)               ;read row (inverted)
  bit   1,a
  jp    nz,.LeftNotPressed
  set   2,c                   ;left is now pressed
  .LeftNotPressed:
  ;/check left

  ;check right
  ld    a,b
  or    3                     ;row 3, check G (right)
  out   ($aa),a
  in    a,($a9)               ;read row (inverted)
  bit   4,a
  jp    nz,.RightNotPressed
  set   3,c                   ;right is now pressed
  .RightNotPressed:
  ;/check right

  ;check hard punch
  ld    a,b
  or    4                     ;row 4, check Q (right)
  out   ($aa),a
  in    a,($a9)               ;read row (inverted)
  bit   6,a
  jp    nz,.HardPunchNotPressed
  set   4,c                   ;right is now pressed
  .HardPunchNotPressed:
  ;/check hard punch

  ;check hard kick
  ld    a,b
  or    5                     ;row 5, check W (right)
  out   ($aa),a
  in    a,($a9)               ;read row (inverted)
  bit   4,a
  jp    nz,.HardKickNotPressed
  set   5,c                   ;right is now pressed
  .HardKickNotPressed:
  ;/check hard kick

  ;check soft punch
  ld    a,b
  or    2                     ;row 2, check A (right)
  out   ($aa),a
  in    a,($a9)               ;read row (inverted)
  bit   6,a
  jp    nz,.SoftPunchNotPressed
  set   6,c                   ;right is now pressed
  .SoftPunchNotPressed:
  ;/check soft punch

  ;check soft kick
  ld    a,b
  or    5                     ;row 5, check S (right)
  out   ($aa),a
  in    a,($a9)               ;read row (inverted)
  bit   0,a
  jp    nz,.SoftKickNotPressed
  set   7,c                   ;right is now pressed
  .SoftKickNotPressed:
  ;/check soft kick

	ld		hl,ControlsP2
	ld		a,(hl)
	xor		c
	and		c
	ld		(NewPrControlsP2),a
	ld		(hl),c
  ret

Player1Controls:
  in    a,($aa)
  and   $f0                   ;only change bits 0-3
  ld    b,a                   ;row select copy
  ld    c,0                   ;Controls pressed

  ;check up
  or    8                     ;row 8, check up (up)
  out   ($aa),a
  in    a,($a9)               ;read row (inverted)
  bit   5,a
  jp    nz,.UpNotPressed
  set   0,c                   ;up is now pressed
  .UpNotPressed:
  ;/check up

  ;check down
  ld    a,b
  or    8                     ;row 8, check down (down)
  out   ($aa),a
  in    a,($a9)               ;read row (inverted)
  bit   6,a
  jp    nz,.DownNotPressed
  set   1,c                   ;down is now pressed
  .DownNotPressed:  
  ;/check down

  ;check left
  ld    a,b
  or    8                     ;row 8, check left (left)
  out   ($aa),a
  in    a,($a9)               ;read row (inverted)
  bit   4,a
  jp    nz,.LeftNotPressed
  set   2,c                   ;left is now pressed
  .LeftNotPressed:
  ;/check left

  ;check right
  ld    a,b
  or    8                     ;row 8, check right (right)
  out   ($aa),a
  in    a,($a9)               ;read row (inverted)
  bit   7,a
  jp    nz,.RightNotPressed
  set   3,c                   ;right is now pressed
  .RightNotPressed:
  ;/check right

  ;check hard punch
  ld    a,b
  or    1                     ;row 1, check ;
  out   ($aa),a
  in    a,($a9)               ;read row (inverted)
  bit   7,a
  jp    nz,.HardPunchNotPressed
  set   4,c                   ;right is now pressed
  .HardPunchNotPressed:
  ;/check hard punch

  ;check hard kick
  ld    a,b
  or    2                     ;row 2, check '
  out   ($aa),a
  in    a,($a9)               ;read row (inverted)
  bit   0,a
  jp    nz,.HardKickNotPressed
  set   5,c                   ;right is now pressed
  .HardKickNotPressed:
  ;/check hard kick

  ;check soft punch
  ld    a,b
  or    2                     ;row 2, check .
  out   ($aa),a
  in    a,($a9)               ;read row (inverted)
  bit   3,a
  jp    nz,.SoftPunchNotPressed
  set   6,c                   ;right is now pressed
  .SoftPunchNotPressed:
  ;/check soft punch

  ;check soft kick
  ld    a,b
  or    2                     ;row 2, check /
  out   ($aa),a
  in    a,($a9)               ;read row (inverted)
  bit   4,a
  jp    nz,.SoftKickNotPressed
  set   7,c                   ;right is now pressed
  .SoftKickNotPressed:
  ;/check soft kick

	ld		hl,ControlsP1
	ld		a,(hl)
	xor		c
	and		c
	ld		(NewPrControlsP1),a
	ld		(hl),c
  ret

SetTestControls:
  in    a,($aa)
  and   $f0                   ;only change bits 0-3
  ld    b,a                   ;row select copy
  ld    c,0                   ;Controls pressed

  ;check 1
  or    0                     ;row 0, check "1"
  out   ($aa),a
  in    a,($a9)               ;read row (inverted)
  bit   1,a
  jp    nz,.1NotPressed
  set   7,c                   ;1 is now pressed
  .1NotPressed:
  ;/check 1

  ;check 2
  ld    a,b
  or    0                     ;row 0, check "2"
  out   ($aa),a
  in    a,($a9)               ;read row (inverted)
  bit   2,a
  jp    nz,.2NotPressed
  set   6,c                   ;2 is now pressed
  .2NotPressed:
  ;/check 2

  ;check 0
  ld    a,b
  or    0                     ;row 0, check "0"
  out   ($aa),a
  in    a,($a9)               ;read row (inverted)
  bit   0,a
  jp    nz,.0NotPressed
  set   0,c                   ;2 is now pressed
  .0NotPressed:
  ;/check 0

  ;check 9
  ld    a,b
  or    1                     ;row 1, check "9"
  out   ($aa),a
  in    a,($a9)               ;read row (inverted)
  bit   1,a
  jp    nz,.9NotPressed
  set   1,c                   ;2 is now pressed
  .9NotPressed:
  ;/check 9

  ;check 7
  ld    a,b
  or    0                     ;row 0, check "7"
  out   ($aa),a
  in    a,($a9)               ;read row (inverted)
  bit   7,a
  jp    nz,.7NotPressed
  set   3,c                   ;"7" is now pressed
  .7NotPressed:
  ;/check 7

  ;check 8
  ld    a,b
  or    1                     ;row 1, check "8"
  out   ($aa),a
  in    a,($a9)               ;read row (inverted)
  bit   0,a
  jp    nz,.8NotPressed
  set   2,c                   ;"8" is now pressed
  .8NotPressed:
  ;/check 8

	ld		a,c
	ld		(TestControls),a
  ret
;
;Fast DoCopy, by Grauw
;In:  HL = pointer to 15-byte VDP command data
;Out: HL = updated
;
DoCopygrauw:	
  ld    a,32
	di
	out	  ($99),a
	ld	  a,17+128
	out	  ($99),a
	ld	  c,$9B
.VDPready:
	ld	  a,2
	di
	out	  ($99),a		            ;select s#2
	ld	  a,15+128
	out	  ($99),a
	in	  a,($99)
	rra
	ld 	  a,0		                ;back to s#0, enable ints
	out	  ($99),a
	ld	  a,15+128
	ei
	out   ($99),a		            ;loop if vdp not ready (CE)
	jp    c,.VDPready
  outi | outi | outi | outi | outi
  outi | outi | outi | outi | outi
  outi | outi | outi | outi | outi
	ret

;wait vdp ready
VDPready:
  ld    a,2
  di
  out   ($99),a               ;select s#2
  ld    a,15+128
  out   ($99),a
  in    a,($99)
  rra
  ld    a,0                   ;select s#0
  out   ($99),a
  ld    a,15+128
  ei
  out   ($99),a
  jp    c,VDPready
  ret

;
;Set page A (in screen 5).
;
SetPage:      
;  add     a,a                 ;x32
;  add     a,a
;  add     a,a
;  add     a,a
;  add     a,a
;  add     a,31
  ld    (VDP_0+2),a
  di
  out   ($99),a
  ld    a,2+128
  ei
  out   ($99),a
  ret

SetPalette:
	xor		a
	di
	out		($99),a
	ld		a,16+128
	out		($99),a
	ld		bc,$209A
	otir
	ei
	ret

restorebackgroundplayer1page0:
	db    0,0,0,3
	db    0,0,0,0
	db    $2a,0,$50,0
	db    0,0,$d0  
restorebackgroundplayer1page1:
	db    0,0,0,3
	db    0,0,0,1
	db    $2a,0,$50,0
	db    0,0,$d0  
restorebackgroundplayer1page2:
	db    0,0,0,3
	db    0,0,0,2
	db    $2a,0,$50,0
	db    0,0,$d0  
restorebackgroundplayer2page0:
	db    0,0,0,3
	db    0,0,0,0
	db    $2a,0,$50,0
	db    0,0,$d0  
restorebackgroundplayer2page1:
	db    0,0,0,3
	db    0,0,0,1
	db    $2a,0,$50,0
	db    0,0,$d0  
restorebackgroundplayer2page2:
	db    0,0,0,3
	db    0,0,0,2
	db    $2a,0,$50,0
	db    0,0,$d0  

restorebackgroundprojectileP1page0:
	db    0,0,0,3
	db    0,0,0,0
	db    2,0,2,0
	db    0,0,$d0  
restorebackgroundprojectileP1page1:
	db    0,0,0,3
	db    0,0,0,1
	db    2,0,2,0
	db    0,0,$d0  
restorebackgroundprojectileP1page2:
	db    0,0,0,3
	db    0,0,0,2
	db    2,0,2,0
	db    0,0,$d0  
restorebackgroundprojectileP2page0:
	db    0,0,0,3
	db    0,0,0,0
	db    2,0,2,0
	db    0,0,$d0  
restorebackgroundprojectileP2page1:
	db    0,0,0,3
	db    0,0,0,1
	db    2,0,2,0
	db    0,0,$d0  
restorebackgroundprojectileP2page2:
	db    0,0,0,3
	db    0,0,0,2
	db    2,0,2,0
	db    0,0,$d0  

HitAnimationTable:            ;x movement per frame, 128=end
  db    +2,-2,+2,-2,+2,-2,+4,+4,+4,+4,128
HeavilyHitAnimationTable:     ;movement(y,x)
  db    +00,-00, +00,-00, +00,-00, +00,-00, +00,-00, +00,-00, -12,-02, -10,-02, -08,-02, -06,-02, -04,-02, -02,-02, -01,-02, -00,-02
  db    +01,-02, +02,-02, +04,-02, +06,-02, +08,-02, +10,-02, +12,-02, +13,-02, +14,-02, +14,-02, +14,-02, +15,-02, +15,-02, +15,-02
  db    +15,-02, +15,-02, +15,-02, +15,-02, +15,-02
GettingTossedAnimationTable:  ;movement(y,x)
  db    +00,-00, +00,-00, +00,-00, +00,-00, +00,-00, +00,-00, +00,-00, +00,-00, -10,-50, -08,-06, -06,-06, -04,-06, -03,-06, -02,-06
  db    -01,-06, -00,-06, -00,-06, +01,-06, +02,-06, +03,-06, +06,-06, +08,-06, +10,-06, +12,-06, +13,-06, +14,-06, +14,-06, +14,-06
  db    +15,-06, +15,-06, +15,-06, +15,-06, +15,-06, +15,-06, +15,-06, +15,-06

moveplayerleftinscreen:       equ 128
page:                         ds  1
blitpage:                     ds  1
screenpage:                   ds  1
Player1Spritedatablock:       ds  1

Player1Framelistblock:        ds  1
Player1Frame:                 dw  ryupage0frame000
Player1FramePage:             db  3
Player2Framelistblock:        ds  1
Player2Frame:                 dw  ryupage0frame000
Player2FramePage:             db  0

Player1FrameBackup:           ds  2
Player1FramePageBackup:       ds  1
Player2FrameBackup:           ds  2
Player2FramePageBackup:       ds  1

putplayer1:
  ld    a,(screenpage)
  or    a                     ;if current page =0 then que page 1 to be restored
  ld    ix,restorebackgroundplayer1page1
  jp    z,.startsetupque
  dec   a                     ;if current page =1 then que page 2 to be restored
  ld    ix,restorebackgroundplayer1page2
  jp    z,.startsetupque      ;if current page =2 then que page 0 to be restored
  ld    ix,restorebackgroundplayer1page0
  .startsetupque:

	ld		a,(slot.page12rom)    ;all RAM except page 1+2
	out		($a8),a	

  ;set framelist in page 2 in rom ($8000 - $bfff)
  ld    a,(Player1FramePage)
  add   a,a
  ld    hl,Player1Framelistblock
	add   a,(hl)
  call	block34

  ;set framedata in page 1 in rom ($4000 - $7fff)
  ld    a,(Player1FramePage)
  add   a,a
  ld    hl,Player1Spritedatablock
	add   a,(hl)
  call	block12

  ld    bc,player1x
  ld    hl,(Player1Frame)     ;points to player width
  ld    iy,Player1SxB1        ;player collision detection blocks

  di
  call  Putplayer
  ei
  ret

P1ProjectileHit?: db 0
putprojectileP1:
  cp    2
  jp    nz,.end

  ;check if projectile has hit player 2
  ld    a,(P1ProjectileHit?)
  or    a
  jr    z,.NotHit
  dec   a
  ld    (P1ProjectileHit?),a
  jr    nz,.NotEnd

  xor   a
  ld    (P1ProjectileInScreen?),a
  ret

  .NotEnd:
  cp    1
  ret   z
  
  ld    a,(P1ProjectileDirection)
  or    a
  ld    ix,P1ProjectileLeftEndFrame
  jp    m,.directionFound
  ld    ix,P1ProjectileRightEndFrame
  .directionFound:
  call  putprojectileP1.GoAnimate
  ret  

  .NotHit:
  ;/check if projectile has hit player 2

  ld    a,(P1ProjectileDirection)
  or    a
  jp    m,.negative

  ld    ix,P1ProjectileRightFrame
  ld    a,(ProjectileP1x)
  add   a,6
  cp    236
  jp    c,.Set
  .end:
  ld    a,(P1ProjectileInScreen?)
  dec   a
  ld    (P1ProjectileInScreen?),a
  ret

  .negative:
  ld    ix,P1ProjectileLeftFrame
  ld    a,(ProjectileP1x)
  sub   a,6
  jp    nc,.Set
  ld    a,(P1ProjectileInScreen?)
  dec   a
  ld    (P1ProjectileInScreen?),a
  ret

  .Set:
  ld    (ProjectileP1x),a

  .GoAnimate:
  ld    iy,P1projectileFrame
  call  AnimateProjectile
  
  ld    a,(screenpage)
  or    a                     ;if current page =0 then que page 1 to be restored
  ld    ix,restorebackgroundprojectileP1page1
  jp    z,.startsetupque
  dec   a                     ;if current page =1 then que page 2 to be restored
  ld    ix,restorebackgroundprojectileP1page2
  jp    z,.startsetupque      ;if current page =2 then que page 0 to be restored
  ld    ix,restorebackgroundprojectileP1page0
  .startsetupque:

	ld		a,(slot.page12rom)    ;all RAM except page 1+2
	out		($a8),a	

  ;set framelist in page 2 in rom ($8000 - $bfff)
  ld    a,(P1projectileFramePage)
  add   a,a
  ld    hl,Player1Framelistblock
	add   a,(hl)
  call	block34

  ;set framedata in page 1 in rom ($4000 - $7fff)
  ld    a,(P1projectileFramePage)
  add   a,a
  ld    hl,Player1Spritedatablock
	add   a,(hl)
  call	block12

  ld    bc,ProjectileP1x
  ld    hl,(P1projectileFrame);points to projectiles width
  ld    iy,Player1SxB1Projectile

  di
  ld    a,(bc)                ;projectile x
  call  Putprojectile
  ei
  ret

P2ProjectileHit?: db  0
putprojectileP2:
  cp    2
  jp    nz,.end

  ;check if projectile has hit player 2
  ld    a,(P2ProjectileHit?)
  or    a
  jr    z,.NotHit
  dec   a
  ld    (P2ProjectileHit?),a
  jr    nz,.NotEnd

  xor   a
  ld    (P2ProjectileInScreen?),a
  ret

  .NotEnd:
  cp    1
  ret   z
  
  ld    a,(P2ProjectileDirection)
  or    a
  ld    ix,P2ProjectileLeftEndFrame
  jp    m,.directionFound
  ld    ix,P2ProjectileRightEndFrame
  .directionFound:
  call  putprojectileP2.GoAnimate
  ret  

  .NotHit:
  ;/check if projectile has hit player 2

  ld    a,(P2ProjectileDirection)
  or    a
  jp    m,.negative

  ld    ix,P2ProjectileRightFrame
  ld    a,(ProjectileP2x)
  add   a,6
  cp    236
  jp    c,.Set
  .end:
  ld    a,(P2ProjectileInScreen?)
  dec   a
  ld    (P2ProjectileInScreen?),a
  ret

  .negative:
  ld    ix,P2ProjectileLeftFrame
  ld    a,(ProjectileP2x)
  sub   a,6
  jp    nc,.Set
  ld    a,(P2ProjectileInScreen?)
  dec   a
  ld    (P2ProjectileInScreen?),a
  ret

  .Set:
  ld    (ProjectileP2x),a

  .GoAnimate:
  ld    iy,P2projectileFrame
  call  AnimateProjectile

  ld    a,(screenpage)
  or    a                     ;if current page =0 then que page 1 to be restored
  ld    ix,restorebackgroundprojectileP2page1
  jp    z,.startsetupque
  dec   a                     ;if current page =1 then que page 2 to be restored
  ld    ix,restorebackgroundprojectileP2page2
  jp    z,.startsetupque      ;if current page =2 then que page 0 to be restored
  ld    ix,restorebackgroundprojectileP2page0
  .startsetupque:

	ld		a,(slot.page2rom)     ;all RAM except page 2
	out		($a8),a	
	
  ;set framelist in page 2 in rom ($8000 - $bfff)
  ld    a,(P2projectileFramePage)
  add   a,a
  ld    hl,Player2Framelistblock
	add   a,(hl)
  call	block34

  ;set framedata in page 1 in ram ($4000 - $7ffff) bank 4,5,6 or 7
  ld    a,(P2projectileFramePage)
  add   a,4                   ;set bank 4,5,6 or 7 (spritedata player2) in page 1 ($4000 - $7fff)
  out   ($fd),a               ;page x,4567,x,x   

  ld    bc,ProjectileP2x
  ld    hl,(P2projectileFrame);points to projectile width
  ld    iy,Player2SxB1Projectile

  di
  ld    a,(bc)                ;projectile x
  call  Putprojectile
  ei
  ret


P1ProjectileInScreen?:        db  0
P1projectileFrame:            dw  ryupage2frame015
P1projectileFramePage:        db  2
ProjectileP1y:                db  PlayeryAtstart
ProjectileP1x:                db  100
P1ProjectileDirection:        ds  1

P2ProjectileInScreen?:        db  0
P2projectileFrame:            dw  ryupage2frame015
P2projectileFramePage:        db  2
ProjectileP2y:                db  PlayeryAtstart
ProjectileP2x:                db  100
P2ProjectileDirection:        ds  1

  
putplayer2:
  ld    a,(screenpage)
  or    a                     ;if current page =0 then que page 1 to be restored
  ld    ix,restorebackgroundplayer2page1
  jp    z,.startsetupque
  dec   a                     ;if current page =1 then que page 2 to be restored
  ld    ix,restorebackgroundplayer2page2
  jp    z,.startsetupque      ;if current page =2 then que page 0 to be restored
  ld    ix,restorebackgroundplayer2page0
.startsetupque:

	ld		a,(slot.page2rom)     ;all RAM except page 2
	out		($a8),a	
	
  ;set framelist in page 2 in rom ($8000 - $bfff)
  ld    a,(Player2FramePage)
  add   a,a
  ld    hl,Player2Framelistblock
	add   a,(hl)
  call	block34

  ;set framedata in page 1 in ram ($4000 - $7ffff) bank 4,5,6 or 7
  ld    a,(Player2FramePage)
  add   a,4                   ;set bank 4,5,6 or 7 (spritedata player2) in page 1 ($4000 - $7fff)
  out   ($fd),a               ;page x,4567,x,x   

  ld    bc,player2x
  ld    hl,(Player2Frame)     ;points to player width
  ld    iy,Player2SxB1        ;player collision detection blocks

  di
  call  Putplayer
  ei
  ret

ScreenLimitxRight:  equ 256-10
ScreenLimitxLeft:   equ 10
Putplayer:
;screen limit right
  ld    a,(bc)                ;player x
  cp    ScreenLimitxRight
  jp    c,.LimitRight
  ld    a,ScreenLimitxRight
  ld    (bc),a
  .LimitRight:
;screen limit left
  ld    a,(bc)                ;player x
  cp    ScreenLimitxLeft
  jp    nc,.LimitLeft
  ld    a,ScreenLimitxLeft
  ld    (bc),a
  .LimitLeft:

  Putprojectile:              ;projectiles use the same routine as Putplayer
  or    a
  jp    p,PutSpriteleftSideOfScreen

PutSpriteRightSideOfScreen:
  ;Set up restore background que player
  ;set width
  ld    a,(hl)                ;player width
  ld    (ix+nx),a             ;set player width to be restored by background
    ;set height
  inc   hl                    ;player height
  ld    a,(hl)
  ld    (ix+ny),a             ;set player height to be restored by background
    ;set sx,dx by adding offset x to player x
  inc   hl                    ;offset x
  ld    e,(hl)                ;offset x
  ld    a,(bc)                ;player x
  sub   a,moveplayerleftinscreen
  add   a,e

  jp    c,putplayer_clipright_totallyoutofscreenright

  ld    (ix+sx),a             ;set sx/dx to restore by background
  ld    (ix+dx),a
    ;set sy,dy by adding offset y to player y
  inc   hl                    ;player y offset
  dec   bc                    ;player y
  ld    a,(bc)
  add   a,(hl)
  ld    d,a
  ld    (ix+sy),a             ;set sy/dy to restore by background
  ld    (ix+dy),a
  ld    (iy+1),a              ;Player1SyB1 (set block 1 sy)
  ;/Set up restore background que player
  inc   hl                    ;player x offset for first line
  inc   bc                    ;player x

  ;clipping check
  ld    a,(bc)                ;player x
  sub   moveplayerleftinscreen
  add   a,e                   ;player x + offset x
  add   a,(ix+nx)
  jp    c,putplayer_clipright
  jp    putplayer_noclip

  
PutSpriteleftSideOfScreen:
  ;Set up restore background que player
    ;set width
  ld    a,(hl)                ;player width
  ld    (ix+nx),a             ;set player width to be restored by background
    ;set height
  inc   hl                    ;player height
  ld    a,(hl)
  ld    (ix+ny),a             ;set player height to be restored by background
    ;set sx,dx by adding offset x to player x
  inc   hl                    ;offset x
  ld    e,(hl)                ;offset x
  ld    a,(bc)                ;player x
  sub   a,moveplayerleftinscreen
  add   a,e
  jr    c,.carry
  xor   a
  .carry:
  ld    (ix+sx),a             ;set sx/dx to restore by background
  ld    (ix+dx),a
    ;set sy,dy by adding offset y to player y
  inc   hl                    ;player y offset
  dec   bc                    ;player y
  ld    a,(bc)
  add   a,(hl)
  ld    d,a
  ld    (ix+sy),a             ;set sy/dy to restore by background
  ld    (ix+dy),a
  ld    (iy+1),a              ;Player1SyB1 (set block 1 sy)
  ;/Set up restore background que player
  inc   hl                    ;player x offset for first line
  inc   bc                    ;player x

  ;clipping check
  ld    a,(bc)                ;player x
  sub   a,moveplayerleftinscreen
  add   a,e                   ;player x + offset x
  jp    nc,putplayer_clipleft
  ;/clipping check

putplayer_noclip:
  ld    a,(bc)                ;player x
  add   a,(hl)                ;add offset x  for first line to destination x
  sub   a,moveplayerleftinscreen
  ld    e,a
  call  SetOffsetBlocksAndAttackpoints
  inc   hl                    ;lenght + increment first spriteline

  ;if screenpage=0 then blit in page 1
  ;if screenpage=1 then blit in page 2
  ;if screenpage=2 then blit in page 0
  ld    a,(screenpage)
  inc   a
  cp    3
  jr    nz,.not3
  xor   a
  .not3:  
  add   a,a

  bit   7,d

  jp    z,.setpage
  inc   a
  .setpage:
  ld    (blitpage),a
  out   ($99),a               ;write page instellen
  ld    a,14+128
  out   ($99),a

  srl   d                     ;write addres is de/2
  rr    e

  set   6,d                   ;write access

  ld    (spatpointer),sp  
  ld    sp,hl

  ld    a,e
  ld    c,$98
  .loop:
  out   ($99),a               ;set x to write to
  ld    a,d
  out   ($99),a               ;set y to write to

  pop   hl                    ;pop lenght + increment  
  ld    b,h                   ;length
  ld    a,l                   ;increment
  pop   hl                    ;pop source address

  otir
  or    a
  jr    z,.exit

  add   a,e                   ;add increment to x
  ld    e,a                   ;new x
  jr    nc,.loop
  inc   d                     ;0100 0000

  jp    p,.loop

  set   6,d
  res   7,d

  ld    a,(blitpage)
  xor   1
  out   ($99),a               ;write page instellen
  ld    a,14+128
  out   ($99),a
  
  ld    a,e
  jp    .loop

  .exit:
  ld    sp,(spatpointer)
  ret
  
putplayer_clipright_totallyoutofscreenright:
  inc   hl                    ;player y offset
  inc   hl                    ;player x offset for first line
  inc   hl                    ;player x offset for first line
  inc   bc                    ;player x
  ld    a,(bc)                ;player x
  sub   a,moveplayerleftinscreen
  add   a,(hl)                ;add player x offset for first line
  ld    e,a
  jp    SetOffsetBlocksAndAttackpoints
  
putplayer_clipright:
  ld    a,(bc)                ;player x
  sub   a,moveplayerleftinscreen
  add   a,(hl)                ;add player x offset for first line
  ld    e,a
  call  SetOffsetBlocksAndAttackpoints
  inc   hl                    ;lenght + increment first spriteline

  ;if screenpage=0 then blit in page 1
  ;if screenpage=1 then blit in page 2
  ;if screenpage=2 then blit in page 0
  ld    a,(screenpage)
  inc   a
  cp    3
  jr    nz,.not3
  xor   a
  .not3:  
  add   a,a

  bit   7,d

  jp    z,.setpage
  inc   a
  .setpage:
  ld    (blitpage),a
  out   ($99),a               ;write page instellen
  ld    a,14+128
  out   ($99),a

  srl   d                     ;write addres is de/2
  rr    e

  set   6,d                   ;write access

  ld    (spatpointer),sp  
  ld    sp,hl

  ld    a,e
  ld    c,$98
  .loop:
  out   ($99),a               ;set x to write to
  ld    a,d
  out   ($99),a               ;set y to write to

  pop   hl                    ;pop lenght + increment  
  ld    b,h                   ;length

  ;extra code in case of clipping right
    ;first check if total piece is out of screen right (or x<64)
  bit   6,e                   
  jr    z,.totallyoutofscreenright
  
    ;check if piece is fully within screen
  ld    a,e                   ;x
  or    %1000 0000

  add   a,b
  jr    nc,.endoverflowcheck1  ;nc-> piece is fully within screen
    
  sub   a,b
  neg
  ld    b,a
  .endoverflowcheck1:
  ;/extra code in case of clipping right

  ld    a,l                   ;increment
  pop   hl                    ;pop source address

  otir
  .skipotir:
  or    a
  jr    z,.exit

  add   a,e                   ;add increment to x
  ld    e,a                   ;new x
  jr    nc,.loop
  inc   d                     ;0100 0000

  jp    p,.loop

  set   6,d
  res   7,d

  ld    a,(blitpage)
  xor   1
  out   ($99),a               ;write page instellen
  ld    a,14+128
  out   ($99),a
  
  ld    a,e
  jp    .loop

  .exit:
  ld    sp,(spatpointer)
  ret

.totallyoutofscreenright:
  ld    a,l
  pop   hl
  jp    .skipotir             ;piece is totally out of screen, dont otir


putplayer_clipleft:
  ld    a,(bc)
  add   a,(hl)
  sub   a,moveplayerleftinscreen
  ld    e,a
  jp    nc,.notcarry
  dec   d
  .notcarry:
  call  SetOffsetBlocksAndAttackpoints
  inc   hl                    ;lenght + increment first spriteline

  ;if screenpage=0 then blit in page 1
  ;if screenpage=1 then blit in page 2
  ;if screenpage=2 then blit in page 0
  ld    a,(screenpage)
  inc   a
  cp    3
  jr    nz,.not3
  xor   a
  .not3:  
  add   a,a

  bit   7,d

  jp    z,.setpage
  inc   a
  .setpage:
  ld    (blitpage),a
  out   ($99),a               ;write page instellen
  ld    a,14+128
  out   ($99),a

  srl   d                     ;write addres is de/2
  rr    e

  set   6,d                   ;write access

  ld    (spatpointer),sp  
  ld    sp,hl

  ld    a,e
  ld    c,$98
  .loop:

  pop   hl                    ;pop lenght + increment  
  ld    b,h                   ;length

  ;check if piece is fully in screen
  bit   6,e                   ;first check if total piece is in screen left (or x<64)
  jr    z,.totallyinscreen    ;z-> piece is fully within screen left

  ;look at current x, add lenght, set new lenght accordingly, and then dont output if piece is totally out of screen
  ld    a,e
  or    %1000 0000
  ld    h,a
  add   a,b
  ld    b,a                   ;set new lenght (this is the part that is in screen)
  dec   a
  jp    m,.totallyoutofscreen

  ;set new write address
  ld    a,h
  neg
  ld    h,a                   ;distance from x to border of screen
  add   a,e
  out   ($99),a               ;set x to write to
  ld    a,d
  adc   a,0

  jp    p,.nopageoverflow

  set   6,a
  res   7,a
  out   ($99),a               ;set y to write to

  ld    a,(blitpage)
  xor   1
  out   ($99),a               ;write page instellen
  ld    a,14+128
  .nopageoverflow:

  out   ($99),a               ;set y to write to

  .gosourceaddress:
  ;set new source address
  ld    a,l                   ;increment
  ex    af,af'                ;store increment
  ld    a,h                   ;distance from x to border of screen

  pop   hl                    ;source address
  add   a,l                   ;add distance from x to border of screen to source address
  ld    l,a
  jr    nc,.noinch
  inc   h
  .noinch:

  ex    af,af'                ;recall stored increment
  otir
  jp    .skipotir

  .totallyoutofscreen:
  ld    a,l
  pop   hl
  jp    .skipotir             ;piece is totally out of screen, dont otir

  .totallyinscreen:
  ld    a,e
  out   ($99),a               ;set x to write to
  ld    a,d
  out   ($99),a               ;set y to write to

  ld    a,l                   ;increment
  pop   hl                    ;pop source address

  otir
  .skipotir:
  or    a                     ;check increment
  jr    z,.exit

  add   a,e                   ;add increment to x
  ld    e,a                   ;new x
  jr    nc,.loop

  inc   d                     ;01xx xxxx

  jp    p,.loop

  set   6,d
  res   7,d

  ld    a,(blitpage)
  xor   1
  out   ($99),a               ;write page instellen
  ld    a,14+128
  out   ($99),a
  
  ld    a,e
  jp    .loop

  .exit:
  ld    sp,(spatpointer)
  ret  

SetOffsetBlocksAndAttackpoints:
  Setblock1:
  ld    a,(bc)                ;player x
  or    a
  jp    p,PlayerLeftOfscreenSetBlocksAndattackpoints

PlayerRightOfscreenSetBlocksAndattackpoints:
  ;block 1
  sub   a,moveplayerleftinscreen
  inc   hl                    ;offsetx Block1
  add   a,(hl)
  jp    nc,.notcarry1
  ld    a,252                 ;if Sx Block1 is out of screen right, then set sx Block to 252
  .notcarry1:
  ld    (iy+0),a              ;Sx block 1
  inc   hl                    ;Nx block 1
  ld    a,(hl)
  ld    (iy+2),a              ;Nx block 1
  inc   hl                    ;Ny block 1
  ld    a,(hl)
  ld    (iy+3),a              ;Ny block 1
  add   a,(iy+1)              ;Ny block 1 + Sy block 1
  ld    (iy+5),a              ;Sy block 2

  ;block 2
  ld    a,(bc)                ;player x
  sub   a,moveplayerleftinscreen
  inc   hl                    ;offsetx Block1
  add   a,(hl)
  jp    nc,.notcarry2
  ld    a,252                 ;if Sx Block1 is out of screen right, then
  .notcarry2:
  ld    (iy+4),a              ;Sx block 2
  inc   hl                    ;Nx block 2
  ld    a,(hl)
  ld    (iy+6),a              ;Nx block 2
  ld    a,(ix+ny)             ;player height total (=Ny block 1 + Ny block 2)
  sub   a,(iy+3)
  ld    (iy+7),a              ;Ny block 2

  ;attack point1
  inc   hl                    ;attack point 1 offset x
  ld    a,(hl)
  or    a
  jr    z,.setattackpoint1sx  ;if there is no attack point, then set attackpoint1sx to 0

  ld    a,(bc)                ;player x
  add   a,(hl)
  jp    c,.carry1
  sub   a,moveplayerleftinscreen
  jp    .setattackpoint1sx
  .carry1:
  sub   a,moveplayerleftinscreen
  jp    c,.setattackpoint1sx
  ld    a,254
  .setattackpoint1sx:
  ld    (iy+8),a              ;attack point 1 sx
  inc   hl                    ;attack point 1 offset y
  dec   bc                    ;player y
  ld    a,(bc)                ;player y
  add   a,(hl)
  ld    (iy+9),a              ;attack point 1 sy

  ;attack point2
  inc   hl                    ;attack point 2 offset x
  inc   bc
  ld    a,(bc)                ;player x
  add   a,(hl)
  jp    c,.carry2
  sub   a,moveplayerleftinscreen
  jp    .setattackpoint2sx
  .carry2:
  sub   a,moveplayerleftinscreen
  jp    c,.setattackpoint2sx
  ld    a,254
  .setattackpoint2sx:
  ld    (iy+10),a             ;attack point 2 sx
  inc   hl                    ;attack point 2 offset y
  dec   bc
  ld    a,(bc)                ;player y
  add   a,(hl)
  ld    (iy+11),a             ;attack point 2 sy
  ret

PlayerLeftOfscreenSetBlocksAndattackpoints:
  ;block 1
  sub   a,moveplayerleftinscreen
  inc   hl                    ;offsetx Block1
  add   a,(hl)
  jp    c,.notcarry1
  ld    (iy+0),0              ;Sx block 1
  inc   hl                    ;Nx block 1
  add   a,(hl)
  jp    p,.positive1
  ld    a,1
  jp    .positive1
  .notcarry1:
  ld    (iy+0),a              ;Sx block 1
  inc   hl                    ;Nx block 1
  ld    a,(hl)
  .positive1:
  ld    (iy+2),a              ;Nx block 1
  inc   hl                    ;Ny block 1
  ld    a,(hl)
  ld    (iy+3),a              ;Ny block 1
  add   a,(iy+1)              ;Ny block 1 + Sy block 1
  ld    (iy+5),a              ;Sy block 2

  ;block 2
  ld    a,(bc)                ;player x
  sub   a,moveplayerleftinscreen
  inc   hl                    ;offsetx block 2
  add   a,(hl)
  jp    c,.notcarry2
  ld    (iy+4),0              ;Sx block 2
  inc   hl                    ;Nx block 2
  add   a,(hl)
  jp    p,.positive2
  ld    a,1
  jp    .positive2
  .notcarry2:
  ld    (iy+4),a              ;Sx block 2
  inc   hl                    ;Nx block 2
  ld    a,(hl)
  .positive2:
  ld    (iy+6),a              ;Nx block 2
  ld    a,(ix+ny)             ;player height total (=Ny block 1 + Ny block 2)
  sub   a,(iy+3)
  ld    (iy+7),a              ;Ny block 2

  ;attack point1
  inc   hl                    ;attack point 1 offset x
  ld    a,(hl)
  or    a
  jr    z,.setattackpoint1sx  ;if there is no attack point, then set attackpoint1sx to 0

  ld    a,(bc)                ;player x
  add   a,(hl)
  jp    nc,.notcarry3
  sub   a,moveplayerleftinscreen
  jp    .setattackpoint1sx
  
  .notcarry3:
  sub   a,moveplayerleftinscreen
  jr    z,.set1a
  cp    200
  jp    c,.setattackpoint1sx
  .set1a:
  ld    a,1
  .setattackpoint1sx:
  ld    (iy+8),a              ;attack point 1 sx
  inc   hl                    ;attack point 1 offset y
  dec   bc                    ;player y
  ld    a,(bc)                ;player y
  add   a,(hl)
  ld    (iy+9),a              ;attack point 1 sy

  ;attack point2
  inc   hl                    ;attack point 2 offset x
  inc   bc
  ld    a,(bc)                ;player x
  add   a,(hl)
  sub   a,moveplayerleftinscreen
  jr    z,.set1b
  cp    224
  jr    c,.setattackpoint2sx
.set1b:
  ld    a,1
  .setattackpoint2sx:
  ld    (iy+10),a             ;attack point 2 sx
  inc   hl                    ;attack point 2 offset y
  dec   bc
  ld    a,(bc)                ;player y
  add   a,(hl)
  ld    (iy+11),a             ;attack point 2 sy
  ret
	
keys: ds  12
PopulateKeyMatrix:
  in    a,($aa)
  and   %11110000
  ld    d,a
  ld    c,$a9
  ld    b,11
  ld    hl,keys + 11
.loop:
  ld    a,d
  or    b
  out   ($aa),a
  ind
  jp    nz,.Loop
  ld    a,d
  out   ($aa),a
  ind
  ret	

;
;Set VDP port #98 to start writing at address AHL (17-bit)
;
SetVdp_Write: 
;first set register 14 (actually this only needs to be done once)
	rlc     h
	rla
	rlc     h
	rla
	srl     h
	srl     h
	di
	out     ($99),a             ;set bits 15-17
	ld      a,14+128
	out     ($99),a
;/first set register 14 (actually this only needs to be done once)

	ld      a,l                 ;set bits 0-7
	nop
	out     ($99),a
	ld      a,h                 ;set bits 8-14
	or      64                  ; + write access
	ei
	out     ($99),a       
	ret
	
Depack:                       ;In: HL: source, DE: destination
	inc	hl		                  ;skip original file length
	inc	hl		                  ;which is stored in 4 bytes
	inc	hl
	inc	hl

	ld	a,128
	
	exx
	ld	de,1
	exx
	
depack_loop:
	call getbits
	jr	c,output_compressed	    ;if set, we got lz77 compression
	ldi				                  ;copy byte from compressed data to destination (literal byte)

	jr	depack_loop
	
;handle compressed data
output_compressed:
	ld	c,(hl)		              ;get lowest 7 bits of offset, plus offset extension bit
	inc	hl		                  ;to next byte in compressed data

output_match:
	ld	b,0
	bit	7,c
	jr	z,output_match1	        ;no need to get extra bits if carry not set

	call getbits
	call rlbgetbits
	call rlbgetbits
	call rlbgetbits

	jr	c,output_match1	        ;since extension mark already makes bit 7 set 
	res	7,c		                  ;only clear it if the bit should be cleared
output_match1:
	inc	bc
	
;return a gamma-encoded value
;length returned in HL
	exx			                    ;to second register set!
	ld	h,d
	ld	l,e                     ;initial length to 1
	ld	b,e		                  ;bitcount to 1

;determine number of bits used to encode value
get_gamma_value_size:
	exx
	call getbits
	exx
	jr	nc,get_gamma_value_size_end	;if bit not set, bitlength of remaining is known
	inc	b				                ;increase bitcount
	jr	get_gamma_value_size		;repeat...

get_gamma_value_bits:
	exx
	call getbits
	exx
	
	adc	hl,hl				            ;insert new bit in HL
get_gamma_value_size_end:
	djnz	get_gamma_value_bits	;repeat if more bits to go

get_gamma_value_end:
	inc	hl		                  ;length was stored as length-2 so correct this
	exx			                    ;back to normal register set
	ret	c
;HL' = length
	push	hl		                ;address compressed data on stack
	exx
	push	hl		                ;match length on stack
	exx
	ld	h,d
	ld	l,e		                  ;destination address in HL...
	sbc	hl,bc		                ;calculate source address
	pop	bc		                  ;match length from stack
	ldir			                  ;transfer data
	pop	hl		                  ;address compressed data back from stack
	jr	depack_loop

rlbgetbits:
	rl b
getbits:
	add	a,a
	ret	nz
	ld	a,(hl)
	inc	hl
	rla
	ret       

fm1_reg:	                    equ	  $c4
fm1_data:	                    equ	  $c5
fm2_reg:	                    equ	  $c6
fm2_data:	                    equ	  $c7
pcm_reg:	                    equ	  $7e
pcm_data:	                    equ	  $7f

P1SetSFX?: db  0
P2SetSFX?: db  0
ExtraSFX?: db  0

tonedata:	
  ds    3                     ;8bit sample / start address in moonsound
  ds    2                     ;sample lenght
  ds    2                     ;sample lenght inverted
  db    0						          ;LFO / VIB
  db    %11110000				      ;AT / D1R
  db    %00000000				      ;DL / D2R
  db    %00001111				      ;RC / RR
  db    %00000000				      ;AM

sampleaddress:
  ds    2
samplelenght:
  ds    2
MoonSoundSampleAddress:
  ds    3

ToneDataAddressSpecial:
  ds    2
P1SpecialSampleAddress:
  ds    3

ToneDataAddressCountry:
  ds    2
CountrySampleAddress:
  ds    3


;
; bit 7     6     5     4     3     2     1     0
;		  softk softp hardk hardp right left  down  up  (action)
;		  /     .     '     ;     right left  down  up  (ControlsP1)
;		  S     A     W     Q     G     D     V     R   (ControlsP2)
;		  1     2                 7     8     9     0   (TestControls)
;
Samples:
  ld    a,(ExtraSFX?)
  or    a
  jr    z,.endExtraSFX
  ld    c,a
  xor   a
  ld    (ExtraSFX?),a
  call  PlaySampleChannel3
  .endExtraSFX:

  ld    a,(P1SetSFX?)
  or    a
  jr    z,.endP1SFX
  ld    c,a
  xor   a
  ld    (P1SetSFX?),a
  call  PlaySampleChannel1
  .endP1SFX:

  ld    a,(P2SetSFX?)
  or    a
  ret   z
  ld    c,a
  xor   a
  ld    (P2SetSFX?),a
  jp    PlaySampleChannel2

PlaySampleChannel1:           ;this channel is used by player 1
  ld		a,$08                 ;register number
  call	opl4_Register_Write
  ld		a,$68                 ;register number
  ld		c,%0000 0000          ;key off, tone will only be played if status changes from OFF to ON
  call	opl4_Register_Write   ;In: A: register C: data, changes: a
  ld		a,$68                 ;register number
  ld		c,%1000 0000		      ;key on / damp / lfo reset / output channel selection / panpot (0111 = right, 0000 = center, 1001 = left)
  jp    opl4_Register_Write	

PlaySampleChannel2:           ;this channel is used by player 2
  ld		a,$09                 ;register number
  call	opl4_Register_Write
  ld		a,$69                 ;register number
  ld		c,%0000 0000          ;key off, tone will only be played if status changes from OFF to ON
  call	opl4_Register_Write   ;In: A: register C: data, changes: a
  ld		a,$69                 ;register number
  ld		c,%1000 0000		      ;key on / damp / lfo reset / output channel selection / panpot (0111 = right, 0000 = center, 1001 = left)
  jp    opl4_Register_Write	

PlaySampleChannel3:           ;this channel is used by player 2
  ld		a,$0a                 ;register number
  call	opl4_Register_Write
  ld		a,$6a                 ;register number
  ld		c,%0000 0000          ;key off, tone will only be played if status changes from OFF to ON
  call	opl4_Register_Write   ;In: A: register C: data, changes: a
  ld		a,$6a                 ;register number
  ld		c,%1000 0000		      ;key on / damp / lfo reset / output channel selection / panpot (0111 = right, 0000 = center, 1001 = left)
  jp    opl4_Register_Write	

;-------------------------------------------------------
;output data to OPL4
;In: A: register
;    C: data
;Out:
;Changes: a
opl4_Register_Write:
	out   (pcm_reg),a
  ld    a,c
	nop                         ;OPL4 is slow, and you need to wait in between writing to its register
	nop                         ;OPL4 is slow, and you need to wait in between writing to its register
	out   (pcm_data),a
	ret

StageFlagTile:
	db    0,0,22,1
	db    0,0,0,0
	db    16,0,12,0
	db    0,0,$d0
StageNameTile:
	db    0,0,0,1
	db    0,0,0,0
	db    0,0,7,0
	db    0,0,$98

DoCopyLockedFaces:
	db    0,0,0,1
	db    0,0,0,0
	db    28,0,28,0
	db    0,0,$d0
P1IconTop:
	db    196,0,0,1
	db    20,0,146 ,0
	db    31,0,08,0
	db    0,0,$98
P1IconBottom:
	db    196,0,8,1
	db    0,0,0,0
	db    31,0,06,0
	db    0,0,$98
P1BackupBackgroundTop:
	db    0,0,0,0
	db    0,0,0,1
	db    32,0,8,0
	db    0,0,$d0
P1BackupBackgroundBottom:
	db    0,0,0,0
	db    0,0,8,1
	db    32,0,6,0
	db    0,0,$d0
P1restoreBackgroundTop:
	db    0,0,0,1
	db    20,0,115,0
	db    31,0,8,0
	db    0,0,$90
P1restoreBackgroundBottom:
	db    0,0,8,1
	db    20,0,115,0
	db    31,0,6,0
	db    0,0,$90
P2IconTop:
	db    196,0,14,1
	db    206,0,146 ,0
	db    31,0,08,0
	db    0,0,$98
P2IconBottom:
	db    196,0,22,1
	db    0,0,0,0
	db    31,0,06,0
	db    0,0,$98
P2BackupBackgroundTop:
	db    0,0,0,0
	db    32,0,0,1
	db    32,0,8,0
	db    0,0,$d0
P2BackupBackgroundBottom:
	db    0,0,0,0
	db    32,0,8,1
	db    32,0,6,0
	db    0,0,$d0
P2restoreBackgroundTop:
	db    32,0,0,1
	db    20,0,115,0
	db    31,0,8,0
	db    0,0,$90
P2restoreBackgroundBottom:
	db    32,0,8,1
	db    20,0,115,0
	db    31,0,6,0
	db    0,0,$90
SelectStage1:
	db    146,0,212,0
	db    06,0,36,1
	db    146,1,20,0
	db    0,%0000 0100,$90
SelectStage2:
	db    0,0,232,0
	db    -8,0,57,1
	db    0,1,8,0
	db    0,0,$90
Docopycountries:
	db    221,0,212,0
	db    100,0,100,0
	db    35,0,10,0
	db    0,0,$98	
CountrySelecticon:
	db    147,0,212,0
	db    100,0,100,1
	db    38,0,14,0
	db    0,0,$98	
restoreBackgroundCountrySelect:
	db    0,0,0,1
	db    0,0,0,1
	db    38,0,14,0
	db    0,0,$d0
BackupBackgroundCountrySelect:
	db    0,0,0,1
	db    0,0,0,1
	db    38,0,14,0
	db    0,0,$d0
putletter:
	db    0,0,240,0
	db    100,0,0,1
	db    9,0,9,0
	db    0,0,$98
NextStageText:
	db    0,0,249,0
	db    -8,0,57,1
	db    0,1,7,0
	db    0,0,$90
PutContinueLetter:
	db    0,0,0,2
	db    0,0,0,0
	db    10,0,14,0
	db    0,0,$d0  

spat:						;sprite attribute table
	db		066,034,000,0	,066,034,004,0	,066,050,008,0	,066,050,012,0
	db		082,034,016,0	,082,034,020,0	,082,050,024,0	,082,050,028,0
	db		066,096,032,0	,066,096,036,0	,066,112,040,0	,066,112,044,0
	db		082,096,048,0	,082,096,052,0	,082,112,056,0	,082,112,060,0

	db		220,000,064,0	,220,000,068,0	,220,000,072,0	,220,000,076,0
	db		220,000,080,0	,220,000,084,0	,220,000,088,0	,220,000,092,0
	db		220,000,096,0	,220,000,100,0	,220,000,104,0	,220,000,108,0
	db		220,000,112,0	,220,000,116,0	,220,000,120,0	,220,000,124,0

endenginepage3:
dephase
enginepage3length:	          Equ	$-enginepage3

variables:                    org $c000+enginepage3length
slot:						
.ram:		                      equ	  $e000
.page1rom:	                  equ	  slot.ram+1
.page2rom:	                  equ	  slot.ram+2
.page12rom:	                  equ	  slot.ram+3
memblocks:
.1:			                      equ	  slot.ram+4
.2:			                      equ	  slot.ram+5
.3:			                      equ	  slot.ram+6
.4:			                      equ	  slot.ram+7	
VDP_0:		                    equ   $F3DF
VDP_8:		                    equ   $FFE7
engaddr:	                    equ	  $03e
loader.address:               equ   $8000
enginepage3addr:              equ   $c000
base:                         equ   $4000         ;address of heroframes
sx:                           equ   0
sy:                           equ   2
spage:                        equ   3
dx:                           equ   4
dy:                           equ   6
dpage:                        equ   7 
nx:                           equ   8
ny:                           equ   10
copytype:                     equ   14
movescreendown:               equ   60
MoveBackXBorder:              equ   120

sfxSoftAttack:                equ   128 + 00
sfxHardAttack:                equ   128 + 01
sfxSoftPunchHit:              equ   128 + 02
sfxHardPunchHit:              equ   128 + 03
sfxSoftKickHit:               equ   128 + 04
sfxHardKickHit:               equ   128 + 05
sfxMalethrow:                 equ   128 + 06
sfxFemalethrow:               equ   128 + 07
sfxDefend:                    equ   128 + 08
sfxLanding:                   equ   128 + 09
sfxKnockdown:                 equ   128 + 10
sfxMaleDies:                  equ   128 + 11
sfxFemaleDies:                equ   128 + 12
sfxone:                       equ   128 + 13
sfxtwo:                       equ   128 + 14
sfxthree:                     equ   128 + 15
sfxfinal:                     equ   128 + 16
sfxround:                     equ   128 + 17
sfxperfect:                   equ   128 + 18
sfxyou:                       equ   128 + 19
sfxwin:                       equ   128 + 20
sfxlose:                      equ   128 + 21
sfxfight:                     equ   128 + 22
sfxcursormove:                equ   128 + 23
sfxmenuselect:                equ   128 + 24
sfxcharacterselected:         equ   128 + 25
sfxplane:                     equ   128 + 26
sfxcountdown:                 equ   128 + 27
sfxP1Special1:                equ   128 + 28
sfxP1Special2:                equ   128 + 29
sfxP1Special3:                equ   128 + 30
sfxP2Special1:                equ   128 + 31
sfxP2Special2:                equ   128 + 32
sfxP2Special3:                equ   128 + 33
sfxCountry:                   equ   128 + 34

Opl3Present?:                 rb    1
Opl4Present?:                 rb    1
currentpalette:               rb    16*2
editablepalette:              rb    16*2
TestControls:                 rb		1
ControlsP1:                   rb		1
ControlsP2:                   rb		1
NewPrControlsP1:              rb		1
NewPrControlsP2:              rb		1
framecounter:                 rb		1
spatpointer:                  rb		2
Background:                   rb    1             ;0=Ryu, 1=Chunli, 2=Dhalsim  
Player1Male?:                 rb    1             ;0=Female, 1=Male
Player2Male?:                 rb    1             ;0=Female, 1=Male
Player1:                      rb    1             ;0=Ryu, 1=Chunli, 2=Dhalsim  
Player2:                      rb    1             ;0=Ryu, 1=Chunli, 2=Dhalsim
PlayerPriority:               rb    1             ;1=player1   2=player2 (which player comes in front of the other ?)
P1StaticDirection:            rb    1             ;Used only in Actions which won't change direction (Jump, attack)
P2StaticDirection:            rb    1             ;Used only in Actions which won't change direction (Jump, attack)
Player1JumpDirection:         rb    1             ;0= player jumps straight up, 1=jumps right, 2=jumps left
Player2JumpDirection:         rb    1             ;0= player jumps straight up, 1=jumps right, 2=jumps left

P1HitAnimationStep:           rb    1
P1JumpHitAnimationStep:       rb    1
P1HeavilyHitAnimationStep:    rb    1
P1GettingTossedAnimationStep: rb    1
P2HitAnimationStep:           rb    1
P2JumpHitAnimationStep:       rb    1
P2HeavilyHitAnimationStep:    rb    1
P2GettingTossedAnimationStep: rb    1
P1AttackingInAir?:            rb    2             ;Currently attacking in air?, already attacked once this jump?
P2AttackingInAir?:            rb    2             ;Currently attacking in air?, already attacked once this jump?
HandleWhichPlayer?:           rb    1
player1Action:                rb    1
player2Action:                rb    1
emptyVar:                     rb    1
freezeplayer1?:               rb    1
freezeplayer2?:               rb    1

P1TossCooldown?:              rb    1
P2TossCooldown?:              rb    1

Player1Hp:                    rb    1
Player1HpbarHp:               rb    1
Player2Hp:                    rb    1
Player2HpbarHp:               rb    1
changeHpbarPlayer1?:          rb    1
changeHpbarPlayer2?:          rb    1
HpbarP1ChangeStep:            rb    1
HpbarP2ChangeStep:            rb    1

Player1SxB1:                  rb    1
Player1SyB1:                  rb    1
Player1NxB1:                  rb    1
Player1NyB1:                  rb    1
Player1SxB2:                  rb    1
Player1SyB2:                  rb    1
Player1NxB2:                  rb    1
Player1NyB2:                  rb    1
P1Attpoint1Sx:                rb    1
P1Attpoint1Sy:                rb    1
P1Attpoint2Sx:                rb    1
P1Attpoint2Sy:                rb    1
Player2SxB1:                  rb    1
Player2SyB1:                  rb    1
Player2NxB1:                  rb    1
Player2NyB1:                  rb    1
Player2SxB2:                  rb    1
Player2SyB2:                  rb    1
Player2NxB2:                  rb    1
Player2NyB2:                  rb    1
P2Attpoint1Sx:                rb    1
P2Attpoint1Sy:                rb    1
P2Attpoint2Sx:                rb    1
P2Attpoint2Sy:                rb    1
Player1SxB1Projectile:        rb    1
Player1SyB1Projectile:        rb    1
Player1NxB1Projectile:        rb    1
Player1NyB1Projectile:        rb    1
Player1SxB2Projectile:        rb    1
Player1SyB2Projectile:        rb    1
Player1NxB2Projectile:        rb    1
Player1NyB2Projectile:        rb    1
P1Attpoint1SxProjectile:      rb    1
P1Attpoint1SyProjectile:      rb    1
P1Attpoint2SxProjectile:      rb    1
P1Attpoint2SyProjectile:      rb    1
Player2SxB1Projectile:        rb    1
Player2SyB1Projectile:        rb    1
Player2NxB1Projectile:        rb    1
Player2NyB1Projectile:        rb    1
Player2SxB2Projectile:        rb    1
Player2SyB2Projectile:        rb    1
Player2NxB2Projectile:        rb    1
Player2NyB2Projectile:        rb    1
P2Attpoint1SxProjectile:      rb    1
P2Attpoint1SyProjectile:      rb    1
P2Attpoint2SxProjectile:      rb    1
P2Attpoint2SyProjectile:      rb    1
prepareActionP1:              rb    1
fadeoutmusic?:                rb    1
fadeinout?:                   rb    1             ;0=nothing, 1=fade in, 2=fade out
fadeinoutstep:                rb    1
CurrentRound:                 rb    1             ;1=Round1, 2=Round2, 3=Round3, 4=Final Round
AnnounceRound?:               rb    1             ;0=no, 1=yes
AnnounceRoundStep:            rb    1
freezecontrols?:              rb    1             ;players cant move until announcer says: "Fight!"
P1RoundsWon:                  rb    1             ;how many round have the players won
P2RoundsWon:                  rb    1             ;how many round have the players won
fightended?:                  rb    1             ;0=no, 1=fight, go to next round or go the menu
P1Dies?:                      rb    1
P2Dies?:                      rb    1
PlayeryAtstart:               equ   60+movescreendown
player1y:                     rb    1
player1x:                     rb    1
player2y:                     rb    1
player2x:                     rb    1
backgroundmusicblock:         rb    1
capcomlogostep:               rb    1
P1ButtonPressed?:             rb    1
P2ButtonPressed?:             rb    1
VAdjust:                      rb    1
StageSelectPointer:           rb    1
character01unlocked?:         rb    1             ;bison
character02unlocked?:         rb    1             ;titanic tim
character03unlocked?:         rb    1             ;harman do elan
character04unlocked?:         rb    1             ;fei long
character05unlocked?:         rb    1             ;mai shiranui
character06unlocked?:         rb    1             ;guy
character07unlocked?:         rb    1             ;dee jay

character08unlocked?:         rb    1             ;blanka
character09unlocked?:         rb    1             ;scorpio
character10unlocked?:         rb    1             ;billy kane
character11unlocked?:         rb    1             ;mike haggar
character12unlocked?:         rb    1             ;joe higashi
character13unlocked?:         rb    1             ;damnd
character14unlocked?:         rb    1             ;t hawk  

character00defeated?:         rb    1
character01defeated?:         rb    1
character02defeated?:         rb    1
character03defeated?:         rb    1
character04defeated?:         rb    1
character05defeated?:         rb    1
character06defeated?:         rb    1
character07defeated?:         rb    1
character08defeated?:         rb    1
character09defeated?:         rb    1
character10defeated?:         rb    1
character11defeated?:         rb    1
character12defeated?:         rb    1
character13defeated?:         rb    1
character14defeated?:         rb    1
character15defeated?:         rb    1
character16defeated?:         rb    1
character17defeated?:         rb    1
character18defeated?:         rb    1
character19defeated?:         rb    1
character20defeated?:         rb    1
P1IconTopdxOld:               rb    1
P1IconTopdyOld:               rb    1
player1mayChooseChar?:        rb    1
ContinuePage:                 rb    1
whichlineint?:                rb    1
flickerstep:                  rb    1





tempstorage:                  rb    1
oldbackground:                rb    1             ;used for stage select putting stage text
mainmenulineinton?:           rb    1             ;lineint used for palettesplits
ylineint:                     rb    1             ;y of lineint
mainmenupage:                 rb    1             ;y of lineint
mainmenuselectioniconpointer: rb    1             ;points to main menu selection (arcade, vs, trials or option)
mainmenuoptionselected?:      rb    1             ;0=nothing, 1=arcade, 2=vs, 3=trials, 4=option
SearchTemp:                   rb    1
FMSlot:                       rb    1             ;0=fmpac NOT found









endenginepage3variables:      equ $+enginepage3length
org variables
