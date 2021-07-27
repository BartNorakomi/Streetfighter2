phase	engaddr

LevelEngine:
  call  switchpage
  call  playmusic
	call  PopulateControls      ;read out keyboard or joystick data
  call  CheckSpecialActions   ;check controls for a special move
  call  CheckFreezeControls   ;At start and at end of game you cant move
  call  HandlePlayers         ;restore background P1, handle action P1, put P1 in screen, play music, 
                              ;restore background P2, handle action P2, put P2 in screen, collision detection, set prepared collision action
  call  PlayersCloseTogether  ;make sure players cant move through each other. always keep distance.
  call  CheatControls         ;Use "1","2","3",... for testing purposes
  call  HandleTossCoolDown    ;after having tossed you cannot toss for several seconds (this is to prevent toss stacking)
  call  Handleprojectile
  call  HandleScoreboard      ;handles lifebar of players when they are hit
  call  Samples               ;moonsound sound effects
  call  ScreenFadeInOut       ;handle screen fading in/out
  call  Announcer             ;At start of fight announce round number
  ld    a,(framecounter)
  inc   a
  ld    (framecounter),a

  ld    a,(fightended?)
  or    a
  jr    nz,.end

  ld    a,1
  ld    hl,vblankflag
  .vblankwait:
  cp    (hl)
  jr    nc,.vblankwait
  ld    (hl),0
  jp    LevelEngine
	
	.end:
	ld		a,(slot.page12rom)	  ;all RAM except page 1+2
	out		($a8),a	
	ld		a,loaderblock
	jp    block34			          ;at address $8000 / page 2  

vblankflag: db 0
InterruptHandler1:            ;this one has music on the interrupt, and is not used during fights
  push  af

  ld    a,1                   ;set s#1
  out   ($99),a
  ld    a,15+128
  out   ($99),a 
  in    a,($99)               ;check and acknowledge line interrupt 
  rrca

  jp    c,LineInt             ;lineint detected, so jp to that routine

  xor   a                     ;set s#0
  out   ($99),a
  ld    a,15+128
  out   ($99),a
  in    a,($99)               ;check and acknowledge vblank interrupt
  rlca
  jr    nc,.Novblank
  .vblank:
  ld    a,(vblankflag)
  inc   a
  ld    (vblankflag),a

  push  hl
  exx
  ex    af,af'
  push  af
  push  bc
  push  de
  push  hl 
  push  ix 
  call  playmusic
;  - Updates music/SFX.
;  - Changes: AF, AF', BC, DE, HL, HL', IX
  pop   ix
  pop   hl
  pop   de
  pop   bc
  pop   af
  ex    af,af'
  exx
  pop   hl

  pop   af
  ei
  ret
  .Novblank:
  pop   af
  ei
  ret

InterruptHandler2:            ;this one has no music on the interrupt, and is used during fights
  push  af
  in    a,($99)               ;check and acknowledge vblank interrupt
  rlca
  jr    nc,.Novblank
  .vblank:
  ld    a,(vblankflag)
  inc   a
  ld    (vblankflag),a
  pop   af
  ei
  ret
  .Novblank:
  pop   af
  ei
  ret


;Action=0	 Idle
;Action=1	 Bend
;Action=2	 Walk
;Action=3	 Jump
;Action=4	 SoftStandPunch
;Action=5	 HardStandPunch
;Action=6	 SoftStandKick
;Action=7	 HardStandKick
;Action=8	 SoftSitPunch
;Action=9	 HardSitPunch
;Action=10 SoftSitKick
;Action=11 HardSitKick
;Action=12 JumpSoftPunch
;Action=13 JumpHardPunch
;Action=14 JumpSoftKick
;Action=15 JumpHardKick
;Action=16 StandDefend
;Action=17 BendDefend
;Action=18 StandHit
;Action=19 BendHit
;Action=20 Jumphit
;Action=21 HeavilyHit
;Action=22 KnockDownRecover
;Action=23 StandDefendHit
;Action=24 BendDefendHit
;Action=25 Special1
;Action=26 Special2
;Action=27 Special3
;Action=28 Special4
;Action=29 Special5
;Action=30 Toss
;Action=31 GettingTossed
;Action=32 Victory
SpriteActionTable:  
  dw    Idle, Bend, Walk, Jump, SoftStandPunch, HardStandPunch
  dw    SoftStandKick, HardStandKick, SoftSitPunch, HardSitPunch
  dw    SoftSitKick, HardSitKick, JumpSoftPunch, JumpHardPunch
  dw    JumpSoftKick, JumpHardKick, StandDefend, BendDefend, StandHit
  dw    BendHit, Jumphit, HeavilyHit, KnockDownRecover, StandDefendHit, BendDefendHit
  dw    Special1, Special2, Special3, Special4, Special5, Toss, GettingTossed, Victory

HandlePlayerAction:
  add   a,a
  ld    d,0
  ld    e,a
  ld    hl,SpriteActionTable
  add   hl,de
  ld    e,(hl)
  inc   hl
  ld    d,(hl)
  ex    de,hl
  jp    (hl)
  
HandlePlayers:                ;players should be handled 1 by 1, the one with highest priority first
  ld    a,(PlayerPriority)    ;1=player1   2=player2 (which player comes in front of the other ?)
  dec   a
  jr    nz,.P1first

  .P2first:
  ;handle player 2
  call  restoreBackgroundP2
  call  handleP2Action
  call  putplayer2
  ;handle music in between
  call  playmusic
  ;handle player 1
  call  restoreBackgroundP1
  call  handleP1Action
  call  putplayer1
  ;now check for collision
  call  Checkplayer1hit       ;this prepares action in case player 1 is hit
  call  Checkplayer2hit
  call  SetPreparedActionP1   ;now set the action which we prepared
  call  Checkplayer1hitbyprojectile
  call  Checkplayer2hitbyprojectile
  ret

  .P1first:
  ;handle player 1
  call  restoreBackgroundP1
  call  handleP1Action
  call  putplayer1
  ;handle music in between
  call  playmusic
  ;handle player 2
  call  restoreBackgroundP2
  call  handleP2Action
  call  putplayer2
  ;now check for collision
  call  Checkplayer1hit       ;this prepares action in case player 1 is hit
  call  Checkplayer2hit
  call  SetPreparedActionP1   ;now set the action which we prepared
  call  Checkplayer1hitbyprojectile
  call  Checkplayer2hitbyprojectile
  ret

  
  handleP1Action:
  ;check if any player died, then handle PlayerActions every other interrupt (which results in a slowdown of the game)
  ld    a,(P1Dies?)
  ld    hl,P2Dies?
  or    (hl)                  ;check if a player died
  jp    z,.endcheckplayerdied

  ;at this point one or both players have died
  ld    a,(framecounter)
  and   1                     ;handle PlayerActions every other interrupt (which results in a slowdown of the game)
  jp    z,.endcheckplayerdied

  ld    a,(P1Dies?)
  or    a
  jp    nz,Player1Died
  ret

  .endcheckplayerdied:
  ld    a,(freezeplayer1?)    ;when you hit your opponent you freeze shortly
  or    a
  jp    z,.notFrozen
  dec   a
  ld    (freezeplayer1?),a    ;when you hit your opponent you freeze shortly
  ret   nz
  .notFrozen:
  ld    a,1
  ld    (HandleWhichPlayer?),a
  ld    a,(player1Action)
  jp    HandlePlayerAction

  handleP2Action:
  ;check if any player died, then handle PlayerActions every other interrupt (which results in a slowdown of the game)
  ld    a,(P1Dies?)
  ld    hl,P2Dies?
  or    (hl)                  ;check if a player died
  jp    z,.endcheckplayerdied

  ;at this point one or both players have died
  ld    a,(framecounter)
  and   1                     ;handle PlayerActions every other interrupt (which results in a slowdown of the game)
  jp    z,.endcheckplayerdied

  ld    a,(P2Dies?)
  or    a
  jp    nz,Player2Died
  ret

  .endcheckplayerdied:
  ld    a,(freezeplayer2?)    ;when you hit your opponent you freeze shortly
  or    a
  jp    z,.notFrozen
  dec   a
  ld    (freezeplayer2?),a    ;when you hit your opponent you freeze shortly
  ret   nz
  .notFrozen:
  ld    a,2
  ld    (HandleWhichPlayer?),a
  ld    a,(player2Action)
  jp    HandlePlayerAction
























CheatControls:                ;here some cheat controls can be added for testing purposes
  ld    a,(TestControls)
  bit   7,a
  jp    nz,.P1
  bit   6,a
  jp    nz,.P2
  bit   0,a
  jp    nz,.P11
  bit   1,a
  jp    nz,.P22
  bit   2,a
  jp    nz,.P111
  bit   3,a
  jp    nz,.P222
  ret
  .P1:
  ld    a,1
  ld    (HandleWhichPlayer?),a
  
  jp setheavilyhit
  
  jp    SetSpecial1.test
  .P2:
  ld    a,2
  ld    (HandleWhichPlayer?),a
  jp    SetSpecial1.test
  .P11:
  ld    a,1
  ld    (HandleWhichPlayer?),a
  
  jp setjumphit
  
  jp    SetSpecial2.test
  .P22:
  ld    a,2
  ld    (HandleWhichPlayer?),a
  jp    SetSpecial2.test
  .P111:
  ld    a,1
  ld    (HandleWhichPlayer?),a
  jp    SetSpecial3.test
  .P222:
  ld    a,2
  ld    (HandleWhichPlayer?),a
  jp    SetSpecial3.test







  
  









Idle:
  ld    a,(P1Dies?)
  ld    hl,P2Dies?
  or    (hl)                  ;check if a player died
  ret   nz

  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  jp    c,.IdleLeft

  .Idleright:
  ld    ix,P1RightIdleFrame
  ld    iy,Player1Frame
  jr    z,.Go
  ld    ix,P2RightIdleFrame
  ld    iy,Player2Frame
  jp    .Go

  .IdleLeft:
  ld    ix,P1LeftIdleFrame
  ld    iy,Player1Frame
  jr    z,.Go
  ld    ix,P2LeftIdleFrame
  ld    iy,Player2Frame
  .Go:
  ;
  ; bit 7     6     5     4     3     2     1     0
  ;		  softk softp hardk hardp right left  down  up  (action)
  ;		  /     .     '     ;     right left  down  up  (ControlsP1)
  ;		  S     A     W     Q     right left  down  up  (ControlsP2)
  ;
  call  CheckControlsinBC     ;out a->controls, b=x move, c=y move, changes: af, bc
  dec   c                     ;down pressed ?
  jp    z,SetBend
  inc   c                     ;up pressed ?
  jp    nz,.uppressed

  dec   b                     ;right pressed ?
  jp    z,SetWalkRight
  inc   b                     ;not right nor left pressed ?
  jp    nz,SetWalkLeft
  ;
  ; bit 7     6     5     4     3     2     1     0
  ;		  softk softp hardk hardp right left  down  up  (action)
  ;		  /     .     '     ;     right left  down  up  (ControlsP1)
  ;		  S     A     W     Q     right left  down  up  (ControlsP2)
  ;
  call  GetNewPrControls      ;out a-> NewPrcontrols, Changes: af
  bit   6,a
  jp    nz,SetSoftStandPunch
  bit   4,a
  jp    nz,SetHardStandPunch
  bit   7,a
  jp    nz,SetSoftStandKick
  bit   5,a
  jp    nz,SetHardStandKick
  
  jp    AnimatePlayer         ;if left NOR right is pressed, then stay in Idle

  .uppressed:
  dec   b                     ;right pressed ?
  jp    z,SetjumpRight
  inc   b                     ;not right nor left pressed ?
  jp    z,SetjumpStraightUp   ;if left NOR right is pressed, then jump   
  jp    SetjumpLeft

Bend:
  ;
  ; bit 7     6     5     4     3     2     1     0
  ;		  softk softp hardk hardp right left  down  up  (action)
  ;		  /     .     '     ;     right left  down  up  (ControlsP1)
  ;		  S     A     W     Q     right left  down  up  (ControlsP2)
  ;
  call  GetNewPrControls      ;out a-> NewPrcontrols, Changes: af
  bit   6,a
  jp    nz,SetSoftSitPunch
  bit   4,a
  jp    nz,SetHardSitPunch
  bit   7,a
  jp    nz,SetSoftSitKick
  bit   5,a
  jp    nz,SetHardSitKick
  ;
  ; bit 7     6     5     4     3     2     1     0
  ;		  softk softp hardk hardp right left  down  up  (action)
  ;		  /     .     '     ;     right left  down  up  (ControlsP1)
  ;		  S     A     W     Q     right left  down  up  (ControlsP2)
  ;
  call  CheckControlsinBC     ;out a->controls, b=x move, c=y move, changes: af, bc
  dec   c                     ;down pressed ?
  jp    z,.DownPressed        ;in case you have to change direction, SetBend instead of ret

  dec   b                     ;right pressed ?
  jp    z,SetWalkRight
  inc   b                     ;not right nor left pressed ?
  jp    z,SetIdle             ;if left NOR right is pressed, then set Idle
  jp    SetWalkLeft

  .DownPressed:
  dec   b                     ;right pressed ?
  jp    z,.RightPressed
  inc   b
  jp    z,SetBend

  .LeftPressed:
  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  jp    c,SetBend
  call  CheckEnemyAttacking   ;out-> c:enemy is attacking, changes: af
  jp    c,SetBendDefend
  jp    SetBend

  .RightPressed:  
  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  jp    nc,SetBend
  call  CheckEnemyAttacking   ;out-> c:enemy is attacking, changes: af
  jp    c,SetBendDefend
  jp    SetBend
  
Walk:
  ;
  ; bit 7     6     5     4     3     2     1     0
  ;		  softk softp hardk hardp right left  down  up  (action)
  ;		  /     .     '     ;     right left  down  up  (ControlsP1)
  ;		  S     A     W     Q     right left  down  up  (ControlsP2)
  ;
  call  GetNewPrControls      ;out a-> NewPrcontrols, Changes: af
  bit   6,a
  jp    nz,SetSoftStandPunch
  bit   4,a
  jp    nz,SetHardStandPunch
  bit   7,a
  jp    nz,SetSoftStandKick
  bit   5,a
  jp    nz,SetHardStandKick
  ;
  ; bit	7	  6	  5		    4		    3		    2		  1		  0
  ;		  0	  0	  trig-b	trig-a	right	  left	down	up	(joystick)
  ;		  F5	F1	'M'		  space	  right	  left	down	up	(keyboard)
  ;
  call  CheckControlsinBC     ;out a->controls, b=x move, c=y move, changes: af, bc
  dec   c                     ;down pressed ?
  jp    z,SetBend
  inc   c                     ;up pressed ?
  jp    z,.upnotpressed
  ;up is pressed, so jump
  dec   b                     ;right pressed ?
  jp    z,SetjumpRight
  inc   b                     ;not right nor left pressed ?
  jp    z,SetjumpStraightUp   ;if left NOR right is pressed, then jump   
  jp    SetjumpLeft

  .upnotpressed:
  ;set player x in hl
  ld    a,(HandleWhichPlayer?)
  dec   a
  ld    hl,player1x
  jr    z,.PlayerSet
  ld    hl,player2x
  .PlayerSet:
  ;/set player x in hl
  ld    a,b
  or    b                     ;is left OR right pressed ?
  jp    z,SetIdle             ;no ? then set Idle
  inc   a
  jp    z,.Walkleft           ;left if pressed, do walk left

  .Walkright:
  ;get movement speed
  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  jp    z,.Rplayer1
  .Rplayer2:
  ld    de,P2HorSpeedWalkFastTable
  jp    nc,.Goright           ;player walks forwards (walks right and looks right)
  call  CheckEnemyAttacking   ;out-> c:enemy is attacking, changes: af
  jp    c,SetStandDefend
  ld    de,P2HorSpeedWalkSlowTable
  jp    .Goright              ;player walks backwards (walks right and looks left)
  .Rplayer1:
  ld    de,P1HorSpeedWalkFastTable
  jp    nc,.Goright           ;player walks forwards (walks right and looks right)
  call  CheckEnemyAttacking   ;out-> c:enemy is attacking, changes: af
  jp    c,SetStandDefend
  ld    de,P1HorSpeedWalkSlowTable

  .Goright:
  call  GetmovementSpeed      ;in: de->table lenght, out: a=movem. speed, changes af, bc, de

  ld    b,a
  ld    a,(hl)
  add   a,b                   ;add movement speed to player x
  jp    c,.outofscreenright
  ld    (hl),a
  .outofscreenright:

  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  jr    c,.DoFaceLeftWalkRight

  .DoFaceRightWalkRight:
  ld    ix,P1RightWalkRightFrame
  ld    iy,Player1Frame
  jp    z,AnimatePlayer
  ld    ix,P2RightWalkRightFrame
  ld    iy,Player2Frame
  jp    AnimatePlayer
  .DoFaceLeftWalkRight:
  ld    ix,P1LeftWalkRightFrame
  ld    iy,Player1Frame
  jp    z,AnimatePlayer
  ld    ix,P2LeftWalkRightFrame
  ld    iy,Player2Frame
  jp    AnimatePlayer

  .Walkleft:  
  ;get movement speed
  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  jp    z,.Lplayer1
  .Lplayer2:
  ld    de,P2HorSpeedWalkFastTable
  jp    c,.Goleft             ;player walks forwards (walks left and looks left)
  call  CheckEnemyAttacking   ;out-> c:enemy is attacking, changes: af
  jp    c,SetStandDefend
  ld    de,P2HorSpeedWalkSlowTable
  jp    .Goleft               ;player walks backwards (walks left and looks right)
  .Lplayer1:
  ld    de,P1HorSpeedWalkFastTable
  jp    c,.Goleft             ;player walks forwards (walks left and looks left)
  call  CheckEnemyAttacking   ;out-> c:enemy is attacking, changes: af
  jp    c,SetStandDefend
  ld    de,P1HorSpeedWalkSlowTable

  .Goleft:
  call  GetmovementSpeed      ;in: de->table lenght, out: a=movem. speed, changes af, bc, de

  ld    b,a
  ld    a,(hl)
  sub   a,b                   ;sub movement speed from player x
  jp    c,.outofscreenleft
  ld    (hl),a
  .outofscreenleft:
  
  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  jr    c,.DoFaceLeftWalkLeft

  .DoFaceRightWalkLeft:
  ld    ix,P1RightWalkLeftFrame
  ld    iy,Player1Frame
  jp    z,AnimatePlayer
  ld    ix,P2RightWalkLeftFrame
  ld    iy,Player2Frame
  jp    AnimatePlayer
  .DoFaceLeftWalkLeft:
  ld    ix,P1LeftWalkLeftFrame
  ld    iy,Player1Frame
  jp    z,AnimatePlayer
  ld    ix,P2LeftWalkLeftFrame
  ld    iy,Player2Frame
  jp    AnimatePlayer

Jump:
  ld    a,(HandleWhichPlayer?)
  dec   a
  ld    a,(Player1JumpDirection)
  jr    z,.JumpDirectionFound
  ld    a,(Player2JumpDirection)
  .JumpDirectionFound:

  or    a
  jp    z,JumpStraightUp
  dec   a                     ;jump right ?
  jp    z,JumpRight

JumpLeft:
  call  .handleJump           ;first move and animate jump
  jp    CheckAttackWhileJump  ;then check if player pressed an attack button
  .handleJump:

  ;move player horizontally
  ld    a,(HandleWhichPlayer?)
  dec   a
  ld    hl,player1x
  ld    bc,player1y
  jr    z,.PlayerSet
  ld    hl,player2x
  ld    bc,player2y
  .PlayerSet:

  ld    a,(bc)
  cp    PlayeryAtstart        ;player landed ? then dont move horizontally
  jp    z,.endHorizontalMovement
  
    ;get movement speed
  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  jp    z,.Lplayer1
  .Lplayer2:
  ld    de,P2HorSpeedJumpFastTable
  jp    nc,.Goleft            ;player walks backwards or forwards/ slow or fast
  ld    de,P2HorSpeedJumpSlowTable
  jp    .Goleft               ;player walks backwards or forwards/ slow or fast
  .Lplayer1:
  ld    de,P1HorSpeedJumpFastTable
  jp    nc,.Goleft            ;player walks backwards or forwards/ slow or fast
  ld    de,P1HorSpeedJumpSlowTable

  .Goleft:
  call  GetmovementSpeed      ;in: de->table lenght, out: a=movem. speed, changes af, bc, de
    ;/get movement speed

  ld    b,a
  ld    a,(hl)
  sub   a,b                   ;sub movement speed from player x
  jp    c,.endHorizontalMovement
  ld    (hl),a
  .endHorizontalMovement:  
  ;/move player horizontally

  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af
  jp    c,.JumpLeftFaceLeft

  .JumpLeftFaceRight:
  ld    hl,P1jumptable
  ld    bc,player1y
  ld    de,P1RightJumpLeftStartframe+3
  jp    z,AnimateAndHandleJump
  ld    hl,P2jumptable
  ld    bc,player2y
  ld    de,P2RightJumpLeftStartframe+3
  jp    AnimateAndHandleJump

  .JumpLeftFaceLeft:
  ld    hl,P1jumptable
  ld    bc,player1y
  ld    de,P1LeftJumpLeftStartframe+3
  jp    z,AnimateAndHandleJump
  ld    hl,P2jumptable
  ld    bc,player2y
  ld    de,P2LeftJumpLeftStartframe+3
  jp    AnimateAndHandleJump

JumpRight:
  call  .handleJump           ;first move and animate jump
  jp    CheckAttackWhileJump  ;then check if player pressed an attack button
  .handleJump:

  ;move player horizontally
  ld    a,(HandleWhichPlayer?)
  dec   a
  ld    hl,player1x
  ld    bc,player1y
  jr    z,.PlayerSet
  ld    hl,player2x
  ld    bc,player2y
  .PlayerSet:

  ld    a,(bc)
  cp    PlayeryAtstart        ;player landed ? then dont move horizontally
  jp    z,.endHorizontalMovement
  
    ;get movement speed
  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  jp    z,.Rplayer1
  .Rplayer2:
  ld    de,P2HorSpeedJumpFastTable
  jp    c,.Goright            ;player walks backwards or forwards/ slow or fast
  ld    de,P2HorSpeedJumpSlowTable
  jp    .Goright              ;player walks backwards or forwards/ slow or fast
  .Rplayer1:
  ld    de,P1HorSpeedJumpFastTable
  jp    c,.Goright            ;player walks backwards or forwards/ slow or fast
  ld    de,P1HorSpeedJumpSlowTable

  .Goright:
  call  GetmovementSpeed      ;in: de->table lenght, out: a=movem. speed, changes af, bc, de
    ;/get movement speed

  ld    b,a
  ld    a,(hl)
  add   a,b                   ;add movement speed to player x
  jp    c,.endHorizontalMovement
  ld    (hl),a
  .endHorizontalMovement:  
  ;/move player horizontally

  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af
  jp    c,.JumpRightFaceLeft

  .JumpRightFaceRight:
  ld    hl,P1jumptable
  ld    bc,player1y
  ld    de,P1RightJumpRightStartframe+3
  jp    z,AnimateAndHandleJump
  ld    hl,P2jumptable
  ld    bc,player2y
  ld    de,P2RightJumpRightStartframe+3
  jp    AnimateAndHandleJump

  .JumpRightFaceLeft:
  ld    hl,P1jumptable
  ld    bc,player1y
  ld    de,P1LeftJumpRightStartframe+3
  jp    z,AnimateAndHandleJump
  ld    hl,P2jumptable
  ld    bc,player2y
  ld    de,P2LeftJumpRightStartframe+3
  jp    AnimateAndHandleJump

JumpStraightUp:
  call  .handleJump           ;first move and animate jump
  jp    CheckAttackWhileJump  ;then check if player pressed an attack button

  .handleJump:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af
  jp    c,.JumpStraightFaceLeft

  .JumpStraightFaceRight:
  ld    hl,P1jumptable
  ld    bc,player1y
  ld    de,P1RightJumpStraightStartframe+3
  jp    z,AnimateAndHandleJump
  ld    hl,P2jumptable
  ld    bc,player2y
  ld    de,P2RightJumpStraightStartframe+3
  jp    AnimateAndHandleJump

  .JumpStraightFaceLeft:
  ld    hl,P1jumptable
  ld    bc,player1y
  ld    de,P1LeftJumpStraightStartframe+3
  jp    z,AnimateAndHandleJump
  ld    hl,P2jumptable
  ld    bc,player2y
  ld    de,P2LeftJumpStraightStartframe+3
  jp    AnimateAndHandleJump

SoftStandPunch:
  call  .goAttack
  jp    CheckAttackOnTopOfSoftAttackPunch

  .goAttack:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af
  ld    iy,Player1Frame
  jp    c,.Left

  .Right:
  ld    ix,P1StandSoftPunchRightFrame
  jp    z,StandardAttackActionStand
  ld    ix,P2StandSoftPunchRightFrame
  ld    iy,Player2Frame
  jp    StandardAttackActionStand

  .Left:
  ld    ix,P1StandSoftPunchLeftFrame
  jp    z,StandardAttackActionStand
  ld    ix,P2StandSoftPunchLeftFrame
  ld    iy,Player2Frame
  jp    StandardAttackActionStand

HardStandPunch:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af
  ld    iy,Player1Frame
  jp    c,.Left

  .Right:
  ld    ix,P1StandHardPunchRightFrame
  jp    z,StandardAttackActionStand
  ld    ix,P2StandHardPunchRightFrame
  ld    iy,Player2Frame
  jp    StandardAttackActionStand

  .Left:
  ld    ix,P1StandHardPunchLeftFrame
  jp    z,StandardAttackActionStand
  ld    ix,P2StandHardPunchLeftFrame
  ld    iy,Player2Frame
  jp    StandardAttackActionStand

SoftStandKick:
  call  .goAttack
  jp    CheckAttackOnTopOfSoftAttackKick

  .goAttack:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af
  ld    iy,Player1Frame
  jp    c,.Left

  .Right:
  ld    ix,P1StandSoftKickRightFrame
  jp    z,StandardAttackActionStand
  ld    ix,P2StandSoftKickRightFrame
  ld    iy,Player2Frame
  jp    StandardAttackActionStand

  .Left:
  ld    ix,P1StandSoftKickLeftFrame
  jp    z,StandardAttackActionStand
  ld    ix,P2StandSoftKickLeftFrame
  ld    iy,Player2Frame
  jp    StandardAttackActionStand

HardStandKick:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af
  ld    iy,Player1Frame
  jp    c,.Left

  .Right:
  ld    ix,P1StandHardKickRightFrame
  jp    z,StandardAttackActionStand
  ld    ix,P2StandHardKickRightFrame
  ld    iy,Player2Frame
  jp    StandardAttackActionStand

  .Left:
  ld    ix,P1StandHardKickLeftFrame
  jp    z,StandardAttackActionStand
  ld    ix,P2StandHardKickLeftFrame
  ld    iy,Player2Frame
  jp    StandardAttackActionStand

SoftSitPunch:
  call  .goAttack
  jp    CheckAttackOnTopOfSoftAttackPunch

  .goAttack:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af
  ld    iy,Player1Frame
  jp    c,.Left

  .Right:
  ld    ix,P1SitSoftPunchRightFrame
  jp    z,StandardAttackActionSit
  ld    ix,P2SitSoftPunchRightFrame
  ld    iy,Player2Frame
  jp    StandardAttackActionSit

  .Left:
  ld    ix,P1SitSoftPunchLeftFrame
  jp    z,StandardAttackActionSit
  ld    ix,P2SitSoftPunchLeftFrame
  ld    iy,Player2Frame
  jp    StandardAttackActionSit

HardSitPunch:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af
  ld    iy,Player1Frame
  jp    c,.Left

  .Right:
  ld    ix,P1SitHardPunchRightFrame
  jp    z,StandardAttackActionSit
  ld    ix,P2SitHardPunchRightFrame
  ld    iy,Player2Frame
  jp    StandardAttackActionSit

  .Left:
  ld    ix,P1SitHardPunchLeftFrame
  jp    z,StandardAttackActionSit
  ld    ix,P2SitHardPunchLeftFrame
  ld    iy,Player2Frame
  jp    StandardAttackActionSit

SoftSitKick:
  call  .goAttack
  jp    CheckAttackOnTopOfSoftAttackKick

  .goAttack:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af
  ld    iy,Player1Frame
  jp    c,.Left

  .Right:
  ld    ix,P1SitSoftKickRightFrame
  jp    z,StandardAttackActionSit
  ld    ix,P2SitSoftKickRightFrame
  ld    iy,Player2Frame
  jp    StandardAttackActionSit

  .Left:
  ld    ix,P1SitSoftKickLeftFrame
  jp    z,StandardAttackActionSit
  ld    ix,P2SitSoftKickLeftFrame
  ld    iy,Player2Frame
  jp    StandardAttackActionSit

HardSitKick:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af
  ld    iy,Player1Frame
  jp    c,.Left

  .Right:
  ld    ix,P1SitHardKickRightFrame
  ld    hl,player1x
  jp    z,HardSitKickAction
  ld    ix,P2SitHardKickRightFrame
  ld    iy,Player2Frame
  ld    hl,player2x
  jp    HardSitKickAction

  .Left:
  ld    ix,P1SitHardKickLeftFrame
  ld    hl,player1x
  jp    z,HardSitKickAction
  ld    ix,P2SitHardKickLeftFrame
  ld    iy,Player2Frame
  ld    hl,player2x
  jp    HardSitKickAction

JumpSoftPunch:
  call  HandleJump            ;first handle the jump normally
  call  CheckJumpStraight     ;out zero=Jump Straight up, changes: af
  jp    z,.StraightUpSoftPunch

  .DiagonalSoftPunch:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af
  ld    iy,Player1Frame
  jp    c,.LeftSoftJumpPunchDiagonalUp
  .RightSoftJumpPunchDiagonalUp:;now write airattack frame, and check end airattack
  ld    ix,P1RightSoftJumpPunchDiagonalUp
  jp    z,StandardAttackActionJumping
  ld    ix,P2RightSoftJumpPunchDiagonalUp
  ld    iy,Player2Frame
  jp    StandardAttackActionJumping
  .LeftSoftJumpPunchDiagonalUp:;now write airattack frame, and check end airattack
  ld    ix,P1LeftSoftJumpPunchDiagonalUp
  jp    z,StandardAttackActionJumping
  ld    ix,P2LeftSoftJumpPunchDiagonalUp
  ld    iy,Player2Frame
  jp    StandardAttackActionJumping

  .StraightUpSoftPunch:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af
  ld    iy,Player1Frame
  jp    c,.LeftSoftJumpPunchStraightUp
  .RightSoftJumpPunchStraightUp:;now write airattack frame, and check end airattack
  ld    ix,P1RightSoftJumpPunchStraightUp
  jp    z,StandardAttackActionJumping
  ld    ix,P2RightSoftJumpPunchStraightUp
  ld    iy,Player2Frame
  jp    StandardAttackActionJumping
  .LeftSoftJumpPunchStraightUp:;now write airattack frame, and check end airattack
  ld    ix,P1LeftSoftJumpPunchStraightUp
  jp    z,StandardAttackActionJumping
  ld    ix,P2LeftSoftJumpPunchStraightUp
  ld    iy,Player2Frame
  jp    StandardAttackActionJumping

JumpHardPunch:
  call  HandleJump            ;first handle the jump normally
  call  CheckJumpStraight     ;out zero=Jump Straight up, changes: af
  jp    z,.StraightUpHardPunch

  .DiagonalHardPunch:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af
  ld    iy,Player1Frame
  jp    c,.LeftHardJumpPunchDiagonalUp
  .RightHardJumpPunchDiagonalUp:;now write airattack frame, and check end airattack
  ld    ix,P1RightHardJumpPunchDiagonalUp
  jp    z,StandardAttackActionJumping
  ld    ix,P2RightHardJumpPunchDiagonalUp
  ld    iy,Player2Frame
  jp    StandardAttackActionJumping
  .LeftHardJumpPunchDiagonalUp:;now write airattack frame, and check end airattack
  ld    ix,P1LeftHardJumpPunchDiagonalUp
  jp    z,StandardAttackActionJumping
  ld    ix,P2LeftHardJumpPunchDiagonalUp
  ld    iy,Player2Frame
  jp    StandardAttackActionJumping

  .StraightUpHardPunch:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af
  ld    iy,Player1Frame
  jp    c,.LeftHardJumpPunchStraightUp
  .RightHardJumpPunchStraightUp:;now write airattack frame, and check end airattack
  ld    ix,P1RightHardJumpPunchStraightUp
  jp    z,StandardAttackActionJumping
  ld    ix,P2RightHardJumpPunchStraightUp
  ld    iy,Player2Frame
  jp    StandardAttackActionJumping
  .LeftHardJumpPunchStraightUp:;now write airattack frame, and check end airattack
  ld    ix,P1LeftHardJumpPunchStraightUp
  jp    z,StandardAttackActionJumping
  ld    ix,P2LeftHardJumpPunchStraightUp
  ld    iy,Player2Frame
  jp    StandardAttackActionJumping

JumpSoftKick:
  call  HandleJump            ;first handle the jump normally
  call  CheckJumpStraight     ;out zero=Jump Straight up, changes: af
  jp    z,.StraightUpSoftKick

  .DiagonalSoftKick:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af
  ld    iy,Player1Frame
  jp    c,.LeftSoftJumpKickDiagonalUp
  .RightSoftJumpKickDiagonalUp:;now write airattack frame, and check end airattack
  ld    ix,P1RightSoftJumpKickDiagonalUp
  jp    z,StandardAttackActionJumping
  ld    ix,P2RightSoftJumpKickDiagonalUp
  ld    iy,Player2Frame
  jp    StandardAttackActionJumping
  .LeftSoftJumpKickDiagonalUp:;now write airattack frame, and check end airattack
  ld    ix,P1LeftSoftJumpKickDiagonalUp
  jp    z,StandardAttackActionJumping
  ld    ix,P2LeftSoftJumpKickDiagonalUp
  ld    iy,Player2Frame
  jp    StandardAttackActionJumping

  .StraightUpSoftKick:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af
  ld    iy,Player1Frame
  jp    c,.LeftSoftJumpKickStraightUp
  .RightSoftJumpKickStraightUp:;now write airattack frame, and check end airattack
  ld    ix,P1RightSoftJumpKickStraightUp
  jp    z,StandardAttackActionJumping
  ld    ix,P2RightSoftJumpKickStraightUp
  ld    iy,Player2Frame
  jp    StandardAttackActionJumping
  .LeftSoftJumpKickStraightUp:;now write airattack frame, and check end airattack
  ld    ix,P1LeftSoftJumpKickStraightUp
  jp    z,StandardAttackActionJumping
  ld    ix,P2LeftSoftJumpKickStraightUp
  ld    iy,Player2Frame
  jp    StandardAttackActionJumping

JumpHardKick:
  call  HandleJump            ;first handle the jump normally
  call  CheckJumpStraight     ;out zero=Jump Straight up, changes: af
  jp    z,.StraightUpHardKick

  .DiagonalHardKick:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af
  ld    iy,Player1Frame
  jp    c,.LeftHardJumpKickDiagonalUp
  .RightHardJumpKickDiagonalUp:;now write airattack frame, and check end airattack
  ld    ix,P1RightHardJumpKickDiagonalUp
  jp    z,StandardAttackActionJumping
  ld    ix,P2RightHardJumpKickDiagonalUp
  ld    iy,Player2Frame
  jp    StandardAttackActionJumping
  .LeftHardJumpKickDiagonalUp:;now write airattack frame, and check end airattack
  ld    ix,P1LeftHardJumpKickDiagonalUp
  jp    z,StandardAttackActionJumping
  ld    ix,P2LeftHardJumpKickDiagonalUp
  ld    iy,Player2Frame
  jp    StandardAttackActionJumping

  .StraightUpHardKick:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af
  ld    iy,Player1Frame
  jp    c,.LeftHardJumpKickStraightUp
  .RightHardJumpKickStraightUp:;now write airattack frame, and check end airattack
  ld    ix,P1RightHardJumpKickStraightUp
  jp    z,StandardAttackActionJumping
  ld    ix,P2RightHardJumpKickStraightUp
  ld    iy,Player2Frame
  jp    StandardAttackActionJumping
  .LeftHardJumpKickStraightUp:;now write airattack frame, and check end airattack
  ld    ix,P1LeftHardJumpKickStraightUp
  jp    z,StandardAttackActionJumping
  ld    ix,P2LeftHardJumpKickStraightUp
  ld    iy,Player2Frame
  jp    StandardAttackActionJumping
      
BendDefend:
  call  GetNewPrControls      ;out a-> NewPrcontrols, Changes: af
  bit   6,a
  jp    nz,SetSoftSitPunch
  bit   4,a
  jp    nz,SetHardSitPunch
  bit   7,a
  jp    nz,SetSoftSitKick
  bit   5,a
  jp    nz,SetHardSitKick

  ;
  ; bit	7	  6	  5		    4		    3		    2		  1		  0
  ;		  0	  0	  trig-b	trig-a	right	  left	down	up	(joystick)
  ;		  F5	F1	'M'		  space	  right	  left	down	up	(keyboard)
  ;
  call  CheckControlsinBC     ;out a->controls, b=x move, c=y move, changes: af, bc

  dec   c                     ;down pressed ?
  jp    z,.DownPressed
  inc   c                     ;up pressed ?
  jp    nz,.uppressed
  jp    SetStandDefend

  .DownPressed:
  dec   b                     ;right pressed ?
  jp    z,.RightPressed
  inc   b                     ;not right nor left pressed ?
  jp    z,SetBend

  .LeftPressed:
  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  ld    de,P1RightBendDefendFrame
  ld    hl,Player1Frame
  jp    z,.PlayerFound1
  ld    de,P2RightBendDefendFrame
  ld    hl,Player2Frame
  .PlayerFound1:

  jp    c,SetBend             ;if LeftPressed, and player looks left, then dont defend
  call  CheckEnemyAttacking   ;out-> c:enemy is attacking, changes: af
  jp    nc,SetBend
  jp    QuickSetFrame         ;in: de->frame, hl->Player12Frame, changes: a,de,hl

  .RightPressed:
  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  ld    de,P1LeftBendDefendFrame
  ld    hl,Player1Frame
  jp    z,.PlayerFound2
  ld    de,P2LeftBendDefendFrame
  ld    hl,Player2Frame
  .PlayerFound2:
  
  jp    nc,SetBend            ;if RightPressed, and player looks right, then dont defend
  call  CheckEnemyAttacking   ;out-> c:enemy is attacking, changes: af
  jp    nc,SetBend
  jp    QuickSetFrame         ;in: de->frame, hl->Player12Frame, changes: a,de,hl

  .uppressed:
  dec   b                     ;right pressed ?
  jp    z,SetjumpRight
  inc   b                     ;not right nor left pressed ?
  jp    z,SetjumpStraightUp   ;if left NOR right is pressed, then jump   
  jp    SetjumpLeft

StandDefend:
  call  GetNewPrControls      ;out a-> NewPrcontrols, Changes: af
  bit   6,a
  jp    nz,SetSoftStandPunch
  bit   4,a
  jp    nz,SetHardStandPunch
  bit   7,a
  jp    nz,SetSoftStandKick
  bit   5,a
  jp    nz,SetHardStandKick
  
  ;
  ; bit	7	  6	  5		    4		    3		    2		  1		  0
  ;		  0	  0	  trig-b	trig-a	right	  left	down	up	(joystick)
  ;		  F5	F1	'M'		  space	  right	  left	down	up	(keyboard)
  ;
  call  CheckControlsinBC     ;out a->controls, b=x move, c=y move, changes: af, bc
  dec   c                     ;down pressed ?
  jp    z,SetBendDefend
  inc   c                     ;up pressed ?
  jp    nz,.uppressed

  dec   b                     ;right pressed ?
  jp    z,.RightPressed
  inc   b                     ;not right nor left pressed ?
  jp    z,SetIdle

  .LeftPressed:
  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  ld    de,P1RightStandDefendFrame
  ld    hl,Player1Frame
  jp    z,.PlayerFound1
  ld    de,P2RightStandDefendFrame
  ld    hl,Player2Frame
  .PlayerFound1:

  jp    c,SetWalkLeft         ;if LeftPressed, and player looks left, then dont defend
  call  CheckEnemyAttacking   ;out-> c:enemy is attacking, changes: af
  jp    nc,SetWalkLeft
  jp    QuickSetFrame         ;in: de->frame, hl->Player12Frame, changes: a,de,hl

  .RightPressed:
  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  ld    de,P1LeftStandDefendFrame
  ld    hl,Player1Frame
  jp    z,.PlayerFound2
  ld    de,P2LeftStandDefendFrame
  ld    hl,Player2Frame
  .PlayerFound2:

  jp    nc,SetWalkRight       ;if RightPressed, and player looks right, then dont defend
  call  CheckEnemyAttacking   ;out-> c:enemy is attacking, changes: af
  jp    nc,SetWalkRight
  jp    QuickSetFrame         ;in: de->frame, hl->Player12Frame, changes: a,de,hl

  .uppressed:
  dec   b                     ;right pressed ?
  jp    z,SetjumpRight
  inc   b                     ;not right nor left pressed ?
  jp    z,SetjumpStraightUp   ;if left NOR right is pressed, then jump   
  jp    SetjumpLeft

StandDefendHit:
BendDefendHit:
StandHit:
BendHit:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af
  jp    z,.Rplayer1
  .Rplayer2:
  ld    hl,P2HitAnimationStep
  ld    iy,player1x
  ld    de,player2x
  ld    ix,player2Action
  jp    c,.lookleft
  jp    .lookright
  .Rplayer1:
  ld    hl,P1HitAnimationStep
  ld    iy,player2x
  ld    de,player1x
  ld    ix,player1Action
  jp    c,.lookleft
  jp    .lookright

  .lookright:
  inc   (hl)                  ;increase animation step
  ld    c,(hl)
  ld    b,0
  
  ld    hl,HitAnimationTable-1
  add   hl,bc                 ;go to movement
  
  ld    a,(hl)                ;x movement
  cp    128
  jp    z,.endhit
  neg
  jp    p,.moveright
  jp    .moveleft

  .lookleft:
  inc   (hl)                  ;increase animation step
  ld    c,(hl)
  ld    b,0

  ld    hl,HitAnimationTable-1  ;
  add   hl,bc                 ;go to movement
  
  ld    a,(hl)                ;x movement
  cp    128
  jp    z,.endhit
  jr    c,.moveright

  .moveleft:
  neg
  ld    b,a                   ;store movement
  ld    a,(de)                ;playerx
  sub   a,b                   ;add movement
  cp    ScreenLimitxLeft
  jr    c,.borderreachedleft
  ld    (de),a
  ret

  .moveright:
  ld    b,a                   ;store movement
  ld    a,(de)                ;playerx
  add   a,b                   ;add movement
  cp    ScreenLimitxRight
  jr    nc,.borderreachedright
  ld    (de),a
  ret

  .borderreachedleft:         ;if a border is reached while being hit, then move opponent backwards instead
  ld    a,(iy)                ;opponent's x
  add   a,b
  cp    MoveBackXBorder       ;move opponent to the right, even when he used a special,
  ret   nc                    ;but not if opponent's x>90, otherwise he would move to the right
  ld    (iy),a                ;at any impact of a projectile
  ret

  .borderreachedright:        ;if a border is reached while being hit, then move opponent backwards instead
  ld    a,(iy)                ;opponent's x
  sub   a,b
  cp    256-MoveBackXBorder   ;move opponent to the left, even when he used a special,
  ret   c                     ;but not if opponent's x<170, otherwise he would move to the left
  ld    (iy),a                ;at any impact of a projectile
  ret  

  .endhit:                    ;being hit routine ends, now if you were defending, you  have a chance to keep defening
  ld    a,(ix)                ;player action
  cp    18                    ;check for Action18 (StandHit) or Action19 (BendHit)
  jp    z,SetIdle             ;player was hit, now go to Idle, and you need 1 frame to defend
  cp    19                    ;check for Action18 (StandHit) or Action19 (BendHit)
  jp    z,SetBend             ;player was hit, now go to Idle, and you need 1 frame to defend
  ;at this point you were succesfully defending before you got hit, so continue defending, don't go to Idle
  cp    23                    ;Action23 (StandDefendHit)
  jp    z,.SetStandDefend
  cp    24                    ;Action23 (BendDefendHit)
  jp    z,.SetBendDefend
  ret
  .SetStandDefend:
  ld    b,16                  ;Action=16	StandDefend
  jp    WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af
  .SetBendDefend:
  ld    b,17                  ;Action=17	BendDefend
  jp    WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af

Jumphit:
  call  JumpHitAnimation
  call  JumpHitMovement
  ret   c
  ld    c,sfxLanding
  call  SetSfx
  jp    SetIdle

  JumpHitAnimation:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af, b
  ld    iy,Player1Frame
  jp    c,.Left

  .Right:
  ld    ix,P1JumpHitRightFrame
  jp    z,AnimatePlayer
  ld    ix,P2JumpHitRightFrame
  ld    iy,Player2Frame
  jp    AnimatePlayer
  .Left:
  ld    ix,P1JumpHitLeftFrame
  jp    z,AnimatePlayer
  ld    ix,P2JumpHitLeftFrame
  ld    iy,Player2Frame
  jp    AnimatePlayer

  JumpHitMovement:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af
  jp    z,.Rplayer1
  .Rplayer2:
  ld    hl,P2JumpHitAnimationStep
  ld    bc,HeavilyHitAnimationTable-2
  ld    iy,player1x
  ld    de,player2y
  jp    c,GeneralMovement.lookleft
  jp    GeneralMovement.lookright
  .Rplayer1:
  ld    hl,P1JumpHitAnimationStep
  ld    bc,HeavilyHitAnimationTable-2
  ld    iy,player2x
  ld    de,player1y
  jp    c,GeneralMovement.lookleft
  jp    GeneralMovement.lookright

HeavilyHit:
  call  HeavilyHitAnimation
  call  HeavilyHitMovement    ;out: nc=ground found
  ret   c
  ld    c,sfxKnockdown
  call  SetSfx
  jp    SetKnockDownRecover

  HeavilyHitAnimation:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af, b
  ld    iy,Player1Frame
  jp    c,.Left

  .Right:
  ld    ix,P1HeavilyHitRightFrame
  jp    z,AnimatePlayer
  ld    ix,P2HeavilyHitRightFrame
  ld    iy,Player2Frame
  jp    AnimatePlayer
  .Left:
  ld    ix,P1HeavilyHitLeftFrame
  jp    z,AnimatePlayer
  ld    ix,P2HeavilyHitLeftFrame
  ld    iy,Player2Frame
  jp    AnimatePlayer
  
  HeavilyHitMovement:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af
  jp    z,.Rplayer1
  .Rplayer2:
  ld    hl,P2HeavilyHitAnimationStep
  ld    bc,HeavilyHitAnimationTable-2
  ld    iy,player1x
  ld    de,player2y
  jp    c,GeneralMovement.lookleft
  jp    GeneralMovement.lookright
  .Rplayer1:
  ld    hl,P1HeavilyHitAnimationStep
  ld    bc,HeavilyHitAnimationTable-2
  ld    iy,player2x
  ld    de,player1y
  jp    c,GeneralMovement.lookleft
  jp    GeneralMovement.lookright

KnockDownRecover:
  call  StaDirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  ld    iy,Player1Frame
  jp    c,.Left

  .Right:
  ld    a,(P1Dies?)
  ld    de,P1DiedRightFrame
  ld    ix,P1KnockDownRecoverRightFrame
  jp    z,StandardKnockDownRecoverAction
  ld    a,(P2Dies?)
  ld    de,P2DiedRightFrame
  ld    ix,P2KnockDownRecoverRightFrame
  ld    iy,Player2Frame
  jp    StandardKnockDownRecoverAction

  .Left:
  ld    a,(P1Dies?)
  ld    de,P1DiedLeftFrame
  ld    ix,P1KnockDownRecoverLeftFrame
  jp    z,StandardKnockDownRecoverAction
  ld    a,(P2Dies?)
  ld    de,P2DiedLeftFrame
  ld    ix,P2KnockDownRecoverLeftFrame
  ld    iy,Player2Frame
  jp    StandardKnockDownRecoverAction

Special1:
  call  AnimateSpecial1  
  call  Special1Movement      ;out: nc=ground found

  ld    a,(HandleWhichPlayer?)
  dec   a
  ld    hl,P1Special1MovementTablePointer
  ld    de,P1VariableTableSpecial1+2
  jp    z,HandleSpecial
  ld    hl,P2Special1MovementTablePointer
  ld    de,P2VariableTableSpecial1+2
  jp    HandleSpecial
  
Special2:
  call  AnimateSpecial2
  call  Special2Movement      ;out: nc=ground found
  
  ld    a,(HandleWhichPlayer?)
  dec   a
  ld    hl,P1Special2MovementTablePointer
  ld    de,P1VariableTableSpecial2+2
  jp    z,HandleSpecial
  ld    hl,P2Special2MovementTablePointer
  ld    de,P2VariableTableSpecial2+2  
  jp    HandleSpecial

Special3:
  call  AnimateSpecial3
  call  Special3Movement      ;out: nc=ground found
  
  ld    a,(HandleWhichPlayer?)
  dec   a
  ld    hl,P1Special3MovementTablePointer
  ld    de,P1VariableTableSpecial3+2
  jp    z,HandleSpecial
  ld    hl,P2Special3MovementTablePointer
  ld    de,P2VariableTableSpecial3+2
  jp    HandleSpecial

Special4:
  call  AnimateSpecial4
  call  Special4Movement      ;out: nc=ground found
  
  ld    a,(HandleWhichPlayer?)
  dec   a
  ld    hl,P1Special4MovementTablePointer
  ld    de,P1VariableTableSpecial4+2
  jp    z,HandleSpecial
  ld    hl,P2Special4MovementTablePointer
  ld    de,P2VariableTableSpecial4+2
  jp    HandleSpecial

Special5:
  call  AnimateSpecial5
  call  Special5Movement      ;out: nc=ground found
  
  ld    a,(HandleWhichPlayer?)
  dec   a
  ld    hl,P1Special5MovementTablePointer
  ld    de,P1VariableTableSpecial5+2
  jp    z,HandleSpecial
  ld    hl,P2Special5MovementTablePointer
  ld    de,P2VariableTableSpecial5+2
  jp    HandleSpecial

Toss:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af, b
  ld    iy,Player1Frame
  jp    c,.Left

  .Right:
  ld    ix,P1TossRightFrame
  jp    z,StandardTossAction
  ld    ix,P2TossRightFrame
  ld    iy,Player2Frame
  jp    StandardTossAction

  .Left:
  ld    ix,P1TossLeftFrame
  jp    z,StandardTossAction
  ld    ix,P2TossLeftFrame
  ld    iy,Player2Frame
  jp    StandardTossAction

GettingTossed:
  call  GettingTossedDrainHp
  call  GettingTossedAnimation
  call  GettingTossedMovement ;out: nc=ground found
  ret   c
  ld    c,sfxKnockdown
  call  SetSfx
  jp    SetKnockDownRecover

  GettingTossedAnimation:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af, b
  ld    iy,Player1Frame
  jp    c,.Left

  .Right:
  ld    ix,P1GettingTossedRightFrame
  jp    z,AnimatePlayer
  ld    ix,P2GettingTossedRightFrame
  ld    iy,Player2Frame
  jp    AnimatePlayer
  .Left:
  ld    ix,P1GettingTossedLeftFrame
  jp    z,AnimatePlayer
  ld    ix,P2GettingTossedLeftFrame
  ld    iy,Player2Frame
  jp    AnimatePlayer
  
  GettingTossedMovement:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af
  jp    z,.Rplayer1
  .Rplayer2:
  ld    hl,P2GettingTossedAnimationStep
  ld    bc,GettingTossedAnimationTable-2
  ld    iy,player1x
  ld    de,player2y
  jp    nc,GeneralMovement.lookleft
  jp    GeneralMovement.lookright
  .Rplayer1:
  ld    hl,P1GettingTossedAnimationStep
  ld    bc,GettingTossedAnimationTable-2
  ld    iy,player2x
  ld    de,player1y
  jp    nc,GeneralMovement.lookleft
  jp    GeneralMovement.lookright

  GettingTossedDrainHp:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af
  ld    a,(P1GettingTossedAnimationStep)
  jp    z,.PlayerFound
  ld    a,(P2GettingTossedAnimationStep)
  .PlayerFound:
  cp    20
  jp    z,DrainHp             ;out: c=player died, changes af,bc,de,hl,ix
  ret
  
Victory:
  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  ld    iy,Player1Frame
  jp    c,.Left

  .Right:
  ld    ix,P1VictoryRightFrame
  jp    z,AnimatePlayer
  ld    ix,P2VictoryRightFrame
  ld    iy,Player2Frame
  jp    AnimatePlayer

  .Left:
  ld    ix,P1VictoryLeftFrame
  jp    z,AnimatePlayer
  ld    ix,P2VictoryLeftFrame
  ld    iy,Player2Frame
  jp    AnimatePlayer

  
  
  


































































































































SetIdle:
  call  SetIdlePriority       ;which player comes in the front, and which comes in the back ?
  call  TotalResetAirAttack

  ld    b,0                   ;Action=0	Idle
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af
  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  jr    c,.SetLeftIdle

  .SetRightIdle:
  ld    a,0                   ;start with position 0
  ld    bc,P1RightIdleFrame
  ld    de,P1RightIdleFrame+5
  ld    hl,Player1Frame
  jp    z,SetFrame
  ld    bc,P2RightIdleFrame
  ld    de,P2RightIdleFrame+5
  ld    hl,Player2Frame
  jp    SetFrame

  .SetLeftIdle:
  ld    a,0                   ;start with position 0
  ld    bc,P1LeftIdleFrame
  ld    de,P1LeftIdleFrame+5
  ld    hl,Player1Frame
  jp    z,SetFrame
  ld    bc,P2LeftIdleFrame
  ld    de,P2LeftIdleFrame+5
  ld    hl,Player2Frame
  jp    SetFrame
  
SetBend:
  call  SetBendPriority       ;which player comes in the front, and which comes in the back ?
  ld    b,1                   ;Action=1	Bend
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af
  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  jp    c,.SetLeftBend

  .SetRightBend:
  ld    de,P1RightBendFrame
  ld    hl,Player1Frame
  jp    z,QuickSetFrame       ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    de,P2RightBendFrame
  ld    hl,Player2Frame
  jp    QuickSetFrame         ;in: de->frame, hl->Player12Frame, changes: a,de,hl

  .SetLeftBend:
  ld    de,P1LeftBendFrame
  ld    hl,Player1Frame
  jp    z,QuickSetFrame       ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    de,P2LeftBendFrame
  ld    hl,Player2Frame
  jp    QuickSetFrame         ;in: de->frame, hl->Player12Frame, changes: a,de,hl

SetWalkLeft:
  ld    b,2                   ;Action=2	Walk
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af

  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  jp    c,.SetFaceLeftWalkLeft

  .SetFaceRightWalkLeft:
  ld    a,(P1RightWalkLeftFrame+29)
  ld    bc,P1RightWalkLeftFrame
  ld    de,P1RightWalkLeftFrame+30
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: a=starting framenumber, de=starting frame
  ld    a,(P2RightWalkLeftFrame+29)
  ld    bc,P2RightWalkLeftFrame
  ld    de,P2RightWalkLeftFrame+30
  ld    hl,Player2Frame
  jp    SetFrame              ;in: a=starting framenumber, de=starting frame
  
  .SetFaceLeftWalkLeft:
  ld    a,(P1LeftWalkLeftFrame+29)
  ld    bc,P1LeftWalkLeftFrame
  ld    de,P1LeftWalkLeftFrame+30
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: a=starting framenumber, de=starting frame
  ld    a,(P2LeftWalkLeftFrame+29)
  ld    bc,P2LeftWalkLeftFrame
  ld    de,P2LeftWalkLeftFrame+30
  ld    hl,Player2Frame
  jp    SetFrame              ;in: a=starting framenumber, de=starting frame

SetWalkRight:
  ld    b,2                   ;Action=2	Walk
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af

  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  jp    c,.SetFaceLeftWalkRight

  .SetFaceRightWalkRight:
  ld    a,(P1RightWalkRightFrame+29)
  ld    bc,P1RightWalkRightFrame
  ld    de,P1RightWalkRightFrame+30
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: a=starting framenumber, de=starting frame
  ld    a,(P2RightWalkRightFrame+29)
  ld    bc,P2RightWalkRightFrame
  ld    de,P2RightWalkRightFrame+30
  ld    hl,Player2Frame
  jp    SetFrame              ;in: a=starting framenumber, de=starting frame

  .SetFaceLeftWalkRight:
  ld    a,(P1LeftWalkRightFrame+29)
  ld    bc,P1LeftWalkRightFrame
  ld    de,P1LeftWalkRightFrame+30
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: a=starting framenumber, de=starting frame
  ld    a,(P2LeftWalkRightFrame+29)
  ld    bc,P2LeftWalkRightFrame
  ld    de,P2LeftWalkRightFrame+30
  ld    hl,Player2Frame
  jp    SetFrame              ;in: a=starting framenumber, de=starting frame

SetJumpDirection:
  ld    a,(HandleWhichPlayer?)
  dec   a
  ld    hl,Player1JumpDirection
  jp    z,.setJumpDirection
  ld    hl,Player2JumpDirection
  .setJumpDirection:
  ld    (hl),b                ;set jump direction
  ret

SetjumpStraightUp:  
  call  RaisePriority         ;current player goes to the front
  ld    b,+0                  ;0= player jumps straight up, 1=jumps right, -1=jumps left
  call  SetJumpDirection
  ld    b,3                   ;Action=3	Jump
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af
  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  jp    c,.SetFaceLeftJumpStraight

  .SetFaceRightJumpStraight:
  ld    a,+1                  ;P1 looking right
  ld    ix,P1StaticDirection
  ld    bc,P1jumptable
  ld    de,P1RightJumpStraightStartframe
  ld    hl,Player1Frame
  jp    z,SetJumpFrame       ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    ix,P2StaticDirection
  ld    bc,P2jumptable
  ld    de,P2RightJumpStraightStartframe
  ld    hl,Player2Frame
  jp    SetJumpFrame         ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  .SetFaceLeftJumpStraight:
  ld    a,-1                  ;P1 looking left
  ld    ix,P1StaticDirection
  ld    bc,P1jumptable
  ld    de,P1LeftJumpStraightStartframe
  ld    hl,Player1Frame
  jp    z,SetJumpFrame       ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    ix,P2StaticDirection
  ld    bc,P2jumptable
  ld    de,P2LeftJumpStraightStartframe
  ld    hl,Player2Frame
  jp    SetJumpFrame         ;in: de->frame, hl->Player12Frame, changes: a,de,hl

SetjumpLeft:  
  call  RaisePriority         ;current player goes to the front
  ld    b,-1                  ;0= player jumps straight up, 1=jumps right, -1=jumps left
  call  SetJumpDirection
  ld    b,3                   ;Action=3	Jump
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af
  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  jp    c,.SetFaceLeftJumpLeft

  .SetFaceRightJumpLeft:
  ld    a,+1                  ;P1 looking right
  ld    ix,P1StaticDirection
  ld    bc,P1jumptable
  ld    de,P1RightJumpLeftStartframe
  ld    hl,Player1Frame
  jp    z,SetJumpFrame       ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    ix,P2StaticDirection
  ld    bc,P2jumptable
  ld    de,P2RightJumpLeftStartframe
  ld    hl,Player2Frame
  jp    SetJumpFrame         ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  .SetFaceLeftJumpLeft:
  ld    a,-1                  ;P1 looking left
  ld    ix,P1StaticDirection
  ld    bc,P1jumptable
  ld    de,P1LeftJumpLeftStartframe
  ld    hl,Player1Frame
  jp    z,SetJumpFrame       ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    ix,P2StaticDirection
  ld    bc,P2jumptable
  ld    de,P2LeftJumpLeftStartframe
  ld    hl,Player2Frame
  jp    SetJumpFrame         ;in: de->frame, hl->Player12Frame, changes: a,de,hl

SetjumpRight:  
  call  RaisePriority         ;current player goes to the front
  ld    b,+1                  ;0= player jumps Straight up, 1=jumps right, -1=jumps left
  call  SetJumpDirection
  ld    b,3                   ;Action=3	Jump
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af
  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  jp    c,.SetFaceLeftJumpRight

  .SetFaceRightJumpRight:
  ld    a,+1                  ;P1 looking right
  ld    ix,P1StaticDirection
  ld    bc,P1jumptable
  ld    de,P1RightJumpRightStartframe
  ld    hl,Player1Frame
  jp    z,SetJumpFrame       ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    ix,P2StaticDirection
  ld    bc,P2jumptable
  ld    de,P2RightJumpRightStartframe
  ld    hl,Player2Frame
  jp    SetJumpFrame         ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  .SetFaceLeftJumpRight:
  ld    a,-1                  ;P1 looking left
  ld    ix,P1StaticDirection
  ld    bc,P1jumptable
  ld    de,P1LeftJumpRightStartframe
  ld    hl,Player1Frame
  jp    z,SetJumpFrame       ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    ix,P2StaticDirection
  ld    bc,P2jumptable
  ld    de,P2LeftJumpRightStartframe
  ld    hl,Player2Frame
  jp    SetJumpFrame         ;in: de->frame, hl->Player12Frame, changes: a,de,hl

SetSoftStandPunch:
  ld    c,sfxSoftAttack
  call  SetSfx

  call  RaisePriority         ;current player goes to the front
  ld    b,4                   ;Action=4	SoftStandPunch
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af
  call  SetStaticDirection    ;out: c=look left, z=player 1, changes af, b, hl
;  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  ld    a,0                   ;reset animation pointer
  jp    c,.SetLeftSoftStandPunch

  .SetRightSoftStandPunch:
  ld    bc,P1StandSoftPunchRightFrame
  ld    de,P1StandSoftPunchRightFrame+5
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2StandSoftPunchRightFrame
  ld    de,P2StandSoftPunchRightFrame+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  .SetLeftSoftStandPunch:
  ld    bc,P1StandSoftPunchLeftFrame
  ld    de,P1StandSoftPunchLeftFrame+5
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2StandSoftPunchLeftFrame
  ld    de,P2StandSoftPunchLeftFrame+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl
 
SetHardStandPunch:
  call  checkPlayersNear      ;out: c=players are near to each other, able to toss
  jp    nc,.NoToss

  call  CheckControlsinBC     ;out a->controls, b=x move, c=y move, changes: af, bc
  dec   b                     ;right pressed ?
  jp    z,.checkTossRight
  inc   b                     ;not right nor left pressed ?
  jp    z,.NoToss
  
  .checkTossLeft:
  call  CheckPossibleToss     ;out:  c=toss allowed
  jp    c,SetTossLeft
  jp    .NoToss

  .checkTossRight:
  call  CheckPossibleToss     ;out:  c=toss allowed
  jp    c,SetTossRight
  jp    .NoToss

  .NoToss:
  ld    c,sfxHardAttack
  call  SetSfx
  call  RaisePriority         ;current player goes to the front
  ld    b,5                   ;Action=5	SoftStandPunch
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af
  call  SetStaticDirection    ;out: c=look left, z=player 1, changes af, b, hl
;  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  ld    a,0                   ;reset animation pointer
  jp    c,.SetLeftHardStandPunch

  .SetRightHardStandPunch:
  ld    bc,P1StandHardPunchRightFrame
  ld    de,P1StandHardPunchRightFrame+5
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2StandHardPunchRightFrame
  ld    de,P2StandHardPunchRightFrame+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  .SetLeftHardStandPunch:
  ld    bc,P1StandHardPunchLeftFrame
  ld    de,P1StandHardPunchLeftFrame+5
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2StandHardPunchLeftFrame
  ld    de,P2StandHardPunchLeftFrame+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl

SetSoftStandKick:
  ld    c,sfxSoftAttack
  call  SetSfx

  call  RaisePriority         ;current player goes to the front
  ld    b,6                   ;Action=6	SoftStandKick
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af
  call  SetStaticDirection    ;out: c=look left, z=player 1, changes af, b, hl
;  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  ld    a,0                   ;reset animation pointer
  jp    c,.SetLeftSoftStandKick

  .SetRightSoftStandKick:
  ld    bc,P1StandSoftKickRightFrame
  ld    de,P1StandSoftKickRightFrame+5
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2StandSoftKickRightFrame
  ld    de,P2StandSoftKickRightFrame+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  .SetLeftSoftStandKick:
  ld    bc,P1StandSoftKickLeftFrame
  ld    de,P1StandSoftKickLeftFrame+5
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2StandSoftKickLeftFrame
  ld    de,P2StandSoftKickLeftFrame+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl

SetHardStandKick:
  ld    c,sfxHardAttack
  call  SetSfx
  
  call  RaisePriority         ;current player goes to the front
  ld    b,7                   ;Action=7	HardStandKick
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af
  call  SetStaticDirection    ;out: c=look left, z=player 1, changes af, b, hl
;  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  ld    a,0                   ;reset animation pointer
  jp    c,.SetLeftHardStandKick

  .SetRightHardStandKick:
  ld    bc,P1StandHardKickRightFrame
  ld    de,P1StandHardKickRightFrame+5
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2StandHardKickRightFrame
  ld    de,P2StandHardKickRightFrame+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  .SetLeftHardStandKick:
  ld    bc,P1StandHardKickLeftFrame
  ld    de,P1StandHardKickLeftFrame+5
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2StandHardKickLeftFrame
  ld    de,P2StandHardKickLeftFrame+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl

SetSoftSitPunch:
  ld    c,sfxSoftAttack
  call  SetSfx

  call  RaisePriority         ;current player goes to the front
  ld    b,8                   ;Action=8	SoftSitPunch
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af
  call  SetStaticDirection    ;out: c=look left, z=player 1, changes af, b, hl
;  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  ld    a,0                   ;reset animation pointer
  jp    c,.SetLeftSoftSitPunch

  .SetRightSoftSitPunch:
  ld    bc,P1SitSoftPunchRightFrame
  ld    de,P1SitSoftPunchRightFrame+5
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2SitSoftPunchRightFrame
  ld    de,P2SitSoftPunchRightFrame+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  .SetLeftSoftSitPunch:
  ld    bc,P1SitSoftPunchLeftFrame
  ld    de,P1SitSoftPunchLeftFrame+5
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2SitSoftPunchLeftFrame
  ld    de,P2SitSoftPunchLeftFrame+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl

SetHardSitPunch:
  ld    c,sfxHardAttack
  call  SetSfx
  
  call  RaisePriority         ;current player goes to the front
  ld    b,9                   ;Action=9	SoftSitPunch
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af
  call  SetStaticDirection    ;out: c=look left, z=player 1, changes af, b, hl
;  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  ld    a,0                   ;reset animation pointer
  jp    c,.SetLeftHardSitPunch

  .SetRightHardSitPunch:
  ld    bc,P1SitHardPunchRightFrame
  ld    de,P1SitHardPunchRightFrame+5
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2SitHardPunchRightFrame
  ld    de,P2SitHardPunchRightFrame+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  .SetLeftHardSitPunch:
  ld    bc,P1SitHardPunchLeftFrame
  ld    de,P1SitHardPunchLeftFrame+5
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2SitHardPunchLeftFrame
  ld    de,P2SitHardPunchLeftFrame+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl

SetSoftSitKick:
  ld    c,sfxSoftAttack
  call  SetSfx

  call  RaisePriority         ;current player goes to the front
  ld    b,10                  ;Action=10	SoftSitKick
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af
  call  SetStaticDirection    ;out: c=look left, z=player 1, changes af, b, hl
;  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  ld    a,0                   ;reset animation pointer
  jp    c,.SetLeftSoftSitKick

  .SetRightSoftSitKick:
  ld    bc,P1SitSoftKickRightFrame
  ld    de,P1SitSoftKickRightFrame+5
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2SitSoftKickRightFrame
  ld    de,P2SitSoftKickRightFrame+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  .SetLeftSoftSitKick:
  ld    bc,P1SitSoftKickLeftFrame
  ld    de,P1SitSoftKickLeftFrame+5
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2SitSoftKickLeftFrame
  ld    de,P2SitSoftKickLeftFrame+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl

SetHardSitKick:
  ld    c,sfxHardAttack
  call  SetSfx

  call  RaisePriority         ;current player goes to the front
  ld    b,11                  ;Action=11	HardSitKick
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af
  call  SetStaticDirection    ;out: c=look left, z=player 1, changes af, b, hl
;  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  ld    a,0                   ;reset animation pointer
  jp    c,.SetLeftHardSitKick

  .SetRightHardSitKick:
  ld    bc,P1SitHardKickRightFrame
  ld    de,P1SitHardKickRightFrame+5
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2SitHardKickRightFrame
  ld    de,P2SitHardKickRightFrame+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  .SetLeftHardSitKick:
  ld    bc,P1SitHardKickLeftFrame
  ld    de,P1SitHardKickLeftFrame+5
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2SitHardKickLeftFrame
  ld    de,P2SitHardKickLeftFrame+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl

SetSoftJumpPunch:            ;while jumping SoftPunch is pressed
  call  RaisePriority         ;current player goes to the front
  ld    (hl),1                ;set already attacked once this jump?
  dec   hl
  ld    (hl),1                ;Currently attacking in air?

  ld    c,sfxSoftAttack
  call  SetSfx

  ld    b,12                  ;Action=12	JumpSoftPunch
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af

  call  CheckJumpStraight     ;out zero=Jump Straight up, changes: af
  jp    z,.StraightUpSoftpunch

  .DiagonalSoftpunch:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af
  ld    a,0                   ;reset animation pointer
  jp    c,.SetLeftSoftJumppunchDiagonalUp
  .SetRightSoftJumppunchDiagonalUp:
  ld    bc,P1RightSoftJumppunchDiagonalUp
  ld    de,P1RightSoftJumppunchDiagonalUp+5
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2RightSoftJumppunchDiagonalUp
  ld    de,P2RightSoftJumppunchDiagonalUp+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  .SetLeftSoftJumppunchDiagonalUp:
  ld    bc,P1LeftSoftJumppunchDiagonalUp
  ld    de,P1LeftSoftJumppunchDiagonalUp+5
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2LeftSoftJumppunchDiagonalUp
  ld    de,P2LeftSoftJumppunchDiagonalUp+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl

  .StraightUpSoftpunch:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af
  ld    a,0                   ;reset animation pointer
  jp    c,.SetLeftSoftJumppunchStraightUp
  .SetRightSoftJumppunchStraightUp:
  ld    bc,P1RightSoftJumppunchStraightUp
  ld    de,P1RightSoftJumppunchStraightUp+5
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2RightSoftJumppunchStraightUp
  ld    de,P2RightSoftJumppunchStraightUp+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  .SetLeftSoftJumppunchStraightUp:
  ld    bc,P1LeftSoftJumppunchStraightUp
  ld    de,P1LeftSoftJumppunchStraightUp+5
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2LeftSoftJumppunchStraightUp
  ld    de,P2LeftSoftJumppunchStraightUp+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl

SetHardJumpPunch:            ;while jumping HardPunch is pressed
  call  RaisePriority         ;current player goes to the front
  ld    (hl),1                ;set already attacked once this jump?
  dec   hl
  ld    (hl),1                ;Currently attacking in air?

  ld    c,sfxHardAttack
  call  SetSfx
  
  ld    b,13                  ;Action=13	JumpHardPunch
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af

  call  CheckJumpStraight     ;out zero=Jump Straight up, changes: af
  jp    z,.StraightUpHardpunch

  .DiagonalHardpunch:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af
  ld    a,0                   ;reset animation pointer
  jp    c,.SetLeftHardJumppunchDiagonalUp
  .SetRightHardJumppunchDiagonalUp:
  ld    bc,P1RightHardJumppunchDiagonalUp
  ld    de,P1RightHardJumppunchDiagonalUp+5
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2RightHardJumppunchDiagonalUp
  ld    de,P2RightHardJumppunchDiagonalUp+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  .SetLeftHardJumppunchDiagonalUp:
  ld    bc,P1LeftHardJumppunchDiagonalUp
  ld    de,P1LeftHardJumppunchDiagonalUp+5
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2LeftHardJumppunchDiagonalUp
  ld    de,P2LeftHardJumppunchDiagonalUp+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl

  .StraightUpHardpunch:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af
  ld    a,0                   ;reset animation pointer
  jp    c,.SetLeftHardJumppunchStraightUp
  .SetRightHardJumppunchStraightUp:
  ld    bc,P1RightHardJumppunchStraightUp
  ld    de,P1RightHardJumppunchStraightUp+5
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2RightHardJumppunchStraightUp
  ld    de,P2RightHardJumppunchStraightUp+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  .SetLeftHardJumppunchStraightUp:
  ld    bc,P1LeftHardJumppunchStraightUp
  ld    de,P1LeftHardJumppunchStraightUp+5
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2LeftHardJumppunchStraightUp
  ld    de,P2LeftHardJumppunchStraightUp+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl

SetSoftJumpKick:             ;while jumping SoftKick is pressed
  call  RaisePriority         ;current player goes to the front
  ld    (hl),1                ;set already attacked once this jump?
  dec   hl
  ld    (hl),1                ;Currently attacking in air?

  ld    c,sfxSoftAttack
  call  SetSfx

  ld    b,14                  ;Action=14	JumpSoftKick
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af

  call  CheckJumpStraight     ;out zero=Jump Straight up, changes: af
  jp    z,.StraightUpSoftKick

  .DiagonalSoftKick:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af
  ld    a,0                   ;reset animation pointer
  jp    c,.SetLeftSoftJumpKickDiagonalUp
  .SetRightSoftJumpKickDiagonalUp:
  ld    bc,P1RightSoftJumpKickDiagonalUp
  ld    de,P1RightSoftJumpKickDiagonalUp+5
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2RightSoftJumpKickDiagonalUp
  ld    de,P2RightSoftJumpKickDiagonalUp+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  .SetLeftSoftJumpKickDiagonalUp:
  ld    bc,P1LeftSoftJumpKickDiagonalUp
  ld    de,P1LeftSoftJumpKickDiagonalUp+5
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2LeftSoftJumpKickDiagonalUp
  ld    de,P2LeftSoftJumpKickDiagonalUp+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl

  .StraightUpSoftKick:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af
  ld    a,0                   ;reset animation pointer
  jp    c,.SetLeftSoftJumpKickStraightUp
  .SetRightSoftJumpKickStraightUp:
  ld    bc,P1RightSoftJumpKickStraightUp
  ld    de,P1RightSoftJumpKickStraightUp+5
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2RightSoftJumpKickStraightUp
  ld    de,P2RightSoftJumpKickStraightUp+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  .SetLeftSoftJumpKickStraightUp:
  ld    bc,P1LeftSoftJumpKickStraightUp
  ld    de,P1LeftSoftJumpKickStraightUp+5
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2LeftSoftJumpKickStraightUp
  ld    de,P2LeftSoftJumpKickStraightUp+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl

SetHardJumpKick:             ;while jumping HardKick is pressed
  call  RaisePriority         ;current player goes to the front
  ld    (hl),1                ;set already attacked once this jump?
  dec   hl
  ld    (hl),1                ;Currently attacking in air?

  ld    c,sfxHardAttack
  call  SetSfx
  
  ld    b,15                  ;Action=15	JumpHardKick
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af

  call  CheckJumpStraight     ;out zero=Jump Straight up, changes: af
  jp    z,.StraightUpHardKick

  .DiagonalHardKick:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af
  ld    a,0                   ;reset animation pointer
  jp    c,.SetLeftHardJumpKickDiagonalUp
  .SetRightHardJumpKickDiagonalUp:
  ld    bc,P1RightHardJumpKickDiagonalUp
  ld    de,P1RightHardJumpKickDiagonalUp+5
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2RightHardJumpKickDiagonalUp
  ld    de,P2RightHardJumpKickDiagonalUp+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  .SetLeftHardJumpKickDiagonalUp:
  ld    bc,P1LeftHardJumpKickDiagonalUp
  ld    de,P1LeftHardJumpKickDiagonalUp+5
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2LeftHardJumpKickDiagonalUp
  ld    de,P2LeftHardJumpKickDiagonalUp+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl

  .StraightUpHardKick:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af
  ld    a,0                   ;reset animation pointer
  jp    c,.SetLeftHardJumpKickStraightUp
  .SetRightHardJumpKickStraightUp:
  ld    bc,P1RightHardJumpKickStraightUp
  ld    de,P1RightHardJumpKickStraightUp+5
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2RightHardJumpKickStraightUp
  ld    de,P2RightHardJumpKickStraightUp+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  .SetLeftHardJumpKickStraightUp:
  ld    bc,P1LeftHardJumpKickStraightUp
  ld    de,P1LeftHardJumpKickStraightUp+5
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2LeftHardJumpKickStraightUp
  ld    de,P2LeftHardJumpKickStraightUp+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl

SetStandDefend: 
  ld    b,16                  ;Action=16	StandDefend
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af

  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  jp    c,.SetLeftStandDefend

  .SetRightStandDefend:
  ld    de,P1RightIdleFrame+5
  ld    hl,Player1Frame
  jp    z,QuickSetFrame       ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    de,P2RightIdleFrame+5
  ld    hl,Player2Frame
  jp    QuickSetFrame         ;in: de->frame, hl->Player12Frame, changes: a,de,hl

  .SetLeftStandDefend:
  ld    de,P1LeftIdleFrame+5
  ld    hl,Player1Frame
  jp    z,QuickSetFrame       ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    de,P2LeftIdleFrame+5
  ld    hl,Player2Frame
  jp    QuickSetFrame         ;in: de->frame, hl->Player12Frame, changes: a,de,hl

SetBendDefend:
  ld    b,17                  ;Action=17	BendDefend
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af

  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  jp    c,.SetLeftBendDefend

  .SetRightBendDefend:
  ld    de,P1RightBendFrame
  ld    hl,Player1Frame
  jp    z,QuickSetFrame       ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    de,P2RightBendFrame
  ld    hl,Player2Frame
  jp    QuickSetFrame         ;in: de->frame, hl->Player12Frame, changes: a,de,hl

  .SetLeftBendDefend:
  ld    de,P1LeftBendFrame
  ld    hl,Player1Frame
  jp    z,QuickSetFrame       ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    de,P2LeftBendFrame
  ld    hl,Player2Frame
  jp    QuickSetFrame         ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  
SetStandHit:
  ld    b,18                  ;Action=18 StandHit
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af
  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  jp    c,.SetLeftStandHit

  .SetRightStandHit:
  ld    a,+1                  ;P1 looking right
  ld    ix,P1StaticDirection
  ld    bc,P1HitAnimationStep
  ld    de,P1RightStandHitFrame
  ld    hl,Player1Frame
  jp    z,SetHitFrame         ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    ix,P2StaticDirection
  ld    bc,P2HitAnimationStep
  ld    de,P2RightStandHitFrame
  ld    hl,Player2Frame
  jp    SetHitFrame           ;in: de->frame, hl->Player12Frame, changes: a,de,hl

  .SetLeftStandHit:
  ld    a,-1                  ;P1 looking left
  ld    ix,P1StaticDirection
  ld    bc,P1HitAnimationStep
  ld    de,P1LeftStandHitFrame
  ld    hl,Player1Frame
  jp    z,SetHitFrame         ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    ix,P2StaticDirection
  ld    bc,P2HitAnimationStep
  ld    de,P2LeftStandHitFrame
  ld    hl,Player2Frame
  jp    SetHitFrame           ;in: de->frame, hl->Player12Frame, changes: a,de,hl

SetBendHit:
  ld    b,19                  ;Action=19 BendHit
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af
  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  jp    c,.SetLeftBendHit

  .SetRightBendHit:
  ld    a,+1                  ;P1 looking right
  ld    ix,P1StaticDirection
  ld    bc,P1HitAnimationStep
  ld    de,P1RightBendHitFrame
  ld    hl,Player1Frame
  jp    z,SetHitFrame         ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    ix,P2StaticDirection
  ld    bc,P2HitAnimationStep
  ld    de,P2RightBendHitFrame
  ld    hl,Player2Frame
  jp    SetHitFrame           ;in: de->frame, hl->Player12Frame, changes: a,de,hl

  .SetLeftBendHit:
  ld    a,-1                  ;P1 looking left
  ld    ix,P1StaticDirection
  ld    bc,P1HitAnimationStep
  ld    de,P1LeftBendHitFrame
  ld    hl,Player1Frame
  jp    z,SetHitFrame         ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    ix,P2StaticDirection
  ld    bc,P2HitAnimationStep
  ld    de,P2LeftBendHitFrame
  ld    hl,Player2Frame
  jp    SetHitFrame           ;in: de->frame, hl->Player12Frame, changes: a,de,hl

SetJumpHit:
  ld    b,20                  ;Action=20 JumpHit
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af
  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  jp    c,.SetLeftJumpHit

  .SetRightJumpHit:
  ld    a,+1                  ;P1 looking right
  ld    ix,P1StaticDirection
  ld    iy,P1JumpHitRightFrame
  ld    bc,P1JumpHitAnimationStep
  ld    de,P1RightJumpHitFrame
  ld    hl,Player1Frame
  jp    z,SetJumpHitFrame     ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    ix,P2StaticDirection
  ld    iy,P2JumpHitRightFrame
  ld    bc,P2JumpHitAnimationStep
  ld    de,P2RightJumpHitFrame
  ld    hl,Player2Frame
  jp    SetJumpHitFrame       ;in: de->frame, hl->Player12Frame, changes: a,de,hl

  .SetLeftJumpHit:
  ld    a,-1                  ;P1 looking left
  ld    ix,P1StaticDirection
  ld    iy,P1JumpHitLeftFrame
  ld    bc,P1JumpHitAnimationStep
  ld    de,P1LeftJumpHitFrame
  ld    hl,Player1Frame
  jp    z,SetJumpHitFrame     ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    ix,P2StaticDirection
  ld    iy,P2JumpHitLeftFrame
  ld    bc,P2JumpHitAnimationStep
  ld    de,P2LeftJumpHitFrame
  ld    hl,Player2Frame
  jp    SetJumpHitFrame       ;in: de->frame, hl->Player12Frame, changes: a,de,hl

SetHeavilyHit:
  ld    b,21                  ;Action=21 Heavily
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af
  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  jp    c,.SetLeftHeavilyHit

  .SetRightHeavilyHit:
  ld    a,+1                  ;P1 looking right
  ld    ix,P1StaticDirection
  ld    iy,P1HeavilyHitRightFrame
  ld    bc,P1HeavilyHitAnimationStep
  ld    de,P1HeavilyHitRightFrame+5
  ld    hl,Player1Frame
  jp    z,SetHeavilyHitFrame  ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    ix,P2StaticDirection
  ld    iy,P2HeavilyHitRightFrame
  ld    bc,P2HeavilyHitAnimationStep
  ld    de,P2HeavilyHitRightFrame+5
  ld    hl,Player2Frame
  jp    SetHeavilyHitFrame    ;in: de->frame, hl->Player12Frame, changes: a,de,hl

  .SetLeftHeavilyHit:
  ld    a,-1                  ;P1 looking left
  ld    ix,P1StaticDirection
  ld    iy,P1HeavilyHitLeftFrame
  ld    bc,P1HeavilyHitAnimationStep
  ld    de,P1HeavilyHitLeftFrame+5
  ld    hl,Player1Frame
  jp    z,SetHeavilyHitFrame  ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    ix,P2StaticDirection
  ld    iy,P2HeavilyHitLeftFrame
  ld    bc,P2HeavilyHitAnimationStep
  ld    de,P2HeavilyHitLeftFrame+5
  ld    hl,Player2Frame
  jp    SetHeavilyHitFrame    ;in: de->frame, hl->Player12Frame, changes: a,de,hl

SetKnockDownRecover:
  ld    b,22                  ;Action=22 KnockDownRecover
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af
  call  StaDirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  ld    a,0                   ;reset animation pointer
  jp    c,.SetLeftSetKnockDownRecover

  .SetRightSetKnockDownRecover:
  ld    bc,P1KnockDownRecoverRightFrame
  ld    de,P1KnockDownRecoverRightFrame+5
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2KnockDownRecoverRightFrame
  ld    de,P2KnockDownRecoverRightFrame+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  .SetLeftSetKnockDownRecover:
  ld    bc,P1KnockDownRecoverLeftFrame
  ld    de,P1KnockDownRecoverLeftFrame+5
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2KnockDownRecoverLeftFrame
  ld    de,P2KnockDownRecoverLeftFrame+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl 

SetStandDefendHit:
  call  CheckControlsinBC     ;out a->controls, b=x move, c=y move, changes: af, bc
  dec   c                     ;down pressed ?
  jp    z,SetBendDefendHit.go

  .go:
  ld    c,sfxDefend
  call  SetSfx

  ld    b,23                  ;Action=23 StandDefendHit
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af
  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  jp    c,.SetLeftStandDefendHit

  .SetRightStandDefendHit:
  ld    a,+1                  ;P1 looking right
  ld    ix,P1StaticDirection
  ld    bc,P1HitAnimationStep
  ld    de,P1RightStandDefendFrame
  ld    hl,Player1Frame
  jp    z,SetHitFrame         ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    ix,P2StaticDirection
  ld    bc,P2HitAnimationStep
  ld    de,P2RightStandDefendFrame
  ld    hl,Player2Frame
  jp    SetHitFrame           ;in: de->frame, hl->Player12Frame, changes: a,de,hl

  .SetLeftStandDefendHit:
  ld    a,-1                  ;P1 looking left
  ld    ix,P1StaticDirection
  ld    bc,P1HitAnimationStep
  ld    de,P1LeftStandDefendFrame
  ld    hl,Player1Frame
  jp    z,SetHitFrame         ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    ix,P2StaticDirection
  ld    bc,P2HitAnimationStep
  ld    de,P2LeftStandDefendFrame
  ld    hl,Player2Frame
  jp    SetHitFrame           ;in: de->frame, hl->Player12Frame, changes: a,de,hl

SetBendDefendHit:
  call  CheckControlsinBC     ;out a->controls, b=x move, c=y move, changes: af, bc
  dec   c                     ;down pressed ?
  jp    nz,SetStandDefendHit.go

  .go:
  ld    c,sfxDefend
  call  SetSfx

  ld    b,24                  ;Action=24 StandDefendHit
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af
  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  jp    c,.SetLeftBendDefendHit

  .SetRightBendDefendHit:
  ld    a,+1                  ;P1 looking right
  ld    ix,P1StaticDirection
  ld    bc,P1HitAnimationStep
  ld    de,P1RightBendDefendFrame
  ld    hl,Player1Frame
  jp    z,SetHitFrame         ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    ix,P2StaticDirection
  ld    bc,P2HitAnimationStep
  ld    de,P2RightBendDefendFrame
  ld    hl,Player2Frame
  jp    SetHitFrame           ;in: de->frame, hl->Player12Frame, changes: a,de,hl

  .SetLeftBendDefendHit:
  ld    a,-1                  ;P1 looking left
  ld    ix,P1StaticDirection
  ld    bc,P1HitAnimationStep
  ld    de,P1LeftBendDefendFrame
  ld    hl,Player1Frame
  jp    z,SetHitFrame         ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    ix,P2StaticDirection
  ld    bc,P2HitAnimationStep
  ld    de,P2LeftBendDefendFrame
  ld    hl,Player2Frame
  jp    SetHitFrame           ;in: de->frame, hl->Player12Frame, changes: a,de,hl

CheckConditionsMet:           ;out c=allowed. Is special move allowed ?
  ;search for special
  ld    de,LenghtSpecialVariableTable
  .loop:
  cp    (ix+0*LenghtSpecialVariableTable)
  jr    z,.specialfound
  add   ix,de
  jp    .loop
  .specialfound:
  ;set player y in hl
  ld    a,(HandleWhichPlayer?)
  dec   a
  ld    hl,player1y
  ld    de,player1action
  jr    z,.go
  ld    hl,player2y
  ld    de,player2action
  .go:
  ;check freecontrols
  ld    a,(freezecontrols?)
  or    a
  ret   nz                    ;dont allow special moves at start or at end of fight
  ;check if special can only be executed on ground, air or both
  SpecialAttackConditionCheck1:
  ld    a,(ix+1)              ;1=execute only on ground, 2=execute only in air (0=both)
  or    a
  jr    z,.allowed
  dec   a
  jr    z,.checkground
  .checkair:                  ;special move can only be performed while in the air
  ld    a,(hl)
  cp    PlayeryAtstart
  jr    nz,.allowed
  xor   a                     ;reset carry flag (special is not allowed)
  ret
  .checkground:               ;special move can only be performed while on the ground
  ld    a,(hl)                ;is opponent in the air ?
  cp    PlayeryAtstart
  jr    z,.allowed
  xor   a                     ;reset carry flag (special is not allowed)
  ret
  .allowed:

  ;Special attack can only be executed in Idle, Bend, Walk, Jump, Jumpattack and defend
  SpecialAttackConditionCheck2:
  ld    a,(de)                ;player action
  cp    4                     ;check Idle, Bend, Walk, Jump
  jp    c,.allowed
  cp    12                    ;check standing or sitting attacks
  jp    c,.standingorsittingattackfound
  cp    18                    ;check jumping attacks, and defend
  jp    c,.allowed
  ld    b,1                   ;special attack 1
  cp    25
  jp    z,.checkspecial
  inc   b                     ;special attack 2
  cp    26
  jp    z,.checkspecial
  inc   b                     ;special attack 3
  cp    27
  jp    z,.checkspecial
  inc   b                     ;special attack 4
  cp    28
  jp    z,.checkspecial
  inc   b                     ;special attack 5
  cp    29
  jp    nz,.notallowed
  ;some special attacks can be stacked on top of each other, check that here
  .checkspecial:
  ld    a,b
  cp    (ix+5)
  jp    z,.allowed2
  jp    .notallowed

  ;can special be executed while punching/kicking standing or bend down ?
  .standingorsittingattackfound:
  ld    a,(ix+5)              ;can execute while punching/kicking ?
  or    a
  jr    nz,.allowed2
  .notallowed:
  xor   a                     ;reset carry flag (special is not allowed)
  ret

  .allowed:
  
  ;is special attack shooting a projectile ? and if so is projectile already in screen ?
  ld    a,(ix+4)
  or    a
  jr    z,.allowed2
  
  ;check projectile in screen
  ld    a,(HandleWhichPlayer?)
  dec   a
  ld    a,(P1ProjectileInScreen?)  
  jr    z,.playerfound
  ld    a,(P2ProjectileInScreen?)  
  .playerfound:
  or    a
  jr    nz,.notallowed
  
  .allowed2:
  scf                         ;set carry flag (special is allowed)  
  ret

SetSpecial1:
  call  CheckConditionsMet    ;out c=allowed. Is special move allowed ?
  ret   nc
.test:

  ld    a,(HandleWhichPlayer?)
  dec   a
  ld    c,sfxP1Special1
  jr    z,.PlayerFound
  ld    c,sfxP2Special1
  .PlayerFound:
  call  SetSfx 

  call  RaisePriority         ;current player goes to the front
  call  SetStaticDirection

  .SkipStaticDirection:

  ld    b,25                  ;Action=25 Special1
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af, b
  jp    c,.SetLeftSpecial1

  .SetRightSpecial1:
  ld    iy,P1Special1MovementTablePointer
  ld    bc,P1Special1RightFrame
  ld    de,P1Special1RightFrame+5
  ld    hl,Player1Frame
  jp    z,SetSpecialFrame     ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    iy,P2Special1MovementTablePointer
  ld    bc,P2Special1RightFrame
  ld    de,P2Special1RightFrame+5
  ld    hl,Player2Frame
  jp    SetSpecialFrame       ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  .SetLeftSpecial1:
  ld    iy,P1Special1MovementTablePointer
  ld    bc,P1Special1LeftFrame
  ld    de,P1Special1LeftFrame+5
  ld    hl,Player1Frame
  jp    z,SetSpecialFrame     ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    iy,P2Special1MovementTablePointer
  ld    bc,P2Special1LeftFrame
  ld    de,P2Special1LeftFrame+5
  ld    hl,Player2Frame
  jp    SetSpecialFrame       ;in: de->frame, hl->Player12Frame, changes: a,de,hl  
  
SetSpecial2:
  call  CheckConditionsMet    ;out c=allowed. Is special move allowed ?
  ret   nc
.test:

  ld    a,(HandleWhichPlayer?)
  dec   a
  ld    c,sfxP1Special2
  jr    z,.PlayerFound
  ld    c,sfxP2Special2
  .PlayerFound:
  call  SetSfx 

  call  RaisePriority         ;current player goes to the front
  call  SetStaticDirection

  .SkipStaticDirection:
  ld    b,26                  ;Action=26 Special2
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af, b
  jp    c,.SetLeftSpecial2

  .SetRightSpecial2:
  ld    iy,P1Special2MovementTablePointer
  ld    bc,P1Special2RightFrame
  ld    de,P1Special2RightFrame+5
  ld    hl,Player1Frame
  jp    z,SetSpecialFrame     ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    iy,P2Special2MovementTablePointer
  ld    bc,P2Special2RightFrame
  ld    de,P2Special2RightFrame+5
  ld    hl,Player2Frame
  jp    SetSpecialFrame       ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  .SetLeftSpecial2:
  ld    iy,P1Special2MovementTablePointer
  ld    bc,P1Special2LeftFrame
  ld    de,P1Special2LeftFrame+5
  ld    hl,Player1Frame
  jp    z,SetSpecialFrame     ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    iy,P2Special2MovementTablePointer
  ld    bc,P2Special2LeftFrame
  ld    de,P2Special2LeftFrame+5
  ld    hl,Player2Frame
  jp    SetSpecialFrame       ;in: de->frame, hl->Player12Frame, changes: a,de,hl  

SetSpecial3:
  call  CheckConditionsMet    ;out c=allowed. Is special move allowed (can it only be performed in the air/ground or both ?)
  ret   nc
.test:

  ld    a,(HandleWhichPlayer?)
  dec   a
  ld    c,sfxP1Special3
  jr    z,.PlayerFound
  ld    c,sfxP2Special3
  .PlayerFound:
  call  SetSfx 

  call  RaisePriority         ;current player goes to the front
  call  SetStaticDirection

  .SkipStaticDirection:
  ld    b,27                  ;Action=27 Special3
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af, b
  jp    c,.SetLeftSpecial3

  .SetRightSpecial3:
  ld    iy,P1Special3MovementTablePointer
  ld    bc,P1Special3RightFrame
  ld    de,P1Special3RightFrame+5
  ld    hl,Player1Frame
  jp    z,SetSpecialFrame     ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    iy,P2Special3MovementTablePointer
  ld    bc,P2Special3RightFrame
  ld    de,P2Special3RightFrame+5
  ld    hl,Player2Frame
  jp    SetSpecialFrame       ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  .SetLeftSpecial3:
  ld    iy,P1Special3MovementTablePointer
  ld    bc,P1Special3LeftFrame
  ld    de,P1Special3LeftFrame+5
  ld    hl,Player1Frame
  jp    z,SetSpecialFrame     ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    iy,P2Special3MovementTablePointer
  ld    bc,P2Special3LeftFrame
  ld    de,P2Special3LeftFrame+5
  ld    hl,Player2Frame
  jp    SetSpecialFrame       ;in: de->frame, hl->Player12Frame, changes: a,de,hl  

SetSpecial4:
  call  CheckConditionsMet    ;out c=allowed. Is special move allowed (can it only be performed in the air/ground or both ?)
  ret   nc
  call  RaisePriority         ;current player goes to the front
  call  SetStaticDirection

  .SkipStaticDirection:
  ld    b,28                  ;Action=28 Special4
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af, b
  jp    c,.SetLeftSpecial4

  .SetRightSpecial4:
  ld    iy,P1Special4MovementTablePointer
  ld    bc,P1Special4RightFrame
  ld    de,P1Special4RightFrame+5
  ld    hl,Player1Frame
  jp    z,SetSpecialFrame     ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    iy,P2Special4MovementTablePointer
  ld    bc,P2Special4RightFrame
  ld    de,P2Special4RightFrame+5
  ld    hl,Player2Frame
  jp    SetSpecialFrame       ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  .SetLeftSpecial4:
  ld    iy,P1Special4MovementTablePointer
  ld    bc,P1Special4LeftFrame
  ld    de,P1Special4LeftFrame+5
  ld    hl,Player1Frame
  jp    z,SetSpecialFrame     ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    iy,P2Special4MovementTablePointer
  ld    bc,P2Special4LeftFrame
  ld    de,P2Special4LeftFrame+5
  ld    hl,Player2Frame
  jp    SetSpecialFrame       ;in: de->frame, hl->Player12Frame, changes: a,de,hl  

SetSpecial5:
  call  CheckConditionsMet    ;out c=allowed. Is special move allowed (can it only be performed in the air/ground or both ?)
  ret   nc
  call  RaisePriority         ;current player goes to the front
  call  SetStaticDirection

  .SkipStaticDirection:
  ld    b,29                  ;Action=29 Special5
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af, b
  jp    c,.SetLeftSpecial5

  .SetRightSpecial5:
  ld    iy,P1Special5MovementTablePointer
  ld    bc,P1Special5RightFrame
  ld    de,P1Special5RightFrame+5
  ld    hl,Player1Frame
  jp    z,SetSpecialFrame     ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    iy,P2Special5MovementTablePointer
  ld    bc,P2Special5RightFrame
  ld    de,P2Special5RightFrame+5
  ld    hl,Player2Frame
  jp    SetSpecialFrame       ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  .SetLeftSpecial5:
  ld    iy,P1Special5MovementTablePointer
  ld    bc,P1Special5LeftFrame
  ld    de,P1Special5LeftFrame+5
  ld    hl,Player1Frame
  jp    z,SetSpecialFrame     ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    iy,P2Special5MovementTablePointer
  ld    bc,P2Special5LeftFrame
  ld    de,P2Special5LeftFrame+5
  ld    hl,Player2Frame
  jp    SetSpecialFrame       ;in: de->frame, hl->Player12Frame, changes: a,de,hl  

SetTossLeft:
  xor   a
  ld    (P1Attpoint1Sx),a     ;dont check if players get hit when tossing
  ld    (P2Attpoint1Sx),a     ;dont check if players get hit when tossing

  ld    b,30                  ;Action=30 Toss
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af
  call  SetCoordinatesPlayersWhenTossingLeft
  call  SetGettingTossed      ;opponent gets tossed
  call  RaisePriority         ;current player goes to the front
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af, b
  call  SetTossCooldown       ;after having tossed you cannot toss for a short while
  .SetLeftToss:
  ld    a,0                   ;reset animation pointer
  ld    bc,P1TossLeftFrame
  ld    de,P1TossLeftFrame+5
  ld    hl,Player1Frame
  jp    z,SetFrameTossP1      ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2TossLeftFrame
  ld    de,P2TossLeftFrame+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl

SetTossRight:
  xor   a
  ld    (P1Attpoint1Sx),a     ;dont check if players get hit when tossing
  ld    (P2Attpoint1Sx),a     ;dont check if players get hit when tossing

  ld    b,30                  ;Action=30 Toss
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af
  call  SetCoordinatesPlayersWhenTossingRight
  call  SetGettingTossed      ;opponent gets tossed
  call  RaisePriority         ;current player goes to the front
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af, b
  call  SetTossCooldown       ;after having tossed you cannot toss for a short while
  .SetRightToss:
  ld    a,0                   ;reset animation pointer
  ld    bc,P1TossRightFrame
  ld    de,P1TossRightFrame+5
  ld    hl,Player1Frame
  jp    z,SetFrameTossP1      ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2TossRightFrame
  ld    de,P2TossRightFrame+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl             

SetTossCooldown:
  ld    a,56
  jr    z,.p1
  .p2:
  ld    (P2TossCooldown?),a
  ret
  .p1:
  ld    (P1TossCooldown?),a
  ret

SetGettingTossed:             ;opponent gets tossed
  call  switchplayer          ;switch between 2 players
  ld    b,31                  ;Action=31 GettingTossed
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af
  call  .Setframe
  call  SetSfxWhenHit
  jp    switchplayer          ;switch back between 2 players
  
  .Setframe:
  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  jp    c,.SetLeftGettingTossed

  .SetRightGettingTossed:
  ld    a,+1                  ;P1 looking right
  ld    ix,P1StaticDirection
  ld    iy,P1GettingTossedRightFrame
  ld    bc,P1GettingTossedAnimationStep
  ld    de,P1GettingTossedRightFrame+5
  ld    hl,Player1Frame
  jp    z,SetGettingTossedFrame  ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    ix,P2StaticDirection
  ld    iy,P2GettingTossedRightFrame
  ld    bc,P2GettingTossedAnimationStep
  ld    de,P2GettingTossedRightFrame+5
  ld    hl,Player2Frame
  jp    SetGettingTossedFrame    ;in: de->frame, hl->Player12Frame, changes: a,de,hl

  .SetLeftGettingTossed:
  ld    a,-1                  ;P1 looking left
  ld    ix,P1StaticDirection
  ld    iy,P1GettingTossedLeftFrame
  ld    bc,P1GettingTossedAnimationStep
  ld    de,P1GettingTossedLeftFrame+5
  ld    hl,Player1Frame
  jp    z,SetGettingTossedFrame  ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    ix,P2StaticDirection
  ld    iy,P2GettingTossedLeftFrame
  ld    bc,P2GettingTossedAnimationStep
  ld    de,P2GettingTossedLeftFrame+5
  ld    hl,Player2Frame
  jp    SetGettingTossedFrame    ;in: de->frame, hl->Player12Frame, changes: a,de,hl
        
SetVictory:  
  call  RaisePriority         ;current player goes to the front
  ld    b,32                  ;Action=32 Victory
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af
  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  ld    a,0                   ;reset animation pointer
  jp    c,.SetLeftVictory

  .SetRightVictory:
  ld    bc,P1VictoryRightFrame
  ld    de,P1VictoryRightFrame+5
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2VictoryRightFrame
  ld    de,P2VictoryRightFrame+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  .SetLeftVictory:
  ld    bc,P1VictoryLeftFrame
  ld    de,P1VictoryLeftFrame+5
  ld    hl,Player1Frame
  jp    z,SetFrame            ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ld    bc,P2VictoryLeftFrame
  ld    de,P2VictoryLeftFrame+5
  ld    hl,Player2Frame
  jp    SetFrame              ;in: de->frame, hl->Player12Frame, changes: a,de,hl
      





















  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  















CheckFreezeControls:
  ld    a,(freezecontrols?)
  or    a
  ret   z
  xor   a
  ld    (ControlsP1),a
  ld    (NewPrControlsP1),a
  ld    (ControlsP2),a
  ld    (NewPrControlsP2),a
  ret

Announcer:
  ld    a,(AnnounceRound?)
  or    a
  ret   z
  ld    a,(CurrentRound)
  dec   a
  jr    z,.Round1
  dec   a
  jr    z,.Round2
  dec   a
  jr    z,.Round3
  dec   a
  jr    z,.FinalRound
  ret
  
  .Round1:
  ld    a,(AnnounceRoundStep)
  inc   a
  ld    (AnnounceRoundStep),a
  cp    10
  jr    z,.Sfxround
  cp    24
  jr    z,.Sfxone
  jp    .end
  
  .Round2:
  ld    a,(AnnounceRoundStep)
  inc   a
  ld    (AnnounceRoundStep),a
  cp    10
  jr    z,.Sfxround
  cp    24
  jr    z,.Sfxtwo
  jp    .end

  .Round3:
  ld    a,(AnnounceRoundStep)
  inc   a
  ld    (AnnounceRoundStep),a
  cp    10
  jr    z,.Sfxround
  cp    24
  jr    z,.Sfxthree
  jp    .end

  .FinalRound:
  ld    a,(AnnounceRoundStep)
  inc   a
  ld    (AnnounceRoundStep),a
  cp    10
  jr    z,.Sfxfinal
  cp    24
  jr    z,.Sfxround
  jp    .end

  .end:
  cp    57
  jr    z,.Sfxfight
  cp    63
  jr    z,.startfight
  ret

  .Sfxround:
  ld    c,sfxround
  jp    SetSfx
  .Sfxone:
  ld    c,sfxone
  jp    SetSfx
  .Sfxtwo:
  ld    c,sfxtwo
  jp    SetSfx
  .Sfxthree:
  ld    c,sfxthree
  jp    SetSfx
  .Sfxfinal:
  ld    c,sfxfinal
  jp    SetSfx
  .Sfxfight:
  ld    c,sfxfight
  jp    SetSfx
  .startfight:
  xor   a
  ld    (freezecontrols?),a
  ld    (AnnounceRound?),a
  ret
  
ScreenFadeInOut:
  ld    a,(fadeinout?)        ;0=nothing, 1=fade in, 2=fade out
  or    a
  ret   z

  ld    a,(fadeinoutstep)
  inc   a
  ld    (fadeinoutstep),a
	srl		a					            ;/2
  ;change the palette every 2 frames
  cp    9                     ;it takes 8 steps to fade in/out, at step 9 stop the fade
  jr    nz,.DoFade

  xor   a
  ld    (fadeinout?),a
  ret
  
  .DoFade:
  ld    b,a
  ld    a,(fadeinout?)        ;0=nothing, 1=fade in, 2=fade out
  dec   a
  ld    a,b
  jr    nz,.fadeout
  sub   a,8                   ;a fade in does the opposite of fade out
  neg                         ;so use the opposite fadeinoutstep
  .fadeout:
  
  .GoSetPalette: 
  ld    hl,currentpalette
  ld    de,editablepalette
  ld    bc,16*2
  ldir

  ld    hl,editablepalette
  ld    b,a                   ;amount of steps to darken

  call  .darkenpalette

  ld    hl,editablepalette
  call  setpalette
  ret

  .darkenpalette:
  ld    a,b
  or    a
  ret   z
  
  ld    c,b                   ;amount of steps to darken stored in c
  ld    e,16                  ;16 colors
  .loop:
  call  .darkencolor
  dec   e
  jp    nz,.loop
  ret

  .darkencolor:
  ;first make blue darker
  ld    a,(hl)
  and   %0000 1111            ;blue
  jp    z,.endDarkenBlue      ;there is no blue at all in this palletecolor
  .darkenblueloop:
  dec   a
  jp    z,.endDarkenBlue      ;there is no more blue in this palletecolor
  djnz  .darkenblueloop
  .endDarkenBlue:  
  ld    d,a                   ;store the darker blue in d

  ld    b,c                   ;amount of steps to darken
  ;then make red darker
  ld    a,(hl)
  and   %1111 0000            ;red
  jp    z,.endDarkenRed       ;there is no red at all in this palletecolor
  .darkenredloop:
  sub   a,16
  jp    z,.endDarkenRed       ;there is no more red in this palletecolor
  djnz  .darkenredloop
  .endDarkenRed:  
  ;then add blue and red together again
  or    d
  ld    (hl),a
  
  inc   hl                    ;green
  ld    b,c                   ;amount of steps to darken
  ;and finally make green darker
  ld    a,(hl)
  and   %0000 1111            ;green
  jp    z,.endDarkenGreen     ;there is no green at all in this palletecolor
  .darkengreenloop:
  dec   a
  jp    z,.endDarkenGreen     ;there is no more green in this palletecolor
  djnz  .darkengreenloop
  .endDarkenGreen:  
  ld    (hl),a
  
  inc   hl                    ;next color
  ld    b,c                   ;amount of steps to darken
	ret

HandleSpecial:                ;nc=ground found
  ;check end special move     ;end of movement?
  jp    nc,.groundFound
  ld    a,(hl)
  inc   hl                    ;PxSpecialxDurationTimer / Amount of frames that Special move takes
  cp    (hl)
  jp    z,.endTimerFound

  ;check if this special shoots a projectile
  inc   de
  inc   de
  ld    a,(de)                ;shoot projectile ? the value in a is also used to determine when projectile should be shot
  or    a
  jr    nz,.CheckShootprojectile

  ;check if character can move while performing special (but ONLY do this if this special does NOT shoot a projectile)
  inc   de
  inc   de
  ld    a,(de)                ;can move horizontally (slowly) during special attack ?
  or    a
  jr    nz,.movehorizontally

  ;if this special does NOT shoot a projectile AND may NOT move horizontally, ONLY then check initiate another special attack on enemy impact ?
  inc   de
  ld    a,(de)                ;initiate another special attack on enemy impact ?
  or    a
  ret   z
  ex    af,af'
  
  ;check enemy impact (at this point only a check is done for enemy impact when enemy defends)
  ld    a,(HandleWhichPlayer?)
  dec   a
  ld    hl,player2Action
  ld    bc,P2HitAnimationStep
  ld    de,P2HeavilyHitAnimationStep
  jr    z,.playeractionfound
  ld    hl,player1Action
  ld    bc,P1HitAnimationStep
  ld    de,P1HeavilyHitAnimationStep
  .playeractionfound:
  ld    a,(hl)
  cp    23                    ;Action=23 StandDefendHit
  jr    z,.impactfound
  cp    24                    ;Action=24 BendDefendHit
  jr    z,.impactfound
  cp    21                    ;Action=21 HeavilyHit
  ret   nz

  .heavilyhitImpactfound:
  ld    a,(de)                ;only continue if impact is on the first few frames of when enemy gets hit
  cp    13
  jr    c,.setnextspecial

  .impactfound:
  ld    a,(bc)                ;only continue if impact is on the first few frames of when enemy gets hit
  cp    7
  ret   nc

  .setnextspecial:
  ex    af,af'
  dec   a
  jp    z,SetSpecial1.SkipStaticDirection
  dec   a
  jp    z,SetSpecial2.SkipStaticDirection
  dec   a
  jp    z,SetSpecial3.SkipStaticDirection
  dec   a
  jp    z,SetSpecial4.SkipStaticDirection
  jp    SetSpecial5.SkipStaticDirection

  .movehorizontally:
  ;character may move horizontally while performing special
  ld    a,(framecounter)
  and   1
  ret   z                     ;move every other frame

  call  CheckControlsinBC     ;out a->controls, b=x move, c=y move, changes: af, bc
  ld    a,(HandleWhichPlayer?);if current player is player 1, then if carry
  dec   a                     ;player 1 is right of player 2, so player 1 
  ld    hl,player1x
  jr    z,.playerfound
  ld    hl,player2x
  .playerfound:
  ld    a,b
  add   a,a                   ;move with 2 pixels per time
  add   a,(hl)
  ld    (hl),a
  ret
  
  .CheckShootprojectile:
  ;check at which frame projectile should be shot
  dec   hl                    ;Special MovementTablePointer (points to the frame of this special move)
  cp    (hl)
  ret   nz

;hier kun je weer kijken naar de framepage van de sprite, adhv die waarde kun je de richting bepalen

  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af, b
  ld    b,-1                  ;projectile going left
  jp    c,.setprojectiledirection
  ld    b,+1                  ;projectile going right
  .setprojectiledirection:  
  jr    z,.player1

  .player2:
  ld    a,b
  ld    (P2ProjectileDirection),a
  ld    a,2
  ld    (P2ProjectileInScreen?),a
  xor   a
  ld    (P2ProjectileHit?),a
  
  ld    a,(player2x)
  cp    210
  jr    c,.endcheckprojectileRightofscreen2
  ld    a,b                   ;if player shoots projectile while standing far in the right corner of the screen,
  cp    +1                    ;AND if projectile is going right
  ld    a,(player2x)
  jr    nz,.endcheckprojectileRightofscreen2
  sub   a,18                  ;then sub 18 from the projectile x
  .endcheckprojectileRightofscreen2:
  ld    (ProjectileP2x),a
  ld    (P2Attpoint1SxProjectile),a
;  ld    a,(player2y)
;  ld    (ProjectileP2y),a
;  ld    (P2Attpoint1SyProjectile),a
  ret
  
  .player1:
  ld    a,b
  ld    (P1ProjectileDirection),a
  ld    a,2
  ld    (P1ProjectileInScreen?),a
  xor   a
  ld    (P1ProjectileHit?),a
  
  ld    a,(player1x)          
  cp    210
  jr    c,.endcheckprojectileRightofscreen1
  ld    a,b                   ;if player shoots projectile while standing far in the right corner of the screen,
  cp    +1                    ;AND if projectile is going right
  ld    a,(player1x)
  jr    nz,.endcheckprojectileRightofscreen1
  sub   a,18                  ;then sub 18 from the projectile x
  .endcheckprojectileRightofscreen1:
  ld    (ProjectileP1x),a
  ld    (P1Attpoint1SxProjectile),a
;  ld    a,(player1y)
;  ld    (ProjectileP1y),a
;  ld    (P1Attpoint1SyProjectile),a
  ret

  ;/check end special move
  .endTimerFound:
  .groundFound:
  ;initiate another special attack ?
  ld    a,(de)                ;initiate another special attack at end ? (0=no, 1,2,3,4... = attack number)
  or    a                     
  jp    z,.endspecial
  dec   a
  jp    z,SetSpecial1.SkipStaticDirection
  dec   a
  jp    z,SetSpecial2.SkipStaticDirection
  dec   a
  jp    z,SetSpecial3.SkipStaticDirection
  dec   a
  jp    z,SetSpecial4.SkipStaticDirection
;  dec   a
;  jp    z,SetSpecial5.SkipStaticDirection
  jp    SetSpecial5.SkipStaticDirection
  ;/initiate another special attack ?

  .endspecial:
  ;for guile's flash kick it's nicer if you allow player to end bend down, instead of in idle
  call  CheckControlsinBC     ;out a->controls, b=x move, c=y move, changes: af, bc
  dec   c                     ;down pressed ?
  jp    z,SetBend
  jp    SetIdle

P1ChargeDownCounter:          db    0
P1DownCharged?:               db    0
P1ChargeBackCounter:          db    0
P1BackCharged?:               db    0
P1PunchPressedCounter:        db    0
P1KickPressedCounter:         db    0
P1DownChargedShortly?:        db    0
P1BackChargedShortly?:        db    0
P1ForwChargedShortly?:        db    0
P1ForwChargedMedium?:         db    0
P2ChargeDownCounter:          db    0
P2DownCharged?:               db    0
P2ChargeBackCounter:          db    0
P2BackCharged?:               db    0
P2PunchPressedCounter:        db    0
P2KickPressedCounter:         db    0
P2DownChargedShortly?:        db    0
P2BackChargedShortly?:        db    0
P2ForwChargedShortly?:        db    0
P2BackChargedMedium?:         db    0
  
CheckSpecialActions:
  ;Player 1
  ld    ix,P1VariableTableSpecial1
  ld    a,1
  ld    (HandleWhichPlayer?),a
  ld    hl,P1ChargeDownCounter
  call  checkAttackType1      ;Special Attack Type1 = charge down, up + kick
  ld    hl,P1ChargeBackCounter
  call  checkAttackType2      ;Special Attack Type2 = charge back, forward + punch
  ld    hl,P1PunchPressedCounter
  call  checkAttackType3      ;Special Attack Type3 = punch pressed rapidly 
  ld    hl,P1KickPressedCounter
  call  checkAttackType4      ;Special Attack Type4 = kick pressed rapidly 
  ld    hl,P1DownChargedShortly?
  call  checkAttackType567    ;Special Attack Type5 = down, forward + punch, Type6 = down, back + kick, Type7 = foreward, down, foreward + punch
  ld    hl,P1DownChargedShortly?
  call  CheckForwMediumCharge ;Check medium charge foreward (used in foreward, down, foreward + punch)
  ;Player 2
  ld    ix,P2VariableTableSpecial1
  ld    a,2
  ld    (HandleWhichPlayer?),a
  ld    hl,P2ChargeDownCounter
  call  checkAttackType1      ;Special Attack Type1 = charge down, up + kick
  ld    hl,P2ChargeBackCounter
  call  checkAttackType2      ;Special Attack Type2 = charge back, forward + punch
  ld    hl,P2PunchPressedCounter
  call  checkAttackType3      ;Special Attack Type3 = punch pressed rapidly 
  ld    hl,P2KickPressedCounter
  call  checkAttackType4      ;Special Attack Type4 = kick pressed rapidly 
  ld    hl,P2DownChargedShortly?
  call  checkAttackType567    ;Special Attack Type5 = down, forward + punch, Type6 = down, back + kick, Type7 = foreward, down, foreward + punch
  ld    hl,P2DownChargedShortly?
  call  CheckForwMediumCharge ;Check medium charge foreward (used in foreward, down, foreward + punch)
  ret

checkAttackType1:             ;Special Attack Type1 = charge down, up + kick
  call  checkchargedown      ;out: hl->PxDownCharged?
  ;only continue if down is charged
  xor   a
  cp    (hl)                  ;hl=PxDownCharged?
  ret   z
  ;now check if up and kick is pressed
  call  CheckControlsinBC     ;out a->controls, b=x move, c=y move, changes: af, bc
  inc   c                     ;up pressed ?
  ret   nz
  bit   5,a                   ;hard kick pressed ?
  jp    nz,.AttackType1found
  bit   7,a                   ;soft kick pressed ?
  ret   z
  ;now check if player has a special attack type 1 in his/her special attack list
  .AttackType1found:
  ld    (hl),0                ;reset PxDownCharged
  ld    a,1                   ;check attack type 1 (ch-d,u+k)
  cp    (ix+0*LenghtSpecialVariableTable)
  jp    z,SetSpecial1
  cp    (ix+1*LenghtSpecialVariableTable)
  jp    z,SetSpecial2
  cp    (ix+2*LenghtSpecialVariableTable)
  jp    z,SetSpecial3
  cp    (ix+3*LenghtSpecialVariableTable)
  jp    z,SetSpecial4
  cp    (ix+4*LenghtSpecialVariableTable)
  jp    z,SetSpecial5
  ret
  checkchargedown:             ;first step is checking if down key is charged
  ;
  ; bit 7     6     5     4     3     2     1     0
  ;		  softk softp hardk hardp right left  down  up  (action)
  ;		  /     .     '     ;     right left  down  up  (ControlsP1)
  ;		  S     A     W     Q     right left  down  up  (ControlsP2)
  ;
  call  CheckControlsinBC     ;out a->controls, b=x move, c=y move, changes: af, bc
  dec   c                     ;down pressed ?
  jr    z,.downpressed
  xor   a
  ld    (hl),a                ;down not pressed, reset counter
  inc   hl                    ;PxDownCharged?
  dec   (hl)
  ret   p
  ld    (hl),a                ;down not pressed, slowly reset PxDownCharged?
  ret
  .downpressed:
  ld    a,(hl)                ;PxChargeDownCounter
  inc   a
  and   31
  ld    (hl),a
  inc   hl
  ret   nz
  ld    (hl),4                ;PxDownCharged? (remain charged for 4 frames after release)
  ret

checkAttackType2:             ;Special Attack Type2 = charge back, foreward + punch
  call  checkchargeback      ;out: hl->PxBackCharged?
  ;only continue if down is charged
  xor   a
  cp    (hl)                  ;hl=PxBackCharged?
  ret   z
  ;now check if foreward and punch is pressed
  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  jp    nc,.LookrightChargeBack
  .LookleftChargeBack:
  call  CheckControlsinBC     ;out a->controls, b=x move, c=y move, changes: af, bc
  inc   b                     ;left pressed ?
  jp    .check
  .LookrightChargeBack:
  call  CheckControlsinBC     ;out a->controls, b=x move, c=y move, changes: af, bc
  dec   b                     ;right pressed ?
  .check:
  ret   nz
  bit   4,a                   ;hard punch pressed ?
  jp    nz,.AttackType2found
  bit   6,a                   ;soft punch pressed ?
  ret   z
  ;now check if player has a special attack type 1 in his/her special attack list
  .AttackType2found:
  ld    (hl),0                ;reset PxBackCharged
  ld    a,2                   ;check attack type 1 (ch-d,u+k)
  cp    (ix+0*LenghtSpecialVariableTable)
  jp    z,SetSpecial1
  cp    (ix+1*LenghtSpecialVariableTable)
  jp    z,SetSpecial2
  cp    (ix+2*LenghtSpecialVariableTable)
  jp    z,SetSpecial3
  cp    (ix+3*LenghtSpecialVariableTable)
  jp    z,SetSpecial4
  cp    (ix+4*LenghtSpecialVariableTable)
  jp    z,SetSpecial5
  ret
  checkchargeback:             ;first step is checking if back key is charged
  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  jp    nc,.LookrightChargeBack
  .LookleftChargeBack:
  call  CheckControlsinBC     ;out a->controls, b=x move, c=y move, changes: af, bc
  dec   b                     ;right pressed ?
  jp    .check
  .LookrightChargeBack:
  ;
  ; bit 7     6     5     4     3     2     1     0
  ;		  softk softp hardk hardp right left  down  up  (action)
  ;		  /     .     '     ;     right left  down  up  (ControlsP1)
  ;		  S     A     W     Q     right left  down  up  (ControlsP2)
  ;
  call  CheckControlsinBC     ;out a->controls, b=x move, c=y move, changes: af, bc
  inc   b                     ;left pressed ?
  .check:
  jr    z,.backpressed
  xor   a
  ld    (hl),a                ;back not pressed, reset counter
  inc   hl                    ;P1BackCharged?
  dec   (hl)
  ret   p
  ld    (hl),a                ;back not pressed, slowly reset PxBackCharged?
  ret
  .backpressed:
  ld    a,(hl)                ;PxChargeBackCounter
  inc   a
  and   31
  ld    (hl),a
  inc   hl
  ret   nz
  ld    (hl),4                ;PxBackCharged? (remain charged for 4 frames after release)
  ret

checkAttackType3:             ;Special Attack Type3 = punch pressed rapidly 
  call  GetNewPrControls      ;out a-> NewPrcontrols, Changes: af
  ;
  ; bit 7     6     5     4     3     2     1     0
  ;		  softk softp hardk hardp right left  down  up  (action)
  ;		  /     .     '     ;     right left  down  up  (ControlsP1)
  ;		  S     A     W     Q     right left  down  up  (ControlsP2)
  ;
  bit   6,a                   ;softpunch pressed ?
  jp    nz,.punchpressed
  bit   4,a                   ;hardpunch pressed ?
  jp    nz,.punchpressed

  ld    a,(hl)
  dec   a                     ;punch not pressed, slowly reset PxPunchPressedCounter
  ret   m
  ld    (hl),a
  ret
  .punchpressed:  
  ld    a,(hl)
  add   a,4
  ld    (hl),a
  cp    10
  ret   c
  ld    (hl),0                ;reset PxPunchPressedCounter
  ;now check if player has a special attack type 3 in his/her special attack list
  ld    a,3                   ;check attack type 3 (repeat punch)
  cp    (ix+0*LenghtSpecialVariableTable)
  jp    z,SetSpecial1
  cp    (ix+1*LenghtSpecialVariableTable)
  jp    z,SetSpecial2
  cp    (ix+2*LenghtSpecialVariableTable)
  jp    z,SetSpecial3
  cp    (ix+3*LenghtSpecialVariableTable)
  jp    z,SetSpecial4
  cp    (ix+4*LenghtSpecialVariableTable)
  jp    z,SetSpecial5
  ret

checkAttackType4:             ;Special Attack Type4 = kick pressed rapidly 
  call  GetNewPrControls      ;out a-> NewPrcontrols, Changes: af
  ;
  ; bit 7     6     5     4     3     2     1     0
  ;		  softk softp hardk hardp right left  down  up  (action)
  ;		  /     .     '     ;     right left  down  up  (ControlsP1)
  ;		  S     A     W     Q     right left  down  up  (ControlsP2)
  ;
  bit   7,a                   ;softkick pressed ?
  jp    nz,.kickpressed
  bit   5,a                   ;hardkick pressed ?
  jp    nz,.kickpressed

  ld    a,(hl)
  dec   a                     ;kick not pressed, slowly reset PxKickPressedCounter
  ret   m
  ld    (hl),a
  ret
  .kickpressed:  
  ld    a,(hl)
  add   a,4
  ld    (hl),a
  cp    10
  ret   c
  ld    (hl),0                ;reset PxPunchPressedCounter
  ;now check if player has a special attack type 4 in his/her special attack list
  ld    a,4                   ;check attack type 4 (repeat kick)
  cp    (ix+0*LenghtSpecialVariableTable)
  jp    z,SetSpecial1
  cp    (ix+1*LenghtSpecialVariableTable)
  jp    z,SetSpecial2
  cp    (ix+2*LenghtSpecialVariableTable)
  jp    z,SetSpecial3
  cp    (ix+3*LenghtSpecialVariableTable)
  jp    z,SetSpecial4
  cp    (ix+4*LenghtSpecialVariableTable)
  jp    z,SetSpecial5
  ret
  
checkAttackType567:  
  Call  CheckDownShortCharge  ;Checks if down has been pressed within the last 6 frames
  ld    a,(hl)                ;PxDownChargedShortly?
  or    a
  ret   z                     ;dont continue if down hasnt been pressed within the last 6 frames
  inc   hl                    ;PxBackChargedShortly?
  Call  CheckBackShortCharge  ;Checks if back has been pressed within the last 6 frames
  ld    a,(hl)                ;Back charged ?
  or    a
  jp    nz,.DocheckAttack6    ;down and back are shortly charged, check kick for attack type 6
  inc   hl                    ;PxForeChargedShortly?
  Call  CheckForeShortCharge  ;Checks if foreward has been pressed within the last 6 frames
  ld    a,(hl)                ;Foreward charged ?
  or    a
  ret   z
  ;
  ; bit 7     6     5     4     3     2     1     0
  ;		  softk softp hardk hardp right left  down  up  (action)
  ;		  /     .     '     ;     right left  down  up  (ControlsP1)
  ;		  S     A     W     Q     right left  down  up  (ControlsP2)
  ;
  call  GetNewPrControls      ;out a-> NewPrcontrols, Changes: af
  ;now check if punch is pressed
  bit   4,a                   ;hard punch pressed ?
  jp    nz,.AttackType5found
  bit   6,a                   ;soft punch pressed ?
  ret   z
  .AttackType5found:

  ;at this point down, foreward + punch are pressed, check if foreward, down, foreward + punch is pressed for attack type 7
  inc   hl                    ;PxBackChargedMedium?
  ld    a,(hl)
  or    a
  ld    a,5                   ;check attack type 5 (down, forward + punch)
  jp    z,.DocheckAttack5     ;back medium not charged
  ld    a,7                   ;check attack type 7 (foreward, down, forward + punch)
  ;now check if player has a special attack type 7 in his/her special attack list
  cp    (ix+0*LenghtSpecialVariableTable)
  jp    z,SetSpecial1
  cp    (ix+1*LenghtSpecialVariableTable)
  jp    z,SetSpecial2
  cp    (ix+2*LenghtSpecialVariableTable)
  jp    z,SetSpecial3
  cp    (ix+3*LenghtSpecialVariableTable)
  jp    z,SetSpecial4
  cp    (ix+4*LenghtSpecialVariableTable)
  jp    z,SetSpecial5
  ld    a,5                   ;check attack type 5 (down, forward + punch)
  ;when attack type 7 is checked and not found, then also check attack type 5
  .DocheckAttack5:
  ;now check if player has a special attack type 5 in his/her special attack list
  cp    (ix+0*LenghtSpecialVariableTable)
  jp    z,SetSpecial1
  cp    (ix+1*LenghtSpecialVariableTable)
  jp    z,SetSpecial2
  cp    (ix+2*LenghtSpecialVariableTable)
  jp    z,SetSpecial3
  cp    (ix+3*LenghtSpecialVariableTable)
  jp    z,SetSpecial4
  cp    (ix+4*LenghtSpecialVariableTable)
  jp    z,SetSpecial5
  ret

  .DocheckAttack6:            ;down and back are shortly charged, check kick for attack type 6
  ;
  ; bit 7     6     5     4     3     2     1     0
  ;		  softk softp hardk hardp right left  down  up  (action)
  ;		  /     .     '     ;     right left  down  up  (ControlsP1)
  ;		  S     A     W     Q     right left  down  up  (ControlsP2)
  ;
  call  GetNewPrControls      ;out a-> NewPrcontrols, Changes: af
  ;now check if kick is pressed
  bit   5,a                   ;hardkick pressed ?
  jp    nz,.AttackType6found
  bit   7,a                   ;softkick pressed ?
  ret   z
  .AttackType6found:
  ;now check if player has a special attack type 6 in his/her special attack list
  ld    a,6                   ;check attack type 6 (down, back + kick)
  cp    (ix+0*LenghtSpecialVariableTable)
  jp    z,SetSpecial1
  cp    (ix+1*LenghtSpecialVariableTable)
  jp    z,SetSpecial2
  cp    (ix+2*LenghtSpecialVariableTable)
  jp    z,SetSpecial3
  cp    (ix+3*LenghtSpecialVariableTable)
  jp    z,SetSpecial4
  cp    (ix+4*LenghtSpecialVariableTable)
  jp    z,SetSpecial5
  ret  

CheckDownShortCharge:         ;Checks if down has been pressed within the last 6 frames
  ;
  ; bit 7     6     5     4     3     2     1     0
  ;		  softk softp hardk hardp right left  down  up  (action)
  ;		  /     .     '     ;     right left  down  up  (ControlsP1)
  ;		  S     A     W     Q     right left  down  up  (ControlsP2)
  ;
  call  GetNewPrControls      ;out a-> NewPrcontrols, Changes: af
  bit   1,a                   ;down pressed ?
  jr    z,.no
  ld    (hl),6                ;PxDownPrPreviousFrame? x frames to do the next combination
  inc   hl                    ;PxBackPrPreviousFrame?
  ld    (hl),0                ;reset back charge when down gets charged
  dec   hl
  ret
  .no:
  dec   (hl)                  ;down is not pressed, so slowly reset counter
  ret   p
  inc   (hl)
  ret

CheckBackShortCharge:         ;Checks if back has been pressed within the last 6 frames
  ;
  ; bit 7     6     5     4     3     2     1     0
  ;		  softk softp hardk hardp right left  down  up  (action)
  ;		  /     .     '     ;     right left  down  up  (ControlsP1)
  ;		  S     A     W     Q     right left  down  up  (ControlsP2)
  ;
  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  call  GetNewPrControls      ;out a-> NewPrcontrols, Changes: af
  bit   2,a                   ;left pressed ?
  jp    nc,.directionset
  bit   3,a                   ;right pressed
  .directionset:
  jr    z,.no
  ld    (hl),6                ;PxBackPrPreviousFrame? x frames to do the next combination
  ret
  .no:
  dec   (hl)                  ;down is not pressed, so slowly reset counter
  ret   p
  inc   (hl)
  ret

CheckForeShortCharge:         ;Checks if foreward has been pressed within the last 6 frames
  ;
  ; bit 7     6     5     4     3     2     1     0
  ;		  softk softp hardk hardp right left  down  up  (action)
  ;		  /     .     '     ;     right left  down  up  (ControlsP1)
  ;		  S     A     W     Q     right left  down  up  (ControlsP2)
  ;
  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  call  GetNewPrControls      ;out a-> NewPrcontrols, Changes: af
  bit   3,a                   ;right pressed ?
  jp    nc,.directionset
  bit   2,a                   ;left pressed ?
  .directionset:
  jr    z,.no
  ld    (hl),6                ;PxBackPrPreviousFrame? x frames to do the next combination
  ret
  .no:
  dec   (hl)                  ;down is not pressed, so slowly reset counter
  ret   p
  inc   (hl)
  ret

CheckForwMediumCharge:
  ld    a,(hl)                ;PxDownChargedShortly?
  inc   hl
  inc   hl
  inc   hl                    ;PxForwChargedMedium?
  or    a
  jp    nz,.no
  ;
  ; bit 7     6     5     4     3     2     1     0
  ;		  softk softp hardk hardp right left  down  up  (action)
  ;		  /     .     '     ;     right left  down  up  (ControlsP1)
  ;		  S     A     W     Q     right left  down  up  (ControlsP2)
  ;
  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  call  GetNewPrControls      ;out a-> NewPrcontrols, Changes: af
  bit   3,a                   ;right pressed
  jp    nc,.directionset
  bit   2,a                   ;left pressed ?
  .directionset:
  jr    z,.no
  ld    (hl),12               ;PxBackChargedMedium? x frames to do the next combination
  ret
  .no:
  dec   (hl)                  ;back is not pressed, so slowly reset counter
  ret   p
  inc   (hl)
  ret  
  
;CheckPlayerHit:
;  call  Checkplayer1hit
;  call  Checkplayer2hit
;  call  Checkplayer2hitbyprojectile
;  call  SetPreparedActionP1
;  call  Checkplayer1hitbyprojectile
;  ret

Checkplayer1hitbyprojectile:
  ld    a,(P2ProjectileInScreen?)
  or    a
  ret   z

  ld    a,(player1Action)     ;check if player 1 is already being hit
  cp    18                    ;StandHit
  jp    c,.PlayerNotAlreadyBeingHit
  cp    25                    ;Special1
  ret   c
  ;all actions from 18 up to 24 are action of player being hit
  .PlayerNotAlreadyBeingHit:

  ;collision detection x between player 1 block 1 and player 2's projectile
  ld    a,(Player1NxB1)
  add   a,30                  ;pretend that the player we are checking is much wider, this way we only use 1 attack point on our projectile

  ld    c,a

  ld    a,(P2Attpoint1SxProjectile)
  ld    b,a
  ld    a,(Player1SxB1)
  ;because we pretend that the player we are checking is much wider, we must also change his Sx
  sub   a,15
  jr    nc,.notcarry
  
  add   a,c
  ld    c,a
  
  xor   a
  .notcarry:

  cp    b
  jr    nc,.NoCollisionAttackpoint1Block1
  add   a,c                   ;right side of player 1 block 1 (Sx + Nx)
  jp    nc,.notcarry1
  ld    a,255
  .notcarry1:
  cp    b
  jr    c,.NoCollisionAttackpoint1Block1
  ;/collision detection x between player 1 block 1 and player 2's projectile
  ;collision detection y between player 1 block 1 and player 2's projectile
  ld    a,(Player1NyB1)
  ld    c,a
  ld    a,(Player1NyB2)
  add   a,c                   ;we now take block 1 and block 2 together (which is not very precise, but it is faster)
  ld    c,a


;  ld    a,(P2Attpoint1SyProjectile)
;  ld    b,a
  ld    a,(Player1SyB1)
;  cp    b
  cp    $d6                   ;lets use a static y for projectile, but this means that ALL projectiles have the same y and dont move vertically
  jr    nc,.NoCollisionAttackpoint1Block1
  add   a,c                   ;bottom side of player 2 block 1 (Sy + Ny)
  jr    z,.CollisionFound
;  cp    b
  cp    $d6
  jr    nc,.CollisionFound
  ;/collision detection y between player 1 block 1 and player 2's projectile

  .NoCollisionAttackpoint1Block1:
  ret

  .CollisionFound:
  ld    a,07                  ;amount of frames that projectile impact sprite animation lasts
  ld    (P2ProjectileHit?),a

  xor   a                     ;reset projectile impact variables
  ld    (P2Attpoint1SxProjectile),a
;  ld    (P2Attpoint1SyProjectile),a    
  ld    (P2ProjectileRightEndFrame),a
  ld    (P2ProjectileRightEndFrame+2),a
  ld    (P2ProjectileLeftEndFrame),a
  ld    (P2ProjectileLeftEndFrame+2),a
    
  ld    a,1
  ld    (HandleWhichPlayer?),a

  ;Action=16 StandDefend
  ;Action=17 BendDefend
  ld    a,(player1Action)
  cp    16
  jp    z,SetStandDefendHit
  cp    17
  jp    z,SetBendDefendHit

  
  ld    a,(player1Action)
  or    a
  jp    z,.SetStandHit        ;player is in Idle
  dec   a
  jp    z,.SetBendHit         ;player is Bend
  dec   a
  jp    z,.SetStandHit        ;player is Walking
  dec   a
  jp    z,.SetJumpHit         ;player is Jumping
  sub   5
  jp    c,.SetStandHit        ;player is attacking Standing
  sub   4
  jp    c,.SetBendHit         ;player is attacking Bending
  sub   4
  jp    c,.SetJumpHit         ;player is attacking Jumping

  ld    a,(player1Action)
  cp    30                    ;player is tossing, but gets hit (by projectile is only option)
  jp    z,.SetStandHit
  ;at this point player is hit while doing a special move
  ld    a,(player1y)          ;is player 2 in the air when performing special move or on ground height ?
  cp    PlayeryAtstart
  jp    z,.SetStandHit
  jp    .SetJumpHit

  .SetBendHit:
  ld    b,16
  call  QuickDrainHp          ;out: c=player died, in: b=amount of hp to drain. Changes: af,de,ix
  ld    c,sfxHardKickHit      ;also used for projectile hit impact
  call  SetSfx 
  jp    SetBendHit

  .SetStandHit:
  ld    b,16
  call  QuickDrainHp          ;out: c=player died, in: b=amount of hp to drain. Changes: af,de,ix
  ld    c,sfxHardKickHit      ;also used for projectile hit impact
  call  SetSfx 
  jp    SetStandHit

  .SetJumpHit:
  ld    b,16
  call  QuickDrainHp          ;out: c=player died, in: b=amount of hp to drain. Changes: af,de,ix
  ld    c,sfxHardKickHit      ;also used for projectile hit impact
  call  SetSfx 
  jp    SetJumpHit

Checkplayer1hit:
  ld    a,(P2Attpoint1Sx)     ;if attack point = 0 then player 2 is not attacking
  or    a
  ret   z

  ld    a,(player1Action)     ;check if player 1 is already being hit
  cp    18                    ;StandHit
  jp    c,.PlayerNotAlreadyBeingHit
  cp    25                    ;Special1
  ret   c
  cp    31                    ;GettingTossed
  ret   z
  ;all actions from 18 up to 24 are action of player being hit
  .PlayerNotAlreadyBeingHit:

  ;collision detection x between player 1 block 1 and player 2 attack point 1
  ld    a,(Player1NxB1)
  ld    c,a

  ld    a,(P2Attpoint1Sx)
  ld    b,a
  ld    a,(Player1SxB1)
  cp    b
  jr    nc,.NoCollisionAttackpoint1Block1
  add   a,c                   ;right side of player 1 block 1 (Sx + Nx)
  jp    nc,.notcarry1
  ld    a,255
  .notcarry1:
  cp    b
  jr    c,.NoCollisionAttackpoint1Block1
  ;/collision detection x between player 1 block 1 and player 2 attack point 1
  ;collision detection y between player 1 block 1 and player 2 attack point 1
  ld    a,(Player1NyB1)
  ld    c,a

  ld    a,(P2Attpoint1Sy)
  ld    b,a
  ld    a,(Player1SyB1)
  cp    b
  jr    nc,.NoCollisionAttackpoint1Block1
  add   a,c                   ;bottom side of player 1 block 1 (Sy + Ny)
  cp    b
  jr    nc,.CollisionFound
  ;/collision detection y between player 1 block 1 and player 2 attack point 1

  .NoCollisionAttackpoint1Block1:
  ;collision detection x between player 1 block 2 and player 2 attack point 1
  ld    a,(Player1NxB2)
  ld    c,a

  ld    a,(P2Attpoint1Sx)
  ld    b,a
  ld    a,(Player1SxB2)
  cp    b
  jr    nc,.NoCollisionAttackpoint1Block2
  add   a,c                   ;right side of player 1 block 1 (Sx + Nx)
  jp    nc,.notcarry2
  ld    a,255
  .notcarry2:
  cp    b
  jr    c,.NoCollisionAttackpoint1Block2
  ;/collision detection x between player 1 block 2 and player 2 attack point 1
  ;collision detection y between player 1 block 2 and player 2 attack point 1
  ld    a,(Player1NyB2)
  ld    c,a

  ld    a,(P2Attpoint1Sy)
  ld    b,a
  ld    a,(Player1SyB2)
  cp    b
  jr    nc,.NoCollisionAttackpoint1Block2
  add   a,c                   ;bottom side of player 1 block 1 (Sy + Ny)
  cp    b
  jr    nc,.CollisionFound
  ;/collision detection y between player 1 block 2 and player 2 attack point 1

  .NoCollisionAttackpoint1Block2:
  ;collision detection x between player 1 block 1 and player 2 attack point 2
  ld    a,(Player1NxB1)
  ld    c,a

  ld    a,(P2Attpoint2Sx)
  ld    b,a
  ld    a,(Player1SxB1)
  cp    b
  jr    nc,.NoCollisionAttackpoint2Block1
  add   a,c                   ;right side of player 1 block 1 (Sx + Nx)
  jp    nc,.notcarry3
  ld    a,255
  .notcarry3:
  cp    b
  jr    c,.NoCollisionAttackpoint2Block1
  ;/collision detection x between player 1 block 1 and player 2 attack point 2
  ;collision detection y between player 1 block 1 and player 2 attack point 2
  ld    a,(Player1NyB1)
  ld    c,a

  ld    a,(P2Attpoint2Sy)
  ld    b,a
  ld    a,(Player1SyB1)
  cp    b
  jr    nc,.NoCollisionAttackpoint2Block1
  add   a,c                   ;bottom side of player 1 block 1 (Sy + Ny)
  cp    b
  jr    nc,.CollisionFound
  ;/collision detection y between player 1 block 1 and player 2 attack point 2

  .NoCollisionAttackpoint2Block1:
  ;collision detection x between player 1 block 2 and player 2 attack point 2
  ld    a,(Player1NxB2)
  ld    c,a

  ld    a,(P2Attpoint2Sx)
  ld    b,a
  ld    a,(Player1SxB2)
  cp    b
  ret   nc
  add   a,c                   ;right side of player 1 block 1 (Sx + Nx)
  jp    nc,.notcarry4
  ld    a,255
  .notcarry4:
  cp    b
  ret   c
  ;/collision detection x between player 1 block 2 and player 2 attack point 2
  ;collision detection y between player 1 block 2 and player 2 attack point 2
  ld    a,(Player1NyB2)
  ld    c,a

  ld    a,(P2Attpoint2Sy)
  ld    b,a
  ld    a,(Player1SyB2)
  cp    b
  ret   nc
  add   a,c                   ;bottom side of player 1 block 1 (Sy + Ny)
  cp    b
  jr    nc,.CollisionFound
  ;/collision detection y between player 1 block 2 and player 2 attack point 2
  ret

  .CollisionFound:
  ld    a,6
  ld    (freezeplayer2?),a    ;when you hit your opponent you freeze shortly
  ld    a,1
  ld    (HandleWhichPlayer?),a

  ;Action=16 StandDefend
  ;Action=17 BendDefend
  ld    a,(player1Action)
  cp    16
  jp    z,.standdefend
  cp    17
  jp    z,.benddefend

  ld    bc,player2Action
  ld    hl,P2VariableTableSpecial1+3
  call  checkknockdownspecial ;out: c=heavily hit. if hit by a special move you might get knocked down (heavily hit)
  jp    nc,.SetHeavilyHit
    
  ;when you are hit in the air you go to Jumphit, if you are hit standing you go to Standhit
  ;if you are hit bending down then you go to bendhit. IF you get hit heavily then go to heavilyhit (it knock you off your feet)

  ld    a,(player1Action)
  or    a
  jp    z,.StandHit           ;player is in Idle
  dec   a
  jp    z,.BendHit            ;player is Bend
  dec   a
  jp    z,.StandHit           ;player is Walking
  dec   a
  jp    z,.SetJumpHit         ;player is Jumping
  sub   5
  jp    c,.StandHit           ;player is attacking Standing
  sub   4
  jp    c,.BendHit            ;player is attacking Bending
  sub   4
  jp    c,.SetJumpHit         ;player is attacking Jumping

  ld    a,(player1Action)
  cp    30                    ;player is tossing, but gets hit (by projectile is only option)
  jp    z,.SetStandHit
  ;at this point player is hit while doing a special move
  ld    a,(player1y)          ;is player 1 hit while performing a special move in the air or on ground height ?
  cp    PlayeryAtstart
  jp    z,.SetStandHit
  jp    .SetJumpHit

  .SetJumpHit:
  call  DrainHp               ;out: c=player died, changes af,bc,de,hl,ix
  call  SetSfxWhenHit
  ld    a,20                  ;Action=20 Jumphit
  ld    (prepareActionP1),a
  ret

  .BendHit:
  ld    a,(player2Action)
  cp    11                    ;Action=11 HardSitKick
  jp    z,.SetHeavilyHit
  jp    .SetBendHit

  .StandHit:
  ld    a,(player2Action)
  cp    11                    ;Action=11 HardSitKick
  jp    z,.SetHeavilyHit
  jp    .SetStandHit

  .standdefend:               ;player is hit, but defends standing
  ld    a,(player2Action)
  cp    11                    ;Action=11 HardSitKick
  jp    z,.SetHeavilyHit    
  ;a low attack (attack point y>234) MUST be defended sitting
  ld    a,(P2Attpoint1Sy)
  cp    234
  jp    nc,.SetStandHit
  ld    a,(P2Attpoint2Sy)
  cp    234
  jp    nc,.SetStandHit
  jp    .SetStandDefendHit

  .benddefend:                ;player is hit, but defends bend down
  ld    a,(player2y)          ;is player 2 in the air when attacking or on ground height ?
  cp    PlayeryAtstart-14
  jp    nc,.SetBendDefendHit   ;if player 2 is on ground height while attacking player 1 can defend

  .SetBendHit:
  call  DrainHp               ;out: c=player died, changes af,bc,de,hl,ix
  call  SetSfxWhenHit
  ld    a,19                  ;Action=19 BendHit
  ld    (prepareActionP1),a   ;player 1 defends bend down, but player 2 attacks him from the air, which should have been defended standing
  ret

  .SetStandHit:
  call  DrainHp               ;out: c=player died, changes af,bc,de,hl,ix
  call  SetSfxWhenHit
  ld    a,18                  ;Action=18 StandHit
  ld    (prepareActionP1),a
  ret
  
  .SetHeavilyHit:
  call  DrainHp               ;out: c=player died, changes af,bc,de,hl,ix
  call  SetSfxWhenHit
  ld    a,21                  ;Action=21 HeavilyHit
  ld    (prepareActionP1),a
  ret
  
  .SetStandDefendHit:
  ld    a,23                  ;Action=23 StandDefendHit
  ld    (prepareActionP1),a
  ret  

  .SetBendDefendHit:
  ld    a,24                  ;Action=24 BendDefendHit
  ld    (prepareActionP1),a
  ret    

SetPreparedActionP1:
  ld    a,1
  ld    (HandleWhichPlayer?),a

  ld    hl,prepareActionP1
  ld    a,(hl)
  or    a
  ret   z
  ld    (hl),0

  cp    18
  jp    z,SetStandHit
  cp    19
  jp    z,SetBendHit
  cp    20
  jp    z,SetJumphit
  cp    21
  jp    z,SetHeavilyHit
  cp    23
  jp    z,SetStandDefendHit
  cp    24
  jp    z,SetBendDefendHit
  ret

checkknockdownspecial:        ;out: nc=heavily hit. if hit by a special move you might get knocked down (heavily hit)
  ld    a,(bc)
  cp    25
  ret   c                     ;c=not heavily hit
  ld    de,LenghtSpecialVariableTable
  ;at this point player got hit by a special, check which special and check if player should get knocked down
  cp    25
  jr    z,.specialfound
  cp    26
  add   hl,de
  jr    z,.specialfound
  cp    27
  add   hl,de
  jr    z,.specialfound
  cp    28
  add   hl,de
  jr    z,.specialfound
  cp    29
  add   hl,de
  jr    z,.specialfound
  jp    .notheavilyhit

  .specialfound:
  ld    a,(hl)
  or    a
  jr    z,.notheavilyhit
  xor   a                     ;nc=heavily hit
  ret  
  .notheavilyhit:
  scf                         ;c=not heavily hit
  ret  

Checkplayer2hitbyprojectile:
  ld    a,(P1ProjectileInScreen?)
  or    a
  ret   z

  ld    a,(player2Action)     ;check if player 2 is already being hit
  cp    18                    ;StandHit
  jp    c,.PlayerNotAlreadyBeingHit
  cp    25                    ;Special1
  ret   c
  ;all actions from 18 up to 24 are action of player being hit
  .PlayerNotAlreadyBeingHit:

  ;collision detection x between player 2 block 1 and player 1 attack point 1
  ld    a,(Player2NxB1)
  add   a,30                  ;pretend that the player we are checking is much wider, this way we only use 1 attack point on our projectile

  ld    c,a

  ld    a,(P1Attpoint1SxProjectile)
  ld    b,a
  ld    a,(Player2SxB1)
  ;because we pretend that the player we are checking is much wider, we must also change his Sx
  sub   a,15
  jr    nc,.notcarry
  
  add   a,c
  ld    c,a
  
  xor   a
  .notcarry:

  cp    b
  jr    nc,.NoCollisionAttackpoint1Block1
  add   a,c                   ;right side of player 2 block 1 (Sx + Nx)
  jp    nc,.notcarry1
  ld    a,255
  .notcarry1:
  cp    b
  jr    c,.NoCollisionAttackpoint1Block1
  ;/collision detection x between player 2 block 1 and player 1 attack point 1
  ;collision detection y between player 2 block 1 and player 1 attack point 1
  ld    a,(Player2NyB1)
  ld    c,a

  ld    a,(Player2NyB2)
  add   a,c                   ;we now take block 1 and block 2 together (which is not very precise, but it is faster)
  ld    c,a

;  ld    a,(P1Attpoint1SyProjectile)
;  ld    b,a
  ld    a,(Player2SyB1)
;  cp    b
  cp    $d6                   ;lets use a static y for projectile, but this means that ALL projectiles have the same y and dont move vertically
  jr    nc,.NoCollisionAttackpoint1Block1
  add   a,c                   ;bottom side of player 2 block 1 (Sy + Ny)
  jr    z,.CollisionFound
;  cp    b
  cp    $d6
  jr    nc,.CollisionFound
  ;/collision detection y between player 2 block 1 and player 1 attack point 1

 .NoCollisionAttackpoint1Block1:
  ret

  .CollisionFound:
  ld    a,07                  ;amount of frames that projectile impact sprite animation lasts
  ld    (P1ProjectileHit?),a

  xor   a                     ;reset projectile impact variables
  ld    (P1Attpoint1SxProjectile),a
;  ld    (P1Attpoint1SyProjectile),a
  ld    (P1ProjectileRightEndFrame),a
  ld    (P1ProjectileRightEndFrame+2),a
  ld    (P1ProjectileLeftEndFrame),a
  ld    (P1ProjectileLeftEndFrame+2),a
    
  ld    a,2
  ld    (HandleWhichPlayer?),a

  ;Action=16 StandDefend
  ;Action=17 BendDefend
  ld    a,(player2Action)
  cp    16
  jp    z,SetStandDefendHit
  cp    17
  jp    z,SetBendDefendHit

  
  ld    a,(player2Action)
  or    a
  jp    z,.SetStandHit        ;player is in Idle
  dec   a
  jp    z,.SetBendHit         ;player is Bend
  dec   a
  jp    z,.SetStandHit        ;player is Walking
  dec   a
  jp    z,.SetJumpHit         ;player is Jumping
  sub   5
  jp    c,.SetStandHit        ;player is attacking Standing
  sub   4
  jp    c,.SetBendHit         ;player is attacking Bending
  sub   4
  jp    c,.SetJumpHit         ;player is attacking Jumping
  
  ld    a,(player2Action)
  cp    30                    ;player is tossing, but gets hit (by projectile is only option)
  jp    z,.SetStandHit
  ;at this point player is hit while doing a special move
  ld    a,(player2y)          ;is player 2 in the air when performing special move or on ground height ?
  cp    PlayeryAtstart
  jp    z,.SetStandHit
  jp    .SetJumpHit

  .SetBendHit:
  ld    b,16
  call  QuickDrainHp          ;out: c=player died, in: b=amount of hp to drain. Changes: af,de,ix
  ld    c,sfxHardKickHit      ;also used for projectile hit impact
  call  SetSfx 
  jp    SetBendHit

  .SetStandHit:
  ld    b,16
  call  QuickDrainHp          ;out: c=player died, in: b=amount of hp to drain. Changes: af,de,ix
  ld    c,sfxHardKickHit      ;also used for projectile hit impact
  call  SetSfx 
  jp    SetStandHit

  .SetJumpHit:
  ld    b,16
  call  QuickDrainHp          ;out: c=player died, in: b=amount of hp to drain. Changes: af,de,ix
  ld    c,sfxHardKickHit      ;also used for projectile hit impact
  call  SetSfx 
  jp    SetJumpHit
  
Checkplayer2hit:
  ld    a,(P1Attpoint1Sx)     ;if attack point = 0 then player 1 is not attacking
  or    a
  ret   z

  ld    a,(player2Action)     ;check if player 2 is already being hit
  cp    18                    ;StandHit
  jp    c,.PlayerNotAlreadyBeingHit
  cp    25                    ;Special1
  ret   c
  cp    31                    ;GettingTossed
  ret   z
  ;all actions from 18 up to 24 are action of player being hit
  .PlayerNotAlreadyBeingHit:

  ;collision detection x between player 2 block 1 and player 1 attack point 1
  ld    a,(Player2NxB1)
  ld    c,a

  ld    a,(P1Attpoint1Sx)
  ld    b,a
  ld    a,(Player2SxB1)
  cp    b
  jr    nc,.NoCollisionAttackpoint1Block1
  add   a,c                   ;right side of player 2 block 1 (Sx + Nx)
  jp    nc,.notcarry1
  ld    a,255
  .notcarry1:
  cp    b
  jr    c,.NoCollisionAttackpoint1Block1
  ;/collision detection x between player 2 block 1 and player 1 attack point 1
  ;collision detection y between player 2 block 1 and player 1 attack point 1
  ld    a,(Player2NyB1)
  ld    c,a

  ld    a,(P1Attpoint1Sy)
  ld    b,a
  ld    a,(Player2SyB1)
  cp    b
  jr    nc,.NoCollisionAttackpoint1Block1
  add   a,c                   ;bottom side of player 2 block 1 (Sy + Ny)
  cp    b
  jr    nc,.CollisionFound
  ;/collision detection y between player 2 block 1 and player 1 attack point 1

  .NoCollisionAttackpoint1Block1:
  ;collision detection x between player 2 block 2 and player 1 attack point 1
  ld    a,(Player2NxB2)
  ld    c,a

  ld    a,(P1Attpoint1Sx)
  ld    b,a
  ld    a,(Player2SxB2)
  cp    b
  jr    nc,.NoCollisionAttackpoint1Block2
  add   a,c                   ;right side of player 2 block 1 (Sx + Nx)
  jp    nc,.notcarry2
  ld    a,255
  .notcarry2:
  cp    b
  jr    c,.NoCollisionAttackpoint1Block2
  ;/collision detection x between player 2 block 2 and player 1 attack point 1
  ;collision detection y between player 2 block 2 and player 1 attack point 1
  ld    a,(Player2NyB2)
  ld    c,a

  ld    a,(P1Attpoint1Sy)
  ld    b,a
  ld    a,(Player2SyB2)
  cp    b
  jr    nc,.NoCollisionAttackpoint1Block2
  add   a,c                   ;bottom side of player 2 block 1 (Sy + Ny)
  cp    b
  jr    nc,.CollisionFound
  ;/collision detection y between player 2 block 2 and player 1 attack point 1

  .NoCollisionAttackpoint1Block2:
  ;collision detection x between player 2 block 1 and player 1 attack point 2
  ld    a,(Player2NxB1)
  ld    c,a

  ld    a,(P1Attpoint2Sx)
  ld    b,a
  ld    a,(Player2SxB1)
  cp    b
  jr    nc,.NoCollisionAttackpoint2Block1
  add   a,c                   ;right side of player 2 block 1 (Sx + Nx)
  jp    nc,.notcarry3
  ld    a,255
  .notcarry3:
  cp    b
  jr    c,.NoCollisionAttackpoint2Block1
  ;/collision detection x between player 2 block 1 and player 1 attack point 2
  ;collision detection y between player 2 block 1 and player 1 attack point 2
  ld    a,(Player2NyB1)
  ld    c,a

  ld    a,(P1Attpoint2Sy)
  ld    b,a
  ld    a,(Player2SyB1)
  cp    b
  jr    nc,.NoCollisionAttackpoint2Block1
  add   a,c                   ;bottom side of player 2 block 1 (Sy + Ny)
  cp    b
  jr    nc,.CollisionFound
  ;/collision detection y between player 2 block 1 and player 1 attack point 2

  .NoCollisionAttackpoint2Block1:
  ;collision detection x between player 2 block 2 and player 1 attack point 2
  ld    a,(Player2NxB2)
  ld    c,a

  ld    a,(P1Attpoint2Sx)
  ld    b,a
  ld    a,(Player2SxB2)
  cp    b
  ret   nc
  add   a,c                   ;right side of player 2 block 1 (Sx + Nx)
  jp    nc,.notcarry4
  ld    a,255
  .notcarry4:
  cp    b
  ret   c
  ;/collision detection x between player 2 block 2 and player 1 attack point 2
  ;collision detection y between player 2 block 2 and player 1 attack point 2
  ld    a,(Player2NyB2)
  ld    c,a

  ld    a,(P1Attpoint2Sy)
  ld    b,a
  ld    a,(Player2SyB2)
  cp    b
  ret   nc
  add   a,c                   ;bottom side of player 2 block 1 (Sy + Ny)
  cp    b
  jr    nc,.CollisionFound
  ;/collision detection y between player 2 block 2 and player 1 attack point 2
  ret

  .CollisionFound:
  ld    a,2
  ld    (HandleWhichPlayer?),a;
;check if player 1 is still attacking (he could have stopped this frame, but attack point still remains 1 frame longer)
;  call  CheckEnemyAttacking.endCheckFacingSameDirection
;  ret   nc                    ;out-> c:enemy is attacking, changes: af 

  ld    a,6
  ld    (freezeplayer1?),a    ;when you hit your opponent you freeze shortly

  ;Action=16 StandDefend
  ;Action=17 BendDefend
  ld    a,(player2Action)
  cp    16
  jp    z,.standdefend
  cp    17
  jp    z,.benddefend
  
  ld    bc,player1Action
  ld    hl,P1VariableTableSpecial1+3
  call  checkknockdownspecial ;out: c=heavily hit. if hit by a special move you might get knocked down (heavily hit)
  jp    nc,.SetHeavilyHit
  
  ;when you are hit in the air you go to Jumphit, if you are hit standing you go to Standhit
  ;if you are hit bending down then you go to bendhit. IF you get hit heavily then go to heavilyhit (it knock you off your feet)

  ld    a,(player2Action)
  or    a
  jp    z,.SetStandHit        ;player is in Idle
  dec   a
  jp    z,.SetBendHit         ;player is Bend
  dec   a
  jp    z,.SetStandHit        ;player is Walking
  dec   a
  jp    z,.SetJumpHit         ;player is Jumping
  sub   5
  jp    c,.SetStandHit        ;player is attacking Standing
  sub   4
  jp    c,.SetBendHit         ;player is attacking Bending
  sub   4
  jp    c,.SetJumpHit         ;player is attacking Jumping

  ld    a,(player2Action)
  cp    30                    ;player is tossing, but gets hit (by projectile is only option)
  jp    z,.SetStandHitWithoutHeavilyHitCheck
  ;at this point player is hit while doing a special move
  ld    a,(player2y)          ;is player 2 in the air when performing special move or on ground height ?
  cp    PlayeryAtstart
  jp    z,.SetStandHit
  jp    .SetJumpHit

  .SetBendHit:
  ld    a,(player1Action)
  cp    11                    ;Action=11 HardSitKick
  jp    z,.SetHeavilyHit

  call  DrainHp               ;out: c=player died, changes af,bc,de,hl,ix
  call  SetSfxWhenHit
  jp    SetBendHit

  .SetStandHit:
  ld    a,(player1Action)
  cp    11                    ;Action=11 HardSitKick
  jp    z,.SetHeavilyHit
  .SetStandHitWithoutHeavilyHitCheck:
  call  DrainHp               ;out: c=player died, changes af,bc,de,hl,ix
  call  SetSfxWhenHit
  jp    SetStandHit

  .SetJumpHit:
  call  DrainHp               ;out: c=player died, changes af,bc,de,hl,ix
  call  SetSfxWhenHit
  jp    SetJumpHit
  
  .standdefend:               ;player is hit, but defends standing
  ld    a,(player1Action)
  cp    11                    ;Action=11 HardSitKick
  jp    z,.SetHeavilyHit
  ;a low attack (attack point y>234) MUST be defended sitting
  ld    a,(P1Attpoint1Sy)
  cp    234
  jp    nc,.SetStandHit
  ld    a,(P1Attpoint2Sy)
  cp    234
  jp    nc,.SetStandHit
  jp    SetStandDefendHit  

  .benddefend:                ;player is hit, but defends bend down
  ld    a,(player1y)          ;is player 1 in the air when attacking or on ground height ?
  cp    PlayeryAtstart-14
  jp    nc,SetBendDefendHit    ;if player 2 is on ground height while attacking player 1 can defend
  call  DrainHp               ;out: c=player died, changes af,bc,de,hl,ix
  call  SetSfxWhenHit
  jp    SetBendHit            ;player 1 defends bend down, but player 2 attacks him from the air, which needs to be defended standing
  
  .SetHeavilyHit:
  call  DrainHp               ;out: c=player died, changes af,bc,de,hl,ix
  call  SetSfxWhenHit
  jp    SetHeavilyHit  
  
;PlayerDied:
;  ld    a,(P1Dies?)
;  or    a
;  call  nz,.P1Dies
;  ld    a,(P2Dies?)
;  or    a
;  call  nz,.P2Dies
;  ret

Player1Died:
  ld    a,(P1Dies?)
  add   a,1
  ret   c
  ld    (P1Dies?),a
  cp    2                     ;first frame of death ?
  ld		c,%0000 0000          ;total level / level direct
  jr    z,.P1step1
  cp    14                    ;first frame of death ?
  ld		c,%0011 1000          ;total level / level direct
  jr    z,.P1step2and3
  cp    26                    ;first frame of death ?
  ld		c,%0111 0000          ;total level / level direct
  jr    z,.P1step2and3
  cp    50                    ;go to victory action
  jr    z,.P1step4
  jp    laststepsafterplayerdied
  .P1step1:
  call	.P1step2and3          ;set sfx male/female dies
  ld    a,1
  ld    (fadeoutmusic?),a
  ld    a,1
  ld    (HandleWhichPlayer?),a
  ld    a,(player1Action)     ;check if player 2 is getting tossed
  cp    31                    ;Action=31 GettingTossed
  jp    nz,SetHeavilyHit
  ret

  .P1step2and3:
  ld    a,(Player1Male?)
  or    a
  ld    a,sfxMaleDies
  jr    nz,.P1SexFound
  inc   a
  .P1SexFound:
  ld    (ExtraSFX?),a
  ld		a,$52                 ;second time Male Dies sample is played volume is lower
  jp    opl4_Register_Write   ;In: A: register C: data, changes: a  

  .P1step4:
  ld    a,(P2Dies?)
  or    a
  ret   nz
  ld    a,(P2RoundsWon)       ;p2 won a round
  inc   a
  ld    (P2RoundsWon),a
  
  ld    a,2
  ld    (HandleWhichPlayer?),a
  jp    SetVictory 

Player2Died:
  ld    a,(P2Dies?)
  add   a,1
  ret   c
  ld    (P2Dies?),a
  cp    2                     ;first frame of death ?
  ld		c,%0000 0000          ;total level / level direct
  jr    z,.P2step1
  cp    14                    ;first frame of death ?
  ld		c,%0011 1000          ;total level / level direct
  jr    z,.P2step2and3
  cp    26                    ;first frame of death ?
  ld		c,%0111 0000          ;total level / level direct
  jr    z,.P2step2and3
  cp    50                    ;go to victory action
  jr    z,.P2step4
  jp    laststepsafterplayerdied
  .P2step1:
  call	.P2step2and3          ;set sfx male/female dies
  ld    a,1
  ld    (fadeoutmusic?),a
  ld    a,2
  ld    (HandleWhichPlayer?),a
  ld    a,(player2Action)     ;check if player 2 is getting tossed
  cp    31                    ;Action=31 GettingTossed
  jp    nz,SetHeavilyHit
  ret

  .P2step2and3:
  ;if both player die simultaneously then use different channel for sfx P1dies
  ld    a,(P1Dies?)
  or    a
  ld    hl,ExtraSFX?
  jp    z,.SetSfxChannel
  ld    hl,P2SetSFX?
  .SetSfxChannel:
  ;/if both player die simultaneously then use different channel for sfx P1dies

  ld    a,(Player2Male?)
  or    a
  ld    a,sfxMaleDies
  jr    nz,.P2SexFound
  inc   a
  .P2SexFound:
  ld    (hl),a
  ld		a,$52                 ;second time Male Dies sample is played volume is lower
  jp    opl4_Register_Write   ;In: A: register C: data, changes: a  

  .P2step4:
  ld    a,(P1Dies?)
  or    a
  ret   nz
  ld    a,(P1RoundsWon)       ;p1 won a round
  inc   a
  ld    (P1RoundsWon),a

  ld    a,1
  ld    (HandleWhichPlayer?),a
  jp    SetVictory   

  laststepsafterplayerdied:
  cp    40                    ;stop music
  jp    z,stopmusic
  cp    51                    ;announcer: "you"
  jr    z,.sfxyou
  cp    58                    ;announcer: "win"
  jr    z,.sfxwin
  cp    68                    ;announcer: "perfect"
  jr    z,.sfxperfect
  cp    80                    ;fadeout screen
  jr    z,.fadeout
  cp    100                   ;end fight
  jr    z,.endfight
  ret
  .sfxperfect:
  ;IF arcade mode is active, AND p1 lost, then sfx perfect should not be played
  ld    a,(mainmenuoptionselected?)
  cp    1                     ;arcade is pressed
  jp    nz,.endchecksfxPerfect
  ld    a,(P1Dies?)
  or    a
  ret   nz
  .endchecksfxPerfect:
  ;/IF arcade mode is active, AND p1 lost, then sfx LOSE should be played

  ld    c,sfxperfect
  ld    a,(Player1Hp)
  cp    188  
  jp    z,SetSfx
  ld    a,(Player2Hp)
  cp    188  
  jp    z,SetSfx
  ret
  
  .sfxyou:
  call  .checkDoubleKo        ;c=double KO
  ret   c
  ld    c,sfxyou
  jp    SetSfx
  .sfxwin:
  call  .checkDoubleKo        ;c=double KO
  ret   c

  ld    c,sfxwin
  ;IF arcade mode is active, AND p1 lost, then sfx LOSE should be played
  ld    a,(mainmenuoptionselected?)
  cp    1                     ;arcade is pressed
  jp    nz,.endchecksfxLose
  ld    a,(P1Dies?)
  or    a
  jp    z,.endchecksfxLose
  ld    c,sfxlose
  .endchecksfxLose:
  ;/IF arcade mode is active, AND p1 lost, then sfx LOSE should be played
  jp    SetSfx
  .fadeout:
  ld    a,2
  ld    (fadeinout?),a        ;0=nothing, 1=fade in, 2=fade out
  xor   a
  ld    (fadeinoutstep),a
  ret  
  .endfight:
  ld    a,1
  ld    (fightended?),a
  ret

  .checkDoubleKo:             ;c=double KO
  ld    a,(P2Dies?)
  or    a
  ret   z
  ld    a,(P1Dies?)
  or    a
  ret   z
  scf
  ret
  
P1HpbarChangeTablepointer:
  ds    1
P1HpbarChangeTable:
  db    1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
  db    3,4,2,3,4,3,4,2,3,4,2,2,2,2,2,2,2,2,2,2,2,2,5,6
P2HpbarChangeTablepointer:
  ds    1
P2HpbarChangeTable:
  db    1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
  db    3,4,2,3,4,3,4,2,3,4,2,2,2,2,2,2,2,2,2,2,2,2,5,6
AdjustscoreboardPlayer1:
	db    0,0,0,3
	db    0,0,0,0
	db    4,0,7,0
	db    0,0,$98
AdjustscoreboardPlayer2:
	db    0,0,0,3
	db    0,0,0,0
	db    4,0,7,0
	db    0,0,$98
		
HandleScoreboard:
  call  CheckHpPlayer1        ;check if hp has dropped, and if so set changeHpbarPlayer1? to 1
  call  CheckHpPlayer2        ;same same
  call  AlterHpBarPlayer1     ;is changeHpbarPlayer1 set to 1 ? then alter hp bar in 4 steps
;  call  AlterHpBarPlayer2     ;same same same same 
;  ret

AlterHpBarPlayer2:
  ;altering the hp bar is done in 4 steps
  ld    a,(changeHpbarPlayer2?)
  or    a
  ret   z

  ld    a,(HpbarP2ChangeStep)
  inc   a
  ld    (HpbarP2ChangeStep),a
  dec   a
  jp    z,.step1
  cp    4
  jp    c,.step2till4
  xor   a
  ld    (changeHpbarPlayer2?),a
  ld    (HpbarP2ChangeStep),a
  ret

  ;step 1 - change the hp bar in the buffer page (page 3)
  .step1:
  ld    b,3
  jp    .set
  ;step 2 - change the hp bar in the page that was displayer previous frame
  ;step 3 - change the hp bar in the page that was displayer previous frame
  ;step 4 - change the hp bar in the page that was displayer previous frame
  .step2till4:
  ld    a,(screenpage)
  or    a
  ld    b,2                   ;if current page =0 then restore page 2
  jr    z,.set
  dec   a
  ld    b,0                   ;if current page =1 then restore page 0
  jr    z,.set
  ld    b,1                   ;if current page =2 then restore page 1
  .set:
  ld    a,b
  ld    (AdjustscoreboardPlayer2+dpage),a
  ld    hl,AdjustscoreboardPlayer2
  jp    DoCopygrauw 

AlterHpBarPlayer1:
  ;altering the hp bar is done in 4 steps
  ld    a,(changeHpbarPlayer1?)
  or    a
  ret   z

  ld    a,(HpbarP1ChangeStep)
  inc   a
  ld    (HpbarP1ChangeStep),a
  dec   a
  jp    z,.step1
  cp    4
  jp    c,.step2till4
  xor   a
  ld    (changeHpbarPlayer1?),a
  ld    (HpbarP1ChangeStep),a
  ret

  ;step 1 - change the hp bar in the buffer page (page 3)
  .step1:
  ld    b,3
  jp    .set
  ;step 2 - change the hp bar in the page that was displayer previous frame
  ;step 3 - change the hp bar in the page that was displayer previous frame
  ;step 4 - change the hp bar in the page that was displayer previous frame
  .step2till4:
  ld    a,(screenpage)
  or    a
  ld    b,2                   ;if current page =0 then restore page 2
  jr    z,.set
  dec   a
  ld    b,0                   ;if current page =1 then restore page 0
  jr    z,.set
  ld    b,1                   ;if current page =2 then restore page 1
  .set:
  ld    a,b
  ld    (AdjustscoreboardPlayer1+dpage),a
  ld    hl,AdjustscoreboardPlayer1
  jp    DoCopygrauw  

CheckHpPlayer1:
  ld    a,(Player1HpbarHp)
  ld    b,a
  ld    a,(Player1Hp)
	srl		a					;/2
  sub   a,b
  ret   nc  

  ld    a,(changeHpbarPlayer1?)
  or    a
  ret   nz                    ;dont continue if hp bar is still being processed

  ld    ix,AdjustscoreboardPlayer1
  ld    b,208
  ld    d,2
  ld    c,0

  ld    a,(Player1HpbarHp)
  sub   a,2
  ld    (Player1HpbarHp),a

  ld    a,1
  ld    (changeHpbarPlayer1?),a
  ld    a,(AdjustscoreboardPlayer1+dx)
  add   a,d
  ld    (AdjustscoreboardPlayer1+dx),a
  ld    hl,P1HpbarChangeTablepointer
  jp    AlterHpbar

CheckHpPlayer2:
  ld    a,(Player2HpbarHp)
  ld    b,a
  ld    a,(Player2Hp)
	srl		a					;/2
  sub   a,b
  ret   nc  

  ld    a,(changeHpbarPlayer2?)
  or    a
  ret   nz                    ;dont continue if hp bar is still being processed

  ld    ix,AdjustscoreboardPlayer2
  ld    b,226
  ld    d,-2
  ld    c,2

  ld    a,(Player2HpbarHp)
  sub   a,2
  ld    (Player2HpbarHp),a

  ld    a,1
  ld    (changeHpbarPlayer2?),a
  ld    a,(AdjustscoreboardPlayer2+dx)
  add   a,d
  ld    (AdjustscoreboardPlayer2+dx),a
  ld    hl,P2HpbarChangeTablepointer
;  jp    AlterHpbar

AlterHpbar:
  inc   (hl)
  ld    d,0
  ld    e,(hl)
  add   hl,de
  ld    a,(hl)
  dec   a                     ;graphic part 1
  jp    z,.graphicpart1
  dec   a                     ;graphic part 2
  jp    z,.graphicpart2
  dec   a                     ;graphic part 3
  jp    z,.graphicpart3
  dec   a                     ;graphic part 4
  jp    z,.graphicpart4
  dec   a                     ;graphic part 5
  jp    z,.graphicpart5
  dec   a                     ;graphic part 6
  jp    z,.graphicpart6
  ret
  .graphicpart1:
  ld    a,b
  ld    (ix+sx),a
  ret
  .graphicpart2:
  ld    a,b
  add   a,4
  ld    (ix+sx),a
  ret
  .graphicpart3:
  ld    a,b
  add   a,8
  ld    (ix+sx),a
  ret
  .graphicpart4:
  ld    a,b
  add   a,4
  ld    (ix+sx),a
  dec   (ix+dy)
  ret
  .graphicpart5:
  ld    a,b
  add   a,12
  ld    (ix+sx),a
  ret
  .graphicpart6:
  ld    a,b
  add   a,16
  ld    (ix+sx),a
  ld    a,2
  ld    (ix+nx),a
  ld    a,(ix+dx)
  add   a,c
  ld    (ix+dx),a
  ret  

HandleTossCoolDown:
  ld    a,(P1TossCooldown?)
  or    a
  jr    nz,.P1
  ld    a,(P2TossCooldown?)
  or    a
  ret   z
  .P2:
  dec   a
  ld    (P2TossCooldown?),a
  ret
  .P1:
  dec   a
  ld    (P1TossCooldown?),a
  ld    a,(P2TossCooldown?)
  dec   a
  ret   m
  ld    (P2TossCooldown?),a
  ret
    
;bending lowers priority, unless opponent is bend as well
SetBendPriority:              ;which player comes in the front, and which comes in the back ?
  ld    a,(HandleWhichPlayer?)
  dec   a
  jr    z,.HandleP1
  .HandleP2:
  ld    a,(player1Action)
  dec   a                     ;player 1 is also bend ?
  ret   z                     ;then don't change priority
  ld    a,1
  ld    (PlayerPriority),a    ;1=player1   2=player2 (which player comes in front of the other ?)
  ret
  .HandleP1:
  ld    a,(player2Action)
  dec   a                     ;player 2 is also bend ?
  ret   z                     ;then don't change priority
  ld    a,2
  ld    (PlayerPriority),a    ;1=player1   2=player2 (which player comes in front of the other ?)
  ret

;going to idle only raises priority if opponent is bend
SetIdlePriority:              ;which player comes in the front, and which comes in the back ?
  ld    a,(HandleWhichPlayer?)
  dec   a
  jr    z,.HandleP1
  .HandleP2:
  ld    a,(player1Action)
  dec   a                     ;player 1 is bend ?
  ret   nz                    ;if not, then don't change priority
  ld    a,2
  ld    (PlayerPriority),a    ;1=player1   2=player2 (which player comes in front of the other ?)
  ret
  .HandleP1:
  ld    a,(player2Action)
  dec   a                     ;player 2 is bend ?
  ret   nz                    ;if not, then don't change priority
  ld    a,1
  ld    (PlayerPriority),a    ;1=player1   2=player2 (which player comes in front of the other ?)
  ret

;when jumping or attacking you become in front of your opponent
RaisePriority:                ;current player goes to the front
  ld    a,(HandleWhichPlayer?)
  ld    (PlayerPriority),a    ;1=player1   2=player2 (which player comes in front of the other ?)
  ret  

GetmovementSpeed:             ;in: de->table lenght, out: a=movem. speed, changes af, bc, de
  ex    de,hl
  ld    b,(hl)                ;lenght table
  inc   hl
  ld    a,(hl)                ;pointer
  cp    b
  jp    nz,.pointerendcheckdone
  xor   a
  .pointerendcheckdone:
  inc   a  
  ld    (hl),a
  
  ld    c,(hl)
  ld    b,0
  add   hl,bc
  ld    a,(hl)                ;movement speed in d
  ex    de,hl
  ret

PlayersCloseTogether:         ;make sure players cant move through each other, push them away when they get too close to each other
  ld    a,(player1y)
  ld    b,a
  ld    a,(player2y)
  sub   a,b
  jr    nc,.notcarry
  neg
  .notcarry:
  cp    40                    ;only continue if players are within 40 pixel radius of each other
  ret   nc

  ld    a,(player1x)
  ld    b,a
  ld    a,(player2x)
  sub   a,b
  jp    c,.player1isrightofplayer2

  .player2isrightofplayer1:
  cp    18
  ret   nc                    ;players are not close together

  cp    14
  ld    b,2
  jp    nc,.setpushback1
  cp    10
  ld    b,4
  jp    nc,.setpushback1
  ld    b,6
  .setpushback1:

  ld    a,(player1x)
  sub   a,b
  jr    c,.carry1
  ld    (player1x),a
  .carry1:
  ld    a,(player2x)
  add   a,b
  ret   c
  ld    (player2x),a
  ret
  
  .player1isrightofplayer2:  
  neg
  cp    18
  ret   nc                    ;players are not close together

  cp    14
  ld    b,2
  jp    nc,.setpushback2
  cp    10
  ld    b,4
  jp    nc,.setpushback2
  ld    b,6
  .setpushback2:

  ld    a,(player1x)
  add   a,b
  jr    c,.carry2
  ld    (player1x),a  
  .carry2:
  ld    a,(player2x)
  sub   a,b
  ret   c
  ld    (player2x),a  
  ret
  
SearchPlayerSetframeAndBackup:;Searches player, sets Frame in de to PlayerxFrame(&Backup)
  ld    a,(HandleWhichPlayer?);if current player is player 1, then if carry
  dec   a                     ;player 1 is right of player 2, so player 1 
  ld    hl,Player1Frame
  ld    bc,Player1FrameBackup
  jr    z,.SetFrameAndBackup
  ld    hl,Player2Frame
  ld    bc,Player2FrameBackup
.SetFrameAndBackup:
  ;set frame
  ld    a,(de)                ;fetch current Idle frame
  ld    (hl),a                ;and write it to PlayerxFrame
  ld    (bc),a                ;and write it to PlayerxFrame
  inc   de
  inc   hl
  inc   bc
  ld    a,(de)                ;fetch current Idle frame
  ld    (hl),a                ;and write it to PlayerxFrame
  ld    (bc),a                ;and write it to PlayerxFrame
  ;set framepage
  inc   de                    
  inc   hl
  inc   bc
  ld    a,(de)                ;fetch frame page
  ld    (hl),a                ;and write it to PlayerxFramePage
  ld    (bc),a                ;and write it to PlayerxFramePage
  ret

SetFrameTossP1:               ;player 1 should toss 1 frame earlier than player 2 (because player 2 will get an "action" the frame he gets tossed)
  ld    (bc),a                ;PxRightIdleFrame
  inc   bc
  inc   bc
  ld    a,1
  ld    (bc),a                ;PxRightIdle current animationspeed step
  jp    QuickSetFrame

SetFrame:
  ld    (bc),a                ;PxRightIdleFrame
  inc   bc
  inc   bc
  xor   a
  ld    (bc),a                ;PxRightIdle current animationspeed step
  QuickSetFrame:              ;in: de->frame, hl->Player12Frame, changes: a,de,hl
  ;set frame
  ld    a,(de)                ;fetch current Idle frame
  ld    (hl),a                ;and write it to PlayerxFrame
  inc   de
  inc   hl
  ld    a,(de)                ;fetch current Idle frame
  ld    (hl),a                ;and write it to PlayerxFrame
  ;set framepage
  inc   de                    
  inc   hl
  ld    a,(de)                ;fetch current Idle frame
  ld    (hl),a                ;and write it to PlayerxFrame
  ret

SetJumpFrame:                 ;sets PxStaticDirection, resets jumptable pointer and SetsFrame
SetHitFrame:
  ld    (ix),a                ;sets PxStaticDirection
  xor   a
  ld    (bc),a                ;reset PxHitAnimationStep
  jp    QuickSetFrame         ;in: de->frame, hl->Player12Frame, changes: a,de,hl

SetGettingTossedFrame:
SetJumpHitFrame:
SetHeavilyHitFrame:
  ld    (ix),a                ;sets PxStaticDirection
  xor   a
  ld    (bc),a                ;reset PxHeavilyHitAnimationStep
  ld    (iy),a                ;reset PxHeavilyHitLeft/RightFrame
  ld    (iy+2),a              ;reset PxHeavilyHitLeft/RightFrame
  jp    QuickSetFrame         ;in: de->frame, hl->Player12Frame, changes: a,de,hl

SetSpecialFrame:
  xor   a
  ld    (bc),a                ;reset PxSpecial1AnimationStep
  ld    (iy),a                ;reset P1Special1MovementTablePointer
  inc   bc
  inc   bc
  ld    (bc),a                ;reset current speed step
  jp    QuickSetFrame         ;in: de->frame, hl->Player12Frame, changes: a,de,hl

SetStaticDirection:           ;out: c=look left, z=player 1, changes af, b, hl
  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  jp    c,.FacingLeft

  .FacingRight:
  ld    a,+1                  ;P1 looking right
  ld    hl,P1StaticDirection
  jp    z,.DoSet
  ld    hl,P2StaticDirection
  jp    .DoSet
  .FacingLeft:
  ld    a,-1                  ;P1 looking left
  ld    hl,P1StaticDirection
  jp    z,.DoSet
  ld    hl,P2StaticDirection
  .DoSet:
  ld    (hl),a
  ret
  
AnimateProjectile:            ;animates, forces writing spriteframe, out: z=animation ended
AnimatePlayer:                ;animates, forces writing spriteframe, out: z=animation ended
;check speed of animation
  ld    a,(ix+3)              ;PxLeftIdleAnimationSpeed+1
  ld    b,a
  ld    a,(ix+2)              ;P1LeftIdleAnimationSpeed+0
  inc   a
  cp    b                     ;overflow check
  jr    nz,.setanimationspeed

  ld    a,(ix+4)              ;P1LeftIdleAnimationSpeed+2
  or    a                     ;should animation speed fluctuate ?
  jp    z,.endcheckfluctuate
  ld    b,a
  neg
  ld    (ix+4),a              ;P1LeftIdleAnimationSpeed+2
  ld    a,(ix+3)              ;PxLeftIdleAnimationSpeed+1
  add   a,b
  ld    (ix+3),a              ;PxLeftIdleAnimationSpeed+1
  .endcheckfluctuate:
  xor   a
  .setanimationspeed:
  ld    (ix+2),a              ;P1LeftIdleAnimationSpeed+0
  jr    nz,.endchangespriteframe
;/check speed of animation

;change/animate sprite
  ld    a,(ix+1)              ;P1LeftIdleFrame+1
  ld    b,a
  ld    a,(ix+0)              ;P1LeftIdleFrame+0
  inc   a
  cp    b                     ;overflow check
  jr    nz,.setstep
  xor   a
  .setstep:
  ld    (ix+0),a              ;P1LeftIdleFrame+0
  .endchangespriteframe:  
;/change/animate sprite  

  ld    a,(ix+0)              ;P1LeftIdleFrame+0
  ld    b,a
  add   a,a
  add   a,b                   ;*3 to fetch frame in table
  ld    b,0
  ld    c,a
  add   ix,bc
  
  ld    a,(ix+7)              ;framepage
  ld    (iy+2),a              ;write to framepage

  ld    a,(ix+5)              ;fetch current Idle frame
  ld    (iy+0),a              ;and write it to PlayerxFrame
  ld    a,(ix+6)
  ld    (iy+1),a

  ld    a,(ix+0)              ;Check if animation ended
  or    (ix+2)
  ret

AnimateAndHandleJump:
  ;check if spriteframe needs to change / animate
  ld    a,(de)                ;jump animation step
  cp    (hl)                  ;jump table pointer
  jr    z,.animate
  inc   de | inc de | inc de | inc de
  ld    a,(de)                ;jump animation step
  cp    (hl)                  ;jump table pointer
  jr    z,.animate
  inc   de | inc de | inc de | inc de
  ld    a,(de)                ;jump animation step
  cp    (hl)                  ;jump table pointer
  jr    z,.animate
  inc   de | inc de | inc de | inc de

  ld    a,(de)                ;jump animation step
  cp    (hl)                  ;jump table pointer
  jr    z,.animate
  inc   de | inc de | inc de | inc de
  ld    a,(de)                ;jump animation step
  cp    (hl)                  ;jump table pointer
  jr    z,.animate
  inc   de | inc de | inc de | inc de
  ld    a,(de)                ;jump animation step
  cp    (hl)                  ;jump table pointer
  jr    z,.animate
  inc   de | inc de | inc de | inc de


  ld    a,(de)                ;jump animation step
  cp    (hl)                  ;jump table pointer
  jr    nz,.endanimate

  .animate:
  inc   de

  push  bc
  push  hl                    ;Now search player, sets Frame in de to PlayerxFrame(&Backup)
  call  SearchPlayerSetframeAndBackup  
  pop   hl
  pop   bc
  .endanimate:  
  ;/check if spriteframe needs to change / animate

  ;move player vertically
  inc   (hl)                  ;increase jump table pointer
  ld    d,0
  ld    e,(hl)
  add   hl,de
  ld    e,(hl)                ;vertical movement in a

  ld    a,e
  cp    128
  jp    z,.endJump

  ld    a,(bc)                ;player y
  add   a,e
  ld    (bc),a
  ;/move player vertically
  
  cp    PlayeryAtstart        ;player landed ?
  ret   nz

  ;set sfx 4 frames before jump officially ends
  inc   hl
  inc   hl
  inc   hl
  inc   hl
  ld    a,(hl)
  cp    128
  ld    c,sfxLanding
  call  z,SetSfx
  ;/set sfx 4 frames before jump officially ends

  call  TotalResetAirAttack   ;Resets: already attacked once this jump 
  ;Now Switch Back to SpriteFrame of Jumping
                              ;        & Currently attacking in air
  call  CheckControlsinBC     ;out a->controls, b=x move, c=y move, changes: af, bc
  dec   c                     ;down pressed ?
  jp    z,SetBend
  inc   c                     ;not down nor up pressed ?
  ret   z
  ;at this point up is pressed, so jump again
  dec   b
  jp    z,SetjumpRight
  inc   b
  jp    z,SetjumpStraightUp   ;if left NOR right is pressed, then jump 
  jp    SetjumpLeft

  .endJump:  
  call  CheckControlsinBC     ;out a->controls, b=x move, c=y move, changes: af, bc
  dec   b                     ;right pressed ?
  jp    z,SetWalkRight
  inc   b                     ;not right nor left pressed ?
  jp    z,SetIdle
  jp    SetWalkLeft

CheckAttackWhileJump:         ;check if player pressed an attack button
  ;Dont Check jump Controls Attack if player is on the floor
  ld    a,(HandleWhichPlayer?)
  dec   a
  ld    a,(player1y)
  jr    z,.CheckendJump
  ld    a,(player2y)
  .CheckendJump:
  cp    PlayeryAtstart        ;player y postion on the floor ?
  ret   z
  ;/Dont Check jump Controls Attack if player is on the floor

  ;Dont Check jump Controls Attack if player has already attacked this jump
  ld    a,(HandleWhichPlayer?)
  dec   a
  ld    hl,P1AttackingInAir?+1;already attacked once this jump?
  jr    z,.CheckAttack
  ld    hl,P2AttackingInAir?+1;already attacked once this jump?
  .CheckAttack:
  
  ld    a,(hl)                ;already attacked once this jump?
  or    a
  ret   nz                    ;can only attack once each jump
  ;/Dont Check jump Controls Attack if player has already attacked this jump
  ;
  ; bit 7     6     5     4     3     2     1     0
  ;		  softk softp hardk hardp right left  down  up  (action)
  ;		  /     .     '     ;     right left  down  up  (ControlsP1)
  ;		  S     A     W     Q     right left  down  up  (ControlsP2)
  ;
  call  GetNewPrControls      ;out a-> NewPrcontrols, Changes: af
  bit   6,a
  jp    nz,SetSoftJumpPunch
  bit   4,a
  jp    nz,SetHardJumpPunch
  bit   7,a
  jp    nz,SetSoftJumpKick
  bit   5,a
  jp    nz,SetHardJumpKick
  ret

switchplayer:
  ld    a,(HandleWhichPlayer?)
  dec   a
  xor   1
  inc   a
  ld    (HandleWhichPlayer?),a
  ret

CheckJumpStraight:            ;out zero=Jump Straight up, changes: af
  ld    a,(HandleWhichPlayer?)
  dec   a
  ld    a,(Player1JumpDirection)
  jr    z,.JumpDirectionFound
  ld    a,(Player2JumpDirection)
  .JumpDirectionFound:
  or    a
  ret

HandleJump:
  ld    a,(HandleWhichPlayer?)
  dec   a
  ld    a,(Player1JumpDirection)
  jr    z,.JumpDirectionFound
  ld    a,(Player2JumpDirection)
  .JumpDirectionFound:

  or    a
  jp    z,JumpStraightUp.handleJump
  dec   a                     ;jump right ?
  jp    z,JumpRight.handleJump
  jp    JumpLeft.handleJump

TotalResetAirAttack:          ;Resets: already attacked once this jump
  ld    a,(HandleWhichPlayer?);& Currently attacking in air
  dec   a
  ld    hl,P1AttackingInAir?+1;already attacked once this jump?
  jr    z,.CheckAttack
  ld    hl,P2AttackingInAir?+1;already attacked once this jump?
  .CheckAttack:
  ld    (hl),0                ;set already attacked once this jump?
  dec   hl
  ld    (hl),0                ;Currently attacking in air?
  ret

ResetCurrentAirAttack:        ;Resets: Currently attacking in air
  ld    a,(HandleWhichPlayer?)
  dec   a
  ld    hl,P1AttackingInAir?  ;Currently attacking in air?
  jr    z,.CheckAttack
  ld    hl,P2AttackingInAir?  ;Currently attacking in air?
  .CheckAttack:
  ld    (hl),0                ;Currently attacking in air?
  ret

HardSitKickAction:
  ld    a,(ix+20)             ;some character move horizontally while performing hard sit kick action
  or    a
  jp    z,.endmove
  ld    c,a

  ld    a,(ix+00)             ;only move at spriteframe 0 or 1
  cp    2
  jp    nc,.endmove

  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af, b
  ld    a,c
  jr    nc,.move
  neg
  .move:
  add   a,(hl)
  ld    (hl),a

  .endmove:
  call  AnimatePlayer         ;animates, forces writing spriteframe, out: z=animation ended
  ret   nz  
  jp    SetBend
  ret

StandardAttackActionSit:
  call  AnimatePlayer         ;animates, forces writing spriteframe, out: z=animation ended
  ret   nz  
  jp    SetBend 

StandardAttackActionStand:
  call  AnimatePlayer         ;animates, forces writing spriteframe, out: z=animation ended
  ret   nz  
  jp    SetIdle

StandardAttackActionJumping:
  call  AnimatePlayer         ;animates, forces writing spriteframe, out: z=animation ended
  jp    z,.EndAirAttack       ;at end of airattack animation end airattack

  ;Also end air attack if player is on the floor
  ld    a,(HandleWhichPlayer?)
  dec   a
  ld    a,(player1y)
  jr    z,.CheckendJump
  ld    a,(player2y)
  .CheckendJump:
  cp    PlayeryAtstart        ;player y postion on the floor ?
  ret   nz
  ;Also end air attack if player is on the floor

  .EndAirAttack:
  ld    b,3                   ;Action=3	Jump
  call  WriteplayerAction     ;in b: Action, Searches player and sets Action, Changes: af
  call  ResetCurrentAirAttack ;Resets: Currently attacking in air
  jp    ResetBackedupFrame    ;Now Switch Back to SpriteFrame of Jumping before attack

StandardKnockDownRecoverAction:
  or    a
  jr    z,.EndCheckPlayerDies  
  cp    27
  ret   c                     ;freeze 6 frames before showing dropped down frame
  push  iy                    ;contains players frame
  pop   hl
  jp    QuickSetFrame         ;in: de->frame, hl->Player12Frame, changes: a,de,hl

  .EndCheckPlayerDies:
  call  AnimatePlayer         ;animates, forces writing spriteframe, out: z=animation ended
  ret   nz  

;  call  CheckEnemyAttacking   ;out-> c:enemy is attacking, changes: af
;  jp    nc,SetIdle
  ;if enemy is attacking you have a chance to defend
  call  CheckControlsinBC     ;out a->controls, b=x move, c=y move, changes: af, bc
  dec   c                     ;down pressed ?
  jp    z,SetBendDefend
  inc   b                     ;left pressed ?
  jp    z,SetStandDefend
  dec   b
  jp    z,SetIdle
  jp    SetStandDefend

;Action=22 KnockDownRecover
;Action=23 StandDefendHit
;Action=24 BendDefendHit

StandardTossAction:
  call  AnimatePlayer         ;animates, forces writing spriteframe, out: z=animation ended
  ret   nz  
  jp    SetIdle
  
ResetBackedupFrame:  
  ;Now Switch Back to SpriteFrame of Jumping before attack
  ld    a,(HandleWhichPlayer?)
  dec   a
  ld    hl,Player1Frame
  ld    de,Player1FrameBackup
  jp    z,QuickSetFrame
  ld    hl,Player2Frame
  ld    de,Player2FrameBackup
  jp    QuickSetFrame

CheckAttackOnTopOfSoftAttackkick:
;
; bit 7     6     5     4     3     2     1     0
;		  softk softp hardk hardp right left  down  up  (action)
;		  /     .     '     ;     right left  down  up  (ControlsP1)
;		  S     A     W     Q     right left  down  up  (ControlsP2)
;
  call  CheckControlsinBC     ;out a->controls, b=x move, c=y move, changes: af, bc
  inc   c
  ret   z                     ;up pressed, dont check for further attack
  call  GetNewPrControls      ;out a-> NewPrcontrols, Changes: af
  dec   c
  jp    nz,.DownPressed

  .NotupNordownPressed:
  bit   7,a
  jp    nz,SetSoftStandKick
  ret
  .DownPressed:
  bit   7,a
  jp    nz,SetSoftSitKick
  ret
  
CheckAttackOnTopOfSoftAttackPunch:
;
; bit 7     6     5     4     3     2     1     0
;		  softk softp hardk hardp right left  down  up  (action)
;		  /     .     '     ;     right left  down  up  (ControlsP1)
;		  S     A     W     Q     right left  down  up  (ControlsP2)
;
  call  CheckControlsinBC     ;out a->controls, b=x move, c=y move, changes: af, bc
  inc   c
  ret   z                     ;up pressed, dont check for further attack
  call  GetNewPrControls      ;out a-> NewPrcontrols, Changes: af
  dec   c
  jp    nz,.DownPressed
  .NotupNordownPressed:
  bit   6,a
  jp    nz,SetSoftStandPunch
  ret
  .DownPressed:
  bit   6,a
  jp    nz,SetSoftSitPunch
  ret

CheckEnemyAttacking:          ;out-> c:enemy is attacking, changes: af
  ;first check if both players are looking in the same direction, because If they do, then dont do CheckEnemyAttacking, cuz no need to defend 
  ld    a,(Player1FramePage)  ;0 or 2 are looking right, 1 or 3 are looking left
  and   1                     ;0 = looking left, 1=looking right
  ld    b,a
  ld    a,(Player2FramePage)  ;0 or 2 are looking right, 1 or 3 are looking left
  and   1                     ;0 = looking left, 1=looking right
  add   a,b                   ;0 = both looking left, 1 = facing each other, 2= both looking right
  dec   a                     ;cp 1
  ret   nz
  .endCheckFacingSameDirection:

  ;Check Enemy Action
  ld    a,(HandleWhichPlayer?)
  dec   a
  ld    a,(player2Action)  
  jr    z,.EnemyFound
  ld    a,(player1Action)  
  .EnemyFound:
  
  cp    4                     ;0=idle 1=bend 2=walk 3=jump
  jp    c,.Setnotattacking
  cp    16                    ;4 - 15 = attacking action
  ret   c                     ;carry enemy is attacking
  cp    25                    ;16 - 24 = not attacking
  jp    c,.Setnotattacking
  cp    30                    ;25 - 29 = special attack
  ret   c

  .Setnotattacking:           ;reset carry, enemy is not attacking
  ;check projectile in screen
  ld    a,(HandleWhichPlayer?)
  dec   a
  ld    a,(P2ProjectileInScreen?)  
  jr    z,.EnemyFound2
  ld    a,(P1ProjectileInScreen?)  
  .EnemyFound2:
  or    a
  ret   z
  scf                         ;carry= enemy is attacking
  ret

StaDirectionAndPlayer:        ;out: c=Look left, z=player 1, changes af
  ld    a,(HandleWhichPlayer?)
  dec   a
  ld    a,(P1StaticDirection) ;-1 facing left,  +1 facing right
  jr    z,.GoCheckDirection
  ld    a,(P2StaticDirection) ;-1 facing left,  +1 facing right
  .GoCheckDirection:
  add   a,2                   ;to generate carry when looking left
  ld    a,(HandleWhichPlayer?);zero flag: player 1
  dec   a                     
  ret

DirectionAndPlayer:           ;out: c=Look left, z=player 1, changes af, b
  ld    a,(HandleWhichPlayer?);if current player is player 1, then if carry
  dec   a                     ;player 1 is right of player 2, so player 1 
  ld    a,(player1x)          ;should look left
  ld    b,a
  ld    a,(player2x)          ;if current player is player 2, then if carry
  jr    z,.GoCheckDirection   ;player 2 is right of player 1, so player 2  
  ld    a,(player2x)          ;should look left
  ld    b,a
  ld    a,(player1x)
  .GoCheckDirection:
  sub   a,b
  ld    a,(HandleWhichPlayer?);zero flag: player 1
  dec   a                     
  ret
  
GetControls:                  ;out a-> controls, Changes: af
  ld    a,(HandleWhichPlayer?)
  dec   a
  ld    a,(ControlsP1)
  ret   z
  ld    a,(ControlsP2)
  ret

GetNewPrControls:             ;out a-> NewPrcontrols, Changes: af
  ld    a,(HandleWhichPlayer?)
  dec   a
  ld    a,(NewPrControlsP1)
  ret   z
  ld    a,(NewPrControlsP2)
  ret

CheckControlsinBC:            ;out a->controls, b=x move, c=y move, changes: af, bc
;
; bit	7	  6	  5		    4		    3		    2		  1		  0
;		  0	  0	  trig-b	trig-a	right	  left	down	up	(joystick)
;		  F5	F1	'M'		  space	  right	  left	down	up	(keyboard)
;
  ld    bc,0                  ;b=left(-1), right(1), c=up(-1), down(1)
  call  GetControls           ;out a-> controls, Changes: af  
  bit   0,a                   ;up pressed ?
  jp    z,.endcheckuppressed
  dec   c
.endcheckuppressed:
  bit   1,a                   ;down pressed ?
  jp    z,.endcheckdownpressed
  inc   c
.endcheckdownpressed:
  bit   2,a                   ;left pressed ?
  jp    z,.endcheckleftpressed
  dec   b
.endcheckleftpressed:
  bit   3,a                   ;right pressed ?
  ret   z
  inc   b
  ret

WriteplayerAction:            ;in b: Action, Searches player and sets Action, Changes: af
  ld    a,(HandleWhichPlayer?)
  dec   a
  ld    a,b
  jr    z,.player1
  .player2:
  ld    (player2Action),a
  ret
  .player1:
  ld    (player1Action),a
  ret

StoredAnimationStep:  ds  1
GeneralMovement:              ;in: hl->step, bc->(y,x)table-2, de->player12y | Moves Player, out: nc=landed
  .lookright:
  inc   (hl)                  
  inc   (hl)                  ;increase animation step by 2 (y,x)

  ld    a,(hl)
  ld    (StoredAnimationStep),a

  ld    l,(hl)
  ld    h,0
  add   hl,bc                 ;go to movement y
  ld    b,(hl)                ;y movement
  ld    a,(de)                ;player y
  add   a,b
  cp    PlayeryAtstart+1
  jr    c,.Notlanded1
  cp    PlayeryAtstart+20
  jp    c,.landed
  .Notlanded1:
  ld    (de),a
  
  inc   hl                    ;go to movement x
  inc   de                    ;player x

  ld    a,(hl)                ;x movement
  or    a
  jp    p,.moveright
  jp    .moveleft

  .lookleft:
  inc   (hl)                  
  inc   (hl)                  ;increase animation step by 2 (y,x)

  ld    a,(hl)
  ld    (StoredAnimationStep),a

  ld    l,(hl)
  ld    h,0
  add   hl,bc                 ;go to movement y
  ld    b,(hl)                ;y movement
  ld    a,(de)                ;player y
  add   a,b
  cp    PlayeryAtstart+1
  jr    c,.Notlanded2
  cp    PlayeryAtstart+20
  jp    c,.landed
  .Notlanded2:

  ld    (de),a
  
  inc   hl                    ;go to movement x
  inc   de                    ;player x

  ld    a,(hl)                ;x movement
  neg
  jp    p,.moveright

  .moveleft:
  neg
  ld    b,a                   ;store movement
  ld    a,(de)                ;playerx
  sub   a,b                   ;add movement
  jr    c,.borderreachedleft  ;for when player gets tossed
  cp    ScreenLimitxLeft
  jr    c,.borderreachedleft
  ld    (de),a
  scf
  ret

  .moveright:
  ld    b,a                   ;store movement
  ld    a,(de)                ;playerx
  add   a,b                   ;add movement
  jr    c,.borderreachedright ;for when player gets tossed
  cp    ScreenLimitxRight
  jr    nc,.borderreachedright
  ld    (de),a
  scf
  ret

  .borderreachedleft:         ;if a border is reached while beind hit, then move opponent backwards instead
  ld    a,(StoredAnimationStep)
  cp    28
  jp    nc,.end1
  cp    20
  ret   c  

  ld    a,(freezecontrols?)   ;dont move opponent backwards when controls are frozen (meaning a player died)
  or    a
  jr    nz,.end1

  ld    a,(player1Action)     ;dont move opponent backwards in a toss situation
  cp    30                    ;GettingTossed
  jr    z,.end1
  ld    a,(player2Action)
  cp    30
  jr    z,.end1

  ld    a,b
  add   a,a
  add   a,b                   ;*3
  add   a,(iy)                ;opponent's x
  ret   c
  cp    MoveBackXBorder
  jr    nc,.end1
  ld    (iy),a
  .end1:
  scf
  ret

  .borderreachedright:        ;if a border is reached while beind hit, then move opponent backwards instead
  ld    a,(StoredAnimationStep)
  cp    28
  jp    nc,.end2
  cp    20
  ret   c  

  ld    a,(freezecontrols?)   ;dont move opponent backwards when controls are frozen (meaning a player died)
  or    a
  jr    nz,.end2

  ld    a,(player1Action)     ;dont move opponent backwards in a toss situation
  cp    30                    ;GettingTossed
  jr    z,.end2
  ld    a,(player2Action)
  cp    30
  jr    z,.end2

  ld    a,b
  add   a,a
  add   a,b                   ;*3
  ld    b,a

  ld    a,(iy)                ;opponent's x
  sub   a,b
  ret   c
  cp    256-MoveBackXBorder
  ret   c
  ld    (iy),a
  .end2:
  scf
  ret
  
.landed:
  ld    a,PlayeryAtstart
  ld    (de),a
  xor   a                     ;nc=landed
  ret

AnimateSpecial1:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af, b
  ld    iy,Player1Frame
  jp    c,.Left

  .Right:
  ld    ix,P1Special1RightFrame
  jp    z,AnimatePlayer
  ld    ix,P2Special1RightFrame
  ld    iy,Player2Frame
  jp    AnimatePlayer
  .Left:
  ld    ix,P1Special1LeftFrame
  jp    z,AnimatePlayer
  ld    ix,P2Special1LeftFrame
  ld    iy,Player2Frame
  jp    AnimatePlayer

AnimateSpecial2:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af, b
  ld    iy,Player1Frame
  jp    c,.Left

  .Right:
  ld    ix,P1Special2RightFrame
  jp    z,AnimatePlayer
  ld    ix,P2Special2RightFrame
  ld    iy,Player2Frame
  jp    AnimatePlayer
  .Left:
  ld    ix,P1Special2LeftFrame
  jp    z,AnimatePlayer
  ld    ix,P2Special2LeftFrame
  ld    iy,Player2Frame
  jp    AnimatePlayer
  
AnimateSpecial3:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af, b
  ld    iy,Player1Frame
  jp    c,.Left

  .Right:
  ld    ix,P1Special3RightFrame
  jp    z,AnimatePlayer
  ld    ix,P2Special3RightFrame
  ld    iy,Player2Frame
  jp    AnimatePlayer
  .Left:
  ld    ix,P1Special3LeftFrame
  jp    z,AnimatePlayer
  ld    ix,P2Special3LeftFrame
  ld    iy,Player2Frame
  jp    AnimatePlayer  

AnimateSpecial4:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af, b
  ld    iy,Player1Frame
  jp    c,.Left

  .Right:
  ld    ix,P1Special4RightFrame
  jp    z,AnimatePlayer
  ld    ix,P2Special4RightFrame
  ld    iy,Player2Frame
  jp    AnimatePlayer
  .Left:
  ld    ix,P1Special4LeftFrame
  jp    z,AnimatePlayer
  ld    ix,P2Special4LeftFrame
  ld    iy,Player2Frame
  jp    AnimatePlayer  

AnimateSpecial5:
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af, b
  ld    iy,Player1Frame
  jp    c,.Left

  .Right:
  ld    ix,P1Special5RightFrame
  jp    z,AnimatePlayer
  ld    ix,P2Special5RightFrame
  ld    iy,Player2Frame
  jp    AnimatePlayer
  .Left:
  ld    ix,P1Special5LeftFrame
  jp    z,AnimatePlayer
  ld    ix,P2Special5LeftFrame
  ld    iy,Player2Frame
  jp    AnimatePlayer    

Special1Movement:           
  ld    iy,emptyVar           ;this is to make sure nothing strange gets written to the standard movement of special moves
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af
  jp    z,.player1
  .player2:
  ld    hl,P2Special1MovementTablePointer
  ld    bc,P2Special1MovementTablePointer
  ld    de,player2y           ;in: hl->step, bc->(y,x)table-2, de->player12y | Moves Player, out: nc=landed
  jp    c,GeneralMovement.lookleft
  jp    GeneralMovement.lookright
  .player1:
  ld    hl,P1Special1MovementTablePointer
  ld    bc,P1Special1MovementTablePointer
  ld    de,player1y
  jp    c,GeneralMovement.lookleft
  jp    GeneralMovement.lookright

Special2Movement:           
  ld    iy,emptyVar           ;this is to make sure nothing strange gets written to the standard movement of special moves
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af
  jp    z,.player1
  .player2:
  ld    hl,P2Special2MovementTablePointer
  ld    bc,P2Special2MovementTablePointer
  ld    de,player2y           ;in: hl->step, bc->(y,x)table-2, de->player12y | Moves Player, out: nc=landed
  jp    c,GeneralMovement.lookleft
  jp    GeneralMovement.lookright
  .player1:
  ld    hl,P1Special2MovementTablePointer
  ld    bc,P1Special2MovementTablePointer
  ld    de,player1y
  jp    c,GeneralMovement.lookleft
  jp    GeneralMovement.lookright

Special3Movement:           
  ld    iy,emptyVar           ;this is to make sure nothing strange gets written to the standard movement of special moves
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af
  jp    z,.player1
  .player2:
  ld    hl,P2Special3MovementTablePointer
  ld    bc,P2Special3MovementTablePointer
  ld    de,player2y           ;in: hl->step, bc->(y,x)table-2, de->player12y | Moves Player, out: nc=landed
  jp    c,GeneralMovement.lookleft
  jp    GeneralMovement.lookright
  .player1:
  ld    hl,P1Special3MovementTablePointer
  ld    bc,P1Special3MovementTablePointer
  ld    de,player1y
  jp    c,GeneralMovement.lookleft
  jp    GeneralMovement.lookright

Special4Movement:           
  ld    iy,emptyVar           ;this is to make sure nothing strange gets written to the standard movement of special moves
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af
  jp    z,.player1
  .player2:
  ld    hl,P2Special4MovementTablePointer
  ld    bc,P2Special4MovementTablePointer
  ld    de,player2y           ;in: hl->step, bc->(y,x)table-2, de->player12y | Moves Player, out: nc=landed
  jp    c,GeneralMovement.lookleft
  jp    GeneralMovement.lookright
  .player1:
  ld    hl,P1Special4MovementTablePointer
  ld    bc,P1Special4MovementTablePointer
  ld    de,player1y
  jp    c,GeneralMovement.lookleft
  jp    GeneralMovement.lookright

Special5Movement:           
  ld    iy,emptyVar           ;this is to make sure nothing strange gets written to the standard movement of special moves
  call  StaDirectionAndPlayer ;out: c=Look left, z=player 1, changes af
  jp    z,.player1
  .player2:
  ld    hl,P2Special5MovementTablePointer
  ld    bc,P2Special5MovementTablePointer
  ld    de,player2y           ;in: hl->step, bc->(y,x)table-2, de->player12y | Moves Player, out: nc=landed
  jp    c,GeneralMovement.lookleft
  jp    GeneralMovement.lookright
  .player1:
  ld    hl,P1Special5MovementTablePointer
  ld    bc,P1Special5MovementTablePointer
  ld    de,player1y
  jp    c,GeneralMovement.lookleft
  jp    GeneralMovement.lookright

SetCoordinatesPlayersWhenTossingRight:
  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  jr    z,TossRightHandleP1

  TossRightHandleP2:          ;player 2 is tossing
  ld    a,+1                  ;when tossing right, static direction is right
  ld    (P2StaticDirection),a

  ld    b,10
  jp    c,.DirectionFound
  ld    b,24
  .DirectionFound:            ;P2 is standing left of P1 and throws him right
  ld    a,(player2x)
  add   a,b
  jr    c,.overflowright
  cp    ScreenLimitxRight  
  jr    c,.nooverflowright
  .overflowright:
  ld    a,240-ScreenLimitxLeft-8
  .nooverflowright:
  ld    (player2x),a
  sub   a,18
  jr    c,.overflowleft
  cp    ScreenLimitxLeft
  jr    nc,.nooverflowleft
  .overflowleft:
  ld    a,18+ScreenLimitxLeft
  ld    (player2x),a
  ld    a,ScreenLimitxLeft
  .nooverflowleft:
  ld    (player1x),a

  ld    a,(player1y)
  sub   a,22
  ld    (player1y),a
  ret

  TossRightHandleP1:          ;player 1 is tossing
  ld    a,+1                  ;when tossing right, static direction is right
  ld    (P1StaticDirection),a

  ld    b,10
  jp    c,.DirectionFound
  ld    b,24
  .DirectionFound:            ;P1 is standing left of P2 and throws him right
  ld    a,(player1x)
  add   a,b
  jr    c,.overflowright
  cp    ScreenLimitxRight  
  jr    c,.nooverflowright
  .overflowright:
  ld    a,240-ScreenLimitxLeft-8
  .nooverflowright:
  ld    (player1x),a
  sub   a,18
  jr    c,.overflowleft
  cp    ScreenLimitxLeft
  jr    nc,.nooverflowleft
  .overflowleft:
  ld    a,18+ScreenLimitxLeft
  ld    (player1x),a
  ld    a,ScreenLimitxLeft
  .nooverflowleft:
  ld    (player2x),a

  ld    a,(player2y)
  sub   a,22
  ld    (player2y),a
  ret

SetCoordinatesPlayersWhenTossingLeft:
  call  DirectionAndPlayer    ;out: c=Look left, z=player 1, changes af, b
  jr    z,TossLeftHandleP1

  TossLeftHandleP2:           ;player 2 is tossing
  ld    a,-1                  ;when tossing left, static direction is left
  ld    (P2StaticDirection),a

  ld    b,24
  jp    c,.DirectionFound
  ld    b,10
  .DirectionFound:            ;P2 is standing left of P1 and throws him left
  ld    a,(player2x)
  sub   a,b
  jr    c,.overflowleft
  cp    ScreenLimitxLeft+8
  jr    nc,.nooverflowleft
  .overflowleft:
  ld    a,18+ScreenLimitxLeft
  .nooverflowleft:
  ld    (player2x),a
  add   a,18
  jr    c,.overflowright
  cp    ScreenLimitxRight    
  jr    c,.nooverflowright
  .overflowright:
  ld    a,254-18-ScreenLimitxLeft
  ld    (player2x),a
  add   a,18
  .nooverflowright:
  ld    (player1x),a

  ld    a,(player1y)
  sub   a,22
  ld    (player1y),a
  ret

  TossLeftHandleP1:           ;player 1 is tossing
  ld    a,-1                  ;when tossing left, static direction is left
  ld    (P1StaticDirection),a

  ld    b,24
  jp    c,.DirectionFound
  ld    b,10
  .DirectionFound:            ;P2 is standing left of P1 and throws him left
  ld    a,(player1x)
  sub   a,b
  jr    c,.overflowleft
  cp    ScreenLimitxLeft+8
  jr    nc,.nooverflowleft
  .overflowleft:
  ld    a,18+ScreenLimitxLeft
  .nooverflowleft:
  ld    (player1x),a
  add   a,18
  jr    c,.overflowright
  cp    ScreenLimitxRight    
  jr    c,.nooverflowright
  .overflowright:
  ld    a,254-18-ScreenLimitxLeft
  ld    (player1x),a
  add   a,18
  .nooverflowright:
  ld    (player2x),a

  ld    a,(player2y)
  sub   a,22
  ld    (player2y),a
  ret

checkPlayersNear:             ;out: c=players are near to each other
  ld    a,(player1x)
  ld    b,a
  ld    a,(player2x)
  sub   a,b
  jp    nc,.positive
  neg
  .positive:
  cp    28
  ret

CheckPossibleToss:            ;out:  c=toss allowed
;dont toss if opponent is Jumping, Jumpattacking, Jumphit, Heavilyhit, KnockdownRecover, Special ???
  ld    a,(HandleWhichPlayer?)
  dec   a
  ld    a,(player2Action)
  ld    de,P1TossCooldown?
  ld    hl,player2y
  jr    z,.go
  ld    a,(player1Action)
  ld    de,P2TossCooldown?
  ld    hl,player1y
  .go:
  cp    3
  jr    z,.TossNotAllowed     ;toss not allowed when opponent is jumping
  cp    12
  jr    c,.TossAllowed        ;action 0-11 (except 3) toss allowed
  cp    16
  jr    c,.TossNotAllowed     ;action 12-15 toss not allowed
  cp    20
  jr    c,.TossAllowed        ;action 16-19 toss allowed
  cp    23
  jr    c,.TossNotAllowed     ;action 20-22 toss not allowed
  ;at this point opponent is performing a special move, only toss if he/she is on the ground
  ld    a,(hl)                ;is opponent in the air ?
  cp    PlayeryAtstart
  jp    nz,.TossNotAllowed

  .TossAllowed:
  ld    a,(de)                ;check toss cooldown, dont toss if still on cooldown
  or    a
  ret   nz
  
  scf                         ;c=toss allowed
  ret

  .TossNotAllowed:
  xor   a                     ;z=no toss allowed
  ret

QuickDrainHp:                 ;out: c=player died, in: b=amount of hp to drain. Changes: af,de,ix
  ld    a,(HandleWhichPlayer?)
  dec   a
  ld    de,Player1Hp
  ld    ix,P1Dies?
  jr    z,DrainHp.QuickDrainHp
  ld    de,Player2Hp
  ld    ix,P2Dies?
  .PlayerFound:
  jp    DrainHp.QuickDrainHp

DrainHp:                      ;out: c=player died, changes af,bc,de,hl,ix
  ld    a,(HandleWhichPlayer?)
  dec   a
  ld    a,(player2Action)
  ld    de,Player1Hp
  ld    ix,P1Dies?
  ld    hl,P2DamageTabel
  jr    z,.PlayerFound
  ld    a,(player1Action)
  ld    de,Player2Hp
  ld    ix,P2Dies?
  ld    hl,P1DamageTabel
  .PlayerFound:
  cp    16                    ;check if attacking player is in Action 4 - 15
  jp    c,.firstgroupfound
  
  sub   a,9                   ;remove Stand/BendDefend, Stand/Bendhit, JumpHit, HeavilyHit, KnockDownRecover, StandDefendHit, BendDefendHit
  .firstgroupfound:
  sub   a,4                   ;remove Idle, Bend, Walk, Jump
  ld    b,0
  ld    c,a
  add   hl,bc                 ;damage of action in damagetable
  ld    b,(hl)
  .QuickDrainHp:
  ld    a,(de)
  ;ld b,200
  ;ld b,1
  sub   a,b
  jp    nc,.set  

  ld    (ix),1                ;player dies

  call  FreezeControls

  scf                         ;c=player died
  .set:
  ld    (de),a  
  ret

FreezeControls:
  ld    a,1
  ld    (freezecontrols?),a
  xor   a
  ld    (ControlsP1),a
  ld    (NewPrControlsP1),a
  ld    (ControlsP2),a
  ld    (NewPrControlsP2),a
  ret

SfxWhenHitTable:  db sfxSoftPunchHit, sfxHardPunchHit, sfxSoftKickHit, sfxHardKickHit, sfxSoftPunchHit, sfxHardPunchHit, sfxSoftKickHit, sfxHardKickHit, sfxSoftPunchHit, sfxHardPunchHit, sfxSoftKickHit, sfxHardKickHit
SetSfxWhenHit:
  ld    a,(HandleWhichPlayer?)
  dec   a
  ld    hl,Player2Male?
  ld    a,(player2Action)
  jr    z,.PlayerFound
  ld    hl,Player1Male?
  ld    a,(player1Action)
  .PlayerFound:
  cp    30                    ;being tossed ?
  ld    c,sfxMalethrow
  jp    z,.SetTossSfx
  cp    25                    ;hit by special attack ?
  ld    c,sfxHardKickHit
  jp    nc,SetSfx

  sub   4                     ;go to action 4 = softstandpunch
  ld    hl,SfxWhenHitTable
  ld    d,0
  ld    e,a
  add   hl,de
  ld    c,(hl)
  jp    SetSfx

  .SetTossSfx:
  ld    a,(hl)
  dec   a
  jp    z,SetSfx
  inc   c
  jp    SetSfx

;Action=0	 Idle
;Action=1	 Bend
;Action=2	 Walk
;Action=3	 Jump
;Action=4	 SoftStandPunch
;Action=5	 HardStandPunch
;Action=6	 SoftStandKick
;Action=7	 HardStandKick
;Action=8	 SoftSitPunch
;Action=9	 HardSitPunch
;Action=10 SoftSitKick
;Action=11 HardSitKick
;Action=12 JumpSoftPunch
;Action=13 JumpHardPunch
;Action=14 JumpSoftKick
;Action=15 JumpHardKick
;Action=16 StandDefend
;Action=17 BendDefend
;Action=18 StandHit
;Action=19 BendHit
;Action=20 Jumphit
;Action=21 HeavilyHit
;Action=22 KnockDownRecover
;Action=23 StandDefendHit
;Action=24 BendDefendHit
;Action=25 Special1
;Action=26 Special2
;Action=27 Special3
;Action=28 Special4
;Action=29 Special5
;Action=30 Toss
;Action=31 GettingTossed
;Action=32 Victory


SetSfx:
  ld    a,(HandleWhichPlayer?)
  dec   a
  ld    hl,P1SetSFX?
  jr    z,.playerfound
  ld    hl,P2SetSFX?
  .playerfound:
  ld    (hl),c
  ret

RDSLT:          equ 000Ch
WRSLT:          equ 0014h
EXPTBL:         equ $FCC1
Playing:        db  0             ;FFh = playing, 7Fh = paused, 00h = not playing
Loop:           db  0             ;0 = infinite, <>0 = no. of loops (decreases each time)
BGMAddr:        dw  0             ;Address of BGM in memory
CurrChn:        db  0             ;Currently processed channel
LoopCount:      db  0             ;counts number of music loops

InitmusicLoopOnce:
  ld    b,1                   ;No. of Repeats (0 = infinite)
  jp    Initmusic.go

Initmusic:
  ld    b,0                   ;No. of Repeats (0 = infinite)
  .go:
  in    a,($a8)
  push  af                    ;store current RAM/ROM page settings

	ld		a,(slot.page12rom)    ;all RAM except page 1+2
	out		($a8),a	

  ;store blocks
	ld		a,(memblocks.1)
	push  af
	ld		a,(memblocks.2)
	push  af
	ld		a,(memblocks.3)
	push  af
	ld		a,(memblocks.4)
	push  af

	ld		a,(backgroundmusicblock)
	ld		($5000),a             ;set music in page 1, block1+2, at $4000
	ld		(memblocks.1),a
	inc		a
	ld		($7000),a
	ld		(memblocks.2),a
  ld    a,musicreplayerblock  ;set music replayer in page 2, block3+4, at $8000
	ld		($9000),a
	ld		(memblocks.3),a
	inc		a
	ld		($b000),a
	ld		(memblocks.4),a

  push  bc
  pop   af                    ;  - In: A = No. of Repeats (0 = infinite), HL = Address of BGM data
  ld    hl,$4000              ;  - Out: A = Playing flag (0 = not playing, 255 = playing)
  Call  R_MPLAY               ;to initialise the music.

  ;Reset Master Volumes
  ld    hl,FMHandlefadeout.VolumeTable
  ld    b,10
  .loop:
  ld    (hl),0
  inc   hl
  djnz  .loop  
  ld    de,FMHandlefadeout.VolumeTable
  call  R_SETMVO  

  ;reset blocks
  pop   af                    ;block 4
	ld		($b000),a
	ld		(memblocks.4),a
  pop   af                    ;block 3
	ld		($9000),a
	ld		(memblocks.3),a
  pop   af                    ;block 2
	ld		($7000),a
	ld		(memblocks.2),a
  pop   af                    ;block 1
	ld		($5000),a
	ld		(memblocks.1),a

  pop   af                    ;recall RAM/ROM page setting
	out		($a8),a	
  ret

playmusic:  
  in    a,($a8)
  push  af                    ;store current RAM/ROM page settings

	ld		a,(slot.page12rom)    ;all RAM except page 1+2
	out		($a8),a	

  ;store blocks
	ld		a,(memblocks.1)
	push  af
	ld		a,(memblocks.2)
	push  af
	ld		a,(memblocks.3)
	push  af
	ld		a,(memblocks.4)
	push  af

	ld		a,(backgroundmusicblock)
	ld		($5000),a             ;set music in page 1, block1+2, at $4000
	ld		(memblocks.1),a
	inc		a
	ld		($7000),a
	ld		(memblocks.2),a
  ld    a,musicreplayerblock  ;set music replayer in page 2, block3+4, at $8000
	ld		($9000),a
	ld		(memblocks.3),a
	inc		a
	ld		($b000),a
	ld		(memblocks.4),a

  ld    a,(fadeoutmusic?)
  or    a
  call  nz,FMHandlefadeout
  call  R_MINTER              ;every VBlank (or any other fixed interval) to play music.

  ;reset blocks
  pop   af                    ;block 4
	ld		($b000),a
	ld		(memblocks.4),a
  pop   af                    ;block 3
	ld		($9000),a
	ld		(memblocks.3),a
  pop   af                    ;block 2
	ld		($7000),a
	ld		(memblocks.2),a
  pop   af                    ;block 1
	ld		($5000),a
	ld		(memblocks.1),a

  pop   af                    ;recall RAM/ROM page setting
	out		($a8),a	
  ret

;R_GETMVO
;  - Gets current Master Volumes for all channels (0-6 FM, 7-9 PSG)
;  - Master volumes: 15 = No Sound, 0 = Max Volume
;  - In: DE = pointer to 10-byte volume table

;R_SETMVO
;  - Set Master Volumes for all channels (0-6 FM, 7-9 PSG)
;  - Master volumes: 15 = No Sound, 0 = Max Volume
;  - In: DE = pointer to 10-byte volume table
FMHandlefadeout:
  ld    a,(fadeoutmusic?)
  inc   a
  ld    (fadeoutmusic?),a
  and   7
  ret   nz
    
  ld    hl,.VolumeTable
  ld    b,10
  .loop:
  ld    a,(hl)
  inc   a
  and   15
  jr    z,.endFadeChannel
  ld    (hl),a
  .endFadeChannel:
  inc   hl
  djnz  .loop

  ld    de,.VolumeTable
  jp    R_SETMVO
  .VolumeTable:
  db    0,0,0,0,0,0,0,0,0,0   ;Master volumes: 15 = No Sound, 0 = Max Volume

stopmusic:  
  in    a,($a8)
  push  af                    ;store current RAM/ROM page settings

	ld		a,(slot.page12rom)    ;all RAM except page 1+2
	out		($a8),a	

  ;store blocks
	ld		a,(memblocks.3)
	push  af
	ld		a,(memblocks.4)
	push  af

  ld    a,musicreplayerblock  ;set music replayer in page 2, block3+4, at $8000
	ld		($9000),a
	ld		(memblocks.3),a
	inc		a
	ld		($b000),a
	ld		(memblocks.4),a

  call  R_MSTOP               ;every VBlank (or any other fixed interval) to play music.

  ;reset blocks
  pop   af                    ;block 4
	ld		($b000),a
	ld		(memblocks.4),a
  pop   af                    ;block 3
	ld		($9000),a
	ld		(memblocks.3),a

  pop   af                    ;recall RAM/ROM page setting
	out		($a8),a	
  ret

switchpage:
;switch to next page
  ld    a,(screenpage)
  inc   a
  cp    3
  jr    nz,.not3
  xor   a
.not3:  
  ld    (screenpage),a

  add   a,a                   ;x32
  add   a,a
  add   a,a
  add   a,a
  add   a,a
  add   a,31
  jp    SetPage

ChangeBackdropBlack:
  ld    a,14
  jp    SetBackdrop
ChangeBackdropDarkpink:
  ld    a,11
  jp    SetBackdrop
ChangeBackdropWhite:
  ld    a,15
  jp    SetBackdrop

SetBackdrop:
  di                          ;this routine changes backdrop color
  out   ($99),a
  ld    a,7+128
  ei
  out   ($99),a	
  ret

handleprojectile:
  call  checkbothprojectilescolliding

  ld    a,(P1ProjectileInScreen?)
  or    a
  call  nz,restoreBackgroundProjectileP1
  ld    a,(P1ProjectileInScreen?)
  or    a
  call  nz,putprojectileP1

  ld    a,(P2ProjectileInScreen?)
  or    a
  call  nz,restoreBackgroundProjectileP2

  ld    a,(P2ProjectileInScreen?)
  or    a
  call  nz,putprojectileP2
  ret

checkbothprojectilescolliding:
  ld    a,(P1ProjectileInScreen?)
  or    a
  ret   z
  ld    a,(P2ProjectileInScreen?)
  or    a
  ret   z
  
  ld    a,(P1ProjectileHit?)
  or    a
  ret   nz

  ;check if both projectiles are within 20 pixels of each other
  ld    a,(ProjectileP1x)
  ld    b,a
  ld    a,(ProjectileP2x)
  sub   a,b
  jp    nc,.notcarry
  .Projectile1IsRightOfProjectile2:
  neg
  .notcarry:
  cp    60
  ret   nc  

  .CollisionFound:
  ld    a,07                  ;amount of frames that projectile impact sprite animation lasts
  ld    (P1ProjectileHit?),a
  ld    (P2ProjectileHit?),a
  xor   a                     ;reset projectile impact variables
  ld    (P1Attpoint1SxProjectile),a
;  ld    (P1Attpoint1SyProjectile),a    
  ld    (P1ProjectileRightEndFrame),a
  ld    (P1ProjectileRightEndFrame+2),a
  ld    (P1ProjectileLeftEndFrame),a
  ld    (P1ProjectileLeftEndFrame+2),a  
  ld    (P2Attpoint1SxProjectile),a
;  ld    (P2Attpoint1SyProjectile),a    
  ld    (P2ProjectileRightEndFrame),a
  ld    (P2ProjectileRightEndFrame+2),a
  ld    (P2ProjectileLeftEndFrame),a
  ld    (P2ProjectileLeftEndFrame+2),a   
  ret
    
restoreBackgroundP1:
  ld    a,(screenpage)
  or    a                     ;if current page =0 then restore page 2
  ld    hl,restorebackgroundplayer1page2
  jp    z,DoCopygrauw
  dec   a                     ;if current page =1 then restore page 0
  ld    hl,restorebackgroundplayer1page0
  jp    z,DoCopygrauw         ;if current page =2 then restore page 1
  ld    hl,restorebackgroundplayer1page1
  jp    DoCopygrauw

restoreBackgroundP2:
  ld    a,(screenpage)
  or    a                     ;if current page =0 then restore page 2
  ld    hl,restorebackgroundplayer2page2
  jp    z,DoCopygrauw
  dec   a                     ;if current page =1 then restore page 0
  ld    hl,restorebackgroundplayer2page0
  jp    z,DoCopygrauw         ;if current page =2 then restore page 1
  ld    hl,restorebackgroundplayer2page1
  jp    DoCopygrauw

restoreBackgroundProjectileP1:
  ld    a,(screenpage)
  or    a                     ;if current page =0 then restore page 2
  ld    hl,restorebackgroundprojectileP1page2
  jp    z,DoCopygrauw
  dec   a                     ;if current page =1 then restore page 0
  ld    hl,restorebackgroundprojectileP1page0
  jp    z,DoCopygrauw         ;if current page =2 then restore page 1
  ld    hl,restorebackgroundprojectileP1page1
  jp    DoCopygrauw

restoreBackgroundProjectileP2:
  ld    a,(screenpage)
  or    a                     ;if current page =0 then restore page 2
  ld    hl,restorebackgroundprojectileP2page2
  jp    z,DoCopygrauw
  dec   a                     ;if current page =1 then restore page 0
  ld    hl,restorebackgroundprojectileP2page0
  jp    z,DoCopygrauw         ;if current page =2 then restore page 1
  ld    hl,restorebackgroundprojectileP2page1
  jp    DoCopygrauw

P1LeftIdleFrame:              ;P1 looking left Idle Frame information
  ds    LenghtIdleAction
P1RightIdleFrame:             ;P1 looking right Idle Frame information
  ds    LenghtIdleAction
P1LeftBendFrame:              ;P1 looking left Bend Frame information
  ds    LenghtBendAction
P1RightBendFrame:             ;P1 looking right Bend Frame information
  ds    LenghtBendAction
P1LeftWalkLeftFrame:          ;P1 looking left Walk left Frame information
  ds    LenghtWalkAction
P1LeftWalkRightFrame:         ;P1 looking left Walk right Frame information
  ds    LenghtWalkAction
P1RightWalkLeftFrame:         ;P1 looking right Walk left Frame information
  ds    LenghtWalkAction
P1RightWalkRightFrame:        ;P1 looking right Walk right Frame information
  ds    LenghtWalkAction
P1HorSpeedWalkSlowTable:      ;P1 slow horizontal movement speed while walking
  ds    LenghtHorSpeedWalkTable
P1HorSpeedWalkFastTable:      ;P1 fast horizontal movement speed while walking
  ds    LenghtHorSpeedWalkTable
P1LeftJumpStraightStartframe: ;P1 Looking left Jumping Straight
  ds    LenghtJumpAnimationTable
P1LeftJumpLeftStartframe:     ;P1 Looking left Jumping Left
  ds    LenghtJumpAnimationTable
P1LeftJumpRightStartframe:    ;P1 Looking left Jumping Right
  ds    LenghtJumpAnimationTable
P1RightJumpStraightStartframe:;P1 Looking right Jumping Straight
  ds    LenghtJumpAnimationTable
P1RightJumpLeftStartframe:    ;P1 Looking right Jumping Left
  ds    LenghtJumpAnimationTable
P1RightJumpRightStartframe:   ;P1 Looking right Jumping Right
  ds    LenghtJumpAnimationTable
P1jumptable:                  ;P1 jump table
  ds    LenghtJumpTable
P1HorSpeedJumpSlowTable:      ;P1 slow horizontal movement speed while jumping
  ds    LenghtHorSpeedJumpTable
P1HorSpeedJumpFastTable:      ;P1 fast horizontal movement speed while jumping
  ds    LenghtHorSpeedJumpTable
P1StandSoftPunchLeftFrame:
  ds    LenghtStandSoftPunchTable
P1StandSoftPunchRightFrame:
  ds    LenghtStandSoftPunchTable
P1StandHardPunchLeftFrame:
  ds    LenghtStandSoftPunchTable
P1StandHardPunchRightFrame:
  ds    LenghtStandSoftPunchTable

P1StandSoftKickLeftFrame:
  ds    LenghtStandSoftKickTable
P1StandSoftKickRightFrame:
  ds    LenghtStandSoftKickTable

P1StandHardKickLeftFrame:
  ds    LenghtStandHardKickTable
P1StandHardKickRightFrame:
  ds    LenghtStandHardKickTable

P1SitSoftPunchLeftFrame:
  ds    LenghtStandSoftPunchTable
P1SitSoftPunchRightFrame:
  ds    LenghtStandSoftPunchTable
P1SitHardPunchLeftFrame:
  ds    LenghtStandSoftPunchTable
P1SitHardPunchRightFrame:
  ds    LenghtStandSoftPunchTable
P1SitSoftKickLeftFrame:
  ds    LenghtStandSoftPunchTable
P1SitSoftKickRightFrame:
  ds    LenghtStandSoftPunchTable

P1SitHardKickLeftFrame:
  ds    LenghtSitHardKickTable
P1SitHardKickRightFrame:
  ds    LenghtSitHardKickTable

P1LeftSoftJumpPunchStraightup:
  ds    LenghtStandSoftPunchTable
P1RightSoftJumpPunchStraightup:
  ds    LenghtStandSoftPunchTable
P1LeftHardJumpPunchStraightup:
  ds    LenghtStandSoftPunchTable
P1RightHardJumpPunchStraightup:
  ds    LenghtStandSoftPunchTable
P1LeftSoftJumpPunchDiagonalup:
  ds    LenghtStandSoftPunchTable
P1RightSoftJumpPunchDiagonalup:
  ds    LenghtStandSoftPunchTable
P1LeftHardJumpPunchDiagonalup:
  ds    LenghtStandSoftPunchTable
P1RightHardJumpPunchDiagonalup:
  ds    LenghtStandSoftPunchTable
P1LeftSoftJumpKickDiagonalUp:
  ds    LenghtStandSoftPunchTable
P1RightSoftJumpKickDiagonalUp:
  ds    LenghtStandSoftPunchTable
P1LeftHardJumpKickDiagonalUp:
  ds    LenghtStandSoftPunchTable
P1RightHardJumpKickDiagonalUp:
  ds    LenghtStandSoftPunchTable
P1LeftSoftJumpKickStraightUp:
  ds    LenghtStandSoftPunchTable
P1RightSoftJumpKickStraightUp:
  ds    LenghtStandSoftPunchTable
P1LeftHardJumpKickStraightUp:
  ds    LenghtStandSoftPunchTable
P1RightHardJumpKickStraightUp:
  ds    LenghtStandSoftPunchTable
P1LeftStandDefendFrame:
  ds    LenghtDefendTable
P1RightStandDefendFrame:
  ds    LenghtDefendTable
P1LeftBendDefendFrame:
  ds    LenghtDefendTable
P1RightBendDefendFrame:
  ds    LenghtDefendTable
P1LeftStandHitFrame:
  ds    LenghtStandHitTable
P1RightStandHitFrame:
  ds    LenghtStandHitTable
P1LeftBendHitFrame:
  ds    LenghtStandHitTable
P1RightBendHitFrame:
  ds    LenghtStandHitTable
P1LeftJumpHitFrame:
  ds    LenghtStandHitTable
P1RightJumpHitFrame:
  ds    LenghtStandHitTable
P1HeavilyHitLeftFrame:
  ds    LenghtHeavilyHitTable
P1HeavilyHitRightFrame:
  ds    LenghtHeavilyHitTable
P1JumpHitLeftFrame:
  ds    LenghtHeavilyHitTable
P1JumpHitRightFrame:
  ds    LenghtHeavilyHitTable
P1KnockDownRecoverLeftFrame:
  ds    LenghtKnockDownRecoverTable
P1KnockDownRecoverRightFrame:
  ds    LenghtKnockDownRecoverTable
P1TossLeftFrame:
  ds    LenghtTossTable
P1TossRightFrame:
  ds    LenghtTossTable
P1VictoryLeftFrame:
  ds    LenghtVictoryTable
P1VictoryRightFrame:
  ds    LenghtVictoryTable
P1Special1MovementTablePointer:
  ds    LenghtSpecialMovementTable
P1Special2MovementTablePointer:
  ds    LenghtSpecialMovementTable
P1Special3MovementTablePointer:
  ds    LenghtSpecialMovementTable
P1Special4MovementTablePointer:
  ds    LenghtSpecialMovementTable
P1Special5MovementTablePointer:
  ds    LenghtSpecialMovementTable
P1VariableTableSpecial1:
  ds    LenghtSpecialVariableTable
P1VariableTableSpecial2:
  ds    LenghtSpecialVariableTable
P1VariableTableSpecial3:
  ds    LenghtSpecialVariableTable
P1VariableTableSpecial4:
  ds    LenghtSpecialVariableTable
P1VariableTableSpecial5:
  ds    LenghtSpecialVariableTable
P1Special1LeftFrame:
  ds    LenghtSpecialFrameTable
P1Special1RightFrame:
  ds    LenghtSpecialFrameTable
P1Special2LeftFrame:
  ds    LenghtSpecialFrameTable
P1Special2RightFrame:
  ds    LenghtSpecialFrameTable
P1Special3LeftFrame:
  ds    LenghtSpecialFrameTable
P1Special3RightFrame:
  ds    LenghtSpecialFrameTable
P1Special4LeftFrame:
  ds    LenghtSpecialFrameTable
P1Special4RightFrame:
  ds    LenghtSpecialFrameTable
P1Special5LeftFrame:
  ds    LenghtSpecialFrameTable
P1Special5RightFrame:
  ds    LenghtSpecialFrameTable
P1GettingTossedLeftFrame:      ;current spriteframe, total animationsteps
  ds  LenghtGettingTossedFrameTable
P1GettingTossedRightFrame:     ;current spriteframe, total animationsteps
  ds  LenghtGettingTossedFrameTable
P1DamageTabel:
  ds  LenghtDamageTabel
P1ProjectileLeftFrame:
  ds    LenghtProjectileTabel                   ;current spriteframe, total animationsteps
P1ProjectileRightFrame:
  ds    LenghtProjectileTabel                   ;current spriteframe, total animationsteps
P1ProjectileLeftEndFrame:
  ds    LenghtProjectileEndTabel
P1ProjectileRightEndFrame:
  ds    LenghtProjectileEndTabel
P1DiedLeftFrame:
  ds    LenghtPlayerDiedTabel
P1DiedRightFrame:
  ds    LenghtPlayerDiedTabel


  

P2LeftIdleFrame:              ;P2 looking left Idle Frame information
  ds    LenghtIdleAction
P2RightIdleFrame:             ;P2 looking right Idle Frame information
  ds    LenghtIdleAction
P2LeftBendFrame:              ;P2 looking left Bend Frame information
  ds    LenghtBendAction
P2RightBendFrame:             ;P2 looking right Bend Frame information
  ds    LenghtBendAction
P2LeftWalkLeftFrame:          ;P2 looking left Walk left Frame information
  ds    LenghtWalkAction
P2LeftWalkRightFrame:         ;P2 looking left Walk right Frame information
  ds    LenghtWalkAction
P2RightWalkLeftFrame:         ;P2 looking right Walk left Frame information
  ds    LenghtWalkAction
P2RightWalkRightFrame:        ;P2 looking right Walk right Frame information
  ds    LenghtWalkAction
P2HorSpeedWalkSlowTable:      ;P2 slow horizontal movement speed while walking
  ds    LenghtHorSpeedWalkTable
P2HorSpeedWalkFastTable:      ;P2 fast horizontal movement speed while walking
  ds    LenghtHorSpeedWalkTable
P2LeftJumpStraightStartframe: ;P2 Looking left Jumping Straight
  ds    LenghtJumpAnimationTable
P2LeftJumpLeftStartframe:     ;P2 Looking left Jumping Left
  ds    LenghtJumpAnimationTable
P2LeftJumpRightStartframe:    ;P2 Looking left Jumping Right
  ds    LenghtJumpAnimationTable
P2RightJumpStraightStartframe:;P2 Looking right Jumping Straight
  ds    LenghtJumpAnimationTable
P2RightJumpLeftStartframe:    ;P2 Looking right Jumping Left
  ds    LenghtJumpAnimationTable
P2RightJumpRightStartframe:   ;P2 Looking right Jumping Right
  ds    LenghtJumpAnimationTable
P2jumptable:                  ;P2 jump table
  ds    LenghtJumpTable
P2HorSpeedJumpSlowTable:      ;P2 slow horizontal movement speed while jumping
  ds    LenghtHorSpeedJumpTable
P2HorSpeedJumpFastTable:      ;P2 fast horizontal movement speed while jumping
  ds    LenghtHorSpeedJumpTable
P2StandSoftPunchLeftFrame:
  ds    LenghtStandSoftPunchTable
P2StandSoftPunchRightFrame:
  ds    LenghtStandSoftPunchTable
P2StandHardPunchLeftFrame:
  ds    LenghtStandSoftPunchTable
P2StandHardPunchRightFrame:
  ds    LenghtStandSoftPunchTable

P2StandSoftKickLeftFrame:
  ds    LenghtStandSoftKickTable
P2StandSoftKickRightFrame:
  ds    LenghtStandSoftKickTable

P2StandHardKickLeftFrame:
  ds    LenghtStandHardKickTable
P2StandHardKickRightFrame:
  ds    LenghtStandHardKickTable

P2SitSoftPunchLeftFrame:
  ds    LenghtStandSoftPunchTable
P2SitSoftPunchRightFrame:
  ds    LenghtStandSoftPunchTable
P2SitHardPunchLeftFrame:
  ds    LenghtStandSoftPunchTable
P2SitHardPunchRightFrame:
  ds    LenghtStandSoftPunchTable
P2SitSoftKickLeftFrame:
  ds    LenghtStandSoftPunchTable
P2SitSoftKickRightFrame:
  ds    LenghtStandSoftPunchTable

P2SitHardKickLeftFrame:
  ds    LenghtSitHardKickTable
P2SitHardKickRightFrame:
  ds    LenghtSitHardKickTable



P2LeftSoftJumpPunchStraightup:
  ds    LenghtStandSoftPunchTable
P2RightSoftJumpPunchStraightup:
  ds    LenghtStandSoftPunchTable
P2LeftHardJumpPunchStraightup:
  ds    LenghtStandSoftPunchTable
P2RightHardJumpPunchStraightup:
  ds    LenghtStandSoftPunchTable
P2LeftSoftJumpPunchDiagonalup:
  ds    LenghtStandSoftPunchTable
P2RightSoftJumpPunchDiagonalup:
  ds    LenghtStandSoftPunchTable
P2LeftHardJumpPunchDiagonalup:
  ds    LenghtStandSoftPunchTable
P2RightHardJumpPunchDiagonalup:
  ds    LenghtStandSoftPunchTable
P2LeftSoftJumpKickDiagonalUp:
  ds    LenghtStandSoftPunchTable
P2RightSoftJumpKickDiagonalUp:
  ds    LenghtStandSoftPunchTable
P2LeftHardJumpKickDiagonalUp:
  ds    LenghtStandSoftPunchTable
P2RightHardJumpKickDiagonalUp:
  ds    LenghtStandSoftPunchTable
P2LeftSoftJumpKickStraightUp:
  ds    LenghtStandSoftPunchTable
P2RightSoftJumpKickStraightUp:
  ds    LenghtStandSoftPunchTable
P2LeftHardJumpKickStraightUp:
  ds    LenghtStandSoftPunchTable
P2RightHardJumpKickStraightUp:
  ds    LenghtStandSoftPunchTable
P2LeftStandDefendFrame:
  ds    LenghtDefendTable
P2RightStandDefendFrame:
  ds    LenghtDefendTable
P2LeftBendDefendFrame:
  ds    LenghtDefendTable
P2RightBendDefendFrame:
  ds    LenghtDefendTable
P2LeftStandHitFrame:
  ds    LenghtStandHitTable
P2RightStandHitFrame:
  ds    LenghtStandHitTable
P2LeftBendHitFrame:
  ds    LenghtStandHitTable
P2RightBendHitFrame:
  ds    LenghtStandHitTable
P2LeftJumpHitFrame:
  ds    LenghtStandHitTable
P2RightJumpHitFrame:
  ds    LenghtStandHitTable
P2HeavilyHitLeftFrame:
  ds    LenghtHeavilyHitTable
P2HeavilyHitRightFrame:
  ds    LenghtHeavilyHitTable
P2JumpHitLeftFrame:
  ds    LenghtHeavilyHitTable
P2JumpHitRightFrame:
  ds    LenghtHeavilyHitTable
P2KnockDownRecoverLeftFrame:
  ds    LenghtKnockDownRecoverTable
P2KnockDownRecoverRightFrame:
  ds    LenghtKnockDownRecoverTable
P2TossLeftFrame:
  ds    LenghtTossTable
P2TossRightFrame:
  ds    LenghtTossTable
P2VictoryLeftFrame:
  ds    LenghtVictoryTable
P2VictoryRightFrame:
  ds    LenghtVictoryTable
P2Special1MovementTablePointer:
  ds    LenghtSpecialMovementTable
P2Special2MovementTablePointer:
  ds    LenghtSpecialMovementTable
P2Special3MovementTablePointer:
  ds    LenghtSpecialMovementTable
P2Special4MovementTablePointer:
  ds    LenghtSpecialMovementTable
P2Special5MovementTablePointer:
  ds    LenghtSpecialMovementTable
P2VariableTableSpecial1:
  ds    LenghtSpecialVariableTable
P2VariableTableSpecial2:
  ds    LenghtSpecialVariableTable
P2VariableTableSpecial3:
  ds    LenghtSpecialVariableTable
P2VariableTableSpecial4:
  ds    LenghtSpecialVariableTable
P2VariableTableSpecial5:
  ds    LenghtSpecialVariableTable
P2Special1LeftFrame:
  ds    LenghtSpecialFrameTable
P2Special1RightFrame:
  ds    LenghtSpecialFrameTable
P2Special2LeftFrame:
  ds    LenghtSpecialFrameTable
P2Special2RightFrame:
  ds    LenghtSpecialFrameTable
P2Special3LeftFrame:
  ds    LenghtSpecialFrameTable
P2Special3RightFrame:
  ds    LenghtSpecialFrameTable
P2Special4LeftFrame:
  ds    LenghtSpecialFrameTable
P2Special4RightFrame:
  ds    LenghtSpecialFrameTable
P2Special5LeftFrame:
  ds    LenghtSpecialFrameTable
P2Special5RightFrame:
  ds    LenghtSpecialFrameTable
P2GettingTossedLeftFrame:      ;current spriteframe, total animationsteps
  ds  LenghtGettingTossedFrameTable
P2GettingTossedRightFrame:     ;current spriteframe, total animationsteps
  ds  LenghtGettingTossedFrameTable
P2DamageTabel:
  ds  LenghtDamageTabel
P2ProjectileLeftFrame:
  ds    LenghtProjectileTabel                   ;current spriteframe, total animationsteps
P2ProjectileRightFrame:
  ds    LenghtProjectileTabel                   ;current spriteframe, total animationsteps
P2ProjectileLeftEndFrame:
  ds    LenghtProjectileEndTabel
P2ProjectileRightEndFrame:
  ds    LenghtProjectileEndTabel
P2DiedLeftFrame:
  ds    LenghtPlayerDiedTabel
P2DiedRightFrame:
  ds    LenghtPlayerDiedTabel
  

endengine:
dephase
enginelength:	              Equ	$-engine