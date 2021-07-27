loader:                       ;all RAM except page 1+2
  call  CheckOpl4Present      ;check if Opl4 is present
  call  unFreezeControls      ;controls are active

  CapcomLogo:
  call  ChoosePlayersandBackground
	call  ResetCharactersUnlocked
	call  ResetCharactersDefeated
  call  loadsamples

;jp VsModeDirect
;jp ArcadeModeDirect
;jp MainMenu

	call  CapcomLogoAnimation   ;Capcom Logo animation and the tune
	call  StreetFighterAnimation;Streetfighter Logo animation

  MainMenu:
  call  MainMenuEngine
  call  VAdjustScreenReset
  call  unFreezeControls      ;controls are active
	call  ResetGeneralvariables
	call  ResetCharactersDefeated

  ld    a,(mainmenuoptionselected?)
  cp    1                     ;arcade is pressed
  jp    z,ArcadeMode
  cp    2                     ;vs is pressed
  jp    z,VsModeCharacterSelect
  cp    3                     ;trials is pressed
  jp    z,TrialsMode
  cp    4                     ;option is pressed
  jp    z,option
  
  option:
  jp    MainMenu
  
  TrialsMode:
  jp    MainMenu

ChoosePlayersandBackground:   ;0=Ryu, 1=Chunli, 2=Dhalsim, 3=Guile, 4=eHonda, 5=Cammy, 6=Vega
  ld    a,0
  ld    (Player1),a
  ld    a,6
  ld    (Player2),a
  ld    a,6
  ld    (Background),a
  ret

ArcadeModeDirect:
  ld    a,1                   ;this enables AI
  ld    (mainmenuoptionselected?),a
  
ArcadeMode:
  call  randomlyChooseP2      ;randomly choose next opponent (used for arcade mode)
  ld    a,1
  ld    (player1mayChooseChar?),a
  ArcadeModeLoop:
  call  SetbackdropColor0Black
  call  spritesoff
  call  CharacterAndStageSelectArcadeMode
  call  Disable_Scr
  call  CountrySample         ;announcing the country
  call  SetSpecialSamples
  call  ResetGeneralvariables
  call  ResetFightvariables
  ArcadeModeFightLoop:        ;this loop is repeated until 1 player has won 2 rounds
  call  Initstagemusic
  call  VAdjustScreenDown
  call  buildupbackground     ;this also copies the background part of the palette
  call  copyplayerPalettes    ;this copies the player part of the palette
  call  SetCurrentpalette
  call  SetbackdropColor14Black
  call  SetspriteData
  call  setFramelists
  call  AdjustPlayer2Colors
  call  LoadPlayerActionTables
  call  SetMaleOrFemale
  call  setscreenfadein
  call  FreezeControls        ;players cant move until announcer says: "Fight!"
  call  Enable_Scr
  call  LevelEngine
.loop:
  call  Disable_Scr
;jp mainmenu
  ld    a,(P1RoundsWon)       ;amount of rounds won
  cp    2                     ;check if fight is completely over
  jr    z,.player1Won
  ld    a,(P2RoundsWon)       ;amount of rounds won
  cp    2                     ;check if fight is completely over
  jp    z,.player1Lost

  ld    a,1                   ;0=no, 1=yes
  ld    (AnnounceRound?),a
  xor   a
  ld    (AnnounceRoundStep),a
  ld    a,(CurrentRound)      ;1=Round1, 2=Round2, 3=Round3, 4=Final Round
  inc   a
  ld    (CurrentRound),a
  cp    4
  jr    nz,.not4
  ld    a,1                   ;at round 4 both players won 1 Round
  ld    (P1RoundsWon),a
  ld    (P2RoundsWon),a
  .not4:
  cp    5                     ;if no one has won at the end of round 4, then player 1 has lost (fight ends)
  jp    z,.player1Lost

	call  ResetGeneralvariables
  jp    ArcadeModeFightLoop   ;next round
  

  .player1Won:
  ;player 2 got defeated
  ld    a,(player2)
  ld    d,0
  ld    e,a
  ld    hl,character00defeated?
  add   hl,de
  ld    (hl),1                ;this character is now defeated

  call  checkallplayersdefeated
  jp    c,.allplayersdefeated
  call  randomlyChooseP2      ;randomly choose next opponent
  jp    ArcadeModeLoop
  
  .player1Lost:
  call  CheckContinue
  jp    c,CapcomLogo          ;c=no continue
  
  ld    a,1
  ld    (player1mayChooseChar?),a
  jp    ArcadeModeLoop
  
  .allplayersdefeated:        ;you finished arcade mode
  call  Congratulations
  jp    CapcomLogo


VsModeDirect:
  call  SetbackdropColor0Black
  call  spritesoff
  call  Disable_Scr
  call  SetSpecialSamples
  call  ResetGeneralvariables
  call  ResetFightvariables
  jp    VsModeFightLoop
  
VsModeCharacterSelect:
  call  SetbackdropColor0Black
  call  spritesoff
  call  CharacterAndStageSelectVsMode
  call  Disable_Scr
  call  CountrySample         ;announcing the country
  call  SetSpecialSamples
  call  ResetGeneralvariables
  call  ResetFightvariables
  VsModeFightLoop:            ;this loop is repeated until 1 player has won 2 rounds
  call  Initstagemusic
  call  VAdjustScreenDown
  call  buildupbackground     ;this also copies the background part of the palette
  call  copyplayerPalettes    ;this copies the player part of the palette
  call  SetCurrentpalette
  call  SetbackdropColor14Black
  call  SetspriteData
  call  setFramelists
  call  AdjustPlayer2Colors
  call  LoadPlayerActionTables
  call  SetMaleOrFemale
  call  setscreenfadein
  call  FreezeControls        ;players cant move until announcer says: "Fight!"
  call  Enable_Scr
  call  LevelEngine
.loop:
  call  Disable_Scr
;jp mainmenu
  ld    a,(P1RoundsWon)       ;amount of rounds won
  cp    2                     ;check if fight is completely over
  jp    z,VsModeCharacterSelect
  ld    a,(P2RoundsWon)       ;amount of rounds won
  cp    2                     ;check if fight is completely over
  jp    z,VsModeCharacterSelect

  ld    a,1                   ;0=no, 1=yes
  ld    (AnnounceRound?),a
  xor   a
  ld    (AnnounceRoundStep),a
  ld    a,(CurrentRound)      ;1=Round1, 2=Round2, 3=Round3, 4=Final Round
  inc   a
  ld    (CurrentRound),a
  cp    4
  jr    nz,.not4
  ld    a,1                   ;at round 4 both players won 1 Round
  ld    (P1RoundsWon),a
  ld    (P2RoundsWon),a
  .not4:
  cp    5                     ;if no one has won at the end of round 4, then go back to MainMenu (fight ends)
  jp    z,VsModeCharacterSelect

	call  ResetGeneralvariables
  jp    VsModeFightLoop       ;next round



StreetFighterAnimation:
  call  Disable_Scr

  call  spritesoff

  ld    hl,StreetfighterAnimationPalette
  call  setpalette
  call  SetbackdropColor0Black;assign color 0 to the backdrop

  xor   a
  ld    (flickerstep),a
  call  buildupallanimationframes

  ld    a,2                   ;set street fighter logo animation lineint
  ld    (whichlineint?),a

  halt
  call  setInterrupt1         ;line int also uses interrupt1
  ;line int also uses Interrupt1
  ld    a,SfLogoLineInt1
  ld    (ylineint),a
  di
  out   ($99),a
  ld    a,19+128
  ei
  out   ($99),a
  
  ;enable lineint
  ld    a,(vdp_0)            ;set ei1
  or    %0001 0000           ;ei1 checks for lineint and vblankint
  ld    (vdp_0),a            ;ei0 (which is default at boot) only checks vblankint
  di
  out   ($99),a
  ld    a,128
  ei
  out   ($99),a

  ld    a,0*32 + 31
  call  SetPage
    
  xor   a
  ld    (framecounter),a

.Animationloop:  
  ld    a,(framecounter)
  inc   a
  jp    z,.end
  ld    (framecounter),a
  halt
  jp    .Animationloop

  .end:
  ld    b,200
  .waitloop:
  halt
  djnz  .waitloop

  ;disable lineint
  ld    a,(vdp_0)            ;reset ei1
  and   %1110 1111           ;ei1 checks for lineint and vblankint
  ld    (vdp_0),a            ;ei0 (which is default at boot) only checks vblankint
  di
  out   ($99),a
  ld    a,128
  ei
  out   ($99),a
  
  call  Disable_Scr  

  ld    b,100
  .waitloop2:
  halt
  djnz  .waitloop2


  ret

    

StreetfighterLogoAnimationLineint:
  xor   a                     ;set s#0
  out   ($99),a
  ld    a,15+128
  out   ($99),a

  push  bc
  push  de
  push  hl 
  call  .setnextlineint
  pop   hl
  pop   de
  pop   bc

  pop   af
  ei
  ret

.setnextlineint:
  ld    a,(ylineint)
  cp    SfLogoLineInt1
  jp    z,.firstLineInt
  cp    SfLogoLineInt2
  jp    z,.secondLineInt
  cp    SfLogoLineInt3
  jp    z,.thirdLineInt
  cp    SfLogoLineInt4
  jp    z,.fourthLineInt
  ret
  
  .firstLineInt:
  ld    a,SfLogoLineInt2      ;prepare 2nd lineint
  ld    (ylineint),a
  di
  out   ($99),a
  ld    a,19+128
  ei
  out   ($99),a  
  call  Enable_Scr
  ret

  .secondLineInt:
  ld    a,SfLogoLineInt1      ;prepare 1st lineint
  ld    (ylineint),a
  di
  out   ($99),a
  ld    a,19+128
  ei
  out   ($99),a  
  call  Disable_Scr
  ld    a,90-128
  call  VAdjustScreenManual

  ld    a,(framecounter)
  and   7
  ret   nz

  ld    a,SfLogoLineInt3      ;prepare 3d lineint
  ld    (ylineint),a
  di
  out   ($99),a
  ld    a,19+128
  ei
  out   ($99),a  
  
  ld    a,90
  call  VAdjustScreenManual
  ret

  .thirdLineInt:
  ld    a,SfLogoLineInt4      ;prepare 4th lineint
  ld    (ylineint),a
  di
  out   ($99),a
  ld    a,19+128
  ei
  out   ($99),a  
  call  Enable_Scr
  ret

  .fourthLineInt:
  ld    a,SfLogoLineInt3      ;prepare 3d lineint
  ld    (ylineint),a
  di
  out   ($99),a
  ld    a,19+128
  ei
  out   ($99),a  
  call  Disable_Scr
  ld    a,90
  call  VAdjustScreenManual

  ld    a,(framecounter)
  and   7
  ret   nz

  ld    a,SfLogoLineInt1      ;prepare 1st lineint
  ld    (ylineint),a
  di
  out   ($99),a
  ld    a,19+128
  ei
  out   ($99),a  
  
  ld    a,90-128
  call  VAdjustScreenManual

  ;make the page switch here
  ld    a,(VDP_0+2)
  add   a,32
  cp    4*32 + 31
  jp    nz,setpage
  
  ld    a,SfLogoLineInt3      ;prepare 3d lineint
  ld    (ylineint),a
  di
  out   ($99),a
  ld    a,19+128
  ei
  out   ($99),a  
  
  ld    a,90
  call  VAdjustScreenManual
  ret

  




SfLogoLineInt1: equ 0
SfLogoLineInt2: equ 125
SfLogoLineInt3: equ 125+1
SfLogoLineInt4: equ 252

buildupallanimationframes:
;  ld    a,Streetfighterlogoaniblock
;  call	block12      		      ;at address $4000 / page 1
;  ld    hl,logo1pck           ;source
;  ld    a,0
;  ld    ix,$0000              ;address $00000 (page 0 screen 5)
;  call  .putimage

  ld    a,Streetfighterlogoaniblock
  call	block12      		      ;at address $4000 / page 1
  ld    hl,logo2pck           ;source
  ld    a,0
  ld    ix,$0000              ;address $04000 (page 0 screen 5)
  call  .putimage

  ld    a,Streetfighterlogoaniblock
  call	block12      		      ;at address $4000 / page 1
  ld    hl,logo3pck           ;source
  ld    a,0
  ld    ix,$4000              ;address $08000 (page 1 screen 5)
  call  .putimage

  ld    a,Streetfighterlogoaniblock
  call	block12      		      ;at address $4000 / page 1
  ld    hl,logo4pck           ;source
  ld    a,0
  ld    ix,$8000              ;address $0c000 (page 1 screen 5)
  call  .putimage

  ld    a,Streetfighterlogoaniblock
  call	block12      		      ;at address $4000 / page 1
  ld    hl,logo5pck           ;source
  ld    a,0
  ld    ix,$c000              ;address $10000 (page 2 screen 5)
  call  .putimage

  ld    a,Streetfighterlogoaniblock+1
  call	block12     		      ;at address $4000 / page 1
  ld    hl,logo6pck-$2000     ;source
  ld    a,1
  ld    ix,$0000              ;address $14000 (page 2 screen 5)
  call  .putimage

  ld    a,Streetfighterlogoaniblock+2
  call	block12     		      ;at address $4000 / page 1
  ld    hl,logo7pck-$4000     ;source
  ld    a,1
  ld    ix,$4000              ;address $18000 (page 3 screen 5)
  call  .putimage

  ld    a,Streetfighterlogoaniblock+3
  call	block12     		      ;at address $4000 / page 1
  ld    hl,logo8pck-$6000     ;source
  ld    a,1
  ld    ix,$8000              ;address $1c000 (page 3 screen 5)
  call  .putimage

  ld    a,Streetfighterlogoaniblock+3
  call	block12     		      ;at address $4000 / page 1
  ld    hl,logo9pck-$6000     ;source
  ld    a,1
  ld    ix,$c000              ;address $1c000 (page 3 screen 5)
  call  .putimage
  ret

  .putimage:
  push  af
  ld    a,7                   ;set player 2 sprites (4th part) in bank 7
  di
  out   ($fc),a               ;page a,x,x,x

  ld    de,$0000              ;page 0 (address $00000)
  call  Depack                ;In: HL: source, DE: destination

  call  .flickering

  push  ix
  pop   hl                    ;address to write to
  pop   af

;first set register 14 (actually this only needs to be done once
  rlc     h
	rla
	rlc     h
	rla
	srl     h
	srl     h
	out     ($99),a             ;set bits 15-17
	ld      a,14+128
	out     ($99),a
;/first set register 14 (actually this only needs to be done once

	ld      a,l                 ;set bits 0-7
	nop
	out     ($99),a
	ld      a,h                 ;set bits 8-14
	or      64                  ; + write access
	out     ($99),a       


	ld		hl,$0000
	ld		c,$98                 ;256 bytes to port $98
  ld    b,128                 ;128 lines to out

  .loop3:
	call  outix128
  djnz  .loop3

  call  .flickering

  ld    a,3                   ;set bank 3 back in page 0 ($0000 - $3fff)
  out   ($fc),a               ;page 3,x,x,x
  ei
  ret

  .flickering:
  ld    a,(flickerstep)
  inc   a
  ld    (flickerstep),a
  cp    1
  jp    z,.flickeringon
  cp    2
  jp    z,.flickeringoff
  cp    5
  jp    z,.flickeringon
  cp    6
  jp    z,.flickeringoff
  cp    7
  jp    z,.flickeringon
  cp    8
  jp    z,.flickeringoff
  cp    11
  jp    z,.flickeringon
  cp    12
  jp    z,.flickeringoff
  cp    15
  jp    z,.flickeringon
  cp    16
  jp    z,.flickeringoff
  ret

  .flickeringoff:
	xor		a
	di
	out		($99),a
	ld		a,16+128
	out		($99),a

  ld    a,0*16 + 4            ;blue=4, red=0
  out   ($9a),a
  ld    a,1                   ;green=1
  out   ($9a),a
  ret
  
  .flickeringon:
	xor		a
	di
	out		($99),a
	ld		a,16+128
	out		($99),a

  ld    a,255
  out   ($9a),a
  out   ($9a),a
  ret

  


StreetfighterAnimationPalette:
  incbin "..\grapx\streetfighter logo\streetfighter logo animation palette.plt",0,32






























Congratulations:
  call  Disable_Scr
  call  VAdjustScreenReset

  ld    a,Congratulationsblock
  call	block12      		      ;at address $4000 / page 1

  ld    a,0*32 + 31
  call  SetPage

  ld    hl,EraseScreen        ;erase page 0 AND 1
  call  docopygrauw
  call  VDPready              ;wait vdp ready

  ld    a,0
  ld    hl,$8000              ;address $008000 (page 1 screen 5)
  call  SetVdp_Write  

	ld		hl,CongratulationsTilesAddress
	ld		bc,$0098              ;256 bytes to port $98
  
  exx
  ld    b,14                  ;28 lines to out
  .loop:
  exx
	otir
  exx
  djnz  .loop
  exx  
  call  setpalette

  ld    hl,.PutCongratulations1
  call  docopygrauw
  ld    hl,.PutCongratulations2
  call  docopygrauw
  ld    hl,.PutCongratulations3
  call  docopygrauw
  ld    hl,.PutCongratulations4
  call  docopygrauw
  ld    hl,.PutCongratulations5
  call  docopygrauw

  halt
  call  setInterrupt1         ;this interrupt plays the music
  ld    a,Congratulationsblock;then set the new song block
  ld		(backgroundmusicblock),a
  call  InitmusicLoopOnce
  call  Enable_Scr

  ld    a,3
  ld    b,80
  .waitloop:
  halt
  djnz  .waitloop
  dec   a
  jr    nz,.waitloop
  jp    CapcomLogo

.PutCongratulations1:
	db    0,0,0,1
	db    60,0,28,0
	db    126,0,12,0
	db    0,0,$d0  
.PutCongratulations2:
	db    126,0,0,1
	db    76,0,49,0
	db    96,0,8,0
	db    0,0,$d0  
.PutCongratulations3:
	db    150,0,8,1
	db    76,0,85,0
	db    94,0,16,0
	db    0,0,$d0  
.PutCongratulations4:
	db    6,0,12,1
	db    64,0,189,0
	db    126,0,8,0
	db    0,0,$d0  
.PutCongratulations5:
	db    0,0,20,1
	db    56,0,200,0
	db    150,0,8,0
	db    0,0,$d0  

CheckContinue:                ;out: c=no continue
  call  setInterrupt1         ;this interrupt plays the music
  ld    a,Continueblock       ;then set the new song block
  ld		(backgroundmusicblock),a
  call  InitmusicLoopOnce

  ;put continue
  ld    a,Continueblock
  call	block12      		      ;at address $4000 / page 1

  ld    a,0*32 + 31
  call  SetPage
  xor   a
  ld    (ContinuePage),a

  call  Disable_Scr
  call  VAdjustScreenReset

  ld    a,1
  ld    hl,$0000              ;address $10000 (page 2 screen 5)
  call  SetVdp_Write  

	ld		hl,ContinueTilesAddress
	ld		bc,$0098              ;256 bytes to port $98
  
  exx
  ld    b,07                  ;14 lines to out
  .loop1:
  exx
	otir
  exx
  djnz  .loop1
  exx  
  push  hl
  halt
  call  SetbackdropColor0Black
  call  setpalette
  pop   hl
  ld    de,currentpalette
  ld    bc,16*2
  ldir

  ;erase page 0 and 1
  ld    hl,EraseScreen        ;erase page 0 AND 1
  call  docopygrauw
  ld    hl,ContinueText
  call  docopygrauw

  ld    ix,EmptyContinueNumber
  call  putcontinuenumber

  ;set screen to fade in
  ld    a,1
  ld    (fadeinout?),a        ;0=nothing, 1=fade in, 2=fade out
  xor   a
  ld    (fadeinoutstep),a
  ld    a,8                   ;amount of steps to darken (8 = total darkness)
  call  ScreenFadeInOut.GoSetPalette

  halt
  call  Enable_Scr
  call  ScreenFadeLoop        ;lets screen fade in or out

  ld    iy,ContinueNumber9
  exx
  ld    b,10                  ;count till 10

  .countdownloop:
  exx
  ld    b,4
  call  .waitloop             ;out: carry= attack button pressed
  jr    c,.GoContinue
  push  iy
  pop   ix
  ld    a,1                   ;put dark number
  call  putcontinuenumber
  ld    b,4
  call  .waitloop             ;out: carry= attack button pressed
  jr    c,.GoContinue
  push  iy
  pop   ix
  xor   a                     ;put bright number
  call  putcontinuenumber
  ld    b,37
  call  .waitloop             ;out: carry= attack button pressed
  jr    c,.GoContinue
  push  iy
  pop   ix
  ld    a,1                   ;put dark number
  call  putcontinuenumber
  ld    b,4
  call  .waitloop             ;out: carry= attack button pressed
  jr    c,.GoContinue
  ld    ix,EmptyContinueNumber
  call  putcontinuenumber

  ld    de,6*8
  add   iy,de
  exx
  djnz  .countdownloop

  ;set screen fade out
  ld    a,2
  ld    (fadeinout?),a        ;0=nothing, 1=fade in, 2=fade out
  xor   a
  ld    (fadeinoutstep),a
  call  ScreenFadeLoop        ;lets screen fade in or out
  
  ld    b,80
  .waitloop3:
  halt
  djnz  .waitloop3
  
  scf                         ;carry= no conitnue
  ret

  .waitloop:
  halt
  ;check if player 1 presses an attackbutton
  ld    a,1
  ld    (HandleWhichPlayer?),a  
;
; bit 7     6     5     4     3     2     1     0
;		  softk softp hardk hardp right left  down  up  (action)
;		  /     .     '     ;     right left  down  up  (ControlsP1)
;		  S     A     W     Q     right left  down  up  (ControlsP2)
;
  push  bc
	call  PopulateControls
  pop   bc
  call  GetNewPrControls      ;out a-> NewPrcontrols, changes: a, f
  and   %1111 0000            ;check if any attack button is pressed
  jr    nz,.attackbuttonpressed
  djnz  .waitloop
  ret
  .attackbuttonpressed:
  scf                         ;carry= attack button pressed
  ret

  .GoContinue:
  call  stopmusic
  ld    c,sfxP1Special1
  call  Setsfx
  call  Samples

  ld    b,6
  
  .flashlightloop:
  halt
  ld    c,255
  call  .setpalettecolor0
  halt
  halt
  ld    c,0
  call  .setpalettecolor0
  djnz  .flashlightloop
  
  ld    b,60
  .waitloop2:
  halt
  djnz  .waitloop2

  ;set screen fade out
  ld    a,2
  ld    (fadeinout?),a        ;0=nothing, 1=fade in, 2=fade out
  xor   a
  ld    (fadeinoutstep),a
  call  ScreenFadeLoop        ;lets screen fade in or out
  xor   a                     ;no carry= continue
  ret

  .setpalettecolor0:
	xor		a
	di
	out		($99),a
	ld		a,16+128
	out		($99),a
  ld    a,c
  out   ($9a),a
  out   ($9a),a
	ei
  ret

ScreenFadeLoop:
  ld    b,16
.loop:
  halt
  halt
  halt
  push  bc
  call  ScreenFadeInOut
  pop   bc
  djnz  .loop
  ret

putcontinuenumber:
  ld    d,a                   ;0=dark, 1=bright
  ld    a,98
  ld    (PutContinueLetter+dx),a
  ld    a,60
  ld    (PutContinueLetter+dy),a

  ld    c,8
  ld    b,6
  .loop:
  push  bc
  ld    a,d                   ;bright or dark one ?
  or    a
  ld    a,(ix)
  jp    z,.no                 ;should dark number be put ?
  cp    150
  ld    a,000
  jr    nz,.no
  ld    a,160
  .no:
  
  ld    (PutContinueLetter+sx),a
  
  ld    hl,PutContinueLetter
  call  docopygrauw
  pop   bc

  ld    a,(PutContinueLetter+dx)
  add   a,10
  ld    (PutContinueLetter+dx),a
  inc   ix
  djnz  .loop

  ld    a,98
  ld    (PutContinueLetter+dx),a
  ld    a,(PutContinueLetter+dy)
  add   a,14
  ld    (PutContinueLetter+dy),a
  ld    b,6
  dec   c
  jr    nz,.loop
  ret

EmptyContinueNumber:
  db    000,000,000,000,000,000
  db    000,000,000,000,000,000
  db    000,000,000,000,000,000
  db    000,000,000,000,000,000
  db    000,000,000,000,000,000
  db    000,000,000,000,000,000
  db    000,000,000,000,000,000
  db    000,000,000,000,000,000
  
ContinueNumber9:
  db    090,150,150,150,150,100
  db    150,150,070,080,150,150
  db    150,150,030,040,150,150
  db    150,150,100,090,150,150
  db    080,150,150,150,150,150
  db    020,050,010,080,150,150
  db    150,150,100,090,150,150
  db    080,150,150,150,150,070

ContinueNumber8:
  db    090,150,150,150,150,100
  db    150,150,070,080,150,150
  db    150,150,100,090,150,150
  db    120,150,150,150,150,110
  db    150,150,070,080,150,150
  db    150,150,030,040,150,150
  db    150,150,100,090,150,150
  db    080,150,150,150,150,070

ContinueNumber7:
  db    150,150,150,150,150,150
  db    150,150,070,080,150,150
  db    010,010,000,090,150,150
  db    000,000,040,150,150,070
  db    000,000,090,150,150,030
  db    000,040,150,150,070,000
  db    000,040,150,150,030,000
  db    000,040,150,150,030,000

ContinueNumber6:
  db    090,150,150,150,150,100
  db    150,150,070,080,150,150
  db    150,150,100,020,050,010
  db    150,150,150,150,150,100
  db    150,150,070,080,150,150
  db    150,150,030,040,150,150
  db    150,150,100,090,150,150
  db    080,150,150,150,150,070

ContinueNumber5:
  db    150,150,150,150,150,150
  db    150,150,070,010,010,010
  db    150,150,100,020,020,000
  db    150,150,150,150,150,100
  db    050,050,010,080,150,150
  db    150,150,030,040,150,150
  db    150,150,100,090,150,150
  db    080,150,150,150,150,070

ContinueNumber4:
  db    000,000,000,040,150,030
  db    000,000,090,150,150,030
  db    000,090,150,150,150,030
  db    090,150,140,150,150,030
  db    150,110,090,150,150,100
  db    150,150,150,150,150,150
  db    010,010,080,150,150,070
  db    000,000,040,150,150,030

ContinueNumber3:
  db    090,150,150,150,150,100
  db    150,150,070,080,150,150
  db    010,010,020,090,150,150
  db    000,040,150,150,150,110
  db    020,020,010,080,150,150
  db    150,150,030,040,150,150
  db    150,150,100,090,150,150
  db    080,150,150,150,150,070

ContinueNumber2:
  db    090,150,150,150,150,100
  db    150,150,070,080,150,150
  db    150,150,100,090,150,150
  db    010,050,020,090,150,150
  db    090,150,150,150,150,070
  db    150,150,070,010,010,000
  db    150,150,100,020,020,020
  db    150,150,150,150,150,150

ContinueNumber1:
  db    000,090,150,150,030,000
  db    040,150,150,150,030,000
  db    000,080,150,150,030,000
  db    000,040,150,150,030,000
  db    000,040,150,150,030,000
  db    000,040,150,150,030,000
  db    000,090,150,150,100,000
  db    040,150,150,150,150,030

ContinueNumber0:
  db    090,150,150,150,150,100
  db    150,150,070,080,150,150
  db    150,150,030,040,150,150
  db    150,150,030,040,150,150
  db    150,150,030,040,150,150
  db    150,150,030,040,150,150
  db    150,150,100,090,150,150
  db    080,150,150,150,150,070

EraseScreen:
	db    0,0,0,0
	db    0,0,0,0
	db    0,1,0,2
	db    0,0,$c0	

ContinueText:
	db    170,0,0,2
	db    92,0,35,0
	db    74,0,9,0
	db    0,0,$d0  

SetCurrentpalette:
  halt
  ld    hl,currentpalette
  call  SetPalette
  ret

MainMenuEngine:
  halt
  call  Disable_Scr
  call  setscreen5
  ld    a,0*32 + 31
  call  SetPage
  xor   a
  call  VAdjustScreenManual
  ;erase bottom part of screen
  ld    hl,StreetFighterLogobottomErase
  call  docopygrauw

  ;put street fighter logo
  ld    a,streetfighterlogoblock
  call	block12      		      ;at address $4000 / page 1

  ld    a,0
  ld    hl,$0000              ;address $08000 (page 1 screen 5)
  call  SetVdp_Write  

	ld		hl,$4000              ;level grapx in rom
	ld		bc,$0098              ;256 bytes to port $98
  
  exx
  ld    b,56                  ;112 lines to out
  .loop1:
  exx
	otir
  exx
  djnz  .loop1
  exx  

  ld    hl,StreetFighterLogobottom1
  call  docopygrauw
  ld    hl,StreetFighterLogotopErase
  call  docopygrauw
  
  ;put arcade and vs
  ld    a,mainmenuoptionsblock
  call	block12      		      ;at address $4000 / page 1

  ld    a,0
  ld    hl,147*128            ;y=147 (page 0 screen 5)
  call  SetVdp_Write  

	ld		hl,$4000              ;level grapx in rom
	ld		bc,$8098              ;256 bytes to port $98
  
  exx
  ld    b,15                  ;30 - 1 lines to out
  .loop2:
  exx
	otir
  exx
  djnz  .loop2

  ;put trials and option
  ld    a,0
  ld    hl,182*128            ;y=182 (page 0 screen 5)
  call  SetVdp_Write  

  exx    
	ld		bc,$8098              ;256 bytes to port $98
  exx
  ld    b,15                  ;30 - 1 lines to out
  .loop3:
  exx
	otir
  exx
  djnz  .loop3
  
  ;copy page 0 to 1 and 2
  ld    hl,copypage0to1
  call  docopygrauw
  ld    hl,copypage0to2
  call  docopygrauw

  ;put main menu selection icons in page 3
  ld    a,mainmenuoptionsblock
  call	block12      		      ;at address $4000 / page 1

  ld    a,1
  ld    hl,$8000              ;address $18000 (page 3 screen 5)
  call  SetVdp_Write  

	ld		hl,mainmenuselectionicongraphics
	ld		bc,$0098              ;256 bytes to port $98
  
  exx
  ld    b,16                  ;32 lines to out
  .loop4:
  exx
	otir
  exx
  djnz  .loop4

  ld    a,1                   ;set main menu lineint
  ld    (whichlineint?),a

  call  setInterrupt1         ;line int also uses interrupt1
  ;line int also uses Interrupt1
  ld    a,139                 ;lineint at y=139
  ld    (ylineint),a
  di
  out   ($99),a
  ld    a,19+128
  ei
  out   ($99),a
  
  ;enable lineint
  ld    a,(vdp_0)            ;set ei1
  or    %0001 0000           ;ei1 checks for lineint and vblankint
  ld    (vdp_0),a            ;ei0 (which is default at boot) only checks vblankint
  di
  out   ($99),a
  ld    a,128
  ei
  out   ($99),a

  call  SetbackdropColor0Black
  call  Enable_Scr

  xor   a
  ld    (mainmenupage),a
  ld    (mainmenuselectioniconpointer),a
  ld    (mainmenuoptionselected?),a

.mainmenuloop:   
  ;switch to page 1
  ld    a,1*32 + 31
  call  SetPage  
  ;restore page 0
  ld    hl,mainmenurestorepage0
  call  docopygrauw           ;page 1 is always without selection icon, page 0 is always restored when page 1 is active
  ld    b,6
  call  .waitloop
  call  .Copysetselectionicon
  ;switch to page 0
  ld    a,0*32 + 31
  call  SetPage
  ld    b,15
  call  .waitloop
  ld    a,(mainmenuoptionselected?)
  or    a
  jp    z,.mainmenuloop

  call  Disable_Scr  

  ;disable lineint
  ld    a,(vdp_0)             ;reset ei1
  and   %1110 1111            ;ei1 checks for lineint and vblankint
  ld    (vdp_0),a             ;ei0 (which is default at boot) only checks vblankint
  di
  out   ($99),a
  ld    a,128
  ei
  out   ($99),a

  ld    b,20                  ;let sfx continue briefly
  .shortwait:
  halt
  djnz  .shortwait
  ret

  .Copysetselectionicon:
  ld    a,(mainmenuselectioniconpointer)
  or    a
  ld    hl,Mainmenuselectionicon1
  jp    z,docopygrauw  
  dec   a
  ld    hl,Mainmenuselectionicon2
  jp    z,docopygrauw  
  dec   a
  ld    hl,Mainmenuselectionicon3
  jp    z,docopygrauw  
  ld    hl,Mainmenuselectionicon4
  jp    docopygrauw  

  .waitloop:
  push  bc
  halt
	call  PopulateControls
  call  .handlecontrols
  pop   bc
  djnz  .waitloop
  ret

  .handlecontrols:
  ld    a,1
  ld    (HandleWhichPlayer?),a  
;
; bit 7     6     5     4     3     2     1     0
;		  softk softp hardk hardp right left  down  up  (action)
;		  /     .     '     ;     right left  down  up  (ControlsP1)
;		  S     A     W     Q     right left  down  up  (ControlsP2)
;
  call  GetNewPrControls      ;out a-> NewPrcontrols, changes: a, f
  bit   3,a                   ;right pressed ?
  jp    nz,.right
  bit   2,a                   ;left pressed ?
  jp    nz,.left
  bit   1,a                   ;down pressed ?
  jp    nz,.down
  bit   0,a                   ;up pressed ?
  jp    nz,.up
  and   %1111 0000            ;check if any attack button is pressed
  ret   z

  ld    c,sfxmenuselect
  call  SetSfx
  call  Samples

  ld    a,(mainmenuselectioniconpointer)
  inc   a
  ld    (mainmenuoptionselected?),a
  ret

  .right:
  ld    a,(mainmenuselectioniconpointer)
  and   %0000 0001
  ret   nz
  ld    a,(mainmenuselectioniconpointer)
  or    %0000 0001
  jr    .SetAndPlaysfx
  .left:
  ld    a,(mainmenuselectioniconpointer)
  and   %0000 0001
  ret   z
  ld    a,(mainmenuselectioniconpointer)
  and   %1111 1110
  jr    .SetAndPlaysfx
  .down:
  ld    a,(mainmenuselectioniconpointer)
  and   %0000 0010
  ret   nz
  ld    a,(mainmenuselectioniconpointer)
  or    %0000 0010
  jr    .SetAndPlaysfx
  .up:
  ld    a,(mainmenuselectioniconpointer)
  and   %0000 0010
  ret   z
  ld    a,(mainmenuselectioniconpointer)
  and   %1111 1101
;  ld    (mainmenuselectioniconpointer),a
;  ret

  .SetAndPlaysfx:
  ld    (mainmenuselectioniconpointer),a
  ld    c,sfxcursormove
  call  SetSfx
  jp    Samples

LineInt:
  ld    a,(whichlineint?)
  dec   a
  jp    z,MainMenuLineInt
  dec   a
  jp    z,StreetfighterLogoAnimationLineint

MainMenuLineInt:
  xor   a                     ;set s#0
  out   ($99),a
  ld    a,15+128
  out   ($99),a

  push  bc
  push  de
  push  hl 
  call  .handlePalette
  pop   hl
  pop   de
  pop   bc

  pop   af
  ei
  ret

.handlePalette:
  ld    a,(ylineint)
  cp    139
  jp    z,.firstLineInt
  cp    174
  jp    z,.secondLineInt
  cp    230
  jp    z,.thirdLineInt
  ret
  
  .firstLineInt:
  ld    a,174                 ;set 2nd lineint at y=174
  ld    (ylineint),a
  di
  out   ($99),a
  ld    a,19+128
  ei
  out   ($99),a  

  ld    hl,MainmenuSplitPalette2
  jp    setpalette

  .secondLineInt:
  ld    a,230                 ;set 3d lineint at y=230
  ld    (ylineint),a
  di
  out   ($99),a
  ld    a,19+128
  ei
  out   ($99),a  

  ld    hl,MainmenuSplitPalette3
  jp    setpalette

  .thirdLineInt:
  ld    a,139                 ;set 1st lineint at y=139
  ld    (ylineint),a
  di
  out   ($99),a
  ld    a,19+128
  ei
  out   ($99),a  

  ld    hl,MainmenuSplitPalette1
  jp    setpalette

MainmenuSplitPalette1:
  incbin "..\grapx\main menu\street fighter logo palette.plt"
MainmenuSplitPalette2:
  incbin "..\grapx\main menu\arcade and vs palette.plt"
MainmenuSplitPalette3:
  incbin "..\grapx\main menu\trials and option palette.plt"
 
mainmenurestorepage0:
	db    0,0,145,2
	db    0,0,145,0
	db    0,1,67,0
	db    0,0,$d0  

Mainmenuselectionicon1:
	db    8,0,0,3
	db    8,0,145,0
	db    118,0,32,0
	db    0,0,$98

Mainmenuselectionicon2:
	db    129,0,0,3
	db    129,0,145,0
	db    118,0,32,0
	db    0,0,$98

Mainmenuselectionicon3:
	db    8,0,0,3
	db    8,0,180,0
	db    118,0,32,0
	db    0,0,$98

Mainmenuselectionicon4:
	db    129,0,0,3
	db    129,0,180,0
	db    118,0,32,0
	db    0,0,$98



StreetFighterLogobottomErase:
	db    0,0,0,0
	db    0,0,112,0
	db    0,1,100,0
	db    0,0,$c0	
StreetFighterLogobottom1:
	db    0,0,0,0
	db    34,0,112,0
	db    52,0,30,0
	db    0,0,$d0
StreetFighterLogotopErase:
	db    0,0,0,0
	db    0,0,0,0
	db    52,0,30,0
	db    0,0,$c0	

CapcomLogoAnimation:
  call  setInterrupt1         ;this interrupt plays the music
  ld    a,Capcomlogoblock     ;then set the new song block
  ld		(backgroundmusicblock),a
  call  InitmusicLoopOnce

  ld    a,Capcomlogoblock
  call	block12      		      ;at address $4000 / page 1

  ld    a,0*32 + 31
  call  SetPage

  ld    hl,EraseScreen        ;erase page 0 AND 1
  call  docopygrauw
  call  VDPready              ;wait vdp ready
  call  VAdjustScreenReset

  ld    a,0
  ld    hl,$8000              ;address $08000 (page 1 screen 5)
  call  SetVdp_Write

	ld		hl,CapcomlogoAddress  ;level grapx in rom
	ld		bc,$0098              ;256 bytes to port $98
  
  exx
  ld    b,20                  ;40 lines to out
  .loop1:
  exx
	otir
  exx
  djnz  .loop1
  exx

  ld    de,currentpalette
  ld    bc,16*2
  ldir

  xor   a
  ld    (capcomlogostep),a
  ld    a,1
  ld    (fadeinout?),a        ;0=nothing, 1=fade in, 2=fade out
  xor   a
  ld    (fadeinoutstep),a
  ld    a,8                   ;amount of steps to darken (8 = total darkness)
  call  ScreenFadeInOut.GoSetPalette

.fadeloop:
  halt
  halt
  halt
  call  capcomlogoanimate
  cp    80
  ret   z
  jp    .fadeloop

capcomlogoanimate:
  ld    a,(capcomlogostep)
  inc   a
  ld    (capcomlogostep),a
  cp    10
  call  nc,ScreenFadeInOut
  ld    a,(capcomlogostep)
  
  cp    1
  jr    z,.logo1
  cp    40+2
  jr    z,.logo2
  cp    40+3
  jr    z,.logo3
  cp    40+4
  jr    z,.logo4
  cp    40+5
  jr    z,.logo3
  cp    40+6
  jr    z,.logo2
  cp    40+7
  jr    z,.logo1
  cp    64
  jr    z,.setfadeout
  ret

  .logo1:                     ;put capcom logo 1 in screen
  ld    hl,capcomlogo1copy
  jp    docopygrauw
  .logo2:                     ;put capcom logo 2 in screen
  ld    hl,capcomlogo2copy
  jp    docopygrauw
  .logo3:                     ;put capcom logo 3 in screen
  ld    hl,capcomlogo3copy
  jp    docopygrauw
  .logo4:                     ;put capcom logo 4 in screen
  ld    hl,capcomlogo4copy
  jp    docopygrauw

.setfadeout:  
  ld    a,2
  ld    (fadeinout?),a        ;0=nothing, 1=fade in, 2=fade out
  xor   a
  ld    (fadeinoutstep),a
  ret

capcomlogo1copy:
	db    0,0,0,1
	db    80,0,096,0
	db    96,0,20,0
	db    0,0,$d0
capcomlogo2copy:
	db    96,0,0,1
	db    80,0,096,0
	db    96,0,20,0
	db    0,0,$d0
capcomlogo3copy:
	db    0,0,20,1
	db    80,0,096,0
	db    96,0,20,0
	db    0,0,$d0
capcomlogo4copy:
	db    96,0,20,1
	db    80,0,096,0
	db    96,0,20,0
	db    0,0,$d0		

SetbackdropColor14Black:
  ld    a,14                  ;change backdrop color to black
  di
  out   ($99),a
  ld    a,7+128
  ei
  out   ($99),a
  ret

SetbackdropColor0Black:
  xor   a                     ;change backdrop color to black
  di
  out   ($99),a
  ld    a,7+128
  ei
  out   ($99),a
  ret

unFreezeControls:
  xor   a
  ld    (freezecontrols?),a
  ret

setscreenfadein:
  ld    a,1
  ld    (fadeinout?),a        ;0=nothing, 1=fade in, 2=fade out
  xor   a
  ld    (fadeinoutstep),a
  ld    a,8                   ;amount of steps to darken (8 = total darkness)
  call  ScreenFadeInOut.GoSetPalette
  ret

ResetFightvariables:
  xor   a
  ld    (P1RoundsWon),a
  ld    (P2RoundsWon),a
  ld    (AnnounceRoundStep),a
  ld    a,1                   ;0=no, 1=yes
  ld    (AnnounceRound?),a
  ld    (CurrentRound),a
  ret

ResetCharactersUnlocked:  
  xor   a
  ld    (character01unlocked?),a;bison
  ld    (character02unlocked?),a;titanic tim
  ld    (character03unlocked?),a;harman do elan
  ld    (character05unlocked?),a;mai shiranui
  ld    (character06unlocked?),a;guy
  ld    (character07unlocked?),a;dee jay
  ld    (character08unlocked?),a;blanka
  ld    (character09unlocked?),a;scorpio
  ld    (character10unlocked?),a;billy kane
  ld    (character12unlocked?),a;joe higashi
  ld    (character13unlocked?),a;damnd
  ld    (character14unlocked?),a;t hawk  
  ld    a,1
  ld    (character04unlocked?),a;fei long
  ld    (character11unlocked?),a;mike haggar
  ret  
  
ResetCharactersDefeated:
  xor   a
  ld    (character00defeated?),a;ryu
  ld    (character01defeated?),a;chunli
  ld    (character02defeated?),a;dhalsim
  ld    (character03defeated?),a;guile
  ld    (character04defeated?),a;ehonda
  ld    (character05defeated?),a;cammy
  ld    (character06defeated?),a;vega
  ld    (character07defeated?),a;mbison
  ld    (character08defeated?),a;titanic tim
  ld    (character09defeated?),a;harman do elan
  ld    (character10defeated?),a;fei long
  ld    (character11defeated?),a;mai shiranui
  ld    (character12defeated?),a;guy
  ld    (character13defeated?),a;deejay
  ld    (character14defeated?),a;blanka
  ld    (character15defeated?),a;scorion
  ld    (character16defeated?),a;billy kane
  ld    (character17defeated?),a;mike haggar
  ld    (character18defeated?),a;joe higashi
  ld    (character19defeated?),a;damnd
  ld    (character20defeated?),a;thawk
ret
ld a,1
  ld    (character00defeated?),a;ryu
  ld    (character01defeated?),a;chunli
  ld    (character02defeated?),a;dhalsim
  ld    (character03defeated?),a;guile
  ld    (character04defeated?),a;ehonda
  ld    (character05defeated?),a;cammy
  ld    (character06defeated?),a;vega

  ret  

ResetGeneralvariables:
  xor   a
  ld    (P1AttackingInAir?),a
  ld    (P1AttackingInAir?+1),a
  ld    (P2AttackingInAir?),a
  ld    (P2AttackingInAir?+1),a
  ld    (player1Action),a
  ld    (player2Action),a
  ld    (freezeplayer1?),a
  ld    (freezeplayer2?),a
  ld    (changeHpbarPlayer1?),a
  ld    (changeHpbarPlayer2?),a
  ld    (HpbarP1ChangeStep),a
  ld    (HpbarP2ChangeStep),a
  ld    (P1HpbarChangeTablepointer),a
  ld    (P2HpbarChangeTablepointer),a
  ld    (prepareActionP1),a
  ld    (fightended?),a
  ld    (fadeoutmusic?),a
  ld    (P1Dies?),a
  ld    (P2Dies?),a
  ld    (P1TossCooldown?),a
  ld    (P2TossCooldown?),a
  ld    a,80
  ld    (player1x),a
  ld    a,180
  ld    (player2x),a
  ld    a,PlayeryAtstart
  ld    (player1y),a
  ld    (player2y),a
  ld    a,188
  ld    (Player1Hp),a
  ld    (Player2Hp),a
  ld    a,94
  ld    (Player1HpbarHp),a
  ld    (Player2HpbarHp),a
  ld    a,208
  ld    (AdjustscoreboardPlayer1+sx),a
  ld    a,16
  ld    (AdjustscoreboardPlayer1+sy),a
  ld    (AdjustscoreboardPlayer2+sy),a
  ld    a,19
  ld    (AdjustscoreboardPlayer1+dx),a
  ld    a,74
  ld    (AdjustscoreboardPlayer1+dy),a
  ld    (AdjustscoreboardPlayer2+dy),a
  ld    a,226
  ld    (AdjustscoreboardPlayer2+sx),a
  ld    a,233
  ld    (AdjustscoreboardPlayer2+dx),a
  ld    a,4
  ld    (AdjustscoreboardPlayer1+nx),a
  ld    (AdjustscoreboardPlayer2+nx),a
  ret

Disable_Scr:  
  ld    a,(VDP_0+1)
  and   %10111111
  ld    (VDP_0+1),a
  di
  out   ($99),a
  ld    a,1+128
  ei
  out   ($99),a
  ret

Enable_Scr:   
  ld    a,(VDP_0+1)
  or    %01000000
  ld    (VDP_0+1),a
  di
  out   ($99),a
  ld    a,1+128
  ei
  out   ($99),a
  ret

setInterrupt1:                ;this one has music on the interrupt, and is not used during fights
  di
  ld    hl,InterruptHandler1  ;set new normal interrupt
  ld    ($38+1),hl
  ld    a,$c3
  ld    ($38),a
  ei 
  ret

setInterrupt2:                ;this one has no music on the interrupt, and is used during fights
  di
  ld    hl,InterruptHandler2  ;set new normal interrupt
  ld    ($38+1),hl
  ld    a,$c3
  ld    ($38),a
 
;  ld    a,(vdp_0)            ;set ei1
;  or    16                   ;ei1 checks for lineint and vblankint
;  ld    (vdp_0),a            ;ei0 (which is default at boot) only checks vblankint
;  out   ($99),a
;  ld    a,128
  ei
;  out   ($99),a
  ret

spritesoff:
  ld    a,(VDP_8)
  or    %00000010             ;sprites off
  ld    (VDP_8),a
  di
  out   ($99),a
  ld    a,8+128
  ei
  out   ($99),a
  ret

spriteson:
  ld    a,(VDP_8)
  and   %11111101             ;sprites on
  ld    (VDP_8),a
  di
  out   ($99),a
  ld    a,8+128
  ei
  out   ($99),a
  ret

VAdjustScreenDown:  
	ld		a,movescreendown      ;move screen 60 pixels down
  di
  out   ($99),a
	ld		a,23+128
	ei
	out		($99),a
  ret

VAdjustScreenManual:  
  di
  out   ($99),a
	ld		a,23+128
	ei
	out		($99),a
  ret

VAdjustScreenReset:  
	xor   a                     ;reset Vertical adjust
  di
  out   ($99),a
	ld		a,23+128
	ei
	out		($99),a
  ret  
  




;R#5   A14 A13 A12 A11 A10 A09   1   1     sprite attribute table (spat) base address
;R#11    0   0   0   0   0   0 A16 A15     Multiply * $200 to get your base address. Example:   1100 0011 = 195  195*$200= $18600
;color table base address is always $200 bytes lower than spat base address
;R#6     0   0 E16 E15 E14 E13 E12 E11     sprite character table. Multiply * $800 to get your base address. Example 0010 1110 = 46  46*$800 =$17000

WorldMapBuildUp1:
	db    0,0,0,0
	db    0,0,209,0
	db    0,1,3,0
	db    $01,0,$c0	
WorldMapBuildUp2:
	db    0,0,0,0
	db    0,0,0,0
	db    0,1,8,0
	db    $42,0,$c0
WorldMapBuildUp3:
	db    0,0,0,0
	db    0,0,54,1
	db    0,1,15,0
	db    $42,0,$c0		
WorldMapBuildUp4:
	db    0,0,0,0
	db    0,0,38,1
	db    0,1,16,0
	db    $22,0,$c0		
WorldMapBuildUp5:
	db    0,0,0,0
	db    0,0,28,1
	db    0,1,10,0
	db    $02,0,$c0
WorldMapBuildUp6:
	db    0,0,0,0
	db    0,0,218,1
	db    0,1,23,0
	db    $42,0,$c0					
WorldMapBuildUp7:
	db    0,0,0,0
	db    0,0,241,1
	db    0,1,5,0
	db    $22,0,$c0					
WorldMapBuildUp8:
	db    0,0,0,0
	db    0,0,246,1
	db    0,1,4,0
	db    $02,0,$c0					
WorldMapBuildUp9:
	db    0,0,0,0
	db    0,0,250,1
	db    0,1,6,0
	db    $01,0,$c0	
WorldMapBuildUp10:
	db    0,0,0,0
	db    0,0,0,1
	db    0,1,28,0
	db    $01,0,$c0	
copyWorldMapToPage1:
	db    0,0,0,0
	db    0,0,69,1
	db    0,1,113,0
	db    0,0,$d0
	
textthailand:
	db    194,0,212+14,0
	db    70,0,55+10,0
	db    28,0,04,0
	db    0,0,$98
thailanddot:	
	db    193,0,212,0
	db    76,0,50+8,0
	db    4,0,4,0
	db    0,0,$98	
textengland:
	db    38,0,30,0
	db    40,0,10+10,0
	db    27,0,04,0
	db    0,0,$98
englanddot:	
	db    193,0,212,0
	db    34,0,37,0
	db    4,0,4,0
	db    0,0,$98
texticeland:
	db    168,0,212+14,0
	db    08,0,15+10,0
	db    26,0,04,0
	db    0,0,$98
icelanddot:	
	db    193,0,212,0
	db    28,0,33,0
	db    4,0,4,0
	db    0,0,$98
textjapan:
	db    116,0,37+8,0
	db    110,0,13+10,0
	db    26,0,04,0
	db    0,0,$98
japandot:	
	db    193,0,212,0
	db    109,0,28+8,0
	db    4,0,4,0
	db    0,0,$98
textusa:
	db    213,0,030+8,0
	db    156,0,15+10,0
	db    26,0,04,0
	db    0,0,$98
usadot:	
	db    193,0,212,0
	db    179,0,23+8,0
	db    4,0,4,0
	db    0,0,$98
textjamaica:
	db    203,0,212+10,0
	db    204,0,54+07,0
	db    27,0,04,0
	db    0,0,$98
jamaicadot:	
	db    217,0,212,0
	db    224,0,49+7,0
	db    4,0,4,0
	db    0,0,$98
textbrazil:
	db    230,0,212+10,0
	db    219,0,76+07,0
	db    26,0,04,0
	db    0,0,$98
brazildot:	
	db    193,0,212,0
	db    235,0,91+7,0
	db    4,0,4,0
	db    0,0,$98	
textcambodia:
	db    222,0,212+14,0
	db    84,0,67+07,0
	db    34,0,04,0
	db    0,0,$98
cambodiadot:	
	db    207,0,212,0
	db    78,0,64+7,0
	db    4,0,4,0
	db    0,0,$98		
textengland2:
	db    38,0,30,0
	db    44,0,30+10,0
	db    27,0,04,0
	db    0,0,$98
england2dot:	
	db    202,0,212,0
	db    39,0,38,0
	db    4,0,4,0
	db    0,0,$98
textjapan2:
	db    116,0,37+8,0
	db    80,0,06+10,0
	db    26,0,04,0
	db    0,0,$98
japan2dot:	
	db    193,0,212,0
	db    111,0,24+8,0
	db    4,0,4,0
	db    0,0,$98
textcuba:
	db    185,0,212+10,0
	db    209,0,38+10,0
	db    18,0,04,0
	db    0,0,$98
cubadot:	
	db    212,0,212,0
	db    217,0,47+8,0
	db    4,0,4,0
	db    0,0,$98
textmexico:
	db    195,0,212+6,0
	db    170,0,39+10,0
	db    26,0,04,0
	db    0,0,$98
mexicodot:	
	db    193,0,212,0
	db    194,0,44,0
	db    4,0,4,0
	db    0,0,$98
copycountries:
  ld    ix,character01unlocked?
  ld    a,(ix+0)
  or    a                     ;bison / thailand
  call  nz,.thailand
  ld    a,(ix+1)
  or    a                     ;titanic tim / england
  call  nz,.england
  ld    a,(ix+2)
  or    a                     ;harman do elan / iceland
  call  nz,.iceland
  ld    a,(ix+4)
  or    a                     ;mai shiranui / japan
  call  nz,.japan
  ld    a,(ix+5)
  or    a                     ;guy / usa
  call  nz,.usa
  ld    a,(ix+6)
  or    a                     ;dee jay / jamaica
  call  nz,.jamaica
  ld    a,(ix+7)
  or    a                     ;blanka / brazil
  call  nz,.brazil
  ld    a,(ix+8)
  or    a                     ;scorpio / cambodia
  call  nz,.cambodia
  ld    a,(ix+9)
  or    a                     ;billy kane / london england
  call  nz,.england2
  ld    a,(ix+11)
  or    a                     ;joe higashi / japan
  call  nz,.japan2
  ld    a,(ix+12)
  or    a                     ;damnd / cuba
  call  nz,.cuba
  ld    a,(ix+13)
  or    a                     ;t hawk / mexico
  call  nz,.mexico
  ret
  .thailand:
  ld    bc,066 * 256   +   55 + 08
  ld    hl,textthailand
  ld    de,thailanddot
  jp    .setCountry
  .england:
  ld    bc,035 * 256   +   10 + 08
  ld    hl,textengland
  ld    de,englanddot
  jp    .setCountry
  .iceland:
  ld    bc,003 * 256   +   15 + 08
  ld    hl,texticeland
  ld    de,icelanddot
  jp    .setCountry
  .japan:
  ld    bc,105 * 256   +   13 + 08
  ld    hl,textjapan
  ld    de,japandot
  jp    .setCountry  
  .usa:
  ld    bc,148 * 256   +   15 + 08
  ld    hl,textusa
  ld    de,usadot
  jp    .setCountry  
  .jamaica:
  ld    bc,200 * 256   +   51 + 08
  ld    hl,textjamaica
  ld    de,jamaicadot
  jp    .setCountry  
  .brazil:
  ld    bc,214 * 256   +   73 + 08
  ld    hl,textbrazil
  ld    de,brazildot
  jp    .setCountry  
  .cambodia:
  ld    bc,084 * 256   +   64 + 08
  ld    hl,textcambodia
  ld    de,cambodiadot
  jp    .setCountry  
  .england2:
  ld    bc,041 * 256   +   30 + 08
  ld    hl,textengland2
  ld    de,england2dot
  jp    .setCountry  
  .japan2:
  ld    bc,075 * 256   +   06 + 08
  ld    hl,textjapan2
  ld    de,japan2dot
  jp    .setCountry  
  .cuba:
  ld    bc,201 * 256   +   38 + 08
  ld    hl,textcuba
  ld    de,cubadot
  jp    .setCountry  
  .mexico:
  ld    bc,165 * 256   +   39 + 08
  ld    hl,textmexico
  ld    de,mexicodot
  jp    .setCountry

  .setCountry:
  push  de
  ld    a,b
  ld    (Docopycountries+dx),a
  ld    a,c
  ld    (Docopycountries+dy),a
  push  hl
  ld    hl,Docopycountries
  call  DoCopygrauw
  pop   hl
  call  DoCopygrauw  
  pop   hl
  jp    DoCopygrauw



checkallplayersdefeated:    ;out: c=all players have been defeated
  ld    ix,character00defeated?
  ld    b,7
  
  .loop:
  ld    a,(ix)
  or    a
  ret   z                     ;this player is not yet defeated
  inc   ix
  djnz  .loop
  
  ld    b,14
  ld    iy,character01unlocked?
  
  .looptopandbottomrow:
  ld    a,(iy)                ;character unlocked ?
  or    a
  jp    z,.checknext          ;if the character is locked then dont check

  ld    a,(ix)
  or    a
  ret   z                     ;this player is not yet defeated
  
  .checknext:
  inc   ix
  inc   iy
  djnz  .looptopandbottomrow
  
  scf                         ;carry= all players have been defeated
  ret  

randomlyChooseP2:
  ;let computer randomly select an opponent
  ;21 characters
  ld    a,r
  and   63
	srl		a					            ;/2
	srl		a					            ;/2 (a now is 0 - 15)
	ld    b,a
	srl		a					            ;/2 (a now is 0 - 7)
	add   a,b 			            ;/2 (a now is 0 - 22)
  cp    21
  jr    c,.carry              ;allow characters 0 - 20 to pass
  ld    a,1
  .carry:

  ld    e,a
  ld    d,0
  ld    ix,character00defeated?
  add   ix,de
  .checkcharacterdefeated:
  ld    a,(ix)                ;is this character already defeated ?
  or    a
  jr    z,.notdefeated
  .checknextcharacter:
  inc   ix
  inc   e
  ld    a,e
  cp    21                    ;check last character
  jr    nz,.checkcharacterdefeated
  ld    e,0                   ;last character checked, then go back to first
  ld    ix,character00defeated?
  jr    .checkcharacterdefeated
  
  .notdefeated:
  ;now check if character is unlocked
  ld    a,e
  cp    7
  jr    c,.characterisunlocked
  cp    14
  jp    c,.checktoprow
  .checkbottomrow:            ;row starting with blanka
  sub   a,14
  ld    iy,character08unlocked?
  ld    b,0
  ld    c,a
  add   iy,bc
  ld    a,(iy)
  or    a                     ;character unlocked
  jr    nz,.characterisunlocked
  jr    .checknextcharacter
  
  .checktoprow:               ;row starting with mbison
  sub   a,7
  ld    iy,character01unlocked?
  ld    b,0
  ld    c,a
  add   iy,bc
  ld    a,(iy)
  or    a                     ;character unlocked
  jr    nz,.characterisunlocked
  jr    .checknextcharacter
  
  .characterisunlocked:
  ld    a,e
  ld    (player2),a
  ret

copydefeatedcharfaces:
	ld		a,Defeatedcharacterfacesblock
  call	block12      		      ;at address $4000 / page 1

  ld    hl,147*256 + 21       ;y,x of character 0, ryu (21,147)
  ld    iy,$4000+ 28*256      ;address of character 0 in ROM
  ld    ix,character00defeated?
  ld    b,7
  call  .rowloop

  ld    hl,116*256 + 21       ;y,x of character 7, mbison (21,116)
  ld    iy,$4000+ 0*256      ;address of character 7 in ROM
  ld    ix,character07defeated?
  ld    b,7
  call  .rowloop

	ld		a,Defeatedcharacterfacesblock+1
  call	block12      		      ;at address $4000 / page 1

  ld    hl,178*256 + 21       ;y,x of character 14, blanka (21,178)
  ld    iy,$4000+ (56-32)*256 ;address of character 14 in ROM
  ld    ix,character14defeated?
  ld    b,7
  call  .rowloop
  ret

  .rowloop:
  push  bc
  push  hl
  push  iy

  ld    a,(ix)
  or    a
  call  nz,.putcharacter

  pop   iy
  pop   hl
  ld    de,31
  add   hl,de
  ld    de,28
  add   iy,de
  inc   ix

  pop   bc
  djnz  .rowloop
  ret

  .putcharacter:
  ld    b,28                  ;28 lines per character face
  .loop:
  push  bc

  ld    a,0                   ;screen 8 page 0
  push  hl
  call  SetVdp_Write

  push  iy
	pop		hl                    ;level grapx in rom
	ld		c,$98                 ;256 bytes to port $98

  ld    b,28
  otir
  pop   hl
  
  ld    de,256
  add   hl,de
  add   iy,de
  
  pop   bc
  djnz  .loop
  ret

PutCharacterFace:
  ;dont put character if its a locked one
  call  SetPlayer1AndCheckIfUnlocked
  ret   nc

	ld		a,Unlockedcharacterfacesblock
  call	block12      		      ;at address $4000 / page 1

  ;find character to put
  ld    iy,$4000+ 3*256       ;address of character 7 in ROM
  ld    a,(P1IconTop+dx)
  inc   a
  ld    d,0
  ld    e,a
  add   iy,de

  ld    a,(P1IconTop+dy)
  cp    146
  ld    d,31
  jr    z,.rowfound
  ld    d,0
  jp    c,.rowfound

	ld		a,Unlockedcharacterfacesblock+1
  call	block12      		      ;at address $4000 / page 1
  ld    d,30
  .rowfound:  
  ld    e,0
  add   iy,de

  ;now put dx/dy
  ld    a,(P1IconTop+dx)
  inc   a
  ld    l,a
  ld    a,(P1IconTop+dy)
  inc   a
  ld    h,a
  
  .putcharacter:
  ld    b,28                  ;28 lines per character face
  .loop:
  push  bc

  ld    a,0                   ;screen 8 page 0
  push  hl
  call  SetVdp_Write

  push  iy
	pop		hl                    ;level grapx in rom
	ld		c,$98                 ;256 bytes to port $98

  ld    b,28
  otir
  pop   hl
  
  ld    de,256
  add   hl,de
  add   iy,de
  
  pop   bc
  djnz  .loop
  ret
  
RestoreCharacterFace:
  ld    a,(P1IconTopdxOld)
  ld    b,a
  ld    a,(P1IconTop+dx)
  cp    b
  jp    nz,.playermoved

  ld    a,(P1IconTopdyOld)
  ld    b,a
  ld    a,(P1IconTop+dy)
  cp    b
  jp    nz,.playermoved

  ld    a,(P1IconTop+dx)
  ld    (P1IconTopdxOld),a
  ld    a,(P1IconTop+dy)
  ld    (P1IconTopdyOld),a
  ret

  .playermoved:

  ld    ix,$4000+ 00*256      ;address of character 7 in ROM
  ld    de,28                 ;lenght of character face

  ;dont put character if it was a locked one
  .go:
  ld    a,(P1IconTopdxOld)
  ld    b,0
  sub   a,21
  .columloop:
  jp    c,.columfound
  add   ix,de
  inc   b
  sub   a,31
  jp    .columloop
  .columfound:

  ld    a,(P1IconTopdyOld)
  cp    146
  ld    a,b
  jr    z,.endcheckrow
  ld    a,7
  jp    c,.rowfound
  ld    a,14
  .rowfound:
  add   a,b
  
  .endcheckrow:
  ld    c,a                   ;store character number in c
  sub   7
  jp    c,.unlocked           ;player 0,1,2,3,4,5,6 are always unlocked
  ld    hl,character01unlocked?
  ld    d,0
  ld    e,a
  add   hl,de
  ld    a,(hl)                ;is this character unlocked ?
  or    a
  jp    z,.endputcharacter    ;character is locked
  .unlocked:
  
  ;dont put character unless it was a defeated character
  ld    e,c                   ;character number
  ld    d,0

  ld    hl,character00defeated?
  add   hl,de
  ld    a,(hl)
  or    a
  jr    z,.endputcharacter    ;not defeated, dont put
  
  ;put address of defeated character in iy
	ld		a,Defeatedcharacterfacesblock
  call	block12      		      ;at address $4000 / page 1

  push  ix
  pop   iy

  ld    a,(P1IconTopdyOld)
  cp    146
  ld    de,28*256
  jr    z,.rowfound2
  ld    de,00*256
  jp    c,.rowfound2
	ld		a,Defeatedcharacterfacesblock+1
  call	block12      		      ;at address $4000 / page 1
  ld    de,24*256
  .rowfound2:
  add   iy,de
 
  ;now put dx/dy
  ld    a,(P1IconTopdxOld)
  inc   a
  ld    l,a
  ld    a,(P1IconTopdyOld)
  inc   a
  ld    h,a
  
  .putcharacter:
  ld    b,28                  ;28 lines per character face
  .loop:
  push  bc

  ld    a,0                   ;screen 8 page 0
  push  hl
  call  SetVdp_Write

  push  iy
	pop		hl                    ;level grapx in rom
	ld		c,$98                 ;256 bytes to port $98

  ld    b,28
  otir
  pop   hl
  
  ld    de,256
  add   hl,de
  add   iy,de
  
  pop   bc
  djnz  .loop


  .endputcharacter:
  ld    a,(P1IconTop+dx)
  ld    (P1IconTopdxOld),a
  ld    a,(P1IconTop+dy)
  ld    (P1IconTopdyOld),a
  ret

CopyLockedFaces:
  ld    ix,character01unlocked?
  ld    a,21
  ld    (DoCopyLockedFaces+dx),a
  ld    a,116
  ld    (DoCopyLockedFaces+dy),a
  xor   a
  ld    (DoCopyLockedFaces+sx),a
  ld    (DoCopyLockedFaces+sy),a

  call  .put7
  
  xor   a
  ld    (DoCopyLockedFaces+sx),a
  ld    a,28
  ld    (DoCopyLockedFaces+sy),a
  ld    a,21
  ld    (DoCopyLockedFaces+dx),a
  ld    a,178
  ld    (DoCopyLockedFaces+dy),a
  
  .put7:
  ld    b,7
  .loop:
  exx

  ld    a,(ix)
  or    a
  ld    hl,DoCopyLockedFaces
  call  z,docopygrauw

  ld    a,(DoCopyLockedFaces+dx)
  add   a,31
  ld    (DoCopyLockedFaces+dx),a
  ld    a,(DoCopyLockedFaces+sx)
  add   a,28
  ld    (DoCopyLockedFaces+sx),a

  inc   ix
  exx
  djnz  .loop  
  ret

CharacterAndStageSelectArcadeMode:
  call  setInterrupt1         ;this interrupt plays the music
  ld    a,chselectmusicblock  ;then set the new song block
  ld		(backgroundmusicblock),a
  call  Initmusic

  ;if player may not choose character then she just won a fight, no need to set character select song
  ld    a,(player1mayChooseChar?)
  or    a
  call  z,Stopmusic

  halt
  call  Disable_Scr
  call  setscreen8
  xor   a
  call  VAdjustScreenManual
  ld    a,0*32 + 31
  call  SetPage

  ;copy select stage text and Font
  xor   a                     ;screen 8 page 0
  ld    hl,256*212
  call  SetVdp_Write
	ld		a,selectstageblock
  ld    b,44                  ;amount of lines to copy
  call  copygrahics
  ;copy all the unlocked character faces
  xor   a                     ;screen 8 page 0
  ld    hl,113 *256           ;copy to y=113
  call  SetVdp_Write
	ld		a,unlockedcharacterfacesblock  
  ld    b,64                  ;amount of lines to copy
  call  copygrahics
	ld		a,unlockedcharacterfacesblock+2
  ld    b,32                  ;amount of lines to copy
  call  copygrahics
  ;copy all the locked character faces
  ld    a,1                   ;screen 8 page 1
  ld    hl,0
  call  SetVdp_Write
	ld		a,lockedcharacterfacesblock
  ld    b,56                  ;amount of lines to copy
  call  copygrahics
  call  CopyLockedFaces
  ;copy all the defeated character faces
  call  copydefeatedcharfaces
  ;copy top and bottom part of the world map
  ld    hl,WorldMapBuildUp1
  call  docopygrauw
  ld    hl,WorldMapBuildUp2
  call  docopygrauw
  ;now copy 80% of the worldmap
  xor   a                     ;screen 8 page 0
  ld    hl,256*8
  call  SetVdp_Write
	ld		a,worldmapblock  
  ld    b,64                  ;amount of lines to copy
  call  copygrahics
  ld    hl,WorldMapBuildUp3
  call  docopygrauw
	ld		a,worldmapblock+2
  ld    b,41                  ;amount of lines to copy
  call  copygrahics
  ;copy all unlocked countries
  call  copycountries
  ;copy this part of the worldmap to page 1
  ld    hl,copyWorldMapToPage1
  call  docopygrauw
  ;now copy remaining 20% of the worldmap to page 1
  ld    a,1                   ;screen 8 page 1
  ld    hl,256*182
  call  SetVdp_Write
	ld		a,worldmapblock+2  
  ld    b,23                  ;amount of lines to copy
  call  copygrahics2
  ld    hl,WorldMapBuildUp4
  call  docopygrauw
	ld		a,worldmapblock+4
  ld    b,13                  ;amount of lines to copy
  call  copygrahics
  ld    hl,WorldMapBuildUp5
  call  docopygrauw
  ld    hl,WorldMapBuildUp6
  call  docopygrauw
  ld    hl,WorldMapBuildUp7
  call  docopygrauw
  ld    hl,WorldMapBuildUp8
  call  docopygrauw
  ld    hl,WorldMapBuildUp9
  call  docopygrauw

  ld    a,(player1mayChooseChar?)
  or    a
  jp    nz,.player1mayChoose
  ld    a,1
  ld    (P1ButtonPressed?),a
  jr    .player1mayNotChoose

  .player1mayChoose:  
  xor   a
  ld    (P1ButtonPressed?),a
  ld    a,20
  ld    (P1IconTop+dx),a
  ld    (P1IconTopdxOld),a
  ld    a,146
  ld    (P1IconTop+dy),a
  ld    (P1IconTopdyOld),a

  .player1mayNotChoose:
  halt
  call  Enable_Scr
    
  .step1:                     ;step 1: player 1 chooses character
  call  RestoreCharacterFace  ;put defeated/undefeated character face if player just moved
  call  PutCharacterFace      ;put undefeated/unlocked character face if player is standing on an unlocked character
  call  P1Puticonsprites
  ld    a,(P1ButtonPressed?)
  or    a
  ld    a,0
  ld    (P1ButtonPressed?),a
  jr    z,.P1ButtonNotpressed
  call  SetPlayer1AndCheckIfUnlocked
  jr    c,.step2              ;out c=player is unlocked
  .P1ButtonNotpressed:

  ld    b,5
  call  .waitloop

  call  P1restoreBackgroundIcons
  ld    b,2
  call  .waitloop
  jr    .step1

  .step2:                     ;step 2: little waitloop in which face of character gets copied
  ld    a,(player1mayChooseChar?)
  or    a
  jr    z,.nosample1

  ld    a,0
  ld    (player1mayChooseChar?),a

  ld    c,sfxcharacterselected
  call  SetSfx
  call  Samples  
  .nosample1:

  ld    a,1
  ld    (fadeoutmusic?),a

  call  putcharacterplayer1
  call  putcharacterplayer2

  ld    b,100                 ;after picking character wait a short while
  call  .waitloop

  ld    c,sfxplane
  call  SetSfx
  call  Samples  

  call  P1restoreBackgroundIcons

;  xor   a
;  ld    (P2ButtonPressed?),a
;  ld    a,206
;  ld    (P2IconTop+dx),a
;  ld    a,146
;  ld    (P2IconTop+dy),a

;  .step3:                     ;step 3: player 2 chooses character
;  call  P2Puticonsprites
;  ld    b,5
;  call  .waitloop

;  ld    a,(P2ButtonPressed?)
;  or    a
;  ld    a,0
;  ld    (P2ButtonPressed?),a
;  jr    z,.P2ButtonNotpressed
;  call  SetPlayer2AndCheckIfUnlocked
;  jr    c,.step4              ;out c=player is unlocked
;  .P2ButtonNotpressed:
;  call  P2restoreBackgroundIcons
;  ld    b,2
;  call  .waitloop
;  jr    .step3


;  ld    b,30
;  call  .waitloop
;  call  P2restoreBackgroundIcons

  .step5:                     ;step 5: switch to worldmap overview with both characters in screen
  ld    hl,WorldMapBuildUp10
  call  docopygrauw
  halt
  ld    a,1*32 + 31
  call  SetPage

  ld    a,69
  ld    (VAdjust),a
  call  VAdjustScreenManual

  ld    b,8
  call  .waitloop

  xor   a
  ld    (fadeoutmusic?),a
  ld    a,StartFightTuneblock ;set song block
  ld		(backgroundmusicblock),a
  call  InitmusicLoopOnce

  ld    b,20
  call  .waitloop

  ld    a,-8
  ld    (NextStageText+dx),a

  ld    hl,NextText
  call  docopygrauw
  ld    hl,NextErase
  call  docopygrauw

  .step6:                     ;step 6: scroll screen down and put Select stage text
  ;scroll down screen
  ld    a,(VAdjust)
  dec   a
  cp    28
  jr    z,.step7
  ld    (VAdjust),a
  .endsetVAdjust:
  call  VAdjustScreenManual
  halt

  ld    a,(NextStageText+dx)
  sub   a,8
  jr    c,.borderleftreached
  ld    (NextStageText+dx),a
   .borderleftreached:
  ld    hl,NextStageText
  call  docopygrauw
  jp    .step6

  .step7:

;  ld    hl,NextStage1
;  call  docopygrauw  

;  ld    a,(NextStage1+dx)
;  add   a,10
;  ld    (NextStage1+dx),a
;  cp    156
;  jp    z,.step7



  ;connect computer character with his/her country and put this value in StageSelectPointer
  ld    b,0
  ld    a,(player2)
  ld    hl,CountryCoordinates+3
  ld    de,4
  .findcountryloop:
  cp    (hl)
  jp    z,.countryfound
  inc   b
  add   hl,de
  jr    .findcountryloop
  .countryfound:
  ld    a,b
  ld    (StageSelectPointer),a

;  xor   a
;  ld    (P1ButtonPressed?),a
  ld    a,255
  ld    (oldbackground),a
  call  SetCountryIconCoordinates
  ld    hl,backupBackgroundBehindLetter1
  call  DoCopygrauw
  ld    hl,backupBackgroundBehindLetter2
  call  DoCopygrauw
  call  putPlayerCountryName

;  jp    .step6

;  .step7:

  ld    b,36
  .step8:                     ;keep flashing country select icon for a while, then go to fight
  exx

  call  PutCountryselecticonsprites
  ld    b,5
  call  .waitloopstep8
  ld    a,(P1ButtonPressed?)
  or    a
  jr    nz,.step9
  call  restoreBackgroundCountryselecicons
  ld    b,2
  call  .waitloopstep8
  call  putPlayerCountryName

  exx
  djnz  .step8

  .step9:  
  call  setscreen5
  ret

  .waitloopstep8:
  push  bc
  halt
;	call  PopulateControls
;  call  CountrySelectHandleControls
  call  SetCountryIconCoordinates
  pop   bc
  djnz  .waitloopstep8
  ret

  .waitloop:
  push  bc
  halt
	call  PopulateControls

  ld    a,(player1mayChooseChar?)
  or    a
  call  nz,P1CharSelHandleControls

  pop   bc
  djnz  .waitloop
  ret

NextText:
	db    203,0,249,0
	db    60,0,49,1
	db    53,0,7,0
	db    0,0,$90
NextErase:
	db    203-53,0,249,0
	db    203,0,249,0
	db    53,0,7,0
	db    0,0,$90































	
CharacterAndStageSelectVsMode:
  call  setInterrupt1         ;this interrupt plays the music
  ld    a,chselectmusicblock  ;then set the new song block
  ld		(backgroundmusicblock),a
  call  Initmusic

  halt
  call  Disable_Scr
  call  setscreen8
  xor   a
  call  VAdjustScreenManual
  ld    a,0*32 + 31
  call  SetPage

  ;set sprite tables
;	ld		a,%1101 1111    ;spr att table to $1ee00
;	di
;	out		($99),a
;	ld		a,5+128
;	out		($99),a
;	ld		a,%0000 0011
;	ld		(vdp_8+3),a
;	out		($99),a
;	ld		a,11+128
;	out		($99),a

;	ld		a,%0011 1111          ;spr chr table to $1f800
;	ld		(vdp_0+6),a
;	out		($99),a
;	ld		a,6+128
;	ei
;	out		($99),a

;	ld		a,1
;	ld		hl,$ee00-$200         ;sprite color table in VRAM
;	call	SetVdp_Write	
;	ld		hl,iconsColor
;  ld    c,$98
;  ld    b,0
;  otir
;  otir
  
;	ld		a,1
;	ld		hl,$ee00		          ;sprite attribute table in VRAM
;	call	SetVdp_Write	
;	ld		hl,spat			          ;sprite attribute table
;  ld    c,$98
;  ld    b,128
;  otir

;	ld		a,1
;	ld		hl,$f800              ;sprite character table in VRAM
;	call	SetVdp_Write	
;	ld    hl,iconsChar
;	ld    c,$98
;  ld    b,0
;  otir
;  otir
;  otir
;  otir
  ;/set sprite tables

  ;copy select stage text and Font
  xor   a                     ;screen 8 page 0
  ld    hl,256*212
  call  SetVdp_Write
	ld		a,selectstageblock
  ld    b,44                  ;amount of lines to copy
  call  copygrahics
  ;copy all the unlocked character faces
  xor   a                     ;screen 8 page 0
  ld    hl,113 *256           ;copy to y=113
  call  SetVdp_Write
	ld		a,unlockedcharacterfacesblock  
  ld    b,64                  ;amount of lines to copy
  call  copygrahics
	ld		a,unlockedcharacterfacesblock+2
  ld    b,32                  ;amount of lines to copy
  call  copygrahics
  ;copy all the locked character faces
  ld    a,1                   ;screen 8 page 1
  ld    hl,0
  call  SetVdp_Write
	ld		a,lockedcharacterfacesblock
  ld    b,56                  ;amount of lines to copy
  call  copygrahics
  call  CopyLockedFaces
  ;copy top and bottom part of the world map
  ld    hl,WorldMapBuildUp1
  call  docopygrauw
  ld    hl,WorldMapBuildUp2
  call  docopygrauw
  ;now copy 80% of the worldmap
  xor   a                     ;screen 8 page 0
  ld    hl,256*8
  call  SetVdp_Write
	ld		a,worldmapblock  
  ld    b,64                  ;amount of lines to copy
  call  copygrahics
  ld    hl,WorldMapBuildUp3
  call  docopygrauw
	ld		a,worldmapblock+2
  ld    b,41                  ;amount of lines to copy
  call  copygrahics
  ;copy all unlocked countries
  call  copycountries
  ;copy this part of the worldmap to page 1
  ld    hl,copyWorldMapToPage1
  call  docopygrauw
  ;now copy remaining 20% of the worldmap to page 1
  ld    a,1                   ;screen 8 page 1
  ld    hl,256*182
  call  SetVdp_Write
	ld		a,worldmapblock+2  
  ld    b,23                  ;amount of lines to copy
  call  copygrahics2
  ld    hl,WorldMapBuildUp4
  call  docopygrauw
	ld		a,worldmapblock+4
  ld    b,13                  ;amount of lines to copy
  call  copygrahics
  ld    hl,WorldMapBuildUp5
  call  docopygrauw

  ld    hl,WorldMapBuildUp6
  call  docopygrauw
  ld    hl,WorldMapBuildUp7
  call  docopygrauw
  ld    hl,WorldMapBuildUp8
  call  docopygrauw
  ld    hl,WorldMapBuildUp9
  call  docopygrauw

  xor   a
  ld    (P1ButtonPressed?),a
  ld    a,20
  ld    (P1IconTop+dx),a
  ld    a,146
  ld    (P1IconTop+dy),a

  halt
  call  Enable_Scr
  .step1:                     ;step 1: player 1 chooses character
  call  P1Puticonsprites
  ld    a,(P1ButtonPressed?)
  or    a
  ld    a,0
  ld    (P1ButtonPressed?),a
  jr    z,.P1ButtonNotpressed
  call  SetPlayer1AndCheckIfUnlocked
  jr    c,.step2              ;out c=player is unlocked
  .P1ButtonNotpressed:
  ld    b,5
  call  .P1waitloop
  call  P1restoreBackgroundIcons
  ld    b,2
  call  .P1waitloop
  jr    .step1

  .step2:                     ;step 2: little waitloop in which face of character gets copied
  ld    c,sfxcharacterselected
  call  SetSfx
  call  Samples

  call  putcharacterplayer1

  ld    b,30
  call  .waitloop
  call  P1restoreBackgroundIcons

  xor   a
  ld    (P2ButtonPressed?),a
  ld    a,206
  ld    (P2IconTop+dx),a
  ld    a,146
  ld    (P2IconTop+dy),a

  .step3:                     ;step 3: player 2 chooses character
  call  P2Puticonsprites
  ld    a,(P2ButtonPressed?)
  or    a
  ld    a,0
  ld    (P2ButtonPressed?),a
  jr    z,.P2ButtonNotpressed
  call  SetPlayer2AndCheckIfUnlocked
  jr    c,.step4              ;out c=player is unlocked
  .P2ButtonNotpressed:
  ld    b,5
  call  .P2waitloop
  call  P2restoreBackgroundIcons
  ld    b,2
  call  .P2waitloop
  jr    .step3

  .step4:                     ;step 4: little waitloop in which face of character gets copied
  ld    c,sfxcharacterselected
  call  SetSfx
  call  Samples
  
  call  putcharacterplayer2

  ld    b,30
  call  .waitloop
  call  P2restoreBackgroundIcons

  .step5:                     ;step 5: switch to worldmap overview with both characters in screen
  ld    hl,WorldMapBuildUp10
  call  docopygrauw
  halt
  ld    a,1*32 + 31
  call  SetPage

  ld    a,69
  ld    (VAdjust),a
  call  VAdjustScreenManual
  ld    b,20
  call  .waitloop

  ld    a,-8
  ld    (SelectStage2+dx),a
  ld    a,6
  ld    (SelectStage1+dx),a

  .step6:                     ;step 6: scroll screen down and put Select stage text
  ;scroll down screen
  ld    a,(VAdjust)
  dec   a
  cp    28
  jr    z,.endsetVAdjust
  ld    (VAdjust),a
  .endsetVAdjust:
  call  VAdjustScreenManual
  halt

  ld    a,(SelectStage2+dx)
  sub   a,8
  jr    c,.borderleftreached
  ld    (SelectStage2+dx),a
  ld    hl,SelectStage2
  call  docopygrauw
  jp    .step6

  .borderleftreached:
  ld    hl,SelectStage1
  call  docopygrauw  

  ld    a,(SelectStage1+dx)
  add   a,10
  ld    (SelectStage1+dx),a
  cp    156
  jp    z,.step7

  xor   a
  ld    (P1ButtonPressed?),a
  ld    a,-11+120
  ld    (CountrySelecticon+dx),a
  ld    a,67+43
  ld    (CountrySelecticon+dy),a
  ld    a,255
  ld    (oldbackground),a
  xor   a                     ;ryu stage
  ld    (background),a
  ld    a,12                  ;set pointer on ryu stage (japan)
  ld    (StageSelectPointer),a
  jp    .step6

  .step7:
  ld    hl,backupBackgroundBehindLetter1
  call  DoCopygrauw
  ld    hl,backupBackgroundBehindLetter2
  call  DoCopygrauw
  call  putPlayerCountryName

  .step8:                     ;step 1: player 1 chooses country/stage
  call  PutCountryselecticonsprites
  ld    b,5
  call  .waitloopstep8
  ld    a,(P1ButtonPressed?)
  or    a
  jr    nz,.step9
  call  restoreBackgroundCountryselecicons
  ld    b,2
  call  .waitloopstep8
  call  putPlayerCountryName
  jr    .step8

  .step9:
  ld    c,sfxcharacterselected
  call  SetSfx
  call  Samples  

  ld    b,40
  .waitloadstep9:
  halt
  djnz  .waitloadstep9
  call  setscreen5
  call  stopmusic
  ret

  .waitloopstep8:
  push  bc
  halt
	call  PopulateControls
  call  CountrySelectHandleControls
  call  SetCountryIconCoordinates
  pop   bc
  djnz  .waitloopstep8
  ret

  .waitloop:
  push  bc
  halt
	call  PopulateControls
  pop   bc
  djnz  .waitloop
  ret

  .P1waitloop:
  push  bc
  halt
	call  PopulateControls
  call  P1CharSelHandleControls
  pop   bc
  djnz  .P1waitloop
  ret

  .P2waitloop:
  push  bc
  halt
	call  PopulateControls
  call  P2CharSelHandleControls
  pop   bc
  djnz  .P2waitloop
  ret

putPlayerCountryName:
  ld    a,(oldbackground)
  ld    b,a
  ld    a,(background)
  cp    b
  ret   z
  ld    (oldbackground),a
  
  ld    ix,NameAndStage00Text
  add   a,150
  .findloop:
  cp    (ix)
  jr    z,.GosetNameAndStage
  inc   ix
  jp    .findloop

  .GosetNameAndStage:
  inc   ix
  ld    a,(ix)                ;dx
  ld    (putletter+dx),a
  .loop:
  inc   ix
  ld    a,(ix)                ;letter
  cp    254                   ;go to stage text ?
  jp    z,.stagetext
  cp    255                   ;end ?
  jp    z,.endtext
  cp    $20                   ;space ?
  jr    z,.space
  cp    6
  jp    c,.movebacktext
  
  sub   $61                   ;$61 = "a"

  ld    hl,CountryTextTable
  ld    d,0
  ld    e,a
  add   hl,de                 ;go to offset of letter
  ld    a,(hl)                ;sx
  ld    (putletter+sx),a
  ld    b,a

  inc   hl
  ld    a,(hl)
  sub   a,b
  ld    (putletter+nx),a

  ld    hl,putletter
  call  docopygrauw

  ld    a,(putletter+nx)
  ld    b,a
  ld    a,(putletter+dx)
  add   a,b
  ld    (putletter+dx),a
  jp    .loop

  .stagetext:
  ld    ix,StageText-1
  jp    .loop

  .space:
  ld    a,(putletter+dx)
  add   a,8
  ld    (putletter+dx),a
  jp    .loop

  .movebacktext:
  ld    b,a
  ld    a,(putletter+dx)
  sub   a,b
  ld    (putletter+dx),a
  jp    .loop

  .endtext:
  ld    hl,copytotalplayercountryname
  call  docopygrauw
  ld    hl,restorebackupBackgroundBehindLetter1
  call  docopygrauw
  ret

CountryTextTable:             ;sx letter(a,b,c,d,e,.....)
  db    0,9,19,29,41,52,62,72,84,92,103,115,123,135,147,156,166,166,176,185,196,207,216,227,237,247,0
NameAndStage00Text: db  150,87,"ryu",254
NameAndStage01Text: db  151,72,"c",2,"hu",2,"n",1,"li",254
NameAndStage02Text: db  152,67,"d",1,"h",2,"alsi",2,"m",254
NameAndStage03Text: db  153,81,"gu",1,"i",1,"le",254
NameAndStage04Text: db  154,67,"eho",1,"n",2,"d",2,"a",254
NameAndStage05Text: db  155,74,"c",1,"am",1,"my",254
NameAndStage06Text: db  156,82,"v",3,"e",1,"g",2,"a",254
NameAndStage07Text: db  157,70,"mbiso",1,"n",254
NameAndStage08Text: db  158,52,"t",2,"it",3,"an",1,"i",1,"c t",2,"i",2,"m",254
NameAndStage09Text: db  159,44,"h",2,"ar",1,"m",2,"an ",3,"do ",4,"e",2,"lan",3,254
NameAndStage10Text: db  160,67,"f",2,"e",2,"i lo",1,"ng",1,254
NameAndStage11Text: db  161,44,"m",1,"ai s",1,"h",1,"i",2,"r",1,"anu",1,"i",254
NameAndStage12Text: db  162,84,"guy",254
NameAndStage13Text: db  163,69,"d",2,"e",2,"e j",2,"ay",254
NameAndStage14Text: db  164,73,"blank",2,"a",254
NameAndStage15Text: db  165,66,"sco",1,"r",1,"p",1,"i",1,"o",1,"n",254
NameAndStage16Text: db  166,50,"bi",1,"lly k",1,"an",2,"e",254
NameAndStage17Text: db  167,51,"m",1,"i",1,"k",2,"e h",1,"agg",2,"ar",254
NameAndStage18Text: db  168,52,"j",1,"o",2,"e h",1,"i",1,"g",2,"as",1,"h",1,"i",254
NameAndStage19Text: db  169,76,"d",1,"am",1,"n",2,"d",254
NameAndStage20Text: db  170,75,"th",2,"aw",1,"k",254
StageText:          db  " st",3,"ag",3,"e",255

backupBackgroundBehindLetter1:
	db    44,0,72,1
	db    44,0,0,1
	db    176,0,9,0
	db    0,0,$d0
backupBackgroundBehindLetter2:
	db    44,0,72,1
	db    44,0,9,1
	db    176,0,9,0
	db    0,0,$d0
restorebackupBackgroundBehindLetter1:
	db    44,0,9,1
	db    44,0,0,1
	db    176,0,9,0
	db    0,0,$d0
copytotalplayercountryname:
	db    44,0,0,1
	db    44,0,72,1
	db    176,0,9,0
	db    0,0,$d0

P1restoreBackgroundIcons:
  ;put backup of background back in screen
  ld    hl,P1restoreBackgroundTop
  call  DoCopygrauw
  ld    hl,P1restoreBackgroundBottom
  call  DoCopygrauw
  ret  
P2restoreBackgroundIcons:
  ;put backup of background back in screen
  ld    hl,P2restoreBackgroundTop
  call  DoCopygrauw
  ld    hl,P2restoreBackgroundBottom
  call  DoCopygrauw
  ret  
restoreBackgroundCountryselecicons:
  ;put backup of background back in screen
  ld    hl,restoreBackgroundCountrySelect
  call  DoCopygrauw
  ret  

P1Puticonsprites:
  ;first make backup of background where icon will be pasted
  ld    a,(P1IconTop+dx)
  ld    (P1restoreBackgroundTop+dx),a
  ld    (P1restoreBackgroundBottom+dx),a
  ld    (P1BackupBackgroundTop+sx),a
  ld    (P1BackupBackgroundBottom+sx),a
  ld    a,(P1IconTop+dy)
  ld    (P1restoreBackgroundTop+dy),a
  ld    (P1BackupBackgroundTop+sy),a
  add   a,25
  ld    (P1BackupBackgroundBottom+sy),a
  ld    (P1restoreBackgroundBottom+dy),a

  ld    hl,P1BackupBackgroundTop
  call  DoCopygrauw
  ld    hl,P1BackupBackgroundBottom
  call  DoCopygrauw

  ;now put icon over background
  ld    hl,P1IconTop
  call  DoCopygrauw
  ld    a,(P1IconTop+dx)
  ld    (P1IconBottom+dx),a
  ld    a,(P1IconTop+dy)
  add   a,25
  ld    (P1IconBottom+dy),a
  ld    hl,P1IconBottom
  call  DoCopygrauw
  ret

P2Puticonsprites:
  ;first make backup of background where icon will be pasted
  ld    a,(P2IconTop+dx)
  ld    (P2restoreBackgroundTop+dx),a
  ld    (P2restoreBackgroundBottom+dx),a
  ld    (P2BackupBackgroundTop+sx),a
  ld    (P2BackupBackgroundBottom+sx),a
  ld    a,(P2IconTop+dy)
  ld    (P2restoreBackgroundTop+dy),a
  ld    (P2BackupBackgroundTop+sy),a
  add   a,25
  ld    (P2BackupBackgroundBottom+sy),a
  ld    (P2restoreBackgroundBottom+dy),a

  ld    hl,P2BackupBackgroundTop
  call  DoCopygrauw
  ld    hl,P2BackupBackgroundBottom
  call  DoCopygrauw

  ;now put icon over background
  ld    hl,P2IconTop
  call  DoCopygrauw
  ld    a,(P2IconTop+dx)
  ld    (P2IconBottom+dx),a
  ld    a,(P2IconTop+dy)
  add   a,25
  ld    (P2IconBottom+dy),a
  ld    hl,P2IconBottom
  call  DoCopygrauw
  ret

PutCountryselecticonsprites:
  ;first make backup of background where icon will be pasted
  ld    a,(CountrySelecticon+dx)
  ld    (restoreBackgroundCountrySelect+dx),a
  ld    (BackupBackgroundCountrySelect+sx),a

  ld    a,(CountrySelecticon+dy)
  ld    (restoreBackgroundCountrySelect+dy),a
  ld    (BackupBackgroundCountrySelect+sy),a

  ld    hl,BackupBackgroundCountrySelect
  call  DoCopygrauw

  ;now put icon over background
  ld    hl,CountrySelecticon
  call  DoCopygrauw
  ret

SetPlayer2AndCheckIfUnlocked:
  ld    ix,P2IconTop
  ld    iy,player2
  jr    SetPlayer1AndCheckIfUnlocked.go

SetPlayer1AndCheckIfUnlocked: ;out: c=unlocked
  ld    ix,P1IconTop
  ld    iy,player1

  .go:
  ld    a,(ix+dx)
  ld    b,0
  sub   a,21
  .columloop:
  jp    c,.columfound
  inc   b
  sub   a,31
  jp    .columloop
  .columfound:

  ld    a,(ix+dy)
  cp    146
  ld    a,b
  jr    z,.endcheckrow
  ld    a,7
  jp    c,.rowfound
  ld    a,14
  .rowfound:
  add   a,b
  
  .endcheckrow:               ;player number is in a
  ld    (iy),a
  sub   7
  jp    c,.unlocked           ;player 0,1,2,3,4,5,6 are always unlocked
  ld    hl,character01unlocked?
  ld    d,0
  ld    e,a
  add   hl,de
  ld    a,(hl)                ;is this character unlocked ?
  or    a
  ret   z
  .unlocked:
  scf
  ret

P2CharselHandleControls:
  ld    a,2
  ld    (HandleWhichPlayer?),a  
  ld    ix,P2IconTop
  ld    iy,P2ButtonPressed?
  jp    P1CharselHandleControls.go
  
P1CharselHandleControls:
  ld    a,1
  ld    (HandleWhichPlayer?),a  
  ld    ix,P1IconTop
  ld    iy,P1ButtonPressed?
  .go:
;
; bit 7     6     5     4     3     2     1     0
;		  softk softp hardk hardp right left  down  up  (action)
;		  /     .     '     ;     right left  down  up  (ControlsP1)
;		  S     A     W     Q     right left  down  up  (ControlsP2)
;
  call  GetNewPrControls      ;out a-> NewPrcontrols, changes: a, f
  bit   3,a                   ;right pressed ?
  jp    nz,.right
  bit   2,a                   ;left pressed ?
  jp    nz,.left
  bit   1,a                   ;down pressed ?
  jp    nz,.down
  bit   0,a                   ;up pressed ?
  jp    nz,.up
  and   %1111 0000            ;check if any attack button is pressed
  ret   z
  ;button is pressed, check if this character is unlocked
  ld    (iy),1
  ret

  .right:
  ld    a,(ix+dx)
  add   a,31
  cp    230
  ret   nc
  ld    (ix+dx),a
  jr    .sfx
  
  .left:
  ld    a,(ix+dx)
  sub   a,31
  ret   c
  ld    (ix+dx),a
  jr    .sfx

  .up:
  ld    a,(ix+dy)
  sub   a,31
  cp    100
  ret   c
  ld    (ix+dy),a
  jr    .sfx
  
  .down:
  ld    a,(ix+dy)
  add   a,31
  cp    198
  ret   nc
  ld    (ix+dy),a
;  jr    .sfx

  .sfx:
  ld    c,sfxcursormove
  call  SetSfx
  jp    Samples


CountrySelectHandleControls:
  ld    a,1
  ld    (HandleWhichPlayer?),a  
;
; bit 7     6     5     4     3     2     1     0
;		  softk softp hardk hardp right left  down  up  (action)
;		  /     .     '     ;     right left  down  up  (ControlsP1)
;		  S     A     W     Q     right left  down  up  (ControlsP2)
;
  call  GetNewPrControls      ;out a-> NewPrcontrols, changes: a, f
  bit   3,a                   ;right pressed ?
  jp    nz,.right
  bit   2,a                   ;left pressed ?
  jp    nz,.left
  and   %1111 0000            ;check if any attack button is pressed
  ret   z
  ld    a,1
  ld    (P1ButtonPressed?),a
  ret

  .right:
  ld    a,(StageSelectPointer)
  inc   a
  cp    21
  ret   z
  ld    (StageSelectPointer),a

  .rightcheck:
  ;check if this country is available
  add   a,a
  add   a,a                   ;*4
  
  ld    hl,CountryCoordinates+2
  ld    d,0
  ld    e,a
  add   hl,de

  ld    a,(hl)                ;country of which player ?
  cp    8
  jp    c,.rightcheckfirst7
  cp    15
  jp    c,.Available          ;if country belongs to the player 8 - 14 then no need to check, those countries are always available
  jp    .rightchecklast7

  .rightcheckfirst7:
  sub   a,1
  
  ld    d,0
  ld    e,a
  ld    hl,character01unlocked?
  add   hl,de
  ld    a,(hl)                ;is this character unlocked ?
  or    a
  jp    nz,.Available

  ld    a,(StageSelectPointer)
  inc   a
  ld    (StageSelectPointer),a
  jp    .rightcheck

  .rightchecklast7:
  sub   a,15
  
  ld    d,0
  ld    e,a
  ld    hl,character08unlocked?
  add   hl,de
  ld    a,(hl)                ;is this character unlocked ?
  or    a
  jp    nz,.Available

  ld    a,(StageSelectPointer)
  inc   a
  ld    (StageSelectPointer),a
  cp    21
  jp    nz,.rightcheck

  ld    a,19
  ld    (StageSelectPointer),a
  ret

   
  .left:
  ld    a,(StageSelectPointer)
  dec   a
  ret   m
  ld    (StageSelectPointer),a
  .leftcheck:
  ;check if this country is available
  add   a,a
  add   a,a                   ;*4
  
  ld    hl,CountryCoordinates+2
  ld    d,0
  ld    e,a
  add   hl,de

  ld    a,(hl)                ;country of which player ?
  cp    8
  jp    c,.leftcheckfirst7
  cp    15
  jp    c,.Available          ;if country belongs to the player 8 - 14 then no need to check, those countries are always available
  jp    .leftchecklast7

  .leftcheckfirst7:
  sub   a,1
  
  ld    d,0
  ld    e,a
  ld    hl,character01unlocked?
  add   hl,de
  ld    a,(hl)                ;is this character unlocked ?
  or    a
  jp    nz,.Available

  ld    a,(StageSelectPointer)
  dec   a
  ld    (StageSelectPointer),a
  jp    p,.leftcheck
  ld    a,1
  ld    (StageSelectPointer),a
  ret

  .leftchecklast7:
  sub   a,15
  
  ld    d,0
  ld    e,a
  ld    hl,character08unlocked?
  add   hl,de
  ld    a,(hl)                ;is this character unlocked ?
  or    a
  jp    nz,.Available

  ld    a,(StageSelectPointer)
  dec   a
  ld    (StageSelectPointer),a
  jp    .leftcheck

  .Available:
  ld    c,sfxcursormove
  call  SetSfx
  jp    Samples

SetCountryIconCoordinates:
  ld    a,(StageSelectPointer)
  ld    b,a
  add   a,a
  add   a,a                   ;*4
  ld    hl,CountryCoordinates
  ld    d,0
  ld    e,a
  add   hl,de

  ld    a,(hl)
  ld    (CountrySelecticon+dx),a
  inc   hl
  ld    a,(hl)
  ld    (CountrySelecticon+dy),a
  inc   hl
  inc   hl                    ;selected country
  ld    a,(hl)
  ld    (background),a
  ret

CountryCoordinates:           ;x coordinate country, y coordinate country, country from left to right, player country
  db    -11+013,67+23,03,09   ;iceland harman do elan
  db    -11+023,67+46,14,06   ;spain vega
  db    -11+045,67+55,10,02   ;india dhalsim
  db    -11+045,67+18,02,08   ;england1 titanic tim
  db    -11+045,67+28,13,05   ;england2 cammy
  db    -11+051,67+38,17,16   ;england3 billy kane
  db    -11+076,67+63,01,07   ;thailand m bison
  db    -11+079,67+31,09,01   ;china chunli
  db    -11+085,67+14,19,18   ;japan2 joe higashi 
  db    -11+094,67+51,04,10   ;hongkong fei long
  db    -11+094,67+72,16,15   ;cambodia scorpion
  db    -11+115,67+21,05,11   ;japan1 mai shiranui
  db    -11+120,67+43,08,00   ;japan3 ryu
  db    -11+126,67+32,12,04   ;japan4 ehonda
  db    -11+158,67+23,06,12   ;usa1 guy
  db    -11+175,67+47,21,20   ;mexico t hawk
  db    -11+203,67+21,18,17   ;usa2 mike haggar
  db    -11+210,67+59,07,13   ;jamaica dee jay
  db    -11+211,67+46,20,19   ;cuba damnd
  db    -11+215,67+36,11,03   ;usa3 guile
  db    -11+224,67+81,15,14   ;brazil blanka

;setspat:
;	ld		a,1
;	ld		hl,$ee00		          ;sprite attribute table in VRAM
;	call	SetVdp_Write	

;	ld		hl,spat			          ;sprite attribute table
;  ld    c,$98
;  ld    b,128
;  otir
;  ret

putcharacterplayer2:
  ld    a,(Player2) ;0=ryu, 1=chunli, 2=dhalsim
  call  SetCharacterFaceBlock
  ;put character face
  ld    hl,240*256+182        ;start address to write to (0,28)  
  call  SetCharacterfacePlayer2
  ret

putcharacterplayer1:
  ld    a,(Player1) ;0=ryu, 1=chunli, 2=dhalsim
  call  SetCharacterFaceBlock
  ;put character face
  ld    hl,158*256            ;start address to write to (0,129)  
  call  SetCharacterfacePlayer1
  ret

SetCharacterFaceBlock:
  or    a
  ld    b,ryucharacterfaceblock
  jr    z,.setcharacterfaceblock
  dec   a
  ld    b,chunlicharacterfaceblock
  jr    z,.setcharacterfaceblock
  dec   a
  ld    b,dhalsimcharacterfaceblock
  jr    z,.setcharacterfaceblock
  dec   a
  ld    b,guilecharacterfaceblock
  jr    z,.setcharacterfaceblock
  dec   a
  ld    b,ehondacharacterfaceblock
  jr    z,.setcharacterfaceblock
  dec   a
  ld    b,cammycharacterfaceblock
  jr    z,.setcharacterfaceblock
  dec   a
  ld    b,vegacharacterfaceblock
  jr    z,.setcharacterfaceblock


  .setcharacterfaceblock:
  ld    a,b
  call	block12      		      ;at address $4000 / page 1
  ret

SetCharacterfacePlayer1:      ;character face width=74, height=83
  exx
  ld    hl,$4000              ;address of Character face
  ld    c,$98                 ;out port for outi
  exx
  ld    de,256                ;next line of face in Vram
  ld    b,83                  ;amount of lines to copy
  .loop:
  push  hl                    ;start address to write to (0,129)
  ld    a,1                   ;screen 8 page 1
  call  SetVdp_Write
  pop   hl
  add   hl,de
  exx
  ld    b,74                  ;74 pixels per line
  .loop2:
  ld    a,(hl)
  cp    $e0
  jp    nz,.normalout
  in    a,($98)
  jp    .passout
  .normalout:
  out   ($98),a
  .passout:
  inc   hl
  djnz  .loop2
  
;  otir
  exx
  djnz  .loop
  ret  
  
SetCharacterfacePlayer2:      ;character face width=74, height=83
  exx
  ld    hl,$4000+74*83-1      ;end address of Character face
  exx
  ld    de,-256               ;start at bottom line, and work your way up
  ld    b,83                  ;amount of lines to copy
  .loop:
  push  hl                    ;start address to write to (0,28)  
  ld    a,1                   ;screen 8 page 1
  call  SetVdp_Write

  pop   hl
  add   hl,de
  exx
  ld    b,74                  ;74 pixels per line
  ld    c,$98                 ;out port for outd
  .loop2:
  ld    a,(hl)                ;face pixel that needs to out if not transparant
  cp    $e0
  jp    nz,.normalout  
  ;transparant pixel found, dont out, but decrease vram pointer
  ex    de,hl
  ld    hl,tempstorage        ;in case ind is used store byte in hltemp
  ind
  ex    de,hl
  dec   hl
  jp    nz,.loop2
  exx
  djnz  .loop
  ret
  
  .normalout:
  outd
  jp    nz,.loop2
  exx
  djnz  .loop
  ret  

copygrahics:  
  push  bc
  call	block12      		      ;at address $4000 / page 1
	ld		hl,$4000              ;level grapx in rom
	ld		bc,$0098              ;256 bytes to port $98

  exx
  pop   bc
  .loop:
  exx
	otir
  exx
  djnz  .loop
  ret

copygrahics2:  
  push  bc
  call	block12      		      ;at address $4000 / page 1
	ld		hl,$4000+41*256              ;level grapx in rom
	ld		bc,$0098              ;256 bytes to port $98

  exx
  pop   bc
  .loop:
  exx
	otir
  exx
  djnz  .loop
  ret

setscreen8:
  ld    a,(vdp_0)
  or    %0000 1000            ;screen 8
  ld    (vdp_0),a
  di
  out   ($99),a
  ld    a,0+128
  ei
  out   ($99),a
  ret
  
setscreen5:
  ld    a,(vdp_0)
  and   %1111 0111            ;screen 5
  ld    (vdp_0),a
  di
  out   ($99),a
  ld    a,0+128
  ei
  out   ($99),a
  ret

SetMaleOrFemale:
  ld    a,(Player1)
  ld    hl,Player1Male?
  call  .SetMaleOrFemale
  ld    a,(Player2)
  ld    hl,Player2Male?
  call  .SetMaleOrFemale
  ret
  
  .SetMaleOrFemale:
  or    a
  ld    b,1                   ;1=Male, 0=Female
  jr    z,.PlayerFound        ;Ryu
  dec   a
  ld    b,0                   ;1=Male, 0=Female
  jr    z,.PlayerFound        ;Chunli
  dec   a
  ld    b,1                   ;1=Male, 0=Female
  jr    z,.PlayerFound        ;Dhalsim
  dec   a
  ld    b,1                   ;1=Male, 0=Female
  jr    z,.PlayerFound        ;Guile
  dec   a
  ld    b,1                   ;1=Male, 0=Female
  jr    z,.PlayerFound        ;eHonda
  dec   a
  ld    b,0                   ;1=Male, 0=Female
  jr    z,.PlayerFound        ;Cammy
  dec   a
  ld    b,1                   ;1=Male, 0=Female
  jr    z,.PlayerFound        ;Vega
  dec   a
  ld    b,1                   ;1=Male, 0=Female
  jr    z,.PlayerFound        ;mBison
  dec   a
  ld    b,1                   ;1=Male, 0=Female
  jr    z,.PlayerFound        ;Titanic Tim 
  .PlayerFound:
  ld    (hl),b
  ret

CheckOpl4Present:
  xor   a
  ld    (Opl3Present?),a
  ld    (Opl4Present?),a

;de OPL1 detectie werkt natuurlijk alleen als FMIO $C0 is 
;voor MoonSound zou je FMIO: equ $C4 moeten gebruiken 
FMIO: equ $c4
WVIO: equ $7e
InitOPL: 
  ld    c,FMIO 
  in    a,[c]                 ;Check for OPL cartridge 
  cp    06h 
  jr    z,.OPL1 
  or    a                     ;Check OPL3 
  ret   nz                    ;Unknown device 
  push  bc 
  ld    a,3                   ;Init OPL3 & OPL4 mode 
  ld    c,5 
  call  opl4_out_bnk1 
  pop   bc 
  ld    c,2                   ;Check OPL4 
  call  opl4_in_wave 
  and   11100000b 
  sub   20h 
  jr    nz,.OPL3 
  ; OPL4 detected 
  ld    a,1
  ld    (Opl4Present?),a
  ret 
  .OPL3: 
  ; OPL3 detected 
  ld    a,1
  ld    (Opl3Present?),a
  ret 
  .OPL1: 
  ; OPL1 detected 
  ret 

opl4_out_bnk1: 
  ex    af,af' 
  call  opl4_busy_wait 
  ld    a,c 
  .io2: out [FMIO + 2],a 
  call  opl4_busy_wait 
  ex    af,af' 
  .io3: out [FMIO + 3],a 
  ret 

opl4_in_wave: 
  call  opl4_busy_wait 
  ld    a,c 
  out   [WVIO],a 
  call  opl4_busy_wait 
  in    a,[WVIO+1] 
  ret 

opl4_busy_wait: 
  .wait:  
  in    a,[FMIO] 
  rrca 
  jp    c,.wait 
  ret 






;je hoort eigenlijk eerst OPL3 te detecten voordat je OPL4 detect 
;<GuyveR800> want je kan de OPL4 alleen aansturen als je de NEW2 flag geschreven hebt 
;<GuyveR800> en die zit in een OPL3 register 
;<GuyveR800> dus waarschijnlijk is je hele detectie kaduuk 
  xor   a
  ld    (Opl4Present?),a

;  ld    a,5
;  out   (fm2_reg),a
;  ld    a,3
;  out   (fm2_data),a	        ;initialize OPL4

;register D7   D6   D5        D4      D3     D2          D1                 D0
; 02      -Device  ID-        Wave table header     memory type     memory access mode
  ld    a,2
	out   (pcm_reg),a
  nop
  nop
	in    a,(pcm_data)          ;check Device ID (is OPL4 present?)

  and   %1110 0000
  cp    %0010 0000
  ret   nz
  ld    a,1
  ld    (Opl4Present?),a
  ret
  
loadsamples:		
  ld    a,(Opl4Present?)
  or    a
  ret   z

  ld    ix,tonedata
  ld    iy,0                  ;first sample tonedata address $200000

  ld		a,$20
  ld    (MoonSoundSampleAddress),a
  ld		hl,$0600              ;$200600 address of sample in moonsound ram
  ld    (MoonSoundSampleAddress+1),hl
  ;SFX00 softattack
  ld    a,Samples1block
  ld	  de,SoftAttacksample
  ld	  hl,SoftAttacksampleLenght
  call  DoLoadSample
  ;SFX01 hardattack
  ld    a,Samples1block
  ld	  de,HardAttacksample
  ld	  hl,HardAttacksampleLenght
  call  DoLoadSample
  ;SFX02 sofpunch
  ld    a,Samples1block
  ld	  de,SoftPunchHitsample
  ld	  hl,SoftPunchHitsampleLenght
  call  DoLoadSample
  ;SFX03 hardpunch
  ld    a,Samples1block
  ld	  de,HardPunchHitsample
  ld	  hl,HardPunchHitsampleLenght
  call  DoLoadSample
  ;SFX04 softkick
  ld    a,Samples1block
  ld	  de,SoftKickHitsample
  ld	  hl,SoftKickHitsampleLenght
  call  DoLoadSample
  ;SFX05 harkkick
  ld    a,Samples2block
  ld	  de,HardKickHitsample
  ld	  hl,HardKickHitsampleLenght
  call  DoLoadSample
  ;SFX06 malethrow
  ld    a,Samples1block
  ld	  de,Malethrowsample
  ld	  hl,MalethrowsampleLenght
  call  DoLoadSample
  ;SFX07 femalethrow
  ld    a,Samples2block
  ld	  de,Femalethrowsample
  ld	  hl,FemalethrowsampleLenght
  call  DoLoadSample
  ;SFX08 defend
  ld    a,Samples2block
  ld	  de,Defendsample
  ld	  hl,DefendsampleLenght
  call  DoLoadSample
  ;SFX09 landing
  ld    a,Samples2block
  ld	  de,Landingsample
  ld	  hl,LandingsampleLenght
  call  DoLoadSample
  ;SFX10 knockdown
  ld    a,Samples3block
  ld	  de,KnockDownsample
  ld	  hl,KnockDownsampleLenght
  call  DoLoadSample
  ;SFX11 male dies
  ld    a,Samples2block
  ld	  de,MaleDiessample
  ld	  hl,MaleDiessampleLenght
  call  DoLoadSample
  ;SFX12 female dies
  ld    a,Samples3block
  ld	  de,FemaleDiessample
  ld	  hl,FemaleDiessampleLenght
  call  DoLoadSample
  ;SFX13 one
  ld    a,Samples3block
  ld	  de,onesample
  ld	  hl,onesampleLenght
  call  DoLoadSample
  ;SFX14 two
  ld    a,Samples3block
  ld	  de,twosample
  ld	  hl,twosampleLenght
  call  DoLoadSample  
  ;SFX15 three
  ld    a,Samples6block
  ld	  de,threesample
  ld	  hl,threesampleLenght
  call  DoLoadSample  
  ;SFX16 final
  ld    a,Samples4block
  ld	  de,finalsample
  ld	  hl,finalsampleLenght
  call  DoLoadSample  
  ;SFX17 round
  ld    a,Samples4block
  ld	  de,roundsample
  ld	  hl,roundsampleLenght
  call  DoLoadSample  
  ;SFX18 perfect
  ld    a,Samples4block
  ld	  de,perfectsample
  ld	  hl,perfectsampleLenght
  call  DoLoadSample  
  ;SFX19 you
  ld    a,Samples4block
  ld	  de,yousample
  ld	  hl,yousampleLenght
  call  DoLoadSample  
  ;SFX20 win
  ld    a,Samples6block
  ld	  de,winsample
  ld	  hl,winsampleLenght
  call  DoLoadSample  
  ;SFX21 lose
  ld    a,Samples5block
  ld	  de,losesample
  ld	  hl,losesampleLenght
  call  DoLoadSample  
  ;SFX22 fight
  ld    a,Samples5block
  ld	  de,fightsample
  ld	  hl,fightsampleLenght
  call  DoLoadSample  
  ;SFX23 cursormove
  ld    a,Samples10block
  ld	  de,cursormovesample
  ld	  hl,cursormovesampleLenght
  call  DoLoadSample  
  ;SFX24 menuselected
  ld    a,Samples7block
  ld	  de,menuselectsample
  ld	  hl,menuselectsampleLenght
  call  DoLoadSample  
  ;SFX25 characterselected
  ld    a,Samples9block
  ld	  de,characterselectedsample
  ld	  hl,characterselectedsampleLenght
  call  DoLoadSample  
  ;SFX26 plane
  ld    a,Samples8block
  ld	  de,planesample
  ld	  hl,planetsampleLenght
  call  DoLoadSample  
  ;SFX27 countdown
  ld    a,Samples7block
  ld	  de,countdownsample
  ld	  hl,countdownsampleLenght
  call  DoLoadSample  

  ld    a,(MoonSoundSampleAddress)
  ld    hl,(MoonSoundSampleAddress+1)

  ;store current MoonsoundSampleAddress as P1special samples
  ld    (P1SpecialSampleAddress),a
  ld    (P1SpecialSampleAddress+1),hl
  ld    (ToneDataAddressSpecial),iy

  ;set country sample address (this sample comes after the 6 special-move samples)
  ld    de,12*6
  add   iy,de                 ;tonedata after special samples will be used for country sample
  ld    (ToneDataAddressCountry),iy

  ld    de,6*$2000            ;6 special samples lenght
  add   hl,de
  ld    (CountrySampleAddress+1),hl
  jr    nc,.notcarry
  inc   a
  .notcarry:
  ld    (CountrySampleAddress),a
  ret

CountrySample:
  ld    a,(Opl4Present?)
  or    a
  ret   z

  ld    ix,tonedata
  ;fetch CountrySampleAddress
  ld    iy,(ToneDataAddressCountry)
  ld    a,(CountrySampleAddress)
  ld    hl,(CountrySampleAddress+1)
  
  ld    (MoonSoundSampleAddress),a
  ld    (MoonSoundSampleAddress+1),hl

  ld    a,(Background)        ;0=Ryu, 1=Chunli, 2=Dhalsim
  or    a
  ld    hl,ryucountrysampleLenght
  ld    b,ryucountrysampleblock
  jr    z,.setsample
  dec   a
  ld    hl,chunlicountrysampleLenght
  ld    b,chunlicountrysampleblock
  jr    z,.setsample
  dec   a
  ld    hl,dhalsimcountrysampleLenght
  ld    b,dhalsimcountrysampleblock
  jr    z,.setsample
  dec   a
  ld    hl,guilecountrysampleLenght
  ld    b,guilecountrysampleblock
  jr    z,.setsample
  dec   a
  ld    hl,ehondacountrysampleLenght
  ld    b,ehondacountrysampleblock
  jr    z,.setsample
  dec   a
  ld    hl,cammycountrysampleLenght
  ld    b,cammycountrysampleblock
  jr    z,.setsample
  dec   a
  ld    hl,vegacountrysampleLenght
  ld    b,vegacountrysampleblock
  jr    z,.setsample

  .setsample:
  ld    a,b
  call  DoLoadSample
    
  ld    c,sfxCountry
  call  PlaySampleChannel1

  ld    b,170
  .waitloop:
  halt
  djnz  .waitloop
  ret

SetSpecialSamples:
  ld    a,(Opl4Present?)
  or    a
  ret   z

  ld    ix,tonedata
  ;fetch MoonsoundSampleAddress of P1special samples
  ld    iy,(ToneDataAddressSpecial)
  ld    a,(P1SpecialSampleAddress)
  ld    hl,(P1SpecialSampleAddress+1)
  
  ld    (MoonSoundSampleAddress),a
  ld    (MoonSoundSampleAddress+1),hl

  ;SFX28-30 P1Special
  ld    hl,Player1
  call  SetSamplesSpecialsPlayer
  ;SFX31-33 P2Special
  ld    hl,Player2
  call  SetSamplesSpecialsPlayer
  ret

SetSamplesSpecialsPlayer:
  ld    a,(hl)                ;0=Ryu, 1=Chunli, 2=Dhalsim
  or    a
  jp    z,.Ryu
  dec   a
  jp    z,.Chunli
  dec   a
  jp    z,.Dhalsim
  dec   a
  jp    z,.Guile
  dec   a
  jp    z,.Ehonda
  dec   a
  jp    z,.Cammy
  dec   a
  jp    z,.Vega
  
  .Ryu:
  ;Special1
  ld    a,ryuspecial1sampleblock
  ld	  de,ryuspecial1sample
  ld	  hl,ryuspecial1sampleLenght
  call  DoLoadSample
  ;Special2
  ld    a,ryuspecial2sampleblock
  ld	  de,ryuspecial2sample
  ld	  hl,ryuspecial2sampleLenght
  call  DoLoadSample
  ;Special3
  ld    a,ryuspecial3sampleblock
  ld	  de,ryuspecial3sample
  ld	  hl,ryuspecial3sampleLenght
  call  DoLoadSample
  ret

  .Chunli:
  ;Special1
  ld    a,chunlispecial1sampleblock
  ld	  de,chunlispecial1sample
  ld	  hl,chunlispecial1sampleLenght
  call  DoLoadSample
  ;Special2
  ld    a,chunlispecial2sampleblock
  ld	  de,chunlispecial2sample
  ld	  hl,chunlispecial2sampleLenght
  call  DoLoadSample
  ;Special3
  ld    a,chunlispecial3sampleblock
  ld	  de,chunlispecial3sample
  ld	  hl,chunlispecial3sampleLenght
  call  DoLoadSample
  ret

  .Dhalsim:
  ;Special1
  ld    a,dhalsimspecial1sampleblock
  ld	  de,dhalsimspecial1sample
  ld	  hl,dhalsimspecial1sampleLenght
  call  DoLoadSample
  ;Special2
  ld    a,dhalsimspecial2sampleblock
  ld	  de,dhalsimspecial2sample
  ld	  hl,dhalsimspecial2sampleLenght
  call  DoLoadSample
  ;Special3
  ld    a,dhalsimspecial3sampleblock
  ld	  de,dhalsimspecial3sample
  ld	  hl,dhalsimspecial3sampleLenght
  call  DoLoadSample
  ret

  .Guile:
  ;Special1
  ld    a,guilespecial1sampleblock
  ld	  de,guilespecial1sample
  ld	  hl,guilespecial1sampleLenght
  call  DoLoadSample
  ;Special2
  ld    a,guilespecial2sampleblock
  ld	  de,guilespecial2sample
  ld	  hl,guilespecial2sampleLenght
  call  DoLoadSample
  ;Special3
  ld    a,guilespecial3sampleblock
  ld	  de,guilespecial3sample
  ld	  hl,guilespecial3sampleLenght
  call  DoLoadSample
  ret

  .Ehonda:
  ;Special1
  ld    a,ehondaspecial1sampleblock
  ld	  de,ehondaspecial1sample
  ld	  hl,ehondaspecial1sampleLenght
  call  DoLoadSample
  ;Special2
  ld    a,ehondaspecial2sampleblock
  ld	  de,ehondaspecial2sample
  ld	  hl,ehondaspecial2sampleLenght
  call  DoLoadSample
  ;Special3
  ld    a,ehondaspecial3sampleblock
  ld	  de,ehondaspecial3sample
  ld	  hl,ehondaspecial3sampleLenght
  call  DoLoadSample
  ret

  .Cammy:
  ;Special1
  ld    a,cammyspecial1sampleblock
  ld	  de,cammyspecial1sample
  ld	  hl,cammyspecial1sampleLenght
  call  DoLoadSample
  ;Special2
  ld    a,cammyspecial2sampleblock
  ld	  de,cammyspecial2sample
  ld	  hl,cammyspecial2sampleLenght
  call  DoLoadSample
  ;Special3
  ld    a,cammyspecial3sampleblock
  ld	  de,cammyspecial3sample
  ld	  hl,cammyspecial3sampleLenght
  call  DoLoadSample
  ret

  .Vega:
  ;Special1
  ld    a,vegaspecial1sampleblock
  ld	  de,vegaspecial1sample
  ld	  hl,vegaspecial1sampleLenght
  call  DoLoadSample
  ;Special2
  ld    a,vegaspecial2sampleblock
  ld	  de,vegaspecial2sample
  ld	  hl,vegaspecial2sampleLenght
  call  DoLoadSample
  ;Special3
  ld    a,vegaspecial3sampleblock
  ld	  de,vegaspecial3sample
  ld	  hl,vegaspecial3sampleLenght
  call  DoLoadSample
  ret
  
DoLoadSample:
  call	block12			          ;at address $4000 / page 2
  ld    (sampleaddress),de    ;sample address
  ld    (samplelenght),hl

  ;set sample lenght, sample lenght inverted and start address
  ld    (ix+3),l              ;sample lenght
  ld    (ix+4),h

  dec   hl                    ;sample lenght - 1

  ;set loop address
  ld    a,h
  cpl
  ld    (ix+5),a              ;(sample lenght -1) inverted
  ld    a,l
  cpl
  ld    (ix+6),a

  ld    a,(MoonSoundSampleAddress)
  ld    hl,(MoonSoundSampleAddress+1)
  ld    (ix),a
  ld    (ix+1),h              ;set sample start address
  ld    (ix+2),l
  ;/set sample lenght, sample lenght inverted and start address

  ld    a,7                   ;set sample in bank 7, page 0 ($0000 - $3fff)
  call  CopyBlocktoBank
  di
  ld    a,7                   ;set sample in bank 7, page 0 ($0000 - $3fff)
  out   ($fc),a               ;page a,x,x,x
  call  SampleLoader          ;load sample in moonsound ram

  ld    de,12
  add   iy,de                 ;next tonedata will be 12 bytes further

  ld    a,3                   ;set bank 3 back in page 0 ($0000 - $3fff)
  out   ($fc),a               ;page 3,x,x,x
  ei
  ret

SampleLoader:
  call	convert			          ;convert sample 8 bits unsigned to 8 bits signed

  ld    a,(MoonSoundSampleAddress)
  ld    e,a
  ld    hl,(MoonSoundSampleAddress+1)
  call	set_opl4_wrt	        ;set write address to $200600 (first 2MB of data is ROM, in other 2MB of address range sample data can be writen)
  ld	  de,(samplelenght)

  add   hl,de
  ld    (MoonSoundSampleAddress+1),hl
  jp    nc,.nooverflow
  ld    a,(MoonSoundSampleAddress)
  inc   a
  ld    (MoonSoundSampleAddress),a
  .nooverflow:

  ld	  hl,(sampleaddress)
  call	ramtosram		          ;move sample data

  ld		e,$20
  push  iy
  pop   hl
  call	set_opl4_wrt	        ;address for tone headers for custom tones is $200000
  ld		hl,tonedata
  ld		de,12
  call	ramtosram		          ;store tonedata in SRAM

  ld		a,2
  ld		c,%10000
  call	opl4_Register_Write   ;In: A: register C: data, changes: a

;set channel 1 ready
;  ld		a,$08
;  ld		c,%10000000		        ;lowest 8 bits of tone number to play (of 9 bits)
;  call	opl4_Register_Write   ;In: A: register C: data, changes: a

  ld		a,$20                 ;register number
  ld		c,%0000 0001		      ;7 bits for frequency (of 10 bits) / 1 bit for tone number to play
  call	opl4_Register_Write   ;In: A: register C: data, changes: a

  ld		a,$38                 ;register number
  ld		c,%11110000		        ;4 bits for octave / 1 bit for pseudoreverb / 3 bits for frequency
  call	opl4_Register_Write   ;In: A: register C: data, changes: a

;  ld		a,$50
;  ld		c,%0000 0000          ;total level / level direct
;  call	opl4_Register_Write   ;In: A: register C: data, changes: a
		
;set channel 2 ready
;  ld		a,$09
;  ld		c,%10000000		        ;lowest 8 bits of tone number to play (of 9 bits)
;  call	opl4_Register_Write   ;In: A: register C: data, changes: a

  ld		a,$21                 ;register number
  ld		c,%0000 0001		      ;7 bits for frequency (of 10 bits) / 1 bit for tone number to play
  call	opl4_Register_Write   ;In: A: register C: data, changes: a
  
  ld		a,$39                 ;register number
  ld		c,%11110000		        ;4 bits for octave / 1 bit for pseudoreverb / 3 bits for frequency
  call	opl4_Register_Write   ;In: A: register C: data, changes: a

;  ld		a,$51
;  ld		c,%0000 0000          ;total level / level direct
;  call	opl4_Register_Write   ;In: A: register C: data, changes: a

;set channel 3 ready
;  ld		a,$0a
;  ld		c,%10000000		        ;lowest 8 bits of tone number to play (of 9 bits)
;  call	opl4_Register_Write   ;In: A: register C: data, changes: a

  ld		a,$22                 ;register number
  ld		c,%0000 0001		      ;7 bits for frequency (of 10 bits) / 1 bit for tone number to play
  call	opl4_Register_Write   ;In: A: register C: data, changes: a
  
  ld		a,$3a                 ;register number
  ld		c,%11110000		        ;4 bits for octave / 1 bit for pseudoreverb / 3 bits for frequency
  call	opl4_Register_Write   ;In: A: register C: data, changes: a

;  ld		a,$52
;  ld		c,%0000 0000          ;total level / level direct
;  call	opl4_Register_Write   ;In: A: register C: data, changes: a
  ret		

convert:	            ;converts the sample in ram	
  ld	  hl,(sampleaddress)
  ld	  bc,(samplelenght)
  ld    d,128
.loop:
  ld	  a,(hl)
  sub	  a,d
  ld	  (hl),a
  inc	  hl
  dec   bc
  ld	  a,b
  or	  c
  jp	  nz,.loop
  ret
	
;-------------------------------------------------------
;move data in ram to SRAM
;In: HL: source data
;    DE: size
;Out: -
;Changes: AF
ramtosram:		
  ld    c,pcm_data
ramtosram_lp:	
  in    a,(fm1_reg)
	rra
	jr    c,ramtosram_lp
	outi
	dec   de
	ld    a,d
	or    e
	jr    nz,ramtosram_lp
	ret
	
;-------------------------------------------------------
;set SRAM read/write address
;In: ehl = SRAM address
;Changes: af, c
set_opl4_wrt:
	ld	  a,2                   ;register number 
	ld	  c,%0001 0001
  call	opl4_Register_Write   ;In: A: register C: data, changes: a

  ld    a,3                   ;register number
	ld	  c,e
	and	  %0011 1111
  call	opl4_Register_Write   ;In: A: register C: data, changes: a

  ld    a,4                   ;register number
	ld	  c,h
  call	opl4_Register_Write   ;In: A: register C: data, changes: a

  ld    a,5                   ;register number
 	ld	  c,l
  call	opl4_Register_Write   ;In: A: register C: data, changes: a

	ld	  a,6                   ;register number
	out	  (pcm_reg),a		        ;SRAM access

;	ld	  c,pcm_data		        ;data port
	ret

LoadPlayerActionTables:
;set Actions player 1
  ld    a,(Player1)           ;0=Ryu, 1=Chunli, 2=Dhalsim  
  call  .setactiontablesblock
  call  SetActionsPlayer1

;set Actions player 2
  ld    a,(Player2)           ;0=Ryu, 1=Chunli, 2=Dhalsim  
  call  .setactiontablesblock
  jp    SetActionsPlayer2


.setactiontablesblock:
  or    a
  ld    b,ryuactiontablesblock
	ld		hl,RyuActions
  ret   z
  dec   a
  ld    b,chunliactiontablesblock
	ld		hl,ChunliActions
  ret   z
  dec   a
  ld    b,dhalsimactiontablesblock
	ld		hl,DhalsimActions
  ret   z
  dec   a
  ld    b,guileactiontablesblock
	ld		hl,GuileActions
  ret   z
  dec   a
  ld    b,ehondaactiontablesblock
	ld		hl,ehondaActions
  ret   z
  dec   a
  ld    b,cammyactiontablesblock
	ld		hl,cammyActions
  ret   z
  dec   a
  ld    b,vegaactiontablesblock
	ld		hl,vegaActions
  ret   z

  ret

SetActionsPlayer1:  
  ld    a,b
  call	block12
  
  ld    de,P1LeftIdleFrame
  ld    bc,LenghtIdleAction*2
  ldir

  ld    de,P1LeftBendFrame
  ld    bc,LenghtBendAction*2
  ldir

  ld    de,P1LeftWalkLeftFrame
  ld    bc,LenghtWalkAction*4
  ldir

  ld    de,P1HorSpeedWalkSlowTable
  ld    bc,LenghtHorSpeedWalkTable*2
  ldir

  ld    de,P1LeftJumpStraightStartframe
  ld    bc,LenghtJumpAnimationTable*6
  ldir

  ld    de,P1jumptable
  ld    bc,LenghtJumpTable
  ldir

  ld    de,P1HorSpeedJumpSlowTable
  ld    bc,LenghtHorSpeedJumpTable*2
  ldir

;  ld    de,P1StandSoftPunchLeftFrame        ;ldir Stand Soft Punch, Stand Hard Punch,
;  ld    bc,LenghtStandSoftPunchTable*2 * 7  ;Stand Soft Kick, Stand Hard Kick, Sit Soft
;  ldir                                      ;Punch, Sit Hard Punch, Sit Soft Kick 
;  ld    bc,LenghtSitHardKickTable*2         ;Sit Hard Kick
;  ldir

  ld    de,P1StandSoftPunchLeftFrame
  ld    bc,LenghtStandSoftPunchTable*2 * 2  ;SoftStandPunching, HardStandPunching
  ldir
  ld    bc,LenghtStandSoftKickTable*2       ;SoftStandKicking
  ldir
  ld    bc,LenghtStandHardKickTable*2       ;HardStandKicking
  ldir
  ld    bc,LenghtStandSoftPunchTable*2 * 3  ;SoftStandPunching, HardStandPunching
  ldir
  ld    bc,LenghtSitHardKickTable*2         ;Sit Hard Kick
  ldir

  ld    de,P1LeftSoftJumpPunchStraightup    ;ldir SoftJumpPunchStraight, HardJumpPunchStraight
  ld    bc,LenghtStandSoftPunchTable*2 * 8  ;SoftJumpPunchDiagonal, HardJumpPunchDiagonal
  ldir                                      ;and the same for the kicking jumps

  ld    de,P1LeftStandDefendFrame
  ld    bc,LenghtDefendTable*4
  ldir

  ld    de,P1LeftStandHitFrame
  ld    bc,LenghtStandHitTable*6
  ldir

  ld    de,P1HeavilyHitLeftFrame
  ld    bc,LenghtHeavilyHitTable*4
  ldir

  ld    de,P1KnockDownRecoverLeftFrame
  ld    bc,LenghtKnockDownRecoverTable*2
  ldir

  ld    de,P1TossLeftFrame
  ld    bc,LenghtTossTable*2
  ldir

  ld    de,P1VictoryLeftFrame
  ld    bc,LenghtVictoryTable*2
  ldir  

  ld    de,P1Special1MovementTablePointer
  ld    bc,LenghtSpecialMovementTable*5
  ldir  

  ld    de,P1VariableTableSpecial1
  ld    bc,LenghtSpecialVariableTable*5
  ldir  

  ld    de,P1Special1LeftFrame
  ld    bc,LenghtSpecialFrameTable*10
  ldir  

  ld    de,P1GettingTossedLeftFrame
  ld    bc,LenghtGettingTossedFrameTable*2
  ldir  

  ld    de,P1DamageTabel
  ld    bc,LenghtDamageTabel
  ldir  

  ld    de,P1ProjectileLeftFrame
  ld    bc,LenghtProjectileTabel*2
  ldir  

  ld    de,P1ProjectileLeftEndFrame
  ld    bc,LenghtProjectileEndTabel*2
  ldir  

  ld    de,P1DiedLeftFrame
  ld    bc,LenghtPlayerDiedTabel*2
  ldir  
  ret

SetActionsPlayer2:
  ld    a,b
  call	block12

  ld    de,P2LeftIdleFrame
  ld    bc,LenghtIdleAction*2
  ldir

  ld    de,P2LeftBendFrame
  ld    bc,LenghtBendAction*2
  ldir

  ld    de,P2LeftWalkLeftFrame
  ld    bc,LenghtWalkAction*4
  ldir

  ld    de,P2HorSpeedWalkSlowTable
  ld    bc,LenghtHorSpeedWalkTable*2
  ldir

  ld    de,P2LeftJumpStraightStartframe
  ld    bc,LenghtJumpAnimationTable*6
  ldir

  ld    de,P2jumptable
  ld    bc,LenghtJumpTable
  ldir

  ld    de,P2HorSpeedJumpSlowTable
  ld    bc,LenghtHorSpeedJumpTable*2
  ldir

;  ld    de,P2StandSoftPunchLeftFrame        ;ldir Stand Soft Punch, Stand Hard Punch,
;  ld    bc,LenghtStandSoftPunchTable*2 * 7  ;Stand Soft Kick, Stand Hard Kick, Sit Soft
;  ldir                                      ;Punch, Sit Hard Punch, Sit Soft Kick 
;  ld    bc,LenghtSitHardKickTable*2         ;Sit Hard Kick
;  ldir



  ld    de,P2StandSoftPunchLeftFrame
  ld    bc,LenghtStandSoftPunchTable*2 * 2  ;SoftStandPunching, HardStandPunching
  ldir
  ld    bc,LenghtStandSoftKickTable*2       ;SoftStandKicking
  ldir
  ld    bc,LenghtStandHardKickTable*2       ;HardStandKicking
  ldir
  ld    bc,LenghtStandSoftPunchTable*2 * 3  ;SoftStandPunching, HardStandPunching
  ldir
  ld    bc,LenghtSitHardKickTable*2         ;Sit Hard Kick
  ldir



  ld    de,P2LeftSoftJumpPunchStraightup    ;ldir SoftJumpPunchStraight, HardJumpPunchStraight
  ld    bc,LenghtStandSoftPunchTable*2 * 8  ;SoftJumpPunchDiagonal, HardJumpPunchDiagonal
  ldir                                      ;and the same for the kicking jumps

  ld    de,P2LeftStandDefendFrame
  ld    bc,LenghtDefendTable*4
  ldir

  ld    de,P2LeftStandHitFrame
  ld    bc,LenghtStandHitTable*6
  ldir

  ld    de,P2HeavilyHitLeftFrame
  ld    bc,LenghtHeavilyHitTable*4
  ldir

  ld    de,P2KnockDownRecoverLeftFrame
  ld    bc,LenghtKnockDownRecoverTable*2
  ldir

  ld    de,P2TossLeftFrame
  ld    bc,LenghtTossTable*2
  ldir

  ld    de,P2VictoryLeftFrame
  ld    bc,LenghtVictoryTable*2
  ldir  

  ld    de,P2Special1MovementTablePointer
  ld    bc,LenghtSpecialMovementTable*5
  ldir  

  ld    de,P2VariableTableSpecial1
  ld    bc,LenghtSpecialVariableTable*5
  ldir  

  ld    de,P2Special1LeftFrame
  ld    bc,LenghtSpecialFrameTable*10
  ldir  

  ld    de,P2GettingTossedLeftFrame
  ld    bc,LenghtGettingTossedFrameTable*2
  ldir  

  ld    de,P2DamageTabel
  ld    bc,LenghtDamageTabel
  ldir  

  ld    de,P2ProjectileLeftFrame
  ld    bc,LenghtProjectileTabel*2
  ldir  

  ld    de,P2ProjectileLeftEndFrame
  ld    bc,LenghtProjectileEndTabel*2
  ldir  

  ld    de,P2DiedLeftFrame
  ld    bc,LenghtPlayerDiedTabel*2
  ldir  
  ret

;  include "actiontables\ryuactiontables.asm"
;  include "actiontables\chunliactiontables.asm"
;  include "actiontables\dhalsimactiontables.asm"



AdjustPlayer2Colors:          ;colors 5,6,7 have to become 8,9,10
  ld    a,4                   ;set player 2 sprites (1st part) in bank 4
  call  .DoSetadjustcolors
  ld    a,5                   ;set player 2 sprites (2nd part) in bank 5
  call  .DoSetadjustcolors
  ld    a,6                   ;set player 2 sprites (3d part) in bank 6
  call  .DoSetadjustcolors
  ld    a,7                   ;set player 2 sprites (4th part) in bank 7

  .DoSetadjustcolors:
  di
  out   ($fc),a               ;page 7,x,x,x

  ld    hl,$0000
  ld    c,64                  ;64*256 = $4000, check color of $4000 bytes
  ld    b,0
  ld    d,8*16
  ld    e,3*16
 
  .loop:
  ld    a,(hl)
  cp    d
  jp    nc,.colorLowBitsnotfound
  add   a,e
  .colorLowBitsnotfound:
  bit   3,a
  jr    nz,.colorHiBitsnotfound
  add   a,3
  .colorHiBitsnotfound:
  ld    (hl),a
  inc   hl

  djnz  .loop
  dec   c
  jp    nz,.loop

  ld    a,3                   ;set bank 3 back in page 0 ($0000 - $3fff)
  out   ($fc),a               ;page 3,x,x,x
  ei
  ret   
  
setFramelists:
  ld    a,(Player1)           ;0=Ryu, 1=Chunli, 2=Dhalsim  
  call  .findframelistblock
  ld    a,b
  ld    (Player1Framelistblock),a

  ld    a,(Player2)           ;0=Ryu, 1=Chunli, 2=Dhalsim  
  call  .findframelistblock
  ld    a,b
  ld    (Player2Framelistblock),a
  ret


  .findframelistblock:
  or    a
	ld		b,ryuframelistblock
  ret   z
  dec   a
	ld		b,chunliframelistblock
  ret   z
  dec   a
	ld		b,dhalsimframelistblock
  ret   z
  dec   a
	ld		b,guileframelistblock
  ret   z
  dec   a
	ld		b,ehondaframelistblock
  ret   z
  dec   a
	ld		b,cammyframelistblock
  ret   z
  dec   a
	ld		b,vegaframelistblock
  ret   z

  ret


SetspriteData:
;copy player1 spritedata to bank 2,3 and 4
  ld    a,(Player1)           ;0=Ryu, 1=Chunli, 2=Dhalsim  
  call  .findspritedatablock
  ld    a,b
  ld    (Player1Spritedatablock),a

  ld    a,(Player2)           ;0=Ryu, 1=Chunli, 2=Dhalsim  
  call  .findspritedatablock
	ld		a,b
  push  af
  call	block12
  ld    a,4                   ;set player 2 sprites (1st part) in bank 4
  call  CopyBlocktoBank
  pop   af
  add   a,2
  push  af
  call	block12
  ld    a,5                   ;set player 2 sprites (2nd part) in bank 5
  call  CopyBlocktoBank
  pop   af
  add   a,2
  push  af
  call	block12
  ld    a,6                   ;set player 2 sprites (3d part) in bank 6
  call  CopyBlocktoBank
  pop   af
  add   a,2
  call	block12
  ld    a,7                   ;set player 2 sprites (4th part) in bank 7
  call  CopyBlocktoBank
  ret


.findspritedatablock:
  or    a
	ld		b,ryuspritedatablock
  ret   z
  dec   a
	ld		b,chunlispritedatablock
  ret   z
  dec   a
	ld		b,dhalsimspritedatablock
  ret   z
  dec   a
	ld		b,guilespritedatablock
  ret   z
  dec   a
	ld		b,ehondaspritedatablock
  ret   z
  dec   a
	ld		b,cammyspritedatablock
  ret   z
  dec   a
	ld		b,vegaspritedatablock
  ret   z

  ret


CopyBlocktoBank:
  di
  out   ($fc),a               ;page a,x,x,x
  ld    hl,$4000
  ld    de,$0000
  ld    bc,$4000
  ldir
  ld    a,3                   ;set bank 3 back in page 0 ($0000 - $3fff)
  out   ($fc),a               ;page 3,x,x,x
  ei
  ret

copyplayerPalettes:
;set Palette player 1
  ld    a,(Player1)           ;0=Ryu, 1=Chunli, 2=Dhalsim
  call  .findpaletteblock
	ld		a,b                   ;paletteblock
  call	block12      		      ;at address $4000 / page 1
  ld    de,currentpalette+10  ;set palettecolors player 1
  ld    bc,3*2
  ldir
  
  ld    a,(Player2)           ;0=Ryu, 1=Chunli, 2=Dhalsim  
  call  .findpaletteblock
	ld		a,b                   ;paletteblock
  call	block12      		      ;at address $4000 / page 1

  ;if both players are the same, then set alternative palette for player 2
  ld    a,(Player1)           ;0=Ryu, 1=Chunli, 2=Dhalsim  
  ld    b,a
  ld    a,(Player2)           ;0=Ryu, 1=Chunli, 2=Dhalsim  
  cp    b
  jr    nz,.notthesame
  ld    de,6
  add   hl,de                 ;switch to alternate palette
  .notthesame:
  ld    de,currentpalette+16  ;set palettecolors player 2
  ld    bc,3*2
  ldir
  ret

  
  .findpaletteblock:
  or    a
  ld    hl,ryupalette
  ld    b,backgroundryublock+2
  ret   z
  dec   a
  ld    hl,chunlipalette
  ld    b,backgroundchunliblock+2
  ret   z
  dec   a
  ld    hl,dhalsimpalette
  ld    b,backgrounddhalsimblock+2
  ret   z
  dec   a
  ld    hl,guilepalette
  ld    b,backgroundguileblock+2
  ret   z
  dec   a
  ld    hl,ehondapalette
  ld    b,backgroundehondablock+2
  ret   z
  dec   a
  ld    hl,cammypalette
  ld    b,backgroundcammyblock+2
  ret   z
  dec   a
  ld    hl,vegapalette
  ld    b,backgroundvegablock+2
  ret   z
  
  ret



Initstagemusic:
  call  setInterrupt2         ;this one has no music on the interrupt, and is used during fights

  ld    a,(background)        ;0=Ryu, 1=Chunli, 2=Dhalsim, 3=Guile
  or    a
  ld    b,ryumusicblock
  jp    z,.Setmusicblock
  dec   a
  ld    b,chunlimusicblock
  jp    z,.Setmusicblock
  dec   a
  ld    b,dhalsimmusicblock
  jp    z,.Setmusicblock
  dec   a
  ld    b,guilemusicblock
  jp    z,.Setmusicblock
  dec   a
  ld    b,ehondamusicblock
  jp    z,.Setmusicblock
  dec   a
  ld    b,cammymusicblock
  jp    z,.Setmusicblock
  dec   a
  ld    b,vegamusicblock
  jp    z,.Setmusicblock
  .Setmusicblock:
  ld    a,b
  ld    (backgroundmusicblock),a
  call  Initmusic
  ret

buildupbackground:
  ld    a,(background)        ;0=Ryu, 1=Chunli, 2=Dhalsim
  or    a
  ld    b,backgroundryublock
  jp    z,.DosetBackground
  dec   a
  ld    b,backgroundchunliblock
  jp    z,.DosetBackground
  dec   a
  ld    b,backgrounddhalsimblock
  jp    z,.DosetBackground
  dec   a
  ld    b,backgroundguileblock
  jp    z,.DosetBackground
  dec   a
  ld    b,backgroundehondablock
  jp    z,.DosetBackground
  dec   a
  ld    b,backgroundcammyblock
  jp    z,.DosetBackground
  dec   a
  ld    b,backgroundvegablock
  jp    z,.DosetBackground
  .DosetBackground:
	ld		a,b
  push  af
  call	block12      		      ;at address $4000 / page 1
   
  xor   a
  ld    hl,movescreendown*128
  call  SetVdp_Write

	ld		hl,$4000              ;level grapx in rom
	ld		bc,$0098              ;256 bytes to port $98
  
  exx
  ld    b,64                  ;128 lines to out
  .loop1:
  exx
	otir
  exx
  djnz  .loop1

  pop   af
  add   a,2
  call	block12      		      ;at address $4000 / page 1

	ld		hl,$4000              ;level grapx in rom
	ld		bc,$0098              ;256 bytes to port $98
  
  exx
  ld    b,42 + 6                 ;84 lines to out
  .loop2:
  exx
	otir
  exx
  djnz  .loop2  

  ld    hl,backgroundryupalette
  ld    de,currentpalette     ;now background fils current palette
  ld    bc,16*2               ;with background colors AND with
  ldir                        ;standard palette colors

	ld		a,scoreboardblock
  call	block12      		      ;at address $4000 / page 1

  ;scoreboard on (0,16)-(255,27)
  xor   a
  ld    hl,16*128
  call  SetVdp_Write
  ld    hl,scoreboard
	ld		bc,$0098              ;256 bytes to port $98
  otir | otir | otir | otir | otir | otir

  ld    a,(Player1)           ;0=Ryu, 1=Chunli, 2=Dhalsim
  call  InitScoreboardface    ;sets block and start address in de
  ;scoreboardface player1 on (0,28)-(19,49)
  ld    hl,28*128             ;start address to write to (0,28)
  call  SetScoreboardfacePlayer1

  ld    a,(Player2)           ;0=Ryu, 1=Chunli, 2=Dhalsim
  call  InitScoreboardface    ;sets block and start address in de
  ;scoreboardface player2 on (20,28)-(39,49)
  ld    hl,28*128 +10         ;start address to write to (20,28)
  call  SetScoreboardfacePlayer2

  ld    hl,scoreboardpart1    ;left part scoreboard (player 1)
  call  DoCopygrauw
  ld    hl,scoreboardpart2    ;right part scoreboard (player 1)
  call  DoCopygrauw
  ld    hl,scoreboardpart3    ;KO
  call  DoCopygrauw
  ld    hl,scoreboardpart4    ;KO bottom part
  call  DoCopygrauw
  ld    hl,scoreboardpart5    ;scoreboard face player 1
  call  DoCopygrauw
  ld    hl,scoreboardpart6    ;scoreboard face player 2
  call  DoCopygrauw

  ld    a,(P1RoundsWon)
  or    a
  ld    hl,scoreboardpart7    ;amount of rounds won P1
  call  nz,DoCopygrauw
  ld    a,(P2RoundsWon)
  or    a
  ld    hl,scoreboardpart8    ;amount of rounds won P2
  call  nz,DoCopygrauw

  if movescreendown>43
  ld    hl,FixBottompage0
  call  DoCopygrauw
  endif
  ld    hl,copypage0to1
  call  DoCopygrauw
  ld    hl,copypage0to2
  call  DoCopygrauw
  ld    hl,copypage0to3
  call  DoCopygrauw
  jp    vdpready              ;wait for vdp to become ready

InitScoreboardface:
  or    a                     ;0=Ryu, 1=Chunli, 2=Dhalsim
  ld    b,backgroundryublock+2
  ld    de,scoreboardfaceryu
  jp    z,.setscoreboardfaceblock
  dec   a
  ld    b,backgroundchunliblock+2
  ld    de,scoreboardfacechunli
  jp    z,.setscoreboardfaceblock
  dec   a
  ld    b,backgrounddhalsimblock+2
  ld    de,scoreboardfacedhalsim
  jp    z,.setscoreboardfaceblock
  dec   a
  ld    b,backgroundguileblock+2
  ld    de,scoreboardfaceguile
  jp    z,.setscoreboardfaceblock
  dec   a
  ld    b,backgroundehondablock+2
  ld    de,scoreboardfaceehonda
  jp    z,.setscoreboardfaceblock
  dec   a
  ld    b,backgroundcammyblock+2
  ld    de,scoreboardfacecammy
  jp    z,.setscoreboardfaceblock
  dec   a
  ld    b,backgroundvegablock+2
  ld    de,scoreboardfacevega
  jp    z,.setscoreboardfaceblock

  .setscoreboardfaceblock:
	ld		a,b
  call	block12      		      ;at address $4000 / page 1
  ret

scoreboardpart1:
	db    0,0,16,0
	db    20,0,70,0
	db    96,0,12,0
	db    0,0,$98
scoreboardpart2:
	db    96,0,16,0
	db    140,0,70,0
	db    96,0,12,0
	db    0,0,$98
scoreboardpart3:
	db    192,0,16,0
	db    120,0,67,0
	db    16,0,12,0
	db    0,0,$98
scoreboardpart4:
	db    192+16,0,16+10,0
	db    120,0,67+12,0
	db    16,0,2,0
	db    0,0,$98	
scoreboardpart5:
	db    0,0,28,0
	db    0,0,62,0
	db    20,0,22,0
	db    0,0,$98
scoreboardpart6:
	db    20,0,28,0
	db    236,0,62,0
	db    20,0,22,0
	db    0,0,$98
scoreboardpart7:
	db    244,0,16,0
	db    104,0,80,0
	db    11,0,11,0
	db    0,0,$98	
scoreboardpart8:
	db    244,0,16,0
	db    141,0,80,0
	db    11,0,11,0
	db    0,0,$98	
  if movescreendown>43
FixBottompage0:
	db    0,0,0,1
	db    0,0,0,0
	db    0,1,-44+movescreendown,0
	db    0,0,$d0
  endif
copypage0to1:
	db    0,0,0,0
	db    0,0,0,1
	db    0,1,0,1
	db    0,0,$d0
copypage0to2:
	db    0,0,0,0
	db    0,0,0,2
	db    0,1,0,1
	db    0,0,$d0
copypage0to3:
	db    0,0,0,0
	db    0,0,0,3
	db    0,1,0,1
	db    0,0,$d0	

SetScoreboardfacePlayer1:
  push  de
  exx
  pop   hl
  ld    de,10
  exx
  ld    de,128
  ld    b,22                  ;amount of lines to copy
  .loop:
  xor   a                     ;SetVdp_Write page 0
  call  SetVdp_Write
  add   hl,de
  exx
  ld    b,10
  ld    c,$98                 ;out port for otir
  otir
  add   hl,de
  exx
  djnz  .loop
  ret

SetScoreboardfacePlayer2:     ;player 2 need to adjust colors 3,4+5
  push  de
  exx
  pop   hl
  ld    de,10
  exx
  ld    de,128
  ld    b,22                  ;amount of lines to copy
  .loop:
  xor   a                     ;SetVdp_Write page 0
  call  SetVdp_Write
  add   hl,de
  exx
  ld    b,10
  add   hl,de
  .outiloop:
  ld    a,(hl)
  ;convert color 3,4,5 to color 6,7,8
  and   %0000 1111
  jr    z,.lowbitsdone
  cp    8
  jr    nc,.lowbitsdone
  add   a,3
  .lowbitsdone:
  ld    c,a                   ;low bits in c
  
  ld    a,(hl)  
  and   %1111 0000
  jr    z,.highbitsdone
  cp    8*16
  jr    nc,.highbitsdone
  add   a,3*16
  .highbitsdone:
  or    c                     ;high bits in a, add low bits which are in e
  ;/convert color 3,4,5 to color 6,7,8
  out   ($98),a
  inc   hl
  dec   b
  jp    nz,.outiloop
  exx
  djnz  .loop
  ret