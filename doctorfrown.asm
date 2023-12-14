    processor 6502

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Include required filed with VCS register memory mapping and macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    include "macro.h"
    include "vcs.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare variables starting from memory address $80
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg.u Variables
    org $80

BackgroundColor     byte                    ; color of the background
PlayerXPos          byte                    ; player x-position
PlayerYPos          byte                    ; player y-position
EnemyXPos           byte                    ; enemy x-position
EnemyYPos           byte                    ; enemy y-position
EnemyMoveCheck      byte                    ; check for if enemy can move
GameState           byte                    ; state of game, bit 0 value of 0 is game over, bit 1 value shows if smile is held
SmileXPos           byte                    ; smile x-position
SmileYPos           byte                    ; smile y-position
Random              byte                    ; random number generated to set smile position, number of smiles before frown appears, and frames Doctor Frown will appear for
FrownTime           byte                    ; amount of frames left that Doctor Frown will appear for
SmileCount          byte                    ; amount of smiles held for this round
ScoreSprite         byte                    ; stores sprite bit pattern for score
TimerSprite         byte                    ; stores sprite bit pattern for timer
OnesDigitOffset     word                    ; lookup table offset for digit units
TensDigitOffset     word                    ; lookup table offset for digit tens
Score               byte                    ; 2-digit score stored as BCD
FrownCounts         byte                    ; 2-digit timer stored as BCD, amount of counts left for Doctor Frown
Temp                byte                    ; variable to store temporary values
AudioTime           byte                    ; timer for current audio round

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PLAYER_HEIGHT = 4
SPRITE_HEIGHT = 9
DIGITS_HEIGHT = 5

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start ROM code at memory address $F000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg code
    org $F000

Reset:
    CLEAN_START                             ; reset memory and registers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize RAM variables and TIA registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #$35
    sta BackgroundColor                     ; store background color as orange
    lda #62
    sta PlayerXPos                          ; start player x-position at 68
    lda #10
    sta PlayerYPos                          ; start player y-position at 10
    lda #62
    sta EnemyXPos                           ; start enemy y-position at 62
    lda #75
    sta EnemyYPos                           ; start enemy y-position at 75
    lda #5
    sta EnemyMoveCheck                      ; set enemy move check to 5
    lda #1
    sta GameState                           ; set game state to active, with no smiles held
    lda #62
    sta SmileXPos                           ; start smile x-position at 62
    lda #83
    sta SmileYPos                           ; start smile y-position at 83
    lda #%11010100
    sta Random                              ; Random = $D4
    lda #0
    sta FrownTime                           ; set FrownTime to 0
    sta FrownCounts                         ; set FrownCounts to 0
    sta SmileCount                          ; set SmileCount to 0
    lda #$FC
    sta COLUPF                              ; set ball color to gold
    lda #$1C
    sta COLUP1                              ; set smile color to yellow
    lda #$79
    sta COLUP0                              ; set frown color to purple
    lda #0
    sta Score                               ; set score to 0
    lda #60
    sta AudioTime                           ; set audio time to 60 (frames)
    lda #12
    sta AUDC1                               ; set Audio tone

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start main display loop and frame rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StartFrame:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display 3 Lines of VSYNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2
    sta VBLANK
    sta VSYNC                               ; turn on VBLANK and VSYNC

    sta WSYNC
    sta WSYNC
    sta WSYNC                               ; display 3 lines of VSYNC

    lda #0
    sta VSYNC                               ; turn off VSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations and tasks performed in VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda PlayerXPos
    ldy #4
    jsr SetObjectXPos                       ; set ball (player) x-position

    lda EnemyXPos
    ldy #0
    jsr SetObjectXPos                       ; set player 0 (enemy) x-position

    lda SmileXPos
    ldy #1
    jsr SetObjectXPos                       ; set player 1 (smile) x-position

    jsr CalculateDigitOffset                ; calculate the scoreboard digit lookup table offset

    sta WSYNC                               ; HMOVE must occur immediately as HBLANK starts, so begin a WSYNC command before
    sta HMOVE                               ; apply the horizontal offsets previously set

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display remaining 32 Lines of VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ldx #32
LoopVBlank:
    sta WSYNC
    dex
    bne LoopVBlank                          ; display remaining 32 lines of VBLANK

    sta VBLANK                              ; turn off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clear playfield for scoreboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #0
    sta COLUBK
    sta PF0
    sta PF1
    sta PF2
    sta GRP0
    sta GRP1
    sta CTRLPF                              ; clear TIA registers to display scoreboard

    lda #$1E
    sta COLUPF                              ; set digit color to yellow

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the 10 visible scanlines of the scoreboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ldx #DIGITS_HEIGHT

.ScoreDigitLoop:
    ldy TensDigitOffset
    lda Digits,Y                            ; load tens digit bit pattern from lookup table
    and #$F0                                ; mask/remove graphic for units digit
    sta ScoreSprite

    ldy OnesDigitOffset
    lda Digits,Y                            ; load ones digit bit pattern from lookup table
    and #$0F                                ; mask/remove graphic for tens digit
    ora ScoreSprite                         ; OR, has effect of merging, merge Unit with Tens
    sta ScoreSprite
    sta WSYNC
    sta PF1                                 ; update PF1 (middle) to display score sprite

    ldy TensDigitOffset+1                   ; perform above for the timer
    lda Digits,Y
    and #$F0
    sta TimerSprite

    ldy OnesDigitOffset+1
    lda Digits,Y
    and #$0F
    ora TimerSprite
    sta TimerSprite

    jsr Sleep12Cycles                       ; wait for beam to reach position to display timer

    sta PF1

    ldy ScoreSprite                         ; preload for next scanline
    sta WSYNC                               ; wait for next scanline

    sty PF1                                 ; update playfield for score
    inc TensDigitOffset
    inc TensDigitOffset+1
    inc OnesDigitOffset
    inc OnesDigitOffset+1

    jsr Sleep12Cycles

    dex
    sta PF1
    bne .ScoreDigitLoop                     ; loop for height of digits

    sta WSYNC

    lda #0
    sta PF0
    sta PF1
    sta PF2

    sta WSYNC
    sta WSYNC
    sta WSYNC
    sta WSYNC                               ; 4 lines of padding for scoreboard

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the remaining 178 visible scanlines [2-line Kernel, 88 scanlines]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
VisibleScanline:
    lda BackgroundColor
    sta COLUBK                              ; set background color TIA register

    lda #%00100000
    sta CTRLPF                              ; set the ball width to 2 (5th bit set)

    ldx #88
.GameLineLoop:

    lda GameState
    and #%00000001
    beq .GameStateCheck                     ; if Game Over, skip drawing player

.DrawPlayerCheck:
    txa
    sec
    sbc PlayerYPos
    cmp #PLAYER_HEIGHT                      ; compare current scanline with player y-position
    lda #%00000010                          ; draw ball
    bcc .GameStateCheck                     ; if scanline within player height, draw the player
    lda #0                                  ; skip drawing ball

.GameStateCheck:
    sta WSYNC
    sta ENABL                               ; store ball into TIA register

    lda #0
    ora FrownCounts
    bne .DrawEnemyCheck                     ; if FrownCounts not 0, draw enemy

.DrawSmileCheck:
    txa                                     ; else, draw smile
    sec
    sbc SmileYPos
    cmp #SPRITE_HEIGHT
    bcc .DrawSmile
    lda #0

.DrawSmile:
    tay
    lda #%00000101
    sta NUSIZ1                              ; stretch player1 sprite
    lda SmileSprite,Y
    sta WSYNC
    sta GRP1
    jmp .FinishGameLoop                     ; skip over drawing enemy

.DrawEnemyCheck:
    txa
    sec
    sbc EnemyYPos
    cmp #SPRITE_HEIGHT
    bcc .DrawEnemy
    lda #0

.DrawEnemy:
    tay
    lda #%00000101
    sta NUSIZ0                              ; stretch player0 sprite
    lda FrownSprite,Y
    sta WSYNC
    sta GRP0

.FinishGameLoop:
    dex
    bne .GameLineLoop                       ; loop back to top whilst drawing visible scanlines

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start Overscan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2
    sta VBLANK                              ; turn on VBLANK

    ldx #25
LoopOverscan:
    sta WSYNC
    dex
    bne LoopOverscan                        ; display 25 lines of VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process joystick input checks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    sta WSYNC

    lda GameState
    and #%00000001
    beq EndPlayerInput                      ; if Game Over, ignore player input

CheckP0Up:
    lda #%00010000                          ; player0 joystick up
    bit SWCHA
    bne CheckP0Down                         ; if bit pattern doesn't match, bypass

    lda #80
    cmp PlayerYPos
    beq CheckP0Down                         ; prevent moving beyond top of screen
    inc PlayerYPos

CheckP0Down:
    lda #%00100000                          ; player0 joystick down
    bit SWCHA
    bne CheckP0Left                         ; if bit pattern doesn't match, bypass

    lda #5
    cmp PlayerYPos
    beq CheckP0Left                         ; prevent moving beyond bottom of screen
    dec PlayerYPos

CheckP0Left:
    lda #%01000000                          ; player0 joystick left
    bit SWCHA
    bne CheckP0right                        ; if bit pattern doesn't match, bypass

    lda #1
    cmp PlayerXPos
    beq CheckP0right                        ; prevent moving beyond left of screen
    dec PlayerXPos

CheckP0right:
    lda #%10000000                          ; player0 joystick right
    bit SWCHA
    bne EndPlayerInput                      ; if bit pattern doesn't match, bypass
    
    lda #140
    cmp PlayerXPos
    beq EndPlayerInput                      ; prevent moving beyond right of screen
    inc PlayerXPos

EndPlayerInput:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CheckFrownTime
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckFrownTime:
    lda #0
    ora FrownCounts
    bne MoveEnemy                           ; if FrownCounts not 0, move to Enemy Logic

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Move Smile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MoveSmile:
    sta WSYNC

    lda #$35
    sta BackgroundColor                     ; set background color to happy background

    lda SmileYPos
    clc
    cmp #5
    bmi .NewSmilePos                        ; if smile y-pos < 5, reset y-pos
    dec SmileYPos                           ; else, decrement smile y-pos
    jmp ProcessSmileAudio

.NewSmilePos:
    jsr ResetSmilePos                       ; reset and randomize smile position

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process Smile Audio
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ProcessSmileAudio:
    sta WSYNC

    dec AudioTime
    lda #1
    sta AUDV1

    lda #45
    cmp AudioTime
    bmi .PlaySmileAudio                     ; if audio time is above 45, play audio

    lda #0
    sta AUDV1                               ; else mute audio
    cmp AudioTime
    bne CheckCollisions                     ; if audio time is not 0, jump to collision check

    lda #90
    sta AudioTime                           ; else, reset audio time to 90
    jmp CheckCollisions

.PlaySmileAudio
    lda #25
    sec
    sbc SmileCount
    sta AUDF1                               ; increase pitch of audio as smiles are collected
    jmp CheckCollisions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Move enemy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MoveEnemy:
    sta WSYNC

    lda GameState
    and #%00000001
    beq CheckCollisions                     ; if Game Over, jump straight to processing collisions

    dec EnemyMoveCheck

    lda #0
    ora FrownTime
    bne DecrementFrownTime                  ; if FrownTime not 0, decrement FrownTime
    
    dec FrownCounts                         ; else, decrement the FrownCount
    lda #120
    sta FrownTime                           ; reset FrownTime back to 120
    jmp CheckCanEnemyMove

DecrementFrownTime:
    dec FrownTime

CheckCanEnemyMove:
    lda #0
    cmp EnemyMoveCheck
    bne ProcessEnemyAudio                   ; if enemy shouldn't move, jump to process enemy audio
    lda #4
    sta EnemyMoveCheck                      ; else, reset move check, and start enemy movement

CheckEnemyY:
    lda PlayerYPos
    cmp EnemyYPos
    beq CheckEnemyX                         ; if player y-position is same as enemy y-position, move to check x-position
    bmi EnemyMoveDown                       ; if player is below enemy, move enemy down

EnemyMoveUp:
    inc EnemyYPos                           ; else, move enemy up
    jmp CheckEnemyX

EnemyMoveDown:
    dec EnemyYPos

CheckEnemyX:
    lda PlayerXPos
    cmp EnemyXPos
    beq ProcessEnemyAudio                   ; if player x-position is same as enemy x-position, move to process audio
    bmi EnemyMoveLeft                       ; if player is left of enemy, move enemy left

EnemyMoveRight:
    inc EnemyXPos                           ; else move enemy right
    jmp ProcessEnemyAudio

EnemyMoveLeft:
    dec EnemyXPos

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process Enemy Audio
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ProcessEnemyAudio:
    sta WSYNC

    dec AudioTime
    lda #1
    sta AUDV1

    lda #20
    cmp AudioTime
    bmi CheckCollisions                     ; if audio time is above 20, play audio

    lda #0
    sta AUDV1                               ; else, mute audio
    cmp AudioTime
    bne CheckCollisions                     ; if audio time is not 0, jump to collision check

    lda #40
    sta AudioTime                           ; else, reset audio time to 40
    jmp CheckCollisions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check Collision Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckCollisions:

CheckSmileCollision:
    sta WSYNC

    lda #%01000000                          ; bit 6 set for collision between P1 and Ball
    bit CXP1FB
    beq CheckEnemyCollision                 ; if there was no collision, jump to check enemy collision

AddSmile:
    inc SmileCount                          ; increment the smile count for this round

    sed                                     ; increment score for BCD
    lda Score
    clc
    adc #1
    sta Score
    cld

    jsr ResetSmilePos                       ; reset and randomize the smile position

    lda #5
    cmp SmileCount
    bne CheckEnemyCollision                 ; if smile count is not equal to 5, jump to check enemy collision
    sta FrownCounts                         ; else, set FrownCounts to 5
    lda #120
    sta FrownTime                           ; set FrownTime to 120
    lda #0
    sta SmileCount                          ; reset smile counter
    lda #$61
    sta BackgroundColor                     ; set background color to enemy round color

    lda #30
    sta AUDF1                               ; start enemy audio

    lda #48                                 ; load screen mid point (192 / 2 = 96 => / 2 [2-line kernel] = 48)
    sec
    sbc PlayerYPos                          ; subtract this from player y-pos
    sta EnemyYPos                           ; set as new enemy y-pos, will prevent enemy from spawning on player
                                            ; and negative wrap-around should prevent enemy going off screen

CheckEnemyCollision:
    sta WSYNC

    lda #%01000000                          ; bit 6 set for collision between P0 and Ball
    bit CXP0FB                              ; compare with bit in collision register
    bne GameOver                            ; if not zero, N flag set, collision occured
    jmp ProcessSwitches

GameOver:
    lda #$30
    sta BackgroundColor                     ; set background color to red
    lda #$46
    sta COLUP0                              ; set doctor frown color to red
    lda #%00000000
    sta GameState                           ; set Game State to Game Over
    lda #0
    sta AUDV1                               ; mute audio

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process Switch Input on the GameBoard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ProcessSwitches:
        lda SWCHB       ; load in the state of the switches
        lsr             ; D0 is now in C
        bcs EndFrame    ; if D0 was on, the RESET switch was not held
        jmp Reset       ; reset the game

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop back to start next frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
EndFrame:
    sta CXCLR                               ; clear collision flags before next frame

    lda #0
    sta VBLANK                              ; turn off VBLANK

    jmp StartFrame                          ; loop back to a new frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handle object horizontal position with fine offset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A: target x-coordinate in pixels of object
;; Y: object type (0: player0, 1: player1, 2: missile0, 3: missile1, 4: ball)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetObjectXPos subroutine
    sta WSYNC                               ; start a fresh new scanline
    sec                                     ; make sure carry-flag is set before subtraction
.Div15Loop
    sbc #15                                 ; subtract 15 from accumulator
    bcs .Div15Loop                          ; loop until carry-flag is clear
    eor #7                                  ; handle offset range from -8 to 7
    asl
    asl
    asl
    asl                                     ; four shift lefts to get only the top 4 bits
    sta HMP0,Y                              ; store the fine offset to the correct HMxx
    sta RESP0,Y                             ; fix object position in 15-step increment
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate random value using Linear-Feedback Shift Register
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate a LFSR random number
;; Divide random value by 2 to limit range of result to screen width
;; Add 15 to compensate for left playfield width
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GetRandom subroutine
    lda Random
    asl
    eor Random
    asl
    eor Random
    asl
    asl
    eor Random
    asl
    rol Random                              ; perform series of shifts and bit operations
    
    lsr                                     ; divide value by 4 with 2 right shifts
    sta Random
    lda #5
    adc Random                              ; adds 5 to compensate for left side playfield
    sta Random                              ; set random

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reset Smile Position
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reset the current Smile position
;; Generate a new Random value, load this value into the Smile X-Position
;; Reset Smile Y-Position to the starting value
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ResetSmilePos subroutine
    jsr GetRandom
    lda Random
    sta SmileXPos
    lda #83
    sta SmileYPos

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handle scoreboard digits to be displayed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For lo-byte, multiply by 5 to get offset
;;      - (N*2*2)+N
;;
;; For hi-byte, divide by 16 then multiply by 5 to get offset
;;      - (N/2/2)+(N/2/2/2/2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CalculateDigitOffset subroutine
    ldx #1
.PrepareScoreLoop
    lda Score,X                             ; retrieve timer (X=1), then score (X=0)
    and #$0F                                ; AND to mask first 4 bits to 0, retaining last 4 only
    sta Temp
    asl
    asl
    adc Temp                                ; multiply A by 5
    sta OnesDigitOffset,X

    lda Score,X
    and #$F0                                ; AND to mask last 4 bits to 0, retaining first 4 only
    lsr
    lsr
    sta Temp                                ; divide by 4
    lsr
    lsr                                     ; divide by 16
    adc Temp
    sta TensDigitOffset,X

    dex
    bpl .PrepareScoreLoop                   ; loop twice, while X >= 0

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wait 12 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Sleep12Cycles subroutine
    rts                                     ; total cycles used for instructions - 12

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ROM Lookup Tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FrownSprite:
    .byte #%00000000
    .byte #%00011100
    .byte #%00111110
    .byte #%01011101
    .byte #%01100011
    .byte #%01111111
    .byte #%01101011
    .byte #%00101010
    .byte #%00011100

SmileSprite:
    .byte #%00000000
    .byte #%00011100
    .byte #%00111110
    .byte #%01100011
    .byte #%01011101
    .byte #%01111111
    .byte #%01101011
    .byte #%00101010
    .byte #%00011100

Digits:
    .byte %01110111                 ; ### ###
    .byte %01010101                 ; # # # #
    .byte %01010101                 ; # # # #
    .byte %01010101                 ; # # # #
    .byte %01110111                 ; ### ###

    .byte %00010001                 ;   #   #
    .byte %00010001                 ;   #   #
    .byte %00010001                 ;   #   #
    .byte %00010001                 ;   #   #
    .byte %00010001                 ;   #   #

    .byte %01110111                 ; ### ###
    .byte %00010001                 ;   #   #
    .byte %01110111                 ; ### ###
    .byte %01000100                 ; #   #
    .byte %01110111                 ; ### ###

    .byte %01110111                 ; ### ###
    .byte %00010001                 ;   #   #
    .byte %00110011                 ;  ##  ##
    .byte %00010001                 ;   #   #
    .byte %01110111                 ; ### ###

    .byte %01010101                 ; # # # #
    .byte %01010101                 ; # # # #
    .byte %01110111                 ; ### ###
    .byte %00010001                 ;   #   #
    .byte %00010001                 ;   #   #

    .byte %01110111                 ; ### ###
    .byte %01000100                 ; #   #
    .byte %01110111                 ; ### ###
    .byte %00010001                 ;   #   #
    .byte %01110111                 ; ### ###

    .byte %01110111                 ; ### ###
    .byte %01000100                 ; #   #
    .byte %01110111                 ; ### ###
    .byte %01010101                 ; # # # #
    .byte %01110111                 ; ### ###

    .byte %01110111                 ; ### ###
    .byte %00010001                 ;   #   #
    .byte %00010001                 ;   #   #
    .byte %00010001                 ;   #   #
    .byte %00010001                 ;   #   #

    .byte %01110111                 ; ### ###
    .byte %01010101                 ; # # # #
    .byte %01110111                 ; ### ###
    .byte %01010101                 ; # # # #
    .byte %01110111                 ; ### ###

    .byte %01110111                 ; ### ###
    .byte %01010101                 ; # # # #
    .byte %01110111                 ; ### ###
    .byte %00010001                 ;   #   #
    .byte %01110111                 ; ### ###

    .byte %00100010                 ;  #   #
    .byte %01010101                 ; # # # #
    .byte %01110111                 ; ### ###
    .byte %01010101                 ; # # # #
    .byte %01010101                 ; # # # #

    .byte %01110111                 ; ### ###
    .byte %01010101                 ; # # # #
    .byte %01100110                 ; ##  ##
    .byte %01010101                 ; # # # #
    .byte %01110111                 ; ### ###

    .byte %01110111                 ; ### ###
    .byte %01000100                 ; #   #
    .byte %01000100                 ; #   #
    .byte %01000100                 ; #   #
    .byte %01110111                 ; ### ###

    .byte %01100110                 ; ##  ##
    .byte %01010101                 ; # # # #
    .byte %01010101                 ; # # # #
    .byte %01010101                 ; # # # #
    .byte %01100110                 ; ##  ##

    .byte %01110111                 ; ### ###
    .byte %01000100                 ; #   #
    .byte %01110111                 ; ### ###
    .byte %01000100                 ; #   #
    .byte %01110111                 ; ### ###

    .byte %01110111                 ; ### ###
    .byte %01000100                 ; #   #
    .byte %01100110                 ; ##  ##
    .byte %01000100                 ; #   #
    .byte %01000100                 ; #   #

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete ROM size with 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC
    .word Reset
    .word Reset