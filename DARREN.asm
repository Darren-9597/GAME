EXTERNDELAY = 3
.model small
.stack 100h
.data
;-----------MAIN MENU-------------------------------------------------------------------------
	main_menu db 'MAIN MENU','$'
	game1 db 'PRESS 1 TO PLAY BRICK BREAKER','$';
	game2 db 'PRESS 2 TO PLAY TRUTH AND DARE', '$';
	mechanics_instruction db 'PRESS 3 TO SHOW MECHANICS','$'
	main_menu_exit db 'PRESS 4 TO EXIT TO DOSBOX','$';
	subject1 db 'COMPUTER ARCHITECTURE','$'
	subject2 db 'AND ORGANIZATION','$'
	subject3 db '(CPE 412)FINAL PROJECT','$'
	prepared_by db 'PREPARED BY: DARREN ESCABARTE','$'
	
	exit_game db 0
	choice db 0
	
;---------------- MECHANICS -------------------------------------------------------------------
	TOD_game_mechanics db 'Truth or Dare Game Mechanics:', '$'
	mechanics_tod1 db  '1.Play with your friends','$'
	mechanics_tod2 db	'2.Decide if Heads or Tail','$'
	mechanics_tod3 db	'3.The result will randomly generate','$'
	mechanics_tod4 db	'4.If, Heads: answer a Truth question',0AH,0DH,0AH,0DH,'Else, Tails: perform a Dare challenge',0AH,0DH,'$'
	mechanics_tod5 db	'5.Retry until boredom ','$'
	
	brick_breaker_game_mechanics db 'BRICK BREAKER GAME MECHANICS: ', '$'
	mechanics_bb1 db  '1.Use the left and right arrow keys to    move the striker.','$'
	mechanics_bb2 db  '2.Bounce the ball off the striker to      hit the bricks.', '$'
	mechanics_bb3 db  '3.Break all the bricks to score points.', '$'
	mechanics_bb4 db  '4.If the ball falls off the bottom,       you lose a life.', '$'
	mechanics_bb5 db  '5.Retry until boredom ','$'

	title_mechanics db 'MECHANICS:','$'
	bb_mechanics db 'PRESS 1 TO FOR BRICK BREAKER MECHANICS','$'
	tod_mechanics db 'PRESS 2 TO FOR TRUTH OR DARE MECHANICS','$'
	mechanics_to_main_menu db 'PRESS 3 TO BACK TO MAIN MENU','$'
	
	mechanics_main_menu db 'PRESS X TO BACK TO THE MECHANICS MENU','$'



;-------------------BRICK BREAKER---------------------------------------------------------------------
msg DB "GAME OVER", 0	; first msg
	nSize DW ($ - msg)-1
	
score db 'Score: $'
scoreCount dw 0
lives db '              Lives: '
livesCount db 51
ending db ' $'

ballY dw 163
ballX dw 158
ballLeft db 1
ballUp db 1
color db ?
startx dw ?
starty dw ?
endx dw ?
endy dw ?       
begin db 0
strikerX dw 140
strikerY dw 170
innerDelay db 0
boundaryEnd dw 250
boundaryStart dw 30

brick1x dw 45
brick1y dw 25
brick2x dw 85
brick2y dw 25
brick3x dw 125
brick3y dw 25
brick4x dw 165
brick4y dw 25
brick5x dw 205
brick5y dw 25
brick6x dw 245
brick6y dw 25


brick7x dw 45
brick7y dw 45
brick8x dw 85
brick8y dw 45
brick9x dw 125
brick9y dw 45
brick10x dw 165
brick10y dw 45
brick11x dw 205
brick11y dw 45
brick12x dw 245
brick12y dw 45


terminate_brickbreaker db  'PRESS X TO TERMINATE', '$'


	
;-----------------TRUTH OR DARE GAME--------------------------------------------------------
	;TRUTH OR DARE GAME
	prompt_menu_tod db 'TRUTH AND DARE GAME','$' 
	start_game_tod db 'PRESS 1 TO START THE GAME','$'
	mechanics_tod db 'PRESS 2 FOR MECHANICS','$'
	tod_exit_program db 'PRESS 3 TO EXIT TO THE MAIN MENU','$'
	
	chonker_back_to_main_menu db 'PRESS X TO CHONKER GAME MENU', '$'
	chonker_try_again db 'PRESS A TO RETRY', '$'
	spaces db "                                        ",'$'
	
	tod_start_game 	db 'PRESS "SPACE" TO TOSS A COIN','$'
	tod_toss_coin 	db  'Tossing a coin....','$'
	toss_coin_result db 'The result is: ','$'
	result_one		db 'You got HEAD.','$'
	result_heads 	db	'Here is your truth question','$'
					
	result_two 		db 'You got TAILS','$'
	result_tails	db	'Here is your dare challenge','$'
	
	back_to_main_menu db 'PRESS X TO TRUTH AND DARE GAME MENU', '$'
	please_try_again db 'PRESS A TO RETRY', '$'
	
    truth_question1    DB 'WHAT IS YOUR BIGGEST FEAR?$','$'
    truth_question2    DB 'WHEN WAS THE LAST TIME YOU CRIED?', '$'
    truth_question3    DB 'DO YOU HAVE A HIDDEN TALENT?', '$'
    truth_question4    DB 'WHAT IS YOUR DREAM LIFE?','$'
    truth_question5    DB 'WHAT DO YOU VALUE THE MOST -' ,0AH,0DH,0AH,0DH,' MONEY, FAME, SUCCESS, FRIENDS, FAMILY?',0AH,0DH,0AH,0DH, '$'
    truth_question6    DB 'WHAT IS THE BIGGEST MISCONCEPTION ABOUT YOU?',  '$'
    truth_question7    DB 'WHAT IS THE BEST PIECE OF', 0AH,0DH,0AH,0DH, '       ADVICE YOU HAVE BEEN GIVEN?' ,  '$'
    truth_question8    DB 'WHAT IS THE BEST THING ANYONE HAS EVER DONE FOR YOU?','$'
    truth_question9	  DB 'WHAT IS THE BIGGEST MISTAKE',0AH,0DH,0AH,0DH,'           YOU HAVE EVER MADE?', '$'
    truth_question10    DB 'WHAT IS YOUR PROUDEST ACCOMPLISHMENT?',  '$'
	
	truth_questions dw offset truth_question1
					dw offset truth_question2
					dw offset truth_question3
					dw offset truth_question4
					dw offset truth_question5
					dw offset truth_question6
					dw offset truth_question7
					dw offset truth_question8
					dw offset truth_question9
					dw offset truth_question10

    num_truth         EQU ($ - truth_questions) /2

    dare_challenge1    DB 'SING A SONG', '$'
    dare_challenge2    DB 'DANCE A TIKTOK CHALLENGE',  '$'
    dare_challenge3    DB 'TELL A JOKE','$'
    dare_challenge4    DB 'SPIN AROUND 10 TIMES', '$'
    dare_challenge5    DB 'CLAP YOUR HANDS 20 TIMES','$'
	dare_challenge6	   DB 'PAPASRA MI MAAM, PLEASE', '$'
    dare_challenge7    DB 'MAKE A FUNNY FACE', '$'
    dare_challenge8    DB 'CLAP YOUR HANDS 20 TIMES', '$'
    dare_challenge9    DB 'MAKE A FUNNY FACE','$'
    dare_challenge10   DB 'DO PUSH-UPS 10 TIMES', '$'

	
	dare_challenges dw offset dare_challenge1
					dw offset dare_challenge2
					dw offset dare_challenge3
					dw offset dare_challenge4
					dw offset dare_challenge5
					dw offset dare_challenge6
					dw offset dare_challenge7
					dw offset dare_challenge8
					dw offset dare_challenge9
					dw offset dare_challenge10
					 			 
    num_dare          EQU ($ - dare_challenges) /2	
;--------------------------------------------------------------------------------------------------------------------------
				
.code
  main proc near
    mov ax, @data ; initialize data segment
    mov ds, ax
	
	call clear_screen
	
	call video_mode_black
	call project_main_menu
	
; 	waits for a key press
	wait_for_key: ;
	mov ah, 00h
	int 16h
	mov choice, al
	
	
; Process user choice
    cmp choice, '1'
    je play_game1       ; If '1' was pressed, jump to play_game1
    cmp choice, '2'
    je play_game2       ; If '2' was pressed, jump to play_game2
	cmp choice, '3'
    je mechanics_of_game       ; If '2' was pressed, jump to play_game2
    cmp choice, '4'
    je exit_program     ; If '3' was pressed, jump to exit_program
	jmp wait_for_key
	
	exit_program:
	mov exit_game, 1
	call exit_process
	ret

play_game1:
	call clear_screen
	call run_game1
    jmp main            ; Return to main menu after the game ends


play_game2:
	call clear_screen
	call run_game2
    jmp main
	
mechanics_of_game:
	call clear_screen
	call video_mode_black
	call mechanics_of_games
		
		mechanics_wait_for_key:
			call wait_for_keys
			
					;Process user choice
				cmp choice, '1'
				je bb_instructions       ; If '1' was pressed, jump to play_game1
				cmp choice, '2'
				je tod_instructions      ; If '2' was pressed, jump to play_game2
				cmp choice, '3'
				je go_back_main_menu     ; If '3' was pressed, jump to exit_program
				jmp mechanics_wait_for_key	
				
		bb_instructions:
				call clear_screen
				call video_mode_black
				call mechanics_bb_game
				
					mechanics_to_bb_menu:
						mov dh, 20 ; row position (top)
						mov dl, 2 ; column position (left)
						call set_cursor
						
						mov dx, offset mechanics_main_menu
						call display_screen
						
						call wait_for_keys
					
						cmp choice, 'X'
						je bb_main_menu
						cmp choice, 'x'
						je bb_main_menu
						
						jmp mechanics_to_bb_menu
				
				bb_main_menu:
					jmp main
	
		tod_instructions:
				call clear_screen
				call video_mode_black	
				call mechanics_tod_game
				
					mechanics_to_tod_menu:
						mov dh, 20 ; row position (top)
						mov dl, 2 ; column position (left)
						call set_cursor
						
						mov dx, offset mechanics_main_menu
						call display_screen
						
						call wait_for_keys
					
						cmp choice, 'X'
						je tod_main_menu
						cmp choice, 'x'
						je tod_main_menu
						
						jmp mechanics_to_tod_menu
				
				tod_main_menu:
					jmp mechanics_of_game

		go_back_main_menu:
			jmp main















				
;--------------------------------------------------------------------------------------------------------------------
run_game1:
	
redrawStriker macro visColor

mov color, visColor
call drawStriker
endm

redrawBall macro visColor
    mov color, visColor
    call drawball
endm

BuildBrick macro  A, B
    push ax
    push bx
    mov ax, A
    mov bx, B
    call AddBrick
    pop bx
    pop ax
endm
DestroyBrick macro  A, B
    push ax
    push bx
    mov ax, A
    mov bx, B
    call RemoveBrick
    call beep     
    inc scoreCount
    call DrawLivesScores
    pop bx
    pop ax
endm

BrickCollision MACRO X, Y
local copper
    push ax
    push bx
    push cx
    push dx
    mov ax, ballY
    mov bx, ballX
    mov cx, X
    mov dx, Y
    
    cmp dx, ballY
    jl copper
    sub dx, 7
    
    cmp ballY, dx
    jl copper
    
    
    mov dx, X 
    
    cmp ballX, dx
    jl copper
    add dx, 30
    cmp dx, ballX
    jl copper
    
    call switcher
    DestroyBrick X, Y
    mov Y, 300
    cmp scoreCount, 12
    jne copper
    mov ah,4ch
    int 21h
    
    copper:
    pop dx
    pop cx
    pop bx
    pop ax                      
    
endm

    call setVideoMode
    call drawBoundary
    BuildBrick brick1x brick1y
    BuildBrick brick2x brick2y
    BuildBrick brick3x brick3y
    BuildBrick brick4x brick4y
    BuildBrick brick5x brick5y
    BuildBrick brick6x brick6y
    BuildBrick brick7x brick7y
    BuildBrick brick8x brick8y
    BuildBrick brick9x brick9y
    BuildBrick brick10x brick10y
    BuildBrick brick11x brick11y
    BuildBrick brick12x brick12y
    redrawStriker 7
    redrawBall 3                
    call DrawLivesScores
    
    call gameLoop  
	call terminate_game1
call exit_program	
	
	
;-------------------------------------------------------------------------------------------------	
run_game2:
    ; Code to start game 2 (TRUTH OR DARE)
		call clear_screen
		call video_mode_black
		jmp play_start_tod
					
	play_start_tod:
		call clear_screen	
		call video_mode_black
		
				mov dh, 2 ; row position (top)
				mov dl, 5 ; column position (left)
				call set_cursor
				
				lea dx, tod_start_game
				call display_screen
				
				;wait to press "space" key
			space_key:
				call wait_for_keys
				
				cmp choice, ' '
				je tossing_loop
				jne space_key
			
			tossing_loop:
			call clear_screen
				call coin_toss_function
				;make consequence
				jmp exit_to_main
				jmp exit_program
			
			;Add a new label to handle the game refresh logic
			game_main_menu:
				jmp main ; Return to main menu
			try_again:
				jmp play_start_tod
				jmp main
			
			exit_to_main:
				mov dh, 20 ; row position (top)
				mov dl, 2 ; column position (left)
				call set_cursor
				
				mov dx, offset back_to_main_menu
				call display_screen
				
				mov dh, 22 ; row position (top)
				mov dl, 2 ; column position (left)
				call set_cursor
				
				mov dx, offset please_try_again
				call display_screen
			
			;read a character from the keyboard	
				call wait_for_keys
			
				cmp choice, 'X'
				je game_main_menu
				cmp choice, 'x'
				je game_main_menu
				
				cmp choice, 'A'
				je try_again
				cmp choice, 'a'
				je try_again
				
				jmp exit_to_main
							

;###################################################################################################
;################################## MAIN MENU ######################################################
;###################################################################################################

project_main_menu proc near
; 	show the main menu
    mov ah, 02h ; set function to set cursor position
    mov dh, 3 ; row position (top)
    mov dl, 15 ; column position (left)
    mov bh, 0 ; page number
    int 10h ; call BIOS interrupt to set cursor position

	mov ah, 09h
	lea dx, main_menu
	int 21h
	
; 	show the game1
	mov ah, 02h ; set function to set cursor position
    mov dh, 6 ; row position (top)
    mov dl, 5 ; column position (left)
    mov bh, 0 ; page number
    int 10h ; call BIOS interrupt to set cursor position

	mov ah, 09h
	lea dx, game1
	int 21h
	
; 	show the game2
	mov ah, 02h ; set function to set cursor position
    mov dh, 8 ; row position (top)
    mov dl, 5 ; column position (left)
    mov bh, 0 ; page number
    int 10h ; call BIOS interrupt to set cursor position

	mov ah, 09h
	lea dx, game2
	int 21h
	
; 	show the exit to dosbox
	mov ah, 02h ; set function to set cursor position
    mov dh, 10 ; row position (top)
    mov dl, 5 ; column position (left)
    mov bh, 0 ; page number
    int 10h ; call BIOS interrupt to set cursor position

	mov ah, 09h
	lea dx, mechanics_instruction
	int 21h
	
; 	show the exit to dosbox
	mov ah, 02h ; set function to set cursor position
    mov dh, 12 ; row position (top)
    mov dl, 5 ; column position (left)
    mov bh, 0 ; page number
    int 10h ; call BIOS interrupt to set cursor position

	mov ah, 09h
	lea dx, main_menu_exit
	int 21h
	
; 	show the exit to dosbox
	mov ah, 02h ; set function to set cursor position
    mov dh, 15 ; row position (top)
    mov dl, 10 ; column position (left)
    mov bh, 0 ; page number
    int 10h ; call BIOS interrupt to set cursor position

	mov ah, 09h
	lea dx, subject1
	int 21h
	
	mov ah, 02h ; set function to set cursor position
    mov dh, 17 ; row position (top)
    mov dl, 12 ; column position (left)
    mov bh, 0 ; page number
    int 10h ; call BIOS interrupt to set cursor position

	mov ah, 09h
	lea dx, subject2
	int 21h
	
	mov ah, 02h ; set function to set cursor position
    mov dh, 19 ; row position (top)
    mov dl, 10 ; column position (left)
    mov bh, 0 ; page number
    int 10h ; call BIOS interrupt to set cursor position

	mov ah, 09h
	lea dx, subject3
	int 21h
	
; 	show the exit to dosbox
	mov ah, 02h ; set function to set cursor position
    mov dh, 22 ; row position (top)
    mov dl, 6 ; column position (left)
    mov bh, 0 ; page number
    int 10h ; call BIOS interrupt to set cursor position

	mov ah, 09h
	lea dx, prepared_by
	int 21h
ret
project_main_menu endp
		
;main menu for the mechanics of the games
mechanics_of_games proc near
	mov dh, 4 ; row position (top)
	mov dl, 5 ; column position (left)
	call set_cursor
			
	lea dx, title_mechanics
	call display_screen
			
	;shows the start game
	mov dh, 8 ; row position (top)
	mov dl, 1 ; column position (left)
	call set_cursor
			
	lea dx, bb_mechanics
	call display_screen
			
	;shows the mechanics 
	mov dh, 10 ; row position (top)
	mov dl, 1 ; column position (left)
	call set_cursor
			
	lea dx, tod_mechanics
	call display_screen
			
	;shows the exit to main menu
	mov dh, 12 ; row position (top)
	mov dl, 1 ; column position (left)
	call set_cursor
			
	lea dx, mechanics_to_main_menu
	call display_screen	

ret
mechanics_of_games endp

;######################################################################################################
;########################### MECHANICS PROCEDURES #####################################################
;######################################################################################################

;Shows the mechanics of the brick breaker game

mechanics_bb_game proc near
	mov dh, 2 ; row position (top)
	mov dl, 2 ; column position (left)
	call set_cursor
			
	lea dx, brick_breaker_game_mechanics
	call display_screen
			
	mov dh, 5 ; row position (top)
	mov dl, 1 ; column position (left)
	call set_cursor
			
	lea dx, mechanics_bb1
	call display_screen
			 
	mov dh, 8 ; row position (top)
	mov dl, 1 ; column position (left)
	call set_cursor
			
	lea dx, mechanics_bb2
	call display_screen
			
	mov dh, 11 ; row position (top)
	mov dl, 1 ; column position (left)
	call set_cursor
			
	lea dx, mechanics_bb3
	call display_screen	
	
	mov dh, 13 ; row position (top)
	mov dl, 1 ; column position (left)
	call set_cursor
			
	lea dx, mechanics_bb4
	call display_screen	
	
	mov dh, 16 ; row position (top)
	mov dl, 1 ; column position (left)
	call set_cursor
			
	lea dx, mechanics_bb5
	call display_screen	

ret
mechanics_bb_game endp 

;Shows the mechanics of the truth and dare game
mechanics_tod_game proc near
	mov dh, 2 ; row position (top)
	mov dl, 2 ; column position (left)
	call set_cursor
			
	lea dx, TOD_game_mechanics
	call display_screen
			
	mov dh, 5 ; row position (top)
	mov dl, 1 ; column position (left)
	call set_cursor
			
	lea dx, mechanics_tod1
	call display_screen
			 
	mov dh, 7 ; row position (top)
	mov dl, 1 ; column position (left)
	call set_cursor
			
	lea dx, mechanics_tod2
	call display_screen
			
	mov dh, 9 ; row position (top)
	mov dl, 1 ; column position (left)
	call set_cursor
			
	lea dx, mechanics_tod3
	call display_screen	
	
	mov dh, 11 ; row position (top)
	mov dl, 1 ; column position (left)
	call set_cursor
			
	lea dx, mechanics_tod4
	call display_screen	
	
	mov dh, 16 ; row position (top)
	mov dl, 1 ; column position (left)
	call set_cursor
			
	lea dx, mechanics_tod5
	call display_screen	
ret
mechanics_tod_game endp

;####################################################################################################
;######################## BRICK BREAKER PROCEDURES ##################################################
;####################################################################################################

DrawLivesScores proc
    push dx
    push ax
                 
    mov dh, 23 ;row
    mov dl, 5 ;col
    mov ah, 2 
    int 10h
    
    lea dx, score
    mov ah, 9
    int 21h
    
    call printScore
    
    lea dx,lives
    mov ah,9
    int 21h  

    pop ax
    pop dx
    ret
    DrawLivesScores endp

printScore proc
    push ax
    push bx
    push cx
    push dx
    
    mov cx,0
    
    mov ax,scoreCount
    ll:
    mov bx,10
    mov dx,0
    div bx
    push dx
    inc cx
    cmp ax,0
    jne ll
    
    l2:
    pop dx
    mov ah,2
    add dl,'0'
    int 21h
    loop l2
    
    pop dx
    pop cx
    pop bx
    pop ax
    
    ret
    printScore endp

sleep proc

mov cx,111111111111111b 

l:
loop l
ret
sleep endp

drawball proc
    push bx
    mov bx, ballX
    mov startx, bx
    add bx, 4 
    mov endx,   bx
    mov bx, ballY
    mov starty, bx
    add bx, 4
    mov endy,   bx
    
    pop bx
    
    call draw
ret
drawball endp

CollisionStriker proc    
    push ax
    push bx
    push cx
    push dx
    
    mov dx, ballY
    cmp dx, 165 ; striker surface check
    jl bhaag
    cmp dx, 170 ; striker missed
    jg fail 
    
    
    
    mov cx,strikerX   
    mov ax, ballX   
    cmp ax, cx  
    jl bhaag
    add cx , 40 
    cmp ax, cx
    jg bhaag
    
    mov ballUp, 1
    jmp bhaag
    
    
    fail:
    mov begin,0 
    dec livesCount
    cmp livesCount,48
    je khatam
    push ax
    push bx
    push cx
    push dx
    
    
    redrawBall 0
    
    mov ax, strikerX
    mov ballX,ax
    add ballX,18
    
    mov ballY,  163
    
    redrawBall 3
    mov ballUp, 1     ;monis
    mov ballLeft,0
    
    
    
    pop dx
    pop cx
    pop bx
    pop ax
    
    call DrawLivesScores
    jmp bhaag
    
    
    
    khatam:             
    call DrawLivesScores
    call terminate_game1
	call exit_program
	
                  
    bhaag:  
    
    pop dx
    pop cx
    pop bx
    pop ax
    ret
    CollisionStriker endp


switcher:
    cmp ballUp, 1
    je DownT
    jne UpT
    UpT:
    inc ballUp
    ret
    DownT:
    dec ballUp
    ret

AddBrick proc
    push ax
    push bx    
    mov startx, ax
    mov color, 13  
    mov ax, bx
    mov bx, startx
    
    add bx, 30
    
    mov endx,bx
    
    mov starty, ax 
    
    mov bx,starty
                    
    add bx,7
    mov endy,bx
     
    call draw
    pop bx
    pop ax 
    ret
    AddBrick endp

RemoveBrick proc 
    
    push ax
    push bx
    push cx
    push dx
       
    mov startx, ax
    mov color, 0  
    mov ax, bx
    mov bx, startx
    
    add bx, 30
    
    mov endx,bx
    
    mov starty, ax 
    
    mov bx,starty
    
    add bx,7
    mov endy,bx
     
    call draw 
    
    pop dx
    pop cx
    pop bx
    pop ax
    ret
    RemoveBrick endp

Collisionwall proc     
    
    mov bx, ballX
    mov cx, ballY
    
    checkLeftRight:
    cmp bx, 25; max left
    jl goRight
    cmp bx, 290; Max Right
    jg goLeft
    jmp checkUpDown
    goRight:
    mov ballLeft, 0 
    jmp checkUpDown;
    goLeft:
    mov ballLeft, 1
    checkUpDown:
    
    cmp cx, 13;max top
    jl goDown
    cmp cx, 184;max bottom
    jg goUp
    
    
    jmp noInput
    goUp:                                            
    mov ballUp,1
    jmp noInput
    goDown: 
    mov ballUp, 0
  
    ret
    Collisionwall endp

ajeebse:
ret
baller proc  
    
	inc innerDelay
	cmp innerDelay, EXTERNDELAY
	jne ajeebse 
	mov innerDelay, 0
    redrawBall 0  
    
	mov bx,ballX 
	cmp ballLeft, 1
	je Left
	jne Right
	
	Left:   
	sub bx, 2 
	jmp P2;  
	Right:   
	add bx, 2
	
	P2:
	mov ballX,  bx
	mov bx, ballY
	cmp ballUp, 1   
	je Up
	jne Down
	Up:
    sub bx, 2
	jmp P3
	Down:
    add bx, 2
	P3:
    mov ballY,  bx
   
    redrawBall 3
    
ret
baller endp   

repeat:
gameLoop:        
   CALL    checkKeyboard
   cmp begin,1
   jne repeat

   call Collisionwall
   call CollisionStriker 
   BrickCollision Brick1x, Brick1y
   BrickCollision Brick2x, Brick2y
   BrickCollision Brick3x, Brick3y
   BrickCollision Brick4x, Brick4y
   BrickCollision Brick5x, Brick5y
   BrickCollision Brick6x, Brick6y 
   BrickCollision Brick7x, Brick7y
   BrickCollision Brick8x, Brick8y
   BrickCollision Brick9x, Brick9y
   BrickCollision Brick10x, Brick10y
   BrickCollision Brick11x, Brick11y
   BrickCollision Brick12x, Brick12y
   
   CALL baller  
   CALL sleep
   JMP gameLoop 
    
exit:
	call terminate_game1
	call exit_program

checkKeyboard proc
    mov     ah,     1h
    int     16h         ; check keypress
    jz      noInput     ; no keypress
    mov     ah,     0h
    int     16h
    cmp     ax,     4D00h
    je      rightKey
    cmp     ax,     4B00h
    je      leftKey
    cmp     al,     27D
    je      exit
    cmp     ax,     3920h;space to begin
    je      beg
    jne     noInput
    
    beg:
    mov begin,1
    
    noInput:
    ret  

    rightKey:     
    mov bx, boundaryEnd
    cmp     strikerX, bx ;max right limit
    jg      noInput
    redrawStriker 0
    add     strikerX, 5
    redrawStriker 7
    cmp begin,0
    jz moveBallRight
    jmp     noInput
    
    
    leftKey:   
    mov bx, boundaryStart                            
    cmp     strikerX, bx ;max left limit
    jl      noInput
    redrawStriker 0
    sub     strikerX, 5
    redrawStriker 7
    cmp begin,0
    jz moveBallLeft
    jmp     noInput
    
    
    moveBallLeft:
    redrawBall 0
    sub     ballX, 5
    redrawBall 3
    jmp     noInput
    
    
    moveBallRight:
    redrawBall 0
    add     ballX, 5
    redrawBall 3
    jmp     noInput

checkKeyboard endp

draw proc
    push ax
    push cx
    push dx
     
    mov dx,starty
    mov cx,startx
    mov ah,0ch
    mov al,color
    c:
    inc cx
    int 10h
    cmp cx,endx
    jne c

    mov cx,startx
    inc dx
    cmp dx,endy
    jne c 
    
    pop dx
    pop cx
    pop ax
    ret
draw endp

drawStriker proc
    push bx
    push cx
        
    mov bx, strikerX
    mov cx, strikerY   
    mov startx,bx
    add bx, 40
    mov endx,bx
    mov starty,cx
    mov endy,175
    call draw
    
    pop cx
    pop bx
    ret
    drawStriker endp

drawBoundary proc
    mov color,6    
    ;------TOP------------
    mov startx,20
    mov endx,300
    mov starty,5
    mov endy,8
    call draw
    ;------RIGHT------------
    mov startx,297
    mov endx,300
    mov starty,7
    mov endy,180
    call draw
    ;------LEFT------------
    mov startx,20
    mov endx,23
    mov starty,7
    mov endy,180
    call draw
    ;------BOTTOM------------
    mov startx,20
    mov endx,300
    mov starty,177
    mov endy,180
    call draw 
   
    ret
    drawBoundary endp

beep proc
        push ax
        push bx
        push cx
        push dx
        mov     al, 182         ; Prepare the speaker for the
        out     43h, al         ;  note.
        mov     ax, 400        ; Frequency number (in decimal)
                                ;  for middle C.
        out     42h, al         ; Output low byte.
        mov     al, ah          ; Output high byte.
        out     42h, al 
        in      al, 61h         ; Turn on note (get value from
                                ;  port 61h).
        or      al, 00000011b   ; Set bits 1 and 0.
        out     61h, al         ; Send new value.
        mov     bx, 2          ; Pause for duration of note.
.pause1:
        mov     cx, 65535
.pause2:
        dec     cx
        jne     .pause2
        dec     bx
        jne     .pause1
        in      al, 61h         ; Turn off note (get value from
                                ;  port 61h).
        and     al, 11111100b   ; Reset bits 1 and 0.
        out     61h, al         ; Send new value.

        pop dx
        pop cx
        pop bx
        pop ax

ret
beep endp

;#################################################################################################################################	
;########################### TRUTH AND DARE PROCEDURE ###########################################################################
;#################################################################################################################################	

tod_game_menu proc near
	mov dh, 4 ; row position (top)
	mov dl, 10 ; column position (left)
	call set_cursor
			
	lea dx, prompt_menu_tod
	call display_screen
			
	;shows the start game
	mov dh, 8 ; row position (top)
	mov dl, 6 ; column position (left)
	call set_cursor
			
	lea dx, start_game_tod
	call display_screen
			
	;shows the mechanics 
	mov dh, 10 ; row position (top)
	mov dl, 6 ; column position (left)
	call set_cursor
			
	lea dx, mechanics_tod
	call display_screen
			
	;shows the exit to main menu
	mov dh, 12 ; row position (top)
	mov dl, 6 ; column position (left)
	call set_cursor
			
	lea dx, tod_exit_program
	call display_screen	
ret
tod_game_menu endp
	
;Displaying the starting of the game
coin_toss_function proc near
	call video_mode_black
	
	mov dh, 2 ; row position (top)
	mov dl, 5 ; column position (left)
	call set_cursor
		
	mov dx, offset tod_toss_coin 
	call display_screen
		
	mov dh, 4 ; row position (top)
	mov dl, 5 ; column position (left)
	call set_cursor

	mov dx, offset toss_coin_result
	call display_screen
		
	mov dh, 6 ; row position (top)
	mov dl, 12 ; column position (left)
	call set_cursor
		
	mov ah, 00h   ; get system timer
	int 1Ah
	mov ax, dx    ; use DX as RNG seed

	; Generate a random number between 0 and 1
	and ax, 0001h ; ensure only the least significant bit is considered

	; Check the least significant bit for either Heads or Tails
	cmp ax, 0     ; compare with 0
	je print_heads; if equal, print Heads
	jmp print_tails;

	print_heads:
	;Code to print "Heads" message
		lea dx, result_one
		call display_screen
			
		mov dh, 8 ; row position (top)
		mov dl, 6 ; column position (left)
		call set_cursor
			
		lea dx, result_heads ; Load the message for truth question
		call display_screen
		
		mov ax, num_truth
		call random_truth
			
		push ax ; Save the value of ax
        push bx ; Save the value of bx
		
		mov dh, 12 ; row position (top)
		mov dl, 8 ; column position (left)
		call set_cursor			
		
			
		mov si, offset truth_questions ; Load the base address of the array
		mov ax, bx ; Copy the random number to ax
		shl ax, 1 ; Multiply by 2
		add si, ax ; Add to the base address
		mov si, [si] ; Load the offset of the question
		mov dx, si ; Load the offset of the question
		
		call display_screen

        pop bx ; Restore the value of bx
        pop ax ; Restore the value of ax

		jmp end_coin_toss  ; Jump to the end of the coin toss handling

		print_tails:
			;Code to print "Tails" message
			lea dx, result_two
			call display_screen
			
			mov dh, 8 ; row position (top)
			mov dl, 6 ; column position (left)
			call set_cursor
			
			lea dx, result_tails ; Load the message for dare challenge
			call display_screen

			mov ax, num_dare	
			call random_dare
			
		push ax ; Save the value of ax
        push bx ; Save the value of bx
		
		mov dh, 12 ; row position (top)
		mov dl, 8 ; column position (left)
		call set_cursor			
			
		mov si, offset dare_challenges ; Load the base address of the array
		mov ax, bx ; Copy the random number to ax
		shl ax, 1 ; Multiply by 2
		add si, ax ; Add to the base address
		mov si, [si] ; Load the offset of the question
		mov dx, si ; Load the offset of the question
		
		call display_screen

        pop bx ; Restore the value of bx
        pop ax ; Restore the value of ax


		end_coin_toss:
ret
coin_toss_function endp

; Random truth question using system timer
random_truth proc near
   mov ax, 00h
    int 1ah ;get number of clock ticks since midnight in dx
    mov ax, dx
    xor dx, dx
    mov cx, num_truth ; Use the correct range
    div cx ;divide dx:ax by cx, get quotient in ax and remainder in dx
    mov bx, dx ;move the remainder to bl
    ret
random_truth endp

; random dare challenge using system timer
random_dare proc near
   mov ax, 00h
    int 1ah ;get number of clock ticks since midnight in dx
    mov ax, dx
    xor dx, dx
    mov cx, num_dare ; Use the correct range
    div cx ;divide dx:ax by cx, get quotient in ax and remainder in dx
    mov bx, dx ;move the remainder to bl
    ret
ret
random_dare endp


;#################################################################################################################################	
;########################### INTERRUPTS PROC ###########################################################################
;#################################################################################################################################	
;
;Wait for users to press the key
	wait_for_keys proc near
		mov ah, 00h
		int 16h
		mov choice, al
	ret
	wait_for_keys endp

;Set cursor PROCEDURE
	set_cursor proc near
		mov ah, 02h ; set function to set cursor position
		mov bh, 0 ; page number
		int 10h ; call BIOS interrupt to set cursor position
	ret
	set_cursor endp


;Display to the screen
	display_screen proc near
		mov ah, 09h
		int 21h
	ret
	display_screen endp
;Set video mode and black background
	video_mode_black proc near
		call video_mode
		call black_background
	ret
	video_mode_black endp

;Set Video mode into 5h (grayscale)
	video_mode proc near
		mov ah, 00h
		mov al, 05h
		int 10h
	ret
	video_mode endp
	
;Set blackground color to black
	black_background proc near			
		mov ah, 0Bh ; Set the configuration
		mov bh, 00h ; To the background color
		mov bl, 00h ; Black background color
		int 10h     ; Video Services - Set Video Mode
	ret
	black_background endp
;
terminate_game1 proc near
	call clear_screen

;VideoMOde to grayscale
	call setVideoMode
	
	mov ah, 0Bh ; Set the configuration
	mov bh, 00h ; To the background color
	mov bl, 00h ; Black background color
	int 10h     ; Video Services - Set Video Mode
	
	mov AL, 1		 ; the following lines set up for the string print
	mov BH,0
	mov BL,6
	mov CX, nSize

	MOV DX, @data
	MOV ES, DX
	
	mov dh, 5 ; row position (top)
	mov dl, 15 ; column position (left)
	mov ah, 02h ; set function to set cursor position
	mov bh, 0 ; page number
	int 10h ; call BIOS interrupt to set cursor position

	MOV BP, OFFSET msg

	mov AH, 13h              ; Print the "Chonker Terminated" message.
	int 10h
	
		exit_brick_breaker:
		mov dh, 18 ; row position (top)
		mov dl, 2 ; column position (left)
		mov ah, 02h ; set function to set cursor position
		mov bh, 0 ; page number
		int 10h ; call BIOS interrupt to set cursor position
					
		mov dx, offset terminate_brickbreaker
		mov ah, 09h
		int 21h
		
		;read a character from the keyboard	
		mov ah, 00h
		int 16h
		mov choice, al
					
		cmp choice, 'X'
		je brick_breaker_exit
		cmp choice, 'x'
		je brick_breaker_exit
	
	brick_breaker_exit:
ret
terminate_game1 endp 
;VIDEO MODE PROC
setVideoMode proc
    
    mov ah, 0   ; set display mode function.
    mov al, 13h ; mode 13h = 320x200 pixels, 256 colors.
    int 10h     
    
    ret
    setVideoMode endp
;CLEAR SCREEN PROCEDURE
	clear_screen proc near
		mov ah, 06h         ; clear screen
		mov al, 0
		mov bh, 07h
		mov cx, 0
		mov dx, 184fh
		int 10h
		ret
	clear_screen endp

;TERMINATE THE PROGRAM PROCEDURE
	exit_process proc near
		mov ah, 00h
		mov al, 02h
		int 10h
		
		mov ah,4Ch
		int 21h
	exit_process endp
	
    main endp
    end main
