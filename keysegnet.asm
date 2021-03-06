$MOD186
$EP
NAME TIMER
; Main program for uPD70208 microcomputer system
;
; Author: 	Dr Tay Teng Tiow
; Address:     	Department of Electrical Engineering 
;         	National University of Singapore
;		10, Kent Ridge Crescent
;		Singapore 0511.	
; Date:   	6th September 1991
;
; This file contains proprietory information and cannot be copied 
; or distributed without prior permission from the author.
; =========================================================================
; 1 KEYBOARD VERSION (TESTING ONLY)
public	serial_rec_action, timer2_action
extrn	print_char:far, print_2hex:far, iodefine:far
extrn   set_timer2:far
PORTA 	        equ 080h
PORTB           EQU 081H
PORTC           EQU 082H
CWR             EQU 083H
LED_SELECT	EQU	0100H
LED_OUTPUT	EQU	0180H 
PCSBA           EQU    0FFA4H ; Peripheral Chip Select Base Address
MPCS            EQU    0FFA8H ; MMCS and PCS Alter Control Register
KBD_BUFFER_LEN	EQU		9
KBD_BUFFER_LEN2	EQU		9

NET_READY EQU 0
NET_PREADDR_BUFFERING EQU 1
NET_ADDR_RECIEVING EQU 2
NET_SELECTED EQU 3
NET_BUFFERING EQU 4
ADDR_LEN EQU 2
NACK    EQU     'N'
NET_PREADDR_CHAR EQU '`'
NET_PREADDR_LEN  EQU 3
NET_PAYLOAD_LEN  EQU 15
PRICE_LEN EQU 6

STACK_SEG	SEGMENT
		DB	256 DUP(?)
	TOS	LABEL	WORD
STACK_SEG	ENDS


DATA_SEG	SEGMENT
	
	TIMER0_MESS	DB	10,13,'TIMER2 INTERRUPT    '
	T_COUNT		DB	2FH
	T_COUNT_SET	DB	2FH
	REC_MESS	DB	10,13,'Period of timer0 = '  
	array       DB      '1','2','3','4','5','6','7','8','9','*','0','#'
        ARRAY2      DB      'A','B','C','D','E','F','G','H','I','J','K','L' 
	BCD	    DB 	3FH,06H,5BH,4FH,66H,6DH,7DH,07H,7FH,06FH
	LED_BUFFER	DB	3fh,06h,5bh,4fh,66h,6dh
	CUR_LED		DB	0H
	DISPLAY_NUM	DW	1234H
	KBD_BUFFER	DB	10 DUP(?)
        KBD_BUFFER_SEEK DB      0H
		
	KBD_ROW_COUNTER	DB	0H
	;USED TO OUTPUT TO KBD
	KBD_OUTPUT	DB	0H
	KBD_INPUT	DB	0H
	
	LED_CURSOR	DB	0H
        KBD_ROW_COUNTER2 DB     0H
        KBD_OUTPUT2      DB     0H
        KBD_BUFFER2	DB	20 DUP(?)
        KBD_BUFFER_SEEK2  DB      0H
        KBD_INPUT2	DB	0H
    
	
;=========================================================================
;Port B of the 8255 is used as the input port for the keybad
;Port C is used as the output and grounds the rows one by one
;=========================================================================
	BARCODE	    DB	'!18243337:0012#'
	RECV_MESS	DB	10,13,'PRICE RECV'
	CUR_PRICE	DB	8 DUP(?)
	CUR_INDEX	DB	0
	NET_STATE	DB	0
	ADDRESS		DW	'ab'
        ADDR_INCOM_COUNT      DB      0
        ADDR_COUNT      DB      0
        ADDR_BUFFER     DB      4 DUP(?)
	
	;; network state machine:
        ;; READY
        ;;  |
        ;; ` -> NET_PREADDR_BUFFERING
        ;; ``` -> NET_ADDR_RECIEVING (A)
        ;; 0023H
        ;;      |
        ;;     /  \
        ;; myaddr?  return READY + NACK
        ;;   |
        ;;  NET_SELECTED (transmit BARCODE+ Quantity)
        ;;   |
        ;;  NET_BUFFERING -> '1'*PRICE_LEN -> CUR_PRICE UPDATED ->ACK CUR_PRICE
        ;; return READY

DATA_SEG	ENDS


CODE_SEG	SEGMENT

	PUBLIC		START

ASSUME	CS:CODE_SEG, SS:STACK_SEG, DS:DATA_SEG

START:
 ; Initialize MPCS to MAP peripheral to IO address
         MOV DX, MPCS
         MOV AX, 0083H
         OUT DX, AL
; PCSBA initial, set the serial port start from 00H
         MOV DX, PCSBA
         MOV AX, 0003H ; Peripheral starting address 00H no READY, No Waits
         OUT DX, AL

;initialize stack area
		MOV	AX,STACK_SEG		
		MOV	SS,AX
		MOV	SP,TOS
;Initialise data segment
		MOV AX,DATA_SEG
		MOV DS,AX

; Initialize the on-chip pheripherals
		CALL FAR PTR IODEFINE
		


; ^^^^^^^^^^^^^^^^^  Start of User Main Routine  ^^^^^^^^^^^^^^^^^^
   
MOV AL, 82H       ;mode 0, A - out, B-in ;changed value of CW from 82H
		MOV DX, CWR
		OUT DX, AL	;send the control word
		MOV BL, 00H	;initialize BL for key code
		XOR AX, AX	;clear ax flags
		MOV DX, PORTC  ;port C address to DX
		OUT DX, AL	;ground all rows
		


   
;code
 call set_timer2
                STI

NEXT:
       ; MOV AX,DS:DISPLAY_NUM
	
	;INC DS:CUR_LED
	;CALL FAR PTR KEYBOARD2
	CALL FAR PTR KEYBOARD
	
	;CALL FAR PTR DISPLAY_LED
 JMP NEXT

; ^^^^^^^^^^^^^^^ End of User main routine ^^^^^^^^^^^^^^^^^^^^^^^^^


SERIAL_REC_ACTION	PROC	FAR
		PUSH	CX
		PUSH 	BX
		PUSH	DS

		MOV	BX,DATA_SEG		;initialize data segment register
		MOV	DS,BX
		MOV BL, DS:NET_STATE
                CMP BL, NET_PREADDR_BUFFERING
                JZ ADDR_BUFFERING
                CMP BL, NET_ADDR_RECIEVING
                JZ ADDR_RECIEVING
		CMP BL, NET_BUFFERING
		JZ CALL_BUFFERING
		CMP BL, NET_SELECTED
		JZ SELECTED
        ;; 
                CMP AL, NET_PREADDR_CHAR
                JZ ADDR_BUFFERING

        
		JMP	S_RET
        
        ;; 	JNE	WRONG_ADDRESS
;		IF POLL IS SELECTING DS:ADDRESS, TRANSMIT THE BARCODE
CALL_BUFFERING:
                CALL FAR PTR DATA_BUFFERING
                JMP S_RET
SELECTED:
        ;; starts sending the barcode and quantity

		MOV CX, NET_PAYLOAD_LEN
		MOV BX, 0
START_TRANSMIT:		
		MOV AL,DS:BARCODE[BX]
		CALL FAR PTR PRINT_CHAR
		INC BX
		LOOP	START_TRANSMIT
        ;; change state to recieving
		MOV DS:NET_STATE, NET_BUFFERING
		JMP	S_RET

WRONG_ADDRESS:		
		CALL FAR PTR PRINT_CHAR
		JMP S_RET
 
ADDR_BUFFERING:
        ;; buffers the incoming addr char '`' as by protocol
                INC DS:ADDR_INCOM_COUNT
                MOV AL, DS:ADDR_INCOM_COUNT
                CMP AL,NET_PREADDR_LEN
                JNE SKIP_UPDATE
                MOV DS:NET_STATE, NET_ADDR_RECIEVING
                MOV AL, 0
                MOV DS:ADDR_INCOM_COUNT, AL
SKIP_UPDATE:    
                JMP S_RET
        
ADDR_RECIEVING:
        ;; recives the address being broadcasted. address is only 2 bytes
                MOV BL, DS:ADDR_COUNT
                XOR BH, BH
                MOV DS:ADDR_BUFFER[BX], AL
                INC DS:ADDR_COUNT
                MOV AL, DS:ADDR_COUNT
                CMP AL,ADDR_LEN
                JGE ADDR_DONE
                JMP S_RET
ADDR_DONE:
                MOV AH,DS:ADDR_BUFFER
                MOV AL,DS:ADDR_BUFFER[1]
                CMP AX,DS:ADDRESS
                JE ISMYADDR
        ;; FOR DEV PURPOSES ONLY
                MOV AL, NACK
                CALL FAR PTR PRINT_CHAR
        ;; RESET STATE TO READY SINCE NOT MY ADDR
                MOV AL, NET_READY
                MOV DS:NET_STATE, AL
                XOR AL,AL
                MOV DS:ADDR_COUNT, AL
                JMP S_RET
ISMYADDR:
        ;; SET STATE TO SELECTED
                MOV AL,NET_SELECTED
                MOV DS:NET_STATE, AL
                MOV AL, 0
        ;; RESET ADDR_COUNT
                MOV DS:ADDR_COUNT, AL
                JMP SELECTED

S_RET:
		POP	DS
		POP	BX
		POP	CX
		RET
SERIAL_REC_ACTION	ENDP


DATA_BUFFERING  PROC    FAR
        
                PUSH AX
                PUSH BX
        
       		MOV BL, DS:CUR_INDEX
		XOR BH,BH
		MOV DS:CUR_PRICE[BX], AL
		
		INC BL
		CMP BL, PRICE_LEN
		JGE BUFFER_END
		MOV DS:CUR_INDEX, BL
		JMP ESCAPE_PROC 
BUFFER_END:
		MOV CX,PRICE_LEN
		MOV BX, 0
		MOV AL, DS:CUR_PRICE[BX]
ACK_LOOP:
		MOV AL, DS:CUR_PRICE[BX]
		CALL FAR PTR PRINT_CHAR
		INC BX
		LOOP	ACK_LOOP
		MOV DS:NET_STATE, NET_READY
		XOR AL,AL
		MOV DS:CUR_INDEX, AL
ESCAPE_PROC:    
                POP BX
                POP AX
                RET
        
DATA_BUFFERING ENDP

KEYBOARD PROC FAR

		PUSH    DX
		PUSH	CX
		PUSH 	BX
		PUSH	AX
		

INIT:		
		
		call far ptr keyboard2
		MOV CL, 07FH	;STORES OUTPUT FOR ROW COUNTER;
		;MOV AL,0FEH ; 1111 1110
		MOV CH, 0H	;set row counter
		MOV DS:KBD_ROW_COUNTER,CH
		MOV DS:KBD_OUTPUT, CL
		
		
NEXT_ROW:
		MOV AL,DS:KBD_ROW_COUNTER
	
		CMP AL,04
		JGE INIT
		MOV CL,DS:KBD_OUTPUT 
		ROL CL, 01H       ;rotate AL to ground next row/ al HAS 8 BITS. so must JMP BACK TO WAIT

		;MOV CH, AL	;save data byte to ground next row ;WAT?
		MOV AL,CL
		MOV DX, PORTC	;port C address to DX; 
		OUT DX, AL	;give positive logic to one of the rows
		MOV DS:KBD_OUTPUT, AL	
        
		MOV DX, PORTB	;port B address to DX  
		IN  AL,DX	;read input port for key closure
		
		
		;mov al,101b; change later
		AND AL, 07H	;Mask D4-D7
		CMP AL,07H
	   	JE RETPOINT
		;ERROR CHECK
		CMP AL,0110B
		JE VALIDATED
		CMP AL,0101B
		JE VALIDATED
		CMP AL,011B
		JE VALIDATED
		JMP RETPOINT
		
VALIDATED:		
		CALL FAR PTR KBD_PROCESS
		;Bl CONTAINS iNDEX OF ARRAY WITH CURRENT KEY
        ;; | SEEK==0 = ADD_TO_BUFFER
        ;; | BUFFER[SEEK]== BL = ADD_TO_BUFFER
        ;; | ELSE = (ADD_TO_BUFFER) & (SEEK 0) AKA RESET
		;JMP NUMBERS

NUMBERS:
		XOR BH, BH
		MOV AL,DS:array[BX] ; Stores character in AL (?)
		XOR AH,AH
		;CALL	FAR PTR PRINT_CHAR
		
        MOV CL, DS:KBD_BUFFER_SEEK
        CMP CL,0H
        JZ ADD_TO_BUFFER
				
		XOR CH,CH
        MOV SI, CX
        MOV CL, KBD_BUFFER
		;COMPARE WITH VALUE AT SEEK
        CMP CL,AL
		JZ  ADD_TO_BUFFER
		
		XOR CL,CL
		MOV DS:KBD_BUFFER_SEEK, CL
                 		

ADD_TO_BUFFER:
        ;;PUT AL INTO SI SO WE CAN PUT CURRENT CHAR IN THE BUFFER 
                XOR CH,CH
                MOV SI,CX
                MOV DS:KBD_BUFFER[SI],AL
                INC DS:KBD_BUFFER_SEEK

		mov cx, 5000
Debounce:
		nop
		loop debounce
CHECKER_SEEK:
		MOV CL, DS:KBD_BUFFER_SEEK
		CMP CL,KBD_BUFFER_LEN
		;ADD CL,48
		;MOV AL,CL
		;CALL FAR PTR PRINT_CHAR
		
		JNE RETPOINT
		;RESET KBD_BUFFER_SEEK
		;AL CONTAINS CHAR
		MOV DS:KBD_INPUT, AL
		CALL	FAR PTR PRINT_CHAR
		CALL FAR PTR ADD_LED_BUFFER
		XOR CL,CL
		;BUFFER_SEEK IS RESET
		MOV DS:KBD_BUFFER_SEEK, CL
RETPOINT:	
	INC DS:KBD_ROW_COUNTER
	
	JMP NEXT_ROW
		
	
	POP AX
	POP BX
	POP CX
	POP DX
	RET
KEYBOARD ENDP

KBD_PROCESS		PROC	FAR
		;0000 0101 => 1111 1010 => 0000 0010  => 0 , 1, 10   
		NOT AL
		AND AL, 07H; MASK OTHER BLOODY BITS OMGOMGOMG
		SHR AL,01 ; DIVIDE AL BY 2. TO GET COL NUMBER
		MOV DL,AL ; TEMP STORE AL IE PORTB INPUT AKA COL NUMBER
		
		mov Al,DS:KBD_ROW_COUNTER ; ROW COUNT MOVED TO AL
		MOV DH,03 ; TO MULTIPLY BY 3
		;MULTIPLY THE FUCKER
		MUL DH
		; RESULT IN AX
		
		
		; NUMBER  <= 9 
		ADD AL, DL ;ADD row*3 NO TO COL NUMBER
		MOV BL,AL
		XOR BH,BH
		;Bl CONTAINS iNDEX OF ARRAY WITH CURRENT KEY
        ;; | SEEK==0 = ADD_TO_BUFFER
        ;; | BUFFER[SEEK]== BL = ADD_TO_BUFFER
        ;; | ELSE = (ADD_TO_BUFFER) & (SEEK 0) AKA RESET

	
RET
KBD_PROCESS		ENDP

KEYBOARD2 PROC FAR

		PUSH    DX
		PUSH	CX
		PUSH 	BX
		PUSH	AX
		

INIT2:		
		
		
		MOV CL, 0F7H	;STORES OUTPUT FOR ROW COUNTER;
		;MOV AL,0FEH ; 1110 1111
		MOV CH, 0H	;set row counter
		MOV DS:KBD_ROW_COUNTER2,CH
		MOV DS:KBD_OUTPUT2, CL
	
		;call far ptr print_char
		jmp next_row2
hacky_shit:
		jmp exit_kbd2
		
NEXT_ROW2:
		MOV AL,DS:KBD_ROW_COUNTER2
	

		CMP AL,04
		JGE hacky_shit
		MOV CL,DS:KBD_OUTPUT2 
		ROL CL, 01H       ;rotate AL to ground next row/ al HAS 8 BITS. so must JMP BACK TO WAIT

		;MOV CH, AL	;save data byte to ground next row ;WAT?
		MOV AL,CL
		MOV DX, PORTC	;port C address to DX; 
		OUT DX, AL	;give positive logic to one of the rows
		MOV DS:KBD_OUTPUT2, AL	
        ;add al,48
		;call far ptr print_char
		MOV DX, PORTB	;port B address to DX  
		IN  AL,DX	;read input port for key closure
		
		
		;mov al,101b; change later
		;call far ptr print_char
		AND AL, 38H	;Mask D4-D7  00xx x000
                SHR AL,03
		CMP AL,07H
	   	JE RETPOINT2
		;ERROR CHECK
		CMP AL,0110B
		JE VALIDATED2
		CMP AL,0101B
		JE VALIDATED2
		CMP AL,011B
		JE VALIDATED2
		JMP RETPOINT2
		
VALIDATED2:		
		CALL FAR PTR KBD_PROCESS2
		;Bl CONTAINS iNDEX OF ARRAY WITH CURRENT KEY
        ;; | SEEK==0 = ADD_TO_BUFFER
        ;; | BUFFER[SEEK]== BL = ADD_TO_BUFFER
        ;; | ELSE = (ADD_TO_BUFFER) & (SEEK 0) AKA RESET
		;JMP NUMBERS

NUMBERS2:
		XOR BH, BH
		MOV AL,DS:array2[BX] ; Stores character in AL (?)
		XOR AH,AH
		;CALL	FAR PTR PRINT_CHAR
		
        MOV CL, DS:KBD_BUFFER_SEEK2
        CMP CL,0H
        JZ ADD_TO_BUFFER2
				
		XOR CH,CH
        MOV SI, CX
        MOV CL, KBD_BUFFER2
		;COMPARE WITH VALUE AT SEEK
        CMP CL,AL
		JZ  ADD_TO_BUFFER2
		
		XOR CL,CL
		MOV DS:KBD_BUFFER_SEEK2, CL
                 		

ADD_TO_BUFFER2:
        ;;PUT AL INTO SI SO WE CAN PUT CURRENT CHAR IN THE BUFFER 
                XOR CH,CH
                MOV SI,CX
                MOV DS:KBD_BUFFER2[SI],AL
                INC DS:KBD_BUFFER_SEEK2

		mov cx, 5000
Debounce2:
		nop
		loop debounce2
CHECKER_SEEK2:
		MOV CL, DS:KBD_BUFFER_SEEK2
		CMP CL,KBD_BUFFER_LEN2
		;ADD CL,48
		;MOV AL,CL
		;CALL FAR PTR PRINT_CHAR
		
		JNE RETPOINT2
		;RESET KBD_BUFFER_SEEK
		;AL CONTAINS CHAR
		MOV DS:KBD_INPUT2, AL
		CALL	FAR PTR PRINT_CHAR
        ;; 	CALL FAR PTR ADD_LED_BUFFER
		XOR CL,CL
		;BUFFER_SEEK IS RESET
		MOV DS:KBD_BUFFER_SEEK2, CL
RETPOINT2:	
	INC DS:KBD_ROW_COUNTER2
	
	JMP NEXT_ROW2
		
exit_kbd2:	
	POP AX
	POP BX
	POP CX
	POP DX
	RET
KEYBOARD2 ENDP

KBD_PROCESS2		PROC	FAR
		;0000 0101 => 1111 1010 => 0000 0010  => 0 , 1, 10   
		NOT AL
		AND AL, 07H; MASK OTHER BLOODY BITS OMGOMGOMG
		SHR AL,01 ; DIVIDE AL BY 2. TO GET COL NUMBER
		MOV DL,AL ; TEMP STORE AL IE PORTB INPUT AKA COL NUMBER
		
		mov Al,DS:KBD_ROW_COUNTER2 ; ROW COUNT MOVED TO AL
		MOV DH,03 ; TO MULTIPLY BY 3
		;MULTIPLY THE FUCKER
		MUL DH
		; RESULT IN AX
		
		
		; NUMBER  <= 9 
		ADD AL, DL ;ADD row*3 NO TO COL NUMBER
		MOV BL,AL
		XOR BH,BH
		;Bl CONTAINS iNDEX OF ARRAY WITH CURRENT KEY
        ;; | SEEK==0 = ADD_TO_BUFFER
        ;; | BUFFER[SEEK]== BL = ADD_TO_BUFFER
        ;; | ELSE = (ADD_TO_BUFFER) & (SEEK 0) AKA RESET

	
RET
KBD_PROCESS2		ENDP
        
TIMER2_ACTION	PROC	FAR
		PUSH	AX
		PUSH	DS
		PUSH	BX
		PUSH	CX

		
;CALL FAR PTR DISPLAY_BCD
;CALL FAR PTR KEYBOARD	
CALL FAR PTR DISPLAY_LED
;CALL FAR PTR ADD_LED_BUFFER
DEC	DS:T_COUNT
		JNZ	T_NEXT1
		MOV	AL,DS:T_COUNT_SET
		MOV	DS:T_COUNT,AL

		MOV	CX,20
		MOV	BX,0H
T_NEXT0:
		MOV	AL,DS:TIMER0_MESS[BX]
        
		INC	BX
        ;; 		CALL 	FAR PTR PRINT_CHAR
		LOOP	T_NEXT0
;	kanmy@comp.nus.edu.sg
T_NEXT1:


		POP	CX
		POP	BX
		POP	DS
		POP 	AX
		RET
TIMER2_ACTION	ENDP

DISPLAY_LED	PROC	FAR
	PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX
	; CONVERTS CUR_LED TO SIGNAL
	MOV BL, DS:CUR_LED
	MOV AL,0FEH
	MOV CL, BL
	ROL AL,CL
	
	MOV DX, LED_SELECT
	OUT DX, AL
	
	XOR BH,BH
	MOV AL, DS:LED_BUFFER[BX]
	MOV DX, LED_OUTPUT
	OUT DX,AL

	MOV CX, 400
SUSTAIN:	
	
	NOP
	LOOP SUSTAIN
	
; CLEAR THE FUCKER
	MOV AL, 0H
	MOV DX, LED_OUTPUT
	OUT DX, AL
	
	MOV AL, DS:CUR_LED
	CMP AL, 5
	JGE RESTORE
	INC DS:CUR_LED
	JMP EXIT
RESTORE:
	XOR AL,AL
	MOV DS:CUR_LED, AL
	
EXIT:
	POP DX
	POP CX
	POP BX
	POP AX
	RET

DISPLAY_LED ENDP

ADD_LED_BUFFER 	PROC	FAR
	PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX
	MOV AL, DS:LED_CURSOR
	CMP AL, 06
	JL INCREMENT
	XOR AL, AL
	MOV DS:LED_CURSOR,AL
	JMP DONE_INC
INCREMENT:
	;MOV AL, DS:KBD_INPUT
	;CMP AL, '*'
	;JNE DONE_INC
	INC DS:LED_CURSOR
	;JMP ENDING
DONE_INC:	
	MOV BL, DS:KBD_INPUT
	;CONVERT ASCII TO NUMBER
	
	SUB BL, 48
	XOR BH, BH
	MOV AL, DS:BCD[BX]
	MOV BL, DS:LED_CURSOR
	XOR BH,BH
	MOV DS:LED_BUFFER[BX],AL
ENDING:	
	POP DX
	POP CX
	POP BX
	POP AX
	RET
ADD_LED_BUFFER	ENDP
CONVERT_TO_BCD	PROC	FAR


	PUSH BX
	MOV BL,AL
	CMP BL,10
	JGE GREATER_THAN_10
	JMP RETURN_BCD
;; IF BL(AL) > = 10 WE ADD 6. AH -> 10H 
GREATER_THAN_10:
	ADD AL,06
	
RETURN_BCD:
	
	POP BX
	RET
CONVERT_TO_BCD	ENDP
;; 
;EXPECTS A 4BIT BCD DIGIT IN AL. RETURNS THE 7SEG CODE IN AL FOR THAT DIGIT	
BCD_TO_7SEG	PROC	FAR
	PUSH BX

	;; STORE AL TEMPORARILY IN BL
	MOV BL,AL
	
	XOR BH,BH
	;; GET THE BLTH INDEX OF THE BCD ARRAY IN DATA_SEG
	MOV AL,DS:BCD[BX]
	POP BX
	RET
BCD_TO_7SEG	ENDP	


CODE_SEG	ENDS
END
