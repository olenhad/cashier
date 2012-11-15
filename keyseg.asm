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
	BCD	    DB 	3FH,06H,5BH,4FH,66H,6DH,7DH,07H,7FH,06FH
	LED_BUFFER	DB	3fh,06h,5bh,4fh,66h,6dh
	CUR_LED		DB	0H
	DISPLAY_NUM	DW	1234H
	KBD_BUFFER	DB	
;=========================================================================
;Port B of the 8255 is used as the input port for the keybad
;Port C is used as the output and grounds the rows one by one
;=========================================================================

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

		CMP	AL,'<'
		JNE	S_FAST

		INC	DS:T_COUNT_SET
		INC	DS:T_COUNT_SET

		JMP	S_NEXT0
S_FAST:
		CMP	AL,'>'
		JNE	S_RET

		DEC	DS:T_COUNT_SET
		DEC	DS:T_COUNT_SET

S_NEXT0:
		MOV	CX,22			;initialize counter for message
		MOV	BX,0

S_NEXT1:	MOV	AL,DS:REC_MESS[BX]	;print message
		call	FAR ptr print_char
		INC	BX
		LOOP	S_NEXT1

		MOV	AL,DS:T_COUNT_SET	;print current period of timer0
		CALL	FAR PTR PRINT_2HEX
S_RET:
		POP	DS
		POP	BX
		POP	CX
		RET
SERIAL_REC_ACTION	ENDP

KEYBOARD PROC FAR

		PUSH    DX
		PUSH	CX
		PUSH 	BX
		PUSH	AX
		

INIT:		
		
		
		MOV CL, 07FH	;STORES OUTPUT FOR ROW COUNTER;
		;MOV AL,0FEH ; 1111 1110
		MOV CH, 0H	;set row counter
		

		
NXTROW:		
		ROL CL, 01H       ;rotate AL to ground next row/ al HAS 8 BITS. so must JMP BACK TO WAIT

		;MOV CH, AL	;save data byte to ground next row ;WAT?
		MOV AL,CL
		MOV DX, PORTC	;port C address to DX; 
		OUT DX, AL	;give positive logic to one of the rows
                
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
		NOT AL
		AND AL, 07H; MASK OTHER BLOODY BITS OMGOMGOMG
		SHR AL,01 ; DIVIDE AL BY 2. TO GET COL NUMBER
		MOV DL,AL ; TEMP STORE AL IE PORTB INPUT AKA COL NUMBER
		
		mov Al,CH ; ROW COUNT MOVED TO AL
		MOV DH,03 ; TO MULTIPLY BY 3
		;MULTIPLY THE FUCKER
		MUL DH
		; RESULT IN AX
		
		
		; NUMBER  <= 9 
		ADD AL, DL ;ADD row*3 NO TO COL NUMBER
		MOV BL,AL
		XOR BH,BH
		;Bl CONTAINS iNDEX OF ARRAY WITH CURRENT KEY
		
		mov cx, 20000
Debounce:
		nop
		loop debounce
	
NUMBERS:
		
		MOV AL,DS:array[BX] ; Stores character in AL (?)
		XOR AH,AH
		CALL	FAR PTR PRINT_CHAR
		
              		
RETPOINT:	
	INC CH
	CMP CH,04
	JNZ NXTROW
	JMP INIT
		
	
	POP AX
	POP BX
	POP CX
	POP DX
KEYBOARD ENDP

TIMER2_ACTION	PROC	FAR
		PUSH	AX
		PUSH	DS
		PUSH	BX
		PUSH	CX

		
;CALL FAR PTR DISPLAY_BCD
;CALL FAR PTR KEYBOARD	
CALL FAR PTR DISPLAY_LED
DEC	DS:T_COUNT
		JNZ	T_NEXT1
		MOV	AL,DS:T_COUNT_SET
		MOV	DS:T_COUNT,AL

		MOV	CX,20
		MOV	BX,0H
T_NEXT0:
		MOV	AL,DS:TIMER0_MESS[BX]
		INC	BX
		CALL 	FAR PTR PRINT_CHAR
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
;; EXPECTS 16 BIT BCD ENCODED NUMBER IN AX.  
DISPLAY_BCD	PROC	FAR
	PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX
	;; NUMBER STORED IN CX
	MOV CX, AX
	MOV BH, 0FEH;1111 1110
	MOV BL, 04H
LOOP_START:
	;; SET LCD_SELECT
	MOV AL, BH
	MOV DX, LED_SELECT
	OUT DX, AL
	
	ROL BH,01 ;1111 1101 -> 1111 1011 -> 1111 0111
	
	MOV AX, CX
	
	AND AL, 0FH
	;5557 : 0101 0101 0101 0111 ->000 0111
	CALL FAR PTR BCD_TO_7SEG
	
	;RESULT IN AL
	MOV DX, LED_OUTPUT
	OUT DX,AL
	MOV AL,0H
	OUT DX,AL
	
	DEC BL
	SHR CX,04H
	;0000 0101 0101 0101
	; 0000 0000 0101 0101
	;BL IS NOW 2
	;0000 0000 0000 0101
	
	CMP BL, 0
	JNZ LOOP_START

	
	POP DX
	POP CX
	POP BX
	POP AX
	RET
DISPLAY_BCD	ENDP
;; EXPECTS A 16 BIT NUMBER IN AX. DISPLAYS IT ON THE 7 SEGMENT 
;; NOTE: 16 BITS MEANS 4 BCD DIGITS. THIS THIS ROUTINE ONLY USES 4 LEDS. 

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
