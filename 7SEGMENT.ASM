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

public	serial_rec_action, timer2_action
extrn	print_char:far, print_2hex:far, iodefine:far
extrn   set_timer2:far
LED_SELECT	EQU	0100H
LED_OUTPUT	EQU	0180H 
STACK_SEG	SEGMENT
		DB	256 DUP(?)
	TOS	LABEL	WORD
STACK_SEG	ENDS

DATA_SEG	SEGMENT
	TIMER0_MESS	DB	10,13,'TIMER2 INTERRUPT    '
	T_COUNT		DB	2FH
	T_COUNT_SET	DB	2FH
	REC_MESS	DB	10,13,'Period of timer0 =     '
	BCD		DB	7EH 30H,6DH,79H,33H,5BH,1FH,70H,7FH,73H

DATA_SEG	ENDS


CODE_SEG	SEGMENT

	PUBLIC		START

ASSUME	CS:CODE_SEG, SS:STACK_SEG

START:
;initialize stack area
		MOV	AX,STACK_SEG		
		MOV	SS,AX
		MOV	SP,TOS

; Initialize the on-chip pheripherals
		CALL	FAR PTR	IODEFINE
		


; ^^^^^^^^^^^^^^^^^  Start of User Main Routine  ^^^^^^^^^^^^^^^^^^
    call set_timer2
                 STI

INIT:
	
	

NEXT:
	
	JMP NEXT

; ^^^^^^^^^^^^^^^ End of User main routine ^^^^^^^^^^^^^^^^^^^^^^^^^

;; EXPECTS 16 BIT BCD ENCODED NUMBER IN AX.  
DISPLAY_BCD	PROC	FAR
	PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX
	;; NUMBER STORED IN BX
	MOV BX, AX
	;; SET LOOP COUNTER
	MOV CL,04H
	;; SET LCD_SELECT
	MOV CH,01H
LOOP_START:
	MOV DX, LED_SELECT
	MOV AX,CH
	OUT DX,AX

	MOV AX, BX
	;; MASK EVERYTHING BUT THE LAST BIT
	AND AX,01H
	;; GET 7SEG CODE
	CALL FAR PTR BCD_TO_7SEG
	;; RESULT IS IN AL
	MOV DX, LED_OUTPUT
	OUT DX,AX
	;; SHIFT CH BY 1. 000010H
	SHL CH,01
	;; SHIFT BX TO THE RIGHT. 2701 BECOMES 270 
	SHR BX,01
	
	DEC CL
	CMP CL,0H
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
	PUSH DS
	;; LOAD DATA SEGMENT
	MOV BX, DATA_SEG
	MOV DS,BX
	;; STORE AL TEMPORARILY IN BL
	MOV BL,AL
	;; GET THE BLTH INDEX OF THE BCD ARRAY IN DATA_SEG
	MOV AL,DS:BCD[BL]
	POP DS
	POP BX
	RET
	
BCDTO7SEG	ENP	

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



TIMER2_ACTION	PROC	FAR
		PUSH	AX
		PUSH	DS
		PUSH	BX
		PUSH	CX

		MOV	AX,DATA_SEG
		MOV	DS,AX
	
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

T_NEXT1:	
		POP	CX
		POP	BX
		POP	DS
		POP 	AX
		RET
TIMER2_ACTION	ENDP


CODE_SEG	ENDS
END
