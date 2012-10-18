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
PORTA 	equ 080h
PORTB EQU 081H
PORTC EQU 082H
CWR EQU 083H

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

;=========================================================================
;Port B of the 8255 is used as the input port for the keybad
;Port C is used as the output and grounds the rows one by one
;=========================================================================

DATA_SEG	ENDS


CODE_SEG	SEGMENT

	PUBLIC		START

ASSUME	CS:CODE_SEG, SS:STACK_SEG, DS:DATA_SEG

START:
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
 ;call set_timer2
                STI

NEXT: 
CALL FAR PTR KEYBOARD
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

T_NEXT1:	CALL FAR PTR KEYBOARD
		POP	CX
		POP	BX
		POP	DS
		POP 	AX
		RET
TIMER2_ACTION	ENDP


CODE_SEG	ENDS
END
