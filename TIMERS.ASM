$MOD186
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

STACK_SEG	SEGMENT
		DB	256 DUP(?)
	TOS	LABEL	WORD
STACK_SEG	ENDS


DATA_SEG	SEGMENT
;For timer 2
	TIMER2_MESS	DB	10,13,'TIMER2 INTERRUPT    '
	T_COUNT		DB	2FH
	T_COUNT_SET	DB	2FH
	REC2_MESS	DB	10,13,'Period of timer2 =     '
;For timer 0
	TIMER0_MESS	DB	10,13,'TIMER0 INTERRUPT    '
	T0_COUNT		DB	2FH
	T0_COUNT_SET	DB	2FH
	REC0_MESS	DB	10,13,'Period of timer0 =     '	
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
		CALL FAR PTR IODEFINE
		


; ^^^^^^^^^^^^^^^^^  Start of User Main Routine  ^^^^^^^^^^^^^^^^^^
    call set_timer2
                 STI

NEXT:     
;Keypad reading
	;CALL FAR PTR KEYPAD
	;Interpret co-ordinates
	
	;Add to item code or quantity
	;CALL FAR PTR APPEND
	;Echo digit read to LED and voice
	;CALL FAR PTR ACTIVATE_LED
	;CALL FAR PTR PUSH_VOICE_QUEUE
	;If IC and quantity fully read, 
		;Write info to serial queue
	;CALL FAR PTR SERIAL_REC_ACTION
		;Update voice synth (announce quantity)
	;CALL FAR PTR ACTIVATE_VOICE
	;Update LED
;Go to main






JMP NEXT


; ^^^^^^^^^^^^^^^ End of User main routine ^^^^^^^^^^^^^^^^^^^^^^^^^
	KEYPAD PROC FAR 
	
	;Read data and load to register
	
	ENDP
	APPEND PROC FAR
	
	;Append data to existing data
	
	ENDP
	
	ACTIVATE_LED PROC FAR
	;Display appended data on LED
	ENDP
	
	PUSH_VOICE_QUEUE PROC FAR
	;Load keypad data to voice queue
	ENDP
	
	ACTIVATE_VOICE PROC FAR
	
	ENDP

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

TIMER0_ACTION	PROC	FAR
		PUSH	AX
		PUSH	DS
		PUSH	BX
		PUSH	CX

		MOV	AX,DATA_SEG
		MOV	DS,AX
	
		DEC	DS:T0_COUNT
		JNZ	T0_NEXT1
		MOV	AL,DS:T0_COUNT_SET
		MOV	DS:T0_COUNT,AL

		MOV	CX,20
		MOV	BX,0H
T0_NEXT0:
		MOV	AL,DS:TIMER0_MESS[BX]
		INC	BX
		CALL 	FAR PTR PRINT_CHAR
		LOOP	T0_NEXT0

T0_NEXT1:	
		POP	CX
		POP	BX
		POP	DS
		POP 	AX
		RET
TIMER0_ACTION	ENDP

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
		MOV	AL,DS:TIMER2_MESS[BX]
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
