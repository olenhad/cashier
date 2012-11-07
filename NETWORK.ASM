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

STACK_SEG	SEGMENT
		DB	256 DUP(?)
	TOS	LABEL	WORD
STACK_SEG	ENDS


DATA_SEG	SEGMENT
	TIMER0_MESS	DB	10,13,'TIMER2 INTERRUPT    '
	T_COUNT		DB	2FH
	T_COUNT_SET	DB	2FH
	REC_MESS	DB	10,13,'Period of timer0 =     '
	BARCODE	    DB	10,13,'18243337'
	RECV_MESS	DB	10,13,'PRICE RECV'
	CUR_PRICE	DB	8 DUP(?)
	CUR_INDEX	DB	0
	NET_STATE	DB	0 
	;0 => READY
	;1 => BUFFERING/RECIEVING
	
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

NEXT:     
JMP NEXT

; ^^^^^^^^^^^^^^^ End of User main routine ^^^^^^^^^^^^^^^^^^^^^^^^^


SERIAL_REC_ACTION	PROC	FAR
		PUSH	CX
		PUSH 	BX
		PUSH	DS

		MOV	BX,DATA_SEG		;initialize data segment register
		MOV	DS,BX
		MOV BL, DS:NET_STATE
		CMP BL, 0
		JNZ BUFFERING
		CMP AL,BL
		
		
		CMP	AL,'<'
		JNE	S_FAST

		INC	DS:T_COUNT_SET
		INC	DS:T_COUNT_SET

		MOV CX, 10
		MOV BX, 0
START_BARCODE:		
		MOV AL,DS:BARCODE[BX]
		CALL FAR PTR PRINT_CHAR
		INC BX
		LOOP	START_BARCODE
		
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
		JMP S_RET
BUFFERING:
		MOV BX, DS:CUR_INDEX
		MOV DS:CUR_PRICE[BX]
		INC BX
		CMP BX, 08
		JGE BUFFER_END
		MOV DS:CUR_INDEX, BX
		JMP S_RET
BUFFER_END:
		MOV CX,08
		MOV BX, 0
ACK_LOOP:
		MOV AL,
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

;		MOV	CX,20
;		MOV	BX,0H
T_NEXT0:
;		MOV	AL,DS:TIMER0_MESS[BX]
;		INC	BX
;		CALL 	FAR PTR PRINT_CHAR
;		LOOP	T_NEXT0

T_NEXT1:
		MOV DX, 0100H
		MOV AX, 01H
		OUT DX, AX
		
		MOV DX, 0180H
		MOV AX, 07EH
		OUT DX, AX
		
		POP	CX
		POP	BX
		POP	DS
		POP 	AX
		RET
TIMER2_ACTION	ENDP


CODE_SEG	ENDS
END
