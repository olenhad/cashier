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
	BARCODE	    DB	10,13,'18243337:12',10,13
	RECV_MESS	DB	10,13,'PRICE RECV'
	CUR_PRICE	DB	8 DUP(?)
	CUR_INDEX	DB	0
	NET_STATE	DB	0
	ADDRESS		DB	'a'
	;0 => READY
	;1 => SELECTED
	;2 => BUFFERING
	
DATA_SEG	ENDS
NET_READY EQU 0
NET_SELECTED EQU 1
NET_BUFFERING EQU 2

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
		CMP BL, NET_BUFFERING
		JZ BUFFERING
		CMP BL, NET_SELECTED
		JZ SELECTED
		
		CMP	AL,DS:ADDRESS
		JNE	WRONG_ADDRESS
;		IF POLL IS SELECTING DS:ADDRESS, TRANSMIT THE BARCODE
SELECTED:
		MOV DS:NET_STATE, 01
		MOV CX, 15
		MOV BX, 0
START_TRANSMIT:		
		MOV AL,DS:BARCODE[BX]
		CALL FAR PTR PRINT_CHAR
		INC BX
		LOOP	START_TRANSMIT
		MOV DS:NET_STATE, NET_BUFFERING
		JMP	S_RET

WRONG_ADDRESS:		
		CALL FAR PTR PRINT_CHAR
		JMP S_RET
BUFFERING:
		MOV BL, DS:CUR_INDEX
		XOR BH,BH
		MOV DS:CUR_PRICE[BX], AL
		
		INC BL
		CMP BL, 08
		JGE BUFFER_END
		MOV DS:CUR_INDEX, BL
		JMP S_RET
BUFFER_END:
		MOV CX,08
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
