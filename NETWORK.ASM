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
	BARCODE	    DB	'!18243337:0012#'
	RECV_MESS	DB	10,13,'PRICE RECV'
	CUR_PRICE	DB	8 DUP(?)
	CUR_INDEX	DB	0
	NET_STATE	DB	0
	ADDRESS		DW	8001H
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
                JMP S_RET
ISMYADDR:
        ;; SET STATE TO SELECTED
                MOV AL,NET_SELECTED
                MOV DS:NET_STATE, AL
                MOV AL, 0
        ;; RESET ADDR_COUNT
                MOV DS:ADDR_COUNT, AL
                JMP S_RET

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
