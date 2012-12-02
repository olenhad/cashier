$MOD186
$EP
NAME TIMER
; Main program for uPD70208 microcomputer system
;
; US, WE WROTE IT BITCHES! hey Colin, TTT, and uncle! ;)
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
MMCS EQU 0FFA6H
PCS4 EQU 200H
KBD_BUFFER_LEN	EQU		5
KBD_BUFFER_LEN2	EQU		5
KBD_DEBOUNCE    EQU     10
KBD_DEBOUNCE2   EQU     10
KBD_NEW EQU 01H
KBD_STALE	EQU	02H

BARCODE_LEN     EQU     08
QUANTITY_LEN    EQU     04
NET_READY EQU 0
NET_PREADDR_BUFFERING EQU 1
NET_ADDR_RECIEVING EQU 2
NET_SELECTED EQU 3
NET_BUFFERING EQU 4
NET_EOT_PROMPT EQU 5
ADDR_LEN EQU 2
NACK    EQU     'N'
NET_PREADDR_CHAR EQU '`'
NET_PREADDR_LEN  EQU 3
NET_PAYLOAD_LEN  EQU 15
NET_M_EOT_PROMPT  EQU '?'
NET_M_EOT_Y       EQU '}'
NET_M_EOT_N       EQU '@'
        ;; PRICE_LEN NEEDS TO TAKE ACCOUNT OF . 
PRICE_LEN EQU 6
PRICE_NEW EQU 01
PRICE_STALE EQU 02
CASHIER_READY EQU 0
CASHIER_START_TRANSACTION EQU 1
CASHIER_RECV_BARCODE EQU 2
CASHIER_RECV_QUANTITY  EQU 4
CASHIER_GET_PRICE	EQU 5
CASHIER_DISPLAY_PRICE	EQU 6
CASHIER_EOT_PROMPT	EQU 7
CASHIER_EOT	EQU	8
SOUND_INPUT_LEN EQU 36
TIMER_CHOOSE_KBD_1 EQU 1
TIMER_CHOOSE_KBD_2 EQU 2
TIMER_COUNTER_LED_LEN   EQU 24
TIMER_COUNTER_KBD_LEN   EQU 48
SOUND_STATE_ENABLED EQU 0
SOUND_STATE_DISABLED EQU 1
BILL_BUFFER_LEN EQU 6
STACK_SEG	SEGMENT
		DB	256 DUP(?)
	TOS	LABEL	WORD
STACK_SEG	ENDS


DATA_SEG	SEGMENT

	CASHIER_STATE	DB	CASHIER_READY
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
		
	KBD_ROW_COUNTER	DB	0H
	;USED TO OUTPUT TO KBD
	KBD_OUTPUT	DB	07FH
	
	; CONTAINS KEYBOARD INPUT ASCII CHARS
	KBD_INPUT	DB	0H
	KBD_LATCH	DB	KBD_STALE
	LED_CURSOR	DB	0H
        KBD_ROW_COUNTER2 DB     0H
        KBD_OUTPUT2      DB     0F7H
        KBD_INPUT2	DB	0H
    TIMER_COUNTER_LED   DB  0H
    TIMER_COUNTER_KBD DW 0H
    TIMER_CHOOSE_KBD    DB TIMER_CHOOSE_KBD_1
;=========================================================================
;Port B of the 8255 is used as the input port for the keybad
;Port C is used as the output and grounds the rows one by one
;=========================================================================
	;; THIS IS  THE PACKET TRANSMITTED. SET LENGTH IN NET_PAYLOAD_LEN
	BARCODE	    DB	'!18243337:0012#'
	
	RECV_MESS	DB	10,13,'PRICE RECV'
    BILL_BUFFER DB 10 DUP(?)
    BILL_BUFFER_SEEK DB 0
	CUR_PRICE	DB	8 DUP(?)
        CUR_PRICE_LATCH DB      PRICE_STALE
        CUR_INDEX	DB	0
	NET_STATE	DB	0
	ADDRESS		DW	'ab'
        ADDR_INCOM_COUNT      DB      0
        ADDR_COUNT      DB      0
        ADDR_BUFFER     DB      4 DUP(?)
            SOUND_SEGNUM DW 8000H
    SOUND_OFFSET DW 0
    SOUND_DATA DB 0
        SOUND_BASE_ADDR     DW  0, 4713,8481,11945,15315,18317,21614,26120,30013,31809,36107,39110,43768,47646,51847,56124
        endsize         DW  4712,8480,11944,15314,18316,21613,26119,30012,31808,36106,39109,43767,47645,51846,56123,60774
    
    SOUND_INPUT_SEEK    DB 0
	SOUND_ENABLE   DB SOUND_STATE_DISABLED

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
        SOUND_INPUT DW  0,1,8,3,0,1,2,3,7,8,9,7,8,9,2,3,5,6 
        SAFETY_BUFFER DB 20 DUP(?)
	BARCODE_BUFFER	DB	50 DUP(?)
	BARCODE_SEEK	DB	0H
	QUANTITY_BUFFER	DB	10 DUP(?)
	QUANTITY_SEEK	DB	0H
        KBD_BUFFER_SEEK DB      0H
        KBD_BUFFER_SEEK2  DB      0H
        KBD_BUFFER2	DB	30 DUP(?)
	KBD_BUFFER	DB	30 DUP(?)

        
        
DATA_SEG	ENDS


CODE_SEG	SEGMENT

	PUBLIC		START

ASSUME	CS:CODE_SEG, SS:STACK_SEG, DS:DATA_SEG

START:
; initialize the ROM addr range
MOV DX, MMCS
MOV AX, 8003H
OUT DX, AX
 ; Initialize MPCS to MAP peripheral to IO address
         MOV DX, MPCS
         MOV AX, 4083H
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

;	CALL FAR PTR KEYBOARD
 ;       call far ptr keyboard2
   CALL FAR PTR CASHIER_STATE_M 
	
        
 JMP NEXT

; ^^^^^^^^^^^^^^^ End of User main routine ^^^^^^^^^^^^^^^^^^^^^^^^^

TRIGGER_START_T EQU	'A'
TRIGGER_BARCODE EQU 'D'
TRIGGER_QUANTITY EQU    'E'
TRIGGER_MORE_ITEMS  EQU 'F'
TRIGGER_EOT EQU 'G'
CASHIER_STATE_M		PROC	FAR
	PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX
	MOV AL, DS:CASHIER_STATE
	CMP AL, CASHIER_READY
	JE C_READY_HANDLE
        
        CMP AL, CASHIER_START_TRANSACTION
         	JE C_START_T_HANDLE
	
	CMP AL, CASHIER_RECV_BARCODE
         	JE C_RECV_BARCODE

        CMP AL, CASHIER_RECV_QUANTITY
                 JE HACKY_RECV_QUANTITY      

        CMP AL, CASHIER_GET_PRICE
                 JE HACKY_GET_PRICE

        CMP AL, CASHIER_DISPLAY_PRICE
        JE HACKY_DISP_PRICE
        
        CMP AL, CASHIER_EOT_PROMPT
        JE HACKY_EOT_PROMPT

        CMP AL, CASHIER_EOT
        JE HACKY_EOT
        JMP EXIT_CASHIER_STATE
HACKY_GET_PRICE:
        JMP C_GET_PRICE
HACKY_DISP_PRICE:
        JMP C_DISP_PRICE
HACKY_RECV_QUANTITY:
        JMP C_RECV_QUANTITY
HACKY_EOT_PROMPT:
        JMP C_EOT_PROMPT        
HACKY_EOT:
        JMP C_EOT
C_READY_HANDLE:
        ;; 	mov bl, ds:kbd_latch
        ;;         cmp bl, kbd_new
        ;;         jne hacky_ready

        ;;         mov ds:kbd_latch, kbd_stale 
        mov al, ds:kbd_input2
        CMP AL, 'A'
	JNE HACKY_READY
	MOV AL, CASHIER_START_TRANSACTION
	MOV DS:CASHIER_STATE, AL
       
       
	JMP EXIT_CASHIER_STATE

HACKY_READY:
       	
        JMP EXIT_CASHIER_STATE
C_START_T_HANDLE:
	CALL FAR PTR CLEAR_LED
    MOV DS:BARCODE_SEEK, 0
	MOV AL, CASHIER_RECV_BARCODE
	MOV DS:CASHIER_STATE, AL
        
	MOV DS:LED_CURSOR, 0H
        MOV AL,'s'
        CALL FAR PTR PRINT_CHAR
        
	JMP EXIT_CASHIER_STATE
		
C_RECV_BARCODE:
        
         MOV AL, DS:KBD_INPUT2
	CMP AL, TRIGGER_BARCODE
	JE UPDATE_B_STATE

	MOV AL, DS:KBD_LATCH
  
	CMP AL, KBD_NEW
	JNE EXIT_RECV_BARCODE
        
	mov al, 'c'
        call far ptr print_char
        
	;; ======== ADD TO BARCODE_BUFFER
	MOV AL, DS:KBD_INPUT
	MOV BL, DS:BARCODE_SEEK
	XOR BH, BH
	MOV DS:BARCODE_BUFFER[BX],AL
      	INC BL


     
	MOV DS:BARCODE_SEEK, BL
        MOV DS:KBD_LATCH, KBD_STALE

       	
        
        JMP EXIT_RECV_BARCODE
	
	
UPDATE_B_STATE:
        MOV DS:CASHIER_STATE, CASHIER_RECV_QUANTITY
        
           call far ptr PRINT_BARCODE_BUFFER 
        call far ptr CLEAR_LED
EXIT_RECV_BARCODE:
	JMP EXIT_CASHIER_STATE

C_RECV_QUANTITY:
        MOV AL,DS:KBD_INPUT2
        ;; HERE TRIGGER_BARCODE IS BEING USED FOR THE QUANTITY
        CMP AL,TRIGGER_QUANTITY
        JE UPDATE_C_STATE

        MOV  AL,DS:KBD_LATCH
        CMP AL,KBD_NEW
        JNE EXIT_RECV_QUANTITY
        mov al, 'q'
        call far ptr print_char
        
        MOV AL,DS:KBD_INPUT
        MOV BL,DS:QUANTITY_SEEK
        XOR BH,BH
        MOV DS:QUANTITY_BUFFER[BX],AL
        INC BL
        MOV DS:QUANTITY_SEEK,BL
        MOV DS:KBD_LATCH,KBD_STALE
        JMP EXIT_RECV_QUANTITY
        
UPDATE_C_STATE:
;;; REMEMBER TO MAKE INTO A PACKET
        MOV DS:QUANTITY_SEEK, 0
                 CALL FAR PTR MAKE_PACKET
                 call far ptr CLEAR_LED
             MOV DS:CASHIER_STATE,CASHIER_GET_PRICE
EXIT_RECV_QUANTITY:
	JMP EXIT_CASHIER_STATE		
	
C_GET_PRICE:
        MOV AL, DS:CUR_PRICE_LATCH
        CMP AL, PRICE_NEW
        JNE EXIT_GET_PRICE
        MOV DS:CASHIER_STATE, CASHIER_DISPLAY_PRICE
        ; reset the CUR_PRICE_LATCH
        MOV DS:CUR_PRICE_LATCH, PRICE_STALE
      

EXIT_GET_PRICE:
        JMP EXIT_CASHIER_STATE

C_DISP_PRICE:
        CALL FAR PTR DISPLAY_PRICE_LED
        CALL FAR PTR SOUND_ADD_BUFFER
        mov ds:SOUND_OFFSET, 0
        ;mov AL, DS:SOUND_INPUT_SEEK
        ;ADD AL,48
        ;CALL FAR PTR PRINT_CHAR
        MOV DS:SOUND_ENABLE, SOUND_STATE_ENABLED

        MOV DS:CASHIER_STATE, CASHIER_EOT_PROMPT
        JMP EXIT_CASHIER_STATE
C_EOT_PROMPT:
        CALL FAR PTR CLEAR_LED
        CMP DS:KBD_INPUT2,TRIGGER_MORE_ITEMS
        JE MORE_ITEMS
        CMP DS:KBD_INPUT2, TRIGGER_EOT
        JE NO_MORE_ITEMS
        JMP EXIT_CASHIER_STATE
MORE_ITEMS:
MOV DS:CASHIER_STATE, CASHIER_START_TRANSACTION
MOV DS:NET_STATE, NET_READY
MOV AL, NET_M_EOT_N
CALL FAR PTR PRINT_CHAR

JMP EXIT_CASHIER_STATE

NO_MORE_ITEMS:
    MOV DS:CASHIER_STATE, CASHIER_EOT
MOV DS:NET_STATE, NET_EOT_PROMPT
MOV AL, NET_M_EOT_Y
CALL FAR PTR PRINT_CHAR
JMP EXIT_CASHIER_STATE

C_EOT:
   MOV DS:CASHIER_STATE, CASHIER_READY
   JMP EXIT_CASHIER_STATE     

EXIT_CASHIER_STATE:
        POP DX
	POP CX
	POP BX
	POP AX
	RET
	
CASHIER_STATE_M 	ENDP

SERIAL_REC_ACTION	PROC	FAR
        PUSH    AX
		PUSH	CX
		PUSH 	BX
		PUSH	DS

; AL CONTAINS THE CHAR RECIEVED        
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
        CMP BL, NET_EOT_PROMPT
        JZ NET_HACKY_EOTP
        ;; 
                CMP AL, NET_PREADDR_CHAR
                JZ ADDR_BUFFERING

        
		JMP	S_RET
        
        ;; 	JNE	WRONG_ADDRESS
;		IF POLL IS SELECTING DS:ADDRESS, TRANSMIT THE BARCODE
NET_HACKY_EOTP:
        JMP EOT_PROMPT
CALL_BUFFERING:
                CALL FAR PTR DATA_BUFFERING
                JMP S_RET
SELECTED:

        
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
NOT_READY:
        MOV DS:NET_STATE, NET_READY
		MOV AL, NACK
		CALL FAR PTR PRINT_CHAR
		JMP S_RET
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
EOT_PROMPT:
        MOV BL, DS:BILL_BUFFER_SEEK
        XOR BH, BH
        MOV DS:BILL_BUFFER[BX], AL
        INC DS:BILL_BUFFER_SEEK
        CMP DS:BILL_BUFFER_SEEK, BILL_BUFFER_LEN
        JNE S_RET
        MOV DS:BILL_BUFFER_SEEK, 0
        MOV DS:NET_STATE, NET_READY
        CALL FAR PTR BILL_DISPLAY

        JMP S_RET
S_RET:
		POP	DS
		POP	BX
		POP	CX
        POP AX
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
                MOV DS:CUR_PRICE_LATCH, PRICE_NEW
		MOV DS:NET_STATE, NET_EOT_PROMPT
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
		
		
        MOV AL,DS:KBD_ROW_COUNTER
    
        CMP AL,04
        JL NEXT_ROW

hacky_stuff:
        MOV CL, 07FH    ;STORES OUTPUT FOR ROW COUNTER;
        ;MOV AL,0FEH ; 1111 1110
        MOV CH, 0H  ;set row counter
        MOV DS:KBD_ROW_COUNTER,CH
        MOV DS:KBD_OUTPUT, CL

		;jmp exit_kbd
NEXT_ROW:
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
        ;; | BUFFER[0]== aL = ADD_TO_BUFFER
        ;; | ELSE = (ADD_TO_BUFFER) & (SEEK 0) AKA RESET
		;JMP NUMBERS

NUMBERS:
		XOR BH, BH
		MOV AL,DS:array[BX] ; Stores character in AL (?)
		XOR AH,AH
		;CALL	FAR PTR PRINT_CHAR
		; Hack to fix the * from irritating
		CMP AL, '*'
		JE Debounce
        MOV CL, DS:KBD_BUFFER_SEEK
        CMP CL,0H
        JZ ADD_TO_BUFFER

	
        MOV CL, KBD_BUFFER
		;COMPARE WITH VALUE AT first
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

		mov cx, KBD_DEBOUNCE
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
		MOV DS:KBD_LATCH, KBD_NEW
		CALL	FAR PTR PRINT_CHAR
		CALL FAR PTR ADD_LED_BUFFER
		XOR CL,CL
		;BUFFER_SEEK IS RESET
		MOV DS:KBD_BUFFER_SEEK, CL
RETPOINT:	
	INC DS:KBD_ROW_COUNTER
	
	;JMP NEXT_ROW
		
exit_kbd:	
	POP AX
	POP BX
	POP CX
	POP DX
	RET
KEYBOARD ENDP

KBD_PROCESS		PROC	FAR
                push ax
                push dx
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

	        pop dx
                pop ax
RET
KBD_PROCESS		ENDP

KEYBOARD2 PROC FAR

		PUSH    DX
		PUSH	CX
		PUSH 	BX
		PUSH	AX
		

INIT2:		
		
		MOV AL,DS:KBD_ROW_COUNTER2
    
        CMP AL,04
        JL NEXT_ROW2
		
		;call far ptr print_char
		
hacky_shit:
        MOV CL, 0F7H    ;STORES OUTPUT FOR ROW COUNTER;
        ;MOV AL,0FEH ; 1110 1111
        MOV CH, 0H  ;set row counter
        MOV DS:KBD_ROW_COUNTER2,CH
        MOV DS:KBD_OUTPUT2, CL

		
NEXT_ROW2:

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

		mov cx, KBD_DEBOUNCE2
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
	
		
exit_kbd2:	
	POP AX
	POP BX
	POP CX
	POP DX
	RET
KEYBOARD2 ENDP

KBD_PROCESS2		PROC	FAR
                push ax
                push dx
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

	        pop dx
                pop ax
RET
KBD_PROCESS2		ENDP
        
TIMER2_ACTION	PROC	FAR
		PUSH	AX
		PUSH	DS
		PUSH	BX
		PUSH	CX

		
        INC DS:TIMER_COUNTER_LED
        INC DS:TIMER_COUNTER_KBD

        CMP DS:TIMER_COUNTER_KBD, TIMER_COUNTER_KBD_LEN
        JE CHOOSE_KBD
        CMP DS:TIMER_COUNTER_LED, TIMER_COUNTER_LED_LEN
        JE EXEC_LED
EXEC_SOUND:
        CMP DS:SOUND_ENABLE, SOUND_STATE_ENABLED
        JNE EXIT_TIMER_ACTION
        CALL FAR PTR SOUND_OUTPUT_CALLBACK
        JMP EXIT_TIMER_ACTION

EXEC_LED:
        MOV DS:TIMER_COUNTER_LED, 0
        CALL FAR PTR DISPLAY_LED
        JMP EXIT_TIMER_ACTION

CHOOSE_KBD:
        MOV DS:TIMER_COUNTER_KBD, 0

        CMP DS:TIMER_CHOOSE_KBD, TIMER_CHOOSE_KBD_1
        JE EXEC_KB1
        CALL FAR PTR KEYBOARD2
        MOV DS:TIMER_CHOOSE_KBD, TIMER_CHOOSE_KBD_1
        JMP EXIT_TIMER_ACTION

EXEC_KB1:

        CALL FAR PTR KEYBOARD
        ;mov al, 'k'
        ;call far ptr print_char
        MOV DS:TIMER_CHOOSE_KBD, TIMER_CHOOSE_KBD_2
        JMP EXIT_TIMER_ACTION

EXIT_TIMER_ACTION:
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

	MOV CX, 200
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

DISPLAY_PRICE_LED PROC FAR
        PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX
        MOV CX, PRICE_LEN
        MOV BX,0
 
LOOP_DISP_LED:
        MOV AL, DS:CUR_PRICE[BX]
        SUB AL,48
        CALL FAR PTR BCD_TO_7SEG
        MOV DS:LED_BUFFER[BX], AL
        INC BX
        LOOP LOOP_DISP_LED
        
        
	POP DX
	POP CX
	POP BX
	POP AX
	RET
DISPLAY_PRICE_LED ENDP
ADD_LED_BUFFER 	PROC	FAR
	PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX
    
	MOV AL, DS:LED_CURSOR
	CMP AL, 06
	JNE DONE_INC
	XOR AL, AL
	MOV DS:LED_CURSOR,AL
	
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
    INC DS:LED_CURSOR	
	POP DX
	POP CX
	POP BX
	POP AX
	RET
ADD_LED_BUFFER	ENDP

CLEAR_LED	PROC	FAR
	PUSH AX
	PUSH BX
	PUSH CX
	;; LEDS DISPLAY -
	MOV AL, 040H
	XOR BX, BX
	MOV CX, 06
CLEAR_START:
	MOV DS:LED_BUFFER[BX], AL
	INC BX
	LOOP CLEAR_START

	POP CX
	POP BX
	POP AX
	RET
CLEAR_LED	ENDP

MAKE_PACKET     PROC    FAR
        PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX
        PUSH SI
        MOV CX,BARCODE_LEN
        XOR BH,BH
        MOV BL, 01
        MOV SI,0
BARCODE_LOOP:
        MOV AL, DS:BARCODE_BUFFER[SI]
        MOV DS:BARCODE[BX],AL
        INC BX
        INC SI
        LOOP BARCODE_LOOP
        
        INC BX
        ; FOR : BETWEEN BARCODE & PRICE
        
        MOV CX, QUANTITY_LEN
        MOV SI,0
QUANTITY_LOOP:
        MOV AL, DS:QUANTITY_BUFFER[SI]
        MOV DS:BARCODE[BX],AL
        INC BX
        INC SI
        LOOP QUANTITY_LOOP

        POP SI
        POP DX
        POP CX
        POP BX
        POP AX
        RET
MAKE_PACKET     ENDP
        

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

PRINT_BARCODE_BUFFER PROC FAR
        PUSH AX
        PUSH BX
        PUSH CX
        MOV AL, 'U'
        CALL FAR PTR PRINT_CHAR
        MOV CX, BARCODE_LEN
        MOV BX,0
LOOP_BB:
        MOV AL, DS:BARCODE_BUFFER[BX]
        CALL FAR PTR PRINT_CHAR
        INC BX
        LOOP LOOP_BB

        POP CX
        POP BX
        POP AX
        RET
PRINT_BARCODE_BUFFER ENDP

SOUND_OUTPUT_CALLBACK   PROC FAR
        PUSH    AX
        PUSH    DS
        PUSH    BX
        PUSH    CX
        PUSH    DX
        PUSH    SI
        PUSH    DI
                MOV AX, DATA_SEG
                MOV DS, AX

        ;T_NEXT1:
                CMP SOUND_OFFSET, 0
                JNE PLAY
                MOV AL, DS:SOUND_INPUT_SEEK
                XOR AH,AH
                MOV SI, AX      
                MOV BX, SOUND_INPUT[SI]
                ;SUB BX,48
                ;mov bx,18
                ADD BX, BX
                
                MOV AX,DS:SOUND_BASE_ADDR[BX]
                MOV DS:SOUND_OFFSET,AX
                
                
                
        PLAY:   

                
                MOV BL, DS:SOUND_INPUT_SEEK
                XOR BH, BH
                MOV BX, DS:SOUND_INPUT[BX]
                ADD BX, BX
                
                mov cx, endsize[bx]
                
                ADD SOUND_OFFSET, 1
                CMP SOUND_OFFSET, cx
                JNE SKIP
                
                INC DS:SOUND_INPUT_SEEK
                INC DS:SOUND_INPUT_SEEK
                MOV AL, SOUND_INPUT_SEEK
                CMP AL, SOUND_INPUT_LEN
                JL RESET_BASE_ADDR

                MOV DS:SOUND_INPUT_SEEK, 0
                MOV DS:SOUND_ENABLE, SOUND_STATE_DISABLED
                MOV DS:SOUND_OFFSET, 0
        RESET_BASE_ADDR:        
                
                MOV BL, DS:SOUND_INPUT_SEEK
                XOR BH,BH
                ; SOUND i BLOODY WNAT TO PLAY
                MOV BX, SOUND_INPUT[BX]
                ADD BX,BX
                ; THE BLOODY OFFSET ADDR OF THAT SOUND
                MOV AX, DS:SOUND_BASE_ADDR[BX]
                MOV DS:SOUND_OFFSET, AX
                ;ADD SOUND_SEGNUM, 1
        SKIP:
                MOV SI, SOUND_OFFSET
                
                ;MOV AX, SOUND_SEGNUM
                MOV AX, 8000H
                MOV DS, AX
                
                ;MOV AX, SI
                MOV AL, DS:[SI]
                MOV DX, PCS4
                OUT DX, AL

                ; RESTORE DATA_SEG
                MOV AX, DATA_SEG
                MOV DS, AX

                
        EXIT_SOUND:     
        POP DI
        POP SI
        POP DX
        POP CX
        POP BX
        POP DS
        POP AX
        RET

SOUND_OUTPUT_CALLBACK ENDP

SOUND_ADD_BUFFER    PROC FAR
        PUSH    AX
        PUSH    BX
        PUSH    CX
        PUSH    DX
        PUSH    SI
        PUSH    DI
        MOV CX, BARCODE_LEN
        XOR BX, BX
        XOR SI, SI
LOOP_B_SOUND:
        MOV AL, BARCODE_BUFFER[BX]
        INC BX 
        SUB AL, 48
        XOR AH,AH
        MOV SOUND_INPUT[SI], AX
        INC SI
        INC SI
        LOOP LOOP_B_SOUND

        MOV CX, QUANTITY_LEN
        XOR BX, BX
LOOP_Q_SOUND:
        MOV AL, QUANTITY_BUFFER[BX]
        INC BX 
        SUB AL, 48
        XOR AH,AH
        MOV SOUND_INPUT[SI], AX
        INC SI
        INC SI
        LOOP LOOP_Q_SOUND

        MOV CX, PRICE_LEN
        XOR BX, BX
LOOP_Q_PRICE:
        MOV AL, CUR_PRICE[BX]
        INC BX 
        SUB AL, 48
        XOR AH,AH
        MOV SOUND_INPUT[SI], AX
        INC SI
        INC SI
        LOOP LOOP_Q_PRICE
        MOV DS:SOUND_INPUT_SEEK, 0

        POP DI
        POP SI
        POP DX
        POP CX
        POP BX
        POP AX
        RET
SOUND_ADD_BUFFER    ENDP

BILL_DISPLAY PROC FAR
        PUSH    AX
        PUSH    BX
        PUSH    CX
        PUSH    DX
        PUSH    SI
        PUSH    DI
        MOV CX, BILL_BUFFER_LEN
        MOV BX, 0
        MOV SI,0

BLOODY_SHIT:

        MOV AL, DS:BILL_BUFFER[BX]
        SUB AL, 48
        CALL FAR PTR BCD_TO_7SEG
        MOV DS:LED_BUFFER[SI], AL
        INC BX
        INC SI
        LOOP BLOODY_SHIT
        POP DI
        POP SI
        POP DX
        POP CX
        POP BX
        POP AX
        RET
BILL_DISPLAY ENDP

CODE_SEG	ENDS
END
