	page	0
	CPU	Z80
;;; 
;;; Universal Monitor for Zilog Z80
;;;   Copyright (C) 2019,2020,2021 Haruo Asano
;;;

;;; 2022.10.10 It was added functions by A.honda
;;; Rev.B01	add trace command
;;; Rev.B02	add disassemble commnand
;;; Rev.B3	add assemble command and PaloAltoTinyBASIC
;;;                and GAME80 interpreter and GAME80 compiler

;;; memory map and console I/O adjusted by Gazelle
;;; 2022.12.13 first relrase as Rev.B03
;;; 2022.12.29 Z80 code worng replacement was fixed
;;;		(SBC HL,BC & SBC HL,DE)
;;;
;;;
;;; original source
;;; https://electrelic.com/electrelic/node/1317
;;; https://github.com/akih-san/EMUZ80-MON

;;; Constants
CR	EQU	0DH
LF	EQU	0AH
BS	EQU	08H
DEL	EQU	7FH
ESC	equ	1BH
BUFLEN	equ	40
NUMLEN	equ	7
F_bitSize	equ	8
NO_UPPER	equ	00000100b
NO_LF		equ	00000010b
NO_CR		equ	00000001b

;;;
;;; Memory switch
;;;

Fullram = 0
RAM12K = 0
RAM8K = 1
BACKDOOR = 0
UART8251 = 1
UARTPIC = 0
UARTMEM = 0

	IF	Fullram
; MEMORY MAP for MEZ80RAM
WORK_B	equ	0FF00H	; work area FF00-FFFF
STACKM	equ	0D000H	; monitor stack
STACK	equ	0CF00H	; user stack
;ROM_B	equ	0D000H	;
RAM_B	equ	08000H	;MEZ80RAM RAM base address
RAM_E	equ	0CFFFH	;MEZ80RAM RAM END address
;IO_B	equ	0E000H	;
	ENDIF

	IF	RAM12K
;; ROM48K RAM12K for PIC18F47Q84
UARTDR	EQU	0FF00H	; UART DATA REGISTOR
UARTCR	EQU	0FF01H	; UART CONTROL REGISTOR
WORK_B	equ	0EF00H	; work area EF00-EFFF
STACKM	equ	0EF00H	; monitor stack
STACK	equ	0EEC0H	; user stack
ROM_B	equ	0000H	;EMUZ80_Q84 ROM base address
RAM_B	equ	0C000H	;EMUZ80_Q84 RAM base address
RAM_E	equ	0EFFFH	;EMUZ80_Q84 RAM END address
IO_B	equ	0F000H	;EMUZ80_Q84 I/O base address
	ENDIF

	IF	RAM8K
; ORIGINAL MEMORY MAP for PIC18F47Q84
UARTDR	EQU	0E000H	; UART DATA REGISTOR
UARTCR	EQU	0E001H	; UART CONTROL REGISTOR
WORK_B	equ	9F00H	; work area 9F00-9FFF
STACKM	equ	9F00H	; monitor stack
STACK	equ	9EC0H	; user stack
ROM_B	equ	0000H	;EMUZ80_Q84 ROM base address
RAM_B	equ	8000H	;EMUZ80_Q84 RAM base address
RAM_E	equ	9FFFH	;EMUZ80_Q84 RAM END address
IO_B	equ	0E000H	;EMUZ80_Q84 I/O base address
	ENDIF

BASIC_TOP	equ	3200H
BASIC_CST	equ	3200H	; basic cold start
BASIC_WST	equ	3203H	; basic warm start

	IF	RAM12K
TIM0_CTL0	equ	0F800H	; timer0 control0 register
TIM0_CTL1	equ	0F801H	; timer0 control1 register
TIMER0_CNTL	equ	0F810H	; timer0 16bit counter LSB
TIMER0_CNTH	equ	0F811H	; timer0 16bit counter MSB
TIMER0_SCTL	equ	0F820H	; timer0 seconds counter LSB
TIMER0_SCTH	equ	0F821H	; timer0 seconds counter MSB
TIMER0_INITC	equ	87e1h	; timer adjust count number
	ENDIF

	IF	Fullram
ENTRY	EQU	0D000H		; Entry point
	ELSE
ENTRY	EQU	0040H		; Entry point
	ENDIF

RAM_SIZ	EQU	(RAM_E + 1) - RAM_B

PATB_TOP	equ	5B00H
PATB_CST	equ	5B00H	; basic cold start
PATB_WST	equ	5B03H	; basic warm start

GM80_CST	equ	5400H	; basic cold start
GM80_WST	equ	5403H	; basic warm start
;;; 
;;; ROM area
;;;
start:
	IF	Fullram

	org	ENTRY

	ld	a,0c3h
	ld	(00h),a
	ld	(08h),a
	ld	(10h),a
	ld	(18h),a
	ld	(30h),a
	ld	(38h),a
	ld	hl,CSTART
	ld	(01h),hl
	ld	hl,CONOUT
	ld	(09h),hl
	ld	hl,CONIN
	ld	(11h),hl
	ld	hl,CONST
	ld	(19h),hl
	ld	hl,RST30H_IN
	ld	(31h),hl
	ld	hl,RST38H_IN
	ld	(39h),hl
	jp	CSTART

	ELSE

	ORG	ROM_B	; (RST 00H)

E_CSTART:
	JP	cold_st

E_WSTART:
	JP	API01

	db	0008H - $ dup(00H)
;	ORG	0008H	; (RST 08H)
	IF	BACKDOOR
	JP	RST08H
	ELSE
	JP	CONOUT
	ENDIF

	db	0010H - $ dup(00H)
;	ORG	0010H	; (RST 10H)
	IF	BACKDOOR
	JP	RST10H
	ELSE
	JP	CONIN
	ENDIF

	db	0018H - $ dup(00H)	; nop
;	ORG	0018H	; (RST 18H)
	IF	BACKDOOR
	JP	RST18H
	ELSE
	jp	CONST
	ENDIF

	db	0020H - $ dup(00H)	; nop
;	ORG	0020H	; (RST 20H)
	IF	BACKDOOR
	JP	RST20H
	ELSE
	RET
	ENDIF

	db	0028H - $ dup(00H)	; nop
;	ORG	0028H	; (RST 28H)
	IF	BACKDOOR
	RET
	ELSE
	RET
	ENDIF

	db	0030H - $ dup(00H)	; nop
;	ORG	0030H
	IF	BACKDOOR
	JP	RST30H
	ELSE
	JP	RST30H_IN
	ENDIF

	db	0038H - $ dup(00H)	; nop
;	ORG	0038H
	IF	BACKDOOR
	JP	RST38H;
	ELSE
	JP	RST38H_IN
	ENDIF

	;;
	;; Entry point
	;;

	db	ENTRY - $ dup(00H)
	;	ORG	ENTRY

cold_st:
	jp	CSTART

	IF	BACKDOOR
;;;
;;; RST 08H Handler
;;;
RST08H:

	ld	(save_hl), hl	; save hl
	ld	hl, (stealRST08)
	jp	jmp_rst				;jr	jmp_rst

;;;
;;; RST 10H Handler
;;;
RST10H:

	ld	(save_hl), hl	; save hl
	ld	hl, (stealRST10)
	jp	jmp_rst				;jr	jmp_rst

;;;
;;; RST 18H Handler
;;;
RST18H:

	ld	(save_hl), hl	; save hl
	ld	hl, (stealRST18)
	jp	jmp_rst				;jr	jmp_rst

;;;
;;; RST 20H Handler
;;;
RST20H:

	ld	(save_hl), hl	; save hl
	ld	hl, (stealRST20)
	jp	jmp_rst				;jr	jmp_rst

;;;
;;; RST 28H Handler
;;;
RST28H:

	ld	(save_hl), hl	; save hl
	ld	hl, (stealRST28)
	jp	jmp_rst				;jr	jmp_rst

;;;
;;; RST 30H Handler
;;;
RST30H:

	ld	(save_hl), hl	; save hl
	ld	hl, (stealRST30)
	jp	jmp_rst				;jr	jmp_rst

;;;
;;; RST 38H Handler
;;;
RST38H:

	ld	(save_hl), hl	; save hl
	ld	hl, (stealRST38)
jmp_rst:
	push	hl		; set jump address
	ld	hl, (save_hl)	; restore hl
RST20H_IN:
RST28H_IN:
	ret			; jump (stealRST38)
	ENDIF	;ELSE BACKDOOR
	ENDIF	;ELSE Fullram
;;;
;;; Universal Monitor Z80 Cold start
;;;

CSTART:
	DI
	LD	SP,STACKM	; monitor stask defines STACKM

	LD	HL,RAM_B
	LD	(DSADDR),HL
	LD	(SADDR),HL
	LD	(GADDR),HL
	ld	(asm_adr),HL
	ld	(tasm_adr),HL
	LD	A,'I'
	LD	(HEXMOD),A
	
	IF	BACKDOOR
	; init back door RST XXH entory point
	ld	hl, CONOUT		; RST 08H : CONOUT
	ld	(stealRST08), hl
	ld	hl, CONIN
	ld	(stealRST10), hl	; RST 10H : CONIN
	ld	hl, CONST
	ld	(stealRST18), hl	; RST 18H : CONST
	ld	hl, RST20H_IN
	ld	(stealRST20), hl
	ld	hl, RST28H_IN
	ld	(stealRST28), hl
	ld	hl, RST30H_IN
	ld	(stealRST30), hl
	ld	hl, RST38H_IN
	ld	(stealRST38), hl
	ENDIF

	;; Initialize register value
	XOR	A
	LD	HL,REG_B
	LD	B,REG_E-REG_B
IR0:
	LD	(HL),A
	INC	HL
	DEC	B			;Z80 CODE DJNZ	IR0 ############
	JP	NZ,IR0			;###############################
	LD	HL,STACK		; user stack define STACK
	LD	(REGSP),HL
	ld	hl, RAM_B
	ld	(REGPC), HL		; set program counter

	ld	b, F_bitSize
	ld	a, '.'
	ld	hl, F_bit
ir00:
	ld	(hl), a
	inc	hl
	DEC	B			;Z80 CODE djnz	ir00 ###########; init F_bit string
	JP	NZ,IR00			;###############################
	xor	a
	ld	(hl), a		; delimiter

; init dbg work area

	LD	B, dbg_wend - dbg_wtop
	ld	hl,dbg_wtop	
	XOR	A

dbg_wini:
	ld	(hl), a
	inc	hl
	DEC	B			;Z80 CODE DJNZ	dbg_wini #######
	JP	NZ,dbg_wini		;###############################

	ld	a, 'I'
	ld	(TM_mode), a	; default call_in mode
	ld	a, 'N'
	ld	(TP_mode), a	; default display reg mode
	ld	l, 0
	ld	h, l
	ld	(TC_cnt), hl	; clear trace step counter to 0
	xor	a
	ld	(fever_t), a	; clear flag trace forever
	; init bp, tp, gstop address & opcode
	ld	hl, RAM_B
	ld	(tpt1_adr), hl
	ld	(tpt2_adr), hl
	ld	(bpt1_adr), hl
	ld	(bpt2_adr), hl
	ld	(tmpb_adr), hl
	ld	a, (hl)
	ld	(tpt1_op), a
	ld	(tpt2_op), a
	ld	(bpt1_op), a
	ld	(bpt2_op), a
	ld	(tmpb_op), a
	
;; Opening message

	LD	HL,OPNMSG
	CALL	STROUT
;	EI

WSTART:
	xor	a
	ld	(ky_flg), a	; clear skip LF flag

	LD	HL,PROMPT
	CALL	STROUT
	CALL	GETLIN
	CALL	SKIPSP
	OR	A
	JP	Z,WSTART	;JR	Z, WSTART

	CP	'A'
	JP	Z, line_asm
	CP	'D'
	JP	Z, DUMP
	CP	'G'
	JP	Z, GO
	CP	'S'
	JP	Z, SETM

	CP	'L'
	JP	Z, LOADH
	CP	'P'
	JP	Z, SAVEH

	CP	'R'
	JP	Z, REG

	cp	'#'
	jp	Z, Launcher

	cp	'B'
	jp	z, brk_cmd	; break point command
	
	cp	'T'
	jp	z, trace_cmd	; trace point command

	cp	'?'
	jp	z, command_hlp	; command help message

ERR:
	LD	HL,ERRMSG
	CALL	STROUT
	JP	WSTART		;JR	WSTART

;;
;; command help
;;
command_hlp:

	LD	HL, cmd_hlp
	CALL	STROUT
	JP	WSTART			;JR	WSTART

;;
;; Launch appended program
;;
Launcher:
	INC	HL
	CALL	SKIPSP		; A <- next char
	OR	A
	JP	Z,ERR		;JR	Z,ERR

	; check L or number
	
	cp	'L'
	JP	Z,LST_PRG	;JR	Z,LST_PRG	; list append program

	; check number

	call	GET_NUM		; return BC : number
	JP	C,ERR		;JR	C, ERR

	; check number
	LD	A, C
	CP	(lanch1 - ApendTBL)/2 +1	; a < 17 ?
	JP	NC, ERR
	OR	A				; check '0'
	JP	Z, ERR

	; A <- append program No.

	DEC	A	; 0 <= A <= 0FH
	RLA		; A = A * 2
	LD	D, 0
	LD	E, A
	LD	HL, ApendTBL
	ADD	HL, DE	; get lanch address point
	LD	E, (HL)
	INC	HL
	LD	D, (HL)	; DE : JUMP POINT

	; Jump to append program
	
	EX	DE, HL
	LD	E, (HL)
	INC	HL
	LD	D, (HL)	; DE : jump address
	LD	A, D
	OR	E	; address check
	JP	Z,ERR		;JR	Z,ERR	; jump address NULL
	EX	DE, HL
	JP	(HL)	; lanch append program
	
	; List out append program
LST_PRG:
	; get address to DE
	LD	HL, ApendTBL

LST_PRG1:
	LD	E, (HL)
	INC	HL
	LD	D, (HL)
	INC	HL

	PUSH	HL			;#### ADDED ####################
	PUSH	DE
	POP	HL			;Z80 CODE POP	IX #############; IX <-DE
	LD	A,(HL)			;Z80 CODE LD	A, (IX+0) ######
	INC	HL			;###############################
	OR	(HL)			;Z80 CODE OR	(IX+1) #########
	POP	HL			;#### ADDED ####################
	JP	Z,LST_END		;JR	Z, LST_END
	INC	DE
	INC	DE	; get string address
	
	EX	DE, HL
	CALL	STROUT
	EX	DE, HL
	JP	lst_PRG1		;JR	LST_PRG1

LST_END:
	JP	WSTART
	
; Append program launch table
;
ApendTBL:
	dw	lanch1
	dw	lanch2
	dw	lanch3
	dw	lanch4
	dw	lanch5
	dw	lanch6
	dw	lanch7
	dw	lanch8
	dw	lanch9
	dw	lanch10
	dw	lanch11
	dw	lanch12
	dw	lanch13
	dw	lanch14
	dw	lanch15
	dw	lanch16
	
lanch1:
	dw	BASIC_CST
	DB	"1. GBASIC Start",CR,LF,00H
lanch2:
	dw	BASIC_WST
	DB	"2. GBASIC Restart",CR,LF,00H

lanch3:
	dw	PATB_CST
	DB	"3. Palo Alto Tiny BASIC Start",CR,LF,00H
		
lanch4:
	dw	PATB_WST
	DB	"4. Palo Alto Tiny BASIC Restart",CR,LF,00H

lanch5:
	dw	GM80_CST
	DB	"5. GAME80 Start",CR,LF,00H
		
lanch6:
	dw	GM80_WST
	DB	"6. GAME80 Restart",CR,LF,00H

lanch7:
lanch8:
lanch9:
lanch10:
lanch11:
lanch12:
lanch13:
lanch14:
lanch15:
lanch16:
	dw	0

GET_dNUM:
	push	de
	push	hl
	call	GET_NUM
	pop	hl
	pop	de
	ret

	; get number
	; input HL : string buffer
	;
	; Return
	; CF =1 : Error
	; BC: Calculation result

GET_NUM:
	XOR	A		; Initialize C
	LD	B, A
	LD	C, A		; clear BC
	
GET_NUM0:
	CALL	SKIPSP		; A <- next char
	OR	A
	RET	Z		; ZF=1, ok! buffer end

	CALL	GET_BI
	RET	C

	push	af
	LD	(alt_A),A		;Z80 CODE ;AF <> AF' ##########; save A
	pop	af
	CALL	MUL_10		; BC = BC * 10
	RET	C		; overflow error
	LD	A,(alt_A)		;Z80 CODE ;AF <> AF' ##########; restor A

	push	hl
	ld	d, 0
	ld	e, a

	ld	h, b
	ld	l, c
	add	hl, de
	ld	b, h
	ld	c, l		; result: BC = BC * 10 + A
	pop	hl
	RET	C		; overflow error
				; result: BC = BC * 10 + A
	INC	HL
	JP	GET_NUM0	;JR	GET_NUM0
;
; Make binary to A
; ERROR if CF=1
;
GET_BI:
	OR	A
	JP	Z,UP_BI		;JR	Z, UP_BI
	CP	'0'
	RET	C
	
	CP	'9'+1	; ASCII':'
	JP	NC,UP_BI	;JR	NC, UP_BI
	SUB	'0'	; Make binary to A
	RET

UP_BI:
	SCF		; Set CF
	RET

;
; multiply by 10
; BC = BC * 10
MUL_10:
	push	hl
	push	de			;#### added #################
	ld	d,a			;#### do not use push af ####

	push	bc
	LD	A,C			;Z80 CODE SLA	C ##############
	ADD	A,C			;###############################
	LD	C,A			;###############################
	LD	A,B			;Z80 CODE RL	B; 2BC #########
	ADC	A,B			;###############################
	LD	B,A			;###############################
	LD	A,C			;Z80 CODE SLA	C ##############
	ADD	A,C			;###############################
	LD	C,A			;###############################
	LD	A,B			;Z80 CODE RL	B; 4BC #########
	ADC	A,B			;###############################
	LD	B,A			;###############################
	pop	hl		; hl = bc
	add	hl, bc
	push	hl
	pop	bc		; 5BC
	LD	A,C			;Z80 CODE SLA	C ##############
	ADD	A,C			;###############################
	LD	C,A			;###############################
	LD	A,B			;Z80 CODE RL	B; 10BC ########
	ADC	A,B			;###############################
	LD	B,A			;###############################
	ld	a,d			;#### do not use pop af ########
	pop	de			;#### added ####################

	pop	hl
	RET			; result : BC = BC * 10
;
; list break point
;
b_list:
	ld	a, (bpt1_f)
	or	a
	jp	z,b_list1		;jr	z, b_list1

	ld	hl, (bpt1_adr)
	ld	a, '1'
	call	b_msg_out

b_list1:
	ld	a, (bpt2_f)
	or	a
	jp	z, WSTART

	ld	hl, (bpt2_adr)
	ld	a, '2'
	call	b_msg_out
	JP	WSTART


;;; 
;;; break point command
;;; 
brk_cmd:
	INC	HL
	CALL	SKIPSP		; A <- next char
	OR	A
	jp	z,b_list	;JR	Z,b_list	; only type "B"

	ld	bc, bpt1_f
	cp	'1'
	jp	z,set_bp1	;jr	z, set_bp1

	ld	bc, bpt2_f
	cp	'2'
	jp	z,set_bp1	;jr	z, set_bp1

	cp	'C'	;clear?
	jp	z,bp_clr	;jr	z, bp_clr
	jp	ERR

set_bp1:
	PUSH	HL			;#### ADDED ####################
	PUSH	AF			;Z80 CODE EX	AF,AF' #########
	LD	HL,(alt_AF)		;###############################
	PUSH	HL			;###############################
	POP	AF			;###############################
	POP	HL			;###############################
	LD	(alt_AF),HL		;###############################
	POP	HL			;###############################

	INC	HL
	CALL	SKIPSP		; A <- next char
	OR	A
	jp	z,bp_LOT	;JR	Z, bp_LOT 	;; No address input -> list out
	cp	','
	jp	nz, ERR

	INC	HL
	CALL	SKIPSP
	push	bc
	CALL	RDHEX		; 1st arg.
	pop	hl		; hl <- bc
	jp	c, ERR

	call	setbpadr
	jp	c, ERR
	JP	WSTART


; hl : point of bp flag( bpt1_f or bpt2_f)
; de : break point address

; check ram area, and set berak point
; 
setbpadr:
	IF	Fullram
	ELSE
	ld	a, d
	cp	RAM_B >> 8	; a - 0C0H
	jp	c, chkERR	; ROM area
	cp	IO_B >> 8	; a - 0F0H
	jp	nc, chkERR	; I/O area
	ENDIF
	ld	a,1
	ld	(hl), a	; set flag
	inc	hl
	ld	a, (de)		; get opcode
	ld	(hl), a		; save opcode
	inc	hl
	ld	(hl), e ; set break point low address
	inc	hl
	ld	(hl), d ; set break point high address
	or	a	; reset carry
	ret

chkERR:
	scf	;set carry
	ret

; clear break point

bp_clr:
	INC	HL
	CALL	SKIPSP		; A <- next char
	OR	A
	jp	z,b_aclr	;JR	Z,b_aclr	; all clear

	ld	bc, bpt1_f
	cp	'1'
	jp	z,bp_clr1	;jr	z, bp_clr1

	ld	bc, bpt2_f
	cp	'2'
	jp	nz, ERR

bp_clr1:
	xor	a
	ld	(bc), a
	JP	WSTART

b_aclr:
	xor	a
	ld	bc, bpt1_f
	ld	(bc), a
	ld	bc, bpt2_f
	ld	(bc), a
	JP	WSTART

; when no address input. list out BP
;
; bc : break pointer buffer offset
bp_LOT:
	ld	a, (bc)		; set break point?
	or	a
	jp	z, WSTART	; no break point setting

	PUSH	AF			;Z80 CODE EX	AF,AF' #########
	LD	HL,(alt_AF)		;## HL destroyed it's OK here ##
	PUSH	HL			;###############################
	POP	AF			;###############################
	POP	HL			;###############################
	LD	(alt_AF),HL		;###############################
	ld	hl, (bpt1_adr)
	cp	'1'
	jp	z,l_b2			;jr	z, l_b2
	ld	hl, (bpt2_adr)
l_b2:
	call	b_msg_out
	JP	WSTART

bp_msg1:	db	"BP(",00H
bp_msg2:	db	"):",00H

b_msg_out:
	push	hl
	push	af
	ld	hl, bp_msg1
	call	STROUT
	pop	af
	call	CONOUT
	ld	hl, bp_msg2
	call	STROUT
	pop	hl
	call	HEXOUT4
	call	CRLF	
	ret

;;; 
;;; trace command
;;; 
trace_cmd:

; T[address][,step”]
; TP[ON | OFF]
; TM[I | S]

; init steps
	push	hl
	ld	l, 0
	ld	h, 0
	ld	(TC_cnt), hl	; clear trace step counter to 0
	ld	hl,(REGPC)
	ld	(tmpT), hl	; init temp address
	xor	a
	ld	(fever_t), a	; clear flag trace forever

	pop	hl
	inc	hl
	CALL	SKIPSP
	CALL	RDHEX		; 1st arg.
	jp	nc, tadr_chk
	CALL	SKIPSP
	ld	a, (hl)
	or	a
	jp	z, t_op1	; only 1 step trace, check opcode
	cp	','
	jp	z, stp_chk	; steps check

	cp	'P'
	jp	z,tp_cmd	;jr	z, tp_cmd
	cp	'M'
	jp	nz, ERR
	
	; tm_cmd
	
	inc	hl
	CALL	SKIPSP		; A <- next char
dip_TM:
	ld	hl, tm_msg_i
	cp	'I'
	jp	z,set_TM	;jr	z, set_TM
	ld	hl, tm_msg_s
	cp	'S'
	jp	z,set_TM	;jr	z, set_TM
	or	a
	jp	nz, ERR

;display T mode
	ld	a, (TM_mode)
	jp	dip_TM		;jr	dip_TM

;set TM mode and display
set_TM:
	ld	(TM_mode),a
	push	hl
	ld	hl, tm_msg_0
	call	STROUT
	pop	hl
	call	STROUT
	jp	WSTART

tm_msg_0:
	db	"TM mode:<CALL ", 00h
tm_msg_i:
	db	"IN>", CR, LF, 00h
tm_msg_s:
	db	"SKIP>", CR, LF, 00h
	
	
	; tp_cmd
tp_cmd:	
	inc	hl
	CALL	SKIPSP		; A <- next char
	or	a
	jp	nz,tp_n1	;jr	nz, tp_n1
	ld	a, (TP_mode)
	jp	tp_n2		;jr	tp_n2
	
tp_n1:
	cp	'O'
	jp	nz, ERR
	inc	hl
	CALL	SKIPSP		; A <- next char

tp_n2:
	ld	hl, tp_msg_on
	cp	'N'
	jp	z,tp_md_on	;jr	z, tp_md_on

	ld	hl, tp_msg_off
	cp	'F'
	jp	nz, ERR

tp_md_on:
	; set trace mode and display mode
	ld	(TP_mode), a
	push	hl
	ld	hl, tp_msg_0
	call	STROUT
	pop	hl
	call	STROUT
	jp	WSTART

tp_msg_0:
	db	"TP mode: ", 00h
tp_msg_on:
	db	"ON", CR, LF, 00h
tp_msg_off:
	db	"OFF", CR, LF, 00h

tadr_chk:
	EX	DE,HL			;Z80 CODE ld	(tmpT), de #####; set start address
	LD	(tmpT),HL		;###############################
	EX	DE,HL			;###############################
	CALL	SKIPSP		; A <- next char
	or	a
	jp	z,t_op1		;jr	z, t_op1	; 1step trace
	cp	','
	jp	z,stp_chk	;jr	z, stp_chk
	jp	ERR

stp_chk:
	inc	hl
	CALL	SKIPSP		; A <- next char
	or	a
	jp	z, ERR
	cp	'-'
	jp	z,chk_fevre	;jr	z, chk_fevre

; check steps

	call	GET_NUM		; get steps to BC
	JP	C, ERR		; number error

t_op11:
	LD	H,B			;Z80 CODE ld	(TC_cnt), bc	; set trace step counter
	LD	L,C			;## HL destroyed it's ok here ##
	LD	(TC_cnt),HL		;###############################
	jp	t_op_chk

t_op1:
	ld	bc, 1
	jp	t_op11		;jr	t_op11

chk_fevre:
	inc	hl
	CALL	SKIPSP		; A <- next char
	cp	'1'
	jp	nz, ERR
	inc	hl
	CALL	SKIPSP		; A <- next char
	or	a
	jp	nz, ERR		; not "-1" then error
	ld	a, 1
	ld	(fever_t), a	; set flag trace forever

t_op_chk:
	ld	hl, (tmpT)	; get PC address
	ld	a, (hl)		; get opcode
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; branch opecode check
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2 insertion Trace code(TC) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
INSTC2:
	; check 1 byte machine code: branch (RET CC)

	ld	bc, RETCC_TBLE - RETCC_TBLS
	ld	hl, RETCC_TBLS
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz,next_bc1		;jr	nz, next_bc1	; not RET CC

	; RET CC
	; trace operation:
	;   1. ea = *REGSP; *ea = TC;
	;   2. ea = *REGPC; *(ea+1) = TC;

	; 1
	ld	c, 1		; first TC point
	call	insBRK_sp
	jp	c, err_trace_seq
	
	; 2
	ld	c, 2		; second TC point
	call	insBRK_1op
	jp	c, err_trace_seq

	jp	END_INS_TC

	; check 2 byte machine code: branch (JR CC, Relative Value)
next_bc1:

	ld	bc, JRCC_TBLE - JRCC_TBLS
	ld	hl, JRCC_TBLS
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz,next_bc2		;jr	nz, next_bc2	; not JR CC

	; JR CC, nn
	; trace operation:
	;   1. ea = *REGPC; *(ea + 2 + *(ea+1)) = TC;
	;   2. ea = *REGPC; *(ea+2) = TC;

	; 1
	ld	c, 1		; first TC point
	ld	hl, (tmpT)
	call	Rel_adr_c
	call	inadr_chk_and_wrt
	jp	c, err_trace_seq

	; 2
	ld	c, 2		; second TC point
	call	insBRK_2op
	jp	c, err_trace_seq

	jp	END_INS_TC

	; check 3 byte machine code: branch JP CC, nnnn 16bit literal)

next_bc2:
	ld	bc, JPCC_TBLE - JPCC_TBLS
	ld	hl, JPCC_TBLS
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz,next_bc21		;jr	nz, next_bc21		; not JP CCC

	; JP CC, nnnn
	; trace operation:
	;   1. ea = *REGPC; *((short *)(ea+1)) = TC;
	;   2. ea = *REGPC; *(ea+3) = TC;

	ld	c, 1		; first TC point
	jp	next_bc222		;jr	next_bc222

	; check 3 byte machine code: branch (CALL CC, nnnn 16bit literal)

next_bc21:
	ld	bc, CLCC_TBLE - CLCC_TBLS
	ld	hl, CLCC_TBLS
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz,INSTC1		;jr	nz, INSTC1		; not CALL CCC

	; CALL CC, nnnn
	; trace operation:
	; TM_mode = 'I'
	;   1. ea = *REGPC; *((short *)(ea+1)) = TC;
	;   2. ea = *REGPC; *(ea+3) = TC;
	;
	; TM_mode = 'S'
	;   2. ea = *REGPC; *(ea+3) = TC;

	ld	c, 1		; first TC point
	ld	a, (TM_mode)
	cp	'S'
	jp	z,next_bc22	;jr	z, next_bc22	; skip insertion 1.

next_bc222:
	; 1. ea = *REGPC; *((char *)(ea+1)) = TC;
	call	insBRK_brp
	jp	c, err_trace_seq
	
next_bc221:
	; 2. ea = *REGPC; *(ea+3) = TC;
	ld	c, 2		; second TC point
next_bc22:
	call	insBRK_3op
	jp	c, err_trace_seq

	jp	END_INS_TC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1 insertion Trace code(TC) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
INSTC1:
	; check 1 byte machine code: branch (return)
	cp	0C9H		; RET ?
	jp	nz,next_bc3		;jr	nz, next_bc3	; not RET

	; RET
	; trace operation:
	;   ea = *REGSP; *ea = TC;
	ld	c, 1		; first TC point
	call	insBRK_sp
	jp	c, err_trace_seq
	jp	END_INS_TC

	; check RST p
next_bc3:
	ld	bc, RST_TBLE - RST_TBLS
	ld	hl, RST_TBLS
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz,next_bc4		;jr	nz, next_bc4		; not RST p

	; RST p
	; can't trace: skip trace
	; trace operation:
	;   ea = *REGPC; *(ea+1) = TC;
;	ld	hl, RST_DMSG
;	call	STROUT		; message out "DETECT RST OPCODE"
	ld	c, 1		; first TC point
	call	insBRK_1op
	jp	c, err_trace_seq
	
	jp	END_INS_TC

;RST_DMSG:
;	db	"(RST nn) WILL BE SKIPPED. AND TRACE NEXT OPCODE", CR, LF,00

	; check code 0EDH
next_bc4:
	cp	0EDH		; CODE 0EDH ?
	jp	nz,next_bc5	;jr	nz, next_bc5	; not 0EDH

	inc	hl
	ld	a, (hl)
	cp	45H		; RETN?
	jp	z,next_bc6	;jr	z, next_bc6	; yes, RETN
	cp	4DH		; RETI?
	jp	nz,next_bc5	;jr	nz, next_bc5	; not RETN

	; trace operation:
	;   ea = *REGSP; *ea = TC;
next_bc6:
	ld	c, 1		; first TC point
	call	insBRK_sp
	jp	c,err_trace_seq	;jr	c, err_trace_seq
	jp	END_INS_TC	;jr	END_INS_TC

	; check JP (HL)
next_bc5:
	ld	hl, (tmpT)
	ld	a, (hl)

	cp	0E9H		; JP (HL) ?
	jp	nz,next_bc7	;jr	nz, next_bc7	; not JP (HL)

	; JP (HL)
	; trace operation:
	;   ea = *REGHL; *ea = TC;
	ld	hl, (REGHL)
djphl:
	ld	c, 1		; first TC point
	call	inadr_chk_and_wrt
	jp	c,err_trace_seq	;jr	c, err_trace_seq
	jp	END_INS_TC	;jr	END_INS_TC

	; check JP (IX)
next_bc7:
	cp	0DDH		; 1st OPOCDE 0DDH ?
	jp	nz,next_bc8	;jr	nz, next_bc8	; no 0DDH
	inc	hl
	ld	a, (hl)
	cp	0E9H		; JP (IX) ?
	jp	nz,next_bc8	;jr	nz, next_bc8	; not JP (IX)

	; JP (IX)
	; trace operation:
	;   ea = *REGIX; *ea = TC;
	ld	hl, (REGIX)
	jp	djphl		;jr	djphl

	; check JP (IY)
next_bc8:
	ld	hl, (tmpT)
	ld	a, (hl)

	cp	0FDH		; 1st OPOCDE 0FDH ?
	jp	nz,next_bc9	;jr	nz, next_bc9	; no 0FDH
	inc	hl
	ld	a, (hl)
	cp	0E9H		; JP (IX) ?
	jp	nz,next_bc9	;jr	nz, next_bc9	; not JP (IX)

	; JP (IY)
	; trace operation:
	;   ea = *REGIY; *ea = TC;
	ld	hl, (REGIY)
	jp	djphl		;jr	djphl

	; check JR relative
next_bc9:
	ld	hl, (tmpT)
	ld	a, (hl)
	cp	18H		; JR relative ?
	jp	nz,next_bc10	;jr	nz, next_bc10	; not JR relative

	; JR Relative
	; trace operation:
	;   ea = *REGPC; *(ea + 2 + *(ea+1)) = TC;
	ld	c, 1		; first TC point
	call	Rel_adr_c
	call	inadr_chk_and_wrt
	jp	c,err_trace_seq	;jr	c, err_trace_seq
	jp	END_INS_TC	;jr	END_INS_TC

	; check JP literal
next_bc10:
	cp	0C3H		; JP literal ?
	jp	nz,next_bc11	;jr	nz, next_bc11	; not JP literal

	; JP literal
	; trace operation:
	; ea = *REGPC; *((char *)(ea+1)) = TC;
	ld	c, 1		; first TC point
	call	insBRK_brp
	jp	c,err_trace_seq	;jr	c, err_trace_seq
	jp	END_INS_TC	;jr	END_INS_TC

	; check call literal
next_bc11:
	cp	0CDH		; CALL literal ?
	jp	nz, INS2	; no, check not branch opcode 

	; CALL literal
	; trace operation:
	; TM_mode = 'I'
	;   ea = *REGPC; *((short *)(ea+1)) = TC;
	; TM_mode = 'S'
	;   2. ea = *REGPC; *(ea+3) = TC;

	ld	c, 1		; first TC point
	ld	a, (TM_mode)
	cp	'S'
	jp	z,next_bc11	;jr	z, next_bc111	; yes, TM_mode='S'

	; TM_mode = 'I'
	; ea = *REGPC; *((char *)(ea+1)) = TC;
	call	insBRK_brp
	jp	c,err_trace_seq	;jr	c, err_trace_seq
	jp	END_INS_TC	;jr	END_INS_TC

	; TM_mode = 'S'
	; ea = *REGPC; *(ea+3) = TC;
next_bc111:
	call	insBRK_3op
	jp	c,err_trace_seq	;jr	c, err_trace_seq

END_INS_TC:
	jp	G0	; go, trace operation

err_trace_seq:
	ld	hl, terr_msg
	call	STROUT
	JP	WSTART
	rst	38h
;	
terr_msg:	db	"Adr ERR", CR, LF, 00H

;--------------------------------------
; 2 byte machine code branch
; - 2nd byte is Relative address
; - input hl : opecode address
; - output hl : target address
;--------------------------------------
Rel_adr_c:
	inc	hl
	ld	e, (hl)		; e = 2nd operand
	inc	hl		; hl = PC + 2
	ld	d, 0ffh
	push	bc 			;Z80 CODE bit	7, e		; test msb bit
	ld	b,a 			;## DO NOT USE PUSH AF #########
	ld	a,e 			;###############################
	and	80h 			;###############################
	ld	a,b 			;###############################
	pop	bc; 			;## DO NOT USE POP AF ##########
	jp	nz,exp_msb	;jr	nz, exp_msb	; 
	ld	d, 0
exp_msb:
	add	hl, de
	ret

;--------------------------------------
; 1 byte op code, insert TC
; ea = *REGPC; *(ea+1) = TC
;--------------------------------------
insBRK_1op:
;	ld	hl, (REGPC)
	ld	hl, (tmpT)
	jp	ib1			;jr	ib1

;--------------------------------------
; 2 byte op code, insert TC
; ea = *REGPC; *(ea+2) = TC
;--------------------------------------
insBRK_2op:
;	ld	hl, (REGPC)
	ld	hl, (tmpT)
	jp	ib2			;jr	ib2

;--------------------------------------
; 3 byte op code, insert TC
; ea = *REGPC; *(ea+3) = TC;
;--------------------------------------
insBRK_3op:
;	ld	hl, (REGPC)
	ld	hl, (tmpT)
ib3:
	inc	hl
ib2:
	inc	hl
ib1:
	inc	hl
	call	inadr_chk_and_wrt
	ret

;--------------------------------------
; 3 byte op code, insert TC in branch point
; ea = *REGPC; *((char *)(ea+1)) = TC;
;--------------------------------------
insBRK_brp:
;	ld	hl, (REGPC)
	ld	hl, (tmpT)
	inc	hl
	ld	e, (hl)
	inc	hl
	ld	d, (hl)
	ex	de,  hl
	call	inadr_chk_and_wrt
	ret

;--------------------------------------
; insert TC at SP
; ea = *REGSP; *ea = TC;
;--------------------------------------
insBRK_sp:
	EX	HL,DE			;Z80 CODE ld	de, (REGSP) ####
	LD	HL,(REGSP)		;###############################
	EX	HL,DE			;###############################
	ld	a, (de)
	ld	l, a
	inc	de
	ld	a, (de)
	ld	h, a		; hl = *sp
	call	inadr_chk_and_wrt
	ret

;--------------------------------------
; check (HL) is RAM AREA
; insert Trace code at (HL)
;--------------------------------------
inadr_chk_and_wrt:
	ld	a, h
	IF	Fullram
	ELSE
	cp	RAM_B >> 8		; 80H
	jp	c,NO_RAM_AREA	;jr	c, NO_RAM_AREA
	cp	RAM_E >> 8		; 8FH
	jp	nc,NO_RAM_AREA		;jr	nc, NO_RAM_AREA
	ENDIF
	ld	a, c
	cp	1		;first save?
	jp	nz,icka1	;jr	nz, icka1
	ld	de, tpt1_f
	ld	(de), a		; set trace ON
	ld	(tpt1_adr), hl
	ld	a, (hl)		; get opcode
	ld	(tpt1_op), a	; save opcode
	jp	icka_end	;jr	icka_end
icka1:
	ld	de, tpt2_f
	ld	(de), a		; set trace ON
	ld	(tpt2_adr), hl
	ld	a, (hl)		; get opcode
	ld	(tpt2_op), a	; save opcode
icka_end:
	xor	a
	ret

NO_RAM_AREA:
	SCF
	ret


;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2 insertion TC TABLE
;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; 1 byte machine code: branch (RET CC)
RETCC_TBLS:
	DB	0C0H	; RET	NZ
	DB	0C8H	; RET	Z
	DB	0D0H	; RET	NC
	DB	0D8H	; RET	C
	DB	0E0H	; RET	PO
	DB	0E8H	; RET	PE
	DB	0F0H	; RET	P
	DB	0F8H	; RET	M
RETCC_TBLE:

; 2 byte machine code: branch (JR CC, Relative)
JRCC_TBLS:
	DB	10H	; DJNZ	$
JRCC_TBLS1:
	DB	20H	; JR	NZ,$
	DB	28H	; JR	Z,$
	DB	30H	; JR	NC,$
	DB	38H	; JR	C,$
JRCC_TBLE:

; 3 byte machine code: branch (JP CC, 16bit literal)
JPCC_TBLS:
	DB	0C2H	; JP	NZ,1234H
	DB	0CAH	; JP	Z,1234H
	DB	0D2H	; JP	NC,1234H
	DB	0DAH	; JP	C,1234H
	DB	0E2H	; JP	PO,1234H
	DB	0EAH	; JP	PE,1234H
	DB	0F2H	; JP	P,1234H
	DB	0FAH	; JP	M,1234H
JPCC_TBLE:

; (call 16bit literal)
CLCC_TBLS:
	DB	0C4H	; CALL	NZ,1234H
	DB	0CCH	; CALL	Z,1234H
	DB	0D4H	; CALL	NC,1234H
	DB	0DCH	; CALL	C,1234H
	DB	0E4H	; CALL	PO,1234H
	DB	0ECH	; CALL	PE,1234H
	DB	0F4H	; CALL	P,1234H
	DB	0FCH	; CALL	M,1234H
CLCC_TBLE:

;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1 insertion TC TABLE
;;;;;;;;;;;;;;;;;;;;;;;;;;

; restart
RST_TBLS:
	DB	0C7H	; RST	00H
	DB	0CFH	; RST	08H
	DB	0D7H	; RST	10H
	DB	0DFH	; RST	18H
	DB	0E7H	; RST	20H
	DB	0EFH	; RST	28H
	DB	0F7H	; RST	30H
	DB	0FFH	; RST	38H
RST_TBLE:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; machine code check(except branch)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
INS2:

	; 2byte machine code search
	ld	bc, TWO_OPTBL_E - TWO_OPTBL
	ld	hl, TWO_OPTBL
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	z, meet_op2

	; 3byte machine code search
	ld	bc, THREE_OPTBL_E - THREE_OPTBL
	ld	hl, THREE_OPTBL
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	z, meet_op3

	; check 0CBH
	
	; readjust hl
	ld	hl, (tmpT)

	cp	0CBH		; opecode 0CBH?
	jp	z,meet_op2	;jr	z, meet_op2
	
	; check 0DDH
	cp	0ddh		; opecode 0DDH?
	jp	z,meet_dd	;jr	z, meet_dd
	
	; check 0EDH
	cp	0edh		; opecode 0EDH?
	jp	z,meet_ed	;jr	z, meet_ed

	; check 0FDH
	cp	0fdh		; opecode 0FDH?
	jp	z,meet_dd	;jr	z, meet_dd

	; 1byte machine code
	jp	meet_op1	;jr	meet_op1

	; opecode 0DDh
meet_dd:
	inc	hl
	ld	a, (hl)	
	cp	0cbh		; 2nd 0CBH?
	jp	z,meet_op4	;jr	z, meet_op4
	cp	21h		; 2nd 21H?
	jp	z,meet_op4	;jr	z, meet_op4
	cp	22h		; 2nd 22H?
	jp	z,meet_op4	;jr	z, meet_op4
	cp	2ah		; 2nd 2AH?
	jp	z,meet_op4	;jr	z, meet_op4
	cp	36h		; 2nd 36H?
	jp	z,meet_op4	;jr	z, meet_op4

	; 2nd code search
	ld	bc, DD_2NDTBL_E - DD_2NDTBL
	ld	hl, DD_2NDTBL
	CALL	CPIR80		;Z80 CODE cpir #################
	jp	z,meet_op2	;jr	z, meet_op2
	jp	meet_op3	;jr	meet_op3

meet_ed:
	inc	hl
	ld	a, (hl)	
	cp	43h		; 2nd 43H?
	jp	z,meet_op4	;jr	z, meet_op4
	cp	4bh		; 2nd 4BH?
	jp	z,meet_op4	;jr	z, meet_op4
	cp	53h		; 2nd 53H?
	jp	z,meet_op4	;jr	z, meet_op4
	cp	5bh		; 2nd 5BH?
	jp	z,meet_op4	;jr	z, meet_op4
	cp	73h		; 2nd 73H?
	jp	z,meet_op4	;jr	z, meet_op4
	cp	7bh		; 2nd 7BH?
	jp	z,meet_op4	;jr	z, meet_op4
	jp	meet_op2	;jr	meet_op2

; 1 machine code
meet_op1:
	ld	c, 1
	call	insBRK_1op
	jp	c, err_trace_seq
	jp	END_INS_TC

; 2 machine code
meet_op2:
	ld	c, 1
	call	insBRK_2op
	jp	c, err_trace_seq
	jp	END_INS_TC
; 3 machine code
meet_op3:
	ld	c, 1
	call	insBRK_3op
	jp	c, err_trace_seq
	jp	END_INS_TC

; 4 machine codee
meet_op4:
	ld	c, 1
;	ld	hl, (REGPC)
	ld	hl, (tmpT)
	inc	hl
	call	ib3
	jp	c, err_trace_seq
	jp	END_INS_TC

TWO_OPTBL:	; second byte is 8bitliteral[nn]
ld_r_nn_s:
	DB	06h	; LD	B,nn
	DB	0Eh	; LD	C,nn
	DB	16h	; LD	D,nn
	DB	1Eh	; LD	E,nn
	DB	26h	; LD	H,nn
	DB	2Eh	; LD	L,nn
	DB	36h	; LD	(HL),nn
	DB	3Eh	; LD	A,nn
ld_r_nn_e:

log_op_s:
	DB	0C6h	; ADD	A,nn
	DB	0CEh	; ADC	A,nn
	DB	0DEh	; SBC	A,nn
	DB	0D6h	; SUB	nn
	DB	0E6h	; AND	nn
	DB	0EEh	; XOR	nn
	DB	0F6h	; OR	nn
	DB	0FEh	; CP	nn
	DB	0DBh	; IN	A,(nn)
	DB	0D3h	; OUT	(nn),A
log_op_e:
TWO_OPTBL_E:

THREE_OPTBL:	; 2nd, 3rd byte is 16bitliteral[nnnn]
	DB	01h	; LD	BC,nnnn
	DB	11h	; LD	DE,nnnn
	DB	21h	; LD	HL,nnnn
	DB	31h	; LD	SP,nnnn
THREE_OPTBLe
	DB	22h	; LD	(nnnn),HL
	DB	32h	; LD	(nnnn),A

	DB	2Ah	; LD	HL,(nnnn)
	DB	3Ah	; LD	A,(nnnn)
THREE_OPTBL_E:

DD_2NDTBL:
	DB	09h	; ADD	IX,BC
	DB	19h	; ADD	IX,DE
	DB	29h	; ADD	IX,IX
	DB	39h	; ADD	IX,SP
DD_2NDTBL1:
	DB	23h	; INC	IX
	DB	2Bh	; DEC	IX
	DB	0E5h	; PUSH	IX
	DB	0E1h	; POP	IX
DD_2NDTBL2:
	DB	0E3h	; EX	(SP),IX
	DB	0F9h	; LD	SP,IX
DD_2NDTBL_E:

;;; 
;;; Dump memory
;;; 

DUMP:
	INC	HL
	LD	A,(HL)
	cp	'I'
	JP	Z,disassemble
	CALL	SKIPSP
	CALL	RDHEX		; 1st arg.
	jp	c, DP0		;jr	c, DP0
	;; 1st arg. found
	EX	HL,DE			;Z80 CODE LD	(DSADDR),DE ####
	LD	(DSADDR),HL		;###############################
	EX	HL,DE			;###############################
	jp	dp00			;jr	dp00

DP0:	;; No arg. chk

	push	hl
	LD	HL,(DSADDR)
	LD	BC,128
	ADD	HL,BC
	LD	(DEADDR),HL
	pop	hl
	LD	A,(HL)
	OR	A
	jp	z,DPM_C		;JR	z, DPM_C	; no arg.

dp00:
	CALL	SKIPSP
	LD	A,(HL)
	CP	','
	jp	z,DP1		;JR	Z,DP1
	OR	A
	jp	nz,ERR		;JP	NZ,ERR

	;; No 2nd arg.

	LD	HL,128
	ADD	HL,DE
	LD	(DEADDR),HL
	jp	DPM_C		;JR	DPM_C

DP1:
	INC	HL
	CALL	SKIPSP
	CALL	RDHEX
	jp	c, ERR
	CALL	SKIPSP
	OR	A
	jp	nz, ERR
	INC	DE
	EX	HL,DE			;Z80 CODE LD	(DEADDR),DE ####
	LD	(DEADDR),HL		;###############################
	EX	HL,DE			;###############################
DPM_C:
	CALL	DPM
	JP	WSTART

	;; DUMP main
DPM:
	LD	HL,(DSADDR)
	LD	A,0F0H
	AND	L
	LD	L,A
	XOR	A
	LD	(DSTATE),A
DPM0:
	PUSH	HL
	CALL	DPL
	POP	HL
	LD	BC,16
	ADD	HL,BC
	CALL	CONST
	JP	NZ,DPM1		;JR	NZ,DPM1
	LD	A,(DSTATE)
	CP	2
	JP	C,DPM0		;JR	C,DPM0
	LD	HL,(DEADDR)
	LD	(DSADDR),HL
	RET
DPM1:
	LD	(DSADDR),HL
	JP	CONIN

DPL:
	;; DUMP line
	CALL	HEXOUT4
	PUSH	HL
	LD	HL,DSEP0
	CALL	STROUT
	POP	HL
	LD	DE,INBUF		;Z80 CODE LD	IX,INBUF ######
	LD	B,16
DPL0:
	CALL	DPB
	DEC	B			;Z80 CODE #####	DJNZ	DPL0
	JP	NZ,DPL0			;##############################

	LD	HL,DSEP1
	CALL	STROUT

	LD	HL,INBUF
	LD	B,16
DPL1:
	LD	A,(HL)
	INC	HL
	CP	' '
	JP	C,DPL2		;JR	C,DPL2
	CP	7FH
	JP	NC,DPL2		;JR	NC,DPL2
	CALL	CONOUT
	JP	DPL3		;JR	DPL3
DPL2:
	LD	A,'.'
	CALL	CONOUT
DPL3:
	DEC	B			;Z80 CODE #####	DJNZ	DPL1 ###
	JP	NZ,DPL1			;###############################
	JP	CRLF

DPB:	; Dump byte
	LD	A,' '
	CALL	CONOUT
	LD	A,(DSTATE)
	OR	A
	JP	NZ,DPB2		;JR	NZ,DPB2
	; Dump state 0
	LD	A,(DSADDR)	; Low byte
	CP	L
	JP	NZ,DPB0		;JR	NZ,DPB0
	LD	A,(DSADDR+1)	; High byte
	CP	H
	JP	Z,DPB1		;JR	Z,DPB1
DPB0:	; Still 0 or 2
	LD	A,' '
	CALL	CONOUT
	CALL	CONOUT
	LD	(DE),A			;Z80 CODE LD	(IX),A #########
	INC	HL
	INC	DE			;Z80 CODE INC	IX #############
	RET
DPB1:	; Found start address
	LD	A,1
	LD	(DSTATE),A
DPB2:
	LD	A,(DSTATE)
	CP	1
	JP	NZ,DPB0		;JR	NZ,DPB0
	; Dump state 1
	LD	A,(HL)
	LD	(DE),A			;Z80 CODE LD	(IX),A #########
	CALL	HEXOUT2
	INC	HL
	INC	DE			;Z80 CODE INC	IX #############
	LD	A,(DEADDR)	; Low byte
	CP	L
	RET	NZ
	LD	A,(DEADDR+1)	; High byte
	CP	H
	RET	NZ
	; Found end address
	LD	A,2
	LD	(DSTATE),A
	RET

;;;
;;; Disassemble
;;; 


; DI[<address>][,s<steps>|<end address>]

disassemble:
	INC	HL
	CALL	SKIPSP
	CALL	RDHEX		; 1st arg.
	jp	nc,get_DI1	;jr	nc, get_DI1

di_nxt:
	;; No arg. chk
	CALL	SKIPSP
	LD	A,(HL)
	OR	A
	jp	nz,chk_DI1	;jr	NZ, chk_DI1	; ',' check

; No arg
	ld	h, 0
	ld	l, 10
	ld	a, l
	ld	(dasm_stpf), a	; set step flag
	ld	(dasm_ed), hl	; set 10 steps
	jp	DISASM_go	;jr	DISASM_go

; 1st arg
get_DI1:
	EX	HL,DE			;Z80 CODE ld	(dasm_adr), de	; save start address
	LD	(dasm_adr),HL		;###############################
	EX	HL,DE			;###############################
;	INC	HL
	jp	di_nxt			;jr	di_nxt

chk_DI1:
	cp	','
	jp	nz, ERR

; check 2nd arg

	INC	HL
	CALL	SKIPSP
	cp	'S'
	jp	nz,chk_stpDI		;jr	nz, chk_stpDI

; step arg
	ld	a, 1
	ld	(dasm_stpf), a	; set step flag
	inc	hl
	call	GET_NUM		; get decimal number to binary
	jp	c, ERR
	PUSH	HL			;Z80 CODE ld	(dasm_ed), bc	; set steps
	LD	H,B			;###############################
	LD	L,C			;###############################
	LD	(dasm_ed),HL		;###############################
	POP	HL			;###############################
	jp	DISASM_go	;jr	DISASM_go

chk_stpDI:
	CALL	RDHEX		; 2nd arg.
	jp	c, ERR
	EX	HL,DE			;Z80 CODE ld	(dasm_ed), de	; set end address
	LD	(dasm_ed),HL		;###############################
	EX	HL,DE			;###############################
	xor	a
	ld	(dasm_stpf), a	; clear step flag

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; operation Disassemble
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DISASM_go:
	call	CONST
	jp	z,dis_go1	;jr	z, dis_go1
	call	CONIN		;discard key
	jp	WSTART		; exit disasm command

dis_go1:
	; get opcode
	call	dis_analysis
	call	mk_adr_str	; conout address and machine code
				; *dasm_adr is next opcode address
	ld	hl, adr_mc_buf
	call	STROUT		; conout disassemble strings

	ld	a, (dasm_stpf)
	or	a
	jp	nz,calc_dis_step	;jr	nz, calc_dis_step
	
	; *dasm_adr > *dasm_ed ?, yes, finish
	ld	HL, (dasm_adr)		;Z80 CODE ld	bc, (dasm_adr) #
	LD	B,H			;## HL destroyed it's ok here ##
	LD	C,L			;## HL destroyed it's ok here ##
	ld	hl, (dasm_ed)
	call	sbc_hlbc		;Z80 CODE sbc	hl, bc
	jp	nc,DISASM_go		;jr	nc, DISASM_go
	jp	WSTART

calc_dis_step:
	ld	hl, (dasm_ed)
	dec	hl
	ld	(dasm_ed), hl
	ld	a, h
	or	l
	jp	nz,DISASM_go	;jr	nz, DISASM_go
	jp	WSTART

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Disassemble and maked strings to user buffer
; input de : user buffer
;       hl : disassemble address
; output de : next MC address
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
get_disasm_st:
	ld	(dasm_adr), hl
	push	de
	call	dis_analysis
	call	mk_adr_str	; conout address and machine code
	pop	de
	ld	hl, adr_mc_buf
	ld	bc, dasm_be - dasm_bs
	PUSH	AF			;Z80 CODE ldir
LDIR1:	LD	A,(HL)			;###############################
	LD	(DE),A			;###############################
	INC	HL			;###############################
	INC	DE			;###############################
	DEC	BC			;###############################
	LD	A,C			;###############################
	OR	B			;###############################
	JP	NZ,LDIR1		;###############################
	POP	AF			;###############################
	EX	HL,DE			;Z80 CODE ld	de, (dasm_adr)
	LD	HL,(dasm_adr)		;###############################
	EX	HL,DE			;###############################
	ld	a, (mc_Size)
	ret

;-------------------------------------------------
; Make address and machine code at adr_mc_buf
; "XXXX XX XX XX XX " (17bytes)
;-------------------------------------------------
mk_adr_str:
	ld	hl, adr_mc_buf
	EX	HL,DE			;Z80 CODE ld	de, (dasm_adr)
	LD	HL,(dasm_adr)		;###############################
	EX	HL,DE			;###############################
	call	hex4str		; address XXXX
	call	ins_spcR
	call	ins_spcR

	ld	b, 4
	ld	a, (mc_Size)
	ld	c, a
mas_1:
	ld	a, (de)
	inc	de

	push	de
	ld	e, a
	call	hex2str		; MC XX
	call	ins_spcR
	pop	de

	dec	b
	jp	z,mas_3		;jr	z, mas_3	; end
	dec	c
	jp	nz,mas_1	;jr	nz, mas_1

mas_2:	
	call	ins_spcR
	call	ins_spcR
	call	ins_spcR
	dec	b
	jp	nz, mas_2
mas_3:
	call	ins_spcR
	EX	HL,DE			;Z80 CODE ld	(dasm_adr), de	; set next analysis address
	LD	(dasm_adr),HL		;###############################
	EX	HL,DE			;###############################
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; dis assemble analysis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

dis_analysis:

	; pre init string buffer

	ld	a, 1
	ld	(mc_Size), a

	ld	de, LDstr	; insert LD string
	call	mkopcstr

	ld	hl, dasm_OprStr
	call	insPostStr	; CR, LF, 00h

	ld	hl, (dasm_adr)
	ld	a, (hl)		; get opcode

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; analysys 1 byte MC
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;  check no operand

	ld	bc, oth_1op_e - oth_1op_s
	ld	hl, oth_1op_s
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz,chk_ld		;jr	nz, chk_ld

;  no operand

	ld	hl, oth_1op_tbl
	call	get_strBufpoint
	jp	mkopcstr

get_r_num:
	and	38H
	rrca
	rrca
	rrca
	ld	c, a
	ld	b, 0		; bc ; register No.
	ret
;
; LD opecode
;

; check LD "A,(BC)", "A,(DE)", "(BC),A", "(DE),A", "SP,HL"

chk_ld:
	cp	0AH	; LD	A,(BC)
	jp	z,ld_a_kbck		;jr	z, ld_a_kbck
	cp	1AH	; LD	A,(DE)
	jp	z,ld_a_kdek		;jr	z, ld_a_kdek
	cp	02H	; LD	(BC),A
	jp	z,ld_kbck_a		;jr	z, ld_kbck_a
	cp	12H	; LD	(DE),A
	jp	z,ld_kdek_a		;jr	z, ld_kdek_a
	cp	0F9H	; LD	SP,HL
	jp	nz,chk_ld1		;jr	nz, chk_ld1

; LD SP, HL
	ld	de, RNSP
	call	mk_rcs		; "SP, "
	jp	hl_crlf
	
; LD A, (BC)
ld_a_kbck:
	ld	de, RNBC
ld_a_kxxk:
	push	de
	call	a_colon_sp	; "A, "
	pop	de
	call	mk_krk		; "(BC)", "(DE)"
	jp	insPostStr	; CR, LF, 00h

; LD A, (DE)
ld_a_kdek:
	ld	de, RNDE
	jp	ld_a_kxxk	;jr	ld_a_kxxk

; LD (BC), A
ld_kbck_a:
	ld	de, RNBC

ld_kxxk_a:
	call	mk_krkcs	; "(BC), ", "(DE), "
	jp	a_crlf		; "A",CR,LF

; LD (DE), A
ld_kdek_a:
	ld	de, RNDE
	jp	ld_kxxk_a	;jr	ld_kxxk_a

; check other 1byte LD MC

chk_ld1:
	cp	40h
	jp	c, chk_inc
	cp	80h
	jp	nc,chk_add	;jr	nc, chk_add
	cp	76h	; HALT?
	jp	nz,LD1op	;jr	nz, LD1op

	ld	de, HLTstr	; HALT string
	jp	mkopcstr	; de : next string buffer addr

; LD

LD1op:
	call	get_r_num
	call	get_rstg_off
	ld	hl, dasm_OprStr
	call	mk_rcs		; "REG, "

mk_2ndopr:
	EX	HL,DE			;Z80 CODE ld	de, (dasm_adr)
	LD	HL,(dasm_adr)		;###############################
	EX	HL,DE			;###############################
	ld	a, (de)		; get opcode
	and	07h
	ld	c, a
	ld	b, 0

mk_2ndopr1:
	push	hl		; save copy buffer
	call	get_rstg_off
	pop	hl		; copy buffer
	jp	cpstr_crlf	; "REG",CR,LF

; ADD 80H - 87H
; check ADD code

chk_add:
	cp	88h
	jp	nc,chk_adc	;jr	nc, chk_adc

; ADD
	call	add_opstr
	call	a_colon_sp
	jp	mk_2ndopr	;jr	mk_2ndopr

; ADC 88H - 8FH
; check ADC code

chk_adc:
	cp	90h
	jp	nc,chk_sub	;jr	nc, chk_sub

; ADC
	ld	de, ADCstr
	call	mkopcstr
	call	a_colon_sp
	jp	mk_2ndopr	;jr	mk_2ndopr

; SUB 90H - 97H
; check SUB code

chk_sub:
	cp	98h
	jp	nc,chk_sbc	;jr	nc, chk_sbc

; SUB
	ld	de, SUBstr
	call	mkopcstr
	jp	mk_2ndopr	;jr	mk_2ndopr

; SBC 98H - 9FH
; check SBC code

chk_sbc:
	cp	0A0h
	jp	nc,chk_and	;jr	nc, chk_and

; SBC
	ld	de, SBCstr
	call	mkopcstr
	call	a_colon_sp
	jp	mk_2ndopr	;jr	mk_2ndopr

; AND 0A0H - 0A7H
; check AND code

chk_and:
	cp	0A8h
	jp	nc,chk_xor	;jr	nc, chk_xor

; AND
	ld	de, ANDstr
	call	mkopcstr
	jp	mk_2ndopr	;jr	mk_2ndopr

; XOR 0A8H - 0AFH
; check XOR code

chk_xor:

	cp	0B0h
	jp	nc,chk_or	;jr	nc, chk_or

; XOR
	ld	de, XORstr
	call	mkopcstr
	jp	mk_2ndopr	;jr	mk_2ndopr

; OR 0B0H - 0B7H
; check OR code

chk_or:
	cp	0B8h
	jp	nc,chk_cp	;jr	nc, chk_cp

; OR
	ld	de, ORstr
	call	mkopcstr
	jp	mk_2ndopr	;jr	mk_2ndopr

; CP 0B8H - 0BFH
; check CP code

chk_cp:
	cp	0C0h
	jp	nc, chk_pop_rp

; CP
	ld	de, CPstr
	call	mkopcstr
	jp	mk_2ndopr	;jr	mk_2ndopr

;check INC

chk_inc:
	ld	bc, INC_OPCDTBLE - INC_OPCDTBLS
	ld	hl, INC_OPCDTBLS
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz,chk_dec	;jr	nz, chk_dec

; INC
	push	af
	ld	de, INCstr
	call	mkopcstr
	pop	af

inc_dec:
	call	get_r_num
	jp	mk_2ndopr1

;check dec

chk_dec:
	ld	bc, DEC_OPCDTBLE - DEC_OPCDTBLS
	ld	hl, DEC_OPCDTBLS
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, chk_inc_rp		;jr	nz, chk_inc_rp

; DEC
	push	af
	ld	de, DECstr
	call	mkopcstr
	pop	af
	jp	inc_dec			;jr	inc_dec

; check inc rp

chk_inc_rp:
	ld	bc, inc_rp_e - inc_rp_s
	ld	hl, inc_rp_s
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, chk_dec_rp

; INC rp
	ld	de, INCstr
	call	mkopcstr

	ld	hl, INC_DEC_ADDrp
	call	mk_str
	jp	insPostStr	; CR, LF, 00h

; check dec rp

chk_dec_rp:
	ld	bc, dec_rp_e - dec_rp_s
	ld	hl, dec_rp_s
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, chk_add_rp		;jr	nz, chk_add_rp

; dec rp
	ld	de, DECstr
	call	mkopcstr

	ld	hl, INC_DEC_ADDrp
	call	mk_str
	jp	insPostStr	; CR, LF, 00h

;check ADD HL, rp

chk_add_rp:
	ld	bc, add_rp_e - add_rp_s
	ld	hl, add_rp_s
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, chk_ex		;jr	nz, chk_ex

; add hl, rp

	call	add_opstr

	ld	de, RNHL
	call	mk_rcs		; "HL, "

	push	hl
	ld	hl, INC_DEC_ADDrp
	call	get_strBufpoint
	pop	hl
	jp	cpstr_crlf	; CR, LF, 00h

; check POP rp

chk_pop_rp:
	ld	bc, pop_rp_e - pop_rp_s
	ld	hl, pop_rp_s
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, chk_push_rp		;jr	nz, chk_push_rp

; POP RP

	ld	de, POPstr
	call	mkopcstr

	ld	hl, POP_PUSHrp
	call	mk_str
	jp	insPostStr	; CR, LF, 00h

; check PUSH rp

chk_push_rp:
	ld	bc, push_rp_e - push_rp_s
	ld	hl, push_rp_s
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, chk_ex	;jr	nz, chk_ex

; PUSH RP

	ld	de, PUSHstr
	call	mkopcstr

	ld	hl, POP_PUSHrp
	call	mk_str
	jp	insPostStr	; CR, LF, 00h

; check EX XX, XX

chk_ex:
	cp	08H		; EX	AF,AF'
	jp	z, ex_af_af	;jr	z, ex_af_af
	cp	0E3H		; EX	(SP),HL
	jp	z, dex_sp_hl	;jr	z, dex_sp_hl
	cp	0EBH		; EX	DE,HL
	jp	nz, chk_1mc_bnh	;jr	nz, chk_1mc_bnh

; EX DE, HL
	ld	de, RNDE
	call	mk_rcs

ins_hl_opr:
	call	hl_crlf

ins_ex_opc:
	ld	de, EXstr
	jp	mkopcstr

; EX AF,AF'
ex_af_af:
	ld	de, RNAF
	call	mk_rcs
	ld	de, RNAFX
	call	cpstr_crlf
	jp ins_ex_opc		;jr ins_ex_opc

; EX (SP),HL
dex_sp_hl:
	ld	de, RNSP
	call	mk_krkcs
	jp	ins_hl_opr	;jr	ins_hl_opr

; check other one MC code

chk_1mc_bnh:

; check JP (HL)

	cp	0E9H		; JP (HL) ?
	jp	nz, chk_ret	;jr	nz, chk_ret

; JP (HL)
	ld	de, JPstr
	call	mkopcstr	; "JP "

	ld	de, RNHL
	call	mk_krk_1
	jp	insPostStr

; check RET CC
chk_ret:
	ld	bc, RETCC_TBLE - RETCC_TBLS
	ld	hl, RETCC_TBLS
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, chk_rst	;jr	nz, chk_rst

; RET CC
; BC : 7 >= BC >= 0

	; make opcode string
	ld	de, RETstr
	call	mkopcstr

	; make operand string

	ld	hl, CC_opr	; string base
	call	mk_str
	jp	insPostStr	; CR, LF, 00h

; check RST p

chk_rst:
	ld	bc, RST_TBLE - RST_TBLS
	ld	hl, RST_TBLS
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, chk_2MC	;jr	nz, chk_2MC		; 2bytes MC

; RST p

	ld	de, RSTstr
	call	mkopcstr	; de : next string buffer addr

; 0 <= BC <= 7
; 7: 00H  (0000 0 111 : 00 000 000)
; 6: 08H  (0000 0 110 : 00 001 000)
; 5: 10H  (0000 0 101 : 00 010 000)
; 4: 18H  (0000 0 100 : 00 011 000)
; 3: 20H  (0000 0 011 : 00 100 000)
; 2: 28H  (0000 0 010 : 00 101 000)
; 1: 30H  (0000 0 001 : 00 110 000)
; 0: 38H  (0000 0 000 : 00 111 000)
;
	ld	a, c
	cpl		; not a
	ADD	A,A			;Z80 CODE sla	a ##############
	ADD	A,A			;Z80 CODE sla	a ##############
	ADD	A,A			;Z80 CODE sla	a ##############
	and	38H	; a = RST No.

	ld	e, a
	ld	hl, dasm_OprStr
	jp	mk_n2crlf	; "nnH", CR, LF

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; analysys 2 byte MC
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

chk_2MC:

	push	af
	ld	a, 2
	ld	(mc_Size), a
	pop	af

; check LD r, nn

	ld	bc, ld_r_nn_e - ld_r_nn_s
	ld	hl, ld_r_nn_s
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, chk_logop2		;jr	nz, chk_logop2

; LD r, nn

	call	get_r_num
	call	get_rstg_off
	call	mk_rcs		; "REG, "

	EX	HL,DE			;Z80 CODE ld	de, (dasm_adr)
	LD	HL,(dasm_adr)		;###############################
	EX	HL,DE			;###############################
	inc	de
	ld	a, (de)		; get nn
	ld	e, a
	call	hex2str_asm	; "nnH"
	jp	insPostStr	; CR, LF, 00h

; check logical operation with 8 bit literal

chk_logop2:

	ld	bc, log_op_e - log_op_s
	ld	hl, log_op_s
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, chk_djnz

; logical operation with 8 bit literal


	push	bc
	ld	hl, logop2str
	call	get_strBufpoint
	call	mkopcstr		; make op code string
	pop	bc
	ld	a, c

	cp	1
	jp	z, acs_kn2k	;jr	z, acs_kn2k	; insert "A, (nn)"
	or	a
	jp	z, ins_kn2k	;jr	z, ins_kn2k	; insert "(nn), A"
	cp	7
	jp	c, ins_n2crlf	;jr	c, ins_n2crlf	; insert  "nnH"

; insert "A, "

	call	a_colon_sp

; "nn"
ins_n2crlf:
	EX	HL,DE			;Z80 CODE ld	de, (dasm_adr)
	LD	HL,(dasm_adr)		;###############################
	EX	HL,DE			;###############################
	inc	de
	ld	a, (de)
	ld	e, a		; get nn
	jp	mk_n2crlf	; "nnH", CR, LF

acs_kn2k:
	call	a_colon_sp	; "A, "
	call	kn2k		; "(nnH)"
	jp	insPostStr	; CR, LF, 00h

; "(nn), A"
ins_kn2k:
	ld	hl, dasm_OprStr	;operand str buffer
	call	kn2k		; "(nnH)"
	call	ins_kmR		; " ,"
	jp	a_crlf		; "A", CR, LF, 00h

; "(nnH)"
kn2k:
	call	ins_kakkoL	; "("
	EX	HL,DE			;Z80 CODE ld	de, (dasm_adr)
	LD	HL,(dasm_adr)		;###############################
	EX	HL,DE			;###############################
	inc	de
	ld	a, (de)		; get nn
	ld	e, a		; e: nn
	call	hex2str_asm	; hex strings
	jp	ins_kakkoR	; ")"


; check DJNZ nn
chk_djnz:
	cp	10H
	jp	nz, chk_jrnn	;jr	nz, chk_jrnn

; check DJNZ nn
	ld	de, DJNZstr
	jp	jr_n4crlf	;jr	jr_n4crlf

; check jr nn
chk_jrnn:
	cp	18H
	jp	nz, chk_jrcc	;jr	nz, chk_jrcc	; not JR relative

; JR Relative
	ld	de, JRstr
jr_n4crlf:
	call	mkopcstr	; de : next string buffer addr
	ld	hl, dasm_OprStr
	jp	mkRel_str	; "nnnnH",cr,lf : nnnn : branch address

; JR CC, nn

chk_jrcc:
	ld	bc, JRCC_TBLE - JRCC_TBLS1
	ld	hl, JRCC_TBLS1
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, chk_3MC	;jr	nz, chk_3MC	; no, check 3 bnytes MC

; JR CC, nn(Relative Value)

	ld	de, JRstr
	call	mkopcstr
	ld	hl, JRCC_opr1	; string base
	call	mk_str		; "NZ", "Z", "NC", "C"
	call	ins_kmR		; ", "
	jp	mkRel_str	; "nnnnH", cr, lf

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; analysys 3 byte MC
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

chk_3MC:
	push	af
	ld	a, 3
	ld	(mc_Size), a
	pop	af

	ld	bc, THREE_OPTBLe - THREE_OPTBL
	ld	hl, THREE_OPTBL
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, chk_ld16

; LD rp, nnnn

	ld	hl, logop3str
	call	get_strBufpoint
	call	mk_rcs		; "Reg, "

get_n4crlf:
	call	get_n4
	jp	mk_n4crlf	; "nnnnH", CR, LF, 00h

; LD 16bit literal
chk_ld16:
	cp	22h
	jp	z, ins_kkhl	;jr	z, ins_kkhl	; "(nnnnH), HL"
	cp	32h
	jp	z,ins_kka	;jr	z, ins_kka	; "(nnnnH), A"
	cp	2ah
	jp	z,ins_hlkk	;jr	z, ins_hlkk	; "HL, (nnnnH)"
	cp	3ah
	jp	nz, chk_jpn4	; check jp n4

; "A, (nnnnH)"
	call	a_colon_sp	; "A, "
	jp	kn4kcrlf	;jr	kn4kcrlf

; "(nnnnH), HL"
ins_kkhl:
	call	kn4hk
	jp	hl_crlf		; "HL",cr,lf

; "(nnnnH), A"
ins_kka:
	call	kn4hk
	jp	a_crlf		; "A",cr,lf

kn4hk:
	call	get_n4
	jp	ins_kn4kcs	; "(nnnnH), "

get_n4:
	push	hl
	ld	hl, (dasm_adr)
	inc	hl
	ld	e, (hl)
	inc	hl
	ld	d, (hl)
	pop	hl
	ret

; "HL, (nnnnH)"
ins_hlkk:
	ld	de, RNHL
	call	mk_rcs		; "HL ,"

kn4kcrlf:
	call	get_n4
	call	mk_kn4k		; "(nnnnH)"
	jp	insPostStr	; cr, lf

; check JP literal

chk_jpn4:
	cp	0C3H		; JP literal ?
	jp	nz, chk_calln4	;jr	nz, chk_calln4	; not JP literal

; "JP nnnnH"

	ld	de, JPstr
	call	mkopcstr	; de : next string buffer addr
	jp	get_n4crlf	;jr	get_n4crlf

; check call literal

chk_calln4:
	cp	0CDH		; CALL literal ?
	jp	nz, chk_jpcc	;jr	nz, chk_jpcc

; "CALL nnnnH",cr,lf

	ld	de, CALLstr
	call	mkopcstr	; de : next string buffer addr
	jp	get_n4crlf	;jr	get_n4crlf


; check 3 byte machine code: branch JP CC, nnnn

chk_jpcc:
	ld	bc, JPCC_TBLE - JPCC_TBLS
	ld	hl, JPCC_TBLS
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, chk_calcc	;jr	nz, chk_calcc

; JP CC, nnnn
; BC : 7 >= BC >= 0

	ld	de, JPstr

CC_NNNN:
	call	mkopcstr	; de : next string buffer addr

	ld	hl, CC_opr	; string base
	call	mk_str		; de: point (string end) + 1
	call	ins_kmR		; " ,"
	jp	get_n4crlf	;jr	get_n4crlf	; "nnnnh", cr,lf


; check 3 byte machine code: branch (CALL CC, nnnn 16bit literal)

chk_calcc:
	ld	bc, CLCC_TBLE - CLCC_TBLS
	ld	hl, CLCC_TBLS
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, chk_0CBH	;jr	nz, chk_0CBH

; CALL CC, nnnn
; BC : 7 >= BC >= 0 CALLstr

	ld	de, CALLstr
	jp	CC_NNNN		;jr	CC_NNNN

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; OPECODE 0CBH check
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

chk_0CBH:
	ld	hl, (dasm_adr)
	inc	hl		; 2nd opecode address

	push	af
	ld	a, 2
	ld	(mc_Size), a	; set 2bytes MC
	pop	af

	cp	0cbh
	jp	nz, chk_0DDH

	ld	a, (hl)		; a : 2nd opecode
	ld	hl, dasm_OprStr ; set operand string buffer

	cp	08h
	jp	c, mk_rlcr
	cp	10h
	jp	c, mk_rrcr
	cp	18h
	jp	c, mk_rlr
	cp	20h
	jp	c, mk_rrr
	cp	28h
	jp	c, mk_slar
	cp	30h
	jp	c, mk_srar
	cp	38h
	jp	c, op_err
	cp	40h
	jp	c, mk_srlr
	cp	80h
	jp	c, mk_bitr
	cp	0c0h
	jp	c, mk_resr

; make SET n, r, SET n, (HL)

	call	mk_bitr_str
	ld	de, SETstr
	jp	mkopcstr


; make RLC r, RLC (HL)
mk_rlcr:
	call	ins_rstg
	ld	de, RLCstr
	jp	mkopcstr

; make RRC r, RRC (HL)
mk_rrcr:
	call	ins_rstg
	ld	de, RRCstr
	jp	mkopcstr

; make RL r, RL (HL)
mk_rlr:
	call	ins_rstg
	ld	de, RLstr
	jp	mkopcstr

; make RR r, RR (HL)
mk_rrr:
	call	ins_rstg
	ld	de, RRstr
	jp	mkopcstr

; make SLA r, SLA (HL)
mk_slar:
	call	ins_rstg
	ld	de, SLAstr
	jp	mkopcstr

; make SRA r, SRA (HL)
mk_srar:
	call	ins_rstg
	ld	de, SRAstr
	jp	mkopcstr

; make SRL r, SRL (HL)
mk_srlr:
	call	ins_rstg
	ld	de, SRLstr
	jp	mkopcstr

; make BIT n, r, BIT n, (HL)
mk_bitr:
	call	mk_bitr_str
	ld	de, BITstr
	jp	mkopcstr

; make RES n, r, RES n, (HL)
mk_resr:
	call	mk_bitr_str
	ld	de, RESstr
	jp	mkopcstr

op_err:
	ld	de, ER_OPMSG
	jp	mkopcstr

;-------------------------------------------
; input a : 2nd opecode
;	hl : make string buffer
; make "bit No, r" string to *hl
; (ex) *hl = "1, B"
;-------------------------------------------
mk_bitr_str:
	push	af
	call	set_bitno
	pop	af

ins_rstg:
	and	07h
	ld	c, a	; table offset
	ld	b, 0		; bc : string offset
	push	hl
	call	get_rstg_off	; get string address to de
	pop	hl
	jp	cpstr_crlf	; "REG", CR, LF, 00h
;
; input hl : make string buffer
;
set_bitno:
	and	38h
	rrca
	rrca
	rrca
	ld	b, a	; bit number

	ld	a, 30h
	or	b		; a : bit string "0" - "7"
	ld	(hl), a
	inc	hl
	jp	ins_kmR

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; OPECODE 0DDH check
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

chk_0DDH:
	cp	0ddh
	jp	nz, chk_0EDH

	ld	de, RNIX
	EX	HL,DE			;Z80 CODE ld	(reg_xy), de	; save index reg string
	LD	(reg_xy),HL		;###############################
	EX	HL,DE			;###############################
	ld	de, addixrp_tbl
	EX	HL,DE			;Z80 CODE ld	(xy_srtp), de
	LD	(xy_srtp),HL		;###############################
	EX	HL,DE			;###############################

chk_0DDH_1:

	ld	a, (hl)		; get 2nd opcode

	cp	0E3h	; EX (SP),IX
	jp	z, ins_kspkix
	cp	0E9h	; JP (IX)
	jp	z, ins_jpix
	cp	0F9h	; LD SP, IX
	jp	z, ins_spix

	ld	bc, DD_2NDTBL1 - DD_2NDTBL
	ld	hl, DD_2NDTBL
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	z, ins_ixrp

	ld	bc, DD_2NDTBL2 - DD_2NDTBL1
	ld	hl, DD_2NDTBL1
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, chk_0DD3op

; INC	IX
; DEC	IX
; PUSH	IX
; POP	IX
	ld	hl, dd_2opt
	call	get_strBufpoint
	call	mkopcstr		; inseert OPCODE strings to dasm_OpcStr

	ld	hl, dasm_OprStr
	jp	ix_crlf			; "IX", CR, LF, 00h

; JP	(IX)
ins_jpix:
	EX	HL,DE			;Z80 CODE ld	de, (reg_xy)
	LD	HL,(reg_xy)		;##############################
	EX	HL,DE			;##############################
ins_jpix1:
	push	de
	ld	de, JPstr
	call	mkopcstr	; de : next string buffer addr

	pop	de
	call	mk_krk_1
	jp	insPostStr	; CR, LF, 00h

; EX	(SP),IX
ins_kspkix:
	ld	de, EXstr
	call	mkopcstr	; de : next string buffer addr
	ld	de, RNSP
	call	mk_krkcs	; "(SP), "
	jp	ix_crlf		; "IX",cr,lf

; LD	SP,IX
ins_spix:
	ld	de, RNSP
	call	mk_rcs		; "SP, "
	jp	ix_crlf		; "IX",cr,lf

; ADD	IX,BC
; ADD	IX,DE
; ADD	IX,IX
; ADD	IX,SP
ins_ixrp:
	call	add_opstr	; de : next string buffer addr

	EX	HL,DE			;Z80 CODE ld	de, (reg_xy)
	LD	HL,(reg_xy)		;##############################
	EX	HL,DE			;##############################
	call	mk_rcs		; "IX, "

	push	hl
	ld	hl, (xy_srtp)
	call	get_strBufpoint
	pop	hl
	jp	cpstr_crlf	; "REG",cr,lf

;
; check 0DD 3bytes MC
;
chk_0DD3op:
	push	af
	ld	a, 3
	ld	(mc_Size), a	; 2byte machine code
	pop	af

	ld	bc, dd_ld_tble - dd_ld_tbl
	ld	hl, dd_ld_tbl
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	z, dd_ld

	cp	86h
	jp	z, dd_mix
	cp	8eh
	jp	z, dd_mix1
	cp	9eh
	jp	z, dd_mix2

	ld	bc, dd_log_tble - dd_log_tbl
	ld	hl, dd_log_tbl
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, chk_0DD4op

;
; make "SUB (IX+nn)", "AND (IX+nn)", "XOR (IX+nn)"
;       "OR (IX+nn)",  "CP (IX+nn)"
;      "INC (IX+nn)", "DEC (IX+nn)"
;
	ld	hl, ddlogtbl
	call	get_strBufpoint
	call	mkopcstr

mk_kixpnk:
	EX	HL,DE			;Z80 CODE ld	de, (reg_xy)
	LD	HL,(reg_xy)		;##############################
	EX	HL,DE			;##############################

mk_kiypnk:
	ld	hl, dasm_OprStr
	jp	kixypnk_crlf		; "(IX+nnH)",cr,lf

;
; make "LD (IX+nn), r" or "LD r, (IX+nn)"
;
dd_ld:
	ld	a, c
	cp	7
	jp	nc, dd_ld1
	
; LD (IX+nn), r
	EX	HL,DE			;Z80 CODE ld	de, (reg_xy)
	LD	HL,(reg_xy)		;##############################
	EX	HL,DE			;##############################
	call	kixypnk_cs	; make "(IX+nn), "

	push	hl
	ld	hl, dd_ldtbl
	call	get_strBufpoint
	pop	hl
	jp	cpstr_crlf	; "A", "L", "H", "E", D", "C", "B"
				; CR, LF, 00h
	
; LD r, (IX+nn)
dd_ld1:
	sub	7
	ld	c, a
	ld	hl, dd_ldtbl
	call	get_strBufpoint

	call	mk_rcs		; "REG, "
dd_mix4:
	EX	HL,DE			;Z80 CODE ld	de, (reg_xy)
	LD	HL,(reg_xy)		;##############################
	EX	HL,DE			;##############################
	jp	kixypnk_crlf	; "(IX+nn)", CR, LF, 00h


; "ADD A,(IX+nn)"
dd_mix:
	ld	de, ADDstr
dd_mix3:
	call	mkopcstr	; "ADD"
	call	a_colon_sp	; "A, "
	jp	dd_mix4		;jr	dd_mix4		; "(IX+nn)",cr,lf

;"ADC A,(IX+nn)"
dd_mix1:
	ld	de, ADCstr
	jp	dd_mix3		;jr	dd_mix3

;"SBC A,(IX+nn)"
dd_mix2:
	ld	de, SBCstr
	jp	dd_mix3		;jr	dd_mix3

;
; check 0DD 4bytes MC
;
chk_0DD4op:
	push	af
	ld	a, 4
	ld	(mc_Size), a	; 2byte machine code
	pop	af

	cp	21h
	jp	z, DD_21
	cp	22h
	jp	z, DD_22
	cp	2ah
	jp	z, DD_2a
	cp	36h
	jp	nz, chk_DD_CB
; DD 36
; "LD (IX+xx), yy"

	EX	HL,DE			;Z80 CODE ld	de, (reg_xy)
	LD	HL,(reg_xy)		;##############################
	EX	HL,DE			;##############################
	call	kixypnk_cs	; "(IX+xx), "
	ld	e, d		; e: yy
	jp	mk_n2crlf	; "0yyH",cr,lf


; "LD IX, nnnn"
DD_21:
	EX	HL,DE			;Z80 CODE ld	de, (reg_xy)
	LD	HL,(reg_xy)		;##############################
	EX	HL,DE			;##############################
	call	mk_rcs		; "IX, "
	call	get_nnnn
	jp	mk_n4crlf	; "nnnnH",cr,lf

; "LD (nnnn), IX"
DD_22:
	call	get_nnnn
	call	ins_kn4kcs	; "(nnnnH), "
	jp	ix_crlf		; "IX",cr,lf

; "LD IX, (nnnn)"
DD_2a:
	EX	HL,DE			;Z80 CODE ld	de, (reg_xy)
	LD	HL,(reg_xy)		;##############################
	EX	HL,DE			;##############################
DD_2a1:
	call	mk_rcs		; "IX, "
	call	get_nnnn
	call	mk_kn4k		; "(nnnnH)"
	jp	insPostStr	; cr,lf

chk_DD_CB
	cp	0CBH
	jp	nz, op_err

	call	get_nnnn	; d: 3rd OP, e:nn
	ld	a, d

	ld	bc, dd_rt_tble - dd_rt_tbles
	ld	hl, dd_rt_tbles
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, DD_CB_nn_XX

	ld	hl, dd_rt_str
	call	get_strBufpoint
	call	mkopcstr	; RLC, RRC, RL, RR, SLA, SRA, SRL
	jp	mk_kixpnk	; "(IX+nnH)",cr,lf

; check BIT, RES, SET
DD_CB_nn_XX:

	; check undefine MC
	ld	bc, dd_bit_opTble - dd_bit_opTbl
	ld	hl, dd_bit_opTbl
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, op_err

	cp	80h
	jp	c, dd_bit
	cp	0c0h
	jp	c, dd_res

; DD_SET
; "SET i, (IX+nn)"

	ld	de, SETstr
	call	mkopcstr

DD_BSR:

	ld	hl, dasm_OprStr ; set operand string buffer
	call	set_bitno	; "i, "
	jp	dd_mix4		; "(IX+nn)", cr,lf

; DD_BIT
; "BIT i, (IX+nn)"
dd_bit:
	ld	de, BITstr
	call	mkopcstr
	jp	DD_BSR		;jr	DD_BSR

; DD_RES
; "RES i, (IX+nn)"
dd_res:
	ld	de, RESstr
	call	mkopcstr
	jp	DD_BSR		;jr	DD_BSR

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; OPECODE 0EDH check
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

chk_0EDH:
	cp	0edh
	jp	nz, chk_0FDH

	ld	a, (hl)		; get 2nd opcode

	; check undefine MC
	ld	bc, ed_op_tble - ed_op_tbl
	ld	hl, ed_op_tbl
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, op_err

	ld	a, c
	cp	6
	jp	c, ed_4mc
	cp	27
	jp	c, ed_no_opr
	cp	30
	jp	c, ed_im
	jp	z, ed_ld_ar
	cp	31
	jp	z, ed_ld_ai
	cp	32
	jp	z, ed_ld_ra
	cp	33
	jp	z, ed_ld_ia
	cp	38
	jp	c, ed_adc
	cp	42
	jp	c, ed_sbc
	cp	49
	jp	c, ed_out

; c: 49 - 55 : IN r,(C) (r:B,C,D,E,H,L,A)

	ld	de, INstr
	call	mkopcstr

	ld	a, c
	sub	49
	ld	c, a
	ld	hl, dd_ldtbl
	call	get_strBufpoint	; get strung buffer
	call	mk_rcs		; "REG, "
	ld	de, KCKstr
	jp	cpstr_crlf	; "(C)",cr,lf

; c: 42 - 48 : OUT (C),r(r:B,C,D,E,H,L,A)
ed_out:
	ld	de, OUTstr
	call	mkopcstr

	ld	de, KCKstr
	call	mk_rcs		; "(C), "

	ld	a, c
	sub	42
	ld	c, a
	push	hl
	ld	hl, dd_ldtbl

ed_out1:
	call	get_strBufpoint	; get strung buffer
	pop	hl
	jp	cpstr_crlf	; "REG",cr,lf

; c: 38 - 41 : SBC HL,rr(rr:BC,DE,HL,SP)
ed_sbc:
	ld	de, SBCstr
	call	mkopcstr

	ld	de, RNHL
	call	mk_rcs		; "HL, "

	ld	a, c
	sub	38
ed_sbc1:
	ld	c, a
	push	hl
	ld	hl, INC_DEC_ADDrp
	jp	ed_out1		;jr	ed_out1

; c: 34 - 37 : ADC HL,rr(rr:BC,DE,HL,SP)
ed_adc:
	ld	de, ADCstr
	call	mkopcstr

	ld	de, RNHL
	call	mk_rcs		; "HL, "

	ld	a, c
	sub	34
	jp	ed_sbc1		;jr	ed_sbc1

; c: 30, 31 : LD A,R ; LD A,I
ed_ld_ar:
	call	a_colon_sp	; "A, "
	ld	de, RNR
	jp	cpstr_crlf	; "R",cr,lf
	
ed_ld_ai:
	call	a_colon_sp	; "A, "
	ld	de, RNI
	jp	cpstr_crlf	; "I",cr,lf
	
; c: 32, 33 : LD R,A ; LD I,A
ed_ld_ra:
	ld	de, RNR
ed_ld_ra1:
	call	mk_rcs		; "R, "
	jp	a_crlf		; "A",cr,lf

ed_ld_ia:
	ld	de, RNI
	jp	ed_ld_ra1	;jr	ed_ld_ra1
	
; c: 27, 28, 29 : IM 2, IM 1, IM 0
ed_im:
	ld	de, IMstr
	call	mkopcstr

	ld	a, 29
	sub	c
	add	a, '0'		; '0', '1', '2'
	ld	hl, dasm_OprStr
	ld	(hl), a
	inc	hl
	jp	insPostStr

; c: 6 - 26 ed_noopr
ed_no_opr:
	sub	6
	ld	c, a
	ld	b, 0
	ld	hl, ed_noopr
	call	get_strBufpoint		; de : string buffer
	jp	mkopcstr
;
; check ED 4 byte MC
;
ed_4mc:
	ld	a, 4
	ld	(mc_Size), a	; 4byte machine code

	push	bc
	ld	a, c
	ld	hl, ed_rp_str
	call	get_strBufpoint		; de : string buffer
	pop	bc
	ld	a, c
	cp	3
	jp	c, DD_2a1		; "rr, (nnnnH)",cr,lf
					; rr: BC, DE, SP
; "(nnnnH), rr" rr: BC, DE, SP

	push	de
	call	get_nnnn
	call	ins_kn4kcs	; "(nnnnH), "
	pop	de
	jp	cpstr_crlf	; "REG",cr,lf

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; OPECODE 0FDH check
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

chk_0FDH:

	ld	de, RNIY
	EX	HL,DE			;Z80 CODE ld	(reg_xy), de	; save index reg string
	LD	(reg_xy),HL		;###############################
	EX	HL,DE			;###############################
	ld	de, addiyrp_tbl
	EX	HL,DE			;Z80 CODE ld	(xy_srtp), de
	LD	(xy_srtp),HL		;###############################
	EX	HL,DE			;###############################

	jp	chk_0DDH_1

;-----------------------------------
; make "(REG+nn), " at dasm_OprStr
;	REG : IX or IY
; input de: RNIX, or RNIY
;-----------------------------------
kixypnk_cs:
	ld	hl, dasm_OprStr
	call	mk_kixypnk
	jp	ins_kmR

;-----------------------------------
; make "(REG+nn)",cr,lf 
;	REG : IX or IY
; input de: RNIX, or RNIY
;       hl: string buffer
;-----------------------------------
kixypnk_crlf:
	call	mk_kixypnk
	jp	insPostStr	; CR, LF, 00h

;-----------------------------------
; make "(IX+nn)", "(IY+nn)" string
; input de: RNIX, or RNIY
;       hl: string buffer
;-----------------------------------
mk_kixypnk:
	call	ins_kakkoL	; "("
	call	st_copy	; IX
	call	get_nnnn
	ld	a, e		;get number
	cp	80h
	jp	c, numPlus	;jr	c, numPlus	; number plus
	CPL				;z80 CODE neg ########	; a = not a + 1
	INC	A			;flags may be wrong, it's OK here
	ld	e, a
	call	ins_misR	; -
sv_nnkr:
	call	hex2str_asm	; nnh
	jp	ins_kakkoR	; ")"
	
numPlus:
	call	ins_plsR	; +
	jp	sv_nnkr		;jr	sv_nnkr

; de : nnnn , or d:yy, e:xx
get_nnnn:
	push	hl
	ld	hl, (dasm_adr)
	inc	hl		; 2nd opecode address
	inc	hl		; operand address
	ld	e, (hl)		; get nn
	inc	hl
	ld	d, (hl)		; d:yy use for "LD (IX+xx), yy"
				; d:OP use for 4bytes MC like "DD CB xx OP"
				; de : YYXX : ED OP XX YY
	pop	hl
	ret

;-------------------------------------------------
; hl: massage table
; bc: found operand point
;
; output: copy strings to dasm_OprStr buffer
;         hl = end point of copied string buffer
;-------------------------------------------------
mk_str:
	call	get_strBufpoint
	ld	hl, dasm_OprStr
	jp	st_copy

;------------------------------------------------
; get register strings address
; input bc: string buffer offset
; output de: register strings address
;------------------------------------------------
get_rstg_off:
	ld	hl, disRegTbl

;-------------------------------------------------
; input
;	bc: found operand point
;	hl: massage table
; output: 
;	DE : string point
;-------------------------------------------------
get_strBufpoint:
	LD	D,A			;Z80 CODE sla	c ##############
	LD	A,C			;###############################
	ADD	A,C			;###############################
	LD	C,A			;###############################
	LD	A,B			;Z80 CODE rl	b		; bc *=2
	ADC	A,B			;###############################
	LD	B,A			;###############################
	LD	A,D			;## D destroyed it's ok here ###
	add	hl, bc		; get operand string point
	ld	e, (hl)
	inc	hl
	ld	d, (hl)
	ret

;------------------------------------------------
; insert "nnH", CR, LF to buffer
;input e: value
;      hl: string buffer
;------------------------------------------------
mk_n2crlf:
	call	hex2str_asm
	jp	insPostStr

;------------------------------------------------
; insert "nnnnH", CR, LF to buffer
;input de: value
;      hl: string buffer
;------------------------------------------------
mk_n4crlf:
	call	hex4str_asm
	jp	insPostStr

;------------------------------------------------
; insert "(nnnnH), " to buffer dasm_OprStr
;input de: value
;
; make "(nnnnH), " to dasm_OprStr buffer
; output hl: end point of string buffer
;------------------------------------------------
ins_kn4kcs:
	ld	hl, dasm_OprStr	;operand str buffer
	call	ins_kakkoL	; "("
	call	hex4str_asm
	call	ins_kakkoR	; ")"
	jp	ins_kmR		; ", "

;------------------------------------------------
; insert "(nnnnH)" to buffer
;input de: value
;      hl: string buffer
;------------------------------------------------
mk_kn4k:
	call	ins_kakkoL	; "("
	call	hex4str_asm
	jp	ins_kakkoR	; ")"

;------------------------------------------------
; Make address string from branch relative value
; input hl: string buffer pointer
; output *hl= "nnnnH",cr,lf,0
;------------------------------------------------
mkRel_str:
	push	hl
	ld	hl, (dasm_adr)
	call	Rel_adr_c	; return hl : target addr
	ex	de, hl		; hl: buffer, de: addr value
	pop	hl
	call	hex4str_asm
	jp	insPostStr	;jr	insPostStr

;------------------------------------------------
;input de: value, hl: save baffer.
;output 4hex chars(asm format)
;	(ex). "0FFFFH"
;             "1234H"
;       hl : hl = hl + 5(or 6); *hl=0
;------------------------------------------------
hex4str_asm:
	ld	a, d
	cp	0a0h
	jp	c, h4sa_1	;jr	c, h4sa_1
	ld	a, '0'
	ld	(hl), a
	inc	hl

h4sa_1:
	call	hex4str
h4sa_2:
	ld	a, 'H'
	jp	ikk_1

;------------------------------------------------
;input e: value, hl: save baffer.
;output: 2 hex chars(asm format)
;	(ex). "0FFH"
;             "12H"
;       hl : hl = hl + 3(or 4); *hl=0
;------------------------------------------------
hex2str_asm:
	ld	a, e
	cp	0a0h
	jp	c, h2sa_1	;jr	c, h2sa_1
	ld	a, '0'
	ld	(hl), a
	inc	hl

h2sa_1:
	call	hex2str
	jp	h4sa_2		;jr	h4sa_2

;------------------------------------------------
;input de: value, hl: save baffer.
;output: 4 hex chras in save buffer
;	 hl = hl + 4
;------------------------------------------------
hex4str:
	ld	a, d
	call	h2s_n1
	ld	a, d
	call	h2s_n2

;------------------------------------------------
;input e: value, hl: save baffer.
;output: 2 hex chras in save buffer
;	 hl = hl + 2
;------------------------------------------------
hex2str:
	ld	a, e
	call	h2s_n1
	ld	a, e
	call	h2s_n2
	ret

h2s_n1:
	rrca
	rrca
	rrca
	rrca
h2s_n2:
	and	0fh
	or	30h
	cp	3aH
	jp	c, ikk_1	;jr	c, ikk_1	; '0' to '9'
	add	a, 07h		; 'A' to 'F'
	jp	ikk_1		;jr	ikk_1

add_opstr:
	ld	de, ADDstr
;-----------------------------------
; make opecode strings
; input de : opecode strings
;-----------------------------------
mkopcstr:
	push	af
	push	bc
	ld	b, 8	;tab size
	ld	hl, dasm_OpcStr
dmkstr0:
	ld	a, (de)
	or	a
	jp	z, dmkst1	;jr	z, dmkst1
	ld	(hl), a
	inc	hl
	inc	de
	dec	b
	jp	dmkstr0		;jr	dmkstr0
dmkst1:
	ld	a, ' '
dmkst2:
	ld	(hl), a
	inc	hl
	dec	b
	jp	nz, dmkst2	;jr	nz, dmkst2
	pop	bc
	pop	af
	ret

;
; "REG",CR,LF
;
a_crlf:
	ld	de, RNA
	jp	cpstr_crlf	;jr	cpstr_crlf	; "A",CR,LF
;bc_crlf:
;	ld	de, RNBC
;	jp	cpstr_crlf	;jr	cpstr_crlf
;de_crlf:
;	ld	de, RNDE
;	jp	cpstr_crlf	;jr	cpstr_crlf
ix_crlf:
	EX	HL,DE			;Z80 CODE ld	de, (reg_xy)
	LD	HL,(reg_xy)		;##############################
	EX	HL,DE			;##############################
	jp	cpstr_crlf	;jr	cpstr_crlf
;sp_crlf:
;	ld	de, RNSP
;	jp	cpstr_crlf	;jr	cpstr_crlf
hl_crlf:
	ld	de, RNHL
cpstr_crlf:
	call	st_copy		; "REG"
	
;-----------------------------------
; insert CR, LF, 00H(end delimiter)
; input hl : insert buffer
;-----------------------------------
insPostStr:
	ld	a, CR
	ld	(hl), a		; CR code
	inc	hl
	ld	a, LF
	ld	(hl), a		; LF code
	inc	hl
	xor	a
	ld	(hl), a		; end delimiter
	ret

mk_krk_1:
	ld	hl, dasm_OprStr
;----------------------------
; insert "(Reg)" to buffer
;
; input de: register string
;	hl: save buffer
;----------------------------
mk_krk:
	call	ins_kakkoL	; "("
	call	st_copy		; "REG"
	jp	ins_kakkoR	;jr	ins_kakkoR	; ")"

;----------------------------
; insert "(Reg), " to dasm_OprStr
;
; input de: register string
;----------------------------
mk_krkcs:
	ld	hl, dasm_OprStr
	call	ins_kakkoL	; "("
	call	st_copy		; "REG"
	call	ins_kakkoR	; ")"
	jp	ins_kmR		;jr	ins_kmR; ", "

;----------------------------
; insert "A, " to dasm_OprStr
;----------------------------
a_colon_sp:
	ld	de, RNA
;----------------------------
; insert "Reg, " to dasm_OprStr
;
; input de: register string
;----------------------------
mk_rcs:
	ld	hl, dasm_OprStr
	call	st_copy		; "REG"
	jp	ins_kmR		;jr	ins_kmR	; ", "

;-------------------------------------------------
; copy string to output buffer
; input:
;	de : string point
;	hl : output buffer
;-------------------------------------------------
st_copy:
	ld	a, (de)
	ld	(hl), a
	or	a
	ret	z	; return after coping delimiter
			; hl : delimiter position
	inc	hl
	inc	de
	jp	st_copy		;jr	st_copy

;--------------
; insert "("
;--------------
ins_kakkoL:
	ld	a, '('
	jp	ikk_1		;jr	ikk_1
	
;--------------
; insert ")"
;--------------
ins_kakkoR:
	ld	a, ')'
ikk_1:
	ld	(hl), a
	inc	hl
	ret

;--------------
; insert " "
;--------------
ins_spcR:
	ld	a, ' '
	jp	ikk_1		;jr	ikk_1

;--------------
; insert "+"
;--------------
ins_plsR:
	ld	a, '+'
	jp	ikk_1		;jr	ikk_1

;--------------
; insert "-"
;--------------
ins_misR:
	ld	a, '-'
	jp	ikk_1		;jr	ikk_1

;--------------
; insert ", "
;--------------
ins_kmR:
	ld	a, ','
	call	ikk_1
	jp	ins_spcR	;jr	ins_spcR

;------------------------------------------
; Dis assemble tables
;------------------------------------------
oth_1op_s:
	DB	00H	; NOP
	DB	07H	; RLCA
	DB	0FH	; RRCA
	DB	17H	; RLA
	DB	1FH	; RRA
	DB	27H	; DAA
	DB	2FH	; CPL
	DB	37H	; SCF
	DB	3FH	; CCF
	DB	0C9H	; RET
	DB	0D9H	; EXX
	DB	0F3H	; DI
	DB	0FBH	; EI
oth_1op_e:

INC_OPCDTBLS:
	DB	04H	; INC	B
	DB	0CH	; INC	C
	DB	14H	; INC	D
	DB	1CH	; INC	E
	DB	24H	; INC	H
	DB	2CH	; INC	L
	DB	34H	; INC	(HL)
	DB	3CH	; INC	A
INC_OPCDTBLE:

DEC_OPCDTBLS:
	DB	05H	; DEC	B
	DB	0DH	; DEC	C
	DB	15H	; DEC	D
	DB	1DH	; DEC	E
	DB	25H	; DEC	H
	DB	2DH	; DEC	L
	DB	35H	; DEC	(HL)
	DB	3DH	; DEC	A
DEC_OPCDTBLE:

inc_rp_s:
	DB	03H	; INC	BC
	DB	13H	; INC	DE
	DB	23H	; INC	HL
	DB	33H	; INC	SP
inc_rp_e:

dec_rp_s:
	DB	0BH	; DEC	BC
	DB	1BH	; DEC	DE
	DB	2BH	; DEC	HL
	DB	3BH	; DEC	SP
dec_rp_e:

add_rp_s:
	DB	09H	; ADD	HL,BC
	DB	19H	; ADD	HL,DE
	DB	29H	; ADD	HL,HL
	DB	39H	; ADD	HL,SP
add_rp_e:

pop_rp_s:
	DB	0C1H	; POP	BC
	DB	0D1H	; POP	DE
	DB	0E1H	; POP	HL
	DB	0F1H	; POP	AF
pop_rp_e:

push_rp_s:
	DB	0C5H	; PUSH	BC
	DB	0D5H	; PUSH	DE
	DB	0E5H	; PUSH	HL
	DB	0F5H	; PUSH	AF
push_rp_e:

dd_ld_tbl:
	DB	46h	; nn: LD B,(IX+nn)
	DB	4Eh	; nn: LD C,(IX+nn)
	DB	56h	; nn: LD D,(IX+nn)
	DB	5Eh	; nn: LD E,(IX+nn)
	DB	66h	; nn: LD H,(IX+nn)
	DB	6Eh	; nn: LD L,(IX+nn)
	DB	7Eh	; nn: LD A,(IX+nn)

	DB	70h	; nn: LD (IX+nn),B
	DB	71h	; nn: LD (IX+nn),C
	DB	72h	; nn: LD (IX+nn),D
	DB	73h	; nn: LD (IX+nn),E
	DB	74h	; nn: LD (IX+nn),H
	DB	75h	; nn: LD (IX+nn),L
	DB	77h	; nn: LD (IX+nn),A
dd_ld_tble:

dd_log_tbl:
	DB	96h	; nn: SUB (IX+nn)
	DB	0A6h	; nn: AND (IX+nn)
	DB	0AEh	; nn: XOR (IX+nn)
	DB	0B6h	; nn: OR  (IX+nn)
	DB	0BEh	; nn: CP  (IX+nn)
	DB	34h	; nn: INC (IX+nn)
	DB	35h	; nn: DEC (IX+nn)
dd_log_tble:

dd_rt_tbles:
	DB	06h	; RLC (IX+nn)
	DB	0Eh	; RRC (IX+nn)
	DB	16h	; RL  (IX+nn)
	DB	1Eh	; RR  (IX+nn)
	DB	26h	; SLA (IX+nn)
	DB	2Eh	; SRA (IX+nn)
	DB	3Eh	; SRL (IX+nn)
dd_rt_tble:

; use checking undefine MC ( DD CB nn XX )
dd_bit_opTbl:
	DB	46h	; BIT 0,(IX+12H)
	DB	4Eh	; BIT 1,(IX+12H)
	DB	56h	; BIT 2,(IX+12H)
	DB	5Eh	; BIT 3,(IX+12H)
	DB	66h	; BIT 4,(IX+12H)
	DB	6Eh	; BIT 5,(IX+12H)
	DB	76h	; BIT 6,(IX+12H)
	DB	7Eh	; BIT 7,(IX+12H)
	DB	86h	; RES 0,(IX+12H)
	DB	8Eh	; RES 1,(IX+12H)
	DB	96h	; RES 2,(IX+12H)
	DB	9Eh	; RES 3,(IX+12H)
	DB	0A6h	; RES 4,(IX+12H)
	DB	0AEh	; RES 5,(IX+12H)
	DB	0B6h	; RES 6,(IX+12H)
	DB	0BEh	; RES 7,(IX+12H)
	DB	0C6h	; SET 0,(IX+12H)
	DB	0CEh	; SET 1,(IX+12H)
	DB	0D6h	; SET 2,(IX+12H)
	DB	0DEh	; SET 3,(IX+12H)
	DB	0E6h	; SET 4,(IX+12H)
	DB	0EEh	; SET 5,(IX+12H)
	DB	0F6h	; SET 6,(IX+12H)
	DB	0FEh	; SET 7,(IX+12H)
dd_bit_opTble:

; use checking undefine MC ( ED XX ....)
ed_op_tbl:
	DB	40h	; 55:IN  B,(C)
 	DB	48h	; 54:IN  C,(C)
	DB	50h	; 53:IN  D,(C)
	DB	58h	; 52:IN  E,(C)
	DB	60h	; 51:IN  H,(C)
	DB	68h	; 50:IN  L,(C)
	DB	78h	; 49:IN  A,(C)

	DB	41h	; 48:OUT (C),B
	DB	49h	; 47:OUT (C),C
	DB	51h	; 46:OUT (C),D
	DB	59h	; 45:OUT (C),E
	DB	61h	; 44:OUT (C),H
	DB	69h	; 43:OUT (C),L
	DB	79h	; 42:OUT (C),A

	DB	42h	; 41:SBC HL,BC
	DB	52h	; 40:SBC HL,DE
	DB	62h	; 39:SBC HL,HL
	DB	72h	; 38:SBC HL,SP

	DB	4Ah	; 37:ADC HL,BC
	DB	5Ah	; 36:ADC HL,DE
	DB	6Ah	; 35:ADC HL,HL
	DB	7Ah	; 34:ADC HL,SP

	DB	47h	; 33:LD I,A
	DB	4Fh	; 32:LD R,A
	DB	57h	; 31:LD A,I
	DB	5Fh	; 30:LD A,R

	DB	46h	; 29:IM  0
	DB	56h	; 28:IM  1
	DB	5Eh	; 27:IM  2

	DB	44h	; 26:NEG
	DB	45h	; 25:RETN
	DB	4Dh	; 24:RETI
	DB	67h	; 23:RRD
	DB	6Fh	; 22:RLD
	DB	0A0h	; 21:LDI
	DB	0A1h	; 20:CPI
	DB	0A2h	; 19:INI
	DB	0A3h	; 18:OUTI
	DB	0A8h	; 17:LDD
	DB	0A9h	; 16:CPD
	DB	0AAh	; 15:IND
	DB	0ABh	; 14:OUTD
	DB	0B0h	; 13:LDIR
	DB	0B1h	; 12:CPIR
	DB	0B2h	; 11:INIR
	DB	0B3h	; 10:OTIR
	DB	0B8h	; 09:LDDR
	DB	0B9h	; 08:CPDR
	DB	0BAh	; 07:INDR
	DB	0BBh	; 06:OTDR

	DB	43h	; 05:LD (nnnn),BC
	DB	53h	; 04:LD (nnnn),DE
	DB	73h	; 03:LD (nnnn),SP

	DB	4Bh	; 02:LD BC,(nnnn)
	DB	5Bh	; 01:LD DE,(nnnn)
	DB	7Bh	; 00:LD SP,(nnnn)
ed_op_tble:

RNAF:		DB	"AF",00H
RNAFX:		DB	"AF'",00H

RETstr:		db	"RET", 00h
JPstr:		db	"JP", 00h
JRstr:		db	"JR", 00h
DJNZstr:	db	"DJNZ", 00h
CALLstr:	db	"CALL", 00h
RSTstr:		db	"RST", 00h
LDstr:		db	"LD", 00h
HLTstr:		db	"HALT", 00h
ADDstr:		db	"ADD", 00h
ADCstr:		db	"ADC", 00h
SUBstr:		db	"SUB", 00h
SBCstr:		db	"SBC", 00h
ANDstr:		db	"AND", 00h
XORstr:		db	"XOR", 00h
ORstr:		db	"OR", 00h
CPstr:		db	"CP", 00h
INCstr:		db	"INC", 00h
DECstr:		db	"DEC", 00h
POPstr:		db	"POP", 00h
PUSHstr:	db	"PUSH",00h
EXstr:		db	"EX", 00h

EXXstr:		db	"EXX", 00h
NOPstr:		db	"NOP", 00h
RLCAstr:	db	"RLCA", 00h
RRCAstr:	db	"RRCA", 00h
RLAstr:		db	"RLA", 00h
RRAstr:		db	"RRA", 00h
DAAstr:		db	"DAA", 00h
CPLstr:		db	"CPL", 00h
SCFstr:		db	"SCF", 00h
CCFstr:		db	"CCF", 00h
DIstr:		db	"DI", 00h
EIstr:		db	"EI", 00h

OUTstr:		db	"OUT", 00h
INstr:		db	"IN", 00h

RLCstr		db	"RLC", 00h
RRCstr		db	"RRC", 00h
RLstr		db	"RL", 00h
RRstr		db	"RR", 00h
SLAstr		db	"SLA", 00h
SRAstr		db	"SRA", 00h
SRLstr		db	"SRL", 00h
BITstr		db	"BIT", 00h
RESstr		db	"RES", 00h
NEGstr		db	"NEG", 00h
IMstr		db	"IM", 00h
RRDstr		db	"RRD", 00h
RLDstr		db	"RLD", 00h
LDIstr		db	"LDI", 00h
CPIstr		db	"CPI", 00h
INIstr		db	"INI", 00h
OUTIstr		db	"OUTI", 00h
LDDstr		db	"LDD", 00h
CPDstr		db	"CPD", 00h
INDstr		db	"IND", 00h
OUTDstr		db	"OUTD", 00h
LDIRstr		db	"LDIR", 00h
CPIRstr		db	"CPIR", 00h
INIRstr		db	"INIR", 00h
OTIRstr		db	"OTIR", 00h
LDDRstr		db	"LDDR", 00h
CPDRstr		db	"CPDR", 00h
INDRstr		db	"INDR", 00h
OTDRstr		db	"OTDR", 00h
RETIstr		db	"RETI", 00h
RETNstr		db	"RETN", 00h
;BITstr		db	"BIT", 00h
;RESstr		db	"RES", 00h
SETstr		db	"SET", 00h
com_errm:
ER_OPMSG:	db	"???", 00h
KCKstr		db	"(C)", 00h

kRNHL:	db	"(HL)", 00h
cc0:	db	"NZ", 00h
cc1:	db	"Z", 00h
cc2:	db	"NC", 00h
cc3:	db	"C", 00h
cc4:	db	"PO", 00h
cc5:	db	"PE", 00h
cc6:	db	"P", 00h
cc7:	db	"M", 00h

disRegTbl:	dw	RNB, RNC, RND, RNE, RNH, RNL, kRNHL, RNA
CC_opr:		dw	cc7, cc6, cc5, cc4, cc3, cc2, cc1, cc0
JRCC_opr1:	dw	cc3, cc2, cc1, cc0
INC_DEC_ADDrp:	dw	RNSP, RNHL, RNDE, RNBC
POP_PUSHrp:	dw	RNAF, RNHL, RNDE, RNBC

oth_1op_tbl:	dw	EIstr, DIstr, EXXstr, RETstr, CCFstr
		dw	SCFstr, CPLstr, DAAstr, RRAstr
		dw	RLAstr, RRCAstr, RLCAstr, NOPstr

logop2str:	dw	OUTstr, INstr, CPstr, ORstr, XORstr
		dw	ANDstr, SUBstr, SBCstr, ADCstr, ADDstr

logop3str	dw	RNSP, RNHL, RNDE, RNBC
ed_rp_str	dw	RNSP, RNDE, RNBC, RNSP, RNDE, RNBC

dd_2opt:	dw	POPstr, PUSHstr, DECstr, INCstr
addixrp_tbl:	dw	RNSP, RNIX, RNDE, RNBC
addiyrp_tbl:	dw	RNSP, RNIY, RNDE, RNBC

ddlogtbl:	dw	DECstr, INCstr, CPstr, ORstr, XORstr, ANDstr, SUBstr
dd_ldtbl:	dw	RNA, RNL, RNH, RNE, RND, RNC, RNB

ed_noopr:	dw	OTDRstr, INDRstr, CPDRstr, LDDRstr
		dw	OTIRstr, INIRstr, CPIRstr, LDIRstr
		dw	OUTDstr, INDstr, CPDstr, LDDstr
		dw	OUTIstr, INIstr, CPIstr, LDIstr
		dw	RLDstr, RRDstr, RETIstr, RETNstr
		dw	NEGstr

dd_rt_str:	dw	SRLstr, SRAstr, SLAstr, RRstr
		dw	RLstr, RRCstr, RLCstr
	
;;;
;;; GO address
;;; 

GO:
	EX	HL,DE			;Z80 CODE ld	de, (REGPC)
	LD	HL,(REGPC)		;###############################
	LD	(goTmp),HL		;Z80 CODE ld	(goTmp), de	; save go tmp go address
	EX	HL,DE			;###############################
	INC	HL
	CALL	SKIPSP
	CALL	RDHEX
	jp	nc, gostmp	;jr	nc, gostmp
	OR	A
	JP	Z, g_stpadr	;JR	Z, g_stpadr
gostmp:
	ld	a, d
	IF	Fullram
	ELSE
	cp	IO_B >> 8	; a - 0F0H
	jp	nc, ERR		; detect I/O area
	ENDIF
	EX	HL,DE			;Z80 CODE LD	(goTmp),DE	; save going address
	LD	(goTmp),HL		;###############################
	EX	HL,DE			;###############################
g_stpadr:
	CALL	SKIPSP
	LD	A,(HL)
	or	a
	jp	z, GO1		;jr	z, GO1
	cp	','
	jp	nz, ERR

; set break point with go command

	INC	HL
	CALL	SKIPSP
	CALL	RDHEX		; 1st arg.
	jp	c, ERR

	ld	hl, tmpb_f	; hl: temp break point buffer
	call	setbpadr
	jp	c, ERR		; address is incorrect

GO1:
	LD	hl, (goTmp)
	ld	(REGPC), hl	; set go address

G0:
; check Trace mode

	ld	a, (tpt1_f)
	or	a
	jp	z, donot_trace	;jr	z, donot_trace
	
	ld	hl,(tmpT)
	ld	(REGPC), hl	; set trace address

	ld	hl, tpt1_f
	ld	de, tpt1_adr
	call	set_bp
	ld	hl, tpt2_f
	ld	de, tpt2_adr
	call	set_bp
	jp	skp_tbp		;jr	skp_tbp		; skip set tmp bp and bp, if tracing

; set break point
donot_trace:
	ld	hl, bpt1_f
	ld	de, bpt1_adr
	call	set_bp
	ld	hl, bpt2_f
	ld	de, bpt2_adr
	call	set_bp


; check go, break pointer

	ld	hl, tmpb_f
	ld	de, tmpb_adr
	call	set_bp

	;; R register adjustment

skp_tbp:
	LD	HL,(REGSP)
	LD	SP,HL
	LD	HL,(REGPC)
	PUSH	HL
;	LD	IX,(REGIX)		;Z80 CODE ignore now ###########
;	LD	IY,(REGIY)		;Z80 CODE ignore now ###########
;	LD	HL,(REGAFX)		;reg AF' restore innore now ####
;	PUSH	HL			;reg AF' restore innore now ####
;	LD	BC,(REGBCX)		;reg BC' restore innore now ####
;	LD	DE,(REGDEX)		;reg DE' restore innore now ####
;	LD	HL,(REGHLX)		;reg HL' restore innore now ####
;	EXX				;Z80 CODE ignore now ###########
;	POP	AF			;reg AF' restore innore now ####
;	EX	AF,AF'			;Z80 CODE ignore now ###########
	LD	HL,(REGAF)
	PUSH	HL
	LD	HL,(REGBC)		;Z80 CODE LD	BC,(REGBC) #####
	LD	B,H			;## HL destroyed it's OK here ##
	LD	C,L			;## HL destroyed it's OK here ##
	LD	HL,(REGBC)		;Z80 CODE LD	DE,(REGBC) #####
	LD	D,H			;## HL destroyed it's OK here ##
	LD	L,L			;## HL destroyed it's OK here ##
	LD	HL,(REGHL)
;	LD	A,(REGI)		;reg I restore ignore now ######
;	LD	I,A			;Z80 CODE ignore now ###########
;	LD	A,(REGR)		;reg R restore ignore now ######
;	LD	R,A			;Z80 CODE ignore now ###########
	POP	AF
	RET			; POP PC

set_bp:
	ld	a, (hl)
	or	a
	ret	z

	ld	a, (de)
	ld	c, a
	inc	de
	ld	a, (de)
	ld	b, a	; bc = break point address

	ld	a, 0FFH
	ld	(bc), a	; insert RST 38H code
	ret
	
;;;
;;; SET memory
;;; 

SETM:
	INC	HL
	CALL	SKIPSP
	CALL	RDHEX

	jp	c, ERR

	CALL	SKIPSP
	LD	A,(HL)
	OR	A
	JP	NZ,ERR
	LD	A,C
	OR	A
	JP	NZ,SM0		;JR	NZ,SM0
	EX	HL,DE			;Z80 CODE LD	DE,(SADDR)
	LD	HL,(SADDR)		;###############################
	EX	HL,DE			;###############################

SM0:
	EX	DE,HL
SM1:
	CALL	HEXOUT4
	PUSH	HL
	LD	HL,dsap
	CALL	STROUT
	POP	HL
	LD	A,(HL)
	PUSH	HL
	CALL	HEXOUT2
	LD	A,' '
	CALL	CONOUT
	CALL	GETLIN
	CALL	SKIPSP
	LD	A,(HL)
	OR	A
	JP	NZ,SM2		;JR	NZ,SM2
	;; Empty  (Increment address)
	POP	HL
	INC	HL
	LD	(SADDR),HL
	JP	SM1		;JR	SM1
SM2:
	CP	'-'
	JP	NZ,SM3		;JR	NZ,SM3
	;; '-'  (Decrement address)
	POP	HL
	DEC	HL
	LD	(SADDR),HL
	JP	SM1		;JR	SM1
SM3:
	CP	'.'
	JP	NZ,SM4		;JR	NZ,SM4
	POP	HL
	LD	(SADDR),HL
	JP	WSTART
SM4:
	CALL	RDHEX
	OR	A
	POP	HL
	JP	Z,ERR
	LD	(HL),E
	INC	HL
	LD	(SADDR),HL	; set value

	; resave opcode for BP command
	EX	HL,DE			;Z80 CODE ld	de, (bpt1_adr)
	LD	HL,(bpt1_adr)		;###############################
	EX	HL,DE			;###############################
	ld	a, (de)
	ld	(bpt1_op), a
	EX	HL,DE			;Z80 CODE ld	de, (bpt2_adr)
	LD	HL,(bpt2_adr)		;###############################
	EX	HL,DE			;###############################
	ld	a, (de)
	ld	(bpt2_op), a

	JP	SM1		;JR	SM1

;;;
;;; LOAD HEX file
;;;

LOADH:
	; clear brk point
	xor	a
	ld	(bpt1_f), a
	ld	(bpt2_f), a
	
	INC	HL
	CALL	SKIPSP
	CALL	RDHEX
	CALL	SKIPSP
	LD	A,(HL)
	OR	A
	JP	NZ,ERR

	LD	A,C
	OR	A
	JP	NZ,LH0		;JR	NZ,LH0

	LD	DE,0		;Offset
LH0:
	CALL	CONIN
	CP	'S'
	JP	Z,LHS0		;JR	Z,LHS0
LH1:
	CP	':'
	JP	Z,LHI0		;JR	Z,LHI0
LH2:
	;; Skip to EOL
	CP	CR
	JP	Z,LH0		;JR	Z,LH0
	CP	LF
	JP	Z,LH0		;JR	Z,LH0
LH3:
	CALL	CONIN
	JP	LH2		;JR	LH2

LHI0:
	ld	a, '.'
	call	CONOUT

	CALL	HEXIN
	LD	C,A		; Checksum
	LD	B,A		; Length

	CALL	HEXIN
	LD	H,A		; Address H
	ADD	A,C
	LD	C,A

	CALL	HEXIN
	LD	L,A		; Address L
	ADD	A,C
	LD	C,A

	;; Add offset
	ADD	HL,DE

	CALL	HEXIN
	LD	(RECTYP),A
	ADD	A,C
	LD	C,A		; Checksum

	LD	A,B
	OR	A
	JP	Z,LHI3		;JR	Z,LHI3
LHI1:
	CALL	HEXIN
	PUSH	AF
	ADD	A,C
	LD	C,A		; Checksum

	LD	A,(RECTYP)
	OR	A
	JP	NZ,LHI20	;JR	NZ,LHI20

	POP	AF
	LD	(HL),A
	INC	HL
	JP	LHI2		;JR	;LHI2
LHI20:
	POP	AF
LHI2:
	DEC	B			;Z80 CODE #####	DJNZ	LHI1
	JP	NZ,LHI1			;##############################
LHI3:
	CALL	HEXIN
	ADD	A,C
	JP	NZ,LHIE		;JR	NZ,LHIE	; Checksum error
	LD	A,(RECTYP)
	OR	A
	JP	Z,LH3
	JP	WSTART
LHIE:
	LD	HL,IHEMSG
	CALL	STROUT
	JP	WSTART
	
LHS0:
	CALL	CONIN
	LD	(RECTYP),A

	CALL	HEXIN
	LD	B,A		; Length+3
	LD	C,A		; Checksum

	CALL	HEXIN
	LD	H,A
	ADD	A,C
	LD	C,A
	
	CALL	HEXIN
	LD	L,A
	ADD	A,C
	LD	C,A

	ADD	HL,DE

	DEC	B
	DEC	B
	DEC	B
	JP	Z,LHS3		;JR	Z,LHS3
LHS1:
	CALL	HEXIN
	PUSH	AF
	ADD	A,C
	LD	C,A		; Checksum

	LD	A,(RECTYP)
	CP	'1'
	JP	NZ,LHS2		;JR	NZ,LHS2

	POP	AF
	LD	(HL),A
	INC	HL
	JP	LHS20		;JR	LHS20
LHS2:
	POP	AF
LHS20:
	DEC	B			;Z80 CODE ##### DJNZ	LHS1
	JP	NZ,LHS1			;###############################
LHS3:
	CALL	HEXIN
	ADD	A,C
	CP	0FFH
	JP	NZ,LHSE		;JR	NZ,LHSE

	LD	A,(RECTYP)
	CP	'7'
	JP	Z,LHSR		;JR	Z,LHSR
	CP	'8'
	JP	Z,LHSR		;JR	Z,LHSR
	CP	'9'
	JP	Z,LHSR		;JR	Z,LHSR
	JP	LH3
LHSE:
	LD	HL,SHEMSG
	CALL	STROUT
LHSR:
	JP	WSTART
	
;;;
;;; SAVE HEX file
;;;

SAVEH:
	INC	HL
	LD	A,(HL)
	CP	'I'
	JP	Z,SH0		;JR	Z,SH0
	CP	'S'
	JP	NZ,SH1		;JR	NZ,SH1
SH0:
	INC	HL
	LD	(HEXMOD),A
SH1:
	CALL	SKIPSP
	CALL	RDHEX
	OR	A
	JP	Z,SHE		;JR	Z,SHE
	EX	HL,DE			;PUSH	DE
	LD	(tmp_HL),HL		;###############################
	EX	HL,DE			;###############################
;	POP	IX			;Z80 CODE NOT TRANSLATED YET	; IX = Start address
	CALL	SKIPSP
	LD	A,(HL)
	CP	','
	JP	NZ,SHE		;JR	NZ,SHE
	INC	HL
	CALL	SKIPSP
	CALL	RDHEX		; DE = End address
	OR	A
	JP	Z,SHE		;JR	Z,SHE
	CALL	SKIPSP
	LD	A,(HL)
	OR	A
	JP	Z,SH2		;JR	Z,SH2
SHE:
	JP	ERR

SH2:
;	PUSH	IX			;Z80 CODE oush ix ##############
	LD	HL,(tmp_HL)		;POP	HL #####################
	EX	DE,HL
	INC	HL
	OR	A
	call	sbc_hlde		;Z80 CODE SBC	HL,DE		; HL = Length
SH3:
	CALL	SHL00
	LD	A,H
	OR	L
	JP	NZ,SH3		;JR	NZ,SH3

	LD	A,(HEXMOD)
	CP	'I'
	JP	NZ,SH4		;JR	NZ,SH4
	;; End record for Intel HEX
	LD	HL,IHEXER
	CALL	STROUT
	JP	WSTART
SH4:
	;; End record for Motorola S record
	LD	HL,SRECER
	CALL	STROUT
	JP	WSTART

SHL00:
	LD	C,16
	LD	A,H
	OR	A
	JP	NZ,SHL0		;JR	NZ,SHL0
	LD	A,L
	CP	C
	JP	NC,SHL0		;JR	NC,SHL0
	LD	C,A
SHL0:
	LD	B,0
	OR	A
	call	sbc_hlbc		;Z80 CODE SBC	HL,BC #####
	LD	B,C

	LD	A,(HEXMOD)
	CP	'I'
	JP	NZ,SHLS		;JR	NZ,SHLS

	;; Intel HEX
	LD	A,':'
	CALL	CONOUT

	LD	A,B
	CALL	HEXOUT2		; Length
	LD	C,B		; Checksum

	LD	A,D
	CALL	HEXOUT2
	LD	A,D
	ADD	A,C
	LD	C,A
	
	LD	A,E
	CALL	HEXOUT2
	LD	A,E
	ADD	A,C
	LD	C,A
	
	XOR	A
	CALL	HEXOUT2
SHLI0:
	LD	A,(DE)
	PUSH	AF
	CALL	HEXOUT2
	POP	AF
	ADD	A,C
	LD	C,A

	INC	DE
	DEC	B			;Z80 CODE #####	DJNZ	SHLI0
	JP	NZ,SHLI0		;############################

	LD	A,C
	CPL				;Z80 CODE NEG
	INC	A			;flags may be wrong, it's OK here
	CALL	HEXOUT2
	JP	CRLF

SHLS:
	;; Motorola S record
	LD	A,'S'
	CALL	CONOUT
	LD	A,'1'
	CALL	CONOUT

	LD	A,B
	ADD	A,2+1		; DataLength + 2(Addr) + 1(Sum)
	LD	C,A
	CALL	HEXOUT2

	LD	A,D
	CALL	HEXOUT2
	LD	A,D
	ADD	A,C
	LD	C,A
	
	LD	A,E
	CALL	HEXOUT2
	LD	A,E
	ADD	A,C
	LD	C,A
SHLS0:
	LD	A,(DE)
	PUSH	AF
	CALL	HEXOUT2		; Data
	POP	AF
	ADD	A,C
	LD	C,A

	INC	DE
	DEC	B			;Z80 CODE ##### DJNZ	SHLS0
	JP	NZ,SHLS0		;###############################

	LD	A,C
	CPL
	CALL	HEXOUT2
	JP	CRLF

;;;
;;; Register
;;;

REG:
	INC	HL
	CALL	SKIPSP
	OR	A
	JP	NZ,RG0		;JR	NZ,RG0
	CALL	RDUMP
	JP	WSTART
RG0:
	EX	DE,HL
	LD	HL,RNTAB
RG1:
	CP	(HL)
	JP	Z,RG2		;JR	Z,RG2		; Character match
	LD	C,A
	INC	HL
	LD	A,(HL)
	OR	A
	JP	Z,RGE		;JR	Z,RGE		; Found end mark
	LD	A,C
	LD	BC,5
	ADD	HL,BC		; Next entry
	JP	RG1		;JR	RG1
RG2:
	INC	HL
	LD	A,(HL)
	CP	0FH		; Link code
	JP	NZ,RG3		;JR	NZ,RG3
	;; Next table
	INC	HL
	LD	C,(HL)
	INC	HL
	LD	H,(HL)
	LD	L,C
	INC	DE
	LD	A,(DE)
	JP	RG1		;JR	RG1
RG3:
	OR	A
	JP	Z,RGE		;JR	Z,RGE		; Found end mark

	LD	C,(HL)		; LD C,A???
	INC	HL
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	PUSH	DE		; Reg storage address
	INC	HL
	LD	A,(HL)
	INC	HL
	LD	H,(HL)
	LD	L,A		; HL: Reg name
	CALL	STROUT
	LD	A,'='
	CALL	CONOUT

	LD	A,C
	AND	07H
	CP	1
	JP	NZ,RG4		;JR	NZ,RG4
	;; 8 bit register
	POP	HL
	LD	A,(HL)
	PUSH	HL
	CALL	HEXOUT2
	JP	RG5		;JR	RG5
RG4:
	;; 16 bit register
	POP	HL
	PUSH	HL
	INC	HL
	LD	A,(HL)
	CALL	HEXOUT2
	DEC	HL
	LD	A,(HL)
	CALL	HEXOUT2
RG5:
	LD	A,' '
	CALL	CONOUT
	PUSH	BC		; C: reg size
	CALL	GETLIN
	CALL	SKIPSP
	CALL	RDHEX
	OR	A
	JP	Z,RGR		;JR	Z,RGR
	POP	BC
	POP	HL
	LD	A,C
	CP	1
	JP	NZ,RG6		;JR	NZ,RG6
	;; 8 bit register
	LD	(HL),E
	JP	RG7		;JR	RG7
RG6:
	;; 16 bit register
	LD	(HL),E
	INC	HL
	LD	(HL),D
RG7:
RGR:
	JP	WSTART
RGE:
	JP	ERR

RDUMP:
	LD	HL,RDTAB
RD0:
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	INC	HL
	LD	A,D
	OR	E
	JP	Z,CRLF		; End
	push	de
	EX	DE,HL
	CALL	STROUT	; print name of register
	EX	DE,HL
	pop	de

; flag check
	ld	a, RDSF_H
	cp	d
	jp	nz, rd101	;jr	nz, rd101
	ld	a, RDSF_L
	cp	e
	jp	nz, rd101	;jr	nz, rd101
	jp	rd20		;jr	rd20

rd101:
	ld	a, RDSFX_H
	cp	d
	jp	nz, rd10	;jr	nz, rd10
	ld	a, RDSFX_L
	cp	e
	jp	z, rd20		;jr	z, rd20

rd10:
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	INC	HL
	LD	A,(HL)
	INC	HL
	EX	DE,HL
	CP	1
	JP	NZ,RD1		;JR	NZ,RD1
	;; 1 byte
	LD	A,(HL)
	CALL	HEXOUT2
	EX	DE,HL
	JP	RD0		;JR	RD0
RD1:
	;; 2 byte
	INC	HL
	LD	A,(HL)
	CALL	HEXOUT2		; High byte
	DEC	HL
	LD	A,(HL)
	CALL	HEXOUT2		; Low byte
	EX	DE,HL
	JP	RD0		;JR	RD0

; make flag image string
rd20:
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	INC	HL
	ld	a, (de)		; get flag values

	push	de
	push	hl

; make flag image

	ld	hl, F_bit
	ld	bc, F_bit_on
	ld	de, F_bit_off
	push	af		; adjustment SP. DO NOT DELETE THIS LINE!

flg_loop:
	pop	af		; "push af" before loop back
	ADD	A,A			;Z80 CODE sla	a ##############
	jp	nc, flg_off	;jr	nc, flg_off
	push	af
	ld	a, (bc)
	ld	(hl), a
	pop	af
	jp	flg_nxt		;jr	flg_nxt
flg_off:
	push	af
	ld	a, (de)
	ld	(hl), a
	pop	af

flg_nxt:
	inc	bc
	inc	de
	inc	hl

	push	af
	ld	a, (hl)
	or	a		;check delimiter
	jp	nz, flg_loop	;jr	nz, flg_loop

	pop	af		; restore stack position
	ld	hl, F_bit
	call	STROUT		; print flag register for bit imaze

	pop	hl
	pop	de

	inc	hl
	JP	RD0		;JR	RD0

F_bit_on:	db	"SZ.H.PNC"
F_bit_off:	db	"........"

;--------------------------------------------------
;
; Minimal assemble
;
;--------------------------------------------------

dm_bit	equ	80h

ope_cds:
; 4 bytes string
	db	"INIR",	dm_bit | 57
	db	"INDR",	dm_bit | 59
	db	"OUTI",	dm_bit | 61
	db	"OTIR",	dm_bit | 62
	db	"OUTD",	dm_bit | 63
	db	"OTDR",	dm_bit | 64
	db	"HALT",	dm_bit | 51
	db	"RETI",	dm_bit | 47
	db	"RETN",	dm_bit | 48
	db	"DJNZ",	dm_bit | 44
	db	"CALL",	dm_bit | 45
	db	"CPDR",	dm_bit | 40
	db	"LDIR",	dm_bit | 03
	db	"LDDR",	dm_bit | 05
	db	"PUSH",	dm_bit | 08
	db	"RLCA",	dm_bit | 10
	db	"RRCA",	dm_bit | 14
	db	"CPIR",	dm_bit | 38

; 3 bytes string
	db	"LDI",	dm_bit | 02
	db	"LDD",	dm_bit | 04
	db	"EXX",	dm_bit | 07
	db	"POP",	dm_bit | 09
	db	"RLA",	dm_bit | 11
	db	"RLC",	dm_bit | 12
	db	"RRA",	dm_bit | 15
	db	"RRC",	dm_bit | 16
	db	"SLA",	dm_bit | 18
	db	"SRA",	dm_bit | 19
	db	"SRL",	dm_bit | 20
	db	"ADD",	dm_bit | 21
	db	"ADC",	dm_bit | 22
	db	"INC",	dm_bit | 23
	db	"SUB",	dm_bit | 24
	db	"SBC",	dm_bit | 25
	db	"DEC",	dm_bit | 26
	db	"AND",	dm_bit | 27
	db	"XOR",	dm_bit | 29
	db	"CPL",	dm_bit | 30
	db	"NEG",	dm_bit | 31
	db	"CCF",	dm_bit | 32
	db	"SCF",	dm_bit | 33
	db	"BIT",	dm_bit | 34
	db	"SET",	dm_bit | 35
	db	"RES",	dm_bit | 36
	db	"CPI",	dm_bit | 37
	db	"CPD",	dm_bit | 39
	db	"RET",	dm_bit | 46
	db	"RST",	dm_bit | 49
	db	"NOP",	dm_bit | 50
	db	"INI",	dm_bit | 56
	db	"IND",	dm_bit | 58
	db	"OUT",	dm_bit | 60
	db	"DAA",	dm_bit | 65
	db	"RLD",	dm_bit | 66
	db	"RRD",	dm_bit | 67
	db	"ORG",	dm_bit | 68

; 2 bytes string
	db	"EX",	dm_bit | 06
	db	"RL",	dm_bit | 13
	db	"RR",	dm_bit | 17
	db	"OR",	dm_bit | 28
	db	"CP",	dm_bit | 41
	db	"JP",	dm_bit | 42
	db	"JR",	dm_bit | 43
	db	"IM",	dm_bit | 54
	db	"IN",	dm_bit | 55
	db	"DI",	dm_bit | 52
	db	"EI",	dm_bit | 53
	db	"LD",	dm_bit | 01
	db	"DB",	dm_bit | 69
	db	"DW",	dm_bit | 70
	db	0	; delimiter

operand_cds:
; 4 bytes string
	db	"(BC)",	dm_bit | 26
	db	"(DE)",	dm_bit | 27
	db	"(HL)",	dm_bit | 28
	db	"(IX)",	dm_bit | 29
	db	"(IY)",	dm_bit | 30
	db	"(SP)",	dm_bit | 32

; 3 bytes string
	db	"(C)",	dm_bit | 31
	db	"AF'",	dm_bit | 3

; 2 bytes string
	db	"AF",	dm_bit | 2
	db	"BC",	dm_bit | 5
	db	"DE",	dm_bit | 8
	db	"HL",	dm_bit | 11
	db	"IX",	dm_bit | 14
	db	"IY",	dm_bit | 15
	db	"SP",	dm_bit | 16
	db	"NZ",	dm_bit | 20
	db	"NC",	dm_bit | 21
	db	"PO",	dm_bit | 23
	db	"PE",	dm_bit | 24

; 1 bytes string
	db	"A",  	dm_bit | 1
	db	"B",	dm_bit | 4
	db	"C",	dm_bit | 6
	db	"D",	dm_bit | 7
	db	"E",	dm_bit | 9
	db	"H",	dm_bit | 10
	db	"L",	dm_bit | 12
	db	"I",	dm_bit | 13
	db	"R",	dm_bit | 17
	db	"Z",	dm_bit | 18
	db	"M",	dm_bit | 19
	db	"P",	dm_bit | 22
	db	0	; delimiter

opr_cd1:
	db	"(IX+",	dm_bit | 33
	db	"(IY+",	dm_bit | 34
	db	"(IX-",	dm_bit | 36
	db	"(IY-",	dm_bit | 37
	db	0	; delimiter

;----------------------------------------------
; A[<address>] : input line assemble mode
; A[.]<cr> : exit assemble mode
;----------------------------------------------
line_asm:
	inc	hl
	call	skp_sp
	or	a
	jp	z, skpa_adr	;jr	z, skpa_adr
	call	RDHEX		; get DE: address
	jp	c, ERR
	ex	de,hl		;####	ld	(asm_adr), de	; set start address
	ld	(asm_adr),hl	;##########################
	ex	de,hl		;##########################
	call	skp_sp
	or	a
	jp	nz, ERR
	
	; init work area
skpa_adr:
	ld	hl, (asm_adr)
	ld	(tasm_adr), hl
	
	; print address

next_asm:
	xor	a
	ld	(element_cnt), a
	ld	(opc_cd), a
	ld	(opr1_cd), a
	ld	(opr2_cd), a

	ld	hl, (tasm_adr)
	call	HEXOUT4
	ld	hl, dsap
	call	STROUT

;-----------------------------
; get a line from console
;-----------------------------
	ld	hl, ky_flg
	push	af			;z80 code ##############
	ld	a,(hl)			;set	NO_LF>>1, (hl) ;set 1,(hl)
	or	NO_LF			;#######################
	ld	(hl),a			;#######################
	pop	af			;#######################

	call	GETLIN

	push	hl
	ld	hl, ky_flg
	push	af			;z80 code ##########
	ld	a,(hl)			;###	res	NO_LF>>1, (hl)
	and	11111101b		;###################
	ld	(hl),a			;###################
	pop	af			;#######################
	pop	hl

	call	recorrect
	ld	a, (hl)
	cp	ESC
	jp	z, ext_asm	;jr	z, ext_asm
	cp	'.'
	jp	z, ext_asm	;jr	z, ext_asm
	or	a		; NULL?
	jp	nz, cont_asm
	call	lf_out
	jp	next_asm	;jr	next_asm

ext_asm:
	call	lf_out
	ld	hl, (tasm_adr)
	ld	(asm_adr), hl	; save new address
	jp	WSTART		; exit assemble mode

lf_out:
	ld	a, LF
	jp	CONOUT

asm_err:
	ld	hl, com_errm
asm_err1:
	call	lf_out
	call	STROUT
	ld	hl, ERRMSG
	call	STROUT
	jp	next_asm	;jr	next_asm

;-------------------------------------------------
; post operation
;-------------------------------------------------
cout_sp:
	ld	a, (opc_cd)
	cp	68
	jp	z, skp_outm	;jr	z, skp_outm
	cp	69
	jp	z, dmp_db	;jr	z, dmp_db
	cp	70
	jp	z, dmp_dw	;jr	z, dmp_dw

	ld	a, ' '
	ld	c, BUFLEN-2	; clear
	ld	hl, line_buf	; disassemble string buffer
cout_sp1:
	ld	(hl), a
	inc	hl
	dec	c
	jp	nz, cout_sp1	;jr	nz, cout_sp1
	ld	a, CR
	ld	(hl), a
	inc	hl
	xor	a
	ld	(hl), a		; set delimitor
	call	STROUT

	ld	hl, (tasm_adr)	; disassemble address
	ld	de, line_buf	; disassemble string buffer
	push	de
	call	get_disasm_st
	pop	hl
	call	STROUT		; cousole out result inline assemble
cout_sp2:
	ld	hl, (tasm_adr)	; disassemble address
	call	setbytec
	ld	(tasm_adr), hl	; next address
	jp	next_asm

skp_outm:
	call	lf_out
	jp	cout_sp2	;jr	cout_sp2

dmp_db:
	call	setdsadr
	call	setbytec
	ld	(deaddr), hl
	jp	prtdmp		;jr	prtdmp

dmp_dw:
	call	setdsadr
	inc	hl
	inc	hl
	ld	(deaddr), hl
prtdmp:	
	call	dpm
	jp	cout_sp2	;jr	cout_sp2

setdsadr:
	call	lf_out
	ld	hl, (tasm_adr)
	ld	(dsaddr), hl
	ret

setbytec:
	ld	a, (byte_count)
	ld	c, a
	xor	a
	ld	b, a
	add	hl, bc
	ret

;----------------------------
; analize input data
;----------------------------
cont_asm:
	call	analize_input
	jp	c, asm_err		;error

; make machine code

	ld	a, (element_cnt)
	cp	1
	jp	z, mk_e1
	cp	2
	jp	z, mk_e2

;333333333333333333333333333333333333333
;
; make machine code,
; element_cnt = 3 (ex. LD SP, HL)
;
;333333333333333333333333333333333333333

el3_um	equ	el3_stbe - el3_stb

mk_e3:	ld	a, (opc_cd)
	ld	bc, el3_um
	ld	hl, el3_stb
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, asm_err

	ld	hl, el3_jtb

jp_each:
	add	hl, bc		; offset : BC = BC * 2
	add	hl, bc		; HL = ent_el2 + offset
	ld	c, (hl)
	inc	hl
	ld	b, (hl)
	push	bc
	ret

el3_stb:	db	1			; normal, DD, FD, ED
		db	6, 21 			; normal, DD, FD
		db	22, 25			; normal, DD, FD, ED
		db	43 			; normal
		db	55, 60			; normal, ED
		db	42, 45			; normal
		db	34, 36, 35 		; CB, DD, FD
el3_stbe:	

el3_jtb:	dw	el3_35
		dw	el3_36
		dw	el3_34
		dw	el3_45
		dw	el3_42
		dw	el3_60
		dw	el3_55
		dw	el3_43
		dw	el3_25
		dw	el3_22
		dw	el3_21
		dw	el3_6
		dw	el3_1


;
; SET section
;
; r_hl_nn:	db	1, 4, 6, 7, 9, 10, 12, 28

set_mcb:	db	0C0h, 0C8h, 0D0h, 0D8h, 0E0h, 0E8h, 0F0h, 0F8h
set_xyt:	db	0C6h, 0CEh, 0D6h, 0DEh, 0E6h, 0EEh, 0F6h, 0FEh

el3_35: ; SET  ( CB, DD, FD )

	ld	hl, set_mcb
	ld	(cb_mcw), hl
	ld	hl, set_xyt
	ld	(cb_xyw), hl
	jp	bit_res_set	;jr	bit_res_set

;
; RES section
;
; r_hl_nn:	db	1, 4, 6, 7, 9, 10, 12, 28

res_mcb:	db	80h, 88h, 90h, 98h, 0A0h, 0A8h, 0B0h, 0B8h
res_xyt:	db	86h, 8Eh, 96h, 9Eh, 0A6h, 0AEh, 0B6h, 0BEh

el3_36: ; RES  ( CB, DD, FD )

	ld	hl, res_mcb
	ld	(cb_mcw), hl
	ld	hl, res_xyt
	ld	(cb_xyw), hl
	jp	bit_res_set	;jr	bit_res_set

;
; BIT section
;
; r_hl_nn:	db	1, 4, 6, 7, 9, 10, 12, 28

bit_mcbn	equ	bit_mcbe - bit_mcb
bit_mcb:	db	40h, 48h, 50h, 58h, 60h, 68h, 70h, 78h
bit_mcbe:
bit_xyt:	db	46h, 4Eh, 56h, 5Eh, 66h, 6Eh, 76h, 7Eh


el3_34: ; BIT  ( CB, DD, FD )

	ld	hl, bit_mcb
	ld	(cb_mcw), hl
	ld	hl, bit_xyt
	ld	(cb_xyw), hl

bit_res_set:
	ld	a, (opr1_cd)
	cp	25
	jp	nz, asm_err

	ld	hl, (opr_num25)
	ld	a, h
	or	a
	jp	nz, asm_err
	ld	a, l
	cp	8
	jp	nc, asm_err

	; HL : 0 - 7

	ld	a, (opr2_cd)
	cp	33
	jp	z, bit_ixp	;jr	z, bit_ixp
	cp	36
	jp	z, bit_ixm	;jr	z, bit_ixm
	cp	34
	jp	z, bit_iyp	;jr	z, bit_iyp
	cp	37
	jp	z, bit_iym	;jr	z, bit_iym
;
; CB : bit n, [r | (hl)]
;
	push	de			;added #########################
	ex	de,hl			;z80 code ld	bc, (cb_mcw)####
	ld	hl,(cb_mcw)		;###############################
	ex	de,hl			;###############################
	add	hl,de			;add	hl,bc ##################
	pop	de			;###############################
	ld	d, (hl)		; get MC base

	ld	a, (opr2_cd)
	ld	hl, r_hl_nn
	ld	bc, bit_mcbn
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, asm_err

	ld	a, c
	cp	7
	jp	z, el3_341	;jr	z, el3_341

	ld	a, 6
	sub	c
el3_341:
	add	a, d		; make 2nd MC
	jp	el2_130

	; HL : 0 - 7

bit_ixp:
	ld	a, (opr_num33)
bit_ixp1:
	ld	e, a
	ld	d, 0DDh

; adjust I/F
; d: 1st MC, a: 2nd MC, c: 3rd MC, e: 4th MC

el3_342:
	push	de			;added #########################
	ex	de,hl			;z80 code ld	bc, (cb_xyw)####
	ld	hl,(cb_xyw)		;###############################
	ex	de,hl			;###############################
	add	hl,de			;add	hl,bc ##################
	pop	de			;###############################
	ld	c, e		; adjust I/F
	ld	e, (hl)		; 4th MC
	ld	a, 0CBh
	jp	mc_end41	; save 4byte MC

bit_ixm:
	ld	a, (opr_num36)
	jp	bit_ixp1	;jr	bit_ixp1

bit_iyp:
	ld	a, (opr_num34)
bit_iyp1:
	ld	e, a
	ld	d, 0FDh
	jp	el3_342		;jr	el3_342

bit_iym:
	ld	a, (opr_num37)
	jp	bit_iyp1	;jr	bit_iyp1

;
; CALL section
; ret_no:	db	6, 18, 19, 20, 21, 22, 23, 24
;
call_mc1t:	db	0ECh, 0E4h, 0F4h, 0D4h, 0C4h, 0FCh, 0CCh, 0DCh

el3_45: ; CALL ( normal )

	ld	de, call_mc1t
	jp	el3_421		;jr	el3_421
;
; JP section
; ret_no:	db	6, 18, 19, 20, 21, 22, 23, 24
;
jp_mc1tm	equ	jp_mc1te - jp_mc1t
jp_mc1t:	db	0EAh, 0E2h, 0F2h, 0D2h, 0C2h, 0FAh, 0CAh, 0DAh
jp_mc1te:

el3_42: ; JP   ( normal )

	ld	de, jp_mc1t

el3_421:
	ld	hl, ret_no
	ld	bc, jp_mc1tm
	ld	a, (opr1_cd)
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, asm_err

	ld	a, (opr2_cd)
	cp	25
	jp	nz, asm_err

	ld	h, d		; DE : MC target table
	ld	l, e		; DE : MC target table
	add	hl, bc
	ld	a, (hl)		; get 1st MC
	jp	el2_451

;
; OUT section
;
in_selm		equ	ld_e2_tbl1 - in_selt

el3_60: ; OUT  ( normal, ED )

	ex	de,hl			;z80 code ld bc, (opr1_cd) ; c = (opr1_cd), b = (opr2_cd)
	ld	hl,(opr1_cd)		;###############################
	ex	de,hl			;###############################
	ld	b,d
	ld	c,e
	ld	de, 41D3H
	jp	el3_550		;jr	el3_550

;
; IN section
;
el3_55: ; IN   ( normal, ED )

	ld	de, 40DBH
	ld	a, (opr1_cd)
	ld	b, a
	ld	a, (opr2_cd)
	ld	c, a

el3_550:
	ld	a, c
	cp	35
	jp	z, in_a_nn	;jr	z, in_a_nn
	cp	31
	jp	nz, asm_err

	ld	a, b
	cp	1
	jp	z, el3_551	;jr	z, el3_551	; passing search
	
	ld	hl, in_selt
	ld	bc, in_selm
	call	mkmc_sh
	jp	c, asm_err
	ld	a, d
	jp	el3_552		;jr	el3_552
	
el3_551:
	ld	a, d
	add	a, 38h		; get 2nd MC
el3_552:
	jp	el2_5411

in_a_nn:
	ld	hl, (opr_num35)
	ld	a, h
	or	a
	jp	nz, asm_err
	ld	a, l
	push	af		; adjust I/F
	ld	a, e		; 1st MC
	jp	el3_431		;jr	el3_431		; save 2byte MC (el2_441)

;
; JR section
;
jr_cct:	db	20, 18, 21, 6

el3_43: ; JR   ( normal )

	ld	hl, jr_cct
	ld	bc, 4
	ld	d, 20h		; base MC
	ld	a, (opr1_cd)
	call	mkmc_sh
	jp	c, asm_err

	ld	a, (opr2_cd)
	cp	25		; number?
	jp	nz, asm_err

	call	calc_reladr
	jp	c, asm_err
	
	push	af		; adjust I/F
	ld	a, d		; adjust I/F
el3_431:
	jp	el2_441		; save 2byte MC
;
; hl : search table
; bc : loop counter
; d  : base MC 
;
; output:
;	  CF=1 : error
;	  CF=0 : D = MC
mkmc_sh:
	cp	(hl)		;Z80 code cpi #############
	inc	hl		;##########################
	dec	bc		;##########################
	jp	z, mkmc_shed	;jr	z, mkmc_shed

	ld	e, a	; save a
	ld	a, d
	add	a, 8
	ld	d, a

	ld	a, c
	or	b
	ld	a, e	; restore a
	jp	nz, mkmc_sh	;jr	nz, mkmc_sh
	scf
	ret

mkmc_shed:
	ret

;
; ADC section
;

el3_25: ; SBC  ( normal, DD, FD, ED )

	ld	de, 429fh
	ld	a, 0DEh
	PUSH	HL			;#### ADDED ####################
	PUSH	AF			;Z80 CODE EX	AF,AF' #########
	LD	HL,(alt_AF)		;###############################
	PUSH	HL			;###############################
	POP	AF			;###############################
	POP	HL			;###############################
	LD	(alt_AF),HL		;###############################
	POP	HL			;###############################
	jp	el3_221		;jr	el3_221
;
; ADC section
;
; defined other section
; r_hl_nn:	db	1, 4, 6, 7, 9, 10, 12, 28
; ld_rr_:	db	25
; ex_sr_:	db	33, 36, 34, 37
; r_hl_nne:

adc_MC	equ	ex_sr_ - r_hl_nn
adc_3MC	equ	r_hl_nne - ex_sr_

el3_22: ; ADC  ( normal, DD, FD, ED )

	ld	de, 4a8fh
	ld	a, 0CEh
	PUSH	HL			;#### ADDED ####################
	PUSH	AF			;Z80 CODE EX	AF,AF' #########
	LD	HL,(alt_AF)		;###############################
	PUSH	HL			;###############################
	POP	AF			;###############################
	POP	HL			;###############################
	LD	(alt_AF),HL		;###############################
	POP	HL			;###############################

el3_221:
	ld	a, (opr1_cd)
	cp	1		; A?
	jp	z, adc_a_	;jr	z, adc_a_
	cp	11		; HL?
	jp	nz, asm_err

; adc hl,[ BC | DE | HL | SP ]

	ld	a, (opr2_cd)
	cp	5
	jp	z, adc_hl_bc	;jr	z, adc_hl_bc
	cp	8
	jp	z, adc_hl_de	;jr	z, adc_hl_de
	cp	11
	jp	z, adc_hl_hl	;jr	z, adc_hl_hl
	cp	16
	jp	nz, asm_err

adc_hl_sp:
	ld	a, 30h
	jp	adc_hl_		;jr	adc_hl_
adc_hl_hl:
	ld	a, 20h
	jp	adc_hl_		;jr	adc_hl_
adc_hl_de:
	ld	a, 10h
	jp	adc_hl_		;jr	adc_hl_
adc_hl_bc:
	xor	a
adc_hl_:
	add	a, d		; make 2nd MC

	jp	el2_5411

adc_a_:
	ld	a, (opr2_cd)
	ld	hl, r_hl_nn
	ld	bc, adc_MC
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, chk_adc3mc	;jr	nz, chk_adc3mc

	ld	a, c
	cp	8
	jp	z, adc_a_a	;jr	z, adc_a_a
	or	a
	jp	z, adc_a_n	;jr	z, adc_a_n
	ld	a, e
	sub	c		; make MC 1
adc_a_x:
	jp	st_mc11
	
adc_a_n:
	ld	hl, (opr_num25)
	PUSH	HL			;#### ADDED ####################
	PUSH	AF			;Z80 CODE EX	AF,AF' #########get 1st MC'
	LD	HL,(alt_AF)		;###############################
	PUSH	HL			;###############################
	POP	AF			;###############################
	POP	HL			;###############################
	LD	(alt_AF),HL		;###############################
	POP	HL			;###############################
	ld	d, a		; adjust I/F
	jp	ld_r251

adc_a_a:
;	inc	e
	ld	a, e
	jp	adc_a_x		;jr	adc_a_x

chk_adc3mc:
	ld	hl, ex_sr_
	ld	bc, adc_3MC
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, asm_err

	ld	hl, opr_num37
	add	hl, bc
	ld	d, (hl)		; 3rd MC

	ld	a, c
	ld	c, 0FDH		; 1st MC
	cp	2
	jp	c, adc_fd	;jr	c, adc_fd

	ld	c, 0DDH		; 1st MC

adc_fd:
	dec	e		; get 2nd MC
	jp	ld_bc_n42	; save 3MC
;
; ADD section
;
el3_21: ; ADD  ( normal, DD, FD )

	ld	de, 0987h
	ld	a, 0C6h
	PUSH	HL			;#### ADDED ####################
	PUSH	AF			;Z80 CODE EX	AF,AF' #########
	LD	HL,(alt_AF)		;###############################
	PUSH	HL			;###############################
	POP	AF			;###############################
	POP	HL			;###############################
	LD	(alt_AF),HL		;###############################
	POP	HL			;###############################

	ld	a, (opr2_cd)
	ld	b, a
	ld	a, (opr1_cd)
	cp	1
	jp	z, adc_a_	;jr	z, adc_a_
	cp	11
	jp	z, add_hl_	;jr	z, add_hl_
	cp	14
	jp	z, add_ix_	;jr	z, add_ix_
	cp	15
	jp	nz, asm_err
;;
;;
add_iy_:
	ld	c, 0FDh
	jp	el3_211		;jr	el3_211
;;
;;
add_ix_:
	ld	c, 0DDh
el3_211:
	ld	a, b
	cp	5
	jp	z, add_xy_bc	;jr	z, add_xy_bc
	cp	8
	jp	z, add_xy_de	;jr	z, add_xy_de
	cp	14
	jp	z, add_ix_ix	;jr	z, add_ix_ix
	cp	15
	jp	z, add_iy_iy
	cp	16
	jp	nz, asm_err
add_xy_sp:
	ld	a, 39h
el3_212:
	push	af		; adjust I/F
	ld	a, c
	jp	el2_441

add_xy_bc:
	ld	a, 9h
	jp	el3_212		;jr	el3_212
add_xy_de:
	ld	a, 19h
	jp	el3_212		;jr	el3_212
add_ix_ix:
	ld	a, c
	cp	0ddh
	jp	nz, asm_err
add_xy_xy:
	ld	a, 29h
	jp	el3_212		;jr	el3_212
add_iy_iy:
	ld	a, c
	cp	0fdh
	jp	nz, asm_err
	jp	add_xy_xy	;jr	add_xy_xy

;;
;;
add_hl_:
	ld	a, b
	cp	5
	jp	z, add_hl_bc	;jr	z, add_hl_bc
	cp	8
	jp	z, add_hl_de	;jr	z, add_hl_de
	cp	11
	jp	z, add_hl_hl	;jr	z, add_hl_hl
	cp	16
	jp	nz, asm_err
add_hl_sp:
	ld	a, 39h
	jp	adc_a_x
add_hl_bc:
	ld	a, 09h
	jp	adc_a_x
add_hl_de:
	ld	a, 19h
	jp	adc_a_x
add_hl_hl:
	ld	a, 29h
	jp	adc_a_x

;
; EX section
;

el3_6:  ; EX   ( normal, DD, FD )

	ld	a, (opr2_cd)
	ld	b, a
	ld	a, (opr1_cd)
	cp	2
	jp	z, ex_af_	;jr	z, ex_af_
	cp	8
	jp	z, ex_de_	;jr	z, ex_de_
	cp	32
	jp	nz, asm_err

; ex (sp),
	ld	a, b
	ld	e, 0E3h

	cp	11		; hl
	jp	z, ex_sp_hl	;jr	z, ex_sp_hl
	cp	14
	ld	d, 0ddh
	jp	z, ex_sp_ix	;jr	z, ex_sp_ix
	cp	15
	jp	nz, asm_err
	ld	d, 0fdh
ex_sp_ix:
	ld	a, e
	push	af		; adjust I/F
	ld	a, d
	jp	el2_441

ex_sp_hl:
	ld	a, e
	jp	el3_61		;jr	el3_61

ex_af_:
	ld	a, b
	cp	3
	jp	nz, asm_err
	ld	a, 08h		; 1st MC
	jp	el3_61		;jr	el3_61

ex_de_:
	ld	a, b
	cp	11		; hl
	jp	nz, asm_err
	ld	a, 0ebh		; 1st MC
el3_61:
	jp	st_mc11

;
; LD section
;

; ld_en1 = 7 (1, 4, 6, 7, 9, 10, 12)
; ld_en2 = 5 (5, 8, 11, 14, 15)
; ld_en3 = 4 (33, 36, 34, 37 )
; ld_en4 =  7 (13, 16, 17, 26, 27, 28, 35)
ld_en1		equ	ld_e2_tbl1 - ld_e2_tbl
ld_en2		equ	ld_e2_tbl2 - ld_e2_tbl1
ld_en3		equ	ld_e2_tbl3 - ld_e2_tbl2
ld_en4		equ	ld_e2_tble - ld_e2_tbl3

ld_e2_tbl:	db	1
in_selt:	db	4, 6, 7, 9, 10, 12
ld_e2_tbl1:	db	5, 8, 11, 14, 15
ld_e2_tbl2:	db	33, 36, 34, 37
ld_e2_tbl3:	db	13
		db	16
		db	17
		db	26
		db	27
		db	28
		db	35
ld_e2_tble:

rhlnnxy		equ	r_hl_nne - r_hl_nn
ld_rrn		equ	ld_rr_ -  r_hl_nn
ld_rxyn_sp	equ	lda_spe - ld_rr_

m_num		equ	r_hl_nne - ld_rr_	; except No.0 - 4  (IX+,IY+,IX-,IY-,nn)

r_hl_nn:	db	1, 4, 6, 7, 9, 10, 12, 28
ld_rr_:		db	25
ex_sr_:		db	33, 36, 34, 37
r_hl_nne:
lda_sp:		db	26 ; LD A (BC)
		db	27 ; LD A (DE)
		db	13 ; LD A I
		db	17 ; LD A R
		db	35 ; LD A (1234H)
lda_spe:

ld1_base:	db	68h	; L
		db	60h	; H
		db	58h	; E
		db	50h	; D
		db	48h	; C
		db	40h	; B
		db	78h	; A

ld_r_jt:	dw	ld_r35 ; LD A (1234H)
		dw	ld_r17 ; LD A R
		dw	ld_r13 ; LD A I
		dw	ld_r27 ; LD A (DE)
		dw	ld_r26 ; LD A (BC)
		dw	ld_r37
		dw	ld_r34
		dw	ld_r36
		dw	ld_r33
		dw	ld_r25

; LD XXXX, XXXX
el3_1:
	ld	a, (opr1_cd)
	ld	hl, ld_e2_tbl
	ld	bc, ld_en1
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, el3_11
	
	ld	hl, ld1_base
	add	hl, bc
	ld	d, (hl)		; met MC base code

	ld	a, (opr2_cd)
	ld	hl, r_hl_nn
	ld	bc, ld_rrn
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, el3_125	;jr	nz, el3_125
	
	ld	a, c
	cp	7		; A param?
	jp	z, el3_12	;jr	z, el3_12
	ld	a, 6
	sub	c		; make B, C, D, E, H, L, (HL) param
el3_12:
	or	d		; make MC
	jp	st_mc11	


el3_125:
	ld	hl, ld_rr_
	ld	bc, ld_rxyn_sp
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, asm_err

	ld	a, c
	cp	5		; check LD A, special
	jp	nc, el3_126	;jr	nc, el3_126

	ld	a, d
	cp	78h		; LD A ?
	jp	nz, asm_err	; err, if B, C, D, E, H, L, (HL)

el3_126:
	ld	hl, ld_r_jt
	jp	jp_each

ld_r35: ; LD A,(1234H)
	ld	a, 3ah		; 1st MC

	ld	hl, (tasm_adr)
	ld	(hl), a
	inc	hl
	ex	de,hl			;Z80 code ld	de, (opr_num35)####
	ld	hl,(opr_num35)		;##################################
	ex	de,hl			;##################################
	ld	(hl), e
	inc	hl
	ld	(hl), d
	jp	mc_end3

ld_r17: ; LD A,R
	ld	a, 5fh
ld_r171:
	jp	el2_5411
	
ld_r13: ; LD A,I
	ld	a, 57h
	jp	ld_r171		;jr	ld_r171

ld_r27: ; LD A,(DE)
	ld	a, 1ah
	jp	st_mc11
	
ld_r26: ; LD A,(BC)
	ld	a, 0ah
	jp	st_mc11

ld_r37:	; LD r,(IY-nn)
	ld	a, (opr_num37)
ld_r371:
	ld	c, a
	ld	e, 0FDH
ld_r372:
	ld	a, 6
	or	d		; make 2nd MC
	ld	d, a		; adjust I/F
	ld	a, e		; adjust I/F
	jp	cp_xy1

ld_r34: ; LD r,(IY+nn)
	ld	a, (opr_num34)
	jp	ld_r371		;jr	ld_r371

ld_r36: ; LD r,(IX-nn)
	ld	a, (opr_num36)
ld_r361:
	ld	c, a
	ld	e, 0DDH
	jp	ld_r372		;jr	ld_r372

ld_r33: ; LD r,(IX+nn)
	ld	a, (opr_num33)
	jp	ld_r361		;jr	ld_r361

ld_r25: ; LD r, nn
	ld	a, d		; get 1st MC base
	sub	3ah		; get 1st MC
	ld	d, a

ld_r251:
	ld	hl, (opr_num25)
	ld	a, h
	or	a
	jp	nz, asm_err
	ld	a, l
	push	af		; adjust I/F
	ld	a, d		; adjust I/F
	jp	el2_441

;
; LD rp16, nnnn
; LD rp16, (nnnn)
; rp16 : BC, DE, HL, IX, IY
; ld_e2_tbl1:	db	5, 8, 11, 14, 15

el3_11:
;	hl = ld_e2_tbl1
	ld	bc, ld_en2
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, el3_1_2	;jr	nz, el3_1_2
	ld	a, c
	or	a
	jp	z, ld_iy_n4	;jr	z, ld_iy_n4
	cp	1
	jp	z, ld_ix_n4	;jr	z, ld_ix_n4
	cp	2
	jp	z, ld_hl_n4	;jr	z, ld_hl_n4
	cp	3
	jp	z, ld_de_n4	;jr	z, ld_de_n4


; LD BC, nnnn; LD BC, (nnnn)
ld_bc_n4:
	ld	d, 0EDH		; 1st MC
	ld	e, 4bh		; 2nd MC
	ld	c, 1		; 1st MC

ld_bc_n40:
	ld	a, (opr2_cd)
	cp	25
	jp	z, ld_bc_n41	;jr	z, ld_bc_n41

	ld	a, e		; 2nd MC
	ld	hl, opr_num35
	ld	c, (hl)		; 3rd MC
	inc	hl
	ld	e, (hl)		; 4th MC
	jp	mc_end41

ld_bc_n41:
	ld	hl, opr_num25
	ld	e, (hl)		; 2rd MC
	inc	hl
	ld	d, (hl)		; 3rd MC
	ld	a, c		; 1st MC

ld_bc_n42:
	ld	hl, (tasm_adr)
	ld	(hl), c		; save 1st MC
	inc	hl
	ld	(hl), e
	inc	hl
	ld	(hl), d
	jp	mc_end3
	
; LD DE, nnnn; LD DE, (nnnn)
ld_de_n4:
	ld	d, 0EDH		; 1st MC
	ld	e, 5bh		; 2nd MC
	ld	c, 11h		; 1st MC
	jp	ld_bc_n40	;jr	ld_bc_n40

; LD HL, nnnn; LD HL, (nnnn)
ld_hl_n4:
	ld	c, 21h
	ex	de,hl			;Z80 code ld	de, (opr_num25)####
	ld	hl,(opr_num25)		;##################################
	ex	de,hl			;##################################
	ld	a, (opr2_cd)
	cp	25
	jp	z, ld_bc_n42	;jr	z, ld_bc_n42
	ld	c, 2Ah
	ex	de,hl			;Z80 code ld	de, (opr_num35)####
	ld	hl,(opr_num35)		;##################################
	ex	de,hl			;##################################
	jp	ld_bc_n42	;jr	ld_bc_n42

; LD IX, nnnn; LD IX, (nnnn)
ld_ix_n4:
	ld	d, 0DDh		; 1st MC

ld_ix_n40:
	ld	c, 21h
	ld	hl, opr_num25

	ld	a, (opr2_cd)
	cp	25
	jp	z, ld_ix_n42	;jr	z, ld_ix_n42

	ld	c, 2Ah
	ld	hl, opr_num35

ld_ix_n42:
	ld	a, c		; 2nd MC
	ld	c, (hl)		; 3rd MC
	inc	hl
	ld	e, (hl)		; 4th MC
	jp	mc_end41

; LD IY, nnnn; LD IY, (nnnn)
ld_iy_n4:
	ld	d, 0FDh		; 1st MC
	jp	ld_ix_n40	;jr	ld_ix_n40

;
; LD ([IX|IY][+|-]nn), [r|nn]
;
;ld_e2_tbl2:	db	33, 36, 34, 37
; LD (IX | IY +|- nn), r | nn
el3_1_2:
;	hl = ld_e2_tbl2
	ld	bc, ld_en3
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, el3_13	;jr	nz, el3_13
	
	ld	hl, opr_num37
	add	hl, bc
	ld	d, (hl)		; get 3rd MC

	ld	a, c
	ld	e, 0FDh		; 1st MC
	cp	2
	jp	c, el3_121	;jr	c, el3_121
	ld	e, 0DDh		; 1st MC

; reg_A = (opr2_cd)
; if (reg_A = 25) 2nd_MC = 36h
; else {
;     if (Reg_C = 6) 2nd_MC =77h
;     else 2nd_MC = 75h - reg_C
; }
el3_121: ; check element No.3
	ld	a, (opr2_cd)
	cp	25
	jp	z, ld_xynln	;jr	z, ld_xynln

	ld	hl, ld_e2_tbl
	ld	bc, ld_en1
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, asm_err

	ld	a, c
	cp	6
	ld	a, 077h		; a : 2nd MC
	jp	z, mc2_77	;jr	z, mc2_77

	ld	a, 75h
	sub	c	; a : 2nd MC

 ; adjust 1: c, 2: e, 3: d
mc2_77:
	ld	c, e		; adjust I/F
	ld	e, a		; adjust I/F
	jp	ld_bc_n42

ld_xynln: ; 4byte MC 
	  ; LD ([IX|IY] [+|-]), nn
	  ; 2nd MC = 36h

	ld	hl, (opr_num25)
	ld	a, h
	or	a
	jp	nz, asm_err

; adjust I/F
; d: 1st MC, a: 2nd MC, c: 3rd MC, e: 4th MC

	ld	a, 36h		; 2nd MC
	ld	c, d		; 3rd MC
	ld	d, e		; 1st MC
	ld	e, l		; 4th MC
	jp	mc_end41

;
; 35 : ld (nnnn), reg
; 28 : ld (hl), r | nn
; 16 : ld SP, hl | ix | iy | nnnn | (nnnn)
; 27 : LD (DE), A
; 26 : LD (BC), A
; 17 : LD R, A
; 13 : LD I, A
;
el3_13:
;	hl = ld_e2_tbl3
	ld	bc, ld_en4
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, asm_err
	ld	hl, el3_14
	jp	jp_each

; jump table
el3_14:		dw	el3_s35
		dw	el3_s28
		dw	el3_s27
		dw	el3_s26
		dw	el3_s17
		dw	el3_s16
		dw	el3_s13

ldnr_n	equ	ldnr_c - ldnr_t

ldnr_t:		db	1, 11, 5, 8, 16, 14, 15
ldnr_c:		db	22h, 22h, 73h, 53h, 43h, 22h, 32h

el3_s35: ; ld (nnnn), reg

	ld	a, (opr2_cd)

	ld	hl, ldnr_t
	ld	bc, ldnr_n
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, asm_err

	ld	hl, ldnr_c
	add	hl, bc
	ld	a, c
	ld	c, (hl)		; 1st MC

	ld	hl, (opr_num35)
	cp	5
	jp	c, el3_s351	;jr	c, el3_s351

 ; adjust 1: c, 2: e, 3: d

	ld	e, l			; 2nd MC
	ld	d, h			; 3rd MC
	jp	ld_bc_n42

el3_s351:
	cp	2
	jp	c, el3_s352	;jr	c, el3_s352
 ; ED
; d: 1st MC, a: 2nd MC, c: 3rd MC, e: 4th MC

	ld	d, 0EDH			; 1st MC
el3_s353:
	ld	a, c			; 2nd MC
	ld	c, l			; 3rd MC
	ld	e, h			; 4th mc
	jp	mc_end41

el3_s352: ; DD, FD
	ld	d, 0FDh			; 1st MC
	or	a
	jp	z, el3_s353	;jr	z, el3_s353
	ld	d, 0DDh			; 1st MC
	jp	el3_s353	;jr	el3_s353

; ld (hl), r | nn
el3_s28:
	ld	a, (opr2_cd)
	cp	25
	jp	z, el3_s281	;jr	z, el3_s281
	cp	1
	jp	z, el3_s282	;jr	z, el3_s282
	ld	hl, ld_e2_tbl + 1
	ld	bc, ld_en1 - 1
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, asm_err
	ld	a, 75h
	sub	c
el3_s283:
	jp	st_mc11

el3_s282: ; LD (HL), A
	ld	a, 77h
	jp	el3_s283	;jr	el3_s283

el3_s281: ; LD (HL), nn
	ld	hl, (opr_num25)
	ld	a, h
	or	a
	jp	nz, asm_err

	ld	a, l
	push	af		; adjust I/F
	ld	a, 36h		; 1st MC
	jp	el2_441

el3_s16: ; ld SP, hl | ix | iy | nnnn | (nnnn)

	ld	a, (opr2_cd)
	cp	11
	jp	z, el3_s161	;jr	z, el3_s161
	cp	14
	jp	z, el3_s162	;jr	z, el3_s162
	cp	15
	jp	z, el3_s163	;jr	z, el3_s163
	cp	25
	jp	z, el3_s164	;jr	z, el3_s164
	cp	35
	jp	nz, asm_err

; LD SP, (nnnn)
; d: 1st MC, a: 2nd MC, c: 3rd MC, e: 4th MC

	ld	d, 0edh		; 1st MC
	ld	a, 7bh		; 2nd MC
	ld	hl, (opr_num35)
	ld	c, l		; 3rd MC
	ld	e, h		; 4th MC
	jp	mc_end41


el3_s161: ; LD SP, HL
	ld	a, 0f9h
	jp	el3_s261	;jr	el3_s261

el3_s162: ; LD SP, IX
	ld	d, 0DDh
	jp	el3_s1631	;jr	el3_s1631

el3_s163: ; LD SP, IY
	ld	d, 0FDh
el3_s1631:
	ld	a, 0f9h
	push	af		; adjust I/F
	ld	a, d
	jp	el2_441

el3_s164: ; LD SP, nnnn
; adjust 1: c, 2: e, 3: d

	ld	c, 31h		; 1st MC
	ex	de,hl			;z80 code ld	de, (opr_num25)	; e: 2nd MC, d: 3rd MC
	ld	hl,(opr_num25)		;############################
	ex	de,hl			;############################
	jp	ld_bc_n42

el3_s27: ; LD (DE), A
	ld	a, 12h		; set MC
	jp	el3_s261	;jr	el3_s261
	
el3_s26: ; LD (BC), A
	ld	a, 2h		; set MC
el3_s261:
	jp	st_mc11
	
el3_s17: ; LD R, A
	ld	a, 4fh		; set 2nd MC
	jp	el2_5411

el3_s13: ; LD I, A
	ld	a, 47h		; set 2nd MC
	jp	el2_5411

;1111111111111111111111111111111111
;
; element count = 1 
;
;1111111111111111111111111111111111

e1s	equ	e1_e - e1_s
e1s1	equ	e1_e1 - e1_s1

e1_s: ;------------------------------------------------
elem1_cd:	db	7, 10, 11, 14, 15, 30, 32
		db	33, 46, 50, 51, 52, 53, 65
e1_e: ;------------------------------------------------

elem1_opcd:	db	27H, 0FBH, 0F3H, 76H, 00H, 0C9H, 37H
		db	3FH, 2FH, 1FH, 0FH, 17H, 07H, 0D9H

e1_s1: ; ----------------------------------------------
elem1_cd1:	db	39, 40, 37, 38, 58, 59, 56
		db	57, 4,  5,  2,  3,  31, 64
		db	62, 63, 61, 47, 48, 66, 67
e1_e1: ; ----------------------------------------------

elem1_opcd1:	db	 67h,  6Fh,  45h,  4Dh, 0A3h, 0ABh, 0B3h
		db	0BBh,  44h, 0B0h, 0A0h, 0B8h, 0A8h, 0B2h
		db	0A2h, 0BAh, 0AAh, 0B1h, 0A1h, 0B9h, 0A9h

;
; make machine code,
; element_cnt = 1 (ex. NOP)
; output : 1 byte Machine code
;
mk_e1:	; 1byte MC
	ld	a, (opc_cd)
	ld	bc, e1s
	ld	hl, elem1_cd
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, mk_e11
	
	ld	hl, elem1_opcd

st_mc1:
	add	hl, bc
	; get MC
	ld	a, (hl)
st_mc11:
	ld	hl, (tasm_adr)
	ld	(hl), a
	ld	a, 1

mc_end:
	ld	(byte_count), a	; set MC bytes
	jp	cout_sp

mk_e11: ; 2byte MC (0EDh, XX)
	ld	bc, e1s1
	ld	hl, elem1_cd1
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, asm_err
	
	ld	hl, (tasm_adr)
	ld	a, 0EDH
	ld	(hl), a		; set MC No.1
	inc	hl
	ex	de, hl

	ld	hl, elem1_opcd1
	add	hl, bc
	; get MC
	ld	a, (hl)
	ld	(de), a		; set MC No.2

mc_end2:
	ld	a, 2
	jp	mc_end		;jr	mc_end

;2222222222222222222222222222222222222222222222
;
; element count = 2 
;
;2222222222222222222222222222222222222222222222

e2s	equ	e2_e - e2_s

e2_s:
elem2_cd:
	db	8, 9			; DD, FD
	db	23, 26			; DD, FD
	db	24, 27, 28 ,29, 41	; DD, FD
	db	42			; DD, FD
	db	46
	db	49
	db	43
	db	44
	db	45
; CB
	db	12			; DD, FD
	db	16			; DD, FD
	db	13			; DD, FD
	db	17			; DD, FD
	db	18			; DD, FD
	db	19			; DD, FD
	db	20			; DD, FD
; ED
	db	54			; ED
	db	68			; ORG
	db	69			; DB
	db	70			; DW
e2_e:

ent_el2:
	dw	el2_70		; DW
	dw	el2_69		; DB
	dw	el2_68		; ORG
	dw	el2_54		; IM
	dw	el2_20		; SRL
	dw	el2_19		; SRA
	dw	el2_18		; SLA
	dw	el2_17		; RR
	dw	el2_13		; RL
	dw	el2_16		; RRC
	dw	el2_12		; RLC

	dw	el2_45		; CALL nnnn
	dw	el2_44		; DJNZ e
	dw	el2_43		; jR e
	dw	el2_49		; RST nn
	dw	el2_46		; RET CC
	dw	el2_42		; JP (HL), JP nnnn

; code base 0a0h, or 00h, 08h, 10h, 18h
	dw	el2_41		; CP
	dw	el2_29		; XOR
	dw	el2_28		; OR
	dw	el2_27		; AND

	dw	el2_24		; SUB

	dw	el2_26		; DEC
	dw	el2_23		; INC

	dw	el2_9		; POP
	dw	el2_8		; PUSH

;
; make machine code,
; element_cnt = 2 (ex. PUSH AF)
; output : 1 to 3 bytes Machine code
;

mk_e2:
	ld	a, (opc_cd)
	ld	bc, e2s
	ld	hl, elem2_cd
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, asm_err
	
	ld	hl, ent_el2
	jp	jp_each

el2_70: ; DW
	ld	a, (opr1_cd)
	cp	25
	jp	nz, asm_err

	ex	de,hl			;z80 code ld	bc, (opr_num25)
	ld	hl,(opr_num25)		;#############################
	ex	de,hl			;#############################
	ld	b,d			;#############################
	ld	c,e			;#############################
	ld	hl, (tasm_adr)
	ld	(hl), c
	inc	hl
	ld	(hl), b
	jp	mc_end2

el2_69: ; DB
	ld	a, (opr1_cd)
	cp	25
	jp	z, el2_691	;jr	z, el2_691
	cp	38
	jp	nz, asm_err
	
	ld	hl, (opr_num25)

	xor	a
	ld	c, a
	ex	de,hl			;z80 code ld	de, (tasm_adr)
	ld	hl,(tasm_adr)		;#############################
	ex	de,hl			;#############################
el2_692:
	ld	a, (hl)
	cp	'"'
	jp	z, cpstrend	;jr	z, cpstrend
	or	a
	jp	z, cpstrend	;jr	z, cpstrend

	ld	(de), a
	inc	hl
	inc	de
	inc	c
	jp	el2_692		;jr	el2_692

cpstrend:
	ld	a, c
	jp	mc_end

el2_691:
	ex	de,hl			;z80 code ld	bc, (opr_num25)
	ld	hl,(opr_num25)		;#############################
	ex	de,hl			;#############################
	ld	b,d			;#############################
	ld	c,e			;#############################
	ld	a, b
	or	a
	jp	nz, asm_err

	ld	hl, (tasm_adr)
	ld	(hl), c
	ld	a, 1
	jp	mc_end

el2_68: ; ORG
	ld	a, (opr1_cd)
	cp	25
	jp	nz, asm_err

	xor	a
	ld	hl, (opr_num25)
	ld	bc, RAM_B
	call	sbc_hlbc		;z80 code sbc	hl, bc #####
	jp	c, asm_err

	ld	bc, RAM_B + RAM_SIZ
	ld	hl, (opr_num25)
	call	sbc_hlbc		;z80 code sbc	hl, bc #####
	jp	c, okram	;jr	c, okram

okram:
	ld	hl, (opr_num25)
	ld	(tasm_adr), hl
	xor	a
	jp	mc_end

; CALL nnnn
el2_45:
	ld	a, (opr1_cd)
	cp	25
	jp	nz, asm_err

	ld	a, 0CDH

el2_451:
	ld	hl, (tasm_adr)
	ld	(hl), a		; save op_code
	inc	hl
	ex	de,hl			;z80 code ld	de, (opr_num25)
	ld	hl,(opr_num25)		;#############################
	ex	de,hl			;#############################
	ld	(hl), e
	inc	hl
	ld	(hl), d

mc_end3:
	ld	a, 3
	jp	mc_end

; DJNZ relative number
el2_44:
	ld	a, (opr1_cd)
	cp	25
	jp	nz, asm_err

	call	calc_reladr
	jp	c, asm_err
	push	af
	ld	a, 10H
el2_441:
	ld	hl, (tasm_adr)
	ld	(hl), a		; save op_code
	inc	hl
	pop	af
	ld	(hl), a
	jp	mc_end2

;ovr_msg:
;	db	"Over. ", 0

; JR e
el2_43:
	ld	a, (opr1_cd)
	cp	25
	jp	nz, asm_err
	call	calc_reladr
	jp	c, asm_err
	push	af
	ld	a, 18H
	jp	el2_441		;jr	el2_441

; RST
el2_49:
	ld	hl, (opr_num25)
	ld	a, h
	or	a
	jp	nz, asm_err
	ld	a, l
	ld	hl, rst_no
	ld	bc, 8
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, asm_err

	ld	hl, rst_cd
	jp	st_mc1		; get and store MC code

rst_no:	db	0, 8, 10H, 18H, 20H, 28H, 30H, 38H
rst_cd:	db	0FFH, 0F7H, 0EFH, 0E7H, 0DFH, 0D7H, 0CFH, 0C7H

;ill_num:	db	"Ill-No. ", 0

; RET CC
el2_46:
	ld	a, (opr1_cd)	; get code number of operand No.1
	ld	hl, ret_no
	ld	bc, 8
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, asm_err

	ld	hl, ret_cd
	jp	st_mc1		; get and store MC code

ret_no:	db	6, 18, 19, 20, 21, 22, 23, 24
ret_cd:	db	0E8H, 0E0H, 0F0H, 0D0H, 0C0H, 0F8H, 0C8H, 0D8H

; JP (HL), JP nnnn, JP (IX), JP (IY)
el2_42:
	ld	a, (opr1_cd)	; get code number of operand No.1
	cp	25
	jp	z, jpnnnn	;jr z, jpnnnn	; JP nnnn
	ld	l, 0E9H
	cp	28		; (HL)?
	jp	z, jphl		;jr	z, jphl
	cp	29		; (IX)?
	jp	z, jpix		;jr	z, jpix
	cp	30		; (IY)?
	jp	nz, asm_err

; JP (IY)
	ld	a, l
	push	af
	ld	a, 0FDH
	jp	el2_441

; JP (IX)
jpix:
	ld	a, l
	push	af
	ld	a, 0DDH
	jp	el2_441

; JP (HL)
jphl:
	ld	a, l
	jp	st_mc11		; set JP (HL) MC code

; JP nnnn
jpnnnn:
	ld	a, 0c3h
	jp	el2_451		; save OP and jmp address

;
; CP, XOR, OR, AND, SUB section
;
; (defined already)
;rhlnnxy	equ	r_hl_nne - r_hl_nn
;m_num		equ	5	; except No.0 - 4  (IX+,IY+,IX-,IY-,nnnn)
;r_hl_nn:	db	1, 4, 6, 7, 9, 10, 12, 28
;		db	25
;ex_sr_:	db	33, 36, 34, 37
;r_hl_nne:
;
base1:		db	06H, 06H, 06H, 06H, 46h
		db	06H, 05H, 04H, 03H, 02H, 01H, 00H, 07H

el2_41: ; CP
	ld	d, 0b8h
	jp	and_cp		;jr	and_cp

el2_29: ; XOR
	ld	d, 0a8h
	jp	and_cp		;jr	and_cp

el2_28: ; OR
	ld	d, 0b0h
	jp	and_cp		;jr	and_cp

el2_27: ; AND
	ld	d, 0a0h
	jp	and_cp		;jr	and_cp

el2_24: ; SUB
	ld	d, 090h

and_cp:
	ld	a, (opr1_cd)	; get code number of operand No.1
	ld	hl, r_hl_nn
	ld	bc, rhlnnxy
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, asm_err

	ld	hl, base1
	add	hl, bc
	ld	a, (hl)
	or	a, d		; make code operand1
	ld	d, a		; save operand1
	ld	a, c
	cp	m_num		; check IX+,IY+,IX-,IY-,nnnn
	jp	c, el2_410
	ld	a, d		; restore
	jp	st_mc11
	
; IX+,IY+,IX-,IY-,nnnn
el2_410:
	cp	4 		; cp nn ?
	jp	c, cp_ixiy	;jr	c, cp_ixiy	; no, IX+,IY+,IX-,IY-

; CP nn
	ld	hl, (opr_num25)
	ld	a, h
	or	a
	jp	nz, asm_err

	ld	a, l
el2_411:
	push	af		; adjust save I/F
	ld	a, d		; restore cp opecode
	jp	el2_441
	
; CP (IX+nn), CP (IY+nn), CP (IX-nn), CP (IY-nn)

cp_ixiy:
	cp	2
	ld	a, 0FDH
	jp	c, cp_iy	;jr	c, cp_iy
	ld	a, 0DDH
cp_iy:
	ld	hl, opr_num37
	add	hl, bc
	ld	c, (hl)
cp_xy1:	
	ld	hl, (tasm_adr)
	ld	(hl), a		; save op_code
	ld	a, d		; restore 2nd operand
	inc	hl
	ld	(hl), a
	inc	hl
	ld	(hl), c
	jp	mc_end3

;
; DEC, INC section
;

di_nbs		equ	d_i_tbe - d_i_tb
di_ixynum	equ	d_i_tbe - dix_tbl
di_ixnnum	equ	d_i_tbe - dixn_tbl
di_iynnum	equ	d_i_tbe - diyn_tbl

d_i_tb:		db	1, 4, 5, 6, 7, 8
		db	9, 10, 11, 12, 16, 28
dix_tbl:	db	14, 15		; IX, IY
dixn_tbl:	db	33, 36		; IX+nn, IX-nn
diyn_tbl:	db	34, 37		; IY+nn, IY-nn
d_i_tbe:

DEC_opr:	db	35h, 35h	; IY-, IY+
		db	35h, 35h	; IX-, IX+
		db	2bh, 2bh 	; IY, IX
		db	35h, 3Bh, 2Dh, 2Bh, 25h, 1Dh
		db	1Bh, 15h, 0Dh, 0Bh, 05h, 3Dh

INC_opr:	db	34h, 34h	; IY-, IY+
		db	34h, 34h	; IX-, IX+
		db	23h, 23h	; IY,  IX
		db	34h, 33h, 2Ch, 23h, 24h, 1Ch
		db	13h, 14h, 0Ch, 03h, 04h, 3Ch

el2_26: ; DEC
	ld	de, DEC_opr
	jp	el2_di		;jr	el2_di

el2_23: ; INC
	ld	de, INC_opr

el2_di:
	ld	a, (opr1_cd)	; get code number of operand No.1
	ld	hl, d_i_tb
	ld	bc, di_nbs
	CALL	CPIR80			;Z80 CODE cpir #################
	ld	h, d
	ld	l, e
	jp	nz, asm_err

	ld	a, c
	ld	d, 0FDH		; set IY extended OP
	cp	di_ixynum
	jp	nc, st_mc1	; 1 MC code

	add	hl, bc
	ld	a, (hl)		; get 2nd operand
	ld	e, a		; save
	ld	a, c
	cp	di_ixnnum
	jp	c, di_3mc	;jr	c, di_3mc
	
	; 2 MC. : INC IX, IY, DEC IX, IY
	
	jp	z, di_iy	;jr	z, di_iy
	ld	d, 0DDH		; set IX extended OP
di_iy:
	ld	a, e		; restoe 2nd operand
	jp	el2_411		; 2 MC (IX, IY)

	; 3 MC. : INC (IX+nn) etc,.
di_3mc:
	cp	di_iynnum	; IY+-nn?
	jp	c, di_3mc1	;jr	c, di_3mc1
	ld	d, 0DDH		; set IX extended OP
di_3mc1:
	ld	a, d		; adjust I/F
	ld	d, e		; adjust I/F
	jp	cp_iy		; make 3 MC
	
;
; PUSH, POP section
;

pp_no		equ	pp_tble - pp_tbl
pp_ixiy		equ	pp_tble - pp_ixy

pp_tbl:		db	2, 5, 8, 11
pp_ixy:		db	14, 15
pp_tble:	

pp_base:	db	0E0h, 0E0h ; IY, IX
		db	0E0h, 0D0h, 0C0h, 0f0h

el2_9:  ; POP
	ld	d, 01h		; base code for pop operand
	jp	el2_81		;jr	el2_81

el2_8:  ; PUSH
	ld	d, 05h

el2_81:
	ld	hl, pp_tbl
	ld	bc, pp_no
	ld	a, (opr1_cd)	; get code number of operand No.1
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, asm_err

	ld	hl, pp_base
	add	hl, bc
	ld	a, (hl)
	or	d		; make opecode
	ld	d, a		; save

	ld	a, c
	cp	pp_ixiy
	ld	a, d		; restore
	jp	nc, st_mc11	; 1 MC code

	ld	e, 0FDH		; set IY extended OP
	ld	a, c
	or	a		; IY
	jp	z, el2_82	;jr	z, el2_82
	ld	e, 0DDH		; set IX code
el2_82:
	ld	a, d
	push	af		; adjust I/F
	ld	a, e		; adjust I/F
	jp	el2_441		; make 2 MC

;
; SRL, SRA, SLA, RR, RL, RRC, RLC section
;


; already defined
; r_hl_nn:	db	1, 4, 6, 7, 9, 10, 12, 28
;		db	25
; ex_sr_:		db	33, 36, 34, 37

sr_num		equ	8
ex_sr_num	equ	ld_en3

;SRL_cd:	db	3Eh, 3Dh, 3Ch, 3Bh, 3Ah, 39h, 38h, 3Fh
;SRA_cd:	db	2Eh, 2Dh, 2Ch, 2Bh, 2Ah, 29h, 28h, 2Fh
;RR_cd:		db	1Eh, 1Dh, 1Ch, 1Bh, 1Ah, 19h, 18h, 1Fh
;RRC_cd:	db	0Eh, 0Dh, 0Ch, 0Bh, 0Ah, 09h, 08h, 0Fh
;SLA_cd:	db	26h, 25h, 24h, 23h, 22h, 21h, 20h, 27h
;RL_cd:		db	16h, 15h, 14h, 13h, 12h, 11h, 10h, 17h
;RLC_cd:	db	06h, 05h, 04h, 03h, 02h, 01h, 00h, 07h
;RLC_cd:	db	06h, 05h, 04h, 03h, 02h, 01h, 00h, 07h

RLC_or	equ	0
RL_or	equ	10h
SLA_or	equ	20h
RRC_or	equ	08h
RR_or	equ	18h
SRA_or	equ	28h
SRL_or	equ	38h

SR_base:	db	06h, 05h, 04h, 03h, 02h, 01h, 00h, 07h



el2_20:	; SRL
	ld	de, (SRL_or << 8) | 3Eh
	jp	sr_lookfor	;jr	sr_lookfor
el2_19:	; SRA
	ld	de, (SRA_or << 8) | 2Eh
	jp	sr_lookfor	;jr	sr_lookfor
el2_18:	; SLA
	ld	de, (SLA_or << 8) | 26h
	jp	sr_lookfor	;jr	sr_lookfor
el2_17:	; RR
	ld	de, (RR_or << 8) | 1Eh
	jp	sr_lookfor	;jr	sr_lookfor
el2_13:	; RL
	ld	de, (RL_or << 8) | 16h
	jp	sr_lookfor	;jr	sr_lookfor
el2_16:	; RRC
	ld	de, (RRC_or << 8) | 0Eh
	jp	sr_lookfor	;jr	sr_lookfor
el2_12:	; RLC
	ld	de, (RLC_or << 8) | 06h

sr_lookfor:
	ld	a, (opr1_cd)	; get code number of operand No.1
	ld	hl, r_hl_nn
	ld	bc, sr_num
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, nxt_sr	;jr	nz, nxt_sr

	ld	hl, SR_base
	add	hl, bc
	ld	a, (hl)
	or	d		; make 2nd opcode

el2_130:
	push	af		; adjust I/F
	ld	a, 0cbh		; set OP code
	jp	el2_441

nxt_sr:
	ld	hl, ex_sr_
	ld	bc, ex_sr_num
	CALL	CPIR80			;Z80 CODE cpir #################
	jp	nz, asm_err

	ld	d, 0FDh		; for IY
	ld	a, c
	cp	2
	jp	c, sr_iy1	;jr	c, sr_iy1
	ld	d, 0DDh		; for IX

sr_iy1:	; D: opecode, E : code operand4

	ld	hl, opr_num37
	add	hl, bc
	ld	c, (hl)
	inc	hl
	ld	b, (hl)
	ld	a, 0CBH

mc_end41:
	ld	hl, (tasm_adr)
	ld	(hl), d		; save 1st MC
	inc	hl
	ld	(hl), a		; save 2nd MC 
	inc	hl
	ld	(hl), c		; save 3rd MC (8bit litelal)
	inc	hl
	ld	(hl), e		; save 4th MC
mc_end4:
	ld	a, 4
	jp	mc_end

;
; IM section
;
el2_54:	; IM
	ld	a, (opr_num25+1)	; get high byte
	or	a
	jp	nz, asm_err
	ld	a, (opr_num25)		; get low byte
	cp	3
	jp	nc, asm_err

	ld	c, 046h		; IM 0
	or	a
	jp	z, el2_541	;jr	z, el2_541

	ld	c, 056h		; IM 1
	dec	a
	jp	z, el2_541	;jr	z, el2_541
	ld	c, 05Eh		; IM 2

el2_541:
	ld	a, c

el2_5411:
	push	af		; adjust I/F
	ld	a, 0EDh		; set MC 1
	jp	el2_441

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; get opecode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
analize_input:
	call	sch_opecode
	ret	c		; error return
	ld	(opc_cd), a	; save code number of opecode
	call	inc_element

	ld	a, (hl)
	or	a
	ret	z		; no operand
	cp	a, ' '		; check opecode delimiter
	jp	nz, sx_err	; not space then syntax error

	; get code of operand 1

	call	analize_opr
	ret	c		; error
	ld	(opr1_cd), a	; save operand code to opr1
	PUSH	HL			;#### ADDED ####################
	PUSH	AF			;Z80 CODE EX	AF,AF' #########
	LD	HL,(alt_AF)		;###############################
	PUSH	HL			;###############################
	POP	AF			;###############################
	POP	HL			;###############################
	LD	(alt_AF),HL		;###############################
	POP	HL			;###############################
	call	inc_element
	PUSH	HL			;#### ADDED ####################
	PUSH	AF			;Z80 CODE EX	AF,AF' #########
	LD	HL,(alt_AF)		;###############################
	PUSH	HL			;###############################
	POP	AF			;###############################
	POP	HL			;###############################
	LD	(alt_AF),HL		;###############################
	POP	HL			;###############################
	cp	38		; check operand code 38:='"'
	ret	z
	ld	a, (hl)
	or	a
	ret	z		; no operand2
	cp	a, ','		; check opecode delimiter
	jp	nz, sx_err	;jr	nz, sx_err

	; get code of operand 2

	call	analize_opr
	ret	c		; error
	ld	(opr2_cd), a	; save operand code to opr1
	call	inc_element

	ld	a, (hl)
	or	a
	jp	nz, sx_err	;jr	nz, sx_err	; error end
	ret

an_err:
	pop	af
sx_err:
	scf
	ret

inc_element:
	ld	a, (element_cnt)
	inc	a
	ld	(element_cnt), a
	ret

;------------------------
; HL : input buffer
;------------------------
analize_opr:
	inc	hl		; HL : top of operand strings point
	call	sch_operand1
	ret	nc		; match, then retrun
	
	; search (IX+, (IY+, (IX-, (IY-

	call	sch_operand1_1
	jp	c, nxt_a1	;jr	c, nxt_a1	; jump, in not match

	; analize "nn)"

	push	af		; save operand code
	call	get_number	; DE : binary
	jp	c, an_err
	
	ld	a, d
	or	a
	jp	nz, an_err	;jr	nz, an_err	; over 255
	
	ld	a, (hl)
	cp	')'
	jp	nz, an_err	;jr	nz, an_err

	inc	hl
	pop	af		; restore operand code

	push	af
	cp 	33		; IX+?
	jp	nz, nxt_a2	;jr	nz, nxt_a2
	ld	bc, opr_num33
	jp	nxt_a21		;jr	nxt_a21

nxt_a2:
	cp 	34		; IY+?
	jp	nz, nxt_a3	;jr	nz, nxt_a3
	ld	bc, opr_num34

nxt_a21:
	ld	a, e
	cp	80H
	jp	nc, an_err	;jr	nc, an_err

nxt_a22:
	ld	(bc),a		; save binary
	pop	af
	ret
	
nxt_a3:
	cp 	36		; IX-?
	jp	nz, nxt_a4	;jr	nz, nxt_a4
	ld	bc, opr_num36

nxt_a31:
	ld	a, e
	cp	81H
	jp	nc, an_err	;jr	nc, an_err
	CPL				;Z80 CODE neg
	INC	A			;flags may be wrong, it's OK here
	jp	nxt_a22		;jr	nxt_a22
	
nxt_a4:	; IY-
	ld	bc, opr_num37
	jp	nxt_a31		;jr	nxt_a31

; check '('
nxt_a1:
	ld	a, (hl)
	cp	'('
	jp	nz, chk_strings	;jr	nz, chk_strings
	
	; get number
	
	inc	hl
	call	get_number
	ret	c		; error number

	ld	a, (hl)
	cp	')'
	jp	nz, sx_err	;jr	nz, sx_err

	inc	hl
	ex	de,hl			;z80 code ld	(opr_num35),de	; save binary
	ld	(opr_num35),hl		;#######################################
	ex	de,hl			;#######################################
	ld	a, 35		; set operand code
	ret			; normal end

chk_strings:
	cp	'"'
	jp	nz, only_num	;jr	nz, only_num
	inc	hl
	ld	(opr_num25), hl
	ld	a, 38
	ret

only_num:
	call	get_number
	ret	c		; error number
	ex	de,hl			;z80 code ld	(opr_num25),de	; save binary
	ld	(opr_num25),hl		;#######################################
	ex	de,hl			;#######################################
	ld	a, 25		; set operand code
	ret			; normal end

;------------------
; HL : input buffer
;------------------
sch_operand1:
	ld	de, operand_cds
	jp	sh_0		;jr	sh_0

;------------------
; HL : input buffer
; search (IX+, (IY+, (IX-, (IY-
;------------------
sch_operand1_1:
	ld	de, opr_cd1
	jp	sh_0		;jr	sh_0		;	sh_0

;----------------------------------------------------------
; Search code number of opecode from input strings
; 
; output:
; if mach opecode ; HL : next point of input strings
;		    A  : a code number of opecode
; not mach	  ; CF = 1
;----------------------------------------------------------
sch_opecode:
	ld	hl, line_buf
	ld	de, ope_cds

sh_0:
	push	hl
sh_1:
	ld	a, (de)
	cp	(hl)
	jp	nz, sch_next	;jr	nz, sch_next
	
	; match
	inc	hl
	inc	de
	jp	sh_1		;jr	sh_1

sch_next:
	and	dm_bit		; delimiter?
	jp	nz, ok_match	;jr	nz, ok_match
	
skip_next:
	inc	de
	ld	a, (de)
	and	dm_bit
	jp	z, skip_next	;jr	z, skip_next

	; detect delimiter string

	inc	de		; next search strings
	
	ld	a, (de)
	or	a		; tabel end?
	jp	z, n_end	;jr	z, n_end	; yes, no match return

	pop	hl
	jp	sh_0		;jr	sh_0		; search again

	; match opecode strings
	; a : opecode number

ok_match:
	ld	a, (de)
	and	a, 7Fh		; mask dm_bit, get code of opecode
	pop	de		; discard top string address
	ret
	
; no opecode strings maching
n_end:
	pop	hl
	scf
	ret

;---------------------
; HL : string buffer
;
; output : de or CF
;----------------------
get_number:

	ld	de, num_string
	ld	c, 0
	
; check first character

	call	dec_chr		; check decimal chrarcter
	ret	c		; no number inputs detect

;  detect 1st number( only 0 to 9 )

	ld	(de), a		; save to buffer
	inc	de
	inc	hl
	inc	c

; check 2nd, 3rd, 4th, 5th number string

lop_gnum:
	call	hex_chr
	jp	c, ck_endmk	;jr	c, ck_endmk	; no number inputs detect

; detect 2nd, 3rd, 4th, 5th number (include A to F)

	ld	(de), a		; save to buffer
	inc	de
	inc	hl
	inc	c
	ld	a, c
	cp	NUMLEN		; buffer check
	jp	z, no_num	;jr	z, no_num	; overfllow. error return
	jp	lop_gnum	;jr	lop_gnum

ck_endmk:
	PUSH	HL			;#### ADDED ####################
	PUSH	AF			;Z80 CODE EX	AF,AF' #########
	LD	HL,(alt_AF)		;###############################
	PUSH	HL			;###############################
	POP	AF			;###############################
	POP	HL			;###############################
	LD	(alt_AF),HL		;###############################
	POP	HL			;###############################
	xor	a
	ld	(de), a		; set delimiter
	PUSH	HL			;#### ADDED ####################
	PUSH	AF			;Z80 CODE EX	AF,AF' #########
	LD	HL,(alt_AF)		;###############################
	PUSH	HL			;###############################
	POP	AF			;###############################
	POP	HL			;###############################
	LD	(alt_AF),HL		;###############################
	POP	HL			;###############################
	cp	'H'		; check hex?
	jp	z, bi_hex	;jr	z, bi_hex

;	get binary from decimal string
;bi_dec:
	push	hl
	ld	hl, num_string
	call	GET_dNUM	; return bc
	pop	hl
	ld	d, b		; set binary to DE
	ld	e, c		; set binary to DE
	ret

;	ret	nc		; if CF =1, try hex to bin
;	dec	hl		; adjust hl pointer

	;hex to binary
bi_hex:
	inc	hl
	push	hl
	ld	hl, num_string
	call	RDHEX		; get binary to DE
	pop	hl
	or	a		; clear CF
	ret

;--------------------------
; skip sp
;--------------------------
skp_sp:
	ld	a, (hl)
	cp	' '
	ret	nz
	inc	hl
	jp	skp_sp		;jr	skp_sp

;--------------------------
; check decimal char
;--------------------------
dec_chr:
	ld	a, (hl)
	cp	':'
	jp	nc, no_num	;jr	nc, no_num
	cp	'0'
	ret	nc
	
no_num:	scf
	ret

;--------------------------
; check HEX char
;--------------------------
hex_chr:
	ld	a, (hl)
	cp	'0'
	ret	c	; error return
	cp	'9'+1
	jp	c, dec_num	;jr	c, dec_num
	cp	'A'
	ret	c	; error return
	cp	'F'+1
	jp	nc, no_num	;jr	nc, no_num
dec_num:
	or	a	; clear carry
	ret

;--------------------------------------
; 2 byte machine code branch
; - 2nd byte is Relative address
; - output a = (e-2) relative number
;          CF=1 : target address error
;--------------------------------------
calc_reladr:
	push	hl
	push	de

	ex	de,hl			;z80 code ld	de, (tasm_adr)	; base address
	ld	hl,(tasm_adr)		;###########################
	ex	de,hl			;###########################
	inc	de
	inc	de
	ld	hl, (opr_num25)	; target address
	xor	a
	call	sbc_hlde		;z80 coce sbc	hl, de #####
	jp	c, cal_1	;jr	c, cal_1	; CF=1 :target address is lower

	ld	a, h
	or	a
	jp	nz, adr_ovr	;jr	nz, adr_ovr

	ld	a, l
	cp	80H
	jp	nc, adr_ovr	;jr	nc, adr_ovr
	or	a

cal_01:
	pop	de
	pop	hl
	ret

cal_1:
	ld	a, h
	cp	0ffh
	jp	nz, adr_ovr	;jr	nz, adr_ovr
	ld	a, l
	CPL				;Z80 CODE neg
	INC	A			;flags may be wrong, it's OK here
	cp	81h
	ld	a, l
	ccf
	jp	cal_01		;jr	cal_01

adr_ovr:
	scf
	jp	cal_01		;jr	cal_01

;-----------------------------
;
; Recorrect input strings
;
;-----------------------------
recorrect:
	push	hl
	push	de
	push	bc

	ld	hl, line_buf
	ld	d, h
	ld	e, l

	ld	c, ' '		; space delimitor. (opecode and operand)
	call	skp_sp
	call	recorr
	or	a
	jp	z, gle_end	;jr	z, gle_end

	ld	c, 0		; end delimitor. (end strings)
	inc	hl
	call	skp_sp
	or	a
	jp	z, gle_end1	;jr	z, gle_end1
	call	recorr

gle_end1:
	dec	de
	ld	(de), a		; replace space to 0
gle_end:
	pop	bc
	pop	de
	pop	hl
	ret

; extract space from input buffer
recorr:
	ld	b, 0
recorr1:
	ld	(de), a
	inc	de
	or	a
	ret	z
	cp	c
	ret	z

next_char:
	cp	'"'
	jp	nz, nxchr	;jr	nz, nxchr

	PUSH	HL			;#### ADDED ####################
	PUSH	AF			;Z80 CODE EX	AF,AF' #########
	LD	HL,(alt_AF)		;###############################
	PUSH	HL			;###############################
	POP	AF			;###############################
	POP	HL			;###############################
	LD	(alt_AF),HL		;###############################
	POP	HL			;###############################
	ld	a, b
	xor	a, 1
	ld	b, a		; toggle '"' flag
	PUSH	HL			;#### ADDED ####################
	PUSH	AF			;Z80 CODE EX	AF,AF' #########
	LD	HL,(alt_AF)		;###############################
	PUSH	HL			;###############################
	POP	AF			;###############################
	POP	HL			;###############################
	LD	(alt_AF),HL		;###############################
	POP	HL			;###############################
;"	
nxchr:
	inc	hl
	ld	a, (hl)
	cp	c
	jp	z, recorr1	;jr	z, recorr1
	ld	(alt_A),a	;z80 code bit	0, b ##################
	ld	a,b		;######################################
	and	1		;######################################
	ld	a,(alt_A)	;######################################
	jp	nz, recorr1	;jr	nz, recorr1

	cp	' '
	jp	z, next_char	;jr	z, next_char
	jp	recorr1		;jr	recorr1

;;;
;;; Other support routines
;;;

STROUT:
	LD	A,(HL)
	AND	A
	RET	Z
	CALL	CONOUT
	INC	HL
	JP	STROUT		;JR	STROUT

; input:  HL
; output: 4 hex_char output console

HEXOUT4:
	LD	A,H
	CALL	HEXOUT2
	LD	A,L

; input:  A
; output: 2 hex_char output console
HEXOUT2:
	PUSH	AF
	RRA
	RRA
	RRA
	RRA
	CALL	HEXOUT1
	POP	AF

; input:  A
; output: 1 hex_char output console
HEXOUT1:
	AND	0FH
	ADD	A,'0'
	CP	'9'+1
	JP	C,CONOUT
	ADD	A,'A'-'9'-1
	JP	CONOUT

HEXIN:
	XOR	A
	CALL	HI0
	RLCA
	RLCA
	RLCA
	RLCA
HI0:
	PUSH	BC
	LD	C,A
	CALL	CONIN
	CP	'0'
	JP	C,HIR		;JR	C,HIR
	CP	'9'+1
	JP	C,HI1		;JR	C,HI1
	CP	'A'
	JP	C,HIR		;JR	C,HIR
	CP	'F'+1
	JP	NC,HIR		;JR	NC,HIR
	SUB	'A'-'9'-1
HI1:
	SUB	'0'
	OR	C
HIR:
	POP	BC
	RET
	
CRLF:
	LD	A,CR
	CALL	CONOUT
	LD	A,LF
	JP	CONOUT

CLR_CRT:
	PUSH	HL
	LD	HL, ESC_CRT_CLR
	CALL	STROUT
	POP	HL
	RET
	
ESC_CRT_CLR:
	db	01BH
	db	"[2"
	db	0

GETLIN:
	LD	HL,INBUF
	ld	b, BUFLEN

GL0:	; input hl

	PUSH	de
	push	hl
	ld	e, b	; E: buffer length
	dec	e	; buffer lenght -1
	LD	B,0

GL00:
	CALL	CONIN
	CP	CR
	JP	Z,GLE		;JR	Z,GLE
	CP	LF
	JP	Z,GLE		;JR	Z,GLE
	CP	BS
	JP	Z,GLB		;JR	Z,GLB
	CP	DEL
	JP	Z,GLB		;JR	Z,GLB
	cp	'"'
	jp	nz, GL001	;jr	nz, GL001
	push	af
	ld	a, (ky_flg)
	xor	NO_UPPER	; toggle UPPER or NO UPPER
	ld	(ky_flg), a
	pop	af

GL001:	CP	' '
	JP	C,GL00		;JR	C,GL00
	CP	80H
	JP	NC,GL00		;JR	NC,GL00
	LD	C,A
	LD	A,B
	CP	e	; buffer full check
	JP	NC,GL00		;JR	NC,GL00	; Too long
	INC	B
	LD	A,C
	CALL	CONOUT
	cp	'a'
	jp	c, GL1		;jr	c, GL1
	cp	'z'+1
	jp	nc, GL1		;jr	nc, GL1

	push	hl
	ld	hl, ky_flg
	ld	(alt_A),a	;Z80 code bit	NO_UPPER>>1, (hl)
	ld	a,(hl)		;#################################
	and	NO_UPPER	;#################################
	ld	a,(alt_A)	;#################################
	jp	nz, skip_upper	;jr	nz, skip_upper
	and	0DFH	; make upper code
skip_upper:
	pop	hl
GL1:
	LD	(HL),A
	INC	HL
	JP	GL00		;JR	GL00
GLB:
	LD	A,B
	AND	A
	JP	Z,GL00		;JR	Z,GL00
	DEC	B
	DEC	HL
	LD	A,08H
	CALL	CONOUT
	LD	A,' '
	CALL	CONOUT
	LD	A,08H
	CALL	CONOUT
	JP	GL00		;JR	GL00
GLE:
	push	hl
	ld	hl, ky_flg
	ld	(alt_A),a	;z80 code bit	NO_CR>>1, (hl)
	ld	a,(hl)		;################################
	and	NO_CR		;################################
	ld	a,(alt_A)	;################################
	jp	nz, skip_cr	;jr	nz, skip_cr
	ld	a, CR
	call	CONOUT
skip_cr:
	ld	(alt_A),a	;z80 code bit	NO_LF>>1, (hl)
	ld	a,(hl)		;################################
	and	NO_LF		;################################
	ld	a,(alt_A)	;################################

	jp	nz, skip_lf	;jr	nz, skip_lf
	ld	a, LF
	call	CONOUT
skip_lf:
	push	af		;z80 code res	NO_UPPER>>1,(HL)	; set upper flag
	ld	a,(hl)		;##############################
	and	11111011b	;##############################
	ld	(hl),a		;##############################
	pop	af		;##############################
	pop	hl
	LD	(HL),00H

	pop	hl
	POP	de
	RET

SKIPSP:
	LD	A,(HL)
	CP	' '
	RET	NZ
	INC	HL
	JP	SKIPSP		;JR	SKIPSP

UPPER:
	CP	'a'
	RET	C
	CP	'z'+1
	RET	NC
	ADD	A,'A'-'a'
	RET

RDHEX:
	LD	C,0
	LD	DE,0
RH0:
	LD	A,(HL)
	CP	'0'
	JP	C,RHE		;JR	C,RHE
	CP	'9'+1
	JP	C,RH1		;JR	C,RH1
	CP	'A'
	JP	C,RHE		;JR	C,RHE
	CP	'F'+1
	JP	NC,RHE		;JR	NC,RHE
	SUB	'A'-'9'-1
RH1:
	SUB	'0'
	RLA
	RLA
	RLA
	RLA
	RLA
	PUSH	BC		;###### ADDED ##########################
	LD	B,A		;## DO NOT USE PUSH AF #################
	LD	A,E		;Z80 CODE RL	E ######################
	ADC	A,E		;#######################################
	LD	E,A		;#######################################
	LD	A,D		;Z80 CODE RL	D ######################
	ADC	A,D		;#######################################
	LD	D,A		;#######################################
	LD	A,B		;#######################################
	RLA
	LD	B,A		;#######################################
	LD	A,E		;Z80 CODE RL	E ######################
	ADC	A,E		;#######################################
	LD	E,A		;#######################################
	LD	A,D		;Z80 CODE RL	D ######################
	ADC	A,D		;#######################################
	LD	D,A		;#######################################
	LD	A,B		;#######################################
	RLA
	LD	B,A		;#######################################
	LD	A,E		;Z80 CODE RL	E ######################
	ADC	A,E		;#######################################
	LD	E,A		;#######################################
	LD	A,D		;Z80 CODE RL	D ######################
	ADC	A,D		;#######################################
	LD	D,A		;#######################################
	LD	A,B		;#######################################
	RLA
	LD	B,A		;#######################################
	LD	A,E		;Z80 CODE RL	E ######################
	ADC	A,E		;#######################################
	LD	E,A		;#######################################
	LD	A,D		;Z80 CODE RL	D ######################
	ADC	A,D		;#######################################
	LD	D,A		;#######################################
	LD	A,B		;## DO NOT USE POP AF ##################
	POP	BC		;###### ADDED ##########################
	INC	HL
	INC	C
	JP	RH0		;JR	RH0
RHE:
	ld	a, c
	or	a
	jp	z, rhe1		;jr	z, rhe1
	cp	5
	jp	nc, rhe1	;jr	nc, rhe1
	or	a	; clear carry
	ret
	
rhe1:
	scf	; set carry
	RET

;;;
;;; API Handler
;;:   C : API entory NO.
;;;

RST30H_IN:

	PUSH	HL
	PUSH	BC
	LD	HL,APITBL
	LD	B,0
	ADD	HL,BC
	ADD	HL,BC

	ld	bc, APITBL_E
	or	a
	push	hl
	call	sbc_hlbc		;Z80 CODE sbc	hl, bc #####
	pop	hl
	jp	nc, no_api	;jr	nc, no_api	; request No. is not exist

	LD	B,(HL)
	INC	HL
	LD	H,(HL)
	LD	L,B

	POP	BC
	EX	(SP),HL		; Restore HL, jump address on stack top
no_operate:
	RET

no_api:
	pop	bc
	pop	hl
	ret

APITBL:
	DW	CSTART		; 00: CSTART
	DW	API01		; 01: WSTART
	DW	CONOUT		; 02: CONOUT
	DW	STROUT		; 03: STROUT
	DW	CONIN		; 04: CONIN
	DW	CONST		; 05: CONST
	DW	API06		; 06: PSPEC
	DW	HEXOUT4		; 07: CONOUT HEX4bytes: input HL
	DW	HEXOUT2		; 08: CONOUT HEX2bytes: input A
	DW	HEXOUT1		; 09: CONOUT HEX1byte : input A
	DW	CLR_CRT		; 10: Clear screen (ESC+[2)
	DW	GL0		; 11: GET a line.  input HL : input buffer address)
				;                         B : buffer length
				;                  output B : length of strings
	DW	SKIPSP		; 12: SKIP Spase
	DW	CRLF		; 13: CONOUT CRLF
	DW	UPPER		; 14: Lower to UPPER
	DW	RDHEX		; 15: get hex number from chr buffer
				;     input  HL : hex string buffer
				;     output DE : hex number
				;            CF=1 : error, C, A = hex counts(1-4)
	DW	DEC_STR		; 16: get decimal srtings
				; input hl : return strings buffer addr.
				;       de : 16bit binary
	DW	DIV16_8		; 17; division 16bit / 8bit
	DW	MUL_8		; 18: multiply 8bit * 8bit
	IF RAM12K
	DW	STOPW		; 19: stopwatch
	ELSE
	DW	no_operate	; 19: no operation : if RAM12K=0
	ENDIF
	dw	get_disasm_st	; 20: dis assemble string
				;     input: HL buffer. need 42bytes
				;     output : DE : next MC address
				;              A  : disassembled MC size
	dw	GET_dNUM	; 21: get number from decimal strings
				;     input HL : string buffer
				;     Return
				;        CF =1 : Error
				;        BC: Calculation result
APITBL_E:

	;; WSTART from API
API01:
	LD	SP,STACKM	; reset SP for monitor

; check stop by bp and trace operation

	ld	a, (tmpb_f)
	or	a
	jp	z, ws_chk1	;jr	z, ws_chk1
	ld	hl, tmpb_op
	call	rstr_tpt
ws_chk1:
	ld	a, (tpt1_f)
	or	a
	jp	z, ws_chk2	;jr	z, ws_chk2
	ld	hl, tpt1_op
	call	rstr_tpt

ws_chk2:
	ld	a, (tpt2_f)
	or	a
	jp	z, ws_chk3	;jr	z, ws_chk3
	ld	hl, tpt2_op
	call	rstr_tpt
ws_chk3:
	ld	a, (bpt2_f)
	or	a
	jp	z, ws_chk4	;jr	z, ws_chk4
	ld	hl, bpt2_op
	call	rstr_bpt
ws_chk4:
	ld	a, (bpt1_f)
	or	a
	jp	z, ws_chk5	;jr	z, ws_chk5
	ld	hl, bpt1_op
	call	rstr_bpt
ws_chk5:
	JP	backTomon

	;; PSPEC
API06:
	XOR	A
	RET

;;;
;;; Break Point
;;; trace Point
;;; go, stop point
;;; operation handler
;;
RST38H_IN:
	PUSH	AF
;	LD	A,R			;Z80 CODE ignore now ###########
;	LD	(REGR),A		;## save R reister ignore now ##
;	LD	A,I			;Z80 CODE ignore now ###########
;	LD	(REGI),A		;## save I reister ignore now ##
	LD	(REGHL),HL
	LD	H,D			;Z80 CODE LD	(REGDE),DE #####
	LD	L,E			;Z80 CODE LD	(REGDE),DE #####
	LD	(REGDE),HL		;## HL destroyed it's ok here ##
	LD	H,B			;Z80 CODE LD	(REGBC),BC #####
	LD	L,C			;Z80 CODE LD	(REGBC),BC #####
	LD	(REGBC),HL		;## HL destroyed it's ok here ##
	POP	HL
	LD	(REGAF),HL
;	EX	AF,AF'			;Z80 CODE ignore now ###########
;	PUSH	AF
;	EXX				;Z80 CODE ignore now ###########
;	LD	(REGHLX),HL		;# save HL' reister ignore now #
;	LD	(REGDEX),DE		;# save DE' reister ignore now #
;	LD	(REGBCX),BC		;# save BC' reister ignore now #
;	POP	HL			;# save AF' reister ignore now #
;	LD	(REGAFX),HL		;# save AF' reister ignore now #
;	LD	(REGIX),IX		;Z80 CODE ignore now ###########
;	LD	(REGIY),IY		;Z80 CODE ignore now ###########
	POP	HL
	DEC	HL
	LD	(REGPC),HL
	LD	HL,0			;Z80 CODE LD	(REGSP),SP #####
	ADD	HL,SP			;Z80 CODE LD	(REGSP),SP #####
	LD	(REGSP),HL		;## HL destroyed it's ok here ##

; check bp and trace operation

	LD	SP,STACKM	; reset SP for monitor
	xor	a
	ld	d, a
	ld	e, a		;clear msg pointer


; check go, end operation
	ld	a, (tmpb_f)
	or	a
	jp	z, chk_bp	;jr	z, chk_bp
	ld	de, stpBrk_msg
	ld	hl, tmpb_op
	call	rstr_tpt

; check set break point

chk_bp:
	ld	a, (bpt2_f)
	or	a
	jp	z, bp_chk1	;jr	z, bp_chk1
	ld	de, stpBrk_msg
	ld	hl, bpt2_op
	call	rstr_bpt

bp_chk1:
	ld	a, (bpt1_f)
	or	a
	jp	z, tp_chk1	;jr	z, tp_chk1
	ld	de, stpBrk_msg
	ld	hl, bpt1_op
	call	rstr_bpt

; check trace operation
tp_chk1:
	ld	a, (tpt1_f)
	or	a
	jp	z, tp_chk2	;jr	z, tp_chk2
	ld	de, trace_msg
	ld	hl, tpt1_op
	call	rstr_tpt

tp_chk2:
	ld	a, (tpt2_f)
	or	a
	jp	z, bp_chk_end	;jr	z, bp_chk_end

	ld	de, trace_msg
	ld	hl, tpt2_op
	call	rstr_tpt

bp_chk_end:
	ld	a, d
	or	e
	jp	nz, no_rst38_msg	;jr	nz, no_rst38_msg

	; set RST 38H message
	LD	de,RST38MSG

no_rst38_msg:
	ld	a, (de)		; get first char of message
	cp	'T'		; trace ?
	jp	z, chk_ntrace	;jr	z, chk_ntrace
	
	ex	de, hl
	CALL	STROUT

	;; R register adjustment

	CALL	RDUMP
	call	dis_call	; list disassemble
	jp	backTomon	;jr	backTomon	; goto WBOOT

;
; check continue trace operation
;
chk_ntrace:
	ld	a, (TP_mode)
	cp	'F'		; chk
	jp	z, skp_rmsg	;jr	z, skp_rmsg

;no_trace:
	ex	de, hl
	CALL	STROUT

	;; R register adjustment

	CALL	RDUMP
	call	dis_call	; list disassemble

skp_rmsg:
	call	CONST
	jp	z, t_no_ky	;jr	z, t_no_ky		; no key in
	call	CONIN
	cp	03h	; chk CTL+C
	jp	nz, t_no_ky	;jr	nz, t_no_ky

	; stop_trace
backTomon:
	xor	a
	ld	(fever_t), a	; clear forever flag
	ld	h, a
	ld	l, a
	ld	(TC_cnt), hl
	JP	WSTART
	
	; check trace forever
t_no_ky:
	ld	a, (fever_t)
	or	a
	jp	nz, repeat_trace

	ld	hl, (TC_cnt)
	dec	hl
	ld	(TC_cnt), hl
	ld	a, l
	or	h
	jp	nz, repeat_trace	;jr	nz, repeat_trace
	JP	WSTART

repeat_trace:
	ld	hl, (REGPC)
	ld	(tmpT), hl
	jp	t_op_chk

rstr_tpt:	; HL=buffer point
	push	hl
	ld	a, (hl)
	inc	hl
	ld	c, (hl)
	inc	hl
	ld	b, (hl)

	ld	(bc), a		; restor OP CODE
	pop	hl
	xor	a
	dec	hl
	ld	(hl), a		; clear trace flag
	ret

rstr_bpt:	; HL=buffer point
	ld	a, (hl)
	inc	hl
	ld	c, (hl)
	inc	hl
	ld	b, (hl)

	ld	(bc), a		; restor OP CODE
	ret

dis_call:
	ld	hl, (REGPC)
	ld	(dasm_adr), hl	; set disasm address
	call	dis_analysis
	call	mk_adr_str	; conout address and machine code
	ld	hl, adr_mc_buf
	call	STROUT		; conout disassemble strings
	ld	hl, (REGPC)
	ld	(dasm_adr), hl	; restrore disasm address
	ret

RST38MSG:
	DB	"RST 38H",CR,LF,00H
stpBrk_msg:	
	db	"Break!",CR,LF,00H
trace_msg:	
	db	"Trace!",CR,LF,00H

	IF RAM12K
STOPW:
	; (input) A = 0 : start timer, (output) none
	; (input) A = 1 : stop timer,  (output) BC : msec time, DE : Sec time
	; (input) A = 2 : lap timer,   (output) BC : msec time, DE : Sec time
	; (input) A = 3 : clear timer counter,  (output) none

	push	af
	push	hl
	or	a
	jp	z, start_tim	;jr	z, start_tim
	cp	1
	jp	z, stop_tim	;jr	z, stop_tim
	cp	2
	jp	z, lap_tim	;jr	z, lap_tim
	cp	3
	jp	z, clr_tim	;jr	z, clr_tim
	pop	hl
	pop	af
	ret

start_tim:
	ld	hl, TIM0_CTL0
	ld	a, 90h
	ld	(hl),a		; enable timer 0
	pop	hl
	pop	af
	ret
	
stop_tim:
	ld	hl, TIM0_CTL0
	ld	a, 10h
	ld	(hl), a		; disable timer 0

	; save timer counter

lap_tim:
	ld	a, (TIMER0_CNTL)
	ld	(c16b), a
	ld	a, (TIMER0_CNTH)
	ld	(c16b+1), a

; adjust 16bit counter

	ld	bc, TIMER0_INITC
	ld	hl, (c16b)
	ld	a, h
	or	l	; HL = 0?
	jp	nz, adj_tim1	;jr	nz, adj_tim1
	ld	(c16b), bc
	jp	rd_sec		:jr	rd_sec

adj_tim1:
	call	sbc_hlbc		;Z80 code sbc hl, bc###########
	ld	(c16b), hl

; read seconds counter

rd_sec:
	ld	a, (TIMER0_SCTL)
	ld	(secb), a
	ld	a, (TIMER0_SCTH)
	ld	(secb+1), a

	; convert msec from 16bit counter

	ld	hl,(c16b)
	call	chg_msec	; result BC
	ld	(msecb),bc	; save result
	ld	de,(secb)	; get sec counter
	pop	hl
	pop	af
	ret

clr_tim:
	xor	a
	ld	h, a
	ld	l, a
	ld	(msecb), hl
	ld	(secb), hl
	ld	(c16b), hl
	
	ld	(TIMER0_SCTL), a
	ld	(TIMER0_SCTH), a
	ld	a, 0e8h
	ld	(TIMER0_CNTL), a
	ld	a, 86H
	ld	(TIMER0_CNTH), a
	pop	hl
	pop	af
	ret

chg_msec:
	xor	a
	ld	b, a
	ld	c ,a		;cear BC
	ld	hl, (c16b)	; get 16bit counter
	ld	a, l
u_00:
	cp	31
	jp	c, u_1		;jr	c, u_1
	sub	31
u_0:
	ld	l, a
	inc	bc
	jp	u_00		;jr	u_00

u_1:
	ld	a, h
	or	a
	ret	z	; can't sub

	ld	a, l
	sub	31
	CPL				;Z80 CODE neg
	INC	A			;flags may be wrong, it's OK here
	dec	h
	jp	u_0		;jr	u_0

	ENDIF

;
; make decimal string
;
; input HL : output string buffer
;       DE : 16bit binary
;
; output (HL) : decimal strings

DEC_STR:
	PUSH	AF
	PUSH	BC
	PUSH	DE
	push	hl
;	push	ix			;Z80 CODE NOT TRANSLATED YET

	LD	(tmp_HL),HL		;push	hl
;	pop	ix			;Z80 CODE NOT TRANSLATED YET; ix: save buffer top address

	ex	de, hl		; hl: 16bit binary, de: buffer
	push	hl		; save 16bit binary
	ld	hl, 5
	add	hl, de		; hl = buffer + 5
	xor	a
	ld	(hl), A
	ex	de, hl		; de: buffer + 5, hl : buffer
	pop	hl		; hl : 16bit binary
	LD	BC, 1

LOOP_DEC:
	LD	A, 10
	CALL	DIV16_8
	OR	30H
	DEC	DE
	LD	(DE), A
	INC	C
	LD	A, H
	OR	L
	JP	NZ, LOOP_DEC	;JR	NZ, LOOP_DEC

	LD	A, C
	CP	6
	JP	Z, END_DEC	;JR	Z, END_DEC

;	push	ix			;Z80 CODE NOT TRANSLATED YET
	LD	HL,(tmp_HL)		;pop	hl		; hl : buffer top address
	EX	DE, HL
	PUSH	AF			;Z80 CODE LDIR
LDIR2:	LD	A,(HL)			;###############################
	LD	(DE),A			;###############################
	INC	HL			;###############################
	INC	DE			;###############################
	DEC	BC			;###############################
	LD	A,C			;###############################
	OR	B			;###############################
	JP	NZ,LDIR2		;###############################
	POP	AF			;###############################

END_DEC:
;	pop	ix			;Z80 CODE NOT TRANSLATED YET
	pop	hl
	POP	DE
	POP	BC
	POP	AF
	RET


; DIV 16bit / 8 bit
; input
;	HL, A
; output
;	result = HL, mod = A

DIV16_8:
	PUSH	BC
	PUSH	DE

	LD	C, A
	LD	B, 15
	XOR	A
	ADD	HL, HL
	RLA
	SUB	C
	JP	C, D16_MINUS_BEFORE	;JR	C, D16_MINUS_BEFORE
	ADD	HL, HL
	INC	L

D16_PLUS:
	RLA
	SUB	C
	JP	C, D16_MINUS_AFTER	;JR	C, D16_MINUS_AFTER

D16_PLUS_AFTER:
	ADD	HL, HL
	INC	L
	DEC	B		;Z80 CODE #####	DJNZ	D16_PLUS
	JP	NZ,d16_PLUS	;###############################
	JP	D16_END

D16_MINUS_BEFORE:
	ADD	HL, HL

D16_MINUS:
	RLA
	ADD	A, C
	JP	C, D16_PLUS_AFTER	;JR	C, D16_PLUS_AFTER

D16_MINUS_AFTER:
	ADD	HL, HL
	DEC	B		;Z80 CODE #####	DJNZ	D16_MINUS
	JP	NZ,D16_MINUS	;#################################
	ADD	A,C
D16_END:
	POP	DE
	POP	BC
	RET

;
; input : HL / DE
; output : quotient HL
;	   remainder DE

DIV16:
	LD	(DIV16_NA), HL
	EX	HL,DE			;Z80 CODE LD	(DIV16_NB), DE
	LD	(DIV16_NB),HL		;###############################
	EX	HL,DE			;###############################

	XOR	A
	LD	(DIV16_NC), A
	LD	(DIV16_NC+1), A
	LD	(DIV16_ND), A
	LD	(DIV16_ND+1), A
	LD	B, 16

DIV16_X2:
	LD	HL, DIV16_NC
	LD	A,(HL)			;Z80 CODE SLA	(HL) ###########
	ADD	A,A			;###############################
	LD	(HL),A			;###############################
	INC	HL
	LD	A,(HL)			;Z80 CODE RL	(HL) ###########
	ADC	A,A			;###############################
	LD	(HL),A			;###############################

	LD	HL, DIV16_NA
	LD	A,(HL)			;Z80 CODE SLA	(HL) ###########
	ADD	A,A			;###############################
	LD	(HL),A			;###############################
	INC	HL
	LD	A,(HL)			;Z80 CODE RL	(HL) ###########
	ADC	A,A			;###############################
	LD	(HL),A			;###############################
	LD	HL, DIV16_ND
	LD	A,(HL)			;Z80 CODE RL	(HL) ###########
	ADC	A,A			;###############################
	LD	(HL),A			;###############################
	INC	HL
	LD	A,(HL)			;Z80 CODE RL	(HL) ###########
	ADC	A,A			;###############################
	LD	(HL),A			;###############################

	LD	HL, (DIV16_NB)
	LD	E, L
	LD	D, H
	LD	HL, (DIV16_ND)
	XOR	A
	call	sbc_hlde		;Z80 CODE SBC	HL,DE ##########
	JP	NC, DIV16_X0	;JR	NC, DIV16_X0
	JP	DIV16_X1	;JR	DIV16_X1

DIV16_X0:
	LD	(DIV16_ND), HL

	LD	A, (DIV16_NC)
	OR	1
	LD	(DIV16_NC), A

DIV16_X1:
	DEC	B			;Z80 CODE DJNZ	DIV16_X2 #######
	JP	NZ,DIV16_X2		;###############################
	LD	HL,(DIV16_ND)		;Z80 COCE LD	DE,(DIV16_ND) ##
	LD	D,H			;## HL desttoyed it's ok here ##
	LD	E,L			;## HL desttoyed it's ok here ##
	LD	HL,(DIV16_NC)
	RET

; 8bit * 8bit : ans = 16bit
; input A , BC
; output HL

MUL_8:
	PUSH	AF
	PUSH	BC
	PUSH	DE			;## ADDED ######################
	OR	A	; clear carry
	JP	ST_MUL8		;JR	ST_MUL8

LOOP_M8:
	LD	D,A			;##### DO NOT USE PUSH AF ######
	LD	A,C			;Z80 CODE SLA	C	########
	ADD	A,C			;###############################
	LD	C,A			;###############################
	LD	A,B			;Z80 CODE RL	B	########
	ADC	A,B			;###############################
	LD	B,A			;###############################
	LD	A,D			;##### DO NOT USE POP AF #######

ST_MUL8:
	RRA
	JP	NC, LOOP_M8	;JR	NC, LOOP_M8
	ADD	HL, BC
	JP	NZ, LOOP_M8	;JR	NZ, LOOP_M8
	POP	DE			;## ADDED ######################
	POP	BC
	POP	AF
	RET
;;;
;;; Messages
;;;

cmd_hlp:	db	"? :Command Help", CR, LF
		db	"#L|<num> :Launch program", CR, LF
		db	"A[<address>] : Mini Assemble mode", CR, LF
		db	"B[1|2[,<adr>]] :Set or List Break Point", CR, LF
		db	"BC[1|2] :Clear Break Point", CR, LF
		db	"D[<adr>] :Dump Memory", CR, LF
		db	"DI[<adr>][,s<steps>|<adr>] :Disassemble", CR, LF
		db	"G[<adr>][,<stop adr>] :Go and Stop", CR, LF
		db	"L :Load HexFile", CR, LF
		db	"P[I|S] :Save HexFile(I:Intel,S:Motorola)", CR, LF
		db	"R[<reg>] :Set or Dump register", CR, LF
		db	"S[<adr>] :Set Memory", CR, LF
		db	"T[<adr>][,<steps>|-1] : Trace command", CR, LF
		db	"TM[I|S] :Trace Option for CALL", CR, LF
		db	"TP[ON|OFF] :Trace Print Mode", CR, LF

OPNMSG:
	DB	CR,LF
	db	"EMUZ80 MONITOR Rev.B03",CR,LF
	db	"This monitor is based on unimon.",CR,LF,00H

PROMPT:
	DB	"] ",00H

IHEMSG:
	DB	"Error ihex",CR,LF,00H
SHEMSG:
	DB	"Error srec",CR,LF,00H
ERRMSG:
	DB	"Error",CR,LF,00H

DSEP0:
	DB	" :",00H
DSEP1:
	DB	" | ",00H

dsap	db	"  ", 00

IHEXER:
        DB	":00000001FF",CR,LF,00H
SRECER:
        DB	"S9030000FC",CR,LF,00H

	;; Register dump table
RDTAB:	DW	RDSA,   REGAF+1
	DB	1
	DW	RDSBC,  REGBC
	DB	2
	DW	RDSDE,  REGDE
	DB	2
	DW	RDSHL,  REGHL
	DB	2
	DW	RDSF,   REGAF
	DB	1

	DW	RDSIX,  REGIX
	DB	2
	DW	RDSIY,  REGIY
	DB	2

	DW	RDSAX,  REGAFX+1
	DB	1
	DW	RDSBCX, REGBCX
	DB	2
	DW	RDSDEX, REGDEX
	DB	2
	DW	RDSHLX, REGHLX
	DB	2
	DW	RDSFX,  REGAFX
	DB	1

	DW	RDSSP,  REGSP
	DB	2
	DW	RDSPC,  REGPC
	DB	2
	DW	RDSI,   REGI
	DB	1
	DW	RDSR,   REGR
	DB	1

	DW	0000H,  0000H
	DB	0

RDSA:	DB	"A =",00H
RDSBC:	DB	" BC =",00H
RDSDE:	DB	" DE =",00H
RDSHL:	DB	" HL =",00H

RDSF:	DB	" F =",00H

RDSF_H	equ	RDSF >> 8
RDSF_L	equ	RDSF & 0FFh


RDSIX:	DB	" IX=",00H
RDSIY:	DB	" IY=",00H
RDSAX:	DB	CR,LF,"A'=",00H
RDSBCX:	DB	" BC'=",00H
RDSDEX:	DB	" DE'=",00H
RDSHLX:	DB	" HL'=",00H

RDSFX:	DB	" F'=",00H

RDSFX_H	equ	RDSFX >> 8
RDSFX_L	equ	RDSFX & 0FFh

RDSSP:	DB	" SP=",00H
RDSPC:	DB	" PC=",00H
RDSI:	DB	" I=",00H
RDSR:	DB	" R=",00H

RNTAB:
	DB	'A',0FH		; "A?"
	DW	RNTABA,0
	DB	'B',0FH		; "B?"
	DW	RNTABB,0
	DB	'C',0FH		; "C?"
	DW	RNTABC,0
	DB	'D',0FH		; "D?"
	DW	RNTABD,0
	DB	'E',0FH		; "E?"
	DW	RNTABE,0
	DB	'F',0FH		; "F?"
	DW	RNTABF,0
	DB	'H',0FH		; "H?"
	DW	RNTABH,0
	DB	'I',0FH		; "I?"
	DW	RNTABI,0
	DB	'L',0FH		; "L?"
	DW	RNTABL,0
	DB	'P',0FH		; "P?"
	DW	RNTABP,0
	DB	'R',1		; "R"
	DW	REGR,RNR
	DB	'S',0FH		; "S?"
	DW	RNTABS,0

	DB	00H,0		; End mark

RNTABA:
	DB	00H,1		; "A"
	DW	REGAF+1,RNA
	DB	27H,1		; "A'"
;;	DB	'\'',1		; "A'"
	DW	REGAFX+1,RNAX

	DB	00H,0
	
RNTABB:
	DB	00H,1		; "B"
	DW	REGBC+1,RNB
	DB	27H,1		; "B'"
;;	DB	'\'',1		; "B'"
	DW	REGBCX+1,RNBX
	DB	'C',0FH		; "BC?"
	DW	RNTABBC,0

	DB	00H,0		; End mark

RNTABBC:
	DB	00H,2		; "BC"
	DW	REGBC,RNBC
	DB	27H,2		; "BC'"
;;	DB	'\'',2		; "BC'"
	DW	REGBCX,RNBCX

	DB	00H,0
	
RNTABC:
	DB	00H,1		; "C"
	DW	REGBC,RNC
	DB	27H,1		; "C'"
;;	DB	'\'',1		; "C'"
	DW	REGBCX,RNCX

	DB	00H,0
	
RNTABD:
	DB	00H,1		; "D"
	DW	REGDE+1,RND
	DB	27H,1		; "D'"
;;	DB	'\'',1		; "D'"
	DW	REGDEX+1,RNDX
	DB	'E',0FH		; "DE?"
	DW	RNTABDE,0

	DB	00H,0

RNTABDE:
	DB	00H,2		; "DE"
	DW	REGDE,RNDE
	DB	27H,2		; "DE'"
;;	DB	'\'',2		; "DE'"
	DW	REGDEX,RNDEX

	DB	00H,0
	
RNTABE:
	DB	00H,1		; "E"
	DW	REGDE,RNE
	DB	27H,1		; "E'"
;;	DB	'\'',1		; "E'"
	DW	REGDEX,RNEX

	DB	00H,0
	
RNTABF:
	DB	00H,1		; "F"
	DW	REGAF,RNF
	DB	27H,1		; "F'"
;;	DB	'\'',1		; "F'"
	DW	REGAFX,RNFX

	DB	00H,0
	
RNTABH:
	DB	00H,1		; "H"
	DW	REGHL+1,RNH
	DB	27H,1		; "H'"
;;	DB	'\'',1		; "H'"
	DW	REGHLX+1,RNHX
	DB	'L',0FH		; "HL?"
	DW	RNTABHL,0

	DB	00H,0

RNTABHL:
	DB	00H,2		; "HL"
	DW	REGHL,RNHL
	DB	27H,2		; "HL'"
;;	DB	'\'',2		; "HL'"
	DW	REGHLX,RNHLX

	DB	00H,0
	
RNTABL:
	DB	00H,1		; "L"
	DW	REGHL,RNL
	DB	27H,1		; "L'"
;;	DB	'\'',1		; "L'"
	DW	REGHLX,RNLX

	DB	00H,0
	
RNTABI:
	DB	00H,1		; "I"
	DW	REGI,RNI
	DB	'X',2		; "IX"
	DW	REGIX,RNIX
	DB	'Y',2		; "IY"
	DW	REGIY,RNIY
	
	DB	00H,0

RNTABP:
	DB	'C',2		; "PC"
	DW	REGPC,RNPC

	DB	00H,0

RNTABS:
	DB	'P',2		; "SP"
	DW	REGSP,RNSP

	DB	00H,0

RNA:	DB	"A",00H
RNBC:	DB	"BC",00H
RNB:	DB	"B",00H
RNC:	DB	"C",00H
RNDE:	DB	"DE",00H
RND:	DB	"D",00H
RNE:	DB	"E",00H
RNHL:	DB	"HL",00H
RNH:	DB	"H",00H
RNL:	DB	"L",00H
RNF:	DB	"F",00H
RNAX:	DB	"A'",00H
RNBCX:	DB	"BC'",00H
RNBX:	DB	"B'",00H
RNCX:	DB	"C'",00H
RNDEX:	DB	"DE'",00H
RNDX:	DB	"D'",00H
RNEX:	DB	"E'",00H
RNHLX:	DB	"HL'",00H
RNHX:	DB	"H'",00H
RNLX:	DB	"L'",00H
RNFX:	DB	"F'",00H
RNIX:	DB	"IX",00H
RNIY:	DB	"IY",00H
RNSP:	DB	"SP",00H
RNPC:	DB	"PC",00H
RNI:	DB	"I",00H
RNR:	DB	"R",00H
;;;
;;; CPIR for 8085 P/V flag may be wrong. it's ok here.
;;;

CPIR80:	CP	(HL)
	INC	HL
	DEC	BC
	RET	Z
	PUSH	AF
	LD	A,B
	OR	C
	JP	Z,CPIR8E
	POP	AF
	JP	CPIR80
CPIR8E:	POP	AF
	RET

SBC_HLBC:
	PUSH	DE			;Z80 CODE sbc	hl, bc
	LD	D,A
	LD	A,L
	SBC	A,C
	LD	L,A
	LD	A,H
	SBC	A,B
	LD	H,A
	JP	C,SBCHLBC1
	JP	M,SBCHLBC1
	OR	L
	JP	NZ,SBCHLBC2
SBCHLBC1:
	LD	A,D
	POP	DE
	RET
SBCHLBC2:
	LD	A,1
	OR	A
	LD	A,D
	POP	DE
	RET

SBC_HLDE:
	PUSH	BC			;Z80 CODE sbc	hl, de
	LD	B,A
	LD	A,L
	SBC	A,E
	LD	L,A
	LD	A,H
	SBC	A,D
	LD	H,A
	JP	C,SBCHLDE1
	JP	M,SBCHLDE1
	OR	L
	JP	NZ,SBCHLDE2
SBCHLDE1:
	LD	A,B
	POP	BC
	RET
SBCHLDE2:
	LD	A,1
	OR	A
	LD	A,B
	POP	BC
	RET

;;;
;;; Console drivers
;;;

	IF	UARTMEM
CONIN:
	LD	A,(UARTCR)
	AND	1
	JP	Z,CONIN		;JR	Z,CONIN
	LD	A,(UARTDR)
	RET

CONST:
	LD	A,(UARTCR)
	AND	1
	RET

CONOUT:
	PUSH	AF
PCST1:	LD	A,(UARTCR)
	AND	2
	JP	Z,PCST1		;JR	Z,PCST1
	POP	AF
	LD	(UARTDR),A
	RET

	ENDIF

	IF	UARTPIC

UARTDR_PIC	EQU	00h	; UART DATA REGISTOR
UARTCR_PIC	EQU	11h	; UART CONTROL REGISTOR

CONIN:
	IN	A,(UARTCR_PIC)
	AND	1
	JP	Z,CONIN		;JR	Z,CONIN
	IN	A,(UARTDR_PIC)
	RET

CONST:
	IN	A,(UARTCR_PIC)
	AND	1
	RET

CONOUT:
	PUSH	AF
PCST1:	IN	A,(UARTCR_PIC)
	AND	2
	JP	Z,PCST1		;JR	Z,PCST1
	POP	AF
	OUT	(UARTDR_PIC),A
	RET

	ENDIF

	IF	UART8251

UARTDR_8251	EQU	00h	; UART DATA REGISTOR
UARTCR_8251	EQU	01h	; UART CONTROL REGISTOR

CONIN:
	IN	A,(UARTCR_8251)
	AND	2
	JP	Z,CONIN		;JR	Z,CONIN
	IN	A,(UARTDR_8251)
	RET

CONST:
	IN	A,(UARTCR_8251)
	AND	2
	RET

CONOUT:
	PUSH	AF
PCST1:	IN	A,(UARTCR_8251)
	AND	1
	JP	Z,PCST1		;JR	Z,PCST1
	POP	AF
	OUT	(UARTDR_8251),A
	RET

	ENDIF

	IF	Fullram
	db	0FE00H - $ dup(0ffH) 
	ELSE
	db	BASIC_TOP - $ dup(0ffH) 
	ENDIF

;	ORG	BASIC_TOP

;;;
;;; RAM area
;;;

	;;
	;; Work Area
	;;
	
	ORG	WORK_B

	IF	BACKDOOR
stealRST08:	ds	2	; hacking RST 08H, set jump address
stealRST10:	ds	2	; hacking RST 10H, set jump address
stealRST18:	ds	2	; hacking RST 18H, set jump address
stealRST20:	ds	2	; hacking RST 20H, set jump address
stealRST28:	ds	2	; hacking RST 28H, set jump address
stealRST30:	ds	2	; hacking RST 30H, set jump address
stealRST38:	ds	2	; hacking RST 38H, set jump address
save_hl:	ds	2
	ENDIF
	
DSADDR:	DS	2	; Dump start address
DEADDR:	DS	2	; Dump end address
DSTATE:	DS	1	; Dump state
GADDR:	DS	2	; Go address
SADDR:	DS	2	; Set address
HEXMOD:	DS	1	; HEX file mode
RECTYP:	DS	1	; Record type
SIZE:	DS	1	; I/O Size 00H,'W','S'

REG_B:	
REGAF:	DS	2
REGBC:	DS	2
REGDE:	DS	2
REGHL:	DS	2
REGAFX:	DS	2		; Register AF'
REGBCX:	DS	2
REGDEX:	DS	2
REGHLX:	DS	2		; Register HL'
REGIX:	DS	2
REGIY:	DS	2
REGSP:	DS	2
REGPC:	DS	2
REGI:	DS	1
REGR:	DS	1
REG_E:

	IF RAM12K
; stop watch timer

c16b:	ds	2	; copy of TIMER0_CNT
secb:	ds	2	; copy of TIMER0_SCTT
msecb:	ds	2
	ENDIF

; DIV 16 /16 buffer
DIV16_NA:	ds	2
DIV16_NB:	ds	2
DIV16_NC:	DS	2
DIV16_ND:	DS	2

; go command temp start address
goTmp:		ds	2

; trace mode switch
TP_mode:	ds	1	; N: display on, F: display off
TM_mode:	ds	1	; 'S':skip call, 'I':trace CALL IN
TC_cnt:		ds	2	; numbers of step
tmpT:		ds	2	; save temp buffer
fever_t:	ds	1	; flag trace forever

; break, trace point work area
dbg_wtop	equ	$
tpt1_f:		ds	1
tpt1_op:	ds	1	; save trace point1 opcode
tpt1_adr:	ds	2
tpt2_f:		ds	1
tpt2_op:	ds	1	; save trace point2 opcode (for branch)
tpt2_adr:	ds	2

; break point work area
bpt1_f:		ds	1
bpt1_op:	ds	1
bpt1_adr:	ds	2
bpt2_f:		ds	1
bpt2_op:	ds	1
bpt2_adr:	ds	2

tmpb_f:		ds	1
tmpb_op:	ds	1
tmpb_adr:	ds	2
dbg_wend	equ	$

; minimal assembler

asm_adr:	ds	2
tasm_adr:	ds	2
element_cnt:	ds	1	; Element count of input string
byte_count:	ds	1	; numbers of Machine code
opc_cd:		ds	1	; opecode number
opr1_cd:	ds	1	; 1st operand number
opr2_cd:	ds	1	; 2nd operand number
opr_num25:	ds	2	; save number nnnn
opr_num35:	ds	2	; save number (nnnn)
opr_num37:	ds	1	; save number (IY-nn)
opr_num34:	ds	1	; save number (IY+nn)
opr_num36:	ds	1	; save number (IX-nn)
opr_num33:	ds	1	; save number (IX+nn)
cb_mcw		ds	2	; use BIT, RES, SET
cb_xyw		ds	2	; use BIT, RES, SET

F_bit:		ds	F_bitSize+1
ky_flg:		ds	1	; b00000ULC ( U: UPPER L: LF C: CR )
				; U=0: Lower To Upper
				; L=0: End of line is added LF
				; C=0: End of line is added CR
;::::::::::::::::::::::::::
; DISASEM work area
;;;;;;;;;;;;;;;;;;;;;;;;;;;
dasm_ed:	ds	2
dasm_stpf:	ds	1
dasm_adr:	ds	2
reg_xy:		ds	2	; RNIX or RNIY
xy_srtp		ds	2	; strings pointer

mc_Size:	ds	1

;;;;;;;;;;;;;;;;;;;
; union area
;;;;;;;;;;;;;;;;;;;
INBUF: ; Line input buffer
line_buf:

;  DidAsm string buffer
; total 42 bytes
dasm_bs:
adr_mc_buf:	ds	19
dasm_OpcStr:	ds	8
dasm_OprStr:	ds	15 - NUMLEN
num_string:	ds	NUMLEN	; max 65536 or 0FFFFH + null( max 7bytes )
dasm_be:

alt_AF:		ds	1
alt_A:		ds	1
tmp_HL:		ds	2
	END
