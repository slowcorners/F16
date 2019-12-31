; ==============================================================================
; F16 - A Fig-FORTH for Commander X16
; ==============================================================================

; DEBUG setting during development

DEBUG=1			; Defined:	Include DEBUG code
			; Not defined:	Assemble release version

; X16 BASIC DEFINITIONS

X16STRT=$0801		; Location of this code, i.e.
			; X16 BASIC: "SYS 2064" (a.k.a. SYS ORIG)

; X16 KERNAL API DEFINITIONS

CHROUT=$FFD2		; CHROUT outputs a character (C64 Kernal API)
CHRIN=$FFCF		; CHRIN read from default input

; GLOBAL FORTH DEFINITIONS

SSIZE=128		; Sector size in bytes
NBUF=8			; Number of RAM buffers
			; (SSIZE*NBUF >= 1024)
SECTR=800		; Sectors per drive ...
			; ... forcing high drive to screen 100
SECTL=1600		; Sector limit for 2 drives ...
			; ... of 800 sectors per drive
BMAG=1056		; Total buffer magnitude, in bytes ...
			; ... expressed by SSIZE+4*NBUF
BOS=$20			; Bottom of data stack, in z-page
TOS=$9E			; Top of data stack, in z-page
N=TOS+8			; xXxxxxxxx     Scratch workspace
IP=N+8			;  Xx           Interpretive pointer
W=IP+3			; xXx           Code field pointer
UP=W+2			;  Xx           User Area pointer
XSAVE=UP+2		;  X            Temporary for X register
XW=XSAVE+2		; Scratch register to text code field address
NP=XW+2			; Scratch register pointing to names CHK+++

TIBX=$0100		; Terminal input buffer of 84 bytes
ORIG=$0810		; Origin of FORTH's dictionary (for X16)
MEM=$4000		; Top of assigned memory+1 byte
UAREA=MEM-128		; 128 bytes of user area
DAREA=UAREA-BMAG	; Disk buffer area

OUTCH=CHROUT		; Common FORTH name (i.e. alias)
INCH=CHRIN		; :

; ------------------------------------------------------------------------------
; X16 Preamble to make this machine code fit into the X16 Basic "shell"
;
*=X16STRT		; Assembled code should start at $0801
			; (where BASIC programs starts)
			; The real program starts at $0810 = 2064
!byte	$0C,$08		; $080C - pointer to next line of BASIC code
!byte	$0A,$00		; 2-byte line number ($000A = 10)
!byte	$9E		; SYS BASIC token
!byte	$20		; [space]
!byte	$32,$30,$36,$34	; $32="2",$30="0",$36="6",$34="4"
			; (ASCII encoded nums for dec starting addr)
!byte	$00		; End of Line
!byte	$00,$00		; This is address $080C containing
			; 2-byte pointer to next line of BASIC code
			; ($0000 = end of program)

; ------------------------------------------------------------------------------
; BOOT UP PARAMETERS. This area provides jump vectors to boot up code, and
;                     parameters describing the system.

*=ORIG                 ; Here starts the real program

ENTER:	NOP		; User cold entry point
	JMP	COLD+2	; Vector to COLD entry
REENTER:
	NOP		; User warm entry point
	JMP	WARM	; Vector to WARM entry
!word	$0004		; 6502 coded in radix-36
!word	$5ED2		; :
!word	NTOP		; Name address of MON
!word	$7F		; Backspace character
!word	UAREA		; Initial user area
!word	TOS		; Initial top of stack
!word	$1FF		; Initial top of return stack
!word	TIBX		; Initial terminal input buffer
!word	31		; Max. name field width
!word	0		; 0:no disk, 1:disk
!word	TOP		; Initial fence address
!word	TOP		; Initial top of dictionary
!word	VL0		; Initial vocabulary link pointer

; NOTE! The following offset adjusts all code fields to avoid an address
; ending in $xxFF. This must be checked and altered on any alteration, for the
; indirect jump at W-1 to function!

*=*+2			; ALWAYS CHECK!

; ------------------------------------------------------------------------------
; NUCLEUS

L22:	!byte	$83,"L","I",$D4				; ***** LIT
	!word	0
LIT:	!word	*+2
	LDA	(IP), Y
	PHA
	INC	IP
	BNE	+
	INC	IP+1
+	LDA	(IP), Y
L31:	INC	IP
	BNE	LPUSH
	INC	IP+1
LPUSH:	DEX
	DEX
LPUT:	STA	1, X
	PLA
	STA	0, X
NEXT:	LDY	#1	; The FORTH Inner Interpreter
	LDA	(IP), Y
	STA	W+1
	DEY
	LDA	(IP), Y
	STA	W
!ifdef DEBUG {
	JSR	TRACE	; Only during development
}
	CLC
	LDA	IP
	ADC	#2
	STA	IP
	BCC	+
	INC	IP+1
+	JMP	W-1

L35:	!byte	$84,"C","L","I",$D4			; ***** CLIT
	!word	L22
CLIT:	!word	*+2
	LDA	(IP), Y
	PHA
	TYA
	BEQ	L31	; re-use code from LIT

!ifdef DEBUG {
; ==============================================================================
; D E B U G   C O D E

; NOTE! These are temporary trace routines to be used until the FORTH is
; generally operating. Then NOP the terminal query "JSR ONEKEY". This will
; allow user input to the text interpreter. When crashes occur, the display
; shows IP, and the word locations of offending code. When all is well, remove:
; TRACE, TCOLON, PRNAM, DECNP and the following monitor/register equates.

LETTER=CHROUT
ONEKEY=CHRIN

XBLANK:	LDA	#$20		; Print <SPACE>
	JSR	CHROUT
	RTS

CRLF:	LDA	#$0D		; Print <CR><LF>
	JSR	CHROUT
	LDA	#$0A
	JSR	CHROUT
	RTS

TCOLON:	STX	XSAVE		; TCOLON is called from DOCOL to label
	LDA	W		; each point where FORTH "nests" one level.
	STA	NP		; :
	LDA	W+1		; :
	STA	NP+1		; :
	JSR	CRLF		; :
	LDA	#":"		; :
	JSR	CHROUT		; :

; -------------------------------
; Output one byte as two HEX characters

HNYBB:	LSR			; Move high nybble to low nybble
	LSR			; :
	LSR			; :
	LSR			; :
LNYBB:	AND	#$0F		; Low nybble only
	CLC			; Assume 0 - 9
	ADC	#"0"		; :
	CMP	#"9"		; Within '0' - '9'?
	BCC	+		; Yes: Do nothing more
	ADC	#7		; Yes: Adjust char to range 'A' - 'F'
+	JSR	CHROUT
	RTS

HEX2:	PHA			; Print one byte in ASCII-HEX
	JSR	HNYBB
	PLA
	JSR	LNYBB
	RTS

; -------------------------------

TRACE:	STX	XSAVE		; ------------------------------
	JSR	CRLF		; Print IP
	LDA	IP+1		; :
	JSR	HEX2		; :
	LDA	IP		; :
	JSR	HEX2		; :
	JSR	XBLANK		; :

	LDY	#0		; ------------------------------
	LDA	(IP), Y		; Print dictionary name
	STA	XW		; :
	STA	NP		; :
	INY			; :
	LDA	(IP), Y		; :
	STA	XW+1		; :
	STA	NP+1		; :
	JSR	PRNAM		; :

	LDA	XW+1		; ------------------------------
	JSR	HEX2		; Print code field address
	LDA	XW		; :
	JSR	HEX2		; :
	JSR	XBLANK		; :

	LDA	XSAVE		; ------------------------------
	JSR	HEX2		; Print stack location in z-page
	JSR	XBLANK		; :

	LDA	#1		; ------------------------------
	JSR	HEX2		; Print return stack location in 1-page
	TSX			; :
	INX			; :
	TXA			; :
	JSR	HEX2		; :
	JSR	XBLANK		; :

	JSR	ONEKEY		; ------------------------------
	LDX	XSAVE		; Wait for operator keystroke
	LDY	#0		; :
	RTS
}


;   ldx	#0	; X register is used to index the string
; loop:
; 	lda	.string,x ; Load character from string into A reg
; 	beq	end	; If the character was 0, jump to end label
; 	jsr	CHROUT	; Output character stored in A register
; 	inx		; Increment X register
; 	jmp	loop	; Jump back to loop label to print next char
; end:
; 	jsr	CHRIN	; Read input until Enter/Return is pressed
; 	rts		; Return to caller
;
; .string !pet	"hello, world!!",13,0
