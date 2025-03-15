		title "lcd"
;=====================================================
;LCD driver and test code
;
;
;=====================================================
;
;Equates
;
CONTRL          equ 	$e0e3	;Return the TBUG    
PIABPR          equ     $8012
PIABCN		equ	$8013
PDATA		equ	$e07e
INCH		equ	$e1ac
;
; Common ASCII control codes
;
EOT             equ     $04
BEL             equ     $07
BS              equ     $08
LF              equ     $0a
FF		equ	$0c
CR              equ     $0d
DEL             equ     $7f
NULL		equ	$0
RUB		equ	$7f
;
SPACE		equ	$20	;space character
;
;LCD constants (now in driver code)
;
LCDWIDTH	equ	16			;16x2 display
LCDROWS		equ	2
LCDBUFSZ        equ     LCDWIDTH*LCDROWS        ;size of backing ram for display

;
; RAM area
;
                bss
                org     $1000
;
lcd_row		ds	1		;Current lcd row, 0 = top row	
lcd_col		ds	1		;Current lcd column, 0 = left-most column
lcd_buf		ds	LCDBUFSZ	;backing buffer for scrolling, etc.
lcd_tempx	ds	2		;tenp store for x reg, used to calculate backing store offsets	
lcd_buf_addr	ds	2		;used to store buffer address value so it can be split into msb/lsb
lcd_charx	ds	3		;temp store for x reg - used to preserve x during lcd_char calls
;
pdatax		ds	2		;Temp pdata x store
;
;Main code	
;
		code
		org	$0100
;
;=====================================================
;
start		jsr	piab_init	;Initalize PIAB
		jsr 	lcd_init	;Initialize lcd dsiplay
;

inchlp		;jsr	INCH
		;jsr	lcd_outch
		;bra	inchlp



more		ldx	#STR1
		bsr	lcd_pdata
		ldx	#$0
		jsr	ldelay
		ldx	#STR2
		bsr	lcd_pdata
		ldx	#$0
		jsr	ldelay
		;jmp	CONTRL
		ldx	#STR3
		bsr	lcd_pdata
		ldx	#$0
		jsr	ldelay
		ldx	#STR4
		bsr	lcd_pdata
		ldx	#$0
		jsr	ldelay
		ldx	#STR5
		bsr	lcd_pdata
		ldx	#$0
		jsr	ldelay
		ldx	#STR6
		bsr	lcd_pdata
		ldx	#$0
		jsr	ldelay
		bra	more
;
		jmp	CONTRL	
;
lcd_pdata   	stx	pdatax
		ldaa     0,x            ; output area of memory as text
        	cmpa    #EOT            ;until eot(04) is found
        	beq     pdata_exit
		jsr 	lcd_outch
		ldx	pdatax
		inx
		bra	lcd_pdata
pdata_exit     	rts
;

;test strings
;
STR1		db	FF,"Line 1",CR,LF,EOT
STR2		db	"Line 2",EOT
STR3		db	" Line 3",EOT
STR4		db	LF, "*",EOT
STR5		db	"This wrapped",EOT
STR6		db	FF,"Welcome to the display!",EOT
;
;####################################
;Driver code
;####################################
;
		include		"lcd_driver.asm"
