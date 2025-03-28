		title "int_test"
;=====================================================
;IRQ Interrupt test code 
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
IRV		equ	$a000	;Interupt vector point for IRQ
;
; RAM area
;
                bss
                org     $6000
;
counter		ds	2	

		code
		org	$7000
;
;=====================================================
;
start		ldx	#int_routine	;set IRQ vector to point at interrupt routine
		stx	IRV
		clr	counter		;clear counter
		ldaa	#$05		;turn on interrupts on CA1
		staa	PIABCN
		cli
		jmp	CONTRL		;exit		
;
int_routine	;ldaa	PIABCN	
		ldaa	PIABPR		;read PIA to reset interrupt
		ldaa	counter		;put counter byte into PIAB
		bne	cont
		ldab	#$41
		stab	$a080
		ldab	#$01
		stab	$a081
cont		anda	#$F0		;only keep upple 4 bits
		oraa	#$80		;keep E high and RS=low for LCD display	
		staa	PIABPR
		inc	counter
		rti	
							
