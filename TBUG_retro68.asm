		title "tbug"
;
; Complete source to Newbear TBUG, hand-typed in
;
; Revision histoy:
; 	1.0	Initial version
;	2.0	Version for Retro68 project
;	2.1	2/16/25 - Removed VDU drivers, changed 'U' to 'Z' ROM command
;	2.2	2/24/25 - Removed remaining VDU mem and code, stack matches TBUG OG
;			  PIAB code removed, moved up remaining TBUG scratchpad to end of stack
;			  vs. A080H	
;	2.3	3/15/25	- Moved TBUG specific scratchpad back to A080H since the prior release conflicted
;			  with the stack for loaded program such as BASIC (duh). Also, hooked up LCD dislay
;			  code.			  			
;	2.4	3/15/25 - Added code to fix RUBIG and WAIT issues.
;
; Note: Spare space labbeled with 'SPARE SPACE'
;
; TEMP Notes area:
;
; - Tagged all the NOPs as SPARE SPACE
; - Removed all PIA B references for now - commented out PIABCN, PIABPR, XIABCN, XIABPR
; 
;
;***************************************************
; This macro is used to verify that the current
; address meets a required value.  Used mostly to
; guarantee changes don't cause entry points to
; move.  These are used right before documented
; entry points.
;
VERIFY          macro   expected
                if * != expected
                fail    Not at requested address (expected)
                endif
                endm
;
;
                bss
                org     $A000
;
STACK_SIZE      equ     46	;as per orig TBUG source, not xTBUG
;
;LCD display size
;
LCDWIDTH       equ     16                      ;16x2 display
LCDROWS        equ     2
LCDBUFSZ       equ     LCDWIDTH*LCDROWS        ;size of backing ram for display
;
; Scratchpad RAM - xTBUG version to maintain compatibility
;
IRV             ds      2       ;IRQ POINTER
BEGA            ds      2       ;BEGINNING ADDR PNCH
ENDA            ds      2       ;ENDING ADDR PNCH
NMV             ds      2       ;NMI INTERRUPT VECTOR
SP              ds      1       ;S HIGH
                ds      1       ;S LOW
CKSM            ds      1       ;CHECKSUM
BYTECT          ds      1       ;BYTECT AND MCONT TEMP.
XHI             ds      1       ;XREG HIGH
XLOW            ds      1       ;XREG LOW
TEMP            ds      1       ;TEMPORARY STORAGE
TW              ds      2       ;TEMPORARY STORAGE
MCONT           ds      1       ;Frame count
XTEMP           ds      2       ;X-REG TEMP STGE
                ds      STACK_SIZE
STACK           ds      1       ;SWTBUG STACK
STACIN		ds	5	;RTI Stack (orig TBUG source has this as 7 bytes)
;
; Note: The locations A048 and A049 contain the address where
; the RTI instruction jumps.  It's often loaded with the
; address of a program just loaded from tape so the "G"
; command will execute it.
;
                VERIFY  $a048
RTIVEC          ds      2	;As per orig TBUG, length of STACIN+RTIVEC = 7 bytes
;
		org	$A080
;
NAME            ds      11	;Additionsl TBUG storage. 
RUBIG           ds      1	
WAIT            ds      1 	
;
; LCD ram storage
;
lcd_row                ds      1               ;Current lcd row, 0 = top row   
lcd_col                ds      1               ;Current lcd column, 0 = left-most column
lcd_buf                ds      LCDBUFSZ        ;backing buffer for scrolling, etc.
lcd_tempx      ds      2                       ;tenp store for x reg, used to calculate backing store offsets  
lcd_buf_addr   ds      2                       ;used to store buffer address value so it can be split into msb/lsb
lcd_charx      ds      3                       ;temp store for x reg - used to preserve x during lcd_char calls
;
;
;***************************************************
; Common ASCII codes
;
EOT             equ     $04
BEL             equ     $07
BS              equ     $08
LF              equ     $0a
CR              equ     $0d
DEL             equ     $7f
FF              equ     $0c
SPACE		equ	$20
NULL		equ	$0
RUB		equ	$7f
;
;***************************************************
; I/O base address and SWTPC type slot numbers.
; These constants are used by each module that uses I/O
; to calculate actual addresses. This gets a little messy
; in the actual modules, but any I/O address changes should
; only be done today.
;
IOBASE          equ     $8000
IOBYTES         equ     4
;
; Slot used by console ACIA
CONSLOT         equ     0
;
; KB/Paper tape slot PIA used by TBUG
KBSLOT          equ     4
;
;***************************************************
;
; This section should end at A090H
;
; Start of Code!
; Copied from OCR. Opcodes and comments should be lower case.
; Labels should all be kept as upper case to maintain
; compatibility with xtbug code
;
ACIACN 		equ   	IOBASE+(IOBYTES*CONSLOT)
ACIADT 		equ   	IOBASE+(IOBYTES*CONSLOT)+1
PIAAPR 		equ   	IOBASE+(IOBYTES*KBSLOT)
PIAACN 		equ   	IOBASE+(IOBYTES*KBSLOT)+1
PIABPR 		equ   	IOBASE+(IOBYTES*KBSLOT)+2
PIABCN 		equ   	IOBASE+(IOBYTES*KBSLOT)+3
YCIACN 		equ   	IOBYTES*CONSLOT
YCIADT 		equ   	IOBYTES*CONSLOT+1
XIAAPR 		equ   	(IOBYTES*KBSLOT)
XIAACN 		equ   	(IOBYTES*KBSLOT)+1
XIABPR 		equ   	(IOBYTES*KBSLOT)+2
XIABCN 		equ   	(IOBYTES*KBSLOT)+3
;
XLOWX		equ	XLOW-XHI
TWX		equ	TW-XHI
XHIX		equ	XHI-XHI
;
;XISAD		equ	DISAD-DISAD
;KOLPTR		equ	COLPTR-DISAD
;XOWPTR		equ	ROWPTR-DISAD
;KHARST		equ	CHARST-DISAD
;
;
		code
		org	$e000
;
		VERIFY	$e000
;
; Interrupt vectors
;
IRQ 	ldx    	IRV    		;irq vector from scratchpad
      	jmp    	0,x    		;and jump to where it points
NMI 	ldx    	NMV    		;get nmi vector
      	jmp    	0,x    		;and jump
;
; S1 Load 'L'
;
LOAD 	bra	LOAD3	
QWAIT 	tst  	WAIT   		;if wait then loop forever
WTLOOP 	beq   	WTLOOP 		;exit is achieved by reset
      	bra    	C1
LOAD3 	bsr    	INCHBR   		;get char look for 's1' (start
      	cmpa 	#'S'    	;of block) or 's9' end of file
      	bne    	LOAD3  		;not 's so look again
      	bsr    	INCHBR
      	cmpa 	#'9'
      	beq    	QWAIT  		;branch if end of file (s9)
      	cmpa 	#'1'
      	bne    	LOAD3  		;if 's1' or 's9' not found
    	clr    	CKSM   		;'s1' found begin load
      	bsr    	BYTE   		;get byte count
      	suba 	#02
    	staa 	BYTECT
      	bsr    	BADDR  		;get address of block
LOAD11 	bsr   	BYTE  		;get data
    	dec    	BYTECT
      	beq    	LOAD15
      	staa 	0,x
        inx
      	bra    	LOAD11
LOAD15 	inc 	CKSM
      	beq    	LOAD3  		;checksum o.k.
LOAD19 	ldaa 	#'?'   		;error so print question mark
      	bsr    	OUTCHBR
C1  	jmp    	CONTRL
;
	VERIFY	$e047
;
BADDR 	bsr    	BYTE   		;get 1st. byte of address
    	staa 	XHI
      	bsr   	BYTE    	;get 2nd. byte
    	staa 	XLOW
    	ldx   	XHI     	;x=address
        rts
;
; Get hex byte
;
	VERIFY	$e055
;
BYTE 	bsr    	INHEX  		;get 1st. hex char (most sig.)
        asla         		;shift left 4 times
        asla
        asla
        asla
        tab           		;save in b
      	bsr   	INHEX   	;get 2nd. hex char.
        aba           		;add accumulators to form byte
        tab
    	addb 	CKSM   		;add this byte to checksum
    	stab 	CKSM
        rts
;
	VERIFY	$e067
;
OUTHL 	lsra
        lsra
        lsra
        lsra
;
	VERIFY	$e06b
;
OUTHR 	anda 	#$0f
      	adda 	#$30   		;convert hex to ascii
      	cmpa 	#$39
      	bls  	OUTCHBR
      	adda 	#07
OUTCHBR	jmp 	OUTCH		;modified from original TBUG, OUTE->OUTCH to match swtbug/xTBUG
INCHBR 	jmp  	INCH		;..and also for INCH, so INPOLL->INCH, then this label is used for local branches
PDT2 	bsr    	OUTCHBR
        inx
;
; Output text string pointed to by x
;
	VERIFY	$e07e
;
PDATA 	ldaa 	0,x    		;output area of memory as text
      	cmpa 	#04    		;until eot(04) is found
      	bne   	PDT2
        rts
;
; Change memory command 'M'
;
CHANGE 	bsr	BADDR  		;memory inspect/modify command
CHA51 	ldx 	#MCL
      	bsr   	PDATA  		;get address
    	ldx   	#XHI
     	bsr   	OUT4HS 		;print address
    	ldx   	XHI
     	bsr   	OUT2HS 		;print contents of this
    	stx   	XHI    		;location
      	bsr   	INCHBR
      	cmpa 	#$20
      	bne   	CHA51  		;if not sp then try next loc
      	bsr   	BYTE   		;get new data for this loc
        dex
      	staa 	0,x    		;and store
      	cmpa 	0,x    		;did it store?
      	beq   	CHA51  		;if it did then look at next
      	bra   	LOAD19 		;memory fault print '?'
INHEX 	bsr   	INCHBR
      	suba 	#$30   		;convert ascii to hex
      	bmi   	C1     		;- to 'C1' if not hex
     	cmpa 	#$09
      	ble    	HGRET
      	cmpa 	#$11
      	bmi    	C1
      	cmpa 	#$16
      	bgt    	C1
      	suba 	#$07
HGRET 	rts
;
	VERIFY	$e0bf
;
OUT2H 	ldaa 	0,x		;output 2 hex chars
OUT2HA 	bsr   	OUTHL  		;output left hex char.
      	ldaa 	0,x
        inx
      	bra    	OUTHR   	;output right hex char return.
;
	VERIFY	$e0c8
;
OUT4HS 	bsr   	OUT2H   	;output 4 hex + space
;
	VERIFY	$e0ca
;
OUT2HS 	bsr   	OUT2H   	;output 2 hex + space
;
	VERIFY	$e0cc
;
OUTS  	ldaa 	#$20    	;output a space
      	bra    	OUTCHBR
START 	lds  	#STACK  	;init stack pointer. Changed from hard coded value in orig TBUG
    	sts    	SP
      	ldaa 	#03     	;reset acia
    	staa 	ACIACN
        inca
    	staa 	PIAACN  	;init pia(a)
    	jsr    	INIT    	;'init' continues initialising
        nop            		;the system
CONTRL 	lds 	#STACK		;Changed from hard coded value in orig TBUG
    	ldx    	#MCL    	;print asterisk
      	bsr    	PDATA
      	bsr    	INCHBR    	;get command
        tab
      	bsr    	OUTS    	;output a space
    	ldx    	#TABLE
SCAN  	tst    	0,x     	;end of table?
      	beq    	NOCOMM
      	cmpb 	0,x    		;does command match?
      	bne    	INCT    	;if not then branch
      	ldx    	1,x     	;get command address
      	jmp    	0,x     	;and jump to it
INCT  	inx            		;x+1 to point at the next
        inx            		;command char.
        inx
      	bra    	SCAN
;
; Change serial format 'U' command
;
SERFORM	ldaa 	#$11    	;change acia serial format
    	staa 	ACIACN		;to 2 stop (for 110 baud)
      	bra    	CONTRL 		;then return
NOCOMM 	jmp 	LOAD19
        nop
;
; Go 'G' command
;
GO  	lds    	SP		;restore stack pointer
        rti            		;and load internal regs and go
SFE 	sts    	SP		;save stack pointer
        tsx
      	tst    	6,x     	;decrement program counter now
      	bne    	NDEC    	;on stack
        dec    	5,x
NDEC  	dec    	6,x
;
; Register dumo 'R' command
;
PRINT 	ldx    	SP      	;print contents of stack
        inx
        bsr    	OUT2HS
        bsr    	OUT2HS
        bsr    	OUT2HS
        bsr    	OUT4HS
        bsr    	OUT4HS
      	ldx    	#SP
        bsr    	OUT4HS
C2      bra    	CONTRL
;
MTAPE 	db     CR,LF,0,0,0,0,"S1",EOT
;
; Punch 'P' command
;
PUNCH 	jsr    	AUTO    	;- settle auto recording level
NAUTP 	ldx    	BEGA
      	stx    	TW      	;- put start address in temp
PUN11 	ldaa 	ENDA+1
      	suba 	TW+1
      	ldab 	ENDA
      	sbcb 	TW
        bne    	PUN22
        cmpa 	#16
        bcs    	PUN23
PUN22 	ldaa 	#15
PUN23 	adda 	#4
      	staa 	MCONT   	;frame count of this record
        suba 	#3
      	staa 	TEMP    	;byte count of this record
      	ldx    	#MTAPE
      	jsr    	PDATA
	clrb			;clear checksum
      	ldx    	#MCONT
       	bsr    	PUNT2   	;output frame count
      	ldx    	#TW
        bsr    	PUNT2   	;output start address of this
        bsr    	PUNT2
      	ldx    	TW
PUN32 	bsr     PUNT2
      	dec    	TEMP
        bne    	PUN32   	;output a byte
      	stx    	TW
        comb
        pshb
        tsx
        bsr    	PUNT2 		;output checksum
        pulb
    	ldx   	TW
        dex
    	cpx   	ENDA
      	bne   	PUN11
    	jmp   	S9OUT   	;output 's9' and return
PUNT2 	addb 	0,x     	;output 2 hex chars and update
    	jmp   	OUT2H
;
MCL   	db	CR,LF,0,0,'*',EOT
;
; Jump 'J' command
;
JUMP  	bsr   	BADLK   	;get address from user
      	jmp   	0,x    		;and jump to it
SAVX	stx   	XTEMP   	;store x
;
    	ldx   	#IOBASE  	;point x at to IO base
        rts
;
	VERIFY	$e1ac
;
INCH	ldaa 	#$7f    	;input one char from port into, changed to INCH from INPOLL
;
	VERIFY	$e1ae
;
INXBIT 	bsr  	SAVX    	;put '7f' ignore m.s. bit
        pshb         		;save b
POLL1 	;ldab 	XIABCN,x  	;does pia(b) have something - No longer use PIAB
      	;bpl   	POLL2
      	;anda 	XIABPR,x  	;get data from pia(b)
      	;bra   	RET
POLL2 	ldab 	XIAACN,x  	;does pia(a) have something
      	bpl   	POLL3
      	anda 	XIAAPR,x  	;get data from pia(a)
      	bra   	RET		
POLL3 	ldab 	YCIACN,x  	;does acia have something?
        lsrb
      	bcc   	POLL1
      	anda 	YCIADT,x  	;get data from acia
RET 	tst   	RUBIG   	;ignore rubout chars?
      	bne   	DIG
      	cmpa 	#RUB		;7fh
      	bra   	CRQ     	;mess of branches is so that
;
	nop			;SPARE SPACE
	nop
	nop
	nop
	nop
	nop
	nop
	nop
;
; OUTCH routine
;
	VERIFY	$e1d1
;
OUTCH  	bra   	OUTP    	;output routine appears at
CRQ   	beq   	POLL1  		;'e1d1'
DIG   	pulb
    	ldx   	XTEMP
OUTP  	bsr   	SAVX
        pshb
NOTRDY 	ldab 	ACIACN 		;acia ready to transmit?
        lsrb
        lsrb
F9      bcc   	NOTRDY 		;no
     	staa 	ACIADT 		;pass char to acia for transmit
	jsr	lcd_outch	;Call lcd_outch to display on LCD display
	pulb
	ldx   	XTEMP   	;restore x
        rts
BADLK 	jmp 	BADDR
;
; SPARE SPACE
;
		org	$e299
;
AUTO    ldab    #$20            ;output 32 nulls
        clra                    ;to allow auto recording
NULOUT  jsr     OUTCH           ;level to settle
        decb
        bne     NULOUT
        rts
INIT 	ldaa 	#$15    	;init acia-one stop bit and
     	staa 	ACIACN  	;8 bits of data no parity
     	;clr   	RUBIG   	;rubout chars ignored.
;
	lda     #$01            ;set the WAIT bit to always return
        staa    RUBIG           ;don't ignore rubout/7F (orig TBUG clears this)
        staa    WAIT            ;Initialize the WAIT variable (used in LOAD) - fixes bug in orig TBUG   
;
	jsr	piab_init	;Initialize the PIA for the LCD display
	jsr	lcd_init	;Initialize the LCD display
     	ldx   	#TBSTR  	;print 'tbug'
     	jsr   	PDATA
;
	rts
	nop			;SPARE SPACE
	nop			;SPARE SPACE
	nop			;SPARE SPACE
	nop			;SPARE SPACE
	nop			;SPARE SPACE
	nop			;SPARE SPACE
	nop			;SPARE SPACE
	nop			;SPARE SPACE
;
S9OUT 	ldx  	#S9STR  	;-output 's9'
     	jsr   	PDATA
       	bra   	C4
;
; Copy 'Y' command
;
COPY 	bsr     BDSP    	;copy a block of memory-get
     	stx   	NAME    	;start of block address
       	bsr   	BDSP    	;get end address of block
        inx           		;increment it
     	stx   	TW      	;and store
      	bsr   	BDSP    	;get start address of start
     	ldx   	NAME    	;od destination block
MOVEIT 	ldaa 	0,x     	;move move move!
     	ldx   	XHI
       	staa 	0,x
        inx
     	stx   	XHI
     	ldx   	NAME
        inx
     	stx   	NAME
     	cpx   	TW      	;end?
       	bne   	MOVEIT
       	bra   	C4
;
; Block tabulate 'K'
; 
BLKTAB 	bsr   	BDSP    	;tabulate block of 64 bytes
       	ldab 	#$40    	;get address of start of block
     	ldx   	#CRS
     	jsr   	PDATA   	;output carriage return etc.
     	ldx   	XHI
TAB2 	jsr   	OUT2H   	;output 2 hex chars (one byte)
       	bsr   	OUTSLK  	;and 3 spaces
       	bsr   	OUTSLK
       	bsr   	OUTSLK
        decb         		;finished?
       	bne   	TAB2
C4   	jmp   	CONTRL
OUTSLK 	jmp 	OUTS
;
	VERIFY	$e315
;
BDSP 	jsr   	BADDR   	;build address and output
       	bsr   	OUTSLK  	;a space
        rts
OUTLNK 	jmp 	OUTCH
TEXSTR 	ldab 	#$0c     	;input up to ten chars
    	ldx   	#NAME   	;and terminate with eot
NFIN 	decb           		
       	bne   	NERR
     	jmp   	LOAD19  	;too many chars! so exit
NERR 	jsr   	INCH
       	cmpa 	#CR     	;carriage return(end of string
       	beq   	ENDSTR
      	staa 	0,x     	;store char
        inx
      	bra   	NFIN
ENDSTR 	ldaa 	#LF	    	;output line feed
      	bsr   	OUTLNK
      	ldaa 	#$04     	;terminate string
      	staa 	0,x
        rts
;
; Save 'V' command
;
SAVE  	bsr   	TEXSTR
    	jsr   	AUTO
        inca         		;soh (start of header)
      	bsr   	OUTLNK  	;output it 4 times to mark the
      	bsr   	OUTLNK  	;beginning of a header
      	bsr   	OUTLNK
      	bsr   	OUTLNK
    	ldx   	#NAME   	;output name of file
    	jsr   	PDATA
    	jmp   	NAUTP   	;output it in 's1' format
;
; Input names file 'I' command
;
INFILE 	bsr  	TEXSTR  	;get name of file which we are
NOMAT 	ldab 	#$04     	;looking for
N4IN 	bsr    	INCASS  	;accept input from serial
      	cmpa 	#$01     	;port onle (cassette)
      	beq   	HDFN    	;'soh' char found? branch if
      	cmpa 	#'S'     	;so end of file (s9)?
      	bne   	NOMAT   	;if neither of these then look
     	bsr   	INCASS  	;again
      	cmpa 	#'9'     	;is9t
      	bne   	NOMAT   	;branch if not
    	ldx   	#ENDST  	;say that we've found the end
    	jsr   	PDATA   	;one of his files
      	bra   	NOMAT   	;and look again
HDFN 	decb
      	bne   	N4IN    	;4 'soh' chars found no branch
    	ldx   	#NAME   	;point x to name searched for
CHREQ 	jsr 	INCH  	;get char of name comming in
      	cmpa 	0,x     	;do they match?
      	beq   	NAMSM   	;if so then branch
INNAM 	jsr 	INCH  	;dont match but print name
      	cmpa 	#LF     	;of file we've found anyway
      	bne   	INNAM
      	bra   	NOMAT   	;and look again
NAMSM 	inx
      	ldaa 	#$04
      	cmpa 	0,x     	;end of name?
      	bne   	CHREQ   	;branch if not
    	jmp   	LOAD    	;load file
; 
ENDST 	db  	"END"
;
CRS	db	CR,LF,EOT
;
INCASS 	ldaa 	ACIACN 		;input from serial port only
        lsra
      	bcc   	INCASS
    	ldaa 	ACIADT
        rts
;
S9STR 	db	" S9",EOT
;
; Branch calculator 'T' command
;
BCALC 	jsr 	BDSP   		;calculate branch offset-
        inx         		; increment it twice
        inx
    	stx   	TW     		;and store
    	jsr   	BDSP    	;get destination address
    	ldx   	#XHI
      	ldab 	XLOWX,x 	;check for out of range
      	subb 	TWX+1,x
      	ldaa 	XHIX,x
      	sbca 	TWX,x
      	bne   	POS
      	cmpb 	#$7f
      	bls   	OK
ERRR 	jmp  	LOAD19 		;out of range print ?
POS   	inca
      	bne   	ERRR
      	cmpb 	#$7f
      	bls   	ERRR
OK    	stab 	XHIX,x
    	jsr   	OUT2H  		;output offset to user
    	jmp   	CONTRL 		;and return
;
TABLE 	db   	'G'     	;-jump table for command
      	dw   	GO
        db   	'M'     	;-memory inspect/modify
      	dw   	CHANGE
        db   	'R'     	;-print stack
      	dw   	PRINT
        db   	'P'     	;-punch in 's1' format
      	dw   	PUNCH
        db   	'L'     	;-load 's1' format
      	dw   	LOAD
        db   	'V'     	;-file with name
      	dw   	SAVE
        db   	'I'     	;-input named file
      	dw   	INFILE
        db   	'T'     	;-calculate branch offset
      	dw   	BCALC
        db   	'K'     	;-output a block of memory
      	dw   	BLKTAB
        db   	'Y'     	;-copy block of memory
      	dw   	COPY
        db   	'J'     	;-jump to address
      	dw   	JUMP
;
;       db   	'U'     	;-change serial format to 2 st
;      	dw   	SERFORM
	db	'Z'
	dw	$c000		;Mod to replicate SWTBUG ROM command
	db	0
;
TBSTR   db      CR,LF,LF,0,0,"TBUG",EOT
;
;Old vectors went here - now moved to end of larger ROM
;
;	dw	IRQ		;IRQ vector
;	dw	SFE		;SWI vector
;	dw	NMI		;NMI vector
;	dw	START		;Reset vector
;
;###########################################
;
; LCD display driver
;

	include	"lcd/lcd_driver.asm"	;LCD driver code
;
;
;###########################################
;
;Vectors - currently set for a 64K ROM, so lock this at the top
;of the address range with its own org statement

        org     $fff8

	dw	IRQ		;IRQ vector
	dw	SFE		;SWI vector
	dw	NMI		;NMI vector
	dw	START		;Reset vector

;
;END
;

