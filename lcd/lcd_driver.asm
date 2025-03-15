		title "lcd_driver"
;=====================================================
;LCD driver for up to 4x40 LCD displays.
;
;NOTE: Current version only supports scrolling on 2x16 displays - see comments
;
;Display is connected to the PIA as folows:
;
;PIA D0 - Enabled display active high
;PIA D1 - Spare
;PIA D2 - R/W
;PIA D3 - E
;PIA D4 - Display D4
;PIA D5 - Display D5
;PIA D6 - Display D6	 
;PIA D7 - Display D7
;
;...display operates in 4-bit mode
;
;IMPORTANT: D0 needs to be high for the lcd display to output characters
;via lcd_outch.
;
;Code is designed to be included (using 'include') with the as02 assembler,
;Include all the equates and storage defined in the next section.
;
;The use this driver, call the following:
;
;piab_init - initializes the PIA
;lcd_init - initializes the LCD display
;
;Note: Driver uses a lot of async delays that assume a 1Mhz CPU clock!
;
;Use lcd_outch to display characters.
;This routine processes CR, LF, BS and FF (clear screen) correctly.
;7F (RUBOUT) and all other ctrl characters are ignored.
;lcd_cursor can be used to set the cursor outside of lcd_outch, a = row, b = col
;
;=====================================================
;
;All these equates must be included in the calling code
;
;Equates
;
;PIABPR         equ     $8012
;PIABCN		equ	$8013
;
; Common ASCII control codes
;
;EOT            equ     $04
;BS             equ     $08
;LF             equ     $0a
;FF		equ	$0c
;CR             equ     $0d
;NULL		equ	$0
;RUB		equ	$7f
;
;SPACE		equ	$20	;space character
;
;LCD constants
;
;LCDWIDTH	equ	16			;16x2 display
;LCDROWS	equ	2
;LCDBUFSZ       equ     LCDWIDTH*LCDROWS        ;size of backing ram for display
;
; RAM area
;
;lcd_row		ds	1		;Current lcd row, 0 = top row	
;lcd_col		ds	1		;Current lcd column, 0 = left-most column
;lcd_buf		ds	LCDBUFSZ	;backing buffer for scrolling, etc.
;lcd_tempx	ds	2			;tenp store for x reg, used to calculate backing store offsets	
;lcd_buf_addr	ds	2			;used to store buffer address value so it can be split into msb/lsb
;lcd_charx	ds	3			;temp store for x reg - used to preserve x during lcd_char calls
;
;=====================================================
;
;####################################
;Driver code
;####################################
;
;Output character in a reg to display
;
;Ignore null, prcesses CR, LF, FF and BS correctly, everyting else displayed
;as a regular character. Display will scroll when last line reached.
;a and x are retained, row and col values are modified to track display state
;
lcd_outch	psha
		ldab	PIABPR		;if D0 of PIA is low, skip outputing characters
		andb	#$01
		beq	outretsb
		cmpa	#NULL		;if null, then do nothing
		beq	outretsb	
		cmpa	#RUB		;ignore rubout chars, so do nothing
		beq	outretsb
		cmpa	#CR		;is it a CR?
		bne	islf		;if not, branch
		ldaa	lcd_row		;for CR, set cursor to col 0
		clrb		
		jsr	lcd_cursor
		bra	outret
;
islf		cmpa	#LF		;is it a LF?	
		bne	isbs		;if not, branch
		ldaa	lcd_row		;iscroll if needed
		cmpa	#LCDROWS-1	;check if we are on the last line
		bne	islf1		;is on last line, just scroll
		ldaa	lcd_col		;get the current col
		jsr	lcd_scroll	;scroll display, a is preserved
		tab			;set back cursor after scroll
		ldaa	#LCDROWS-1
		jsr	lcd_cursor
		bra	outret
;
islf1		inca
		ldab	lcd_col
		jsr	lcd_cursor
		bra	outret
;
isbs		cmpa	#BS		;is it a BS?
		bne	isff		;if not, branch
		ldab	lcd_col		;load current column
		beq	outret		;skip if already at zero column
		decb			;move back one space
		ldaa	lcd_row		;load row
		jsr	lcd_cursor  	;point to previous character
		ldaa	#SPACE		;clear current character
		jsr	lcd_outch
		ldaa	lcd_row		;load cursor position again
		ldab	lcd_col
		decb			;move back one space
		jsr	lcd_cursor
		bra	outret
;
isff		cmpa	#FF		;is it a form feed (clear screen)?
		bne	regchar
		jsr	lcd_clear	;clear screen
outretsb	bra	outret		;label is so a short branch will work above
;
;display a regular character. Wraps text and scrolls display if needed
;
regchar		cmpa	#SPACE		;skip for any control characters (< SP)
		blt	outret
		ldab	lcd_col		;test if at end of row
		cmpb	#LCDWIDTH
		bne	regchar1	
		ldab	lcd_row		;now check if we are on the last row
		cmpb	#LCDROWS-1
		beq	regchar2
		psha			;if not on the last row, send CR,LF then display char
		ldaa	#CR
		jsr	lcd_outch
		ldaa	#LF
		jsr	lcd_outch
		pula
		bra	regchar1		
regchar2	jsr	lcd_scroll	;Last line reached, so scroll
		psha
		ldaa	#LCDROWS-1	;set cursor to left most column
		clrb
		jsr	lcd_cursor
		pula
regchar1	jsr	lcd_char	;display character
		inc	lcd_col		;inc stored col count
outret		pula
		rts	
;
;
;Scroll display up one line. Handles all formats of display (at build time)
;
;Read a line from ther backing store and write to the prvious line, so 1>0, 2>1, etc.,
;then write a line of spaces to the last line.
;Routine preserves a reg
;NOTE: This is hardcode for 2 row displays - see comments
;
;Calculate backing store offset, store in lcd_tempx
;
lcd_scroll	psha
		clrb			;clear offset
		ldaa	#$1		;start at row 1 ***********TEMP, FIX FOR LARGER DISPLAYS
scrlcont	addb	#LCDWIDTH	
		deca
		bne	scrlcont	;done with calculating offset	
;		
		aba			;store final offset in a
		ldx	#lcd_buf	;load start of buffer address into x
		stx	lcd_buf_addr	;store in memory so we can split into msb/lsb
		ldab	lcd_buf_addr+1	;load the lsb of the buffer address into b
		aba			;add offset (which will always be < 4x40, max size of any display)
		staa	lcd_tempx+1	;store lsb in temp mem address		
		ldaa	lcd_buf_addr	;load the msb of the buffer address
		adca	#$0		;add the carry to the msb (this is a 8 + 16 bit add, so msb of offset is always zero)
		staa	lcd_tempx	;store result in temp
		ldx	lcd_tempx	;load full 16 bit address value into x
;	
		clra			;TEMP hard coded as row 0
		clrb
		jsr	lcd_cursor	;point to next row to be copied to
		ldab	#LCDWIDTH	;load col counter with display width
scrlcp		ldaa	0,x		;load current character from backing store
		pshb			;preserve b
		jsr	lcd_char	;write character	
		inc	lcd_col		;increament store col count
		pulb			;get back b
		inx			;point to next char in backing store
		decb	
		bne	scrlcp		;done with copying row?
;
;Clear last line
;
		ldaa	#LCDROWS-1
		clrb
		jsr	lcd_cursor
		ldab	#LCDWIDTH	
		ldaa	#SPACE
scrlcll		pshb
		jsr	lcd_char
		inc	lcd_col		;increment stored col count
		pulb
		decb
		bne	scrlcll
;
		pula
		rts
;
;Init PIAB as 8 bits of output lines
;
piab_init	clra                    ;set up PIA
		staa    PIABCN
		ldaa    #$fe		;set PIA ad D0 input, D1-D7 as output
		staa    PIABPR
		ldaa    #$04
		staa    PIABCN
		clr     PIABPR
		rts
;
;Init lcd display
;Lots off odd timing here. Also, we have to start by sending a sequence three
;30h commands to resync the display into reading 8 bits
;
lcd_init	ldx	#1250		;wait 100ms0
		bsr	ldelay
		ldaa	#$30		;start by sending three 30h commands	
		bsr 	lcd_cmd8
		ldx	#625		;wait 5ms
		bsr	ldelay
		ldaa	#$30
		bsr 	lcd_cmd8
		ldab	#17		;wait 100us
		bsr	sdelay
		ldaa	#$30
		bsr 	lcd_cmd8
		ldab	#17		;wait 100us	
		bsr 	sdelay	
		ldaa	#$20		;Set to 4 bit mode
		bsr 	lcd_cmd8
		ldab	#17
		bsr	sdelay		;wait 100us
;
;Done with 8 bit access, now in 4 bit mode
;
		ldaa	#$28		;set to 2 line 5x7 mode
		jsr 	lcd_cmd
		ldab	#9		;wait 50us
		bsr	sdelay
		ldaa	#$08		;set display off
		jsr 	lcd_cmd
		ldab	#9		;wait 50us
		bsr	sdelay
 		ldaa	#$06		;no shift, cursor increases
		jsr 	lcd_cmd
		ldab	#9		;wait 50us
		bsr	sdelay
		ldaa	#$0e		;display on, cursor on, etc.
		jsr 	lcd_cmd
		ldab	#9		;wait 50us
		bsr	sdelay	
		bsr 	lcd_clear		;clear display, home cursor
		rts
;
;Clear screen, set cursor to home positiom
;
lcd_clear	stx	lcd_charx	;preserve x
		ldaa    #$01            ;clear screen
		jsr     lcd_cmd
		ldab    #$ff            ;delay for 1.53ms after clear screen
		bsr     sdelay
		clr	lcd_row		;reset local copy of cursor position
		clr	lcd_col
		ldx	#lcd_buf	;clear lcd backing buffer (write spaces)
		ldaa	#SPACE		;load a space character
clearlp		staa	0,x		;store in buffer	
		inx
		cpx	#lcd_buf+LCDBUFSZ
		bne	clearlp
		ldx	lcd_charx	;restore x
		rts
;
;Short delay, delay value passed in b
;Delay = (2+4)*b in microseconds
;
sdelay		decb			;decrement b
		bne sdelay
		rts
;
;Long delay, delay valaue passed in x
;Delay = (4+4)*x in microseconds
;
ldelay		dex
		bne ldelay
		rts
;
;Send lcd command when display is in 8 bit mode
;Note: this is only used during init
;
lcd_cmd8	psha			;Save a reg
		anda	#$f0		;remove lower nibble since display is wired as 4 bit
		oraa	#$08		;Set RS = 0, E = 1
		staa	PIABPR		;output PIA
					;placeholder - delay needed if hold time <10ns
		anda	#$F7		;set RS = 0, E = 0
		staa	PIABPR		;leave date as-is but set E = 0
		pula			;restore a reg
		rts	
;
;Send character in a reg to LCD. A contains full 8 bit character.
;Note: a reg is preserved, b is not, x is preserved
;Also store character in backing buffer
;
lcd_char	psha                    ;Save a reg
		stx	lcd_charx	;preserve x
		tab			;send high nible first
		andb	#$f0
		orab	#$0c		;set RS = 1, E = 1
		stab	PIABPR		;send to PIA
					;placeholder - delay needed if hold time <10ns
		andb	#$F7		;set E = 0, RS = 0
		stab	PIABPR		;send to PIA
                lsla                    ;Shift data to upper nibble                             
                lsla
                lsla
                lsla
		oraa    #$0c            ;set RS = 1, E = 1			 
		staa	PIABPR          ;send lower nibble to PIA
					;placeholder - delay needed if hold time <10ns
		anda    #$F7            ;set E = 0
		staa	PIABPR		;send to PIA
;
;Save character to backing store
;
		clrb
		ldaa	lcd_row		;get current row
lcd_addrow	beq	lcd_addcol	;done with row adds
		addb	#LCDWIDTH			
		deca
		bra	lcd_addrow
lcd_addcol	addb	lcd_col		;add the column offset	
		tba			;store final offset in a
		ldx	#lcd_buf	;load start of buffer address into x
		stx	lcd_buf_addr	;store in memory so we can split into msb/lsb
		ldab	lcd_buf_addr+1	;load the lsb of the buffer address into b
		aba			;add offset (which will always be < 4x40, max size of any display)
		staa	lcd_tempx+1	;store lsb in temp mem address		
		ldaa	lcd_buf_addr	;load the msb of the buffer address
		adca	#$0		;add the carry to the msb (this is a 8 + 16 bit add, so msb of offset is always zero)
		staa	lcd_tempx	;store result in temp
;	
		pula			;restore character
		ldx	lcd_tempx	;store char in backing buffer based on calculated index	
		staa	0,x
		ldx	lcd_charx	;restore x
		rts
;
;Send command in a reg to LCD. A contains full 8 bit command.
;Note: a reg is preserved, b is not
;
lcd_cmd		psha                    ;Save a reg
		tab                     ;send high nible first
		andb    #$f0
		orab    #$08            ;set RS = 0, E = 1
		stab    PIABPR          ;send to PIA
					;placeholder - delay needed if hold time <10ns
		andb    #$F3            ;set E = 0, RS = 0
		stab    PIABPR          ;send to PIA
		lsla                    ;Shift data to upper nibble                             
		lsla
		lsla
		lsla
		oraa    #$08            ;set RS = 0, E = 1                       
		staa    PIABPR          ;send lower nibble to PIA
					;placeholder - delay needed if hold time <10ns
		anda    #$F3            ;set E = 0
		staa    PIABPR          ;send to PIA
		pula
		rts
;
;Set cursor position. Row in a, col in b, zero == lowest row/col
;The command to do this is 80h + (row * 40h (so line shifted left 6 times)) + col
;
lcd_cursor	staa	lcd_row		;store local copies of row and col position
		stab	lcd_col	
		lsla			;Multiply row number by 40h
		lsla
		lsla
		lsla
		lsla
		lsla
		ora	#$80		;add to command
		aba			;add col to address
		bsr	lcd_cmd		;send to display
		rts

		
		
