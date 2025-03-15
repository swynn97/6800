		title "romcopy"
;
; Sits at C000H and provides a menu to copy BASIC images in EPROM to lower RAM.
; Once copy is complete, program is run.

; Version History:
; 1.0	Initial version with SWTPC and ALTAIR BASIC and menu
; 1.1	SWTPC BASIC only
;
                org     $c000
;
; Locations in EPROM, destination address exec start addresses for images
;
SWTPC_SRC_START		equ	$c100
SWTPC_SRC_END		equ	$dfff
DEST_START		equ	$0000
SWTPC_EXEC_START	equ	$0100
;
; xTBUG scratch pad locations / routines used by this module
;
BEGA		equ	$a002
ENDA		equ	$a004
TW		equ	$a00f
PDATA		equ	$e07e
INCH		equ	$e1ac
;
;
;***************************************************
; Common ASCII control codes
;
EOT             equ     $04
BEL             equ     $07
BS              equ     $08
LF              equ     $0a
CR              equ     $0d
DEL             equ     $7f
;
; Copy SWTPC image
;
start           ldx     #swtpc_copymsg		;display copy message
                jsr     PDATA
                ldx     #DEST_START             ;load destination start address
                stx     TW
                ldx     #SWTPC_SRC_START        ;load source start addfress
                stx     BEGA
                ldx	#SWTPC_SRC_END        	;load end address
                stx     ENDA
		bsr	memcopy			;copy image	
                ldx     #crlf                   ;newline
                jsr     PDATA
                jmp     SWTPC_EXEC_START       ;jump to program start
;
; memcopy routine. Destination start is in TW, source start/end are in BEGA/ENDA
;
memcopy		ldx	BEGA
		ldaa	0,x 			;load byte to copy
		inx
		cpx	ENDA
		beq	done
		stx	BEGA
		ldx	TW			;load current destination
		staa	0,x			;copy byte
		inx
		stx	TW
		bra	memcopy
done		rts
;
;
swtpc_copymsg	db      CR,LF,"Loading SWTPC BASIC ver 2.2... ",EOT
crlf		db	CR,LF,EOT
;
