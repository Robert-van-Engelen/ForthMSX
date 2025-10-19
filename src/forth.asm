;-------------------------------------------------------------------------------
;
;         FFFFFFFF   OOOOOO   RRRRRRR  TTTTTTTT  HH    HH
;        FF        OO    OO  RR    RR    TT     HH    HH
;       FF        OO    OO  RR    RR    TT     HH    HH
;      FF        OO    OO  RR    RR    TT     HH    HH
;     FFFFFF    OO    OO  RRRRRRR     TT     HHHHHHHH  mmmmmmm   ssssss  xx   xx
;    FF        OO    OO  RR RR       TT     HH    HH  mm mm mm ss        xx xx
;   FF        OO    OO  RR  RR      TT     HH    HH  mm mm mm  ssssss     x
;  FF        OO    OO  RR   RR     TT     HH    HH  mm mm mm       ss  xx xx
; FF         OOOOOO   RR    RR    TT     HH    HH  mm mm mm  ssssss  xx   xx
;
; ForthMSX - a Forth 2012 standard system for MSX v0.9
;
;   Configurable Forth features
;   Highly optimized Z80 assembly routines
;
;   Forth 2012 standard words:
;     - Core and Core-Ext (complete, built-in)
;     - Double-Numbers & Double-Numbers-Ext (complete, built-in)
;     - Exception & Exception-Ext (complete, built-in)
;     - Facility & Facility-Ext (complete except K-*, requires FACILITY.FTH)
;     - File-Access & File-Access-Ext (complete, requires FILE.FTH)
;     - Programming-Tools & Programming-Tools-Ext (partial, requires TOOLS.FTH)
;     - Floating-Point & Floating-Point-Ext (complete, requires FLOAT.FTH)
;     - String & String-Ext (complate except REPLACE SUBSTITUTE ESCAPE)
;
;   Additional non-standard words:
;     - stack: CLEAR -ROT DUP>R RDROP 2DUP>R 2RDROP
;     - variables: ON OFF
;     - values: +TO
;     - arithmetic: UMAX UMIN 2+ 2- UMD*
;     - strings: CHOP TRIM -TRIM -TRAILING /STRING NEXT-CHAR SDUP
;     - screen: CUR-XY MAX-XY
;     - introspection: CFA=
;     - looping: ?LEAVE K
;     - conversion: >DOUBLE
;     - text input and editing: EDIT
;     - vocabulary: VOCABULARY CURRENT CONTEXT
;     - dictionary: LASTXT HIDE REVEAL L>NAME >NAME NAME> FIND-WORD
;     - files: ANEW CLOSE-FILES
;     - other: OK BYE
;
; Author:
;   Dr. Robert van Engelen, Copyright 2025
;
;-------------------------------------------------------------------------------
;
; BSD 3-Clause License
;
; Copyright (c) 2025, Robert van Engelen
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
;
; 1. Redistributions of source code must retain the above copyright notice, this
;    list of conditions and the following disclaimer.
;
; 2. Redistributions in binary form must reproduce the above copyright notice,
;    this list of conditions and the following disclaimer in the documentation
;    and/or other materials provided with the distribution.
;
; 3. Neither the name of the copyright holder nor the names of its
;    contributors may be used to endorse or promote products derived from
;    this software without specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;
;-------------------------------------------------------------------------------

.title		Forth
.list		(me)
.area		PROGRAM (ABS)
.org		0x8400-7		; subtract 7 header bytes from boot addr

;-------------------------------------------------------------------------------
;
;		FORTH REGISTERS
;
;-------------------------------------------------------------------------------
;
;	A	unassigned, used as temp
;	BC	instruction pointer (IP)
;	DE	top of stack (TOS)
;	HL	unassigned, used as temp
;	SP	parameter stack pointer
;	IX	if RPIX: return stack pointer, else unassigned
;	IY	if JPIY: address of the next routine to jp (iy), else unassigned
;	[rp]	if not RPIX: return stack pointer in RAM
;
;	if AUXR: auxiliary (shadow) registers AF' BC' DE' HL' are used as temps
;
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;
;		FORTH CONFIGURATION
;
;-------------------------------------------------------------------------------

UPHI = 0	; put stacks and buffers up to HIMEM (1) or up to 0xde3f (0)
FAST = 0	; faster with some code duplication (1) or small (0)
AUXR = 1	; use aux registers (1) or save ip in memory also use di/ei (0)
RPIX = 1	; use register ix as rp (1) or use rp in memory (0)
JPIY = 1	; use register iy to jump to the next routine (1) or jp next (0)
SAFE = 1	; check stack under- and overflow (1) or faster loops (0)
SAFR = 0	; RECURSE checks return stack under- and overflow (1) or not (0)
STOP = 1	; check STOP key to break execution (1) or faster word calls (0)
EDIT = 1	; use MSX INLIN for EDIT and ACCEPT (1) or a line editor (0)
REPL = 1	; interpreter with REPL (1) or compiled headless to run MAIN (0)
XTRA = 1	; include additional non-standard words
MATH = 1	; include floating-point single precision MSX MATH-PACK words
IEEE = 0	; include floating-point IEEE 754 single precision words
FUNC = 0	; include floating-point IEEE 754 sqrt, log, exp, trig functions
FCBN = 2	; include MSX-DOS file-access words, FCBN = max open files
PORT = 0	; include additional Z80 I/O port words >PORT PORT> >VDP
DUMP = 0	; include additional words DUMP DB.R UB.R
MAIN = 0	; include main.asm

; SAFE = 0 makes DO-LOOP up to 20% faster in the best case (empty loop body)
; STOP = 0 makes word calls up to 12% faster in the best case (empty word body)
; XTRA = 1 adds the following words:
; - arithmetic: MD* D* D/ DMOD D/MOD UD/MOD UMD/MOD
; - stacks: N>R NR>
; - strings and memory: S\" S\>S
; - keyboard: INKEY KEY-CLEAR

.if 1-REPL	; REPL=0 disables STOP
STOP = 0
.endif

.if MATH	; MATH overrides IEEE and FUNC
IEEE = 0
FUNC = 0
.endif

;-------------------------------------------------------------------------------
;
;		MSX BIOS SYSCALLS
;
;-------------------------------------------------------------------------------

INITXT		.equ 0x006c		; screen 0 text mode initialization
INIT32		.equ 0x006f		; screen 1 text mode initialization
CHSNS		.equ 0x009c		; check keyboard status
CHGET		.equ 0x009f		; returns A with ascii key code
CHPUT		.equ 0x00a2		; put A=char to the console at CSRX,CSRY
INLIN		.equ 0x00b1		; editor, returns hl=BUF, changes regs
BREAKX		.equ 0x00b7		; set cf when <CTRL><STOP> is pressed
POSIT		.equ 0x00c6		; position cursor at row L col H
ERAFNK		.equ 0x00cc		; turn function key display row off
TOTEXT		.equ 0x00d2		; text mode OLDSCR, changes af,bc,de,hl
KILBUF		.equ 0x0156		; clear key buffer

;-------------------------------------------------------------------------------
;
;		MSX SYSTEM POINTERS AND BUFFERS
;
;-------------------------------------------------------------------------------

TIB		.equ 0xf55e		; TIB = MSX BUF INLIN buffer (258b)
PAD		.equ 0xf41f+62		; PAD = MSX KBUF tokenized buffer (318b)

CSRY		.equ 0xf3dc		; cursor Y postiion (row) >= 1
CSRX		.equ 0xf3dd		; cursor X position (column) >= 1
CSRSW		.equ 0xfca9		; CHPUT cursor off (0) or on (nz)
LINL40		.equ 0xf3ae		; screen mode 0 line length
LINL32		.equ 0xf3af		; screen mode 1 line length
LINLEN		.equ 0xf3b0		; current line length (number of columns)
STYLE		.equ 0xfcaa		; cursor style block (0) underline (nz)
OLDKEY		.equ 0xfbda		; last matrix scan state of the keyboard
HIMEM		.equ 0xfc4a		; highest memory location used by BASIC

;-------------------------------------------------------------------------------
;
;		MSX BASIC HOOKS
;
;-------------------------------------------------------------------------------

HERRO		.equ 0xffb1		; error handler

;-------------------------------------------------------------------------------
;
;		BUFFERS AND STACKS
;
;-------------------------------------------------------------------------------

pad_size	.equ 256		; PAD size (do not change)
tib_size	.equ 256		; TIB size (do not change)
fib_size	.equ 256		; FIB size (adjustable >= 256)
b_size		.equ 256		; TMP string buffer size (do not change)
r_size		.equ 256		; return stack size (adjustable)
s_size		.equ 256		; parameter stack size (adjustable)
h_size		.equ 40			; hold space size (adjustable >= 40)

;-------------------------------------------------------------------------------
;
;		WORD CONTROL BITMASKS
;
;-------------------------------------------------------------------------------

length_mask	.equ 0x3f		; word length bitmask and max length
smudge_bit	.equ 6			; smudge bit, should be bit 6
smudge_mask	.equ 1<<smudge_bit	; smudge bitmask
immediate_bit	.equ 7			; immediate bit, must be bit 7
immediate_mask	.equ 1<<immediate_bit	; immediate bitmask

;-------------------------------------------------------------------------------
;
;		FIG FORTH VOCABULARY KLUDGE
;
;-------------------------------------------------------------------------------

fig_kludge	.equ 0x2001|smudge_mask	; a blank name with smudge bit set

;-------------------------------------------------------------------------------
;
;		FORTH SYS MAGIC CONSTANTS
;
;-------------------------------------------------------------------------------

colon_sys	.equ 0xfdef
do_sys		.equ 0xfedf
dest		.equ 0xfdee
orig		.equ 0xfeed

;-------------------------------------------------------------------------------
;
;		ASSEMBLY MACROS
;
;-------------------------------------------------------------------------------

; Compile a code definition header

last_link = 0

.macro		CODE name,label
.if REPL
		link = last_link
		last_link = .
		.nchr len,^|name|
		.dw link
		.db len
		.str ^|name|
.endif
label:
.endm

; Compile an immediate code definition header

.macro		CODE_IMM name,label
.if REPL
		link = last_link
		last_link = .
		.nchr len,^|name|
		.dw link
		.db len|immediate_mask
		.str ^|name|
.endif
label:
.endm

; Compile a colon definition header

.macro		COLON name,label
		CODE ^|name|,label
		call docol
.endm

; Compile an immediate colon definition header

.macro		COLON_IMM name,label
		CODE_IMM ^|name|,label
		call docol
.endm

; Compile a user variable header

.macro		VARIABLE name,label
		CODE ^|name|,label
		call dovar
.endm

; Compile a user constant header

.macro		CONSTANT name,label
		CODE ^|name|,label
		call docon
.endm

; Compile a user double constant header

.macro		TWOCONSTANT name,label
		CODE ^|name|,label
		call dotwocon
.endm

; Compile a user value header

.macro		VALUE name,label
		CODE ^|name|,label
		call doval
.endm

; Compile a user double value header

.macro		TWOVALUE name,label
		CODE ^|name|,label
		call dotwoval
.endm

; Compile load [ip++] -> pq into register pair pq, increment bc by 2, changes a

.macro		LOAD p,q
		ld a,(bc)	;  7	;
		ld q,a		;  4	;
		inc bc		;  6	;
		ld a,(bc)	;  7	;
		ld p,a		;  4	;
		inc bc		;  6(34); [ip++] -> pq
.endm

; Compile the next routine jump to [ip++] -> xt to fetch and execute xt

.macro		EXEC
		LOAD h,l	; 34	; [ip++] -> hl with xt
		jp (hl)		;  4(38); jump to hl
.endm

.macro		JP_NEXT
.if JPIY
		jp (iy)		;  8(46); jump to EXEC next routine
.else
		jp next		; 10(48); jump to EXEC next routine
.endif
.endm

.macro		NEXT
.if FAST
		EXEC		; 38	; jp [ip++]
.else
		JP_NEXT		;  8(46); jump to EXEC next routine
.endif
.endm

; Save and restore bc with ip

.macro		SVBC
		ld (ip),bc	; save bc with ip
.endm

.macro		LDBC
		ld bc,(ip)	; load bc with ip
.endm

.macro		EXXSVBC
.if AUXR
		exx		; save bc with ip by swapping with using auxiliary registers
.else
		SVBC		; save bc with ip
.endif
.endm

.macro		EXXLDBC
.if AUXR
		exx		; save bc with ip by swapping with using auxiliary registers
.else
		LDBC		; load bc with ip
.endif
.endm

; Save and restore ix and iy when these registers used by the interpreter

.macro		SVIXIY
.if RPIX
		ld (rp),ix	; save ix
.endif
.endm

.macro		LDIXIY
.if RPIX
		ld ix,(rp)	; restore ix
.endif
.if JPIY
		ld iy,next	; restore iy for jp (iy)
.endif
.endm

; Disable and enable interrupts before and after to safely use BC' DE' HL' auxiliary registers

.macro		DINT
.if 1-AUXR
		di			; disable interrupts before using auxiliary registers
.endif
.endm

.macro		EINT
.if 1-AUXR
		ei			; enable interrupts after using auxiliary registers
.endif
.endm

; Stack operations

.macro		DROP
		pop de		; 10
.endm

.macro		DUP
		push de		; 11
.endm

.macro		EXCH
		ex de,hl	;  4	; preserve hl
		ex (sp),hl	; 19	; swap TOS <-> 2OS
		ex de,hl	;  4(27); restore hl
.endm

.macro		SWAP
		pop hl		; 10	; pop hl with 2OS
		push de		; 11	; push de with TOS
		ex de,hl	;  4(25); set new TOS to hl with old 2OS
.endm

.macro		OVER
		pop hl		; 10	; pop hl with 2OS
		push hl		; 11	; push hl to keep 2OS
		push de		; 11	; save TOS
		ex de,hl	;  4(36); set new TOS to hl with old 2OS
.endm

.macro		ROT
		pop hl		; 10	; pop hl with 2OS
		ex (sp),hl	; 19	; save hl as new 3OS, set hl to old 3OS
		push de		; 11	; save TOS
		ex de,hl	;  4(44); set new TOS to hl with old 3OS
.endm

.macro		ROTROT
		pop hl		; 10	; pop hl with 2OS
		ex de,hl	;  4	; set de to 2OS, hl to TOS
		ex (sp),hl	; 19	; save hl as new 3OS, set hl to old 3OS
		push hl		; 11(44); save hl as new 2OS
.endm

; Return stack operations

.if RPIX	; use ix as rp

.macro		RDROP
		inc ix		;  6	;
		inc ix		;  6(12);
.endm

.macro		RTOP p,q
		ld q,(ix+0)	; 19	;
		ld p,(ix+1)	; 19(38); load [rp] -> register pair pq
.endm

.macro		RPUSH p,q
		dec ix		; 10	;
		ld (ix+0),p	; 19	;
		dec ix		; 10	;
		ld (ix+0),q	; 19(58); save register pair pq -> [--rp]
.endm

.macro		RPOP p,q
		ld q,(ix+0)	; 19	;
		inc ix		; 10	;
		ld p,(ix+0)	; 19	;
		inc ix		; 10(58); load [rp++] -> register pair pq
.endm

.else		; use [rp] in RAM

.macro		RDROP
		ld hl,(rp)	; 16	; rp -> hl
		inc hl		;  6	;
		inc hl		;  6	;
		ld (rp),hl	; 16(44); rp + 2 -> rp
.endm

.macro		RTOP p,q
		ld hl,(rp)	; 16	; rp -> hl
		ld q,(hl)	;  7	;
		inc hl		;  6	;
		ld p,(hl)	;  7(36); load [rp] -> register pair pq
.endm

.macro		RPUSH p,q
		ld hl,(rp)	; 16	; rp -> hl
		dec hl		;  6	;
		ld (hl),p	;  7	;
		dec hl		;  6	;
		ld (hl),q	;  7	; save register pair pq -> [--rp]
		ld (rp),hl	; 16(58); ip - 2 -> [rp]
.endm

.macro		RPOP p,q
		ld hl,(rp)	; 16	; rp -> hl
		ld q,(hl)	;  7	;
		inc hl		;  6	;
		ld p,(hl)	;  7	;
		inc hl		;  6	;
		ld (rp),hl	; 16(58); load [rp++] -> register pair pq
.endm

.endif

; Compile an inline string literal

.macro		SLIT string
		.nchr len,^|string|
		.dw doslit
		.db len
		.str ^|string|
.endm

;-------------------------------------------------------------------------------
;
;		MSX BLOAD BIN FILE HEADER
;
;-------------------------------------------------------------------------------

		.db 0xfe		; magic byte
		.dw boot		; start address of binary
		.dw end			; end address of binary
		.dw boot		; execution address

;-------------------------------------------------------------------------------
;
;		ENTRY POINT BOOT UP
;
;-------------------------------------------------------------------------------

boot::		; set hook H.ERRO to intercept Math Pack and MSX-DOS errors
		ld a,0xc3		; JP opcode
		ld hl,bios_throw_e	; bios_throw_e -> hl
		ld (HERRO),a		; JP -> HERRO hook
		ld (HERRO+1),hl		; JP bios_throw_e restore ix/iy and throw
		; init console screen
		call ERAFNK		; MSX ERAFNK disable function key row
		call TOTEXT		; want text mode
		ld a,(LINLEN)		;
		ld (maxxy+3),a		; MSX LINLEN -> X of MAX-XY
		; save BASIC stack pointer to return to BASIC with BYE
.if 1-UPHI
		ld (bsp),sp		; save BASIC sp -> [bsp]
.endif
		; initialize interpreter state
		ld hl,(rp0)		; return stack top
		ld (rp),hl		; set [rp0] -> [rp] return stack top
.if UPHI	; ... by putting stacks and buffers immediately below HIMEM
		ld de,-r_size		;
		add hl,de		; [rp0] - r_size -> hl
		ld (sp0),hl		; set hl -> [sp0] parameter stack top
		ld sp,hl		; set [sp0] -> sp parameter stack top
		ex de,hl		; save hl -> de with [sp0]
		ld hl,-1		;
		sbc hl,sp		;
		ld (sp1),hl		; -1 - [sp0] -> [sp1] for overflow check
		ld bc,s_size		;
		add hl,bc		;
		ld (sp2),hl		; -([sp0] - s_size + 1) = [sp1] + s_size
		ld hl,-s_size-2*b_size	;
		add hl,de		; [sp0] - s_size - 2*b_size -> hl
		ld (buf),hl		; hl -> [buf] TMP buffers base
		ld de,-h_size		; reserve hold space above dictionary
		add hl,de		; [buf] - h_size -> hl
		ld (top),hl		; hl -> [top] max dictionary top
.else
		ld sp,SP0		; set SP0 -> sp parameter stack top
.endif
.if RPIX
		ld ix,(rp)		; set rp -> ix top of return stack
.endif
.if JPIY
		ld iy,next		; set next -> iy for jp (iy) in JP_NEXT
.endif
.if REPL
		; display FORTH banner and start REPL
		call docol
		.dw decimal
		.dw page
		    SLIT ^|FORTH|
		.dw type
		.dw unused,dolit,6,udotr
		    SLIT ^| bytes free\r\n|
		.dw type
		.dw repl
.else
.if MAIN
		; headless run MAIN stand-alone application
		call docol
		.dw main		; main.asm defines MAIN
		.dw bye
.else
		; headless do-nothing
		call docol
		    SLIT ^|FORTH headless|
		.dw type
		.dw unused,dolit,6,udotr
		    SLIT ^| bytes free\r\n|
		.dw type
		.dw bye
.endif;MAIN
.endif;REPL

;-------------------------------------------------------------------------------
;
;		INTERPRETER STATE VARIABLES
;
;-------------------------------------------------------------------------------

ip:		.dw 0			; saved bc with ip
rp:		.dw 0			; saved ix with rp

.if UPHI

rp0		.equ HIMEM		; rp0 top of return stack is HIMEM
sp0:		.dw 0			; sp0 top of parameter stack
sp1:		.dw 0			; sp1 = -1 - sp0
sp2:		.dw 0			; sp2 = -(sp0 - s_size + 1) = sp1 + s_size
buf:		.dw 0			; TMP buffers base
top:		.dw 0			; max dictionary top

.else

bsp:		.dw 0			; saved BASIC stack pointer

RP0		.equ 0xde3f		; lowest HIMEM (MSX2 Technical Handbook)
SP0		.equ RP0-r_size		;
SP1		.equ 0xffff-SP0		;
SP2		.equ SP1+s_size		;
BUF		.equ SP0-s_size-2*b_size;
TOP		.equ BUF-h_size		;

rp0:		.dw RP0			; rp0 top of return stack
sp0:		.dw SP0			; sp0 top of parameter stack
sp1:		.dw SP1			; sp1 = -1 - sp0
sp2:		.dw SP2			; sp2 = -(sp0 - s_size + 1) = sp1 + s_size
buf:		.dw BUF			; TMP buffers base
top:		.dw TOP			; max dictionary top

.endif

;-------------------------------------------------------------------------------
;
;		START OF THE DICTIONARY
;
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;
;		INTERNAL RUNTIME WORDS OF COMPILE-ONLY WORDS
;
;-------------------------------------------------------------------------------

; (:)		-- ; R: -- ip
;		call colon definition;
;		runtime of the : compile-only word

		CODE (:),docol
		RPUSH b,c	; 58	; push bc -> [--rp] with caller ip on the return stack
		pop bc		; 10(68); pop ip saved by call docol
cont:		; continue with BREAK check followed by the next routine
.if STOP
		ld a,(OLDKEY+7) ; 13	; check MSX OLDKEY row 7 bit 4 for STOP
		and 0x10	;  7	;
		jr z,break	;  7(27);
.endif
next:		EXEC		; 38	; jp [ip++]
		; MSX STOP break handling
.if STOP
break:		call KILBUF		; MSX KILBUF clear key buffer
		ld a,-28		;
		jp throw_a		; throw -28 "user interrupt (BREAK was pressed)"
.endif

; (;)		-- ; R: ip --
;		return to caller from colon definition;
;		runtime of the ; compile-only word

		CODE ^|(;)|,doret
		RPOP b,c	; 58	; pop [rp++] -> bc with ip of the caller
		NEXT			; continue

; (EXIT)	-- ; R: ip --
;		return to caller from colon definition;
;		runtime of the EXIT compile-only word

		CODE (EXIT),doexit
		jr doret		; same as (;)

.if REPL

; (;DOES)	-- ; R: ip --
;		set LASTXT cfa to ip and return from colon definition;
;		a runtime word compiled by the DOES> compile-only word

		CODE ^|(;DOES)|,dosemicolondoes
		ld hl,(lastxt+3)	; LASTXT -> hl with last defined word xt
		inc hl			;
		ld (hl),c		;
		inc hl			;
		ld (hl),b		; ip -> [LASTXT+1] overwrite call address
		jr doret		; (;) return to caller

; (DOES)	addr -- addr ; R: -- ip
;		calls the DOES> definition with pfa addr;
;		runtime word compiled by the DOES> compile-only word coded as call dodoes

		CODE (DOES),dodoes
		RPUSH b,c	; 58	; push bc -> [--rp] with old ip on the return stack
		pop bc		; 10	; pop bc with new ip of the DOES> routine saved by call dodoes
		pop hl		; 10	; pop pfa addr
		push de		; 11	; save TOS
		ex de,hl	;  4(93); set new TOS to hl with pfa addr
		NEXT			; continue

.endif;REPL

; (VAR)		-- addr
;		leave parameter field address (pfa) of variable;
;		runtime word of a VARIABLE coded as call dovar

		CODE (VAR),dovar
		pop hl		; 10	; pop hl with pfa addr saved by call dovar
		push de		; 11	; save TOS
		ex de,hl	;  4(25); set new TOS to hl with pfa addr
		NEXT			; continue

; (VAL)		-- x
;		fetch value;
;		runtime word of a VALUE coded as call doval

		CODE (VAL),doval
		pop hl		; 10	; pop hl with pfa addr saved by call doval
push_fetch:	push de		; 11	; save TOS
fetch_hl:	ld e,(hl)	;  7	;
		inc hl		;  6	;
		ld d,(hl)	;  7(41); set [hl] -> de as new TOS
		NEXT			; continue

; (2VAL)	-- dx
;		fetch double value;
;		runtime word of a 2VALUE coded as call dotwoval

		CODE (2VAL),dotwoval
		pop hl		; 10	; pop hl with pfa addr saved by call dotwoval
push_twofetch:	push de		; 11	; save TOS
twofetch_hl:	ld e,(hl)	;  7	;
		inc hl		;  6	;
		ld d,(hl)	;  7	; set [hl] -> de with low order value as new TOS
		inc hl		;  6	;
		ld a,(hl)	;  7	;
		inc hl		;  6	;
		ld h,(hl)	;  7	;
		ld l,a		;  4	; set [hl+2] -> hl with high order value as new 2OS
		push hl		; 11(82); save new 2OS
		NEXT		;	; continue

; (CON)		-- x
;		fetch constant;
;		runtime word of a CONSTANT coded as call docon

		CODE (CON),docon
		jr doval		; same as (VAL)

; (2CON)	-- x
;		fetch double constant;
;		runtime word of a 2CONSTANT coded as call dotwocon

		CODE (2CON),dotwocon
		jr dotwoval		; same as (2VAL)

; (DEF)		--
;		execute deferred word;
;		runtime word of a DEFER coded as call dodef

		CODE (DEF),dodef
		pop hl		; 10	; pop hl with pfa addr saved by call dodef
		ld a,(hl)	;  7	;
		inc hl		;  6	;
		ld h,(hl)	;  7	;
		ld l,a		;  4	; [hl] -> hl with execution token
jphl:		jp (hl)		;  4(38); execute the execution token

; (LIT)		-- x
;		fetch literal;
;		runtime word compiled by EVALUATE, INTERPRET and NUMBER

		CODE (LIT),dolit
		push de			; save TOS
		LOAD d,e		;
		NEXT			; continue

; (2LIT)	-- x1 x2
;		fetch double literal;
;		runtime word compiled by EVALUATE, INTERPRET and NUMBER

		CODE (2LIT),dotwolit
		push de			; save TOS
		LOAD d,e		; set [ip++] -> de as new TOS with high order x2
		LOAD h,l		; set [ip++] -> hl as new 2OS with low order x1
		push hl			; save hl as 2OS
		NEXT			; continue

; (SLIT)	-- c-addr u
;		fetch literal string;
;		runtime word compiled by S" and ."

		CODE (SLIT),doslit
		push de			; save TOS
		ld a,(bc)		;
		inc bc			; [ip++] -> a with string length byte
		push bc			; save bc = c-addr as new 2OS
		ld e,a			;
		ld d,0			; set a -> de with u as new TOS
		add c			;
		ld c,a			;
		ld a,d			; 0 -> a
		adc b			;
		ld b,a			; ip + u -> ip
		NEXT			; continue

;-------------------------------------------------------------------------------
;
;		CONSTANTS
;
;-------------------------------------------------------------------------------

; 0		-- 0
;		leave constant 0
;
;    0 CONSTANT 0

		CODE 0,zero
		push de			; save TOS
zero_next:	ld de,0			; set new TOS to 0
		JP_NEXT			; continue

false		.equ zero		; alias
false_next	.equ zero_next		; alias

; 1		-- 1
;		leave constant 1
;
;    1 CONSTANT 1

		CODE 1,one
		push de			; save TOS
one_next:	ld de,1			; set new TOS to 1
		JP_NEXT			; continue

; 2		-- 2
;		leave constant 2
;
;    2 CONSTANT 2

		CODE 2,two
		push de			; save TOS
		ld de,2			; set new TOS to 2
		JP_NEXT			; continue
; 3		-- 3
;		leave constant 3
;
;    3 CONSTANT 3

		CODE 3,three
		push de			; save TOS
		ld de,3			; set new TOS to 3
		JP_NEXT			; continue

; -1		-- -1
;		leave constant -1
;
;    -1 CONSTANT -1

		CODE -1,mone
		push de			; save TOS
mone_next:	ld de,-1		; set new TOS to -1
		JP_NEXT			; continue

true		.equ mone		; alias
true_next	.equ mone_next		; alias

; FALSE		-- 0
;		leave 0
;
;    0 CONSTANT FALSE

		CODE FALSE,false_
		jr zero

; TRUE		-- -1
;		leave -1
;
;    -1 CONSTANT TRUE

		CODE TRUE,true_
		jr mone

; BL		-- 32
;		leave constant 32 (space)
;
;    32 CONSTANT BL

		CODE BL,bl
		push de			; save TOS
		ld de,0x20		; set new TOS to 0x20
		JP_NEXT			; continue

; PAD		-- c-addr
;		leave address of the PAD;
;		the PAD is a free buffer space of 256 bytes not used by Forth

		CODE PAD,pad
		push de			; save TOS
		ld de,PAD		; set new TOS to PAD
		JP_NEXT			; continue

; TIB		-- c-addr u
;		leave c-addr of the terminal input buffer (TIB) and buffer size u;
;		the terminal input buffer of 256 bytes is used by Forth;

		CODE TIB,tib
		push de			; save TOS
		ld de,TIB		; set new 2OS to TIB
		push de			;
		ld de,tib_size		; set new TOS to tib_size
		JP_NEXT			; continue

; TMP		-- c-addr
;		leave address of the next temp string buffer;
;		switches between two string buffers of 256 free bytes each;
;		used by SDUP, WORD and S" to store a string when interpreting

		CODE TMP,tmp
		push de			; save TOS
		ld hl,1$		; 1$ -> hl
		ld a,(hl)		; [1$] -> a with counter 0 or 1
		xor 1			; a ^ 1 -> a
		ld (hl),a		; [1$] ^ 1 -> [1$]
		ld de,(top)		;
		add d			;
		ld d,a			; set [top] + (a << 8) -> de as new TOS
		JP_NEXT			; continue
1$:		.db 1			; previous tmp buffer 0 or 1

;-------------------------------------------------------------------------------
;
;		STACK OPERATIONS
;
;-------------------------------------------------------------------------------

; DROP		x --
;		drop TOS

		CODE DROP,drop
		DROP		; 10	; pop new TOS
		NEXT			; continue

; DUP		x -- x x
;		duplicate TOS

		CODE DUP,dup
		DUP		; 11	; set new TOS
		NEXT			; continue

; ?DUP		x -- x x or 0 -- 0
;		duplicate TOS if nonzero

		CODE ?DUP,qdup
		ld a,e		;  4	;
		or d		;  4	; test de = 0
		jr nz,dup	; 23/7	; if de <> 0 then DUP
		NEXT			; continue

; SWAP		x1 x2 -- x2 x1
;		swap TOS with 2OS

		CODE SWAP,swap
		SWAP		; 25	; TOS <-> 2OS
		NEXT			; continue

; OVER		x1 x2 -- x1 x2 x1
;		copy 2OS over TOS

		CODE OVER,over
		OVER		; 36	;
		NEXT			; continue

; ROT		x1 x2 x3 -- x2 x3 x1
;		rotate cells
;
;    : ROT >R SWAP R> SWAP ;

		CODE ROT,rot
		ROT		; 44	;
		NEXT			; continue

; -ROT		x1 x2 x3 -- x3 x1 x2
;		undo (or back, or left) rotate cells, or ROT twice
;
;    : -ROT ROT ROT ;

		CODE -ROT,mrot
		ROTROT		; 44	;
		NEXT			; continue

; NIP		x1 x2 -- x2
;		nip 2OS
;
;    : NIP SWAP DROP ;

		CODE NIP,nip
		pop hl		; 10	; discard 2OS
		NEXT			; continue

; TUCK		x1 x2 -- x2 x1 x2
;		tuck TOS under 2OS
;
;    : TUCK SWAP OVER ;

		CODE TUCK,tuck
		pop hl		; 10	; pop hl with 2OS
		push de		; 11	; save TOS as new 3OS
		push hl		; 11(32); save hl as new 2OS
		NEXT			; continue

; 2DROP		xd1 xd2 -- xd1
;		drop double TOS
;
;    : 2DROP DROP DROP ;

		CODE 2DROP,twodrop
		pop de		; 10	; discard 2OS
		pop de		; 10(20); pop new TOS
		NEXT			; continue

; 2DUP		xd -- xd xd
;		duplicate double TOS
;
;    : 2DUP OVER OVER ;

		CODE 2DUP,twodup
		pop hl		; 10	; pop hl with 2OS
		push hl		; 11	; keep 2OS as new 4OS
		push de		; 11	; save de as new 3OS
		push hl		; 11(43); save hl as new 2OS
		NEXT			; continue

; 2SWAP		xd1 xd2 -- xd2 xd1
;		swap double TOS with double 2OS
;
;    : 2SWAP ROT >R ROT R> ;
;    : 2SWAP 3 ROLL 3 ROLL ;

		CODE 2SWAP,twoswap
		pop hl		; 10	; pop hl with 2OS
		pop af		; 10	; pop af with 3OS
		ex (sp),hl	;  4	; save hl as new 4OS, hl with new 2OS
		push de		; 11	; save de as new 3OS
		push hl		; 11	; save hl as new 2OS
		push af		; 11	; push af as new TOS
		pop de		; 10(67); set new TOS
		NEXT			; continue

; 2OVER		xd1 xd2 -- xd1 xd2 xd1
;		copy double 2OS over double TOS
;
;    : 2OVER >R >R 2DUP R> R> 2SWAP ;
;    : 2OVER 3 PICK 3 PICK ;

		CODE 2OVER,twoover
		push de			; save TOS
		ld hl,6			;
		add hl,sp		; sp + 6 -> hl
		ld e,(hl)		;
		inc hl			;
		ld d,(hl)		;
		push de			; save [sp+6] -> de new 2OS
		dec hl			;
		dec hl			;
		ld d,(hl)		;
		dec hl			;
		ld e,(hl)		; set [sp+4] -> de as new TOS
		NEXT			; continue

; 2ROT		xd1 xd2 xd3 -- xd2 xd3 xd1
;		rotate double cells
;
;    : 2ROT 5 ROLL 5 ROLL ;

		COLON 2ROT,tworot
		.dw dolit,5,roll,dolit,5,roll
		.dw doret

; PICK		xu ... x0 u -- xu ... x0 xu
;		pick u'th cell from the parameter stack;
;		0 PICK is the same as DUP;
;		1 PICK is the same as OVER
;
;    : PICK 1+ CELLS SP@ + @ ;

		CODE PICK,pick
		ex de,hl		; TOS -> hl
		add hl,hl		; 2 * n -> hl
		add hl,sp		; sp + 2 * n -> hl
		jp fetch_hl		; set [hl] -> de as new TOS and continue

; ROLL		xu x(u+1) ... x1 x0 u -- x(u+1) ... x1 x0 xu
;		roll u cells on the parameter stack,
;		where u < 128 (u is not checked, using u modulo 128 for safety);
;		0 ROLL does nothing;
;		1 ROLL is the same as SWAP;
;		2 ROLL is the same as ROT

		CODE ROLL,roll
		ld a,e			;
		add a			; 2*u -> a
		jp z,drop		; if lower 7 bits of u = 0 then set new TOS and continue
		ld l,a			;
		ld h,0			; 2*u -> hl (only using lower 7 bits of u)
		add hl,sp		; sp + 2*u -> hl
		ld e,(hl)		;
		inc hl			;
		ld d,(hl)		; xu = [sp + 2*u] -> de
		push de			; save xu as new TOS
		ld d,h			;
		ld e,l			; sp + 2*u + 1 -> de
		dec hl			;
		dec hl			; sp + 2*u - 1 -> hl
		push bc			; save bc with ip
		ld c,a			;
		ld b,0			; 2*u -> bc (only using lower 7 bits of u)
		lddr			; repeat [hl--] -> [de--] until --bc = 0
		pop bc			; restore bc with ip
		pop de			; set new TOS to xu
		pop hl			; discard 2OS with duplicate x0
		JP_NEXT			; continue

; DEPTH		-- u
;		parameter stack depth
;
;    : DEPTH SP0@ SP@ - 2- 2/ ;

		CODE DEPTH,depth
		push de			; save TOS
		ld hl,(sp0)		; [sp0] -> hl
		dec hl			;
		scf			; 1 -> cf
		sbc hl,sp		;
		ex de,hl		; set [sp0] - sp - 2 -> de as TOS
		jp twoslash		; 2/ divide TOS by 2

; CLEAR		... --
;		purge parameter stack
;
;    : CLEAR SP0@ SP! ;

		CODE CLEAR,clear
		ld sp,(sp0)		; [sp0] -> sp
		JP_NEXT			; continue

; SP@		-- addr
;		fetch stack pointer, leave addr of the TOS cell (the TOS before SP@)

		CODE SP@,spfetch
		push de			; save TOS
		ld hl,0			;
		add hl,sp		; sp -> hl
		ex de,hl		; set new TOS to hl with sp
		JP_NEXT			; continue

; SP!		addr --
;		store stack pointer

		CODE SP!,spstore
		ex de,hl		; addr -> hl
		ld sp,hl		; addr -> sp
		pop de			; pop new TOS
		JP_NEXT			; continue

; .S		--
;		display parameter stack
;
;    : .S DEPTH 0 ?DO SP0@ I 2+ CELLS - ? LOOP ;

		COLON .S,dots
		.dw depth,zero,doqdo,2$
1$:		.dw   dolit,sp0,fetch,i,twoplus,cells,minus,question
		.dw doloop,1$
2$:		.dw doret

;-------------------------------------------------------------------------------
;
;		RETURN STACK OPERATIONS
;
;-------------------------------------------------------------------------------

; >R		x -- ; R: -- x
;		move TOS to the return stack

		CODE >R,tor
		RPUSH d,e	; 58	;
		pop de		; 10(68); pop new TOS
		NEXT			; continue

; DUP>R		x -- x ; R: -- x
;		duplicate TOS to the return stack, a single word for DUP >R

		CODE DUP>R,duptor
		RPUSH d,e	; 58	;
		NEXT			; continue

; R>		R: x -- ; -- x
;		move cell from the return stack

		CODE R>,rfrom
		push de		; 11	; save TOS
		RPOP d,e	; 58	; set new TOS
		NEXT			; continue

; RDROP		R: x -- ; --
;		drop cell from the return stack, a single word for R> DROP

		CODE RDROP,rdrop
		RDROP		; 12	;
		NEXT			; continue

; R@		R: x -- x ; -- x
;		fetch cell from the return stack

		CODE R@,rfetch
		push de		; 11	;
		RTOP d,e	; 36(47);
		NEXT			; continue

; 2>R		x1 x2 -- ; R: -- x1 x2
;		move double TOS to the return stack, a single word for SWAP >R >R

		CODE 2>R,twotor
		pop hl			;
.if RPIX
		RPUSH h,l		;
.else
		push de			;
		ex de,hl		;
		RPUSH d,e		; note: can't RPUSH h,l
		pop de			;
.endif
		jr tor			; >R

; 2R>		R: x1 x2 -- ; -- x1 x2
;		move double cell from the return stack, a single word for R> R> SWAP

		CODE 2R>,tworfrom
		push de			;
		RPOP d,e		;
.if RPIX
		RPOP h,l		;
		push hl			;
		NEXT			; continue
.else
		push de			;
		RPOP d,e		; note: can't RPOP h,l
		jp swap			; SWAP
.endif

; 2R@		R: x1 x2 -- x1 x2 ; -- x1 x2
;		fetch double cell from the return stack, a single word for R> R@ SWAP DUP R>

		CODE 2R@,tworfetch
.if RPIX
		push ix			;
		pop hl			; rp -> hl
.else
		ld hl,(rp)		; rp -> hl
.endif
		jp push_twofetch	; 2@

.if XTRA|FCBN

;+ N>R		n*x n -- ; R: -- n*x n
;		move n cells to the return stack;
;		where n < 127 (u is not checked, using n modulo 128 for safety);
;		no stack under/overflow checking

		CODE N>R,ntor
		push de			; save TOS
		ld a,e			;
		inc a			;
		add a			;
		EXXSVBC			; exx or save bc with ip
		ld c,a			;
		xor a			; 0 -> cf and 0 -> a
		ld b,a			; 2 * (n + 1) -> bc
.if RPIX
		push ix			;
		pop hl			; rp -> hl
		sbc hl,bc		; rp - bc -> hl
		push hl			;
		pop ix			; rp - bc -> rp
.else
		ld hl,(rp)		;
		sbc hl,bc		; rp - bc -> hl
		ld (rp),hl		; rp - bc -> rp
.endif
		ex de,hl		; rp - bc -> de
		ld l,a			;
		ld h,a			; 0 -> hl
		add hl,sp		; sp -> hl
		ldir			; [hl++] -> [de++] until --bc = 0
		ld sp,hl		; hl -> sp
		EXXLDBC			; exx or restore bc with ip
		pop de			; pop new TOS
		JP_NEXT			; continue

;+ NR>		R: n*x n -- ; -- n*x n
;		move n cells from the return stack;
;		where n < 127 (u is not checked, using n modulo 128 for safety);
;		no stack under/overflow checking

		CODE NR>,nrfrom
		push de			; save TOS
		EXXSVBC			; exx or save bc with ip
.if RPIX
		push ix			;
		pop de			; rp -> de
		ld a,(de)		; [rp] -> a
		inc a			;
		add a			;
		ld c,a			;
		ld b,0			; 2 * ([rp] + 1) -> bc
		ld l,b			;
		ld h,b			;
		add hl,sp		; sp -> hl
		sbc hl,bc		;
		ld sp,hl		; sp - bc -> sp
		ex de,hl		; rp -> hl, sp -> de
		ldir			; [hl++] -> [de++] until --bc = 0
		push hl			;
		pop ix			; hl -> rp
.else
		ld de,(rp)		; rp -> de
		ld a,(de)		; [rp] -> a
		inc a			;
		add a			;
		ld c,a			;
		ld b,0			; 2 * ([rp] + 1) -> bc
		ld l,b			;
		ld h,b			;
		add hl,sp		;
		sbc hl,bc		;
		ld sp,hl		; sp - bc -> sp
		ex de,hl		; rp -> hl, sp -> de
		ldir			; [hl++] -> [de++] until --bc = 0
		ld (rp),hl		; hl -> rp
.endif
		EXXLDBC			; exx or restore bc with ip
		pop de			; pop new TOS
		JP_NEXT			; continue

.endif;XTRA|FCBN

; RP@		-- addr
;		fetch return stack pointer

		CODE RP@,rpfetch
		push de			; save TOS
.if RPIX
		push ix			;
		pop de			; set rp -> de new TOS
.else
		ld de,(rp)		;
.endif
		JP_NEXT			; continue

; RP!		addr --
;		store return stack pointer

		CODE RP!,rpstore
.if RPIX
		push de			; save TOS
		pop ix			; TOS de -> rp
		pop de			; pop new TOS
		JP_NEXT			; continue
.else
		ld hl,rp		;
		jr store_hl		; set de -> rp pop new TOS and continue
.endif

.if SAFR

; ?RP		--
;		check return stack pointer for under- and overflow,
;		available only when assembled with the SAVR assembly flag;
;		may throw -5 "return stack overflow" or -6 "return stack underflow"

		CODE ?RP,qrp
		push de		; 10	; save TOS
.if UPHI
		ld de,(rp0)	; 16	; [rp0] -> de
.else
		ld de,RP0	; 10	; RP0 -> de
.endif
.if RPIX
		push ix		; 15	;
		pop hl		; 10	; rp -> hl
.else
		ld hl,(rp)	; 16	; rp -> hl
.endif
		xor a		;  4	; 0 -> a, 0 -> cf
		sbc hl,de	; 15	; rp - [rp0] -> hl
		jr nc,1$	;  7	; if rp >= [rp0] then throw -6
		ld de,r_size	; 10	;
		add hl,de	; 11	; if rp - [rp0] + r_size < 0 then throw -5
		pop de		; 10	; pop TOS
		jp c,next	; 10	; continue
		inc a			; throw -5 "return stack overflow"
1$:		sub 6			; throw -6 "return stack underflow"
		jp throw_a		;

.endif;SAFR

;-------------------------------------------------------------------------------
;
;		FETCH AND STORE
;
;-------------------------------------------------------------------------------

; C@		c-addr -- char
;		fetch char

		CODE C@,cfetch
		ld a,(de)	; 7	; [de] -> a
a_next:		ld e,a		; 4	;
		ld d,0		; 7(18)	; a -> de
		NEXT			; continue

; @		addr -- x
;		fetch from cell

		CODE @,fetch
		ex de,hl	;  4	; addr -> hl
.if FAST
		ld e,(hl)	;  7	;
		inc hl		;  6	;
		ld d,(hl)	;  7(24); set [hl] -> de as new TOS
		NEXT			; continue
.else
		jp fetch_hl		; [hl] -> TOS and continue
.endif

; 2@		addr -- x1 x2
;		fetch from double cell
;
;    : 2@ DUP CELL+ @ SWAP @ ;

		CODE 2@,twofetch
		ex de,hl	;  4	; addr -> hl
.if FAST
		ld e,(hl)	;  7	;
		inc hl		;  6	;
		ld d,(hl)	;  7	; set [hl] -> de with high order x2 as new TOS
		inc hl		;  6	;
		ld a,(hl)	;  7	;
		inc hl		;  6	;
		ld h,(hl)	;  7	;
		ld l,a		;  4	; set [hl+2] -> hl with low order x1 as new 2OS
		push hl		; 11(82); save new 2OS
		NEXT		;	; continue
.else
		jp twofetch_hl		; [hl+2] -> 2OS [hl] -> TOS and continue
.endif

; C!		char c-addr --
;		store char in c-addr

		CODE C!,cstore
		pop hl		; 10	; pop addr -> hl
		ex de,hl	;  4	; char -> de, c-addr -> hl
		ld (hl),e	;  7	; set e -> [hl] with char
		pop de		; 10(31); pop new TOS
		NEXT			; continue

; !		x addr --
;		store in cell

		CODE !,store
		pop hl		; 10	; pop addr -> hl
		ex de,hl	;  4	; x -> de, addr -> hl
store_hl:	ld (hl),e	;  7	;
		inc hl		;  6	;
		ld (hl),d	;  7	; de -> [hl] with x
		pop de		; 10(44); pop new TOS
		NEXT			; continue

; 2!		x1 x2 addr --
;		store in double cell
;
;    : 2! TUCK ! CELL+ ! ;

		CODE 2!,twostore
		pop hl			; pop x2 -> hl
		ex de,hl		; x2 -> de, addr -> hl
twostore_hl:	ld (hl),e		;
		inc hl			;
		ld (hl),d		;
		inc hl			; de -> [hl++] with x2
		pop de			; pop de with x1
		jr store_hl		; de -> [hl] with x1 pop new TOS and continue

; (TO)		x --
;		store in value;
;		runtime of the TO compile-only word

		CODE (TO),doto
		LOAD h,l		; [ip++] -> hl
		jr store_hl		; de -> [hl] with x pop new TOS and continue

; (2TO)		dx --
;		store in double value;
;		runtime of the TO compile-only word

		CODE (2TO),dotwoto
		LOAD h,l		; [ip++] -> hl
		jr twostore_hl		; de -> [hl], 2OS -> [hl+2] pop new TOS and continue

; +!		n addr --
;		increment cell

		CODE +!,plusstore
		pop hl			; pop n -> hl
		ex de,hl		; n -> de, addr -> hl
plusstore_hl:	or a			; 0 -> cf
plusstore_cf_hl:ld a,(hl)		;
		adc e			;
		ld (hl),a		;
		inc hl			;
		ld a,(hl)		;
		adc d			;
		ld (hl),a		; [hl] + n -> [hl]
		pop de			; pop new TOS
		NEXT			; continue

; D+!		d addr --
;		increment double cell

		CODE D+!,dplusstore
		pop hl			; pop 2OS -> hl with high order d
		ex (sp),hl		; 2OS <-> 3OS swaps low/high order of d
dplusstore_de:	ex de,hl		; 3OS -> de with low order of d, addr -> hl
		push hl			;
		inc hl			;
		inc hl			; addr+2 -> hl
		ld a,(hl)		;
		add e			;
		ld (hl),a		;
		inc hl			;
		ld a,(hl)		;
		adc d			;
		ld (hl),a		;
		pop hl			; addr -> hl
		pop de			; 2OS -> de with high order of d
		jr plusstore_cf_hl

; (+TO)		n --
;		increment value;
;		runtime of the +TO compile-only word

		CODE (+TO),doplusto
		LOAD h,l		; [ip++] -> hl
		jr plusstore_hl		; set [hl] + n -> [hl] pop new TOS and continue

; (D+TO)	d --
;		increment double value;
;		runtime of the +TO compile-only word

		CODE (D+TO),dodplusto
		pop hl			; 2OS -> hl with low order of d
		push de			; TOS -> 2OS swaps low/high order of d
		LOAD d,e		; [ip++] -> de
		jr dplusstore_de	; set [de] + d -> [de] pop new TOS and continue

; ON		addr --
;		store TRUE (-1) in cell
;
;    : ON -1 SWAP ! ;

		CODE ON,on
		ld a,-1			; -1 -> a
store_a_de:	ex de,hl		; addr -> hl
store_a_hl:	ld (hl),a		;
		inc hl			;
		ld (hl),a		; a -> [hl]
		pop de			; pop new TOS
		NEXT			; continue

; OFF		addr --
;		store FALSE (0) in cell
;
;    : OFF 0 SWAP ! ;

		CODE OFF,off
		xor a			; 0 -> a
		jr store_a_de		; 0 -> [addr] pop new TOS and continue

;-------------------------------------------------------------------------------
;
;		ARITHMETIC AND LOGIC OPERATIONS
;
;-------------------------------------------------------------------------------

; +		n1 n2 -- n3
;		sum n1+n2

		CODE +,plus
		pop hl			; pop n1 -> hl
		add hl,de		; x1 + x2 -> hl
		ex de,hl		; set new TOS to hl
		NEXT			; continue

; M+		d1 n -- d2
;		double sum d1+n

		CODE M+,mplus
		pop hl			; pop hl with high order of d1
		ex (sp),hl		; exchange hl low/high order of d1
		add hl,de		; hl + n -> de
		pop de			; pop de with high order d2
		push hl			; save hl with low order d2
		jr nc,1$		; if cf = 1 then
		inc de			;   de++ increment high order of d2
1$:		NEXT			; continue

; D+		d1 d2 -- d3
;		double sum d1+d2
;
;    : D+ >R M+ R> + ;

		COLON D+,dplus
		.dw tor,mplus,rfrom,plus
		.dw doret

; -		n1 n2 -- n3
;		difference n1-n2

		CODE -,minus
		pop hl			; pop n1 -> hl
		or a			; 0 -> cf
		sbc hl,de		; set n1 - n2 -> de as new TOS
		ex de,hl		; set new TOS to hl
		NEXT			; continue

; D-		d1 d2 -- d3
;		double difference d1-d2
;
;    : D- DNEGATE D+ ;

		COLON D-,dminus
		.dw dnegate,dplus
		.dw doret

; UM*		u1 u2 -- ud
;		unsigned double product u1*u2

		CODE UM*,umstar
		SVBC			; save bc with ip
		pop bc			; pop u1 -> bc
		xor a			; 0 -> cf and 0 -> a
		ld l,a			;
		ld h,a			; 0 -> hl
		ld a,17			; 17 -> a loop counter
1$:		rr h		;  8	; loop
		rr l		;  8	;
		rr b		;  8	;
		rr c		;  8	;   (cf + hlbc) >> 1 -> hlbc
		jr nc,2$	;  7	;   if cf = 1 then
		add hl,de	; 11	;     hl + de -> hl
2$:		dec a		;  4	;
		jp nz,1$	; 10(64); until --a = 0
		push bc			; save bc with low order ud as 2OS
		ex de,hl		; save hl -> de with high order ud as TOS
		LDBC			; restore bc with ip
		JP_NEXT			; continue

; M*		n1 n2 -- d
;		signed double product n1*n2
;
;    : M*
;      2DUP XOR >R
;      ABS SWAP ABS
;      UM*
;      R> 0< IF DNEGATE THEN ;

		COLON M*,mstar
		.dw twodup,xor,tor
		.dw abs,swap,abs
		.dw umstar
		.dw rfrom,zeroless,doif,1$
		.dw   dnegate
1$:		.dw doret

; *		n1|u1 n2|u2 -- n3|u3
;		signed and unsigned product n1*n2
;
;    : * UM* DROP ;

.if FAST

;		new and faster Z80 method for signed 16x16->16 bit multiplication:
;		at most 51 or 33 cy/bit compared to Zilog's published 62 cy/bit,
;		faster than other Z80 methods published in online resources,
;		unless someone can point me to a better method...
;
;		note: because of performance assymetry, <16-bit> <8-bit> *
;		runs almost twice as fast as <8-bit> <16-bit> *

		CODE *,star
		SVBC			; save bc with ip
		pop bc			; pop n1 -> bc
		ld hl,0			; 0 -> hl
		ld a,c			; c -> a low order byte of n1
		ld c,b			; b -> c save high order byte of n1
		ld b,8			; 8 -> b loop counter
1$:		rra		;  4	; loop, a >> 1 -> a set cf
		jr nc,2$	;  7	;   if cf = 1 then
		add hl,de	; 11	;     hl + de -> hl
2$:		sla e		;  8	;
		rl d		;  8	;   de << 1 -> de
		djnz 1$		; 13(51); until --b = 0
		ld a,h			; h -> a do high order, low order is done
		jr 5$			; jump to shift c and loop
3$:		add d		;  4	; loop, a + d -> d
4$:		sla d		;  8	;   d << 1 -> d
5$:		srl c		;  8	;   c >> 1 -> c set cf and z if no bits left
		jr c,3$		; 12/7(32); until cf = 0 repeat with addition
		jp nz,4$	;   10(33); until c = 0 repeat without addition
		ld h,a			; a -> h
		ex de,hl		; save hl -> de with product as TOS
		LDBC			; restore bc with ip
		JP_NEXT			; continue

.else

		COLON *,star
		.dw umstar,drop
		.dw doret

.endif

; UMD*		ud1 u -- ud2
;		unsigned double product ud1*u
;
;    : UMD*
;      DUP>R
;      UM* DROP SWAP
;      R> UM* ROT + ;

		COLON UMD*,umdstar
		.dw duptor
		.dw umstar,drop,swap
		.dw rfrom,umstar,rot,plus
		.dw doret

; UM/MOD	ud u1 -- u2 u3
;		unsigned remainder and quotient ud/u1;
;		the result is undefined when u1 = 0

		CODE UM/MOD,umslashmod
		SVBC			; save bc with ip
		pop hl			; pop ud -> hl high order dividend
		pop bc			; pop ud -> bc low order dividend
		xor a			;
		sub e			;
		ld e,a			;
		sbc a			;
		sub d			;
		ld d,a			; -de -> de with -u1
		ld a,b			; b -> a low order dividend in ac
		ld b,16			; 16 -> b loop counter
		sla c			;
		rla			; ac << 1 -> ac
1$:		adc hl,hl	; 15	; loop, hl << 1 + cf -> hl
		jr c,3$		;  7/12	;   if cf = 1 then hl + -u1 -> hl, 1 -> cf else
		add hl,de	; 11	;     hl + -u1 -> hl
		jr c,2$		; 12/ 7 ;     if cf = 0 then
		sbc hl,de	;    15	;       hl - -u1 -> hl to undo, no carry
2$:		rl c		;  8	;
		rla		;  4	;   ac << 1 + cf -> ac
		djnz 1$		; 13(80); until --b = 0
		ld b,a			; a -> b quotient bc
		push hl			; save hl with u2 remainder as 2OS
		push bc			; save bc with u3 quotient as TOS
		LDBC			; restore bc with ip
		pop de			; pop new TOS with quotient
		JP_NEXT			; continue
3$:		add hl,de	;    11	; hl + -u1 -> hl
		scf		;     4	; 1 -> cf
		jr 2$		;    12	;

; SM/REM	d1 n1 -- n2 n3
;		symmetric remainder and quotient d1/n1 rounded towards zero;
;		the result is undefined when n1 = 0
;
;    : SM/REM
;      2DUP XOR >R
;      OVER >R
;      ABS -ROT DABS ROT
;      UM/MOD
;      R> 0< IF SWAP NEGATE SWAP THEN
;      R> 0< IF NEGATE THEN ;

		COLON SM/REM,smslashrem
		.dw twodup,xor,tor
		.dw over,tor
		.dw abs,mrot,dabs,rot
		.dw umslashmod
		.dw rfrom,zeroless,doif,1$
		.dw   swap,negate,swap
1$:		.dw rfrom,zeroless,doif,2$
		.dw   negate
2$:		.dw doret

; FM/MOD	d1 n1 -- n2 n3
;		floored signed modulus and quotient d1/n1 rounded towards negative (floored);
;		the result is undefined when n1 = 0
;
;    : FM/MOD
;      DUP>R
;      SM/REM
;      DUP 0< IF
;        SWAP R> + SWAP 1-
;      ELSE
;        RDROP
;      THEN ;

		COLON FM/MOD,fmslashmod
		.dw duptor
		.dw smslashrem
		.dw dup,zeroless,doif,1$
		.dw   swap,rfrom,plus,swap,oneminus
		.dw doahead,2$
1$:		.dw   rdrop
2$:		.dw doret

; /MOD		n1 n2 -- n3 n4
;		symmetric remainder and quotient n1/n2;
;		the result is undefined when n2 = 0
;
;    : /MOD SWAP S>D ROT SM/REM ;

		COLON /MOD,slashmod
		.dw swap,stod,rot
		.dw smslashrem
		.dw doret

; MOD		n1 n2 -- n3
;		symmetric remainder of n1/n2;
;		the result is undefined when n2 = 0
;
;    : MOD /MOD DROP ;

		COLON MOD,mod
		.dw slashmod,drop
		.dw doret

; /		n1 n2 -- n3
;		quotient n1/n2;
;		the result is undefined when n2 = 0
;
;    : / /MOD NIP ;

		COLON /,slash
		.dw slashmod,nip
		.dw doret

; */MOD		n1 n2 n3 -- n4 n5
;		product with symmetric remainder and quotient n1*n2/n3;
;		the result is undefined when n3 = 0
;
;    : */MOD -ROT M* ROT SM/REM ;

		COLON */MOD,starslashmod
		.dw mrot,mstar,rot
		.dw smslashrem
		.dw doret

; */		n1 n2 n3 -- n4
;		product with quotient n1*n2/n3;
;		the result is undefined when n3 = 0
;
;    : */ */MOD NIP ;

		COLON */,starslash
		.dw starslashmod,nip
		.dw doret

; UM*/MOD	ud1 u1 u2 -- u3 ud2
;		unsigned double product and quotient ud1*u1/u2 with single remainder u3,
;		with intermediate triple-cell product;
;		the result is undefined when u2 = 0
;
;    \ assume d = dh.dl     = hi.lo parts
;    \ assume t = th.tm.tl  = hi.mid.lo parts
;    \ then
;    \ dl*n -> tm.tl
;    \ dh*n+tm -> th.tm
;    \ gives d*n -> t
;    : UMT*     ( ud u -- ut )
;      DUP>R
;      ROT UM*
;      ROT 0 SWAP R> UM* D+ ;
;    \ assume d = dh.dl     = hi.lo parts
;    \ assume t = th.tm.tl  = hi.mid.lo parts
;    \ then
;    \ (th.tm)/n -> dh
;    \ (th.tm)%n -> r
;    \ (r.tl)/n -> dl
;    \ (r.tl)%n -> r
;    \ gives t/n -> d remainder r
;    : UMT/MOD  ( ut u1 -- u2 ud )
;      DUP>R
;      UM/MOD R> SWAP >R
;      UM/MOD R> ;
;    : UM*/MOD >R UMT* R> UMT/MOD ;

		COLON UM*/MOD,umstarslashmod
		.dw tor
		.dw duptor,rot,umstar,rot,zero,swap,rfrom,umstar,dplus
		.dw rfrom
		.dw duptor,umslashmod,rfrom,swap,tor,umslashmod,rfrom
		.dw doret

; M*/		d1 n1 n2 -- d2
;		double product with quotient d1*n1/n2,
;		with intermediate triple-cell product;
;		the result is undefined when n2 = 0
;
;    : M*/
;      2DUP XOR 3 PICK XOR >R
;      ABS SWAP ABS SWAP 2SWAP DABS 2SWAP
;      UM*/MOD DROP
;      >R 0< IF DNEGATE THEN ;

		COLON M*/,mstarslash
		.dw twodup,xor,three,pick,xor,tor
		.dw abs,swap,abs,swap,twoswap,dabs,twoswap
		.dw umstarslashmod,rot,drop
		.dw rfrom,zeroless,doif,1$
		.dw   dnegate
1$:		.dw doret

.if XTRA

;+ MD*		d1 n -- d2
;		signed double product d1*n with a single
;
;    : MD*
;      2DUP XOR >R
;      ABS -ROT DABS ROT
;      UMD*
;      R> 0< IF DNEGATE THEN ;

		COLON MD*,mdstar
		.dw twodup,xor,tor
		.dw abs,mrot,dabs,rot
		.dw umdstar
		.dw rfrom,zeroless,doif,1$
		.dw   dnegate
1$:		.dw doret

;+ D*		d1|ud1 d2|ud2 -- d3|ud3
;		signed and unsigned double product d1*d2;
;		the result overflows when d1 and d2 are too large;
;		use MD* for signed double product d*n with a single;
;		use UMD* for unsigned double product ud*u with a single;
;
;    : D* >R ROT DUP>R -ROT MD* 2R> * 0 SWAP D+ ;

		COLON D*,dstar
		.dw tor
		.dw rot,duptor,mrot
		.dw mdstar
		.dw tworfrom,star
		.dw zero,swap,dplus
		.dw doret

;+ UMD/MOD	ud1 u1 -- u2 ud2
;		unsigned remainder and unsigned double quotient ud1/u1;
;		the result is undefined when u1 = 0;
;
;    : UMD/MOD DUP>R 0 SWAP UM/MOD -ROT R> UM/MOD ROT ;

		COLON UMD/MOD,umdslashmod
		.dw duptor
		.dw zero,swap,umslashmod
		.dw mrot
		.dw rfrom,umslashmod
		.dw rot
		.dw doret

;+ UD/MOD	ud1 ud2 -- ud3 ud4
;		unsigned double remainder and quotient ud1/ud2;
;		the result is undefined when ud2 = 0

		CODE UD/MOD,udslashmod
		DINT			; disable interrupts before using auxiliary registers
		exx			; exchange TOS -> de' with high order divisor, ip in bc'
		pop de			; pop 2OS -> de with low order divisor
		pop bc			; pop 3OS -> bc with high order dividend
		exx			;
		pop hl			; pop 4OS -> hl' with low order dividend
		push bc			; save bc' with ip
		ld b,h			;
		ld c,l			; hl' -> bc' with low order dividend
		xor a			;
		ld h,a			;
		ld l,a			; 0 -> hl'
		rl c			;
		rl b			;
		exx			;
		ld h,a			;
		ld l,a			; 0 -> hl
		ld a,b			; b -> a
		rl c			;
		rla			; ac << 1 -> ac
		ld b,32			; 32 -> b loop counter
1$:		adc hl,hl	; 15	;
		exx		;  4	;
		adc hl,hl	; 15	;
		exx		;  4	;   hl'.hl << 1 + cf -> hl'.hl no carry
		sbc hl,de	; 15	;
		exx		;  4	;
		sbc hl,de	; 15	;   hl'.hl - de'.de -> hl'.hl
		jr nc,2$	; 12/ 7	;   if cf = 1 then
		exx		;     4	;
		add hl,de	;    11	;
		exx		;     4	;
		adc hl,de	;    15	;     hl'.hl + de'.de -> hl'.hl to undo, sets carry
2$:		ccf		;  4	;   complement cf
		rl c		;  8	;
		rl b		;  8	;
		exx		;  4	;
		rl c		;  8	;
		rla		;  4	;   ac.bc' << 1 + cf
		djnz 1$		; 13(162); until --b = 0
		ld d,a			;
		ld e,c			; save ac -> de with high order quotient as new TOS
		pop bc			; restore bc with ip
		push hl			; save hl with low order quotient as new 4OS
		exx			;
		push hl			; save hl' with high order remainder as new 3OS
		push bc			; save bc' with low order quotient as new 2OS
		exx			;
		EINT			; enable interrupts after using auxiliary registers
		JP_NEXT			; continue

;+ D/MOD	d1 d2 -- d3 d4
;		double symmetric remainder and quotient d1/d2;
;		the result is undefined when d2 = 0
;
;    : D/MOD
;      DUP 3 PICK DUP>R XOR >R
;      DABS 2SWAP DABS 2SWAP
;      UD/MOD
;      R> 0< IF DNEGATE THEN
;      R> 0< IF 2SWAP DNEGATE 2SWAP THEN ;

		COLON D/MOD,dslashmod
		.dw dup,three,pick,duptor,xor,tor
		.dw dabs,twoswap,dabs,twoswap
		.dw udslashmod
		.dw rfrom,zeroless,doif,1$
		.dw   dnegate
1$:		.dw rfrom,zeroless,doif,2$
		.dw   twoswap,dnegate,twoswap
2$:		.dw doret

;+ DMOD		d1 d2 -- d3
;		double symmetric remainder of d1/d2;
;		the result is undefined when d2 = 0
;
;    : DMOD D/MOD 2DROP ;

		COLON DMOD,dmod
		.dw dslashmod,twodrop
		.dw doret

;+ D/		d1 d2 -- d3
;		double quotient d1/d2;
;		the result is undefined when d2 = 0
;
;    : D/ D/MOD 2SWAP 2DROP ;

		COLON D/,dslash
		.dw dslashmod,twoswap,twodrop
		.dw doret

.endif;XTRA

; AND		x1 x2 -- x1&x2
;		bitwise and x1 with x2

		CODE AND,and
		pop hl			; pop x1 -> hl
		ld a,e			;
		and l			;
		ld e,a			;
		ld a,d			;
		and h			;
		ld d,a			; set hl & de -> de as new TOS
		NEXT			; continue

; OR		x1 x2 -- x1|x2
;		bitwise or x1 with x2

		CODE OR,or
		pop hl			; pop x1 -> hl
		ld a,e			;
		or l			;
		ld e,a			;
		ld a,d			;
		or h			;
		ld d,a			; set hl | de -> de as new TOS
		NEXT			; continue

; XOR		x1 x2 -- x1^x2
;		bitwise xor x1 with x2

		CODE XOR,xor
		pop hl			; pop x1 -> hl
		ld a,e			;
		xor l			;
		ld e,a			;
		ld a,d			;
		xor h			;
		ld d,a			; set hl ^ de -> de as new TOS
		NEXT			; continue

; =		x1 x2 -- flag
;		true if x1 = x2

		CODE ^|=|,equal
		pop hl			; pop x1 -> hl
		xor a			; 0 -> a, 0 -> cf
		sbc hl,de		; test if hl = de
true_if_z_next:	ld d,a			; require a = 0
		ld e,a			; set new TOS to 0 (FALSE)
		jr nz,1$		; set new TOS to TRUE if x1 = x2 else FALSE
		dec de			; -1 -> TOS
1$:		NEXT			; continue

; <>		x1 x2 -- flag
;		true if x1 <> x2

		CODE <>,notequal
		pop hl			; pop x1 -> hl
		xor a			; 0 -> a, 0 -> cf
		sbc hl,de		; test if hl = de
true_if_nz_next:ld d,a			; require a = 0
		ld e,a			; set new TOS to FALSE
		jr z,1$			; set new TOS to TRUE if x1 <> x2 else FALSE
		dec de			;
1$:		NEXT			; continue

; <		n1 n2 -- flag
;		true if n1 < n2 signed
;
;    : <
;      2DUP XOR 0< IF
;        DROP 0<
;        EXIT
;      THEN
;      - 0< ;

		CODE <,less
		pop hl			; pop n1 -> hl
less_hl_de:	xor a			; 0 -> a, 0 -> cf
		sbc hl,de		; test hl < de
		jp pe,true_if_p_next	; if not OV then
true_if_m_next:	ld d,a			;   require a = 0
		ld e,a			;   set new TOS to FALSE
		jp p,1$			;   if not positive then
		dec de			;     set new TOS to TRUE
1$:		NEXT			; continue
true_if_p_next:	ld d,a			; require a = 0
		ld e,a			; set new TOS to FALSE
		jp m,1$			; if not negative then
		dec de			;   set new TOS to TRUE
1$:		NEXT			; continue

; >		n1 n2 -- flag
;		true if n1 > n2 signed
;
;    : > SWAP < ;

		CODE >,more
		pop hl			; pop n1 -> hl
		ex de,hl		; n1 -> de, n2 -> hl
		jr less_hl_de		; set new TOS to TRUE if n1 > n2

; U<		u1 u2 -- flag
;		true if u1 < u2 unsigned
;
;    : U<
;      2DUP XOR 0< IF
;        NIP 0<
;        EXIT
;      THEN
;      - 0< ;

		CODE U<,uless
		pop hl			; pop n1 -> hl
uless_hl_de:	or a			; 0 -> cf
		sbc hl,de		; subtract 2OS from TOS
true_if_c_next:	sbc a			; -cf -> a
		ld d,a			;
		ld e,a			; set new TOS to TRUE if cf = 1 else FALSE
		NEXT			; continue

; U>		u1 u2 -- flag
;		true if u1 > u2 unsigned
;
;    : U> SWAP U< ;

		CODE U>,umore
		pop hl			; pop u1 -> hl
		ex de,hl		; u1 -> de. u2 -> hl
		jr uless_hl_de		; set new TOS to TRUE if u1 > u2 else FALSE

; 0=		x -- flag
;		true if x = 0;
;		also serves as a logical NOT

		CODE 0=,zeroequal
		ld a,e			;
		or d			;
		sub 1			; cf = 1 if x = 0
		jr true_if_c_next	; set new TOS to TRUE if x = 0 else FALSE

; 0<		n -- flag
;		true if n < 0

		CODE 0<,zeroless
		sla d			; cf = 1 if n < 0
		jr true_if_c_next	; set new TOS to TRUE if cf = 1 else FALSE

; D0=		dx -- flag
;		true if dx = 0
;
;    : D0= OR 0= ;

		CODE D0=,dzeroequal
		ld a,e			;
		or d			;
		pop de			;
		or e			;
		or d			;
		sub 1			; cf = 1 if dx = 0
		jr true_if_c_next	; set new TOS to TRUE if cf = 1 else FALSE

; D0<		d -- flag
;		true if d < 0
;
;    : D0< NIP 0< ;

		CODE D0<,dzeroless
		sla d			; cf = 1 if d is negative
		pop de			; pop to discard low order of d
		jr true_if_c_next	; set new TOS to TRUE if cf = 1 else FALSE

; S>D		n -- d
;		widen single to double

		CODE S>D,stod
		push de			; save TOS
		sla d			; test if n < 0
		jr true_if_c_next	; set new TOS to -1 if cf = 1 else 0

; D>S		d -- n
;		narrow double to single;
;		may throw -11 "result out of range" valid range is -32768 to 65535

		CODE D>S,dtos
		ld a,e			;
		or d			; test TOS = 0 high order of d
		jr nz,3$		; if TOS = 0 then
		pop de			;   pop to discard TOS high order of d
2$:		JP_NEXT			;   continue
3$:		pop hl			; pop 2OS -> hl low order of d
		bit 7,h			;
		jr z,4$			; if 2OS is negative then
		ld a,d			;
		add e			;
		rra			;
		inc a			;   test TOS = 0xffff
		ex de,hl		;   set new TOS to hl
		jr z,2$			;   if TOS = 0xffff then continue
4$:		ld a,-11		;
		jp throw_a		; throw -11 "result out of range"

; D=		d1 d2 -- flag
;		true if d1 = d2
;
;    : D= D- D0= ;

		COLON D=,dequal
		.dw dminus,dzeroequal
		.dw doret

; D<		d1 d2 -- flag
;		true if d1 < d2
;
;    : D<
;      DUP 3 PICK XOR 0< IF
;        2DROP D0<
;        EXIT
;      THEN
;      D- D0< ;

		COLON D<,dless
		.dw dup,three,pick,xor,zeroless,doif,1$
		.dw   twodrop,dzeroless
		.dw   doexit
1$:		.dw dminus,dzeroless
		.dw doret

; DU<		du1 du2 -- flag
;		true if ud1 < ud2
;
;    : DU<
;      DUP 3 PICK XOR 0< IF
;        2SWAP 2DROP D0<
;        EXIT
;      THEN
;      D- D0< ;

		CODE DU<,duless
		pop hl			; pop hl with low order d2
		ex (sp),hl		; save low order d2, hl with high order d1
		or a			; 0 -> cf
		sbc hl,de		; compare high order d1 with high order d2
		pop de			; pop de with low order d2
		pop hl			; pop hl with low order d1
		jp c,true_next		; if cf = 1 then set TOS to TRUE
		sbc hl,de		; compare low order d1 with low order d2
		jp true_if_c_next	; set TOS to TRUE if cf = 1

; MAX		n1 n2 -- n3
;		signed max of n1 and n2
;
;    : MAX
;      2DUP < IF SWAP THEN
;      DROP ;

		COLON MAX,max
		.dw twodup,less,doif,1$
		.dw   swap
1$:		.dw drop
		.dw doret

; MIN		n1 n2 -- n3
;		signed min of n1 and n2
;
;    : MIN
;      2DUP > IF SWAP THEN
;      DROP ;

		COLON MIN,min
		.dw twodup,more,doif,1$
		.dw   swap
1$:		.dw drop
		.dw doret

; UMAX		u1 u2 -- u3
;		unsigned max of u1 and u2
;
;    : UMAX
;      2DUP U< IF SWAP THEN
;      DROP ;

		COLON UMAX,umax
		.dw twodup,uless,doif,1$
		.dw   swap
1$:		.dw drop
		.dw doret

; UMIN		u1 u2 -- u3
;		unsigned min of u1 and u2
;
;    : UMIN
;      2DUP U> IF SWAP THEN
;      DROP ;

		COLON UMIN,umin
		.dw twodup,umore,doif,1$
		.dw   swap
1$:		.dw drop
		.dw doret

; DMAX		d1 d2 -- d3
;		signed double max of d1 and d2
;
;    : DMAX
;      2OVER 2OVER D< IF 2SWAP THEN
;      2DROP ;

		COLON DMAX,dmax
		.dw twoover,twoover,dless,doif,1$
		.dw   twoswap
1$:		.dw twodrop
		.dw doret

; DMIN		d1 d2 -- d3
;		signed double min of d1 and d2
;
;    : DMIN
;      2OVER 2OVER D< INVERT IF 2SWAP THEN
;      2DROP ;

		COLON DMIN,dmin
		.dw twoover,twoover,dless,invert,doif,1$
		.dw   twoswap
1$:		.dw twodrop
		.dw doret

; WITHIN	x1 x2 x3 -- flag
;		true if x1 is within x2 up to x3 exclusive
;
;    : WITHIN OVER - >R - R> U< ;

		COLON WITHIN,within
		.dw over,minus,tor,minus,rfrom,uless
		.dw doret

; INVERT	x1 -- x2
;		one's complement ~x1
;
;    : INVERT 1+ NEGATE ;
;    : INVERT -1 XOR ;

		CODE INVERT,invert
		inc de			;
		jr negate		; set new TOS to -x1 - 1 and continue

; NEGATE	n1 -- n2
;		two's complement -n1
;
;    : NEGATE 0 SWAP - ;
;    : NEGATE INVERT 1+ ;

		CODE NEGATE,negate
		xor a			; 0 -> a
		sub e			;
		ld e,a			; -e -> e
		sbc a			;
		sub d			;
		ld d,a			; -cf - d -> d, set new TOS
		NEXT			; continue

; ABS		n1 -- n2
;		absolute value |n1|
;
;    : ABS DUP 0< IF NEGATE THEN ;

		CODE ABS,abs
		bit 7,d			; test if TOS is negative
		jr nz,negate		; NEGATE if TOS is negative
		NEXT			; continue

; DNEGATE	d1 -- d2
;		two's complement -d1
;
;    : DNEGATE SWAP INVERT SWAP INVERT 1 M+ ;

		CODE DNEGATE,dnegate
		pop hl			; pop hl with low order d1
		push de			; save de with high order d1
		ex de,hl		; hl -> de
		xor a			; 0 -> cf
		ld l,a			;
		ld h,a			; 0 -> hl
		sbc hl,de		; -de -> hl low order d2
		pop de			; pop de with high order d1
		push hl			; save hl with low order d2
		ld l,a			;
		ld h,a			; 0 -> hl
		sbc hl,de		; -de -> hl high order d2
		ex de,hl		; set new TOS to hl
		JP_NEXT			; continue

; DABS		d1 -- d2
;		absolute value |d1|
;
;    : DABS DUP 0< IF DNEGATE THEN ;

		CODE DABS,dabs
		bit 7,d			; test if TOS is negative
		jr nz,dnegate		; DNEGATE if TOS is negative
		JP_NEXT			; continue

; LSHIFT	x1 u -- x2
;		logical shift left x1 << u

		CODE LSHIFT,lshift
		pop hl			; pop x1 -> hl
		jr 2$			; while --e is nonnegative
1$:		add hl,hl		;   hl << 1 -> hl
2$:		dec e			;
		jp p,1$			; repeat
		ex de,hl		; set new TOS to hl
		JP_NEXT			; continue

; RSHIFT	x1 u -- x2
;		logical shift right x1 >> u

		CODE RSHIFT,rshift
		pop hl			; pop x1 -> hl
		jr 2$			; while --e is nonnegative
1$:		srl h			;
		rr l			;   hl >> 1 -> hl
2$:		dec e			;
		jp p,1$			; repeat
		ex de,hl		; set new TOS to hl
		JP_NEXT			; continue

; 1+		n1 -- n2
;		increment n1+1
;
;    : 1+ 1 + ;

		CODE 1+,oneplus
		inc de			; set de++ as TOS
		NEXT			; continue

; 2+		n1 -- n2
;		increment n1+2
;
;    : 2+ 2 + ;

		CODE 2+,twoplus
		inc de			;
		jr oneplus		; set de + 2 -> de as TOS

; 1-		n1 -- n2
;		decrement n1-1
;
;    : 1- 1 - ;

		CODE 1-,oneminus
		dec de			; set de-- as TOS
		NEXT			; continue

; 2-		n1 -- n2
;		decrement n1-2
;
;    : 2- 2 - ;

		CODE 2-,twominus
		dec de			;
		jr oneminus		; set de - 2 -> de as TOS

; 2*		n1 -- n2
;		arithmetic shift left n1 << 1
;
;    : 2* 2 * ;

		CODE 2*,twostar
		sla e
		rl d			; set 2 * de -> de as TOS
		NEXT			; continue

; 2/		n1 -- n2
;		arithmetic shift right n1 >> 1
;
;    : 2/ 2 / ;

		CODE 2/,twoslash
		sra d
		rr e			; set de / 2 -> de as TOS
		NEXT			; continue

; D2*		d1 -- d2
;		arithmetic shift left d1 << 1
;
;    : D2* 2 MD* ;

		CODE D2*,dtwostar
		pop hl			;
		sla l			;
		rl h			;
		push hl			;
		rl e			;
		rl d			;
		JP_NEXT			;

; D2/		d1 -- d2
;		arithmetic shift right d1 >> 1
;
;    : D2/ 1 2 M*/ ;

		CODE D2/,dtwoslash
		sra d			;
		rr e			;
		pop hl			;
		rr h			;
		rr l			;
		push hl			;
		JP_NEXT			;

; CELL+		addr -- addr
;		increment to next cell
;
;    : CELL+ 2+ ;

		CODE CELL+,cellplus
		jr twoplus		; same as 2+

; CELLS		n1 -- n2
;		convert to cell unit
;
;    : CELLS 2* ;

		CODE CELLS,cells
		jr twostar		; same as 2*

; CHAR+		n1 -- n1
;		increment to next char
;
;    : CHAR+ 1+ ;

		CODE CHAR+,charplus
		jr oneplus		; same as 1+

; CHARS		n1 -- n2
;		convert to char unit (does nothing as chars are bytes)
;
;    : CHARS ;

		CODE CHARS,chars
		JP_NEXT			; do nothing

;-------------------------------------------------------------------------------
;
;		IEEE 754 SINGLE PRECISION FLOATING-POINT OPERATIONS
;
;-------------------------------------------------------------------------------

.if IEEE

; Include mathr.asm with rounding or smaller math.asm with truncation

.include "mathr.asm"

;		mathr.asm and math.asm floating-point operations jump table
fop_tab:	.dw fadd	; 0
		.dw fsuby	; 1
		.dw fmul	; 2
		.dw fdivy	; 3
		.dw ftrunc	; 4
		.dw ffloor	; 5
		.dw fround	; 6
		.dw fneg	; 7
		.dw fabs	; 8

;= F+		r1 r2 -- r3
;		sum r1+r2;
;		may throw -43 "floating-point result out of range"

		CODE F+,fplus
		xor a			; mathr.asm:fadd

fop2:		; floating-point dyadic operation driver
		; may throw -43 "floating-point result out of range"

		DINT			; disable interrupts before using auxiliary registers
		pop hl			; pop hl with 2OS
		exx			;
		pop bc			; pop bc' with 3OS
		pop de			; pop de' with 4OS
		exx			;
fop:		push bc			; save bc with ip
		ld b,d			;
		ld c,e			; de -> bc
		ex de,hl		; hl -> de
		add a			;
		add <fop_tab		;
		ld l,a			; 2*a + lsb(fop_tab) -> l
		adc a,>fop_tab		;
		sub l			;
		ld h,a			; cf + msb(fop_tab) -> h
		ld a,(hl)		;
		inc hl			;
		ld h,(hl)		;
		ld l,a			; [hl] -> hl
		call jphl		; call (hl) to execute bcde <fop> bcde' -> bcde
		EINT			; enable interrupts after using auxiliary registers
		ld a,-43		;
		jp c,throw_a		; if error then throw -43
		ex de,hl		; de -> hl
		ld d,b			;
		ld e,c			; set bc -> de as new TOS
		pop bc			; restore bc with ip
		push hl			; save hl as new 2OS
		JP_NEXT			; continue

;= F-		r1 r2 -- r3
;		difference r1-r2;
;		may throw -43 "floating-point result out of range"

		CODE F-,fminus
		ld a,1			; mathr.asm:fsuby
		jr fop2			;

;= F*		r1 r2 -- r3
;		product r1*r2;
;		may throw -43 "floating-point result out of range"

		CODE F*,fstar
		ld a,2			; mathr.asm:fmul
		jr fop2			;

;= F/		r1 r2 -- r3
;		quotient r1/r2;
;		may throw -42 "floating-point divide by zero";
;		may throw -43 "floating-point result out of range"

		CODE F/,fslash
		ld a,d			;
		or e			; test TOS de with high order float word
		ld a,-42		;
		jp z,throw_a		; if TOS = 0 then throw -42
		ld a,3			; mathr.asm:fdivy
		jr fop2			;

;= FTRUNC	r1 -- r2
;		truncate float towards zero

		CODE FTRUNC,ftrunc_	; note: trailing underscor
		ld a,4			; mathr.asm:ftrunc

fop1:		; floating-point unary function driver
		; may throw -43 "floating-point result out of range"

		pop hl			; pop hl with 2OS
		DINT			; disable interrupts before using auxiliary registers
		jr fop			;

;= FLOOR	r1 -- r2
;		floor float towards negative infinity;
;		may throw -43 "floating-point result out of range"

		CODE FLOOR,floor
		ld a,5			; mathr.asm:ffloor
		jr fop1			;

;= FROUND	r1 -- r2
;		round float to nearest;
;		may throw -43 "floating-point result out of range"

		CODE FROUND,fround_	; note: trailing underscore
		ld a,6			; mathr.asm:fround
		jr fop1			;

;= FNEGATE	r1 -- r2
;		negate float

		CODE FNEGATE,fnegate
		ld a,7			; mathr.asm:fneg
		jr fop1			;

;= FABS		r1 -- r2
;		absolute value |r1|
;
;    : FABS 2DUP F0< IF FNEGATE THEN ;

		CODE FABS,fabs_		; note: trailing underscore
		ld a,8			; mathr.asm:fabs
		jr fop1			;

;= F=		r1 r2 -- flag
;		true if r1 = r2
;
;    : F= D= ; ( works for IEEE 754 floating-point without negative zero and inf/nan )

		CODE F=,fequal
		jp dequal		; IEEE 754 float are comparable as integer

;= F<		r1 r2 -- flag
;		true if r1 < r2
;
;    : F<
;      DUP 3 PICK AND 0< IF
;        2SWAP
;      D< ; ( works for IEEE 754 floating-point without negative zero and inf/nan )

		COLON F<,fless
		.dw dup,three,pick,and,zeroless,doif,1$
		.dw   twoswap
1$:		.dw dless
		.dw doret

;= F0=		r -- flag
;		true if r = 0.0e0
;
;    : F0= D0= ; ( works for IEEE 754 floating-point without negative zero and inf/nan )

		CODE F0=,fzeroequal
		jp dzeroequal		; IEEE 754 float are comparable as integer

;= F0<		r -- flag
;		true if r < 0.0e0
;
;    : F0< D0< ; ( works for IEEE 754 floating-point without negative zero and inf/nan )

		CODE F0<,fzeroless
		jp dzeroless		; IEEE 754 float are comparable as integer

;= FMAX		r1 r2 -- r3
;		max of r1 and r2
;
;    : FMAX
;      2OVER 2OVER F< IF 2SWAP THEN
;      2DROP ;

		COLON FMAX,fmax
		.dw twoover,twoover,fless,doif,1$
		.dw   twoswap
1$:		.dw twodrop
		.dw doret

;= FMIN		r1 r2 -- r3
;		min of r1 and r2
;
;    : FMIN
;      2OVER 2OVER F< INVERT IF 2SWAP THEN
;      2DROP ;

		COLON FMIN,fmin
		.dw twoover,twoover,fless,invert,doif,1$
		.dw   twoswap
1$:		.dw twodrop
		.dw doret

;= D>F		d -- r
;		widen signed double to float

		CODE D>F,dtof
		DINT			; disable interrupts before using auxiliary registers
		push de			; save TOS
		exx			;
		pop bc			; pop bc with TOS
		pop de			; pop de with 2OS
		call itof		; integer bcde -> float bcde
		push de			; save de as new 2OS
		push bc			; save bc as new TOS
		exx			;
		EINT			; enable interrupts after using auxiliary registers
		pop de			; pop new TOS
		JP_NEXT			; continue

;= S>F		n -- r
;		widen signed single to float

		CODE S>F,stof
		push de			; save TOS with n as new 2OS
		sla d			; d << 1 -> cf.d
		sbc a			; -cf -> a
		ld d,a			;
		ld e,a			; set new TOS to sign extend 2OS
		jr dtof			; D>F and continue

;= F>D		r -- d
;		narrow float to a signed double;
;		may throw -11 "result out of range"

		CODE F>D,ftod
		SVBC			; save bc with ip
		ld c,e			;
		ld b,d			; de -> bc with TOS
		pop de			; pop de with 2OS
		call ftoi		; float bcde -> integer bcde
		push de			; save de as new 2OS
		ld e,c			;
		ld d,b			; bc -> de with new TOS
		LDBC			; restore bc with ip
		ld a,-11		;
		jp c,throw_a		; if conversion failed then throw -11
		JP_NEXT			; continue

;= F>S		r -- n
;		narrow float to a signed single;
;		may throw -11 "result out of range"

		COLON F>S,ftos
		.dw ftod,dtos
		.dw doret

;= >FLOAT	c-addr u -- r true | false
;		convert string to float;
;		leaves the float and true if string is converted;
;		leaves false if string is unconvertable

		CODE >FLOAT,tofloat
		pop hl			; pop hl with c-addr
		ld a,e			; e -> a with string length u (8 bits, ignore high byte)
		push bc			; save bc with ip
		DINT			; disable interrupts before using auxiliary registers
		call atof		; [hl..hl+b-1] -> bcde
		EINT			; enable interrupts after using auxiliary registers
		ld h,b			;
		ld l,c			; bc -> hl
		pop bc			; restore bc with ip
		jp c,false_next		; if error then leave FALSE and continue
		push de			; save de with new 3OS
		push hl			; save hl with new 2OS
		jp true_next		; set new TOS to TRUE and continue

;= REPRESENT	r c-addr u -- n flag true
;		convert float to string;
;		store decimal digits of the float in buffer c-addr with size u > 0;
;		leaves decimal exponent n+1 and flag = true if negative

		CODE REPRESENT,represent
		SVBC			; save bc with ip
		ld a,e			; e -> a with buffer size u (ignore high byte)
		pop hl			; pop hl with c-addr
		pop bc			; pop bc with 3OS float r
		pop de			; pop de with 4OS float r
		DINT			; disable interrupts before using auxiliary registers
		call ftoa		; bcde -> [hl...hl+a-1 digits] exponent e and sign d bit 7
		EINT			; enable interrupts after using auxiliary registers
		rl d			;
		sbc a			;
		ld h,a			;
		ld l,a			; TRUE -> hl if d bit 7 else FALSE -> hl
		inc e			; decimal exponent + 1 returned by REPRESENT
		ld a,e			;
		rla			;
		sbc a			;
		ld d,a			; sign extend e to de
		push de			; set new 3OS to de with n
		push hl			; set new 2OS hl with flag
		LDBC			; restore bc with ip
		jp true_next		; set new TOS to TRUE and continue

;= PRECISION	-- +n
;		floating-point output precision, the number of decimal digits displayed is 7 by default
;
;    7 VALUE PRECISION

		VALUE PRECISION,precision
		.dw 7

;= FS.		r --
;		output float in scientific notation with a trailing space
;
;    : FS.
;      HERE PRECISION REPRESENT DROP IF
;        '- EMIT
;      THEN
;      HERE C@ EMIT
;      '. HERE C!
;      HERE PRECISION '0 -TRIM TYPE
;      'E EMIT 1- . ;

		COLON FS.,fsdot
		.dw here,precision,represent,drop,doif,1$
		.dw   dolit,'-,emit
1$:		.dw here,cfetch,emit
		.dw dolit,'.,here,cstore
		.dw here,precision,dolit,'0,mtrim,type
		.dw dolit,'E,emit,oneminus,dot
		.dw doret

;= F.		r --
;		output float with a trailing space;
;		output fixed notation when 1e-1 <= |r| < 1e+7, otherwise output scientific notation
;
;;    : F.
;      HERE PRECISION REPRESENT DROP IF
;        '- EMIT
;      THEN
;      DUP 0 PRECISION 1+ WITHIN IF
;        DUP IF
;          HERE OVER TYPE
;        ELSE
;          '0 EMIT
;        THEN
;        '. EMIT
;        HERE OVER +
;        PRECISION ROT - '0 -TRIM TYPE SPACE
;        EXIT
;      THEN
;      HERE C@ EMIT
;      '. HERE C!
;      HERE PRECISION '0 -TRIM TYPE
;      'E EMIT 1- . ;

		COLON F.,fdot
		.dw here,precision,represent,drop,doif,1$
		.dw   dolit,'-,emit
1$:		.dw dup,zero,precision,oneplus,within,doif,4$
		.dw   dup,doif,2$
		.dw     here,over,type
		.dw   doahead,3$
2$:		.dw     dolit,'0,emit
3$:		.dw   dolit,'.,emit
		.dw   here,over,plus
		.dw   precision,rot,minus,dolit,'0,mtrim,type,space
		.dw   doexit
4$:		.dw here,cfetch,emit
		.dw dolit,'.,here,cstore
		.dw here,precision,dolit,'0,mtrim,type
		.dw dolit,'E,emit,oneminus,dot
		.dw doret

;-------------------------------------------------------------------------------
;
;		IEEE 754 SINGLE PRECISION FLOATING-POINT MATH FUNCTIONS
;
;-------------------------------------------------------------------------------

.if FUNC

FHALF		.equ 0x3f00	; high word of floating-point constant 0.5
FHONE		.equ 0x3f80	; high word of floating-point constant 1.0
FHTWO		.equ 0x4000	; high word of floating-point constant 2.0

;= FSQRT	r1 -- r2
;		square root of float
;
;    : FSQRT
;      2DUP F0< IF -46 THROW THEN
;      2DUP F0= IF EXIT THEN
;      \ map r1 to [0.5,2) using sqrt(x*2^n) = sqrt(x*2^(n%2))*2^(n/2)
;      DUP 8 RSHIFT $3f - -ROT               \ 2^(n/2) = 2^(exponent/2 - bias/2)
;      $ff AND $3f00 +                       \ remove exponent 2^(n/2) from x
;      2DUP                                  \ initial estimate y is x
;      5 0 DO                                \ 5 Newton-Raphson iterations
;        2OVER 2OVER F/ F+ .5E0 F*           \ x y -- x (y+x/y)/2
;      LOOP
;      2SWAP 2DROP                           \ x y -- y
;      ROT $7f + 7 LSHIFT 0 SWAP F* ;        \ y times 2^(n/2)

		COLON FSQRT,fsqrt
		.dw twodup,fzeroless,doif,1$
		.dw   dolit,-46,throw
1$:		.dw twodup,fzeroequal,doif,2$
		.dw   doexit
2$:		.dw dup,dolit,8,rshift,dolit,0x3f,minus,mrot
		.dw dolit,0xff,and,dolit,0x3f00,plus
		.dw twodup
		.dw dolit,5,zero,dodo,4$
3$:		.dw   twoover,twoover,fslash,fplus,dotwolit,FHALF,0,fstar
		.dw doloop,3$
4$:		.dw twoswap,twodrop
		.dw rot,dolit,0x7f,plus,dolit,7,lshift,zero,swap,fstar
		.dw doret

;= PI		-- r
;		floating-point constant pi
;
;    3.1415928E0 2CONSTANT PI

		TWOCONSTANT PI,pi
		.dw 0x4049
		.dw 0x0fdb

;= PI/2		-- r
;		floating-point constant pi/2 (half pi)
;
;    1.5707964E0 2CONSTANT PI/2

		TWOCONSTANT PI/2,halfpi
		.dw 0x3fc9
		.dw 0x0fdb

;= FCOSI	r1 flag -- r2
;		if flag is -1 (TRUE) then leave sin(r1) else if flag is 0 (FALSE) then leave cos(r1)
;
;    : FCOSI                                 \ r2=sin(r1) if flag=-1 else r2=cos(r1) if flag=0
;      >R                                    \ save flag
;      PI/2 F/                               \ map r1 to x in [-pi/4,pi/4]
;      2DUP .5E0 F+ F>D                      \ floor(2x/pi+.5)
;      \ save (floor(2x/pi+.5)+flag+1) mod 4 = quadrant 0,1,2,3 where flag is -1 (sin) or 0 (cos)
;      OVER R> + 1+ >R
;      D>F F- PI/2 F*                        \ pi/2 * (2x/pi - floor(2x/pi + .5))
;      2DUP 2DUP F* FNEGATE 2SWAP            \ -- -x*x x
;      \ quadrant 0:  sin(x) =  x - x^3/3! + x^5/5! - x^7/7! + ...
;      \ quadrant 1:  cos(x) =  1 - x^2/2! + x^4/4! - x^6/6! + ...
;      \ quadrant 2: -sin(x) = -x + x^3/3! - x^5/5! + x^7/7! - ...
;      \ quadrant 3: -cos(x) = -1 + x^2/2! - x^4/4! + x^6/6! - ...
;      R@ 1 AND IF 2DROP 1E0 THEN            \ initial term 1 for quadrant 1 and 3
;      R@ 2 AND IF FNEGATE THEN              \ negate initial term for quadrant 2 and 4
;      2SWAP                                 \ -- x|1|-x|-1 -x*x
;      \ Maclaurin series iterations i=2,4,6,8,10,12 (cos) or i=1,3,5,7,9,11 (sin)
;      13 2 R> 1 AND - DO                    \ 6 iterations
;        2OVER 2OVER F*                      \ -- ... term -x*x -x*x*term
;        I DUP 1+ *                          \ -- ... term -x*x -x*x*term i*(i+1)
;        S>F F/                              \ -- ... term -x*x -x*x*term/(i*(i+1))
;        2SWAP                               \ -- ... term -x*x*term/(i*(i+1)) -x*x
;      2 +LOOP
;      2DROP
;      F+ F+ F+ F+ F+ F+ ;                   \ sum the 7 terms in reverse order for accuracy

		COLON FCOSI,fcosi
		.dw tor
		.dw halfpi,fslash
		.dw twodup,dotwolit,FHALF,0,fplus,ftod
		.dw over,rfrom,plus,oneplus,tor
		.dw dtof,fminus,halfpi,fstar
		.dw twodup,twodup,fstar,fnegate,twoswap
		.dw rfetch,one,and,doif,1$
		.dw   twodrop,dotwolit,FHONE,0
1$:		.dw rfetch,two,and,doif,2$
		.dw   fnegate
2$:		.dw twoswap
		.dw dolit,13,two,rfrom,one,and,minus,dodo,4$
3$:		.dw   twoover,twoover,fstar
		.dw   i,dup,oneplus,star
		.dw   stof,fslash
		.dw   twoswap
		.dw two,doplusloop,3$
4$:		.dw twodrop
		.dw fplus,fplus,fplus,fplus,fplus,fplus
		.dw doret

;= FSIN		r1 -- r2
;		sine of float in radian
;
;    : FSIN TRUE FCOSI ;

		COLON FSIN,fsin
		.dw true,fcosi
		.dw doret

;= FCOS		r1 -- r2
;		cosine of float in radian
;
;    : FCOS FALSE FCOSI ;

		COLON FCOS,fcos
		.dw false,fcosi
		.dw doret

;= FTAN		r1 -- r2
;		tangent of float in radian
;
;    : FTAN 2DUP FSIN 2SWAP FCOS F/ ;

		COLON FTAN,ftan
		.dw twodup,fsin,twoswap,fcos,fslash
		.dw doret

;= FASIN	r1 -- r2
;		arc sine of float, in radian
;
;    : FASIN
;      2DUP F0= IF EXIT THEN
;      2DUP FABS 1E0 F= IF                           \ if |x|=1 then
;        PI/2 2SWAP F0< IF FNEGATE THEN              \ sign(x)*pi/2
;	 EXIT
;      THEN
;      2DUP 2DUP F* 1E0 2SWAP F- FSQRT FATAN2 ;      \ arctan(x/sqrt(1-x^2)) = atan2(x,sqrt(1-x*x))

		COLON FASIN,fasin
		.dw twodup,fzeroequal,doif,1$
		.dw   doexit
1$:		.dw twodup,fabs_,dotwolit,FHONE,0,fequal,doif,3$
		.dw   halfpi,twoswap,fzeroless,doif,2$
		.dw     fnegate
2$:		.dw   doexit
3$:		.dw twodup,twodup,fstar,dotwolit,FHONE,0,twoswap,fminus,fsqrt,fatan2
		.dw doret

;= FACOS	r1 -- r2
;		arc cosine of float, in radian
;
;    : FACOS FASIN PI/2 2SWAP F- ;

		COLON FACOS,facos
		.dw fasin,halfpi,twoswap,fminus
		.dw doret

;= FATAN	r1 -- r2
;		arc tangent of float, in radian
;
;    : FATAN
;      \ map r1 to [-1,1] using arctan(x) = sign(x) * (pi/2-arctan(1/abs(x)))
;      1E0 2OVER FABS F< IF                  \ if |r1| > 1 then
;        2DUP F0< -ROT
;        1E0 2SWAP FABS F/
;        TRUE
;      ELSE
;        FALSE
;      THEN
;      -ROT
;      \ map r1 in [-1,1] to [-sqrt(2)+1,sqrt(2)-1] using arctan(x) = 2*arctan(x/(1+sqrt(1+x^2)))
;      .41423562E0 2OVER FABS F< IF          \ if |r1| > sqrt(2)-1 then
;        2DUP 2DUP F* 1E0 F+ FSQRT 1E0 F+ F/
;        TRUE
;      ELSE
;        FALSE
;      THEN
;      -ROT
;      \ Maclaurin series arctan(x) = x - x^3/3 + x^5/5 - x^7/7 + ... with x in (-1,1)
;      2DUP 2DUP 2DUP F* FNEGATE 2SWAP       \ -- x -x*x x
;      16 3 DO                               \ 7 iterations
;        2OVER F*                            \ -- x -x^3/3 ... -x*x -x*x*term
;        2DUP I S>F F/                       \ -- x -x^3/3 ... -x*x -x*x*term -x*x*term/i
;        2ROT 2ROT                           \ -- x -x^3/3 ... -x*x*term/i -x*x -x*x*term
;      2 +LOOP
;      2DROP 2DROP                           \ -- x -x^3/3 ... x^15/15
;      F+ F+ F+ F+ F+ F+ F+                  \ sum the 8 terms in reverse order for accuracy
;      ROT IF 2E0 F* THEN                    \ 2*arctan(x/(1+sqrt(1+x^2)))
;      ROT IF PI/2 2SWAP F-
;        ROT IF FNEGATE THEN
;      THEN ;                                \ sign(x) * (pi/2-arctan(1/abs(x)))

		COLON FATAN,fatan
		.dw dotwolit,FHONE,0,twoover,fabs_,fless,doif,1$
		.dw   twodup,fzeroless,mrot
		.dw   dotwolit,FHONE,0,twoswap,fabs_,fslash
		.dw   true
		.dw doahead,2$
1$:		.dw   false
2$:		.dw mrot
		.dw dotwolit,0x3ed4,0x16b1,twoover,fabs_,fless,doif,3$
		.dw   twodup,twodup,fstar,dotwolit,FHONE,0,fplus,fsqrt,dotwolit,FHONE,0,fplus,fslash
		.dw   true
		.dw doahead,4$
3$:		.dw   false
4$:		.dw mrot
		.dw twodup,twodup,twodup,fstar,fnegate,twoswap
		.dw dolit,16,three,dodo,6$
5$:		.dw   twoover,fstar
		.dw   twodup,i,stof,fslash
		.dw   tworot,tworot
		.dw two,doplusloop,5$
6$:		.dw twodrop,twodrop
		.dw fplus,fplus,fplus,fplus,fplus,fplus,fplus
		.dw rot,doif,7$
		.dw   dotwolit,FHTWO,0,fstar
7$:		.dw rot,doif,8$
		.dw   halfpi,twoswap,fminus
		.dw   rot,doif,8$
		.dw     fnegate
8$:		.dw doret

;= FATAN2	r1 r2 -- r3
;		atan2(r1,r2) = atan(r1/r2) but using a more accurate formulation
;
;    : FATAN2
;      2DUP FNEGATE F0< IF
;        F/ FATAN
;        EXIT
;      THEN
;      2SWAP
;      2DUP F0= IF
;        2DROP F0< IF PI ELSE PI/2 THEN
;        EXIT
;      THEN
;      PI/2 2OVER F0< IF FNEGATE THEN
;      2ROT 2ROT F/ FATAN F- ;

		COLON FATAN2,fatan2
		.dw twodup,fnegate,fzeroless,doif,1$
		.dw   fslash,fatan
		.dw   doexit
1$:		.dw twoswap
		.dw twodup,fzeroequal,doif,3$
		.dw   twodrop,fzeroless,doif,2$
		.dw     pi
		.dw     doexit
2$:		.dw   halfpi
		.dw   doexit
3$:		.dw halfpi,twoover,fzeroless,doif,4$
		.dw   fnegate
4$:		.dw tworot,tworot,fslash,fatan,fminus
		.dw doret

;= FLN		r1 -- r2
;		natural log of float
;
;    : FLN
;      2DUP 2DUP F0< -ROT F0= OR IF -46 THROW THEN
;      \ map r1 to [0.5,1) using ln(x*2^n) = ln(x) + ln(2^n) = ln(x) + n*ln(2)
;      DUP 7 RSHIFT $7e - -ROT               \ 2^(n+1) = 2^(exponent - bias + 1)
;      $7f AND $3f00 +                       \ remove exponent 2^(n+1)
;      1E0 2SWAP F-                          \ 1-x
;      \ Maclaurin series -ln(1-x) = x + x^2/2 + x^3/3 + ... with x in (0,0.5]
;      2DUP 2DUP                             \ -- x x x
;      22 2 DO                               \ 20 iterations
;        2OVER F*                            \ -- x x^2/2 ... x term*x
;        2DUP I S>F F/                       \ -- x x^2/2 ... x term*x term*x/i
;        2ROT 2ROT                           \ -- x x^2/2 ... term*x/i x term*x
;      LOOP
;      2DROP 2DROP                           \ -- x x^2/2 ... x^19/19
;      20 0 DO F+ LOOP                       \ sum the 21 terms in reverse order
;      FNEGATE
;      ROT S>F .69314724E0 F* F+ ;           \ + n*ln(2) with approx ln(2) such that 1E0 FLN = 0

		COLON FLN,fln
		.dw twodup,twodup,fzeroless,mrot,fzeroequal,or,doif,1$
		.dw   dolit,-46,throw
1$:		.dw dup,dolit,7,rshift,dolit,0x7e,minus,mrot
		.dw dolit,0x7f,and,dolit,0x3f00,plus
		.dw dotwolit,FHONE,0,twoswap,fminus
		.dw twodup,twodup
		.dw dolit,22,two,dodo,3$
2$:		.dw   twoover,fstar
		.dw   twodup,i,stof,fslash
		.dw   tworot,tworot
		.dw doloop,2$
3$:		.dw twodrop,twodrop
		.dw dolit,20,zero,dodo,5$
4$:		.dw   fplus
		.dw doloop,4$
5$:		.dw fnegate
		.dw rot,stof,dotwolit,0x3f31,0x7219,fstar,fplus
		.dw doret

;= FEXP		r1 -- r2
;		natural exponent of float
;
;    : FEXP
;      2DUP F0< -ROT
;      FABS
;      \ map |r1| to [0,ln(2)) using exp(x+k*ln(2)) = exp(x)*2^k
;      2DUP .69314724E0 F/ F>S               \ ln(2) = .69314724E0
;      DUP $7f > IF -43 THROW THEN           \ multiply by 2^k will overflow
;      DUP>R
;      S>F .69314724E0 F* F-                 \ ln(2) = .69314724E0
;      \ Maclaurin series expm1(x) = exp(x) - 1 = x + x^2/2! + x^3/3! + ...
;      2DUP                                  \ -- x x
;      10 2 DO                               \ 8 iterations
;        2OVER 2OVER F*                      \ -- x x^2/2! ... term x term*x
;        I S>F F/                            \ -- x x^2/2! ... term x term*x/i
;        2SWAP                               \ -- x x^2/2! ... term term*x/i x
;      LOOP
;      2DROP                                 \ -- x x^2/2! ... x^9/9!
;      F+ F+ F+ F+ F+ F+ F+ F+               \ sum the 9 terms in reverse order
;      1E0 F+                                \ exp(x) = expm1(x) + 1
;      R> 7 LSHIFT +                         \ multiply exp(x) by 2^k
;      ROT IF 1E0 2SWAP F/ THEN ;            \ return reciprocal for negative r1

		COLON FEXP,fexp
		.dw twodup,fzeroless,mrot
		.dw fabs_
		.dw twodup,dotwolit,0x3f31,0x7219,fslash,ftos
		.dw dup,dolit,0x7f,more,doif,1$
		.dw   dolit,-43,throw
1$:		.dw duptor
		.dw stof,dotwolit,0x3f31,0x7219,fstar,fminus
		.dw twodup
		.dw dolit,10,two,dodo,3$
2$:		.dw   twoover,twoover,fstar
		.dw   i,stof,fslash
		.dw   twoswap
		.dw doloop,2$
3$:		.dw twodrop
		.dw fplus,fplus,fplus,fplus,fplus,fplus,fplus,fplus
		.dw dotwolit,FHONE,0,fplus
		.dw rfrom,dolit,7,lshift,plus
		.dw rot,doif,4$
		.dw   dotwolit,FHONE,0,twoswap,fslash
4$:		.dw doret

;= FLOG		r1 -- r2
;		base 10 log of float
;
;    : FLOG FLN 0.4342945E0 F* ;    \ = ln(x)/ln(10) approx ln(10) such that 10E0 FLOG = 1E0

		COLON FLOG,flog
		.dw fln,dotwolit,0x3ede,0x5bd9,fstar
		.dw doret

;= FALOG	r1 -- r2
;		base 10 exponent of float
;
;    : FALOG 2.3025853E0 F* FEXP ;    \ = exp(x*ln(10))

		COLON FALOG,falog
		.dw dotwolit,0x4013,0x5d8f,fstar,fexp
		.dw doret

;= F^		r1 r2 -- r3
;		raise r1 to r2 using exp(ln(r1)*r2) where r1 > 0
;
;    : F^ 2SWAP FLN F* FEXP ;

		COLON F^,fhat
		.dw twoswap,fln,fstar,fexp
		.dw doret

;= F**		r1 r2 -- r3
;		raise r1 to r2
;
;    : F**
;      2DUP F0= IF                           \ r2 = 0
;        2OVER F0= IF -46 THROW THEN         \ error if r1 = 0 and r2 = 0
;        2DROP 2DROP 1E0 EXIT                \ return 1.0E0
;      THEN
;      2OVER F0= IF                          \ r1 = 0
;        2DUP F0< IF -46 THROW THEN          \ error if r1 = 0 and r2 < 0
;        2DROP 2DROP 0E0 EXIT                \ return 0.0E0
;      THEN
;      \ exponentiation r1^n by repeated squaring when n is a small integer |n|<=16
;      2DUP 2DUP FTRUNC F= IF                \ r2 has no fractional part
;        2DUP ['] F>D CATCH 0= IF            \ r2 is convertable to a double n
;          2DUP DABS 17. DU< IF              \ |n| <= 16
;            DROP                            \ drop high order of n
;            DUP 0< >R                       \ save sign of n
;            ABS >R                          \ save |n|
;            2DROP                           \ drop old r2
;            1E0                             \ -- r1 1.0
;            BEGIN
;              R@ 1 AND IF 2OVER F* THEN
;              R> 1 RSHIFT                   \ -- r1^n product u>>1
;            DUP WHILE
;              >R
;              2SWAP 2DUP F* 2SWAP           \ -- r1^n^2 product u>>1
;            REPEAT
;            DROP 2SWAP 2DROP                \ -- product
;            R> IF 1E0 2SWAP F/ THEN         \ reciprocal when exponent was negative
;            EXIT
;          THEN
;          OVER 1 AND IF                     \ n is odd
;            2OVER F0< IF                    \ r1 is negative
;              2DROP 2SWAP FABS 2SWAP F^ FNEGATE
;              EXIT                          \ return -(|r1|^n)
;            THEN
;          THEN
;          2DROP 2SWAP FABS 2SWAP            \ we want to return |r1|^r2
;        ELSE
;          2DROP                             \ drop copy of r2
;        THEN
;      THEN
;      F^ ;

		COLON F**,fpow
		.dw twodup,fzeroequal,doif,2$
		.dw   twoover,fzeroequal,doif,1$
		.dw     dolit,-46,throw
1$:		.dw   twodrop,twodrop,dotwolit,FHONE,0
		.dw   doexit
2$:		.dw twoover,fzeroequal,doif,4$
		.dw   twodup,fzeroless,doif,3$
		.dw     dolit,-42,throw
3$:		.dw   twodrop,twodrop,zero,zero
		.dw   doexit
4$:		.dw twodup,twodup,ftrunc_,fequal,doif,12$
		.dw   twodup,dolit,ftod,catch,zeroequal,doif,11$
		.dw     twodup,dabs,dotwolit,0,17,duless,doif,9$
		.dw       drop
		.dw       dup,zeroless,tor
		.dw       abs,tor
		.dw       twodrop
		.dw       dotwolit,FHONE,0
5$:		.dw         rfetch,one,and,doif,6$
		.dw           twoover,fstar
6$:		.dw         rfrom,one,rshift
		.dw       dup,doif,7$
		.dw         tor
		.dw         twoswap,twodup,fstar,twoswap
		.dw       doagain,5$
7$:		.dw       drop,twoswap,twodrop
		.dw       rfrom,doif,8$
		.dw         dotwolit,FHONE,0,twoswap,fslash
8$:		.dw       doexit
9$:		.dw     over,one,and,doif,10$
		.dw       twoover,fzeroless,doif,10$
		.dw         twodrop,twoswap,fabs_,twoswap,fhat,fnegate
		.dw         doexit
10$:		.dw     twodrop,twoswap,fabs_,twoswap
		.dw   doahead,12$
11$:		.dw     twodrop
12$:		.dw   fhat
		.dw doret

;= FSINH	r1 -- r2
;		sine hyperbolicus of float
;
;    : FSINH FEXP 2DUP 1E0 2SWAP F/ F- .5E0 F* ;

		COLON FSINH,fsinh
		.dw fexp,twodup,dotwolit,FHONE,0,twoswap,fslash,fminus
		.dw dotwolit,FHALF,0,fstar
		.dw doret

;= FCOSH	r1 -- r2
;		cosine hyperbolicus of float
;
;    : FCOSH FEXP 2DUP 1E0 2SWAP F/ F+ .5E0 F* ;

		COLON FCOSH,fcosh
		.dw fexp,twodup,dotwolit,FHONE,0,twoswap,fslash,fplus
		.dw dotwolit,FHALF,0,fstar
		.dw doret

;= FTANH	r1 -- r2
;		tangent hyperbolicus of float
;
;    : FTANH 2DUP F+ FEXP 2DUP 1E0 F- 2SWAP 1E0 F+ F/ ;

		COLON FTANH,ftanh
		.dw twodup,fplus,fexp,twodup,dotwolit,FHONE,0,fminus
		.dw twoswap,dotwolit,FHONE,0,fplus,fslash
		.dw doret

;= FASINH	r1 -- r2
;		arc sine hyperbolicus of float
;
;    : FASINH 2DUP 2DUP F* 1E0 F+ FSQRT F+ FLN ;

		COLON FASINH,fasinh
		.dw twodup,twodup,fstar,dotwolit,FHONE,0,fplus
		.dw fsqrt,fplus,fln
		.dw doret

;= FACOSH	r1 -- r2
;		arc cosine hyperbolicus of float
;
;    : FACOSH 2DUP 2DUP F* 1E0 F- FSQRT F+ FLN ;

		COLON FACOSH,facosh
		.dw twodup,twodup,fstar,dotwolit,FHONE,0,fminus
		.dw fsqrt,fplus,fln
		.dw doret

;= FATANH	r1 -- r2
;		arc tangent hyperbolicus of float
;
;    : FATANH 2DUP 1E0 F+ 2SWAP 1E0 2SWAP F- F/ FLN .5E0 F* ;

		COLON FATANH,fatanh
		.dw twodup,dotwolit,FHONE,0,fplus,twoswap
		.dw dotwolit,FHONE,0,twoswap,fminus,fslash
		.dw fln,dotwolit,FHALF,0,fstar
		.dw doret

.endif;FUNC
.endif;IEEE

;-------------------------------------------------------------------------------
;
;		MSX MATH-PACK FLOATING-POINT OPERATIONS
;
;-------------------------------------------------------------------------------

.if MATH

; Working area

FBUFFR		.equ 0xf75c	; numeric conversion buffer (43b)
VALTYP		.equ 0xf663	; format of the number in MSX DAC
DAC		.equ 0xf7f6	; floating point accumulator in BCD format
ARG		.equ 0xf847	; argument of MSX DAC

FRCINT		.equ 0x2f8a	; convert MSX DAC to integer in [DAC+2,DAC+3]
FRCSNG		.equ 0x2fb2	; convert MSX DAC to single precision
FIN		.equ 0x3299	; convert string to float in MSX DAC
PUFOUT		.equ 0x3426	; convert MSX DAC to string using format in A,B,C

fop_tab:	.dw 0x269a	;  0 add
		.dw 0x268c	;  1 sub
		.dw 0x27e6	;  2 mul
		.dw 0x289f	;  3 div
		.dw 0x37d7	;  4 pow
		.dw 0x30be	;  5 fix
		.dw 0x30cf	;  6 int
		.dw 0x2e8d	;  7 neg
		.dw 0x2e82	;  8 abs
		.dw 0x2aff	;  9 sqr
		.dw 0x29ac	; 10 sin
		.dw 0x2993	; 11 cos
		.dw 0x29fb	; 12 tan
		.dw 0x2a14	; 13 atn
		.dw 0x2a72	; 14 log
		.dw 0x2b4a	; 15 exp
		.dw 0x2bdf	; 16 rnd

;, FROUND	r1 -- r2
;		round float to nearest
;
;    : FROUND .5E0 F+ FLOOR ;

		COLON FROUND,fround_	; note: trailing underscore
		.dw dotwolit,0x4050,0,fplus,floor
		.dw doret

;. FTRUNC	r1 -- r2
;		truncate float towards zero

		CODE FTRUNC,ftrunc_	; note: trailing underscore
		ld a,5			; fix
		jr fop1			;

;. FLOOR	r1 -- r2
;		floor float towards negative infinity

		CODE FLOOR,floor
		ld a,6			; int
		jr fop1			;

;. FNEGATE	r1 -- r2
;		negate float

		CODE FNEGATE,fnegate
		ld a,7			; neg
		jr fop1			;

;. FABS		r1 -- r2
;		absolute value |r1|
;
;    : FABS 2DUP F0< IF FNEGATE THEN ;

		CODE FABS,fabs
		ld a,8			; abs
		jr fop1			;

;. FSQRT	r1 -- r2
;		take the square root of r1

		CODE FSQRT,fsqrt
		ld a,9			; sqr
		jr fop1			;

;. FSIN		r1 -- r2
;		sine of float in radian

		CODE FSIN,fsin
		ld a,10			; sin
		jr fop1			;

;. FCOS		r1 -- r2
;		cosine of float in radian

		CODE FCOS,fcos
		ld a,11			; cos
		jr fop1			;

;. FTAN		r1 -- r2
;		tangent of float in radian

		CODE FTAN,ftan
		ld a,12			; tan
		jr fop1			;

;. FATAN	r1 -- r2
;		arc tangent of float, in radian

		CODE FATAN,fatan
		ld a,13			; atn
		jr fop1			;

;. FRAND	r1 -- r2
;		if r1 is positive, then leave new random number from 0 to 1 exclusive;
;		if r1 is zero, then leave the last random number;
;		if r1 is negative, then seed the random number using r1

		CODE FRAND,frand
		ld a,16			; rnd
		jr fop1			;

;. F+		r1 r2 -- r3
;		sum r1+r2

		CODE F+,fplus
		xor a			; add
fop2:		; floating-point dyadic operation driver
		ld hl,ARG		;
		ld (hl),d		; TOS msb -> MSX ARG[0]
		inc hl			;
		ld (hl),e		; TOS lsb -> MSX ARG[1]
		inc hl			;
		pop de			; pop de with 2OS
		ld (hl),d		; 2OS msb -> MSX ARG[2]
		inc hl			;
		ld (hl),e		; 2OS lsb -> MSX ARG[3]
		pop de			;
fop1:		ld hl,DAC		;
		ld (hl),d		; TOS (or 3OS) msb -> MSX DAC[0]
		inc hl			;
		ld (hl),e		; TOS (or 3OS) lsb -> MSX DAC[1]
		inc hl			;
		pop de			; pop de with 2OS (or 4OS)
		ld (hl),d		; 2OS (or 4OS) msb -> MSX DAC[2]
		inc hl			;
		ld (hl),e		; 2OS (or 4OS) lsb -> MSX DAC[3]
		ld hl,0			;
		ld (ARG+4),hl		; clear MSX ARG upper to BCD 0000
		ld (ARG+6),hl		;
		ld (DAC+4),hl		; clear MSX DAC upper to BCD 0000
		ld (DAC+6),hl		;
		add a			;
		add <fop_tab		;
		ld l,a			; 2*a + lsb(fop_tab) -> l
		adc a,>fop_tab		;
		sub l			;
		ld h,a			; cf + msb(fop_tab) -> h
		ld a,(hl)		;
		inc hl			;
		ld h,(hl)		;
		ld l,a			; [hl] -> hl
		ld a,4			; 4 -> a
		ld (VALTYP),a		; MSX VALTYPE = 4 single precision
		push bc			; save bc with ip
		SVIXIY			; save ix and iy when necessary 
		call jphl		; call (hl) to execute DAC <fop> ARG -> DAC
fop_csng:	call FRCSNG		; convert MSX DAC to single precision when applicable
		LDIXIY			; restore ix and iy
		pop bc			; restore bc with ip
		ld hl,DAC+3		; MSX DAC+3 -> hl
		ld e,(hl)		; MSX DAC[3] -> 2OS lsb
		dec hl			;
		ld d,(hl)		; MSX DAC[2] -> 2OS msb
		dec hl			;
		push de			; push 2OS
		ld e,(hl)		; MSX DAC[1] -> TOS lsb
		dec hl			;
		ld d,(hl)		; MSX DAC[0] -> TOS msb
		JP_NEXT			; continue

;. F-		r1 r2 -- r3
;		difference r1-r2

		CODE F-,fminus
		ld a,1			; sub
		jr fop2			;

;. F*		r1 r2 -- r3
;		product r1*r2

		CODE F*,fstar
		ld a,2			; mul
		jr fop2			;

;. F/		r1 r2 -- r3
;		quotient r1/r2

		CODE F/,fslash
		ld a,3			; div
		jr fop2			;

;. F**		r1 r2 -- r3
;		raise r1 to r2

		CODE F**,fpow
		ld a,4			; pow
		jr fop2			;

;. FASIN	r1 -- r2
;		arc sine of float, in radian
;
;    : FASIN
;      2DUP F0= IF EXIT THEN
;      2DUP FABS 1E0 F= IF                           \ if |x|=1 then
;        PI/2 2SWAP F0< IF FNEGATE THEN              \ sign(x)*pi/2
;        EXIT
;      THEN
;      2DUP 2DUP F* 1E0 2SWAP F- FSQRT FATAN2 ;      \ arctan(x/sqrt(1-x^2)) = atan2(x,sqrt(1-x*x))

		COLON FASIN,fasin
		.dw twodup,fzeroequal,doif,1$
		.dw   doexit
1$:		.dw twodup,fabs,dotwolit,0x4110,0,fequal,doif,3$
		.dw   halfpi,twoswap,fzeroless,doif,2$
		.dw     fnegate
2$:		.dw   doexit
3$:		.dw twodup,twodup,fstar,dotwolit,0x4110,0,twoswap,fminus,fsqrt,fatan2
		.dw doret

;. FACOS	r1 -- r2
;		arc cosine of float, in radian
;
;    : FACOS FASIN PI/2 2SWAP F- ;

		COLON FACOS,facos
		.dw fasin,halfpi,twoswap,fminus
		.dw doret

;. FATAN2	r1 r2 -- r3
;		atan2(r1,r2) = atan(r1/r2) but using a more accurate formulation

;    : FATAN2
;      2DUP FNEGATE F0< IF
;        F/ FATAN
;        EXIT
;      THEN
;      2SWAP
;      2DUP F0= IF
;        2DROP F0< IF PI ELSE PI/2 THEN
;        EXIT
;      THEN
;      PI/2 2OVER F0< IF FNEGATE THEN
;      2ROT 2ROT F/ FATAN F- ;

		COLON FATAN2,fatan2
		.dw twodup,fnegate,fzeroless,doif,1$
		.dw   fslash,fatan
		.dw   doexit
1$:		.dw twoswap
		.dw twodup,fzeroequal,doif,3$
		.dw   twodrop,fzeroless,doif,2$
		.dw     pi
		.dw     doexit
2$:		.dw   halfpi
		.dw   doexit
3$:		.dw halfpi,twoover,fzeroless,doif,4$
		.dw   fnegate
4$:		.dw tworot,tworot,fslash,fatan,fminus
		.dw doret

;. PI		-- r
;		floating-point constant pi
;
;    3.14159E0 2CONSTANT PI

		TWOCONSTANT PI,pi
		.dw 0x4131,0x4159

;. PI/2		-- r
;		floating-point constant pi/2 (half pi)
;
;    1.57080E0 2CONSTANT PI/2

		TWOCONSTANT PI/2,halfpi
		.dw 0x4115,0x7080

;. FLN		r1 -- r2
;		natural log of float

		CODE FLN,fln
		ld a,14			; log
		jp fop1			;

;. FEXP		r1 -- r2
;		natural exponent of float

		CODE FEXP,fexp
		ld a,15			; exp
		jp fop1			;

;. FLOG		r1 -- r2
;		base 10 log of float
;
;    : FLOG FLN 0.434294E0 F* ;    \ = ln(x)/ln(10) approx ln(10) such that 10E0 FLOG = 1E0

		COLON FLOG,flog
		.dw fln,dotwolit,0x4043,0x4294,fstar
		.dw doret

;. FALOG	r1 -- r2
;		base 10 exponent of float
;
;    : FALOG 2.30259E0 F* FEXP ;    \ = exp(x*ln(10))

		COLON FALOG,falog
		.dw dotwolit,0x4123,0x0259,fstar,fexp
		.dw doret

;. FSINH	r1 -- r2
;		sine hyperbolicus of float
;
;    : FSINH FEXP 2DUP 1E0 2SWAP F/ F- .5E0 F* ;

		COLON FSINH,fsinh
		.dw fexp,twodup,dotwolit,0x4110,0,twoswap,fslash,fminus
		.dw dotwolit,0x4050,0,fstar
		.dw doret

;. FCOSH	r1 -- r2
;		cosine hyperbolicus of float
;
;    : FCOSH FEXP 2DUP 1E0 2SWAP F/ F+ .5E0 F* ;

		COLON FCOSH,fcosh
		.dw fexp,twodup,dotwolit,0x4110,0,twoswap,fslash,fplus
		.dw dotwolit,0x4050,0,fstar
		.dw doret

;. FTANH	r1 -- r2
;		tangent hyperbolicus of float
;
;    : FTANH 2DUP F+ FEXP 2DUP 1E0 F- 2SWAP 1E0 F+ F/ ;

		COLON FTANH,ftanh
		.dw twodup,fplus,fexp,twodup,dotwolit,0x4110,0,fminus
		.dw twoswap,dotwolit,0x4110,0,fplus,fslash
		.dw doret

;. FASINH	r1 -- r2
;		arc sine hyperbolicus of float
;
;    : FASINH 2DUP 2DUP F* 1E0 F+ FSQRT F+ FLN ;

		COLON FASINH,fasinh
		.dw twodup,twodup,fstar,dotwolit,0x4110,0,fplus
		.dw fsqrt,fplus,fln
		.dw doret

;. FACOSH	r1 -- r2
;		arc cosine hyperbolicus of float
;
;    : FACOSH 2DUP 2DUP F* 1E0 F- FSQRT F+ FLN ;

		COLON FACOSH,facosh
		.dw twodup,twodup,fstar,dotwolit,0x4110,0,fminus
		.dw fsqrt,fplus,fln
		.dw doret

;. FATANH	r1 -- r2
;		arc tangent hyperbolicus of float
;
;    : FATANH 2DUP 1E0 F+ 2SWAP 1E0 2SWAP F- F/ FLN .5E0 F* ;

		COLON FATANH,fatanh
		.dw twodup,dotwolit,0x4110,0,fplus,twoswap
		.dw dotwolit,0x4110,0,twoswap,fminus,fslash
		.dw fln,dotwolit,0x4050,0,fstar
		.dw doret

;. F=		r1 r2 -- flag
;		true if r1 = r2

		CODE F=,fequal
		jp dequal		; comparable as integer

;. F<		r1 r2 -- flag
;		true if r1 < r2
;
;    : F<
;      DUP 3 PICK AND 0< IF
;        2SWAP
;      D< ; ( works for MSX MATH-PACK decimals )

		COLON F<,fless
		.dw dup,three,pick,and,zeroless,doif,1$
		.dw   twoswap
1$:		.dw dless
		.dw doret

;. F0=		r -- flag
;		true if r = 0.0e0
;
;    : F0= D0= ; ( works for MSX MATH-PACK decimals )

		CODE F0=,fzeroequal
		jp dzeroequal		; comparable as integer

;. F0<		r -- flag
;		true if r < 0.0e0
;
;    : F0< D0< ; ( works for MSX MATH-PACK decimals )

		CODE F0<,fzeroless
		jp dzeroless		; comparable as integer

;. FMAX		r1 r2 -- r3
;		max of r1 and r2
;
;    : FMAX
;      2OVER 2OVER F< IF 2SWAP THEN
;      2DROP ;

		COLON FMAX,fmax
		.dw twoover,twoover,fless,doif,1$
		.dw   twoswap
1$:		.dw twodrop
		.dw doret

;. FMIN		r1 r2 -- r3
;		min of r1 and r2
;
;    : FMIN
;      2OVER 2OVER F< INVERT IF 2SWAP THEN
;      2DROP ;

		COLON FMIN,fmin
		.dw twoover,twoover,fless,invert,doif,1$
		.dw   twoswap
1$:		.dw twodrop
		.dw doret

;. D>F		d -- r
;		widen signed double to float;
;		this word is much slower than the optimized S>F
;
;    : D>F
;      BASE @ -ROT
;      DECIMAL
;      TUCK DABS <# #S ROT SIGN #> >FLOAT DROP
;      ROT BASE ! ;

		COLON D>F,dtof
		.dw base,fetch,mrot
		.dw decimal
		.dw tuck,dabs,lesshash,hashs,rot,sign,hashmore,tofloat,drop
		.dw rot,base,store
		.dw doret

;. S>F		n -- r
;		widen signed single to float
;
;    : S>F S>D D>F ;

		CODE S>F,stof
		ld a,2			;
		ld (VALTYP),a		; 2 -> [VALTYP] integer op
		ld (DAC+2),de		; de -> DAC integer part
		push bc			; save bc with ip
		SVIXIY			; save ix and iy when necessary 
		jp fop_csng		; convert to single precision, pop bc and continue

;. F>D		r -- d
;		narrow float to a signed double;
;		may throw -11 "result out of range";
;		this word is much slower than the optimized F>S
;
;    : F>D
;      HERE 1+ 10 REPRESENT DROP SWAP
;      DUP 0< IF
;        2DROP 0.
;        EXIT
;      THEN
;      DUP 10 > IF
;        -11 THROW
;      THEN
;      '- HERE C!       \ place '-' in here
;      OVER -           \ sign exp-sign
;      SWAP HERE 1+ +   \ exp-sign here+1+sign
;      SWAP             \ here+1+sign exp-sign
;      >DOUBLE 0= IF
;        -11 THROW
;      THEN ;

		COLON F>D,ftod
		.dw here,oneplus,dolit,10,represent,drop,swap
		.dw dup,zeroless,doif,1$
		.dw   twodrop,zero,zero
		.dw   doexit
1$:		.dw dup,dolit,10,more,doif,2$
		.dw   dolit,-11,throw
2$:		.dw dolit,'-,here,cstore
		.dw over,minus
		.dw swap,here,oneplus,plus
		.dw swap
		.dw todouble,zeroequal,doif,3$
		.dw   dolit,-11,throw
3$:		.dw doret

;. F>S		r -- n
;		narrow float to a signed single;
;		may throw -11 "result out of range" or -250 "numeric overflow"
;
;    : F>S F>D D>S ;

		CODE F>S,ftos
		ld hl,DAC		;
		ld (hl),d		; TOS (or 3OS) msb -> MSX DAC[0]
		inc hl			;
		ld (hl),e		; TOS (or 3OS) lsb -> MSX DAC[1]
		inc hl			;
		pop de			; pop de with 2OS (or 4OS)
		ld (hl),d		; 2OS (or 4OS) msb -> MSX DAC[2]
		inc hl			;
		ld (hl),e		; 2OS (or 4OS) lsb -> MSX DAC[3]
		ld a,4			;
		ld (VALTYP),a		; 4 -> [VALTYP]
		push bc			; save bc with ip
		call FRCINT		; convert to integer
		pop bc			; restore bc with ip
		ld de,(DAC+2)		; set new TOS to de with converted integer
		JP_NEXT			; continue

;. >FLOAT	c-addr u -- r true | false
;		convert string to float;
;		leaves the float and true if string is converted;
;		leaves false if string is unconvertable;

		CODE >FLOAT,tofloat
		ld a,e			; TOS lsb u -> a
		pop hl			; 2OS c-addr -> hl
		push bc			; save bc with ip
	        ld de,FBUFFR		; MSX FBUFFR -> de to copy string to
		push de			; save de
		ld c,a			; TOS lsb u -> bc
		ld b,0			;
		or a			;
		jr z,1$			; if u != 0 then
		ldir			;   loop [hl++] -> [de++] until --bc = 0
1$:		xor a			;
		ld (de),a		; 0 -> [de] terminate string in hold area
	        pop hl			; MSX FBUFFR -> hl to copy string to
		call FIN		; MSX FIN convert string to MSX DAC
		push hl			; save updated hl
		call FRCSNG		; convert to single precision
		pop hl			; restore updated hl
		pop bc			; restore bc with ip
		ld a,(hl)		;
		or a			;
		jp nz,false_next	; if [hl] != 0 then set new TOS to false and continue
		ld hl,DAC+3		; MSX DAC+3 -> hl
		ld e,(hl)		; MSX DAC[3] -> 3OS lsb
		dec hl			;
		ld d,(hl)		; MSX DAC[2] -> 3OS msb
		dec hl			;
		push de			; push 3OS
		ld e,(hl)		; MSX DAC[1] -> 2OS lsb
		dec hl			;
		ld d,(hl)		; MSX DAC[0] -> 2OS msb
		jp true			; push 2OS and new TOS true and continue

;. REPRESENT	r c-addr u -- n flag true
;		convert float to string;
;		store decimal digits of the float in buffer c-addr with size u > 0;
;		leaves decimal exponent n+1 and flag = true if negative

		CODE REPRESENT,represent
		DINT			; disable interrupts before using auxiliary registers
		pop hl			; pop c-addr -> hl
		exx			; save bc with ip, save TOS in de and 2OS in hl
		; copy float r to MSX DAC
		ld hl,DAC		; MSX DAC -> hl'
		pop de			; pop 3OS -> de'
		ld (hl),d		; 3OS msb -> MSX DAC[0]
		inc hl			;
		ld (hl),e		; 3OS lsb -> MSX DAC[1]
		inc hl			;
		pop de			; pop 4OS -> de'
		ld (hl),d		; 4OS msb -> MSX DAC[2]
		inc hl			;
		ld (hl),e		; 4OS lsb -> MSX DAC[3]
		exx
		push hl
		push de
		push bc
		; convert to formatted scientific decimal float with exponenent
		ld a,4
		ld (VALTYP),a
		ld a,0x8f		; formatted floating point with flag bits 0,1,2,3,7 set
		ld bc,0x0007		; 7 means dp followed by 6 digits
		call PUFOUT		; convert MSX DAC to string -> hl
		pop bc			; restore bc with ip
		push hl			; push hl with FBUFFR
		exx			;
		pop hl			; pop FBUFFR -> hl'
		pop bc			; u -> bc'
		pop de			; c-addr -> de'
		inc hl			; skip over leading dp
		; copy converted string [hl'..hl'+10] to c-addr [de'..de'+bc'-1]
		ld a,'E			;
2$:		cp (hl)			; loop
		jr z,4$			;   if [hl'] = 'E' then goto 4
		ldi			;   [hl'++] -> [de'++]
		jp pe,2$		; until --bc' = 0
		; ignore remaining digits in [hl'..] until 'E'
3$:		cp (hl)			; loop
		inc hl			;
		jr nz,3$		; until [hl'++] != 'E'
		jr 6$			; goto 6
		; fill the rest of c-addr buffer with zeros
4$:		ld (hl),'0		; '0' -> [hl']
5$:		ldi			; [hl'] -> [de'++]
		dec hl			;
		jp pe,5$		; until --bc' = 0
		inc hl			; hl'++
		; [hl] = +/- exponent sign after 'E'
6$:		ld b,(hl)		; save sign +/- of exponent [hl'++] -> b
		inc hl			;
		ld a,(hl)		; first digit of the exponent [hl'++] -> a
		inc hl			;
		sub '0			; digit - '0' -> a
		add a			;
		ld c,a			;
		add a			;
		add a			;
		add c			; 10 * (digit - '0') -> a
		add (hl)		; second digit of the exponent
		sub '0			; converted decimal exponent -> a
		ld e,a			; a -> de' with unsigned exponent
		ld d,0
		ld a,b			; b' -> a with exponent sign
		cp '-			;
		jr nz,7$		; if exponent sign is - then
		xor a			;
		sub e			;
		ld e,a			;   -e' -> e'
		dec d			;   sign extend to d'
7$:		push de			; push new 3OS de' with exponent n
		inc hl
		; determine sign flag from [hl] = +/-
		ld a,'+
		cp (hl)
		sbc hl,hl
		push hl			; push new 2OS with sign flag 0 or -1
		exx			; restore bc with ip
		EINT			; enable interrupts after using auxiliary registers
		jp true_next		; set new TOS true and continue

;. PRECISION	-- +n
;		floating-point output precision, the number of decimal digits displayed is 6 by default
;
;    6 VALUE PRECISION

		VALUE PRECISION,precision
		.dw 6

;. FS.		r --
;		output float in scientific notation with a trailing space
;
;    : FS.
;      HERE PRECISION REPRESENT DROP IF
;        '- EMIT
;      THEN
;      HERE C@ DUP EMIT
;      '0 <> +
;      '. HERE C!
;      HERE PRECISION '0 -TRIM TYPE
;      'E EMIT . ;

		COLON FS.,fsdot
		.dw here,precision,represent,drop,doif,1$
		.dw   dolit,'-,emit
1$:		.dw here,cfetch,dup,emit
		.dw dolit,'0,notequal,plus
		.dw dolit,'.,here,cstore
		.dw here,precision,dolit,'0,mtrim,type
		.dw dolit,'E,emit,dot
		.dw doret

;. F.		r --
;		output float with a trailing space;
;		output fixed notation when 1e-1 <= |r| < 1e+7, otherwise output scientific notation;
;		beware that non-scientific output cannot be copy-pasted back into input,
;		as floating-point literals require an exponent
;
;    : F.
;      HERE PRECISION REPRESENT DROP IF
;        '- EMIT
;      THEN
;      DUP 0 PRECISION 1+ WITHIN IF
;        DUP IF
;          HERE OVER TYPE
;        ELSE
;          '0 EMIT
;        THEN
;        '. EMIT
;        HERE OVER +
;        PRECISION ROT - '0 -TRIM TYPE SPACE
;        EXIT
;      THEN
;      HERE C@ EMIT
;      '. HERE C!
;      HERE PRECISION '0 -TRIM TYPE
;      'E EMIT 1- . ;

		COLON F.,fdot
		.dw here,precision,represent,drop,doif,1$
		.dw   dolit,'-,emit
1$:		.dw dup,zero,precision,oneplus,within,doif,4$
		.dw   dup,doif,2$
		.dw     here,over,type
		.dw   doahead,3$
2$:		.dw     dolit,'0,emit
3$:		.dw   dolit,'.,emit
		.dw   here,over,plus
		.dw   precision,rot,minus,dolit,'0,mtrim,type,space
		.dw   doexit
4$:		.dw here,cfetch,emit
		.dw dolit,'.,here,cstore
		.dw here,precision,dolit,'0,mtrim,type
		.dw dolit,'E,emit,oneminus,dot
		.dw doret

.endif;MATH

;-------------------------------------------------------------------------------
;
;		FILE ACCESS
;
;-------------------------------------------------------------------------------

.if FCBN ; MSX-DOS https://www.msx.org/wiki/FCB https://map.grauw.nl/resources/dos2_functioncalls.php

BDOS		.equ 0xf37d		; Disk BASIC ROMBDOS
PHYDIO		.equ 0xffa7		; PHYDIO hook [PHYDIO] != 0xc9 when Disk BASIC is available

;/ DRV		-- c-addr
;		last used drive letter, the default drive when none is specified explicitly, initially drive A

		VALUE DRV,drv
		.dw 'A

;/ FCX		-- addr
;		array of FCB+FIB per open file
;
;    CREATE FCX 37 FCBN * ALLOT

		VARIABLE FCX,fcx
		.ds (37+fib_size)*FCBN	; reserve one FCB+FIB per open file

;/ (FCB)	-- addr
;		allocate a new FCB;
;		may throw -204 "bad file number" when max files are in use

		CODE (FCB),dofcb
		push de			; save TOS
		push bc			; save bc with ip
		ld b,FCBN		; number of FCB -> b
		ld hl,fcx+3		; file control blocks base address -> hl
		ld de,37+fib_size	; 37 + fib_size -> de
		xor a			;
1$:		cp (hl)			; loop
		jr z,2$			;   if [hl] == 0 goto 2
		add hl,de		;   hl + 37 + fib_size -> hl
		djnz 1$			; until --b == 0
		pop bc			; restore bc with ip
		ld a,-204		;
		jp throw_a		; throw -204 "bad file number"
2$:		pop bc			; restore bc with ip
		ex de,hl		;
		JP_NEXT			; continue

;/ S>FCB	c-addr u -- addr
;		store the filename string of the form [D:]FILENAME[.EXT] in a new FCB;
;		wildcard '?' matches any character when used in FILENAME and EXT;
;		wildcard '*' matches any sequence of characters, at most one '*' may be used in FILENAME and in EXT;
;		may throw -204 "bad file number" when max files are in use
;
;    : S>FCB
;      \ drive letter or use default drive
;      S" :" SEARCH IF
;        OVER 1- C@ TO DRV
;        1 /STRING
;      THEN
;      (FCB)
;      DUP 37 ERASE
;      DUP 12 BLANK 
;      \ c-addr u fcb-addr
;      \ set drive number
;      DRV $1f AND OVER C!
;      \ set filename, expand * into ???...
;      -ROT
;      \ fcb-addr c-addr u
;      8 0 ?DO
;        DUP 0= ?LEAVE
;        OVER C@ '. = ?LEAVE
;        OVER C@ '* = IF
;          '?
;        ELSE
;          NEXT-CHAR
;        THEN
;        3 PICK 1+ I + C!
;      LOOP
;      \ set extension, expand * into ???...
;      S" ." SEARCH IF
;        1 /STRING
;        3 0 ?DO
;          DUP 0= ?LEAVE
;          OVER C@ '* = IF
;            '?
;          ELSE
;            NEXT-CHAR
;          THEN
;          3 PICK 9 + I + C!
;        LOOP
;      THEN
;      2DROP ;

		COLON S>FCB,stofcb
		    SLIT ^|:|
		.dw search,doif,1$
		.dw   over,oneminus,cfetch,dolit,0x5f,and,doto,drv+3
		.dw   one,slashstring
1$:		.dw dofcb
		.dw dup,dolit,37,erase
		.dw dup,dolit,12,blank
		.dw drv,dolit,0x1f,and,over,cstore
		.dw mrot
		.dw dolit,8,zero,doqdo,5$
2$:		.dw   dup,zeroequal,doqleave
		.dw   over,cfetch,dolit,'.,equal,doqleave
		.dw   over,cfetch,dolit,'*,equal,doif,3$
		.dw     dolit,'?
		.dw   doahead,4$
3$:		.dw     nextchar
4$:		.dw   three,pick,oneplus,i,plus,cstore
		.dw doloop,2$
5$:		    SLIT ^|.|
		.dw search,doif,9$
		.dw   one,slashstring
		.dw   three,zero,doqdo,9$
6$:		.dw     dup,zeroequal,doqleave
		.dw     over,cfetch,dolit,'*,equal,doif,7$
		.dw       dolit,'?
		.dw     doahead,8$
7$:		.dw       nextchar
8$:		.dw     three,pick,dolit,9,plus,i,plus,cstore
		.dw   doloop,6$
9$:		.dw twodrop
		.dw doret

;/ FIB		fileid -- c-addr u
;		the file input buffer of size u associated with fileid;
;		used by INCLUDE-FILE, INCLUDE, INCLUDED, otherwise free to use

		CODE FIB,fib
		ld hl,37		;
		add hl,de		;
		push hl			; set new 2OS to fcb-addr + 37
		ld de,fib_size		; set new TOS to fib_size
		JP_NEXT			;

;/ (BDOS)	addr u1 n -- u2 u3 0|1..255
;		execute CP/M style MSX-DOS command C=n with DE=addr and HL=u1;
;		leaves u2=HL and u3=DE returned and register A for 0 (success) or 1 to 255 (failure)

		CODE (BDOS),dobdos
		pop hl			; u1 -> hl
		ld a,(PHYDIO)		; [PHYDIO] -> a
		cp 0xc9			;
		jr z,1$			; if [PHYDIO] = 0xc9 then exit with 0xc9 code
		ld a,e			; n -> a
		pop de			; addr -> de
		push bc			; save bc with ip
		SVIXIY			; save ix and iy when necessary
		ld c,a			; u -> c
		call BDOS		; MSX BDOS
		LDIXIY			; restore ix and iy
		pop bc			; restore bc with ip
		push hl			; save 3OS with u2=HL
1$:		push de			; save 2OS with u3=DE
		ld e,a			;
		ld d,0			; set new TOS to A
		JP_NEXT			; continue

;/ (DOSX)	addr u1 n -- u2 ior
;		execute CP/M style MSX-DOS command C=n with DE=addr and HL=u1;
;		leaves u2=HL returned and 0 (success) or nz (failure);
;		catches BDOS exceptions and then leaves ior = 1 (failure)

		COLON (DOSX),dodosx
		.dw dolit,dobdos,catch,doif,1$
		.dw   drop,one
1$:		.dw nip
		.dw doret

;/ BIN		fam -- fam
;		CREATE-FILE and OPEN-FILE mode fam;
;		note: files are always treaded as binary

		CODE BIN,bin
		JP_NEXT

;/ W/O		-- fam
;		CREATE-FILE and OPEN-FILE mode fam

		CONSTANT W/O,wslasho
		.dw 0

;/ R/O		-- fam
;		CREATE-FILE and OPEN-FILE mode fam

		CONSTANT R/O,rslasho
		.dw 1

;/ R/W		-- fam
;		CREATE-FILE and OPEN-FILE mode fam

		CONSTANT R/W,rslashw
		.dw 0

;/ CREATE-FILE	c-addr u fam -- fileid ior
;		create a new file given by the filename string c-addr u, where fam is R/W, R/O or W/O;
;		if the file already exists, then it is truncated to zero length;
;		leaves fileid (a fcb-addr) and ior 0 (success) or -203 (failure)

		COLON CREATE-FILE,createfile
		.dw mrot,stofcb
		.dw tuck,dolit,13,plus,cstore		; set 1 for R/O or leave 0
		.dw dup,zero,dolit,0x16,dodosx,nip	; MSX BDOS 16h
		.dw over,one,swap,dolit,14,plus,store	; set record size to 1
		.dw dup,doif,1$
		.dw   drop,dup,off,dolit,-203
1$:		.dw doret

;/ OPEN-FILE	c-addr u fam -- fileid ior
;		open a file or device given by the filename string c-addr u, where fam is R/W, R/O or W/O;
;		leaves fileid (a fcb-addr) and ior 0 (success) or -203 (failure);
;		filename format: [D:]FILENAME[.EXT] where D: becomes the default drive when specified;
;		device names are AUX, CON, LST, NUL, and PRN without a drive specified

		COLON OPEN-FILE,openfile
		.dw mrot,stofcb
		.dw tuck,dolit,13,plus,cstore		; set 1 for R/O or leave 0
		.dw dup,zero,dolit,0x0f,dodosx,nip	; MSX BDOS 15h
		.dw over,one,swap,dolit,14,plus,store	; set record size to 1
		.dw dup,doif,1$
		.dw   drop,dup,off,dolit,-203
1$:		.dw doret

;/ CLOSE-FILE	fileid -- ior
;		close file with fileid (a fcb-addr);
;		leaves ior 0 (success) or -197 (failure)

		COLON CLOSE-FILE,closefile
		.dw dup,zero,dolit,0x10,dodosx,nip	; MSX BDOS 10h
		.dw over,sourceid,equal,doif,1$
		.dw   zero,doto,sourceid+3		; if fileid = SOURCE-ID then reset SOURCE-ID to 0
1$:		.dw over,outputid,equal,doif,2$
		.dw   zero,doto,outputid+3		; if fileid = OUTPUT-ID then reset OUTPUT-ID to 0
2$:		.dw swap,off
		.dw dup,doif,3$
		.dw   drop,dolit,-197
3$:		.dw doret

;/ CLOSE-FILES	--
;		close all open files

		COLON CLOSE-FILES,closefiles
		.dw dolit,FCBN,zero,dodo,3$
1$:		.dw   i,dolit,37+fib_size,star,fcx,plus,dup,fetch,doif,2$
		.dw     closefile
2$:		.dw   drop
		.dw doloop,1$
3$:		.dw doret

;/ READ-FILE	c-addr u1 fileid -- u2 ior
;		read into buffer c-addr of size u1 from fileid (a fcb-addr);
;		leaves number u2 of bytes read into the buffer and ior 0 (success) or 1 (u2 is zero and eof) or nz (other failure)
;		to read a single character to a cell on the stack: 0 SP@ 1 fileid READ-FILE -- char 0|1 ior
;

		COLON READ-FILE,readfile
		.dw rot,zero,dolit,0x1a,dodosx,twodrop	; MSX BDOS 1ah set DTA to c-addr
		.dw swap,dolit,0x27,dodosx		; MSX BDOS 27h
		.dw over,zeroequal,and			; (u2 == 0) & ior -> ior
		.dw doret

;/ READ-LINE	c-addr u1 fileid -- u2 flag ior
;		read a line into buffer c-addr of size u1 from fileid;
;		leaves number u2 of bytes read into the buffer (without terminating CR/LF) and flag TRUE;
;		leaves flag FALSE when u2 is zero and EOF is reached;
;		ior is nonzero when an error occurred, use READ-LINE 0= AND 0= which is TRUE for EOF or error
;
;    : READ-LINE
;      DUP FILE-POSITION DROP   \ c-addr u1 fileid ud
;      2>R                      \ c-addr u1 fileid
;      2>R R@ SWAP DUP 2R>      \ fileid c-addr c-addr u1 fileid
;      READ-FILE IF             \ fileid c-addr u2 ior
;        2DROP DROP RDROP RDROP
;        0 FALSE 0
;        EXIT
;      THEN                     \ fileid c-addr u2
;      10 CHOP                  \ fileid c-addr u3
;      DUP 1+                   \ fileid c-addr u3 u3+1
;      2R> ROT M+               \ fileid c-addr u3 ud+u3+1
;      2>R                      \ fileid c-addr u3
;      ROT                      \ c-addr u3 fileid
;      2R> ROT                  \ c-addr u3 ud+u3+1 fileid
;      REPOSITION-FILE DROP     \ c-addr u3
;      13 -TRIM                 \ c-addr u4
;      NIP TRUE 0 ;

		COLON READ-LINE,readline
		.dw dup,fileposition,drop
		.dw twotor
		.dw twotor,rfetch,swap,dup,tworfrom
		.dw readfile,doif,1$
		.dw   twodrop,drop,rdrop,rdrop
		.dw   zero,false,zero
		.dw   doexit
1$:		.dw dolit,10,chop
		.dw dup,oneplus
		.dw tworfrom,rot,mplus
		.dw twotor
		.dw rot
		.dw tworfrom,rot
		.dw repositionfile,drop
		.dw dolit,13,mtrim
		.dw nip,true,zero
		.dw doret

;/ WRITE-FILE	c-addr u1 fileid -- ior
;		write buffer c-addr of size u1 to fileid (a fcb-addr);
;		leaves ior 0 (success) or 1 (disk full) or nz (other failure)

		COLON WRITE-FILE,writefile
		.dw over,doif,1$			; if u1 != 0 then
		.dw   rot,zero,dolit,0x1a,dodosx,twodrop; MSX BDOS 1ah set DTA to c-addr
		.dw   swap,dolit,0x26,dodosx,nip	; MSX BDOS 26h
		.dw   doexit
1$:		.dw drop,twodrop,zero
		.dw doret

;/ WRITE-LINE	c-addr u1 fileid -- ior
;		write buffer c-addr of size u1 to fileid (a fcb-addr) followed by a CR/LF pair;
;		leaves ior 0 (success) or 1 (disk full)

		COLON WRITE-LINE,writeline
		.dw duptor,writefile,qdup,doif,1$
		.dw   rdrop
		.dw   doexit
		; SLIT ^|\r\n| crashes the assembler so manual inline
1$:		.dw doslit
		.db 2
		.str ^|\r\n|
		.dw rfrom,writefile
		.dw doret

;/ FILE-POSITION	fileid -- ud ior
;		the fileid (a fcb-addr);
;		leaves file position ud and ior (always 0 for success)

		COLON FILE-POSITION,fileposition
		.dw dolit,33,plus,twofetch,swap,zero
		.dw doret

;/ REPOSITION-FILE	ud fileid -- ior
;		for the open fileid (a fcb-addr) set the file position to ud;
;		leaves ior (always 0 for success)

		COLON REPOSITION-FILE,repositionfile
		.dw dolit,33,plus,mrot,swap,rot,twostore,zero
		.dw doret

;/ FILE-SIZE	fileid -- ud ior
;		the fileid (a fcb-addr);
;		leaves file size ud and ior (always 0 for success)

		COLON FILE-SIZE,filesize
		.dw dolit,16,plus,twofetch,swap,zero
		.dw doret

;/ RESIZE-FILE	ud fileid -- ior
;		the fileid (a fcb-addr);
;		leaves ior 0 (success) or 1 (disk full) or nz (other failure)

		COLON RESIZE-FILE,resizefile
		.dw duptor,repositionfile,drop
		.dw rfrom,zero,dolit,0x26,dodosx,nip	; MSX BDOS 26h write zero bytes
		.dw doret

;/ DELETE-FILE	c-addr u -- ior
;		delete the file with the name specified by the string c-addr u;
;		leaves ior 0 (success) or nz (failure)

		COLON DELETE-FILE,deletefile
		.dw stofcb
		.dw dup,zero,dolit,0x13,dodosx,nip
		.dw swap,off
		.dw doret

;/ RENAME-FILE	c-addr1 u1 c-addr2 u2 -- ior
;		rename the file with the name specified by the string c-addr1 u1 to c-addr2 u2;
;		leaves ior 0 (success) or nz (failure)

		COLON RENAME-FILE,renamefile
		.dw stofcb
		.dw dup,dup,fib,drop,dolit,12,cmove
		.dw off
		.dw stofcb,dup,fib,drop,over,dolit,16,plus,dolit,12,cmove
		.dw dup,zero,dolit,0x17,dodosx,nip
		.dw swap,off
		.dw doret

.if REPL

;/ INCLUDE-FILE	... fileid -- ...
;		read and interpret Forth source code from fileid;
;		fileid is closed afterwards
;
;    : INCLUDE-FILE
;      SAVE-INPUT N>R
;      TO SOURCE-ID
;      BEGIN 0 REFILL WHILE
;        DROP
;        ['] INTERPRET CATCH
;        ?DUP 0= WHILE
;      REPEAT THEN
;      SOURCE-ID CLOSE-FILE DROP
;      DUP ERROR
;      NR> RESTORE-INPUT DROP
;      THROW ;

		COLON INCLUDE-FILE,includefile
		.dw saveinput,ntor
		.dw doto,sourceid+3
1$:		.dw zero,refill,doif,2$
		.dw   drop
		.dw   dolit,interpret,catch
		.dw   qdup,zeroequal,doif,2$
		.dw doagain,1$
2$:		.dw sourceid,closefile,drop
		.dw dup,error
		.dw nrfrom,restoreinput,drop
		.dw throw
		.dw doret

;/ INCLUDED	... c-addr u -- ...
;		read and interpret Forth source code from the file named by the string c-addr u
;
;    : INCLUDED
;      R/O OPEN-FILE THROW
;      INCLUDE-FILE ;

		COLON INCLUDED,included
		.dw rslasho,openfile,throw
		.dw includefile
		.dw doret

;/ INCLUDE	... "<spaces>name<space>" -- ...
;		read and interpret Forth source code from file "name"
;
;    : INCLUDE PARSE-NAME INCLUDED ;

		COLON INCLUDE,include
		.dw parsename,included
		.dw doret

;/ REQUIRED	... c-addr u -- ...
;		read and interpret Forth source code from the file named by the string c-addr u,
;		if the file was not already included;
;		this also adds file name as a MARKER with a leading '~' to the dictionary;
;		beware of vocabulary definitions crossings
;		(other vocabulary DEFINITIONS after markers also get deleted and corrupt their vocabulary)
;
;    : REQUIRED
;      SDUP -1 /STRING OVER '~ SWAP C!
;      2DUP FIND-WORD NIP IF
;        2DROP
;        EXIT
;      THEN
;      CURRENT DUP @ 2>R HERE >R
;      2DUP NFA, marker_does CFA,  \ create marker with marker DOES> cfa
;      R> , 2R> 2,                 \ marker body stores HERE CURRENT CURRENT@
;      1 /STRING INCLUDED ;

		COLON REQUIRED,required
		.dw sdup,mone,slashstring,over,dolit,'~,swap,cstore
		.dw twodup,findword,nip,doif,1$
		.dw   twodrop
		.dw   doexit
1$:		.dw current,dup,fetch,twotor,here,tor
		.dw twodup,nfacomma,dolit,marker_does,cfacomma
		.dw rfrom,comma,tworfrom,twocomma
		.dw one,slashstring,included
		.dw doret

;/ REQUIRE	... "<spaces>name<space>" -- ...
;		read and interpret Forth source code from file "name",
;		if the file was not already included;
;		this also adds file name with a leading '~' to the dictionary to assert inclusion;
;		beware of vocabulary definitions crossings
;		(other vocabulary DEFINITIONS after markers also get deleted and corrupt their vocabulary)
;
;    : REQUIRE PARSE-NAME REQUIRED ;

		COLON REQUIRE,require
		.dw parsename,required
		.dw doret

;/ ANEW		... "<spaces>name<space>" -- ...
;		read and interpret Forth source code from file "name",
;		same as REQUIRE,
;		but anew by deleting all previously included definitions from the dictionary first
;
;    : ANEW
;      PARSE-NAME SDUP -1 /STRING OVER '~ SWAP C!
;      2DUP FIND-WORD IF
;        EXECUTE
;      ELSE
;        DROP
;      THEN
;      1 /STRING REQUIRED ;

		COLON ANEW,anew
		.dw parsename,sdup,mone,slashstring,over,dolit,'~,swap,cstore
		.dw twodup,findword,doif,1$
		.dw   execute
		.dw doahead,2$
1$:		.dw   drop
2$:		.dw one,slashstring,required
		.dw doret

.endif;REPL
.endif;FCBN

;-------------------------------------------------------------------------------
;
;		STRING AND MEMORY OPERATIONS
;
;-------------------------------------------------------------------------------

; COUNT		c-addr1 -- c-addr2 u
;		convert counted string to string
;
;    : COUNT DUP 1+ SWAP C@ ;

		CODE COUNT,count
		ld a,(de)		; fetch length
		inc de			; increment TOS
		push de			; save TOS as new 2OS
		ld e,a			; set a -> de new TOS
		ld d,0			;
		JP_NEXT			; continue

; COMPARE	c-addr1 u1 c-addr2 u2 -- -1|0|1
;		compare strings, leaves -1 = less or 0 = equal or 1 = greater

.if AUXR

		CODE COMPARE,compare
		push de			; save TOS
		exx			; save bc with ip
		pop bc			; pop u2 -> bc
		pop de			; pop c-addr2 -> de
		pop hl			; u1 -> hl
		push hl			; keep u1 on the stack
		xor a			; 0 -> a means u1 = u2, 0 -> cf
		sbc hl,bc		;
		jr nc,1$		;   if u1 < u2 then
		pop bc			;     pop u1 -> bc
		push bc			;     rebalance stack
1$:		pop hl			; pop to discard u1
		pop hl			; pop c-addr1 -> hl
		ex af,af'		; save a with -1|0|1 flag
		ld a,c			;
		or b			;
		jr z,3$			; if bc <> 0 then
		; compare chars
2$:		ld a,(de)	;  7	;   loop
		cpi		; 16	;     compare [hl++] to [de], --bc
		jr nz,5$	;  7	;     while characters [hl] and [de] are equal
		inc de		;  6	;     de++
		jp pe,2$	; 10(46);   until bc = 0
		; chars match, check lengths
3$:		ex af,af'		; restore a with -1|0|1 flag
4$:		exx			; restore bc with ip
		jp z,zero_next		; set TOS to 0 if equal
		jp c,mone_next		; set TOS to -1 if less
		jp one_next		; set TOS to 1 if greater
		; strings differ
5$:		dec hl			; hl-- to correct cpi overshoot
		cp (hl)			; test a < [hl]
		ccf			; complement cf, cf = 1 if [hl] < a
		jr 4$			;

.else

		CODE COMPARE,compare
		push de			; save TOS
		SVBC			; save bc with ip
		pop bc			; pop u2 -> bc
		pop de			; pop c-addr2 -> de
		pop hl			; u1 -> hl
		push hl			; keep u1 on the stack
		xor a			; 0 -> a
		sbc hl,bc		; compare u1 with u2 and set flags
		jr nc,1$		; if u1 < u2 then
		pop bc			;   pop u1 -> bc
		push bc			;   rebalance stack
1$:		pop hl			; pop to discard u1
		pop hl			; pop c-addr1 -> hl
		push af			; save a and flags
		ld a,c			;
		or b			;
		jr z,3$			; if bc <> 0 then
		; compare chars
2$:		ld a,(de)	;  7	;   loop
		cpi		; 16	;     compare [hl++] to [de], --bc
		jr nz,5$	;  7	;     while characters [hl] and [de] are equal
		inc de		;  6	;     de++
		jp pe,2$	; 10(46);   until bc = 0
		; chars match, check lengths
3$:		pop af			; restore a and length comparison flags
4$:		LDBC			; restore bc with ip
		jp z,zero_next		; set TOS to 0 if equal
		jp c,mone_next		; set TOS to -1 if less
		jp one_next		; set TOS to 1 if greater
		; strings differ
5$:		pop de			; discard af
		dec hl			; hl-- to correct cpi overshoot
		cp (hl)			; retest a < [hl]
		ccf			; complement cf, cf = 1 if [hl] < a
		jr 4$			;

.endif

; SEARCH	c-addr1 u1 c-addr2 u2 -- c-addr3 u3 flag
;		true if the first string contains the second string;
;		leaves matching address, remaining length, and TRUE;
;		or leaves the first string and FALSE

		CODE SEARCH,search
		push de			; save TOS
		EXXSVBC			; exx or save bc with ip
		pop bc			; pop u2 -> bc
		pop de			; pop c-addr2 -> de
		pop hl			; pop u1 -> hl
		or a			; 0 -> cf
		sbc hl,bc		; u1 - u2 -> hl
		jr c,5$			; if u2 > u1 then impossible search
		push bc			;
		pop iy			; u2 -> iy
		ld a,c			;
		or b			;
		ld c,l			;
		ld b,h			; u1 - u2 -> bc
		jr z,4$			; if u2 = 0 then found
		inc bc			; u1 - u2 + 1 -> bc correct for cpir
		pop hl			;
		push hl			; c-addr1 -> hl, keep c-addr1 on the stack
		; find char match
1$:		ld a,c			; loop
		or b			;
		jr z,6$			;   if bc = 0 then not found
		ld a,(de)		;   [de] -> a
		cpir		; 21/16	;   repeat until a = [hl++] or --bc = 0
		jr nz,6$		;   if no match then not found
		push bc			;   save bc with remaining u1-k chars to search
		push de			;   save de with c-addr2
		push hl			;   save hl with c-addr1+k search position
		push iy			;
		pop bc			;   u2 -> bc
		; compare substrings
		dec bc			;   u2 - 1 -> bc since u2 > 0
		ld a,c			;
		or b			;
		jr z,3$			;   if bc <> 0 then
		inc de			;     de++ to start matching at c-addr2 + 1
2$:		ld a,(de)	;  7	;     loop
		cpi		; 16	;       compare [hl++] to [de], --bc
		jr nz,3$	;  7	;       while characters [hl] and [de] are equal
		inc de		;  6	;       de++
		jp pe,2$	; 10(46);     until bc = 0
3$:		pop hl			;   restore hl with c-addr1+k search position
		pop de			;   restore de with c-addr2
		pop bc			;   restore bc with remaining u1-k chars to search
		jr nz,1$		; until substring match
		; substrings match
		dec hl			; hl-- to correct cpir overshoot
		ex (sp),hl		; save hl with c-addr3, discard c-addr1
4$:		add iy,bc		; compute u3 = u2 + bc
		push iy			; save iy with u3 as new 2OS
		EXXLDBC			; exx or restore bc with ip
.if JPIY
		ld iy,next		; restore iy
.endif
		jp true_next		; set new TOS to TRUE
		; impossible search
5$:		add hl,bc		; u1 - u2 + u2 -> hl = u1
		jr 7$			;
		; not found
6$:		push iy			;
		pop bc			;
		dec bc			; u2 - 1 -> bc
		add hl,bc		; c-addr1 + u2 - 1 -> hl
		pop de			;
		push de			; c-addr1 -> de, keep c-addr1 as 3OS
		sbc hl,de		; c-addr1 + u1 - de -> hl = u1
7$:		push hl			; save hl with u1 as 2OS
		EXXLDBC			; exx or restore bc with ip
.if JPIY
		ld iy,next		; restore iy
.endif
		jp false_next		; set new TOS to FALSE

; CMOVE		c-addr1 c-addr2 u --
;		move u bytes from c-addr1 to c-addr2 (from begin going up)
;
;    : CMOVE
;      SWAP >R
;      BEGIN DUP WHILE
;        NEXT-CHAR R@ C!
;        R> 1+ >R
;      REPEAT
;      RDROP
;      2DROP ;

		CODE CMOVE,cmove
		push de			; save TOS
		EXXSVBC			; exx or save bc with ip
		pop bc			; pop u -> bc
		pop de			; pop c-addr2 -> de
		pop hl			; pop c-addr1 -> hl
		ld a,c			;
		or b			; test bc = 0
		jr z,1$			; if bc <> 0 then
		ldir			;   repeat [hl++] -> [de++] until --bc = 0
1$:		EXXLDBC			; exx or restore bc with ip
		pop de			; pop new TOS
		JP_NEXT			; continue

; CMOVE>	c-addr1 c-addr2 u --
;		move u bytes from c-addr1 to c-addr2 up (from end going down)

		CODE CMOVE>,cmoveup
		push de			; save TOS
		EXXSVBC			; exx or save bc with ip
		pop bc			; pop u -> bc
		pop hl			; pop c-addr2 -> hl
		add hl,bc		;
		ex de,hl		; c-addr2 + u -> de
		pop hl			; pop c-addr1 -> hl
		add hl,bc		; c-addr1 + u -> hl
		ld a,c			;
		or b			; test bc = 0
		jr z,1$			; if bc <> 0 then
		dec de			;   c-addr2 + u - 1 -> de
		dec hl			;   c-addr1 + u - 1 -> hl
		lddr			;   repeat [hl--] -> [de--] until --bc = 0
1$:		EXXLDBC			; exx or restore bc with ip
		pop de			; pop new TOS
		JP_NEXT			; continue

; MOVE		c-addr1 c-addr2 u --
;		move u bytes from c-addr1 to c-addr2
;
;    : MOVE
;      -ROT
;      2DUP U< IF
;        ROT CMOVE>
;      ELSE
;        ROT CMOVE
;      THEN ;

		COLON MOVE,move
		.dw mrot,twodup,uless,doif,1$
		.dw   rot,cmoveup
		.dw   doexit
1$:		.dw rot,cmove
		.dw doret

; FILL		c-addr u char --
;		fill memory with char

		CODE FILL,fill
		ld a,e			; char -> a
		EXXSVBC			; exx or save bc with ip
		pop bc			; pop u -> bc
		pop de			; pop c-addr -> de
		or a			; 0 -> cf
		ld hl,-1		;
		adc hl,bc		; bc - 1 -> hl and set flags
		jr nc,1$		; if u <> 0 then
		ld (de),a		;   char -> [de]
		jr z,1$			;   if u <> 1 then
		dec bc			;     bc-- since bc = u > 1
		ld l,e			;
		ld h,d			;     c-addr -> hl
		inc de			;     c-addr + 1 -> de
		ldir			;     [hl++] -> [de++] until --bc = 0
1$:		EXXLDBC			; exx or restore bc with ip
		pop de			; pop new TOS
		JP_NEXT			; continue

; ERASE		c-addr u --
;		fill memory with zeros
;
;    : ERASE 0 FILL ;

		COLON ERASE,erase
		.dw zero,fill
		.dw doret

; BLANK		c-addr u --
;		fill memory with 0x20 (BL) chars
;
;    : ERASE BL FILL ;

		COLON BLANK,blank
		.dw bl,fill
		.dw doret

;-------------------------------------------------------------------------------
;
;		STRING CHOPPING AND TRIMMING
;
;-------------------------------------------------------------------------------

; CHOP		c-addr u1 char -- c-addr u2
;		truncate a string up to a matching char;
;		leaves the string if char not found;
;		char = 0x20 (BL) chops 0x00 to 0x20 (white space and control)

.if AUXR

		CODE CHOP,chop
		ld a,e			; char -> a
		exx			; save bc with ip
		pop bc			; pop u1 -> bc
		ld e,c			;
		ld d,b			; u1 -> de
		ex af,af'		; save a with char
		ld a,c			;
		or b			; test bc = 0, 0 -> cf
		jr z,2$			; if bc = 0 then not found
		pop hl			;
		push hl			; c-addr -> hl
		ex af,af'		; restore a with char
		cp 0x20			;
		jr z,3$			; if a = 0x20 then find white space
		or a			; 0 -> cf for when cpir ends with nz
		; find char in string
		cpir		; 21/16	; repeat until a = [hl++] or --bc = 0
		jr nz,2$		; if match then
1$:		ccf			;   complement cf to correct cpi bc--
2$:		ex de,hl		; u1 -> hl
		sbc hl,bc		; u1 - bc - cf -> hl
		push hl			; save hl as TOS
		exx			; restore bc with ip
		pop de			; pop new TOS
		JP_NEXT			; continue
		; find white space or control char in string
3$:		cp (hl)		;  7	; loop to compare a to [hl]
		cpi		; 16	;   hl++, bc--
		jr nc,1$	;  7	;   if [hl] < a then found
		jp pe,3$	; 10	; until bc = 0
		jr 1$			; not found

.else

		CODE CHOP,chop
		SVBC			; save bc with ip
		pop bc			; pop u1 -> bc
		ld a,c			;
		or b			; test bc = 0, 0 -> cf
		ld a,e			; char -> a
		ld e,c			;
		ld d,b			; u1 -> de
		jr z,2$			; if bc = 0 then not found
		pop hl			;
		push hl			; c-addr -> hl
		cp 0x20			;
		jr z,3$			; if a = 0x20 then find white space
		or a			; 0 -> cf for when cpir ends with nz
		; find char in string
		cpir		; 21/16	; repeat until a = [hl++] or --bc = 0
		jr nz,2$		; if match then
1$:		ccf			;   complement cf to correct cpi bc--
2$:		ex de,hl		; u1 -> hl
		sbc hl,bc		; u1 - bc - cf -> hl
		push hl			; save hl as TOS
		LDBC			; restore bc with ip
		pop de			; pop new TOS
		JP_NEXT			; continue
		; find white space or control char in string
3$:		cp (hl)		;  7	; loop to compare a to [hl]
		cpi		; 16	;   hl++, bc--
		jr nc,1$	;  7	;   if [hl] < a then found
		jp pe,3$	; 10	; until bc = 0
		jr 1$			; not found

.endif

; TRIM		c-addr1 u1 char -- c-addr2 u2
;		trim initial chars from a string;
;		char = 0x20 (BL) trims 0x00 to 0x20 (white space and control)

.if AUXR

		CODE TRIM,trim
		ld a,e			; char -> a
		exx			; save bc with ip
		pop bc			; u1 -> bc
		pop hl			; c-addr1 -> hl
1$:		ex af,af'	;  4	; save a
		ld a,c		;  4	;
		or b		;  4	;
		jr z,3$		;  7	; if bc <> 0 then
		ex af,af'	;  4	;   restore a
		; trim char from front of the string
2$:		cpi		; 16	;   loop
		jr nz,4$	;  7/12	;     while a = [hl++], --bc
		jp pe,2$	; 10	;   until b = 0
		; done trimming
3$:		push hl			; save hl as 2OS
		push bc			; save bc as TOS
		exx			; restore bc with ip
		pop de			; pop new TOS
		JP_NEXT			; continue
4$:		cp 0x20		;  7	;
		jr nz,5$	;  7	; if char = 0x20 then
		; trim white space and control char
		dec hl		;  6	;
		cp (hl)		;  7	;
		inc hl		;  6	;
		jr nc,1$	; 12	;   if [hl-1] <= 0x20 then keep trimming
		; stop trimming at mismatch
5$:		inc bc			; correct bc++ for mismatch
		dec hl			; correct hl-- for mismatch
		jr 3$			; finalize trimming

.else

		CODE TRIM,trim
		SVBC			; save bc with ip
		pop bc			; u1 -> bc
		pop hl			; c-addr1 -> hl
1$:		ld a,c		;  4	;
		or b		;  4	;
		jr z,3$		;  7	; if bc <> 0 then
		ld a,e		;  4	;
		; trim char from front of the string
2$:		cpi		; 16	;   loop
		jr nz,4$	;  7/12	;     while a = [hl++], --bc
		jp pe,2$	; 10	;   until b = 0
		; done trimming
3$:		push hl			; save hl as 2OS
		push bc			; save bc as TOS
		LDBC			; restore bc with ip
		pop de			; pop new TOS
		JP_NEXT			; continue
4$:		cp 0x20		;  7	;
		jr nz,5$	;  7	; if char = 0x20 then
		; trim white space and control char
		dec hl		;  6	;
		cp (hl)		;  7	;
		inc hl		;  6	;
		jr nc,1$	; 12	;   if [hl-1] <= 0x20 then keep trimming
		; stop trimming at mismatch
5$:		inc bc			; correct bc++ for mismatch
		dec hl			; correct hl-- for mismatch
		jr 3$			; finalize trimming

.endif

; -TRIM		c-addr u1 char -- c-addr u2
;		trim trailing chars from a string;
;		char = 0x20 (BL) trims 0x00 to 0x20 (white space and control)

.if AUXR

		CODE -TRIM,mtrim
		ld a,e			; char -> a
		exx			; save bc with ip
		pop bc			; u1 -> bc
		pop hl			; c-addr1 -> hl
		push hl			; keep c-addr1
		add hl,bc		;
		dec hl			; c-addr + u1 - 1 -> hl trim from end
1$:		ex af,af'	;  4	; save a with char
		ld a,c		;  4	;
		or b		;  4	;
		jr z,3$		;  7/12	; if bc <> 0 then
		ex af,af'	;  4	;   restore a with char
		; trim char from back of the string
2$:		cpd		; 16	;   loop
		jr nz,4$	;  7	;     while a = [hl--], --bc
		jp pe,2$	; 10	;   until b = 0
		; done trimming
3$:		push bc			; save bc as TOS
		exx			; restore bc with ip
		pop de			; pop new TOS
		JP_NEXT			; continue
4$:		cp 0x20			;
		jr nz,5$		; if char = 0x20 then
		; trim white space and control char
		inc hl			;
		cp (hl)			;
		dec hl			;
		jr nc,1$		;   if [hl+1] <= 0x20 then keep trimming
		; stop trimming at mismatch
5$:		inc bc			; correct bc++ for cpd bc-- mismatch
		jr 3$			; finalize trimming

.else

		CODE -TRIM,mtrim
		SVBC			; save bc with ip
		pop bc			; u1 -> bc
		pop hl			; c-addr1 -> hl
		push hl			; keep c-addr1
		add hl,bc		;
		dec hl			; c-addr + u1 - 1 -> hl trim from end
1$:		ld a,c		;  4	;
		or b		;  4	;
		jr z,3$		;  7/12	; if bc <> 0 then
		ld a,e		;  4	; char -> a
		; trim char from back of the string
2$:		cpd		; 16	;   loop
		jr nz,4$	;  7	;     while a = [hl--], --bc
		jp pe,2$	; 10	;   until b = 0
		; done trimming
3$:		push bc			; save bc as TOS
		LDBC			; restore bc with ip
		pop de			; pop new TOS
		JP_NEXT			; continue
4$:		cp 0x20			;
		jr nz,5$		; if char = 0x20 then
		; trim white space and control char
		inc hl			;
		cp (hl)			;
		dec hl			;
		jr nc,1$		;   if [hl+1] <= 0x20 then keep trimming
		; stop trimming at mismatch
5$:		inc bc			; correct bc++ for cpd bc-- mismatch
		jr 3$			; finalize trimming

.endif

; -TRAILING	c-addr u1 -- c-addr u2
;		trim trailing white space and control characters from a string
;
;    : -TRAILING BL -TRIM ;

		COLON -TRAILING,mtrailing
		.dw bl,mtrim
		.dw doret

; /STRING	c-addr1 u1 n -- c-addr2 u2
;		slice n characters off the start of a string;
;		if n is larger than u1, then u2 underflows,
;		which should be avoided with OVER UMIN /STRING
;
;    : /STRING ROT OVER + -ROT - ;

		CODE /STRING,slashstring
		pop hl			; pop u1 -> hl
		ex (sp),hl		; c-addr1 -> hl, save u1
		add hl,de		; c-addr1 + n -> hl
		ex (sp),hl		; u1 -> hl, save c-addr1 + n as new 2OS
		or a			; 0 -> cf
		sbc hl,de		; u1 - n -> hl
		ex de,hl		; set new TOS to hl
		JP_NEXT			; continue

; NEXT-CHAR	c-addr1 u1 -- c-addr2 u2 char
;		get next char from a string;
;		increments the string address and decrements its length by one
;
;    : NEXT-CHAR OVER C@ >R 1- SWAP 1+ SWAP R> ;
;    : NEXT-CHAR OVER C@ -ROT 1- SWAP 1+ SWAP ROT ;

		CODE NEXT-CHAR,nextchar
		pop hl			; c-addr1 -> hl
		ld a,(hl)		; [c-addr1] -> a
		inc hl			; c-addr1 + 1 -> hl
		push hl			; save c-addr1 + 1 as 3OS
		dec de			;
		push de			; save u1 - 1 -> de as new 2OS
		ld e,a			;
		ld d,0			; set new TOS to a with char
		JP_NEXT			; continue

;-------------------------------------------------------------------------------
;
;		OUTPUT
;
;-------------------------------------------------------------------------------

; WIDTH		u --
;		set text mode screen and its window width

		CODE WIDTH,width
		push bc			; save bc with ip
		ex de,hl		; save TOS de -> hl
		call ERAFNK		; MSX ERAFNK disable function key row
		ld a,l			;
		cp 33			;
		jr c,1$			; if u >= 33 then
		ld (LINL40),a		;   set MSX screen 0 line length to TOS
		call INITXT		;   MSX INITXT initialize text screen 0
		jr 2$			; else
1$:		ld (LINL32),a		;   set MSX screen 1 line length to TOS
		call INIT32		;   MSX INITXT initialize text screen 1
2$:		ld a,(LINLEN)		;
		ld (maxxy+3),a		; MSX LINLEN -> X of MAX-XY
		pop bc			; restore bc with ip
		pop de			; pop new TOS
		JP_NEXT			; continue

; MAX-XY	-- u1 u2
;		leave number of screen columns x as u1 and rows y as u2

		TWOVALUE MAX-XY,maxxy
		.dw 40			; current line length (copy of LINLEN)
		.dw 24			; number of lines

; CUR-XY	-- u1 u2
;		fetch cursor column x as u1 >= 0 and row y as u2 >= 0

		CODE CUR-XY,curxy
		push de			; save TOS
		ld a,(CSRX)		; [CSRX] -> a
		dec a			;
		ld e,a			;
		ld d,0			; set [CSRX] - 1 -> de as new 2OS
		push de			; save new 2OS
		ld a,(CSRY)		; [CSRY] -> a
		dec a			;
		ld e,a			;
		ld d,0			; set [CSRY] - 1 -> de as new TOS
		JP_NEXT			; continue

; AT-XY		u1 u2 --
;		set column x to u1 >= 0 and row y to u2 >= 0
;
;    : AT-XY Y! X! ;

		CODE AT-XY,atxy
		ld l,e			; TOS y + 1 -> l
		inc l			;
		pop de			;
		ld h,e			; 2OS x + 1 -> h
		inc h			;
		CALL POSIT		; POSIT cursor at xy
		pop de			; pop new TOS
		JP_NEXT			; continue

.if FCBN

; OUTPUT-ID	-- 0|fileid
;		value with 0 = console output, otherwise fileid to redirect output

		VALUE OUTPUT-ID,outputid
		.dw 0

.endif;FCBN

; EMIT		char --
;		emit char to screen or to OUTPUT-ID when set;
;		list of control codes for console output:
;
;		code | effect
;		---- | ---------------------------------------------------------
;		   1 | MSX graphic character header, follow by 0x40 to 0x5f
;		   7 | BELL
;		   8 | BS backspace
;		   9 | TAB
;		  10 | LF line feed
;		  11 | HOME cursor home
;		  12 | FF CLS clear screen and home
;		  13 | CR carriage return
;		  27 | ESC escape sequence (see table below)
;		  28 | cursor right (wraps but does not scroll)
;		  29 | cursor left (wraps but does not scroll)
;		  30 | cursor up (does not scroll)
;		  31 | cursor down (does not scroll)
;		 127 | DEL delete character
;
;		ESC code | effect
;		-------- | -----------------------------------------------------
;		ESC j    | clear screen and home
;		ESC E    | clear screen and home
;		ESC K    | clear to end of line
;		ESC J    | clear to end of screen
;		ESC l    | clear line
;		ESC L    | insert line
;		ESC M    | delete line
;		ESC Y    | set cursor coordinates, follow by row column bytes
;		ESC A    | cursor up, does not scroll
;		ESC B    | cursor down, does not scroll
;		ESC C    | cursor right, wraps if the logical line of text wraps
;		ESC D    | cursor left, does not wrap
;		ESC H    | cursor home
;		ESC x    | change cursor, follow by '4' (block) or '5' (disable)
;		ESC y    | change cursor, follow by '4' (under) or '5' (Enable)

		CODE EMIT,emit
.if FCBN
		ld hl,(outputid+3)	; OUTPUT-ID -> hl
		ld a,l			;
		or h			;
		jr z,2$			; if OUTPUT-ID != 0 then
		ld a,e			;
		ld de,1$		;
		ld (de),a		;   char -> [emit_chbuf]
		push bc			;   save bc with ip
		SVIXIY			;   save ix and iy when necessary
		push hl			;   save hl with OUTPUT-ID value fileid
		ld c,0x1a		;
		call BDOS		;   MSX BDOS 1ah set DTA to c-addr
		pop de			;   pop de with OUTPUT-ID value fileid
		ld hl,1			;   1 -> hl
		ld c,0x26		;
		call BDOS		;   MSX BDOS 26h block write (assume record size = 1)
		LDIXIY			;   restore ix and iy
		pop bc			;   restore bc with ip
		or a			;
		ld a,-37		;
		jp nz,throw_a		;   if error then THROW -37
		pop de			;   pop new TOS
		JP_NEXT			;   continue
1$:		.db 0			;   a single character buffer
.endif
2$:		ld a,e			; TOS char -> a
		pop de			; pop new TOS
chput_a_next:	call CHPUT		; MSX CHPUT a to the console
		JP_NEXT			; continue

; PAGE		--
;		clear console screen
;
;    : PAGE $C EMIT ;

		CODE PAGE,page
		ld a,0x0c		;
		jr chput_a_next		; clear screen

; CR		--
;		emit carriage return and line feed
;
;    : CR 13 EMIT 10 EMIT ;

		COLON CR,cr
		.dw dolit,13,emit
		.dw dolit,10,emit
		.dw doret

; SPACE		--
;		emit a space
;
;    : SPACE BL EMIT ;

		COLON SPACE,space
		.dw bl,emit
		.dw doret

; SPACES	n --
;		emit n spaces (zero or negative n does nothing)
;
;    : SPACES 0 MAX 0 ?DO SPACE LOOP ;

		COLON SPACES,spaces
		.dw zero,max,zero,doqdo,2$
1$:		.dw   space
		.dw doloop,1$
2$:		.dw doret

; TYPE		c-addr u --
;		type string to output or to OUTPUT-ID when set;
;		string may contain control codes, see EMIT
;
;    : TYPE
;      BEGIN DUP WHILE
;        NEXT-CHAR EMIT
;      REPEAT
;      2DROP ;

		CODE TYPE,type
.if FCBN
		ld hl,(outputid+3)	; OUTPUT-ID -> hl
		ld a,l			;
		or h			;
		jr z,1$			; if OUTPUT-ID != 0 then
		ex (sp),hl		;   2OS <-> hl with OUTPUT-ID value fileid
		push bc			;   save bc with ip
		SVIXIY			;   save ix and iy when necessary
		push de			;   save TOS with u
		ex de,hl		;   hl -> de with c-addr
		ld c,0x1a		;
		call BDOS		;   MSX BDOS 1ah set DTA to c-addr
		pop hl			;   pop hl with u
		pop bc			;
		pop de			;   pop de with OUTPUT-ID value fileid
		push bc			;
		ld c,0x26		;
		call BDOS		;   MSX BDOS 26h block write (assume record size = 1)
		LDIXIY			;   restore ix and iy
		pop bc			;   restore bc with ip
		or a			;
		ld a,-37		;
		jp nz,throw_a		;   if error then THROW -37
		pop de			;   pop new TOS
		JP_NEXT			;   continue
.endif
1$:		pop hl			; pop hl with c-addr
		inc d			; outer loop counter adjustment
		inc e			; inner loop counter adjustment
		jr 3$			; start looping
2$:		ld a,(hl)		; loop TOS times
		inc hl			;
		call CHPUT		;   emit [hl++]
3$:		dec e			;
		jr nz,2$		;
		dec d			;
		jr nz,2$		;
		pop de			; pop new TOS
		JP_NEXT			; continue

.if DUMP

;- DUMP		c-addr u --
;		dump memory in hex
;
;    : DUMP
;      OVER 7 AND IF
;        OVER 0 16 U.RB
;      THEN
;      BEGIN DUP WHILE
;        OVER 7 AND 0= IF
;          CR OVER 0 16 U.RB
;        THEN
;        NEXT-CHAR 3 16 U.RB
;      REPEAT
;      2DROP ;

		COLON DUMP,dump
		.dw over,dolit,7,and,doif,1$
		.dw   over,zero,dolit,16,udotrb
1$:		.dw   dup,doif,3$
		.dw   over,dolit,7,and,zeroequal,doif,2$
		.dw     cr,over,zero,dolit,16,udotrb
2$:		.dw   nextchar,three,dolit,16,udotrb
		.dw doagain,1$
3$:		.dw twodrop
		.dw doret

.endif;DUMP

;-------------------------------------------------------------------------------
;
;		PICTURED NUMERIC OUTPUT
;
;-------------------------------------------------------------------------------

; BASE		-- addr
;		variable with numeric base for conversion
;
;    VARIABLE BASE

		VARIABLE BASE,base
		.dw 10

; DECIMAL	--
;		set BASE to 10
;
;    : DECIMAL 10 BASE ! ;

		CODE DECIMAL,decimal
		ld hl,10		;
set_base:	ld (base+3),hl		; 10 -> [base]
		JP_NEXT			; continue

; HEX		--
;		set BASE to 16
;
;    : HEX 16 BASE ! ;

		CODE HEX,hex
		ld hl,16		;
		jr set_base		; 16 -> [base] continue

; HP		-- addr
;		hold pointer
;
;    0 VALUE HP

		VALUE HP,hp
		.dw 0

; <#		--
;		begin pictured numeric output
;
;    : <# HERE h_size + TO HP ;

		COLON <#,lesshash
		.dw here,dolit,h_size,plus,doto,hp+3
		.dw doret

; HOLD		char --
;		hold char for pictured numeric output
;
;    : HOLD -1 +TO HP HP C! ;

		COLON HOLD,hold
		.dw mone,doplusto,hp+3,hp,cstore
		.dw doret

; HOLDS		c-addr u --
;		hold string for pictured numeric output;
;		the string size should be limited to not exceed the hold space size of 40 bytes
;
;    : HOLDS
;      BEGIN DUP WHILE
;        1- 2DUP + C@ HOLD
;      REPEAT
;      2DROP ;

		COLON HOLDS,holds
1$:		.dw dup,doif,2$
		.dw   oneminus,twodup,plus,cfetch,hold
		.dw doagain,1$
2$:		.dw twodrop
		.dw doret

; #		ud1 -- ud2
;		hold digit
;
;    : #
;      0 BASE @ UM/MOD >R
;      BASE @ UM/MOD
;      SWAP DUP 9 > IF
;        7 +
;      THEN
;      '0 + HOLD
;      R> ;

		COLON #,hash
		.dw zero,base,fetch,umslashmod,tor
		.dw base,fetch,umslashmod,swap
		.dw dup,dolit,9,more,doif,1$
		.dw   dolit,7,plus
1$:		.dw dolit,'0,plus,hold
		.dw rfrom
		.dw doret

; #S		ud -- 0 0
;		hold all remaining digits
;
;    : #S BEGIN # 2DUP D0= UNTIL ;

		COLON #S,hashs
1$:		.dw   hash,twodup,dzeroequal
		.dw dountil,1$
		.dw doret

; SIGN		n --
;		hold minus sign if n < 0
;
;    : SIGN 0< IF '- HOLD THEN ;

		COLON SIGN,sign
		.dw zeroless,doif,1$
		.dw   dolit,'-,hold
1$:		.dw doret

; #>		ud -- c-addr u
;		end pictured numeric output, leave string
;
;    : #> 2DROP HP HERE h_size + OVER - ;

		COLON #>,hashmore
		.dw twodrop
		.dw hp,here,dolit,h_size,plus,over,minus
		.dw doret

;-------------------------------------------------------------------------------
;
;		NUMERIC OUTPUT
;
;-------------------------------------------------------------------------------

; D.R		d +n --
;		output signed double d right-aligned in field of +n chars wide
;
;    : D.R -ROT TUCK DABS <# #S ROT SIGN #> ROT OVER - SPACES TYPE ;

		COLON D.R,ddotr
		.dw mrot
		.dw tuck,dabs
		.dw lesshash,hashs,rot,sign,hashmore
		.dw rot,over,minus,spaces,type
		.dw doret

; D.		d --
;		output signed double d with a trailing space
;
;    : D. 0 D.R SPACE ;

		COLON D.,ddot
		.dw zero,ddotr,space
		.dw doret

; U.R		u +n --
;		output unsigned u right-aligned in field of +n chars wide
;
;    : U.R 0 SWAP D.R ;

		COLON U.R,udotr
		.dw zero,swap,ddotr
		.dw doret

; U.		u --
;		output unsigned u with a trailing space
;
;    : U. 0 D. ;

		COLON U.,udot
		.dw zero,ddot
		.dw doret

; .R		n +n --
;		output signed n right-aligned in field of +n chars wide
;
;    : .R SWAP S>D ROT D.R ;

		COLON .R,dotr
		.dw swap,stod,rot,ddotr
		.dw doret

; .		n --
;		output signed n with a trailing space
;
;    : . S>D D. ;

		COLON .,dot
		.dw stod,ddot
		.dw doret

; ?		addr --
;		output signed cell stored at addr
;
;    : ? @ . ;

		COLON ?,question
		.dw fetch,dot
		.dw doret

.if DUMP

;- D.RB		d n1+ n2+ --
;		output signed double d right-aligned in field of +n1 chars wide in base n2+
;
;    : D.RB BASE @ >R BASE ! D.R R> BASE ! ;

		COLON D.RB,ddotrb
		.dw base,fetch,tor,base,store,ddotr,rfrom,base,store
		.dw doret

;- U.RB		u n1+ n2+ --
;		output unsigned u right-aligned in field of +n1 chars wide in base n2+
;
;    : U.RB BASE @ >R BASE ! U.R R> BASE ! ;

		COLON U.RB,udotrb
		.dw base,fetch,tor,base,store,udotr,rfrom,base,store
		.dw doret

.endif;DUMP

;-------------------------------------------------------------------------------
;
;		PORT I/O AND MSX VDP/VRAM CONTROL
;
;-------------------------------------------------------------------------------

.if PORT

;- >PORT	u1 u2 --
;		send byte u1 to Z80 port u2

		CODE >PORT,toport
		ld a,e			; u2 -> a port
		pop de			; pop u1 -> de
		push bc			; save bc with ip
		ld c,a			;
		out (c),e		; u1 -> out(u2)
		pop bc			; restore bc with ip
		pop de			; set new TOS
		JP_NEXT			; continue

;- PORT>	u1 -- u2
;		receive a byte from Z80 port u1

		CODE PORT>,portfrom
		push bc			; save bc with ip
		ld c,e			; u1 -> c
		in e,(c)		; in(u1) -> e
		ld d,0			; 0 -> d
		pop bc			; restore bc with ip
		JP_NEXT			; continue

;- VRAM		addr --
;		set VRAM address for writing, add $4000 to addr to read

		CODE VRAM,vram
		ld a,e			; addr lsb -> a
		di			; disable interrupts (interrupts access the VDP)
		out (0x99),a		; VDP data port address register lsb
		ld a,0x40		;
		or d			; addr msb | 0x40 -> a
		out (0x99),a		; VDP data port address register msb
		ei			; enable interrupts
		pop de			; set new TOS
		JP_NEXT			; continue

;- >VRAM	c-addr u --
;		block write u data bytes at c-addr to VRAM at its current address, auto-increments its address
;		VRAM address must be set for writing

		CODE >VRAM,tovram
		pop hl			; pop hl with c-addr
		ld a,e			;
		or d			;
		jr z,2$			; if u != 0 then
		push bc			;   save bc with ip
		ld c,0x98		;   VDP VRAM data port is IO port c = 0x98
		ld b,e			;   u -> ab
		xor a			;   0 -> a
		cp b			;   set cf if b > 0
		adc d			;   if b > 0 then d+1 -> a else d -> a (is nonzero since u != 0)
1$:		otir			;   [hl++] -> out(0x98) until --b = 0
		dec a			;
		jr nz,1$		; until --a = 0
		pop bc			; restore bc with ip
2$:		pop de			; pop new TOS
		JP_NEXT			; continue

;- VRAM>	c-addr u --
;		block read u data bytes at c-addr from VRAM at its current address, auto-increments its address;
;		VRAM address must be set for reading

		CODE VRAM>,vramfrom
		pop hl			; pop hl with c-addr
		ld a,e			;
		or d			;
		jr z,2$			; if u != 0 then
		push bc			;   save bc with ip
		ld c,0x98		;   VDP VRAM data port is IO port c = 0x98
		ld b,e			;   u -> ab
		xor a			;   0 -> a
		cp b			;   set cf if b > 0
		adc d			;   if b > 0 then d+1 -> a else d -> a (is nonzero since u != 0)
1$:		inir			;   in(0x98) -> [hl]++ until --b = 0
		dec a			;
		jr nz,1$		; until --a = 0
		pop bc			; restore bc with ip
2$:		pop de			; pop new TOS
		JP_NEXT			; continue

;- >VDP		u1 u2 --
;		send data byte u1 to VDP mode register u2

		CODE >VDP,tovdp
		pop hl			; pop u1 -> hl
		ld a,l			; u2 -> a
		di			; disable interrupts (interrupts access the VDP)
		out (0x99),a		; write data byte u1 -> out(0x99)
		ld a,e			;
		or 0x80			; u2 | 0x80 -> a
		out (0x99),a		; select register 10000RRR -> out(0x99)
		ei			; enable interrupts
		pop de			; set new TOS
		JP_NEXT			; continue

.endif;PORT

;-------------------------------------------------------------------------------
;
;		INPUT
;
;-------------------------------------------------------------------------------

.if XTRA

;+ INKEY	-- x
;		check for key press and return the code of the key;
;		0x00 = no key pressed

		CODE INKEY,inkey
		push de			; save TOS
		call CHSNS		; MSX CHSNS check keyboard status
		jp z,zero_next		; if no key then set new TOS to 0
		call CHGET		; MSX CHGET get the key
		jp a_next		; set a -> de new TOS and continue

;+ KEY-CLEAR	--
;		wait until no keys are pressed
;
;    : KEY-CLEAR BEGIN INKEY 0= UNTIL ;

		CODE KEY-CLEAR,keyclear
		call KILBUF		; MSX KILBUF clear key buffer
		JP_NEXT			; continue

.endif;XTRA

; KEY?		-- flag
;		true if a key is pressed

		CODE KEY?,keyq
		push de			; save TOS
		call CHSNS		; MSX CHSNS check keyboard status
		jp true_if_nz_next	; set new TOS to TRUE if nz

; KEY		-- char
;		wait and read key from the console

		CODE KEY,key
		push de			; save TOS
		call CHGET		; MSX CHGET key code
		jp a_next		; set a -> de new TOS and continue

;-------------------------------------------------------------------------------
;
;		EDITOR
;
;-------------------------------------------------------------------------------

; Store (toname) and retrieve (name) an internal integer by name

.macro		INT name
to'name:	ld (addr'name),de
		pop de
		JP_NEXT
name:		push de
		ld de,(addr'name)
		JP_NEXT
addr'name:	.dw 0
.endm

		; min <= len <= max
		INT edmin		; minimum cursor position (for a prompt)
		INT edlen		; length of the string in the buffer
		INT edmax		; maximum string length (buffer size)
		INT edbuf		; buffer string address

; EDIT		c-addr +n1 n2 n3 -- c-addr +n4
;		edit buffer c-addr;
;		buffer size +n1;
;		string in buffer has length n2;
;		non-editable left margin n3;
;		leaves c-addr and length +n4 (MSX INLIN strips first n3 characters)

.if EDIT

		COLON EDIT,edit
		.dw toedmin		; store n3
		.dw toedlen		; store n2
		.dw toedmax		; store n1
		.dw toedbuf		; store c-addr
		.dw edbuf,edlen,type	; type buffer to output
		.dw edmin,zero,doqdo,2$
1$:		.dw   dolit,29,emit	; move cursor left to edmin
		.dw doloop,1$
2$:		.dw doinlin		; MSX INLIN
		.dw edmax,umin
		.dw over,edbuf,notequal,doif,3$
		.dw   tuck,edbuf,swap,cmove,edbuf,swap
3$:		.dw doret

; (INLIN)	-- c-addr u
;		MSX INLIN input a logical line of screen text;
;		leaves TIB c-addr and text input length u
;
;		key          | effect
;		------------ | -------------------------------------------------
;		cursor LEFT  | CTRL-] move cursor down
;		cursor RIGHT | CTRL-\ move cursor down
;		cursor UP    | CTRL-^ move cursor up
;		cursor DOWN  | CTRL-_ move cursor down
;		RETURN       | CTRL-M enter the logical line the cursor is on
;		HOME         | CTRL-K move cursor to the top left corner
;		SHIFT-HOME   | CTRL-L clear screen
;		INS          | CTRL-R toggle insert/overwrite mode
;		DEL          | delete character under the cursor
;		BS           | CTRL-H delete character to the left of the cursor
;		TAB          | CTRL-I insert TAB spacing
;		CTRL-A       | insert graphic header, follow by A to Z [ \ ] ^ _
;		CTRL-B       | move cursor backward to the previous word
;		CTRL-F       | move cursor forward to the next word
;		CTRL-J       | move cursor down (line feed, scrolls)
;		CTRL-N       | move cursor to the end of the logical line
;		CTRL-E       | delete from cursor to the end of the logical line
;		CTRL-U       | delete the entire logical line the cursor is on
;		(CTRL-)STOP  | program break

		CODE (INLIN),doinlin
		push de			; save TOS
		push bc			; save bc with ip
		call INLIN		; MSX INLIN
		inc hl			; hl++ points to TIB
		xor a			; 0 -> a
		ld b,a			;
		ld c,a			; 0 -> bc
		ld e,l			;
		ld d,h			; hl -> de
		cpir			; bc-- until [hl++] = 0
		ld a,c			;
		cpl			; -bc - 1 -> a with input length u
		pop bc			; restore bc with ip
		push de			; push c-addr = TIB
		ld e,a			;
		ld d,0			; set TOS to length u
		JP_NEXT			; continue

.else

		COLON EDIT,edit
		.dw toedmin		; store n3
		.dw toedlen		; store n2
		.dw toedmax		; store n1
		.dw toedbuf		; store c-addr
		.dw edbuf,edlen,type	; type buffer to output
1$:		; begin loop
		.dw   key		; wait for user key
		; case of ENTER
		.dw   dolit,0x0d,doof,2$
		.dw     cr,edbuf,edlen
		.dw     doexit
2$:		; of BS backspace
		.dw   dolit,0x08,doof,3$
		.dw     edlen,edmin,umore,doif,5$
		.dw       edlen,oneminus,toedlen
		.dw       dolit,0x08,bl,over,emit,emit,emit
		.dw     doahead,5$
3$:		; otherwise, append char if within $20 to $7e
		.dw   dup,bl,dolit,0x7f,within,doif,4$
		.dw     edlen,edmax,uless,doif,4$
		.dw       dup,emit
		.dw       dup,edbuf,edlen,plus,cstore
		.dw       edlen,oneplus,toedlen
4$:		.dw   drop
		; endcase again
5$:		.dw doagain,1$
		.dw doret

.endif;EDIT

; ACCEPT	c-addr +n1 -- +n2
;		accept user input into buffer c-addr +n1;
;		leaves length +n2
;
;    : ACCEPT 0 0 EDIT NIP ;

		COLON ACCEPT,accept
		.dw zero,zero,edit,nip
		.dw doret

;-------------------------------------------------------------------------------
;
;		PARSING
;
;-------------------------------------------------------------------------------

.if FCBN

; #IN		-- n
;		value with line number of the input file being read from SOURCE-ID
;
;    0 VALUE #IN

		VALUE #IN,hashin
		.dw 0

.endif

; >IN		-- addr
;		variable with offset into the input buffer
;
;    VARIABLE >IN

		VARIABLE >IN,toin
		.dw 0

; SOURCE-ID	-- 0|-1|fileid
;		value with 0 = console input or -1 = string input, otherwise fileid input
;
;    0 VALUE SOURCE-ID

		VALUE SOURCE-ID,sourceid
		.dw 0

; SOURCE	-- c-addr u
;		double value with input source
;
;    TIB DROP 0 2VALUE SOURCE

		TWOVALUE SOURCE,source
		.dw 0			; input length u
		.dw TIB			; input buffer c-addr

; RESTORE-INPUT	... n -- flag
;		restore input parameters from the stack;
;		flag is always FALSE (success)
;
;    : RESTORE-INPUT DROP >IN ! TO SOURCE TO SOURCE-ID FALSE ;
;    : RESTORE-INPUT DROP TO #IN >IN ! TO SOURCE TO SOURCE-ID FALSE ;

		COLON RESTORE-INPUT,restoreinput
		.dw drop
.if FCBN
		.dw doto,hashin+3
.endif
		.dw toin,store
		.dw dotwoto,source+3
		.dw doto,sourceid+3
		.dw false
		.dw doret

; SAVE-INPUT	-- ... n
;		save input parameters on the stack
;
;    : SAVE-INPUT SOURCE-ID SOURCE >IN @ 4 ;
;    : SAVE-INPUT SOURCE-ID SOURCE >IN @ #IN 5 ;

		COLON SAVE-INPUT,saveinput
		.dw sourceid,source,toin,fetch
.if FCBN
		.dw hashin,dolit,5
.else
		.dw dolit,4
.endif
		.dw doret

; REFILL	-- flag
;		attempt to refill the input buffer;
;		leaves FALSE when the end of input (end of file) is reached
;
;    : REFILL
;      SOURCE-ID INVERT DUP IF
;        TIB OVER SWAP
;        ACCEPT
;        TO SOURCE
;        >IN OFF
;      THEN ;
;
;    : REFILL
;      SOURCE-ID INVERT DUP IF
;        SOURCE-ID ?DUP IF
;          FIB OVER SWAP SOURCE-ID
;          READ-LINE 0= AND 0= IF
;            2DROP DROP FALSE
;            EXIT
;          THEN
;          1 +TO #IN
;        ELSE
;          TIB OVER SWAP
;          ACCEPT
;          0 TO #IN
;        THEN
;        TO SOURCE
;        >IN OFF
;      THEN ;

		COLON REFILL,refill
		.dw sourceid,invert,dup,doif,4$
.if FCBN
		.dw   sourceid,qdup,doif,2$
		.dw     fib,over,swap,sourceid
		.dw     readline,zeroequal,and,zeroequal,doif,1$
		.dw       twodrop,drop,false
		.dw       doexit
1$:		.dw     one,doplusto,hashin+3
		.dw   doahead,3$
.endif
2$:		.dw     tib,over,swap
		.dw     accept
.if FCBN
		.dw     zero,doto,hashin+3
.endif
3$:		.dw   dotwoto,source+3
		.dw   toin,off
4$:		.dw doret

; SKIP		char "<chars>" --
;		skip chars in input when present, 0x20 (BL) skips 0x00 to 0x20 (white space and control)
;
;    : SKIP SOURCE >IN @ /STRING ROT TRIM DROP SOURCE DROP - >IN ! ;

		COLON SKIP,skip
		.dw source,toin,fetch,slashstring
		.dw rot,trim,drop
		.dw source,drop,minus,toin,store
		.dw doret

; PARSE		char "ccc<char>" -- c-addr u
;		parse "ccc" up to char when present
;
;    : PARSE SOURCE >IN @ /STRING ROT CHOP DUP 1+ >IN @ + SOURCE NIP UMIN >IN ! ;

		COLON PARSE,parse
		.dw source,toin,fetch,slashstring
		.dw rot,chop
		.dw dup,oneplus,toin,fetch,plus
		.dw source,nip,umin,toin,store
		.dw doret

; PARSE-WORD	char "<chars>ccc<char>" -- c-addr u
;		parse char-delimited word;
;		may throw -18 "parsed string overflow"
;
;    : PARSE-WORD
;      DUP SKIP PARSE
;      DUP b_size-1 U> IF -18 THROW THEN ;

		COLON PARSE-WORD,parseword
		.dw dup,skip,parse
		.dw dup,dolit,b_size-1,umore,doif,1$
		.dw   dolit,-18,throw
1$:		.dw doret

; WORD		char "<chars>ccc<char>" -- c-addr
;		parse word as a counted string
;
;    : WORD TMP DUP ROT PARSE-WORD ROT 2DUP C! 1+ SWAP CMOVE ;
;    : WORD PARSE-WORD SDUP DROP 1- ;

		COLON WORD,word
		.dw parseword,sdup,drop,oneminus
		.dw doret

; CHECK-NAME	c-addr u -- c-addr u
;		check if name is valid;
;		may throw -16 "attempt to use a zero-length string as a name";
;		may throw -19 "definition name too long"
;
;    : CHECK-NAME
;      DUP 0= IF -16 THROW THEN
;      DUP length_mask U> IF -19 THROW THEN ;

		COLON CHECK-NAME,checkname
		.dw dup,zeroequal,doif,1$
		.dw   dolit,-16,throw
1$:		.dw dup,dolit,length_mask,umore,doif,2$
		.dw   dolit,-19,throw
2$:		.dw doret

; PARSE-NAME	"<spaces>name<space>" -- c-addr u
;		parse space-delimited name;
;		check if name length is valid
;
;    : PARSE-NAME BL PARSE-WORD CHECK-NAME ;

		COLON PARSE-NAME,parsename
		.dw bl,parseword,checkname
		.dw doret

; CHAR		"<spaces>name<space>" -- char
;		parse char
;
;    : CHAR PARSE-NAME DROP C@ ;

		COLON CHAR,char
		.dw parsename,drop,cfetch
		.dw doret

; >DIGIT	char -- n
;		convert char digit to numeric digit when within BASE;
;		leaves -1 if char is invalid

		CODE >DIGIT,todigit
		ld a,d			;
		or a			; test d = 0 TOS high byte
		jp nz,mone_next		; set new TOS to -1 if TOS high byte is nonzero
		ld a,e			; char -> a
		cp '0			; test char < '0'
		jp c,mone_next		; set new TOS to -1 if char < '0'
		cp '9+1			; test char <= '9'
		jr c,1$			; set new TOS to char - '0' if char <= '9'
		and 0xdf		; make char upper case
		cp 'A			; test char < 'A'
		jp c,mone_next		; set new TOS to -1 if char < 'A'
		cp 'Z+1			; test cha > 'Z'
		jp nc,mone_next		; set new TOS to -1 if char > 'Z'
		sub 7			; convert char 'A'..'Z' to n
1$:		sub '0			; convert char to n
		ld hl,base+3		; BASE -> hl
		cp (hl)			; test n >= [BASE] using BASE low order byte
		jp nc,mone_next		; set new TOS to -1 if n >= [BASE]
		ld e,a			; set new TOS to n
		JP_NEXT			; continue

; >NUMBER	ud1 c-addr1 u1 -- ud2 c-addr2 u2
;		convert string to number;
;		updates accumulated double ud1 to ud2;
;		leaves string with the remaining unconvertable chars or empty
;
;    : >NUMBER
;      BEGIN DUP WHILE
;        NEXT-CHAR >DIGIT
;        DUP 0< IF
;          DROP -1 /STRING
;          EXIT
;        THEN
;        >R
;        2SWAP
;        BASE @ UMD*
;        R> M+
;        2SWAP
;      REPEAT ;

		COLON >NUMBER,tonumber
1$:		.dw dup,doif,3$
		.dw   nextchar,todigit
		.dw   dup,zeroless,doif,2$
		.dw     drop
		.dw     mone,slashstring
		.dw     doexit
2$:		.dw   tor
		.dw   twoswap
		.dw   base,fetch,umdstar
		.dw   rfrom,mplus
		.dw   twoswap
		.dw doagain,1$
3$:		.dw doret

; DBL		-- flag
;		true if >DOUBLE or NUMBER parsed and produced a double
;
;    0 VALUE DBL

		VALUE DBL,dbl
		.dw 0

; >DOUBLE	c-addr u -- d true | false
;		convert string to signed double;
;		prefixed with character $ converts hex;
;		prefixed with character # converts decimal;
;		prefixed with character % converts binary;
;		leaves the double and true if string is converted;
;		leaves false if string is unconvertable;
;		sets value DBL to -1 when the number is a double;
;		otherwise sets value DBL to 0

		COLON >DOUBLE,todouble
		.dw zero,doto,dbl+3
		.dw nextchar
		.dw dolit,'$,doof,1$
		.dw   dolit,16
		.dw doahead,6$
1$:		.dw dolit,'#,doof,2$
		.dw   dolit,10
		.dw doahead,6$
2$:		.dw dolit,'%,doof,3$
		.dw   two
		.dw doahead,6$
3$:		.dw dolit,'',doof,5$
		.dw   three,uless,doif,4$
		.dw     cfetch,stod
		.dw     true
		.dw     doexit
4$:		.dw   drop
		.dw   false
		.dw   doexit
		.dw doahead,6$
5$:		.dw   drop
		.dw   mone,slashstring
		.dw   base,fetch
6$:		.dw base,fetch,tor
		.dw base,store
		.dw nextchar
		.dw dolit,'-,equal,duptor
		.dw invert,slashstring
		.dw zero,zero,twoswap
		.dw tonumber,dup,doif,7$
		.dw   mone,doto,dbl+3
		.dw   nextchar
		.dw   dolit,'.,notequal,slashstring
		.dw   tonumber
7$:		.dw nip
		.dw doif,8$
		.dw   rdrop,rfrom,base,store
		.dw   twodrop
		.dw   false
		.dw   doexit
8$:		.dw rfrom,doif,9$
		.dw   dnegate
9$:		.dw rfrom,base,store
		.dw true
		.dw doret

;-------------------------------------------------------------------------------
;
;		DICTIONARY SEARCH
;
;-------------------------------------------------------------------------------

.if REPL

; L>NAME	lfa -- nt
;		convert link field address (lfa) to name token or name field address (nfa)

		CODE L>NAME,ltoname
		jp twoplus		; same as 2+

; NAME>STRING	nt -- c-addr u
;		convert name token or name field address (nfa) to string

		COLON NAME>STRING,nametostring
		.dw count,dolit,length_mask,and
		.dw doret

; NAME>		nt -- xt
;		convert name token or name field address (nfa) to execution token or call field address (cfa)

		COLON NAME>,namefrom
		.dw nametostring,plus
		.dw doret

; >NAME		xt -- nt
;		convert execution token or call field address (cfa) to name token or name field address (nfa);
;		may throw -24 "invalid numeric argument"

		CODE >NAME,toname
		push bc			; save bc with ip
		ld b,0			; 0 -> b for add hl,bc
		ld hl,(context+3)	; CONTEXT -> hl
		; loop over dictionary
1$:		ld a,(hl)	;  7	; loop
		inc hl		;  6	;
		ld h,(hl)	;  7	;
		ld l,a		;  4	;   [hl] -> hl follow link
		or h		;  4	;
		jr z,3$		;  7	;   if hl = 0 then throw -24
		push hl		; 11	;   save hl with lfa
		inc hl		;  6	;
		inc hl		;  6	;   hl + 2 -> hl with nt (nfa)
		ld a,(hl)	;  7	;   get word length
		bit smudge_bit,a;  8	;
		jr nz,2$	;  7	;   if smudge bit not set then
		and length_mask	;  7	;     ignore control bits
		inc a		;  4	;
		ld c,a		;  4	;     word length + 1
		add hl,bc	; 11	;     hl + length + 1 -> hl with cfa
		sbc hl,de	; 15	;     test if hl = de with xt
2$:		pop hl		; 10	;   restore hl with lfa
		jr nz,1$	; 12	; until hl = xt matches
		; found the matching word
		inc hl			;
		inc hl			; hl + 2 - hl with nt (nfa)
		pop bc			; restore bc with ip
		ex de,hl		; set new TOS to hl
		JP_NEXT			; continue
		; not found
3$:		ld a,-24		;
		jp throw_a		; throw -24 "invalid numeric argument"

; >BODY		xt -- addr
;		convert execution token to parameter field address (pfa)

		CODE >BODY,tobody
		inc de			;
		jp twoplus		; set de + 3 -> de new TOS and continue

; FIND-WORD	c-addr u -- c-addr 0 | xt 1 | xt -1
;		search dictionary for matching word (case insensitive);
;		leaves execution token and 1 = immediate or -1 = not immediate;
;		leaves c-addr and 0 when not found

		CODE FIND-WORD,findword
		ld a,d			;
		or a			; test d = 0 high order byte of u
		jp nz,zero_next		; if u is too large then set new TOS to 0
		sla e			; shift u to compare w/o immediate bit
		jp c,zero_next		; if u is too large then set new TOS to 0
		jp z,zero_next		; if u = 0 then set new TOS to 0
		push de			; save de with 2*u
		EXXSVBC			; exx or save bc with ip
		pop bc			; pop 2 * u -> bc
		pop de			; pop c-addr -> de
		ld hl,(context+3)	; CONTEXT -> hl
		jr 3$			; start searching
		; loop over dictionary
1$:		pop de			; restore de with c-addr
2$:		pop hl		; 10	; loop, restore hl with lfa
3$:		ld a,(hl)	;  7	;
		inc hl		;  6	;
		ld h,(hl)	;  7	;
		ld l,a		;  4	;   [hl] -> hl follow link at hl = lfa
		or h		;  4	;
		jr z,6$		;  7	;   if hl = 0 then not found
		push hl		; 11	;   save hl with lfa
		inc hl		;  6	;
		inc hl		;  6	;   hl + 2 -> hl with nt (nfa)
		ld a,(hl)	;  7	;   word length
		add a		;  4	;   shift away immediate bit
		cp c		;  4	;   test a = c word length match (both shifted)
		jr nz,2$	; 12(95);   if lengths differ then continue searching
		; compare string to word
		push de			;   save de with c-addr
		inc hl			;   hl++ point to nfa chars
		ld b,c			;   2 * u -> b
		srl b			;   u -> b word length (nonzero)
		; loop over word chars
4$:		ld a,(de)	;  7	;   loop
		cp (hl)		;  7	;     compare [de] = [hl]
		jr z,5$		; 12/7	;     if mismatch then
		and 0xdf	;    7	;       make upper case
		cp 'A		;    7	;
		jr c,1$		;    7	;       if a < 'A' then continue search
		cp 'Z+1		;    7	;
		jr nc,1$	;    7	;       if a > 'Z' then continue search
		xor (hl)	;    7	;
		and 0xdf	;    7	;       case insensitive compare [de] = [hl]
		jr nz,1$	;    7	;       if mismatch then continue search
5$:		inc de		;  6	;     de++ point to next char of c-addr
		inc hl		;  6	;     hl++ point to next char of word
		djnz 4$		; 13(51/102);until --b = 0
		; found a matching word
		pop de			;   discard saved c-addr
		ex (sp),hl		;   save hl with xt as 2OS, restore hl with lfa
		inc hl			;
		inc hl			;   hl + 2 -> hl with nt (nfa)
		bit immediate_bit,(hl)	;   test immediate bit of [hl] word length
		EXXLDBC			;   exx or restore bc with ip
		jp nz,one_next		;   set new TOS to 1 if word is immediate
		jp mone_next		;   set new TOS to -1
		; not found
6$:		push de			; save de with c-addr as 2OS
		EXXLDBC			; exx or restore bc with ip
		jp zero_next		; set new TOS to 0

; '		"<spaces>name<space>" -- xt
;		parse name and search the dictionary to leave its execution token;
;		may throw -13 "undefined word"
;
;    : ' PARSE-NAME FIND-WORD 0= IF -13 THROW THEN ;

		COLON ',tick
		.dw parsename
		.dw findword,zeroequal,doif,1$
		.dw   dolit,-13,throw
1$:		.dw doret

; FIND		c-addr -- c-addr 0 | xt 1 | xt -1
;		search dictionary for the counted string to match a word (case insensitive);
;		leaves execution token and 1 = immediate or -1 = not immediate;
;		leaves c-addr and 0 when not found
;
;    : FIND COUNT FIND-WORD ;

		COLON FIND,find
		.dw count,findword
		.dw doret

; WORDS		--
;		display context vocabulary words one screen full at a time

		COLON WORDS,words
		.dw cr
		.dw zero			; -- 0
		.dw context			; -- 0 l
1$:		.dw fetch,qdup,doif,5$
		.dw   dup,ltoname		; -- n l nfa
		.dw   dup,cfetch,dolit,smudge_mask,and,doif,2$
		.dw     drop
		.dw   doahead,4$
2$:		.dw     rot,swap		; -- l n nfa
		.dw     nametostring		; -- l n c-addr u
		.dw     rot,over,plus,oneplus	; -- l c-addr u n+u+1
		.dw     dup,maxxy,oneminus,star,umore,doif,3$
		.dw       drop
		.dw       dup			; -- l c-addr u u
.if FCBN
		.dw       outputid,zeroequal,doif,3$
.endif
		.dw         key,drop		; wait for key when outputting to console
3$:		.dw     mrot			; -- l u|n+u+1 c-addr u
		.dw     type,space
		.dw     swap			; -- u|n+u+1 l
4$:		.dw doagain,1$
5$:		.dw drop
		.dw doret

.endif;REPL

;-------------------------------------------------------------------------------
;
;		COMPILING
;
;-------------------------------------------------------------------------------

; HERE		-- addr
;		address of free memory after the dictionary;
;		new definitions are added here;
;		note that numeric output words use HERE for conversion

		VALUE HERE,here
		.dw end

; UNUSED	-- u
;		unused dictionary space
;
;    : UNUSED top @ HERE - ;

		CODE UNUSED,unused
		push de			; save TOS
		ld de,(here+3)		; HERE -> de
		ld hl,(top)		; [top] -> hl
		or a			; 0 -> cf
		sbc hl,de		; [top] - HERE -> hl
		ex de,hl		; set new TOS to hl
		JP_NEXT			; continue

.if REPL

; LASTXT	-- xt
;		leaves the last execution token defined
;
;    0 VALUE LASTXT

		VALUE LASTXT,lastxt
		.dw forth

; RECURSE	... -- ...
;		recursively call the currently defined word;
;		may throw -14 "interpreting a compile-only word";
;		recursion depth should not exceed available return stack space
;
;    : RECURSE ?COMP LASTXT COMPILE, ; IMMEDIATE
;    : RECURSE ?COMP ['] ?RP COMPILE, LASTXT COMPILE, ; IMMEDIATE

		COLON_IMM RECURSE,recurse
		.dw qcomp
.if SAFR
		.dw dolit,qrp,compilecomma
.endif
		.dw lastxt,compilecomma
		.dw doret

; STATE		-- addr
;		compilation state;
;		STATE @ leaves TRUE when compiling;
;		STATE @ leaves FALSE when interpreting
;
;    VARIABLE STATE

		VARIABLE STATE,state
		.dw 0

; [		--
;		switch state to interpreting
;
;    : [ STATE OFF ;

		CODE_IMM [,leftbracket
		ld hl,0			; 0 -> hl
store_state:	ld (state+3),hl		; hl -> STATE
		JP_NEXT			; continue

; ]		--
;		switch state to compiling
;
;    : ] STATE ON ;

		CODE ],rightbracket
		ld hl,-1		; -1 -> hl
		jr store_state		; hl -> STATE and contnue

; [']		"<spaces>name<space>" -- ; -- xt
;		compile xt of name as literal;
;		may throw -14 "interpreting a compile-only word"
;
;    : ['] ?COMP ' LITERAL ; IMMEDIATE

		COLON_IMM ['],brackettick
		.dw qcomp
		.dw tick,literal
		.dw doret

; [CHAR]	"<spaces>char" -- ; -- char
;		compile char as literal;
;		note that the syntax 'char is preferred instead of this legacy word;
;		may throw -14 "interpreting a compile-only word"
;
;    : [CHAR] ?COMP CHAR LITERAL ; IMMEDIATE

		COLON_IMM [CHAR],bracketchar
		.dw qcomp
		.dw char,literal
		.dw doret

; [COMPILE]	"<space>name<space>" -- ; ... -- ...
;		compile name;
;		note that POSTPONE is preferred instead of this legacy word;
;		may throw -14 "interpreting a compile-only word"
;
;    : [COMPILE] ?COMP ' COMPILE, ; IMMEDIATE

		COLON_IMM [COMPILE],bracketcompile
		.dw qcomp
		.dw tick,compilecomma
		.dw doret

; HIDE		--
;		hide the last definition
;
;    : HIDE CURRENT @ L>NAME DUP C@ smudge_mask OR SWAP C! ;

		COLON HIDE,hide
		.dw current,fetch
		.dw ltoname,dup,cfetch
		.dw dolit,smudge_mask,or
		.dw swap,cstore
		.dw doret

; REVEAL	--
;		reveal the last definition, i.e. unhide it
;
;    : REVEAL CURRENT @ L>NAME DUP C@ ~smudge_mask AND SWAP C! ;

		COLON REVEAL,reveal
		.dw current,fetch
		.dw ltoname,dup,cfetch
		.dw dolit,~smudge_mask,and
		.dw swap,cstore
		.dw doret

; IMMEDIATE	--
;		make the last definition immediate
;
;    : IMMEDIATE CURRENT @ L>NAME DUP C@ immediate_mask OR SWAP C! ;

		COLON IMMEDIATE,immediate
		.dw current,fetch
		.dw ltoname,dup,cfetch
		.dw dolit,immediate_mask,or
		.dw swap,cstore
		.dw doret

; ?COMP		--
;		check if compiling;
;		may throw -14 "interpreting a compile-only word"

		CODE ?COMP,qcomp
		ld a,(state+3)		;
		or a			;
		ld a,-14		; "interpreting a compile-only word"
		jp z,throw_a		;
		JP_NEXT			;

; ?SYS		-- ; C: x --
;		check if compiled control structure matches x;
;		may throw -14 "interpreting a compile-only word";
;		may throw -22 "control structure mismatch"

		COLON ?SYS,qsys
		.dw qcomp
		.dw notequal,doif,1$
		.dw   dolit,-22,throw
1$:		.dw doret

; ALLOT		n --
;		allocate n bytes starting from HERE in the dictionary;
;		undo the last ALLOT with negative n to reclaim memory,
;		but beware: don't use negative n when new words were defined;
;		may throw -8 "dictionary overflow"

		CODE ALLOT,allot
		ld hl,(here+3)		; HERE -> hl
		add hl,de		; hl + de -> hl
allot_check:	ld de,(top)		; [top] -> de
		or a			; 0 -> cf
		sbc hl,de		;
		add hl,de		; test hl < [top]
		ld a,-8			; -8 "dictionary overflow"
		jp nc,throw_a		;
		ld (here+3),hl		; hl -> HERE
		pop de			; pop new TOS
		JP_NEXT			; continue

; COMPILE,	xt --
;		append execution token to dictionary;
;		may throw -8 "dictionary overflow"
;
;    : COMPILE, , ;

		CODE ^|COMPILE,|,compilecomma
		jr comma		; same as ,

; ,		x --
;		append cell to dictionary;
;		may throw -8 "dictionary overflow"

		CODE ^|,|,comma
		ld hl,(here+3)		; HERE -> hl
comma_de:	ld (hl),e		;
		inc hl			;
		ld (hl),d		;
		inc hl			; de -> [hl++]
		jr allot_check		; check and set hl -> HERE

; C,		char --
;		append char to dictionary;
;		may throw -8 "dictionary overflow"

		CODE ^|C,|,ccomma
		ld hl,(here+3)		; HERE -> hl
		ld (hl),e		;
		inc hl			; e -> [hl++]
		jr allot_check		; check and set hl -> HERE

; 2,		x1 x2 --
;		append double cell to dictionary;
;		may throw -8 "dictionary overflow"
;
;    : 2, , , ;

		COLON ^|2,|,twocomma
		.dw comma,comma
		.dw doret

; NFA,		c-addr u --
;		append dictionary entry with name string;
;		set LASTXT to HERE;
;		may throw -8 "dictionary overflow"
;
;    : NFA, HERE CURRENT @ , CURRENT ! DUP C, HERE SWAP DUP ALLOT CMOVE HERE TO LASTXT ;

		COLON ^|NFA,|,nfacomma
		.dw here
		.dw current,fetch,comma
		.dw current,store
		.dw dup,ccomma
		.dw here,swap,dup,allot,cmove
		.dw here,doto,lastxt+3
		.dw doret

; CODE		"<spaces>name<space>" --
;		parse name and append dictionary entry with name to execute machine code;
;		machine code should be appended to CODE definitions;
;		set LASTXT to HERE;
;		may throw -8 "dictionary overflow"
;
;    : CODE PARSE-NAME NFA, ;

		COLON ^|CODE|,code
		.dw parsename,nfacomma
		.dw doret

; CFA,		addr --
;		append cfa call addr to dictionary;
;		may throw -8 "dictionary overflow"

		CODE ^|CFA,|,cfacomma
		ld hl,(here+3)		; HERE -> hl
		ld (hl),0xcd		; Z80 'call nn' opcode
		inc hl			; 0xcd -> [hl++]
		jr comma_de		; append addr, check and set hl -> HERE

; :CFA		-- addr colon_sys
;		append cfa colon definition with cfa call addr to dictionary;
;		make CONTEXT the CURRENT vocabulary;
;		start compiling and leave HERE and colon_sys to end compiling with the ; word;
;		may throw -8 "dictionary overflow"
;
;    : :CFA ] HERE colon_sys ['] (:) CFA, CURRENT TO CONTEXT ;

		COLON ^|:CFA|,coloncfa
		.dw rightbracket
		.dw here
		.dw dolit,colon_sys
		.dw dolit,docol,cfacomma
		.dw current,doto,context+3
		.dw doret

; CFA=		xt1 xt2 -- flag
;		true if xt1 has a cfa equal to a call to addr xt2;
;		used for introspection of
;		VARIABLE, 2VARIABLE and CREATE without DOES> that leave a pfa (VAR),
;		CREATE with DOES> (DOES),
;		VALUE (VAL), 2VALUE (2VAL),
;		CONSTANT (CON), 2CONSTANT (2CON),
;		DEFER (DEF), for example:
;		    3 VALUE X
;		    ' X ' (VAL) CFA=
;		leaves -1 (TRUE) meaning X is a VALUE which calls runtime (VAL)
;
;    : CFA= OVER C@ $CD = -ROT SWAP 1+ @ = AND ;

		CODE CFA=,cfaequal
		pop hl			; xt -> hl
		ld a,0xcd		; Z80 'call nn' opcode
		cp (hl)			; test if [hl] Z80 'call nn' opcode
		jr nz,1$		; if [hl] Z80 'call nn' opcode then
		inc hl			;
		ld a,e			;
		cp (hl)			;
		jr nz,1$		;   if [++hl] = e then
		inc hl			;
		ld a,d			;
		cp (hl)			;     test [++hl] = d
1$:		ld a,0			; require a = 0
		jp true_if_z_next	; set new TOS to TRUE if xt matches else FALSE

; POSTPONE	"<spaces>name<space>" --
;		postpone compile action of name;
;		if name is immediate, then compile name instead of executing it;
;		otherwise compile name into the current colon definition;
;		can be used to create macros, e.g. : TRUE POSTPONE -1 ; IMMEDIATE;
;		may throw -13 "undefined word";
;		may throw -14 "interpreting a compile-only word"

		COLON_IMM POSTPONE,postpone
		.dw qcomp
		.dw parsename,findword,qdup,zeroequal,doif,1$
		.dw   dolit,-13,throw
1$:		.dw zeroless,doif,2$
		.dw   literal
		.dw   dolit,compilecomma
2$:		.dw compilecomma
		.dw doret

; BUFFER:	n "<spaces>name<space>" -- ; -- addr
;		define buffer with n bytes;
;		executing name leaves address of n bytes
;
;    : BUFFER: CREATE ALLOT ;

		COLON ^|BUFFER:|,buffercolon
		.dw create
		.dw allot
		.dw doret

; :NONAME	-- xt
;		colon definition without name;
;		leaves the execution token of the definition to be used or saved
;
;    : :NONAME HERE DUP TO LASTXT :CFA ;

		COLON ^|:NONAME|,colonnoname
		.dw here,dup,doto,lastxt+3
		.dw coloncfa
		.dw doret

; :		-- ; C: "<spaces>name<space>" -- addr colon_sys
;		define name and start compiling
;
;    : : CODE HIDE :CFA ;

		COLON ^|:|,colon
		.dw code
		.dw hide
		.dw coloncfa
		.dw doret

; ;		-- ; C: addr colon_sys --
;		end colon definition and stop compiling;
;		may throw -14 "interpreting a compile-only word";
;		may throw -22 "control structure mismatch"
;
;    : ; ?COMP colon_sys <> IF -22 THROW THEN DROP POSTPONE (;) REVEAL [ ; IMMEDIATE

		COLON_IMM ^|;|,semicolon
		.dw dolit,colon_sys,qsys
		.dw drop
		.dw dolit,doret,compilecomma
		.dw reveal
		.dw leftbracket
		.dw doret

; EXIT		--
;		exit colon definition;
;		to EXIT the definition from within a DO-LOOP, first call UNLOOP
;
;    : EXIT ?COMP POSTPONE (EXIT) ; IMMEDIATE

		COLON_IMM EXIT,exit
		.dw qcomp
		.dw dolit,doexit,compilecomma
		.dw doret

; CREATE	"<spaces>name<space>" -- ; -- addr
;		create name;
;		executing name leaves address (HERE address after CREATE)
;
;    : CREATE CODE ['] (VAR) CFA, ;

		COLON CREATE,create
		.dw code
		.dw dolit,dovar,cfacomma
		.dw doret

; DOES>		-- ; ... -- ...
;		change CREATE name behavior to execute code after DOES>
;
;    : DOES> ?COMP POSTPONE (;DOES) ['] (DOES) CFA, ; IMMEDIATE

		COLON_IMM DOES>,does
		.dw qcomp
		.dw dolit,dosemicolondoes,compilecomma
		.dw dolit,dodoes,cfacomma
		.dw doret

; VARIABLE	"<spaces>name<space>" -- ; -- addr
;		define a variable;
;		executing name leaves address of value (initialized to zero)
;
;    : VARIABLE CREATE 0 , ;

		COLON VARIABLE,variable
		.dw create
		.dw zero,comma
		.dw doret

; 2VARIABLE	"<spaces>name<space>" -- ; -- addr
;		define a double variable;
;		executing name leaves address of double value (initialized to zero)
;
;    : 2VARIABLE CREATE 0 0 2, ;

		COLON 2VARIABLE,twovariable
		.dw create
		.dw zero,zero,twocomma
		.dw doret

; CONSTANT	x "<spaces>name<space>" -- ; -- x
;		define a constant;
;		executing name leaves x
;
;    : CONSTANT CODE ['] (CON) CFA, , ;
;    : CONSTANT CREATE , DOES> @ ;

		COLON CONSTANT,constant
		.dw code
		.dw dolit,docon,cfacomma
		.dw comma
		.dw doret

; 2CONSTANT	x1 x2 "<spaces>name<space>" -- ; -- x1 x2
;		define a double constant;
;		executing name leaves x1 x2
;
;    : 2CONSTANT CODE ['] (2CON) CFA, 2, ;
;    : 2CONSTANT CREATE 2, DOES> 2@ ;

		COLON 2CONSTANT,twoconstant
		.dw code
		.dw dolit,dotwocon,cfacomma
		.dw twocomma
		.dw doret

; VALUE		x "<spaces>name<space>" -- ; -- x
;		define a value;
;		executing name leaves x
;
;    : VALUE CODE ['] (VAL) CFA, , ;

		COLON VALUE,value
		.dw code
		.dw dolit,doval,cfacomma
		.dw comma
		.dw doret

; 2VALUE	dx "<spaces>name<space>" -- ; -- dx
;		define a double value;
;		executing name leaves dx
;
;    : 2VALUE CODE ['] (2VAL) CFA, 2, ;

		COLON 2VALUE,twovalue
		.dw code
		.dw dolit,dotwoval,cfacomma
		.dw twocomma
		.dw doret

; TO		"<spaces>name<space>" -- ; x --
;		assign value name;
;		may throw -32 "invalid name argument"
;
;    : TO
;      '
;      DUP ['] (VAL) CFA= IF
;        >BODY
;        STATE @ IF
;          POSTPONE (TO)
;          ,
;          EXIT
;        THEN
;        !
;        EXIT
;      THEN
;      DUP ['] (2VAL) CFA= IF
;        >BODY
;        STATE @ IF
;          POSTPONE (2TO)
;          ,
;          EXIT
;        THEN
;        2!
;        EXIT
;      THEN
;      -32 THROW ; IMMEDIATE

		COLON_IMM TO,to
		.dw tick
		.dw dup,dolit,doval,cfaequal,doif,2$
		.dw   tobody
		.dw   state,fetch,doif,1$
		.dw     dolit,doto,compilecomma
		.dw     comma
		.dw     doexit
1$:		.dw   store
		.dw   doexit
2$:		.dw dup,dolit,dotwoval,cfaequal,doif,4$
		.dw   tobody
		.dw   state,fetch,doif,3$
		.dw     dolit,dotwoto,compilecomma
		.dw     comma
		.dw     doexit
3$:		.dw   twostore
		.dw   doexit
4$:		.dw dolit,-32,throw
		.dw doret

; +TO		"<spaces>name<space>" -- ; n --
;		increment value name;
;		may throw -32 "invalid name argument"
;
;    : +TO
;      '
;      DUP ['] (VAL) CFA= IF
;        >BODY
;        STATE @ IF
;          POSTPONE (+TO)
;          ,
;          EXIT
;          THEN
;        +!
;        EXIT
;      THEN
;      DUP ['] (2VAL) CFA= IF
;        >BODY
;        STATE @ IF
;          POSTPONE (D+TO)
;          ,
;          EXIT
;          THEN
;        D+!
;        EXIT
;      -32 THROW ; IMMEDIATE

		COLON_IMM +TO,plusto
		.dw tick
		.dw dup,dolit,doval,cfaequal,doif,2$
		.dw   tobody
		.dw   state,fetch,doif,1$
		.dw     dolit,doplusto,compilecomma
		.dw     comma
		.dw     doexit
1$:		.dw   plusstore
		.dw   doexit
2$:		.dw dup,dolit,dotwoval,cfaequal,doif,4$
		.dw   tobody
		.dw   state,fetch,doif,3$
		.dw     dolit,dodplusto,compilecomma
		.dw     comma
		.dw     doexit
3$:		.dw   dplusstore
		.dw   doexit
4$:		.dw dolit,-32,throw
		.dw doret

; DEFER		"<spaces>name<space>" -- ; ... -- ...
;		define a deferred name
;
;    : DEFER CODE ['] (DEF) CFA, ['] UNDEF , ;

		COLON DEFER,defer
		.dw code
		.dw dolit,dodef,cfacomma
		.dw dolit,undef,comma
		.dw doret
undef:		push de			; save TOS
		ld de,-256		; set new TOS
		jp throw		; THROW

; DEFER!	xt1 xt2 --
;		store xt1 in deferred xt2
;
;    : DEFER! >BODY ! ;

		CODE DEFER!,deferstore
		inc de			;
		inc de			;
		inc de			; >BODY
		jp store		; !

; DEFER@	xt1 -- xt2
;		fetch execution token from deferred xt1
;
;    : DEFER@ >BODY @ ;

		CODE DEFER@,deferfetch
		inc de			;
		inc de			;
		inc de			; >BODY
		jp fetch		; @

; IS		xt "<spaces>name<space>" --
;		assign execution token to deferred name;
;		may throw -32 "invalid name argument"
;
;    : IS
;      '
;      DUP ['] (DEF) CFA= IF
;        STATE @ IF
;          LITERAL
;          POSTPONE DEFER!
;          EXIT
;        THEN
;        DEFER!
;        EXIT
;      THEN
;      #-32 THROW ; IMMEDIATE

		COLON_IMM IS,is
		.dw tick
		.dw dup,dolit,dodef,cfaequal,doif,2$
		.dw   state,fetch,doif,1$
		.dw     literal
		.dw     dolit,deferstore,compilecomma
		.dw     doexit
1$:		.dw   deferstore
		.dw   doexit
2$:		.dw dolit,-32,throw
		.dw doret

; ACTION-OF	"<spaces>name<space>" -- xt
;		fetch execution token of deferred name;
;		may throw -32 "invalid name argument"
;
;    : ACTION-OF
;      '
;      DUP ['] (DEF) CFA= IF
;        STATE @ IF
;          LITERAL
;          POSTPONE DEFER@
;          EXIT
;        THEN
;        DEFER@
;        EXIT
;      THEN
;      #-32 THROW ; IMMEDIATE

		COLON_IMM ACTION-OF,actionof
		.dw tick
		.dw dup,dolit,dodef,cfaequal,doif,2$
		.dw   state,fetch,doif,1$
		.dw     literal
		.dw     dolit,deferfetch,compilecomma
		.dw     doexit
1$:		.dw   deferfetch
		.dw   doexit
2$:		.dw dolit,-32,throw
		.dw doret

; LITERAL	x -- ; -- x
;		compile a literal
;
;    : LITERAL ?COMP POSTPONE (LIT) , ; IMMEDIATE

		COLON_IMM LITERAL,literal
		.dw qcomp
		.dw dolit,dolit,compilecomma
		.dw comma
		.dw doret

; 2LITERAL	x1 x2 -- ; -- x1 x2
;		compile a double literal
;
;    : 2LITERAL ?COMP POSTPONE (2LIT) 2, ; IMMEDIATE

		COLON_IMM 2LITERAL,twoliteral
		.dw qcomp
		.dw dolit,dotwolit,compilecomma
		.dw twocomma
		.dw doret

; SLITERAL	c-addr u -- ; -- c-addr u
;		compile a string literal;
;		max literal string length is 255
;
;    : SLITERAL
;      ?COMP
;      DUP 255 U> IF -18 THROW THEN
;      POSTPONE (SLIT)
;      DUP C,
;      HERE OVER ALLOT SWAP CMOVE ; IMMEDIATE

		COLON_IMM SLITERAL,sliteral
		.dw qcomp
		.dw dup,dolit,255,umore,doif,1$
		.dw   dolit,-18,throw
1$:		.dw dolit,doslit,compilecomma
		.dw dup,ccomma
		.dw here,over,allot,swap,cmove
		.dw doret

; ."		"ccc<quote>" -- ; --
;		type "ccc" (compiled, not interpreted)
;
;    : ." ?COMP '" PARSE SLITERAL POSTPONE TYPE ; IMMEDIATE

		COLON_IMM ^|."|,dotquote
		.dw qcomp
		.dw dolit,'",parse
		.dw sliteral
		.dw dolit,type,compilecomma
		.dw doret

; C"		"ccc<quote>" -- ; -- c-addr
;		leave counted string "ccc" (compiled, not interpreted);
;		may throw -18 "parsed string overflow"
;
;    : C" ?COMP POSTPONE S" POSTPONE DROP POSTPONE 1- ; IMMEDIATE

		COLON_IMM ^|C"|,cquote
		.dw qcomp
		.dw squote
		.dw dolit,drop,compilecomma
		.dw dolit,oneminus,compilecomma
		.dw doret

; S"		"ccc<quote>" -- ; -- c-addr u
;		leave string "ccc" (compiled and interpreted);
;		truncates string to 255 characters long when excessive
;
;    : S" '" PARSE SDUP ; IMMEDIATE

		COLON_IMM ^|S"|,squote
		.dw dolit,'",parse,sdup
		.dw doret

.if XTRA

;+ S\"		"ccc<quote>" -- ; -- c-addr u
;		leave string "ccc" (compiled and interpreted);
;		truncates string to 255 characters long when excessive
;
;    : S\" '" PARSE S\>S SDUP ; IMMEDIATE

		COLON_IMM ^|S\\"|,sbackslashquote
		.dw dolit,'",parse,sbackslashtos,sdup
		.dw doret

;+ S\>S		c-addr u -- c-addr u
;		convert string in place by translating \-escape codes
;
;		code | corresponding ASCII character code
;		---- | ----------------------------------------------------------
;		\\   | \ backslash (92)
;		\a   | BEL (7)
;		\b   | BS (8)
;		\e   | ESC (27)
;		\f   | FF (12)
;		\l   | LF (10)
;		\m   | CR/LF (13 then 10)
;		\n   | LF (10)
;		\q   | " quote (34)
;		\r   | CR (13)
;		\t   | TAB (9)
;		\v   | VT (11)
;		\xhh | hh in hex (0xhh)
;		\z   | NUL (0)
;		\G   | MSX graphic character header (1) e.g. \GA

		CODE S\\>S,sbackslashtos
		pop hl			;
		push hl			; 2OS -> hl with c-addr
		push bc			; save bc with ip
		ld c,e			;
		ld b,d			; u -> bc
		ld e,l			;
		ld d,h			; hl -> de with c-addr
		; translate
1$:		ld a,c			;
		or b			;
		jr z,3$			; if bc = 0 then done
		ld a,'\			;
2$:		cp (hl)		;  7	; loop
		jr z,4$		;  7	;   if [hl] = '\' then goto translate
		ldi		; 16	;   [hl++] -> [de++], bc--
		jp pe,2$	; 10(40); until bc = 0
		; done
3$:		pop bc			; restore bc with ip
		ex de,hl		; de -> hl
		pop de			;
		push de			;
		or a			; 0 -> cf
		sbc hl,de		; hl - de -> hl length of converted string
		ex de,hl		; set new TOS to length of converted string
		JP_NEXT			; continue
		; translate \-escape
4$:		inc hl			; hl++ over the \ char
		dec bc			; bc-- for the \ char
		ld a,c			;
		or b			;
		jr z,3$			; if bc = 0 then done
		dec bc			; bc-- for next char after \ char
5$:		ld a,(hl)		; [hl++] -> a with char after \ char
		DINT			;
		inc hl			;
		exx			;
		ld hl,11$		; lookup table -> hl'
		ld b,13			; with 13 entries -> b'
6$:		cp (hl)			; loop
		inc hl			;
		jr z,9$			;   if a = [hl'++] then goto 9
		inc hl			;   hl'++
		djnz 6$			; until --b' = 0
		exx			;
		EINT			;
		cp 'x			; if a = 'x' then
		jr nz,10$		;
		ld a,c			;
		or b			;
		jr z,3$			; if bc = 0 then done
		dec bc			; bc--
		ld a,c			;
		or b			;
		jr z,3$			; if bc = 0 then done
		dec bc			; bc--
		ld a,(hl)		; [nl++] -> a with hex digit
		inc hl			;
		sub '0			;
		cp 10			;
		jr c,7$			;
		sub 7			;
7$:		add a			;
		add a			;
		add a			;
		add a			; 4 msb bits from hex digit -> a
		ex af,af'		;
		ld a,(hl)		; [hl++] -> a with hex digit
		sub '0			;
		cp 10			;
		jr c,8$			;
		sub 7			;
		and 0x0f		; 4 lsb bits from hex digit -> a'
8$:		ld (hl),a		; save a' -> [hl]
		ex af,af'		;
		or (hl)			; lsb + msb 4 bits -> a
		inc hl			; hl++
		jr 10$			; goto store translated code in a
		; get translated code from table
9$:		cp a,'m			; test if a = 'm' 
		ld a,(hl)		; [hl'] -> a with translated code
		exx			;
		EINT			;
		jr nz,10$		; if tested a = 'm' then
		ld (de),a		;   13 -> [de++] with CR of CR/LF pair
		inc de			;
		ld a,10			;   10 -> a with LF of CR/LF pair
10$:		ld (de),a		; store a -> [de++]
		inc de			;
		jr 1$			; loop again
		; \-escape translation table
11$:		.db 'a,7		; BEL
		.db 'b,8		; BS
		.db 'e,27		; ESC
		.db 'f,12		; FF
		.db 'l,10		; LF
		.db 'm,13		; CR/LF pair 13,10
		.db 'n,10		; LF
		.db 'q,34		; quote
		.db 'r,13		; CR
		.db 't,9		; TAB
		.db 'v,11		; VT
		.db 'z,0		; NUL
		.db 'G,1		; MSX graphic header

.endif;XTRA
.endif;REPL

; SDUP		c-addr1 u -- c-addr2 u
;		duplicate string to a TMP buffer or to a string literal when compiling;
;		stores the copy of the string as a counted string at c-addr2 - 1;
;		truncates the copy to 255 characters long when excessive
;
;    : SDUP
;      STATE @ IF
;        SLITERAL
;        EXIT
;      THEN
;      255 UMIN
;      TMP
;      2DUP C!
;      1+
;      SWAP
;      2DUP 2>R
;      CMOVE
;      2R> ;

		COLON SDUP,sdup
.if REPL
		.dw state,fetch,doif,1$
		.dw   sliteral
		.dw   doexit
.endif
1$:		.dw dolit,255,umin
		.dw tmp
		.dw twodup,cstore
		.dw oneplus
		.dw swap
		.dw twodup,twotor
		.dw cmove
		.dw tworfrom
		.dw doret

;-------------------------------------------------------------------------------
;
;		RUNTIME CONTROL
;
;-------------------------------------------------------------------------------

; (UNTIL)	x --
;		branch if x = 0;
;		runtime of the UNTIL compile-only word

		CODE (UNTIL),dountil
		ld a,e		;  4	;
		or d		;  4	; test if TOS = 0
		pop de		; 10	; set new TOS
		jr z,doagain	; 12/7	; (AGAIN) if TOS = 0
		jr skip_jp_next 	; skip jump target address and continue

; (IF)		x --
;		branch if x = 0;
;		runtime of the IF and WHILE compile-only words

		CODE (IF),doif
		ld a,e		;  4	;
		or d		;  4	; test if TOS = 0
		pop de		; 10	; set new TOS
		jr z,doahead	; 12/7 	; (AHEAD) if TOS = 0
skip_jp_next:	inc bc		;    7	;
		inc bc		;    7	; ip + 2 -> ip skip jump target address
		NEXT			; continue

; (AGAIN)	--
;		branch;
;		runtime of the AGAIN and REPEAT compile-only words

		CODE (AGAIN),doagain
		ld l,c		;  4	;
		ld h,b		;  4	;
		ld c,(hl)	;  7	;
		inc hl		;  6	;
		ld b,(hl)	;  7(28); [bc] -> bc
.if SAFE	; check stack underflow/overflow
.if UPHI
		ld hl,(sp1)	; 16	; -1 - [sp0] -> hl
.else
		ld hl,SP1	; 10	; -1 - SP0 -> hl
.endif
		add hl,sp	; 11	; test sp > [sp0] with sp-1-[sp0] > 0xffff
		jr c,1$		;  7	; if sp > [sp0] then throw -4
.if UPHI
		ld hl,(sp2)	; 16	; -([sp0] - s_size + 1) -> hl
.else
		ld hl,SP2	; 10	; -(SP0 - s_size + 1) -> hl
.endif
		add hl,sp	; 11	; if sp < [sp0] - s_size + 1 then throw -3
		jp c,cont	; 10(71|59|53); continue with break check
1$:		sbc a			; if c then throw -4 "stack underflow"
		sub 3			; else throw -3 "stack overflow"
		jp throw_a
.else
		NEXT			; continue
.endif

; (AHEAD)	--
;		branch;
;		runtime of the AHEAD, ELSE and ENDOF compile-only words

		CODE (AHEAD),doahead
		ld l,c		;  4	;
		ld h,b		;  4	;
		ld c,(hl)	;  7	;
		inc hl		;  6	;
		ld b,(hl)	;  7(28); [bc] -> bc
		NEXT			; continue

; (OF)		x1 x2 -- x1 or x1 x2 --
;		branch if x1 <> x2;
;		runtime of the OF compile-only word

		CODE (OF),doof
		pop hl			; pop x1 -> hl
		ex de,hl		; x2 -> hl, x1 -> de
		or a			; 0 -> cf
		sbc hl,de		; test x1 = x2
		jr nz,doahead		; AHEAD if x1 <> x2
		pop de			; pop new TOS
		jr skip_jp_next 	; skip jump target address and continue

; (+LOOP)	n --
;		increment loop counter by n and repeat loop unless counter crosses the limit;
;		runtime of the +LOOP compile-only word

		CODE (+LOOP),doplusloop
.if 1-RPIX
		ld ix,(rp)		; rp -> ix
.endif
		ld l,(ix+0)		;
		ld h,(ix+1)		; [rp] -> hl sliced loop counter (Laxen & Perry F83)
		or a			; 0 -> cf
		adc hl,de		; counter + step -> hl set flags
		jp pe,1$		; if overflow then exit loop
		ld (ix+0),l		;
		ld (ix+1),h		; hl -> [rp] save updated counter
		pop de			; pop new TOS
		jr doagain		; (AGAIN)
1$:		ld de,6			;
		add ix,de		;
.if 1-RPIX
		ld (rp),ix		; ix -> rp
.endif
		pop de			; pop new TOS
		jr skip_jp_next 	; skip jump target address and continue

; (LOOP)	--
;		increment loop counter by 1 and repeat loop unless loop counter crosses the limit;
;		runtime of the LOOP compile-only word

		CODE (LOOP),doloop
		push de			; save TOS
		ld de,1			; step by 1
		jr doplusloop		; (+LOOP)

; (?DO)		n1|u1 n2|u2 --
;		begin loop with limit n1|u1 and initial value n2|u2;
;		skip loop when zero trip loop;
;		runtime of the ?DO compile-only word

		CODE (?DO),doqdo
		pop hl			;
		push hl			; 2OS -> hl with loop limit
		or a			; 0 -> cf
		sbc hl,de		; test de = hl initial equals limit
		jr nz,dodo		; if de <> hl then (DO)
		pop hl			; discard 2OS
		pop de			; set new TOS
		jr doahead		; AHEAD

; (DO)		n1|u1 n2|u2 --
;		begin loop with limit n1|u1 and initial value n2|u2;
;		loop at least once;
;		runtime of the DO compile-only word

		CODE (DO),dodo
.if RPIX
		ex de,hl		; save TOS initial value -> hl
		ld de,-6		;
		add ix,de		; rp - 6 -> rp to make room for 3 loop parameters
.else
		ld ix,(rp)		; rp -> ix
		ex de,hl		; save TOS initial value -> hl
		ld de,-6		;
		add ix,de		; rp - 6 -> rp to make room for 3 loop parameters
		ld (rp),ix		; ix -> rp
.endif
		ex (sp),hl		; swap TOS <-> 2OS
		ex de,hl		; 2OS loop limit -> de
		ld hl,0x8000		; slice the loop limit in de (Laxen & Perry F83)
		or a			; 0 -> cf
		sbc hl,de		; slice 0x8000 - limit
		ld (ix+2),l		;
		ld (ix+3),h		; slice 0x8000 - limit -> [rp+2]
		pop de			; restore TOS initial value
		add hl,de		; slice initial + 0x8000 - limit
		ld (ix+0),l		;
		ld (ix+1),h		; slice initial + 0x8000 - limit -> [rp]
		LOAD (ix+5),(ix+4)	; [ip++] -> [rp+4] with the LEAVE address
		pop de			; pop new TOS
		JP_NEXT			; continue

; (UNLOOP)	R: ... --
;		remove loop parameters;
;		runtime of the UNLOOP compile-only word

		CODE (UNLOOP),dounloop
.if RPIX
		ex de,hl		; save TOS
		ld de,6			;
		add ix,de		; rp + 6 -> rp
		ex de,hl		; restore TOS
.else
		push de			; save TOS
		ld hl,(rp)		; rp -> hl
		ld de,6			;
		add hl,de		;
		ld (rp),hl		; rp + 6 -> rp
		pop de			; pop TOS
.endif
		JP_NEXT			; continue

; (?LEAVE)	x --
;		if x is nonzero (not FALSE) then discard the loop parameters and exit the innermost do-loop;
;		runtime of the ?LEAVE compile-only word

		CODE (?LEAVE),doqleave
		ld a,e			;
		or d			;
		pop de			; pop new TOS
		jr nz,doleave		; if x != 0 then LEAVE
		JP_NEXT			; continue

; (LEAVE)	--
;		discard the loop parameters and exit the innermost do-loop;
;		runtime of the LEAVE compile-only word

		CODE (LEAVE),doleave
.if RPIX
		ld bc,4			;
		add ix,bc		; rp + 4 -> rp
		jp doret		; LEAVE -> ip
.else
		ld hl,(rp)		;
		inc hl			;
		inc hl			;
		inc hl			;
		inc hl			;
		ld (rp),hl		; rp + 4 -> rp
		jp doret		; LEAVE -> ip
.endif

; I		-- n
;		the loop counter value of innermost do-loop

		CODE I,i
		push de			; save TOS
.if RPIX
		push ix			;
		pop hl			; rp -> hl
.else
		ld hl,(rp)		; rp -> hl
.endif
loop_counter:	ld e,(hl)		;
		inc hl			;
		ld d,(hl)		;
		inc hl			; [rp] -> de with loop counter
		ld a,(hl)		;
		inc hl			;
		ld h,(hl)		;
		ld l,a			; [rp+2] -> hl with loop limit
		ex de,hl		; exchange limit -> de, counter -> hl
		or a			; 0 -> cf
		sbc hl,de		; undo the do loop 'slice' counter - limit
		ex de,hl		; set new TOS to hl
		JP_NEXT			; continue

; J		-- n
;		the loop counter value of outer (second) do-loop

		CODE J,j
		push de			; save TOS
.if RPIX
		push ix			;
		pop hl			; rp -> hl
.else
		ld hl,(rp)		; rp -> hl
.endif
		ld de,6			;
		add hl,de		; rp + 6 -> hl
		jr loop_counter		; execute I with rp+6

; K		-- n
;		the loop counter value of outer (third) do-loop

		CODE K,k
		push de			; save TOS
.if RPIX
		push ix			;
		pop hl			; rp -> hl
.else
		ld hl,(rp)		;
.endif
		ld de,12		;
		add hl,de		; rp + 12 -> hl
		jr loop_counter		; execute I with rp+12

;-------------------------------------------------------------------------------
;
;		CONTROL
;
;-------------------------------------------------------------------------------

.if REPL

; AHEAD		-- ; C: -- addr orig
;		branch ahead to THEN;
;		may throw -14 "interpreting a compile-only word"

		COLON_IMM AHEAD,ahead
		.dw qcomp
		.dw dolit,doahead,compilecomma
		.dw here,dolit,orig
		.dw zero,comma
		.dw doret

; BEGIN		-- ; C: -- addr dest
;		begin WHILE REPEAT;
;		may throw -14 "interpreting a compile-only word"

		COLON_IMM BEGIN,begin
		.dw qcomp
		.dw here,dolit,dest
		.dw doret

; AGAIN		-- ; C: addr dest --
;		branch back to BEGIN;
;		may throw -14 "interpreting a compile-only word";
;		may throw -22 "control structure mismatch"

		COLON_IMM AGAIN,again
		.dw dolit,dest,qsys
		.dw dolit,doagain,compilecomma
		.dw comma
		.dw doret

; UNTIL		x -- ; C: addr dest --
;		branch back to BEGIN if x = 0 (FALSE);
;		may throw -14 "interpreting a compile-only word";
;		may throw -22 "control structure mismatch"

		COLON_IMM UNTIL,until
		.dw dolit,dest,qsys
		.dw dolit,dountil,compilecomma
		.dw comma
		.dw doret

; IF		x -- ; C: -- addr orig
;		branch to closest ELSE or THEN if x = 0 (FALSE);
;		may throw -14 "interpreting a compile-only word"

		COLON_IMM IF,if
		.dw qcomp
		.dw dolit,doif,compilecomma
		.dw here,dolit,orig
		.dw zero,comma
		.dw doret

; THEN		-- ; C: addr orig --
;		close AHEAD, IF, ELSE;
;		may throw -14 "interpreting a compile-only word";
;		may throw -22 "control structure mismatch"

		COLON_IMM THEN,then
		.dw dolit,orig,qsys
		.dw here,swap,store
		.dw doret

; ELSE		-- ; C: addr orig -- addr orig
;		close IF and branch to THEN;
;		may throw -14 "interpreting a compile-only word";
;		may throw -22 "control structure mismatch"

		COLON_IMM ELSE,else
		.dw ahead	; AHEAD
		.dw twoswap	; CS-ROLL
		.dw then	; POSTPONE THEN
		.dw doret

; WHILE		x -- ; C: addr sys -- addr orig addr sys
;		branch to exit REPEAT if x = 0 (FALSE);
;		may throw -14 "interpreting a compile-only word"

		COLON_IMM WHILE,while
		.dw if		; POSTPONE IF
		.dw twoswap	; CS-ROLL
		.dw doret

; REPEAT	-- ; C: addr orig addr dest --
;		branch back to BEGIN after WHILE;
;		may throw -14 "interpreting a compile-only word";
;		may throw -22 "control structure mismatch"

		COLON_IMM REPEAT,repeat
		.dw again	; POSTPONE AGAIN
		.dw then	; POSTPONE THEN
		.dw doret

; DO		n1|u1 n2|u2 -- ; C: -- addr do_sys
;		begin loop from initial value n2|u2 to the limit n1|u1;
;		loop at least once;
;		pushes do-loop parameters onto the return stack;
;		may throw -14 "interpreting a compile-only word"

		COLON_IMM DO,do
		.dw qcomp
		.dw dolit,dodo,compilecomma
		.dw here,dolit,do_sys
		.dw zero,comma
		.dw doret

; ?DO		n1|u1 n2|u2 -- ; C: -- addr do_sys
;		begin loop from initial value n2|u2 to the limit n1|u1;
;		pushes do-loop parameters onto the return stack;
;		skip loop when zero trip loop;
;		may throw -14 "interpreting a compile-only word"

		COLON_IMM ?DO,qdo
		.dw qcomp
		.dw dolit,doqdo,compilecomma
		.dw here,dolit,do_sys
		.dw zero,comma
		.dw doret

; LOOP		-- ; C: addr do_sys --
;		repeat loop unless loop counter crosses the limit;
;		may throw -14 "interpreting a compile-only word";
;		may throw -22 "control structure mismatch"

		COLON_IMM LOOP,loop
		.dw dolit,do_sys,qsys
		.dw dolit,doloop,compilecomma
		.dw dup,twoplus,comma
		.dw here,swap,store
		.dw doret

; +LOOP		n|u -- ; C: addr do_sys --
;		increment counter and repeat loop unless counter crosses the limit;
;		may throw -14 "interpreting a compile-only word";
;		may throw -22 "control structure mismatch"

		COLON_IMM +LOOP,plusloop
		.dw dolit,do_sys,qsys
		.dw dolit,doplusloop,compilecomma
		.dw dup,twoplus,comma
		.dw here,swap,store
		.dw doret

; UNLOOP	--
;		remove do-loop parameters from the return stack;
;		may throw -14 "interpreting a compile-only word"

		COLON_IMM UNLOOP,unloop
		.dw qcomp
		.dw dolit,dounloop,compilecomma
		.dw doret

; ?LEAVE	x --
;		if x is nonzero (not FALSE) then exit the innermost do-loop;
;		may throw -14 "interpreting a compile-only word"

		COLON_IMM ?LEAVE,qleave
		.dw qcomp
		.dw dolit,doqleave,compilecomma
		.dw doret

; LEAVE		--
;		exit the innermost do-loop;
;		may throw -14 "interpreting a compile-only word"

		COLON_IMM LEAVE,leave
		.dw qcomp
		.dw dolit,doleave,compilecomma
		.dw doret

; CASE		x -- ; C: -- 0
;		begin CASE ENDCASE switch;
;		may throw -14 "interpreting a compile-only word"

		COLON_IMM CASE,case
		.dw qcomp
		.dw zero
		.dw doret

; OF		x1 x2 -- x1 or x1 x2 -- ; C: n1 -- orig n2
;		take CASE arm if x1 = x2;
;		otherwise branch to next OF;
;		may throw -14 "interpreting a compile-only word"

		COLON_IMM OF,of
		.dw qcomp
		.dw oneplus,tor
		.dw dolit,doof,compilecomma
		.dw here,dolit,orig
		.dw zero,comma
		.dw rfrom
		.dw doret

; ENDOF		-- ; C: n -- orig n
;		branch to ENDCASE;
;		may throw -14 "interpreting a compile-only word";
;		may throw -22 "control structure mismatch"

		COLON_IMM ENDOF,endof
		.dw qcomp
		.dw tor
		.dw else
		.dw rfrom
		.dw doret

; ENDCASE	x -- ; C: n*orig n --
;		close CASE;
;		may throw -14 "interpreting a compile-only word";
;		may throw -22 "control structure mismatch"

		COLON_IMM ENDCASE,endcase
		.dw qcomp
		.dw dolit,drop,compilecomma
		.dw zero
		.dw doqdo,2$
1$:		.dw   then
		.dw doloop,1$
2$:		.dw doret

.endif;REPL

;-------------------------------------------------------------------------------
;
;		EXCEPTION HANDLING
;
;-------------------------------------------------------------------------------

; ERROR		n --
;		display exception n at the offending location in the input;
;		n = 0 return without displaying an error message;
;		n = -1 ABORT and n = -2 ABORT" also clears the parameter stack
;               n = -28 break is displayed as <STOP>
;		n = -56 QUIT stays silent;
;		other errors n are displayed as <ERR-n> switched to decimal,
;		a line number is included <ERR-n:line> when loading from files
;		list of Forth errors:
;
;		code | error
;		---- | ---------------------------------------------------------
;		-1   | ABORT
;		-2   | ABORT"
;		-3   | stack overflow
;		-4   | stack underflow
;		-5   | return stack overflow
;		-6   | return stack underflow
;		-8   | dictionary overflow
;		-10  | division by zero
;		-11  | result out of range
;		-13  | undefined word
;		-14  | interpreting a compile-only word
;		-15  | invalid FORGET
;		-16  | attempt to use zero-length string as a name
;		-18  | parsed string overflow
;		-19  | definition name too long
;		-22  | control structure mismatch
;		-24  | invalid numeric argument
;		-28  | user interrupt (BREAK was pressed)
;		-32  | invalid name argument (invalid TO name)
;		-36  | invalid file position
;		-37  | file I/O exception
;		-38  | non-existent file
;		-39  | unexpected end of file
;		-42  | floating-point divide by zero
;		-43  | floating-point result out of range
;		-46  | floating-point invalid argument
;		-56  | QUIT
;		-256 | execution of an uninitialized deferred word
;
;		code | MSX error = code + 256
;		---- | ---------------------------------------------------------
;		-197 | 59 file not open
;		-198 | 58 sequentual I/O only
;		-200 | 56 incorrect file name
;		-202 | 54 file already open
;		-203 | 53 file not found
;		-204 | 52 bad file number
;		-236 | 20 verify error
;		-231 | 25 line buffer overflow
;		-237 | 19 device I/O error
;		-243 | 13 type mismatch
;		-245 | 11 division by zero
;		-250 |  6 numeric overflow
;		-251 |  5 invalid function argument
;
;    : ERROR
;      DUP -2 0 WITHIN IF
;        CLEAR
;        EXIT
;      THEN
;      DUP -56 = IF
;        DROP
;        EXIT
;      THEN
;      DUP -28 = IF
;        DROP
;        ." <STOP>"
;        EXIT
;      THEN
;      ?DUP IF
;        SOURCE >IN @
;        2DUP U> + UMIN TYPE
;        ." <ERR"
;        DECIMAL
;        0 .R
;        #IN ?DUP IF
;          ': EMIT 0 .R
;        THEN
;        '> EMIT
;      THEN ;

		COLON ERROR,error
		.dw dup,dolit,-2,zero,within,doif,1$
		.dw   clear
		.dw   doexit
1$:		.dw dup,dolit,-56,equal,doif,2$
		.dw   drop
		.dw   doexit
2$:		.dw dup,dolit,-28,equal,doif,3$
		.dw   drop
		      SLIT ^|<STOP>|
		.dw   type
		.dw   doexit
3$:		.dw qdup,doif,5$
		.dw   source,toin,fetch
		.dw   twodup,umore,plus,umin,type
		      SLIT ^|<ERR|
		.dw   type
		.dw   decimal
		.dw   zero,dotr
.if FCBN
		.dw   hashin,qdup,doif,4$
		.dw     dolit,':,emit,zero,dotr
.endif
4$:		.dw   dolit,'>,emit
5$:		.dw doret

; HANDLER	-- addr
;		variable with saved return stack pointer
;
;    VARIABLE HANDLER

		VARIABLE HANDLER,handler
		.dw 0

; EXECUTE	... xt -- ...
;		execute execution token xt

		CODE EXECUTE,execute
		ex de,hl		; xt -> hl
		pop de			; set new TOS
		jp (hl)			; execute xt

; CATCH		... xt -- ... 0 or xt -- n
;		execute xt leaving nonzero exception code n or 0 when no exception occurred;
;		when an exception was caught, the parameter and return stacks are restored
;		to their state before execution of xt
;
;    : CATCH
;      SP@ >R
;      HANDLER @ >R
;      RP@ HANDLER !
;      EXECUTE
;      R> HANDLER !
;      RDROP
;      0 ;

		COLON CATCH,catch
		.dw spfetch,tor
		.dw handler,fetch,tor
		.dw rpfetch,handler,store
		.dw execute
		.dw rfrom,handler,store
		.dw rdrop
		.dw zero
		.dw doret

; THROW		0 -- or ... n -- ... n
;		throw exception n if nonzero
;
;    : THROW
;      ?DUP IF
;        HANDLER @ ?DUP IF
;          RP!
;          R> HANDLER !
;          R> SWAP >R
;          SP!
;          DROP
;          R>
;          EXIT
;        THEN
;        >R CLEAR R>
;        ERROR
;        REPL
;      THEN ;

		COLON THROW,throw
		.dw qdup,doif,2$
		.dw   handler,fetch,qdup,doif,1$
		.dw     rpstore
		.dw     rfrom,handler,store
		.dw     rfrom,swap,tor
		.dw     spstore
		.dw     drop
		.dw     rfrom
		.dw     doexit
1$:		.dw   tor,clear,rfrom
		.dw   error
.if REPL
		.dw   repl
.else
		.dw   bye
.endif
2$:		.dw doret

; QUIT		... -- ; R: ... --
;		throw -56 "QUIT";
;		no exception error is displayed;
;		unlike ABORT, the parameter stack is not cleared
;
;    : QUIT -56 THROW ;

		CODE QUIT,quit
		ld a,-56		;
		jr throw_a		; throw -56 "QUIT"

; ABORT		... -- ; R: ... --
;		throw -1 "ABORT";
;		clears the parameter stack unless caught with CATCH
;
;    : ABORT -1 THROW ;

		CODE ABORT,abort
		ld a,-1			;
throw_a:	ld e,a			;
throw_e:	ld d,-1			; set TOS to negative error code in a
		jr throw		; THROW
bios_throw_e:	; restore ix and iy when changed by BIOS/BDOS call that causes an exception
.if RPIX
		ld ix,(rp)		; rp -> ix
.endif
.if JPIY
		ld iy,next		; set next -> iy for jp (iy) in JP_NEXT
.endif
		jr throw_e		; THROW

; (ABORT")	... flag c-addr u -- ; R: ... --
;		if flag then abort with string message unless an active catch is present;
;		runtime of the ABORT" compile-only word;
;		throw -2 "ABORT""
;
;    : (ABORT")
;      ROT IF
;        HANDLER @ IF
;          2DROP
;        ELSE
;          TYPE
;        THEN
;        -2 THROW
;      THEN
;      2DROP ;

		COLON ^|(ABORT")|,doabortquote
		.dw rot,doif,3$
		.dw   handler,fetch,fetch,doif,1$
		.dw     twodrop
		.dw   doahead,2$
1$:		.dw     type
2$:		.dw   dolit,-2,throw
3$:		.dw twodrop
		.dw doret

.if REPL

; ABORT"	... flag -- ; C: "ccc<quote>" -- ; R: ... --
;		if flag then abort with string message unless an active catch is present;
;		throw -2 "ABORT"";
;		clears the parameter stack unless caught with CATCH;
;		may throw -14 "interpreting a compile-only word"
;
;    : ABORT" ?COMP POSTPONE S" POSTPONE (ABORT") ; IMMEDIATE

		COLON_IMM ^|ABORT"|,abortquote
		.dw qcomp
		.dw squote
		.dw dolit,doabortquote,compilecomma
		.dw doret

.endif;REPL

.if 1-UPHI

; BYE		--
;		return to BASIC

		CODE BYE,bye
		ld a,0xc9		; RET opcode
		ld (HERRO),a		; restore HERRO hook to RET opcode
		ld sp,(bsp)		; restore BASIC sp
		ret			; return to BASIC

.endif;UPHI

;-------------------------------------------------------------------------------
;
;		INTERPRETER
;
;-------------------------------------------------------------------------------

.if REPL

; (		"ccc<paren>" --
;		start a comment block;
;		parse and skip input up to the closing )
;
;    : (
;      ') PARSE
;      BEGIN
;        + DROP
;        SOURCE + = IF
;          DROP REFILL
;        ELSE
;          C@ ') <> IF
;            REFILL
;          ELSE
;            FALSE
;          THEN
;        THEN
;      0= UNTIL ; IMMEDIATE

		COLON_IMM (,paren
1$:		.dw   dolit,'),parse
		.dw   plus,dup
		.dw   source,plus,equal,doif,2$
		.dw     drop,refill
		.dw   doahead,4$
2$:		.dw     cfetch,dolit,'),notequal,doif,3$
		.dw       refill
		.dw     doahead,4$
3$:		.dw       false
4$:		.dw   zeroequal
		.dw dountil,1$
		.dw doret

; \		"ccc<eol>" --
;		start a comment line;
;		parse and skip input up to the end of line
;
;    : \ $A PARSE 2SROP ;

;		COLON_IMM \134,backslash	; this does not work
;		COLON_IMM ^|\134|,backslash	; this neither
;		COLON_IMM \,backslash		; this crashes the assembler
;		COLON_IMM \\,backslash		; this too
;		COLON_IMM ^|\|,backslash	; this too
;		COLON_IMM ^|\\|,backslash	; this too
		link = last_link		; expand macro manually
		last_link = .			; expand macro manually
		.dw link			; expand macro manually
		.db 0x81			; expand macro manually
		.str ^|\134|			; expand macro manually
backslash:	call docol			; expand macro manually
		.dw dolit,'\n,parse
		.dw twodrop
		.dw doret

; OK		"ccc<eol>" --
;		start a comment line;
;		parse and skip input up to the end of line;
;		same as \ but not immediate,
;		so that screen editing of Forth output before OK is made possible
;
;    : OK POSTPONE \ ;

		CODE OK,ok
		jr backslash

; .(		"ccc<paren>" --
;		emit CR then type "ccc" up to the closing )
;
;    : .( ') PARSE CR TYPE ; IMMEDIATE

		COLON_IMM .(,dotparen
		.dw dolit,'),parse
		.dw cr,type
		.dw doret

; NUMBER	c-addr u -- n|u|d|ud|r
;		convert string to number;
;		prefixed with character $ converts hex;
;		prefixed with character # converts decimal;
;		prefixed with character % converts binary;
;		sets value DBL to -1 when the number is a double;
;		sets value DBL to 1 when the number is a float;
;		otherwise sets value DBL to 0;
;		may throw -13 "undefined word" when string is not numeric
;
;    : NUMBER
;      2DUP >DOUBLE IF
;        2SWAP 2DROP
;        DBL INVERT IF
;          d>S
;        THEN
;        STATE @ IF
;          DBL IF
;            2LITERAL
;            EXIT
;          THEN
;          LITERAL
;        THEN
;        EXIT
;      THEN
;      >FLOAT IF
;        1 TO DBL
;        STATE @ IF
;          2LITERAL
;        THEN
;        EXIT
;      THEN
;      -13 THROW ;

.if IEEE|MATH

		COLON NUMBER,number
		.dw twodup
		.dw todouble,doif,4$
		.dw   twoswap,twodrop
		.dw   dbl,invert,doif,1$
		.dw     dtos
1$:		.dw   state,fetch,doif,3$
		.dw     dbl,doif,2$
		.dw       twoliteral
		.dw       doexit
2$:		.dw     literal
3$:		.dw   doexit
4$:		.dw tofloat,doif,6$
		.dw   one,doto,dbl+3
		.dw   state,fetch,doif,5$
		.dw     twoliteral
5$:		.dw   doexit
6$:		.dw dolit,-13,throw
		.dw doret

.else

		COLON NUMBER,number
		.dw todouble,doif,4$
		.dw   dbl,invert,doif,1$
		.dw     dtos
1$:		.dw   state,fetch,doif,3$
		.dw     dbl,doif,2$
		.dw       twoliteral
		.dw       doexit
2$:		.dw     literal
3$:		.dw   doexit
4$:		.dw dolit,-13,throw
		.dw doret

.endif;IEEE|MATH

; INTERPRET	--
;		interpret input while input is available
;
;    : INTERPRET
;      BEGIN BL PARSE-WORD ?DUP WHILE
;        2DUP
;        FIND-WORD ?DUP IF
;          2SWAP 2DROP
;          STATE @ = IF
;            COMPILE,
;          ELSE
;            EXECUTE
;          THEN
;        ELSE
;          DROP
;          NUMBER
;        THEN
;      REPEAT
;      DROP ;

		COLON INTERPRET,interpret
1$:		.dw bl,parseword,qdup,doif,5$
		.dw   twodup
		.dw   findword,qdup,doif,3$
		.dw     twoswap,twodrop
		.dw     state,fetch,equal,doif,2$
		.dw       compilecomma
		.dw     doahead,4$
2$:		.dw       execute
		.dw   doahead,4$
3$:		.dw     drop
		.dw     number
4$:		.dw doagain,1$
5$:		.dw drop
		.dw doret

; EVALUATE	... c-addr u -- ...
;		evaluate string
;
;    : EVALUATE
;      SAVE-INPUT N>R
;      TO SOURCE
;      >IN OFF
;      -1 TO SOURCE-ID
;      ['] INTERPRET CATCH
;      DUP ERROR
;      NR> RESTORE-INPUT DROP
;      THROW ;

.if XTRA|FCBN	; version with SAVE-INPUT N>R and NR> RESTORE-INPUT

		COLON EVALUATE,evaluate
		.dw saveinput,ntor
		.dw dotwoto,source+3
		.dw toin,off
		.dw mone,doto,sourceid+3
		.dw dolit,interpret,catch
		.dw dup,error
		.dw nrfrom,restoreinput,drop
		.dw throw
		.dw doret

.else

		COLON EVALUATE,evaluate
		.dw sourceid,tor
		.dw source,twotor
		.dw toin,fetch,tor
		.dw dotwoto,source+3
		.dw toin,off
		.dw mone,doto,sourceid+3
		.dw dolit,interpret,catch
		.dw dup,error
		.dw rfrom,toin,store
		.dw tworfrom,dotwoto,source+3
		.dw rfrom,doto,sourceid+3
		.dw throw
		.dw doret

.endif;XTRA|FCBN

; REPL		--
;		read-evaluate-print loop
;
;    : REPL
;      RP0@ RP!
;      HANDLER OFF
;      0 TO SOURCE-ID
;      CR
;      [
;      BEGIN
;        BEGIN ['] REFILL CATCH ?DUP WHILE
;          ERROR CR
;        REPEAT
;      WHILE
;        ['] INTERPRET CATCH ?DUP IF
;          ERROR
;          REPL
;        THEN
;        STATE @ INVERT IF
;          ."  OK "
;          DEPTH ?DUP IF . THEN CR
;        THEN
;      REPEAT
;      BYE ;

		COLON REPL,repl
		.dw dolit,rp0,fetch,rpstore
		.dw handler,off
.if FCBN
		.dw zero,doto,outputid+3
.endif
		.dw zero,doto,sourceid+3
		.dw cr
		.dw leftbracket
1$:		.dw   dolit,refill,catch,qdup,doif,2$
		.dw     error,cr
		.dw   doagain,1$
2$:		.dw doif,6$
		.dw   dolit,interpret,catch,qdup,doif,3$
		.dw     error
		.dw     repl
3$:		.dw   state,fetch,invert,doif,5$
		        SLIT ^| OK |
		.dw     type
		.dw     depth,qdup,doif,4$
		.dw       dot
4$:		.dw     cr
5$:		.dw doagain,1$
6$:
.if 1-UPHI
		.dw bye
.endif
		.dw doret

.endif;REPL

;-------------------------------------------------------------------------------
;
;		DICTIONARY MARKER AND FORGET
;
;-------------------------------------------------------------------------------

.if REPL

; MARKER	"<spaces>name<space>" -- ; --
;		define a dictionary marker;
;		executing the name deletes marker and all definitions made after;
;		beware of vocabulary definitions crossings
;		(other vocabulary DEFINITIONS after markers also get deleted and corrupt their vocabulary)
;
;    : MARKER
;      CURRENT
;      DUP @
;      HERE
;      CREATE
;        , 2,
;      DOES>
;        DUP CELL+ 2@
;        SWAP TO CONTEXT
;        DUP CONTEXT !
;        DEFINITIONS
;        L>NAME NAME> TO LASTXT
;        @ HERE - ALLOT ;

		COLON MARKER,marker
		.dw current
		.dw dup,fetch
		.dw here
		.dw create
		.dw comma,twocomma
		.dw dosemicolondoes
marker_does:	call dodoes
		.dw dup,cellplus,twofetch
		.dw swap,doto,context+3
		.dw dup,context,store
		.dw definitions
		.dw ltoname,namefrom,doto,lastxt+3
		.dw fetch,here,minus,allot
		.dw doret

; FENCE		-- addr
;		only permit FORGET past the dictionary FENCE address
;
;    HERE VALUE FENCE

		VALUE FENCE,fence
		.dw end

; FORGET	"<spaces>name<space>" --
;		delete name and all following definitions;
;		may throw -15 "invalid FORGET";
;		beware of vocabulary definitions crossings
;		(other vocabulary DEFINITIONS after markers also get deleted and corrupt their vocabulary)

		COLON FORGET,forget
		.dw tick
		.dw dup,fence,uless,doif,1$
		.dw   dolit,-15,throw
1$:		.dw toname,twominus,context,current,umax
		.dw over,umore,doif,2$
		.dw   forth
2$:		.dw dup,fetch
		.dw dup,context,store
		.dw definitions
		.dw ltoname,namefrom,doto,lastxt+3
		.dw here,minus,allot
		.dw doret

.endif;REPL

;-------------------------------------------------------------------------------
;
;		MAIN
;
;-------------------------------------------------------------------------------

.if MAIN
.include "main.asm"
.endif

;-------------------------------------------------------------------------------
;
;		VOCABULARY
;
;-------------------------------------------------------------------------------

.if REPL

; CONTEXT	-- addr
;		leaves address of link of the last vocabulary context definition
;
;    ' FORTH VALUE CONTEXT

		VALUE CONTEXT,context
		.dw forth+3

; CURRENT	-- addr
;		leaves address of link of the last current vocabulary definition
;
;    ' FORTH VALUE CURRENT

		VALUE CURRENT,current
		.dw forth+3

; DEFINITIONS	--
;		make CURRENT the CONTEXT vocabulary
;
;    : DEFINITIONS CONTEXT TO CURRENT ;

		COLON DEFINITIONS,definitions
		.dw context,doto,current+3
		.dw doret

; VOCABULARY	"<spaces>name<space>" --
;		define a new vocabulary
;
;    : VOCABULARY CURRENT CREATE , fig_kludge , DOES> TO CONTEXT ;

		COLON VOCABULARY,vocabulary
		.dw current
		.dw create
		.dw comma
		.dw dolit,fig_kludge,comma
		.dw dosemicolondoes
vocabulary_does:call dodoes
		.dw doto,context+3
		.dw doret

; FORTH		--
;		make FORTH the CONTEXT vocabulary
;
;    VOCABULARY FORTH

		CODE FORTH,forth
		call vocabulary_does
		.dw last_link
		.dw fig_kludge

.endif;REPL

;-------------------------------------------------------------------------------
;
;		END
;
;-------------------------------------------------------------------------------

end:

.end
