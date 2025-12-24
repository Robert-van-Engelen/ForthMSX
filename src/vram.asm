;-------------------------------------------------------------------------------
;
;		MSX VDP and VRAM access and control
;
;-------------------------------------------------------------------------------

; VPEEK        v-addr -- char
;		read char byte from VRAM at v-addr

		CODE VPEEK,vpeek
;		BIOS code
;		ex de,hl		;
;		call 0x004a		; RDVRM
;		ld e,a			;
;		ld d,0			;
;		JP_NEXT			;
;		faster code
		ld a,e			; v-addr lsb -> a
		di			; disable interrupts (interrupts access the VDP)
		out (0x99),a		; VDP data port address register lsb
		ld a,d			; v-addr msb -> a
		out (0x99),a		; VDP data port address register msb
		in a,(0x98)		; in(0x98) -> a from VDP VRAM data port
		ei			; enable interrupts
		ld e,a			;
		ld d,0			; set new TOS
		JP_NEXT			; continue

; VPOKE		char v-addr --
;		write char byte to VRAM at v-addr

		CODE VPOKE,vpoke
;		BIOS code
;		pop hl			;
;		ex de,hl		;
;		ld a,e			;
;		call 0x004d		; WRTVRM
;		pop de			;
;		JP_NEXT			;
;		faster code
		ld a,e			; v-addr lsb -> a
		di			; disable interrupts (interrupts access the VDP)
		out (0x99),a		; VDP data port address register lsb
		ld a,0x40		;
		or d			; v-addr msb | 0x40 -> a for VRAM writing
		out (0x99),a		; VDP data port address register msb
		pop de			; pop char
		ld a,e			; lsb u -> a
		out (0x98),a		; char -> out(0x98) to VDP VRAM data port
		ei			; enable interrupts
		pop de			; pop new TOS
		JP_NEXT			; continue

; VFILL		v-addr u char --
;		fill VRAM at v-addr with u copies of char byte

		CODE VFILL,vfill
;		BIOS code
;		ld a,e			;
;		SVBC			;
;		pop bc			;
;		pop hl			;
;		call 0x0056		; FILVRM
;		LDBC			;
;		pop de			;
;		JP_NEXT			;
;		faster code
		ld a,e			;
		ex af,af'		; char -> a'
		pop de			; pop de with u
		pop hl			; pop hl with v-addr
		ld a,l			; v-addr lsb -> a
		di			; disable interrupts (interrupts access the VDP)
		out (0x99),a		; VDP data port address register lsb
		ld a,0x40		;
		or h			; addr msb | 0x40 -> a for VRAM writing
		out (0x99),a		; VDP data port address register msb
		ex af,af'		;
		inc d			; outer loop counter adjustment
		inc e			; inner loop counter adjustment
		jr 2$			;
1$:		out (0x98),a		; loop u times a -> out(0x98) to VDP VRAM data port
2$:		dec e			;
		jr nz,1$		;
		dec d			;
		jr nz,1$		;
		ei			; enable interrupts
		pop de			; pop new TOS
		JP_NEXT			; continue

; VREAD		v-addr u c-addr --
;		block read u bytes at VRAM v-addr to c-addr

		CODE VREAD,vread
;		BIOS code
;		SVBC			;
;		pop bc			;
;		pop hl			;
;		call 0x0059		; LDIRMV
;		LDBC			;
;		pop de			;
;		JP_NEXT			;
;		faster code
		ld a,e			; v-addr lsb -> a
		di			; disable interrupts (interrupts access the VDP)
		out (0x99),a		; VDP data port address register lsb
		ld a,d			; v-addr msb -> a
		out (0x99),a		; VDP data port address register msb
		pop de			; pop de with u
		pop hl			; pop hl with c-addr
		push bc			; save bc with ip
		ld c,0x98		; VDP VRAM data port is IO port c = 0x98
		ld b,e			; u -> ab
		xor a			; 0 -> a
		cp b			; set cf if b > 0
		adc d			; if b > 0 then d+1 -> a else d -> a (is nonzero since u != 0)
1$:		inir			; loop in(0x98) -> [hl]++ until --b = 0
		dec a			;
		jr nz,1$		; until --a = 0
		pop bc			; restore bc with ip
		ei			; enable interrupts
		pop de			; pop new TOS
		JP_NEXT			; continue

; VWRITE	c-addr u v-addr --
;		block write u bytes at c-addr to VRAM at v-addr

		CODE VWRITE,vwrite
;		BIOS code
;		SVBC			;
;		pop bc			;
;		pop hl			;
;		call 0x005c		; LDIRVM
;		LDBC			;
;		pop de			;
;		JP_NEXT			;
;		faster code
		ld a,e			; v-addr lsb -> a
		di			; disable interrupts (interrupts access the VDP)
		out (0x99),a		; VDP data port address register lsb
		ld a,0x40		;
		or d			; v-addr msb | 0x40 -> a for VRAM writing
		out (0x99),a		; VDP data port address register msb
		pop de			; pop de with u
		pop hl			; pop hl with c-addr
		push bc			; save bc with ip
		ld c,0x98		; VDP VRAM data port is IO port c = 0x98
		ld b,e			; u -> ab
		xor a			; 0 -> a
		cp b			; set cf if b > 0
		adc d			; if b > 0 then d+1 -> a else d -> a (is nonzero since u != 0)
1$:		otir			; loop [hl++] -> out(0x98) until --b = 0
		dec a			;
		jr nz,1$		; until --a = 0
		pop bc			; restore bc with ip
		ei			; enable interrupts
		pop de			; pop new TOS
		JP_NEXT			; continue

; VMODE		n --
;		change VDP mode to n

		CODE VMODE,vmode
		ld a,e			;
		push bc			;
		call 0x005f		; CHGMOD
		pop bc			;
		pop de			;
		JP_NEXT			;

; VCOLOR	n1 n2 n3 --
;		change foreground color to n1, background color to n2, border color to n3;
;		current color bytes are storeed at $F3E9, $F3EA, $F3EB

		CODE VCOLOR,vcolor
		ld hl,0xf3eb		;
		ld (hl),e		;
		pop de			;
		dec hl			;
		ld (hl),e		;
		pop de			;
		dec hl			;
		ld (hl),e		;
		push bc			;
		call 0x0062		; CHGCLR
		pop bc			;
		pop de			;
		JP_NEXT			;

; VEMIT		char --
;		emit char to graphics screen at position GRPACX=$FCB7 and GRPACY=$FCB9

		CODE VEMIT,vemit
		ld a,e			;
		call 0x008d		; GRPPRT
		pop de			;
		JP_NEXT			;

; VTYPE		c-addr u --
;		type string to graphics screen at position GRPACX=$FCB7 and GRPACY=$FCB9

		CODE VTYPE,vtype
1$:		pop hl			; pop hl with c-addr
		inc d			; outer loop counter adjustment
		inc e			; inner loop counter adjustment
		jr 3$			; start looping
2$:		ld a,(hl)		; loop TOS times
		inc hl			;
		call 0x008d		;   vemit [hl++] GRPPRT
3$:		dec e			;
		jr nz,2$		;
		dec d			;
		jr nz,2$		;
		pop de			; pop new TOS
		JP_NEXT			; continue
