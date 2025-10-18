\ MSX VDP and VRAM access control
\ The "MSX Red Book" has info on MSX VDP IO ports

\ VRAM		addr --
\ set VRAM address for writing, add $4000 to addr to read
\
\ 		CODE VRAM,vram
\ 7B		ld a,e			; addr lsb -> a
\ F3		di			; disable interrupts (interrupts access the VDP)
\ D3 99		out (0x99),a		; VDP data port address register lsb
\ 3E 40		ld a,0x40		;
\ B2		or d			; addr msb | 0x40 -> a
\ D3 99		out (0x99),a		; VDP data port address register msb
\ FB		ei			; enable interrupts
\ D1		pop de			; set new TOS
\ FD E9		JP_NEXT			; continue

CODE VRAM $7B C, $F3 C, $D3 C, $99 C, $3E C, $40 C, $B2 C, $D3 C, $99 C, $FB C, $D1 C, $FD C, $E9 C,

\ >VRAM	c-addr u --
\ block write u data bytes at c-addr to VRAM at its current address, auto-increments its address
\ VRAM address must be set for writing
\
\ 		CODE >VRAM,tovram
\ E1		pop hl			; pop hl with c-addr
\ 7B		ld a,e			;
\ B2		or d			;
\ 28 0D		jr z,2$			; if u != 0 then
\ C5		push bc			;   save bc with ip
\ 0E 98		ld c,0x98		;   VDP VRAM data port is IO port c = 0x98
\ 43		ld b,e			;   u -> ab
\ AF		xor a			;   0 -> a
\ B8		cp b			;   set cf if b > 0
\ 8A		adc d			;   if b > 0 then d+1 -> a else d -> a (is nonzero since u != 0)
\ ED B3	   1$:	otir			;   [hl++] -> out(0x98) until --b = 0
\ 3D		dec a			;
\ 20 FB		jr nz,1$		; until --a = 0
\ C1		pop bc			; restore bc with ip
\ D1       2$:	pop de			; pop new TOS
\ FD E9		JP_NEXT			; continue

CODE >VRAM $E1 C, $7B C, $B2 C, $28 C, $0D C, $C5 C, $0E C, $98 C, $43 C, $AF C, $B8 C, $8A C, $ED C, $B3 C, $3D C, $20 C, $FB C, $C1 C, $D1 C, $FD C, $E9 C,

\ VRAM>	c-addr u --
\ block read u data bytes at c-addr from VRAM at its current address, auto-increments its address;
\ VRAM address must be set for reading
\
\ 		CODE VRAM>,vramfrom
\ E1		pop hl			; pop hl with c-addr
\ 7B		ld a,e			;
\ B2		or d			;
\ 28 0D		jr z,2$			; if u != 0 then
\ C5		push bc			;   save bc with ip
\ 0E 98		ld c,0x98		;   VDP VRAM data port is IO port c = 0x98
\ 43		ld b,e			;   u -> ab
\ AF		xor a			;   0 -> a
\ B8		cp b			;   set cf if b > 0
\ 8A		adc d			;   if b > 0 then d+1 -> a else d -> a (is nonzero since u != 0)
\ ED B2    1$:	inir			;   in(0x98) -> [hl]++ until --b = 0
\ 3D		dec a			;
\ 20 FB		jr nz,1$		; until --a = 0
\ C1		pop bc			; restore bc with ip
\ D1       2$:	pop de			; pop new TOS
\ FC E9		JP_NEXT			; continue

CODE VRAM> $E1 C, $7B C, $B2 C, $28 C, $0D C, $C5 C, $0E C, $98 C, $43 C, $AF C, $B8 C, $8A C, $ED C, $B2 C, $3D C, $20 C, $FB C, $C1 C, $D1 C, $FD C, $E9 C,

\ >VDP		u1 u2 --
\ send data byte u1 to VDP mode register u2
\
\ 		CODE >VDP,tovdp
\ E1		pop hl			; pop u1 -> hl
\ 7D		ld a,l			; u2 -> a
\ F3		di			; disable interrupts (interrupts access the VDP)
\ D3 99		out (0x99),a		; write data byte u1 -> out(0x99)
\ 7B		ld a,e			;
\ F6 80		or 0x80			; u2 | 0x80 -> a
\ D3 99		out (0x99),a		; select register 10000RRR -> out(0x99)
\ FB		ei			; enable interrupts
\ D1		pop de			; set new TOS
\ FD E9		JP_NEXT			; continue

CODE >VDP $E1 C, $7D C, $F3 C, $D3 C, $99 C, $7B C, $F6 C, $80 C, $D3 C, $99 C, $FB C, $D1 C, $FD C, $E9 C,
