\ Z80 IO port access

\ PORT!		( u1 u2 -- )
\ send byte u1 to Z80 port u2
\		CODE PORT!,portstore
\ 7B		ld a,e			; u2 -> a port
\ D1		pop de			; pop u1 -> de
\ C5		push bc			; save bc with ip
\ 4F		ld c,a			;
\ ED 59		out (c),e		; u1 -> out(u2)
\ C1		pop bc			; restore bc with ip
\ D1		pop de			; set new TOS
\ FD E9		JP_NEXT			; continue

CODE PORT! $7B C, $D1 C, $C5 C, $4F C, $ED C, $59 C, $C1 C, $D1 C, $FD C, $E9 C,

\ PORT@		( u1 -- u2 )
\ receive a byte from Z80 port u1
\		CODE PORT@,portfetch
\ C5		push bc			; save bc with ip
\ 4B		ld c,e			; u1 -> c
\ ED 58		in e,(c)		; in(u1) -> e
\ 16 00		ld d,0			; 0 -> d
\ C1		pop bc			; restore bc with ip
\ FD E9		JP_NEXT			; continue

CODE PORT@ $C5 C, $4B C, $ED C, $58 C, $16 C, $00 C, $C1 C, $FD C, $E9 C,
