; Example MAIN definition to use with forth.asm with flag MAIN=1

		COLON MAIN,main			; : MAIN
		    SLIT ^|Hello world!\r\n|	;   S\" Hello world!\r\n"
		.dw type			;   TYPE
		.dw dolit,11,one,dodo,2$	;   11 1 DO
1$:		.dw   i,dot,cr			;     I . CR
		.dw doloop,1$			;   LOOP
2$:		.dw doret			;   ;
