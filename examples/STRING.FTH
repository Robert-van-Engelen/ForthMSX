\ String buffers of variable length
\ Author: Robert van Engelen

\ A string buffer, abbreviated as strbuf, stores its max size and current
\ string length len as the first two bytes in its buffer.

\ Allocate a new strbuf named mystr with 255 characters max size:
\     255 STRING: mystr
\ Now mystr is added as a Forth word that returns -- strbuf len
\ which is a pair c-addr u

\ To copy a string, for example the literal S" text", to mystr:
\     S" text" mystr STRCPY
\ Now mystr contains "text" that we can type out to the screen:
\     mystr TYPE
\ To concat more to mystr, thereby altering it:
\     S"  and more text" mystr STRCAT
\ Slicing a strbuf does not alter the string, but leaves a slice of the string:
\     mystr 9 4 SLICE TYPE
\ Use -1 (which is max unsigned) to slice to the end of the strbuf:
\     mystr 9 -1 SLICE TYPE
\ To input a line into a strbuf:
\     mystr STRACCEPT

.( Loading STRING...)

DECIMAL

: STRMAX	( strbuf -- max ) 2- C@ ;
: STRLEN	( strbuf -- len ) 1- C@ ;
: STRAFT	( strbuf -- u ) DUP STRMAX SWAP STRLEN - ;
: STRUPD	( strbuf len -- ) OVER STRMAX UMIN SWAP 1- C! ;

\ create a new strbuf
: STRING:	( max "<spaces>name<space>" -- ; strbuf len )
  CREATE DUP C, 0 C, ALLOT
  DOES> 2+ DUP STRLEN ;

\ copy a string into a strbuf
: STRCPY	( c-addr u strbuf len -- )
  DROP DUP ROT STRUPD	\ set the new length
  DUP STRLEN CMOVE ;

\ append a string to a strbuf
: STRCAT	( c-addr u strbuf len -- )
  >R			\ save the old length
  SWAP OVER STRAFT UMIN	\ limit the added length prevent overflow
  2DUP R@ + STRUPD	\ set the new length = old length + added
  SWAP R> + SWAP CMOVE ;

\ accept a line of input into a strbuf
: STRACCEPT	( strbuf len -- )
  DROP DUP DUP STRMAX ACCEPT STRUPD ;

\ returns a string slice of a strbuf
: SLICE		( c-addr1 u1 pos len -- c-addr2 u2 )
  >R			\ save len
  OVER UMIN		\ -- c-addr u1 pos where pos is limited to u1
  TUCK			\ -- c-addr pos u1 pos
  - R> UMIN		\ -- c-addr pos len where pos+len is limited to u1
  >R + R> ;

\ add a strbuf field to a Forth structure
: SFIELD:	( u max "name" -- u ; addr -- strbuf len )
  CREATE
    OVER ,		\ store current struct size u
    DUP ,		\ store max
    + 2+		\ update struct size += max+2
  DOES>		( struct-addr addr -- member-addr )
    SWAP OVER @ +	\ compute member address
    DUP ROT		\ -- member-addr member-addr addr
    CELL+ @ C!		\ make sure strbuf max is set
    2+ DUP STRLEN ;

\ create an array of strubf
: SARRAY:	( size max "name" -- ; index -- strbuf len )
  CREATE
    DUP , 2+ * ALLOT	\ save max and allocate space
  DOES>		( array-addr index -- strbuf len )
    SWAP OVER @		\ -- addr index max
    DUP>R		\ save max
    2+ * + CELL+	\ address in the array = (max+2)*index+addr+2
    R> OVER C!		\ make sure the strbuf max is set
    2+			\ skip max and len to get to strbuf
    DUP STRLEN ;

: TOUPPER	( char -- char ) DUP 'a '{ WITHIN IF $20 - THEN ;
: TOLOWER	( char -- char ) DUP 'A '[ WITHIN IF $20 + THEN ;
: STRUPPER	( c-addr u -- ) 0 ?DO DUP I + DUP C@ TOUPPER SWAP C! LOOP DROP ;
: STRLOWER	( c-addr u -- ) 0 ?DO DUP I + DUP C@ TOLOWER SWAP C! LOOP DROP ;

\ use this to set up the PAD as a temporary strbuf
: STRPAD	( -- ) 254 PAD C! PAD 2+ PAD 1+ C@ ;
