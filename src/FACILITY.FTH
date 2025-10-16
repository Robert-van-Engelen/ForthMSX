\ Forth 2012 standard Facility words and more

DECIMAL

: BEGIN-STRUCTURE	( "<spaces>name" -- addr 0 ; -- u )
  CREATE HERE 0 0 ,
  DOES> @
;

: END-STRUCTURE		( addr u -- )
  SWAP !
;

: +FIELD		( u n "<spaces>name" -- u ; addr -- addr )
  CREATE OVER , +
  DOES> @ +
;

: CFIELD:		( u "<spaces>name" -- u ; addr -- addr )
  1 +FIELD
;

: FIELD:		( u "<spaces>name" -- u ; addr -- addr )
  2 +FIELD
;

: 2FIELD:		( u "<spaces>name" -- u ; addr -- addr )
  4 +FIELD
;

: FFIELD:		( u "<spaces>name" -- u ; addr -- addr )
  4 +FIELD
;

: MS		( u -- )
  0 ?DO 7 0 DO LOOP LOOP
;

\ JIFFY OFF	- to reset
\ JIFFY @	- 50Hz or 60Hz elapsed jiffies
$FC9E CONSTANT JIFFY
