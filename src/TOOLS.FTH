\ Programming-Tools extension words
\ Author: Robert van Engelen

\ [IF] [ELSE] [THEN] are case sensitive and MUST be specified in UPPER case!

DECIMAL

: [DEFINED]	( "<spaces>name ..." -- flag )
  PARSE-NAME FIND-WORD NIP
; IMMEDIATE

: [UNDEFINED]	( "<spaces>name ..." -- flag )
  POSTPONE [DEFINED] 0=
; IMMEDIATE

: [ELSE]	( -- )
  1
  BEGIN
    BL PARSE-WORD DUP IF
      2DUP S" [IF]" COMPARE 0= IF
        2DROP 1+
      ELSE
        2DUP S" [ELSE]" COMPARE 0= IF
          2DROP 1- DUP IF 1+ THEN
        ELSE
          S" [THEN]" COMPARE 0= IF 1- THEN
        THEN
      THEN
      ?DUP 0= IF EXIT THEN
      FALSE
    ELSE
      2DROP
      REFILL 0=
    THEN
  UNTIL
  DROP
; IMMEDIATE

: [IF]		( flag -- )
  0= IF POSTPONE [ELSE] THEN
; IMMEDIATE

: [THEN]	( == )
; IMMEDIATE

[UNDEFINED] DUMP [IF]

: D.RB		( d n1+ n2+ -- ) BASE @ >R BASE ! D.R R> BASE ! ;
: U.RB		( u n1+ n2+ -- ) BASE @ >R BASE ! U.R R> BASE ! ;

: DUMP		( addr  u -- )
  OVER 7 AND IF
    OVER 0 16 U.RB
  THEN
  BEGIN DUP WHILE
    OVER 7 AND 0= IF
      CR OVER 0 16 U.RB
    THEN
    NEXT-CHAR 3 16 U.RB
  REPEAT
  2DROP
;

[THEN]
