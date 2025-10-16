\ SEE.FTH SEE word - decompiles a word (this is sensitive to the choice of vocabulary context)
\ Author: Robert van Engelen

.( Loading SEE...)

DECIMAL

\ a value flag to indicate immediate words
0 VALUE IMM

\ display the name of an xt
: .NAME         ( xt -- ) ['] >NAME CATCH IF DROP S" ???" ELSE NAME>STRING THEN TYPE ;

\ display the name of an xt formatted as a colon definition
: .COLON        ( xt -- ) ['] >NAME CATCH IF DROP S" :NONAME" ELSE ." : " NAME>STRING THEN TYPE ;

\ display TO name
: .TO           ( ip -- ip ) CELL+ DUP @ 3 - SPACE .NAME ;

\ display the address of a jump word and increment ip by 2
: .JUMP         ( ip -- ip ) CELL+ DUP @ ."  -> " HEX U. DECIMAL ;

\ display a cell value in hex and decimal, and as a word if the value matches an execution token
: .CELL         ( x -- ) DUP . ['] >NAME CATCH IF DROP ELSE ." ( ['] " NAME>STRING TYPE ."  ) " THEN ;

\ display the body of a colon definition starting at address ip
: .DEF ( ip -- )
  BEGIN
    CR DUP HEX U. DECIMAL SPACE
    DUP @ CASE
      ['] (;)      OF DROP '; EMIT IMM IF ."  IMMEDIATE" THEN EXIT ENDOF
      ['] (;DOES)  OF ." DOES>" >BODY   ENDOF
      ['] (LIT)    OF CELL+ DUP @ .CELL ENDOF
      ['] (2LIT)   OF CELL+ DUP 2@ 0 D.R '. EMIT CELL+ ENDOF
      ['] (SLIT)   OF 'S EMIT '" EMIT SPACE CELL+ DUP COUNT TYPE '" EMIT DUP C@ + 1- ENDOF
      ['] (TO)     OF ." TO"    .TO     ENDOF
      ['] (2TO)    OF ." TO"    .TO     ENDOF
      ['] (+TO)    OF ." +TO"   .TO     ENDOF
      ['] (D+TO)   OF ." +TO"   .TO     ENDOF
      ['] (IF)     OF ." IF"    .JUMP   ENDOF
      ['] (OF)     OF ." OF"    .JUMP   ENDOF
      ['] (AHEAD)  OF ." AHEAD" .JUMP   ENDOF
      ['] (AGAIN)  OF ." AGAIN" .JUMP   ENDOF
      ['] (UNTIL)  OF ." UNTIL" .JUMP   ENDOF
      ['] (DO)     OF ." DO"    .JUMP   ENDOF
      ['] (?DO)    OF ." ?DO"   .JUMP   ENDOF
      ['] (LOOP)   OF ." LOOP"  .JUMP   ENDOF
      ['] (+LOOP)  OF ." +LOOP" .JUMP   ENDOF
      DUP .NAME
    ENDCASE
    CELL+
  AGAIN
;

\ display the meaning of a word's execution token
: .XT ( xt -- )
  DUP C@ $CD = IF
    DUP 1+ @ CASE
      ['] (:)    OF DUP .COLON       >BODY .DEF   ENDOF
      ['] (VAR)  OF ." VARIABLE -> " >BODY DUP HEX U. DECIMAL ." @ = " DUP ? ." 2@ = " 2@ D. ENDOF
      ['] (VAL)  OF ." VALUE = "     >BODY ?      ENDOF
      ['] (2VAL) OF ." 2VALUE = "    >BODY 2@ D.  ENDOF
      ['] (CON)  OF ." CONSTANT = "  >BODY ?      ENDOF
      ['] (2CON) OF ." 2CONSTANT = " >BODY 2@ D.  ENDOF
      ['] (DEF)  OF ." DEFER = "     DEFER@ .NAME ENDOF
      OVER DUP C@ $CD = SWAP 1+ @ DUP C@ $CD = SWAP 1+ @ ['] (DOES) = AND AND IF
        ." DOES> -> " DUP >BODY DUP HEX U. DECIMAL .DEF
      THEN
      NIP
    ENDCASE
    EXIT
  THEN
  DROP ." CODE"
;

: SEE ( "name" -- )
  PARSE-NAME FIND-WORD \ 0 0 | xt 1 | xt -1
  ?DUP IF
    0 > TO IMM
    .XT
  ELSE
    -13 THROW
  THEN
;
