: D.RB          ( d n1+ n2+ -- ) BASE @ >R BASE ! D.R R> BASE ! ;
: U.RB          ( u n1+ n2+ -- ) BASE @ >R BASE ! U.R R> BASE ! ;

: DUMP          ( addr  u -- )
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
