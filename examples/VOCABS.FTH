\ List all vocabularies

: VOCABS	( -- )
  CONTEXT
  ['] FORTH >BODY
  BEGIN
    DUP TO CONTEXT
    DUP 3 - >NAME NAME>STRING TYPE CR
    2+ 2+ @
  ?DUP 0= UNTIL
  TO CONTEXT
;
