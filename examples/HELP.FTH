\ Consult HELP.TXT
\ Author: Robert van Engelen

.( Loading HELP...)

DECIMAL

VARIABLE topic

: (help)	( c-addr u fileid -- )
  BEGIN
    DUP GET-LINE 0= AND WHILE
      \ -- c-addr u fileid c-addr2 u2
      OVER C@ '= = IF
        topic OFF
        2DUP 6 PICK 6 PICK SEARCH -ROT 2DROP IF
          2DUP 1 /STRING CR TYPE CR
          topic ON
        THEN
      ELSE
        topic @ IF
          2DUP TYPE CR
        THEN
      THEN
      2DROP
  REPEAT
  2DROP
;

: help		( "<spaces>name<space>" -- )
  PARSE-NAME
  \ convert to upper case
  2DUP 0 DO
    DUP I + DUP C@
    DUP 'a '{ WITHIN IF $20 - THEN
    SWAP C!
  LOOP
  DROP
  S" A:HELP.TXT" R/O OPEN-FILE THROW
  \ -- c-addr u fileid
  ['] (help) CATCH DROP
  CLOSE-FILE DROP
  2DROP
;
