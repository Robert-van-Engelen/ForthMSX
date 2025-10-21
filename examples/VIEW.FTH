\ VIEW file text on screen, support both CR/LF and LF line endings
\ Author: Robert van Engelen

.( Loading VIEW...)

: VIEW-FILE	( c-addr u -- )
  R/O OPEN-FILE THROW
  BEGIN
    DUP GET-LINE 0= AND WHILE TYPE CR
  REPEAT
  2DROP CLOSE-FILE DROP
;

: VIEW		( "<spaces>name" -- ) PARSE-NAME VIEW-FILE ;
