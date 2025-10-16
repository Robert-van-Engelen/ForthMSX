\ VIEW file content on screen, support both CR/LF and LF line endings

.( Loading VIEW...)

: VIEW-FILE	( c-addr u -- )
  R/O OPEN-FILE THROW
  BEGIN
    DUP DUP FIB ROT READ-LINE 0= AND
  WHILE
    OVER FIB DROP SWAP TYPE CR
  REPEAT
  DROP CLOSE-FILE DROP ;

: VIEW	( "<spaces>name" -- ) PARSE-NAME VIEW-FILE ;
