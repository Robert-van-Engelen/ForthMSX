\ SAVE.FTH save state to binary image file to reload with BLOAD from BASIC
\ Author: Robert van Engelen

\ In ForthMSX execute SAVE with the filename of your choice:
\ save mywork.bin
\ To reload in BASIC:
\ clear 100,&h8400
\ bload "myforth.bin",r

: SAVE          ( "<spaces>name<space>" -- )
  PARSE-NAME W/O CREATE-FILE THROW
  \ create MSX BLOAD 7-byte header at HERE
  HERE $FE C, $8400 , , $8400 ,
  DUP HERE 7 - 7 ROT WRITE-FILE
  \ write image from base address $8400 to HERE
  DUP 0= IF
    DROP DUP $8400 HERE OVER - ROT WRITE-FILE
  THEN
  -7 ALLOT \ undo alloc'ed header bytes
  IF ." can't save" THEN
  CLOSE-FILE DROP
;
