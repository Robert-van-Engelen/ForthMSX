\ Additional Forth 2012 standard File-Access Extension words and more

DECIMAL

\ list files matching the name of the form [D:]FILENAME.EXT or all files when no name is specified
\ wildcard '?' matches any character when used in FILENAME and EXT;
\ wildcard '*' matches any sequence of characters, at most one '*' may be used in FILENAME and in EXT;
\ note: FILES A: with a drive only will not display files, use FILES A:*.* or change the drive first

: FILES		( "<spaces>name" -- )
  BL PARSE-WORD DUP 0= IF
    2DROP S" *.*"
  THEN
  S>FCB
  DRV EMIT ': EMIT CR
  DUP FIB 26 (DOSX) 2DROP	\ MSX BDOS se DTA 1ah
  DUP 0 17 (DOSX) NIP		\ MSX BDOS search for first 11h
  BEGIN 0= WHILE
    DUP FIB DROP
    DUP 1+ DUP 8 TYPE
    '. EMIT
    8 + 3 TYPE
    DUP 0 35 (DOSX) 2DROP	\ MSX BDOS get file size 23h
    FILE-POSITION DROP 255 AND 128 MD* 10 D.R CR
    0 0 18 (DOSX) NIP		\ MSX BDOS search for next 12h
  REPEAT
  OFF
;

\ check fileid for EOF

: EOF		( fileid -- flag )
  DUP FILE-POSITION DROP ROT FILE-SIZE DROP D=
;

\ check if a file with the specified filename exists (ior = 0 exists or ior = 255 does not) and get the size of that file

: FILE-STATUS	( c-addr u -- du ior )
  S>FCB
  DUP 0 $23 (DOSX) NIP	\ MSX BDOS get file size
  IF
    OFF
    0 0 -203
    EXIT
  THEN
  DUP OFF
  FILE-POSITION DROP 255 AND 128 MD* 0
;

\ flush file (flushes all files)

: FLUSH-FILE	( fileid -- ior )
  0 $0D (DOSX) 2DROP 0	\ MSX BDOS disk reset 0dh
;

\ current time and date (actually a Forth 2012 Facilities word)
\ n1=seconds n2=minutes n3=hours n4=day n5=month n6=year

: TIME&DATE	( -- +n1 +n2 +n3 +n4 +n5 +n6 )
  0 0 $2C (BDOS) DROP 8 RSHIFT SWAP 256 /MOD
  0 0 $2A (BDOS) DROP 256 /MOD ROT
;

\ current day and date
\ n1=day of the week (0=Sun..6=Sat) n2=day n3=month n4=year

: DAY&DATE      ( -- +n1 +n2 +n3 +n4 )
  0 0 $2A (BDOS) -ROT 256 /MOD ROT
;
  
