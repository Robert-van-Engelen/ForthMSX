\ Turn on/off (CTRL)-STOP break

DECIMAL

' (:) 11 + C@ $3a <> THROW \ incorrect Forth version

: BREAK-ON	( -- )
  \ move BREAK machine code to ' (:) + 11
  S\" \x3a\xe1\xfb\xe6\x10\x28\x07" ['] (:) 11 + SWAP CMOVE
;

: BREAK-OFF	( -- )
  \ move EXEC machine code to ' (:) + 11
  S\" \x0a\x6f\x03\x0a\x67\x03\xe9" ['] (:) 11 + SWAP CMOVE
;
