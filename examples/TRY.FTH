\ TRY.FTH exception handling like gforth
\ Author: Robert van Engelen
\ TRY <code> IFERROR <handle-code> THEN <restore-code> ENDTRY
\ TRY <code> RESTORE <restore-code> ENDTRY
\ where <handle-code> and <restore-code> may inspect the TOS error code (not drop it)

.( Loading TRY...)

: CALL		( ip -- ) >R ;

: CATCH-CALL	( ip -- ) ['] CALL CATCH ;

: TRY		( -- ip ; -- )
		POSTPONE AHEAD
		HERE ; IMMEDIATE

: IFERROR	( ip --  ; -- x )
		POSTPONE EXIT
		>R POSTPONE THEN
		R> POSTPONE LITERAL POSTPONE CATCH-CALL
		POSTPONE DUP POSTPONE IF
		POSTPONE DROP POSTPONE 0 ; IMMEDIATE

: RESTORE	( ip -- ; -- x )
		POSTPONE EXIT
		>R POSTPONE THEN
		R> POSTPONE LITERAL POSTPONE CATCH-CALL ; IMMEDIATE
  
: ENDTRY        ( -- ; x -- )
		POSTPONE THROW ; IMMEDIATE

\ tests

REQUIRE TOOLS.FTH

0 [IF]
: test1	( n -- )
  TRY
    DUP . THROW
  IFERROR
    ." ERROR"
  THEN
    ." CLEANUP"
  ENDTRY ;
: test2	( n -- )
  TRY
    DUP . THROW
  RESTORE
    ." RESTORE"
  ENDTRY ;
[THEN]
