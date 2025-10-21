\ Additional Forth 2012 standard Floating-Point and Floating-Point Extension words
\ Author: Robert van Engelen

\ Floating-point values are double cells, Double Number words can be used

: F! 2! ;
: F@ 2@ ;
: FALIGN ;
: FALIGNED ;
: FCONSTANT 2CONSTANT ;
: FDEPTH DEPTH 2/ ;
: FDROP 2DROP ;
: FDUP 2DUP ;
: FLITERAL POSTPONE 2LITERAL ; IMMEDIATE
: FLOAT+ 2+ 2+ ;
: FLOATS 2* 2* ;
: FROT 2ROT ;
: FSWAP 2SWAP ;
: FVARIABLE 2VARIABLE ;

: FE. FS. ;
: FEXPM1 FEXP 1E0 F- ;
: FLNP1 1E0 F+ FLN ;
: FSINCOS FDUP FSIN FSWAP FCOS ;
: FVALUE 2VALUE ;
: SET-PRECISION TO PRECISION ;
