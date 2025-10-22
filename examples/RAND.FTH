\ Pseudorandom numbers
\ Author: Robert van Engelen

.( Loading RAND...)

DECIMAL

0. 2VALUE seed

\ ANSI C rand() LCG generates numbers between 0 and 32767

: rand		( -- n ) seed 1103515245. D* 12345. D+ TUCK TO seed 32767 AND ;
: srand		( x -- ) S>D TO seed ;
: randomize	( -- ) $FC9E @ srand ;

randomize	\ seed PRNG with FC9Eh MSX JIFFY

\ Lehmer RNG generates numbers between 0 and 2147483647-1
\ note: minstd-rand n UM/MOD DROP returns values from 0 to n-1

: minstd-rand	( -- +d ) seed 48271 UMD* 2DUP 1 M+ D0< IF 1 M+ $7FFF AND THEN 2DUP TO seed ;
