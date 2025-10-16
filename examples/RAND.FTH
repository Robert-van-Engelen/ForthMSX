\ RAND.FTH C rand() generates pseudo-random numbers between 0 and 32767
\ Author: Robert van Engelen

.( Loading RAND...)

0. 2VALUE seed

: rand		( -- n ) seed 1103515245. D* 12345. D+ TUCK TO seed 32767 AND ;
: srand		( x -- ) S>D TO seed ;
: randomize	( -- ) $FC9E @ srand ;

randomize	\ seed PRNG with FC9Eh MSX JIFFY
