Forth words
-----------

To search this document for Forth words, use the GitHub "outline" icon above
to enter (part of) the word to search its meaning.

For an overview see the [alphabetic list of words](#alphabetic-list-of-words)

Forth words operate on the parameter stack, the return stack, and may parse
delimited sequences of characters from the input buffer.  The parameter stack
updates performed by a word are indicated by _before_ `--` _after_:

**ROT**
<br>
_x1 x2 x3 -- x2 x3 x1_

On the left side of `--` we have three single-cell values are on the parameter
stack with _x3_ the top-of-stack (TOS) value.  On the right side we have three
single-cell values that are returned by ROT, replacing the input values on the
stack in a rotated order with _x1_ the new TOS, _x3_ the second-on-stack (2OS)
and _x2_ the third-on-stack (3OS).

Return stack changes are indicated by an _R_.  For example _x -- ; R: -- x_
moves _x_ from the parameter stack to the return stack.  Beware that moving
values from and to the return stack requires care, since the return stack is
primarily used to return control from word definitions.  When manipulating the
return stack in the body of your word definition, you can add values to the
return stack as long as you remove them before the word returns.  Also DO-LOOP
uses the return stack for loop parameters, which means that moving values
from/to the return stack is possible exclusively outside or inside a DO-LOOP,
not across DO and LOOP control structure.

Different types of values are abbreviated as followes:

abbreviation | description
------------ | -----------
_char_       | an 8-bit character stored in a single-cell 16-bit integer, an MSX ASCII code
_n_          | a single-cell signed 16-bit integer
_+n_         | a single-cell signed positive nonzero 16-bit integer
_u_          | a single-cell unsigned 16-bit integer
_x_          | a single-cell 16-bit value
_d_          | a double-cell signed 32-bit integer
_ud_         | a double-cell unsigned 32-bit integer
_xd_         | a double-cell 32-bit value
_r_          | a double-cell floating-point number
_addr_       | a single-cell 16-bit address
_c-addr_     | a single-cell 16-bit address pointing to a character string
_nt_         | a single-cell 16-bit name token, (a name field address)
_xt_         | a single-cell 16-bit execution token (executable machine code address or code field address)
_flag_       | a single-cell 16-bit flag (zero means false, nonzero means true)
_fileid_     | a single-cell 16-bit file-handle identifier
_ior_        | a single-cell 16-bit I/O return (zero is success, nonzero is an error code)

A _char_ literal is an ASCII numeric code 0 to 255 or a quoted symbol `'s`,
such as `'A` for the ASCII letter A with code 65.  This is a ForthMSX feature.

A single-cell integer literal is a number _n_ between -32768 and 32767 or
unsigned _u_ between 0 and 65535.

A double-cell integer literal _d_ is a number between -2147483648. and
2147483647.  or unsigned _ud_ between 0 and 4294967295., where the literal must
include the point `.` character somewhere in the string of digits, which is
classic Forth

The base of a literal number can be specific with a leading `#` for decimal or
leading `$` for hexadecimal or a `%` for binary.  For example, `%1010` is 10 in
decimal.  Otherwise, the default base is `BASE` which is changed to decimal
with `DECIMAL` and to hex with `HEX`.

a floating-point literal _r_ is specified in scientific notation with an
exponent, even when the exponent is zero.  For example, `1E0` is floating-point
1, but also `1e` without exponent digits following the lower-case `e`.  Because
MSX BASIC floating-point values may be specified with a trailing `!` or `#`,
ForthMSX supports this notation also, e.g. `1#` is floating-point 1.  Likewise,
 `1d` is floating-point 1, and perhaps strangely, `&h1` also.

A string in Forth consists of a _c-addr u_ pair on the paramater stack with
character string address _c-addr_ and length _u_.  A literal string is
specified with `S" ...text."` with a space after `S"` because `S"` is a word to
execute.

Parsed input is represented as _"parsed-input"_ on the left side of `--` with
the following abbreviations:

abbreviation     | parsing behavior
---------------- | ----------------
_&lt;char&gt;_   | the delimiting character marking the end of the string being parsed
_&lt;chars&gt;_  | zero or more consecutive occurrences of the character _&lt;char&gt;_
_&lt;space&gt;_  | a delimiting space character
_&lt;spaces&gt;_ | zero or more consecutive occurrences of the character _&lt;space&gt;_
_&lt;quote&gt;_  | a delimiting double quote
_&lt;paren&gt;_  | a delimiting right parenthesis
_&lt;eol&gt;_    | an implied delimiter marking the end of a line
_ccc_            | a parsed sequence of arbitrary characters, excluding the delimiter character
_name_           | a token delimited by space, equivalent to _ccc&lt;space&gt;_ or _ccc&lt;eol&gt;_

___
### 0
_-- 0_

leave constant 0

    0 CONSTANT 0

___
### 1
_-- 1_

leave constant 1

    1 CONSTANT 1

___
### 2
_-- 2_

leave constant 2

    2 CONSTANT 2

___
### 3
_-- 3_

leave constant 3

    3 CONSTANT 3

___
### -1
_-- -1_

leave constant -1

    -1 CONSTANT -1

___
### FALSE
_-- 0_

leave 0

    0 CONSTANT FALSE

___
### TRUE
_-- -1_

leave -1

    -1 CONSTANT TRUE

___
### BL
_-- 32_

leave constant 32 (space)

    32 CONSTANT BL

___
### PAD
_-- c-addr_

leave address of the PAD;
the PAD is a free buffer space of 256 bytes not used by Forth

___
### TMP
_-- c-addr_

leave address of the next temporary string buffer;
switches between two string buffers of 256 free bytes each;
used by SDUP, WORD and S" to store a string when interpreting

___
### TIB
_-- c-addr_

leave c-addr of the terminal input buffer;
held by SOURCE with input length;
used by the Forth interpreter

___
### #IB
_-- u_

the allocated size of TIB and FIB

___
### DROP
_x --_

drop TOS

___
### DUP
_x -- x x_

duplicate TOS

___
### ?DUP
_x -- x x or 0 -- 0_

duplicate TOS if nonzero

___
### SWAP
_x1 x2 -- x2 x1_

swap TOS with 2OS

___
### OVER
_x1 x2 -- x1 x2 x1_

copy 2OS over TOS

___
### ROT
_x1 x2 x3 -- x2 x3 x1_

rotate cells

    : ROT >R SWAP R> SWAP ;

___
### -ROT
_x1 x2 x3 -- x3 x1 x2_

undo (or back, or left) rotate cells, or ROT twice

    : -ROT ROT ROT ;

___
### NIP
_x1 x2 -- x2_

nip 2OS

    : NIP SWAP DROP ;

___
### TUCK
_x1 x2 -- x2 x1 x2_

tuck TOS under 2OS

    : TUCK SWAP OVER ;

___
### 2DROP
_xd1 xd2 -- xd1_

drop double TOS

    : 2DROP DROP DROP ;

___
### 2DUP
_xd -- xd xd_

duplicate double TOS

    : 2DUP OVER OVER ;

___
### 2SWAP
_xd1 xd2 -- xd2 xd1_

swap double TOS with double 2OS

    : 2SWAP ROT >R ROT R> ;
    : 2SWAP 3 ROLL 3 ROLL ;

___
### 2OVER
_xd1 xd2 -- xd1 xd2 xd1_

copy double 2OS over double TOS

    : 2OVER >R >R 2DUP R> R> 2SWAP ;
    : 2OVER 3 PICK 3 PICK ;

___
### 2ROT
_xd1 xd2 xd3 -- xd2 xd3 xd1_

rotate double cells

    : 2ROT 5 ROLL 5 ROLL ;

___
### PICK
_xu ... x0 u -- xu ... x0 xu_

pick u'th cell from the parameter stack;
0 PICK is the same as DUP;
1 PICK is the same as OVER

    : PICK 1+ CELLS SP@ + @ ;

___
### ROLL
_xu x(u+1) ... x1 x0 u -- x(u+1) ... x1 x0 xu_

roll u cells on the parameter stack,
where u < 128 (u is not checked, using u modulo 128 for safety);
0 ROLL does nothing;
1 ROLL is the same as SWAP;
2 ROLL is the same as ROT

___
### DEPTH
_-- u_

parameter stack depth

    : DEPTH SP0@ SP@ - 2- 2/ ;

___
### CLEAR
_... --_

purge parameter stack

    : CLEAR SP0@ SP! ;

___
### SP@
_-- addr_

fetch stack pointer, leave addr of the TOS cell (the TOS before SP@)

___
### SP!
_addr --_

store stack pointer

___
### .S
_--_

display parameter stack

    : .S DEPTH 0 ?DO SP0@ I 2+ CELLS - ? LOOP ;

___
### >R
_x -- ; R: -- x_

move TOS to the return stack

___
### DUP>R
_x -- x ; R: -- x_

duplicate TOS to the return stack, a single word for DUP >R

___
### R>
_R: x -- ; -- x_

move cell from the return stack

___
### RDROP
_R: x -- ; --_

drop cell from the return stack, a single word for R> DROP

___
### R@
_R: x -- x ; -- x_

fetch cell from the return stack

___
### 2>R
_x1 x2 -- ; R: -- x1 x2_

move double TOS to the return stack, a single word for SWAP >R >R

___
### 2R>
_R: x1 x2 -- ; -- x1 x2_

move double cell from the return stack, a single word for R> R> SWAP

___
### 2R@
_R: x1 x2 -- x1 x2 ; -- x1 x2_

fetch double cell from the return stack, a single word for R> R@ SWAP DUP R>

___
### N>R
_n*x n -- ; R: -- n*x n_

move n cells to the return stack;
where n < 127 (u is not checked, using n modulo 128 for safety);
no stack under/overflow checking

___
### NR>
_R: n*x n -- ; -- n*x n_

move n cells from the return stack;
where n < 127 (u is not checked, using n modulo 128 for safety);
no stack under/overflow checking

___
### RP@
_-- addr_

fetch return stack pointer

___
### RP!
_addr --_

store return stack pointer

___
### ?RP
_--_

check return stack pointer for under- and overflow,
available only when assembled with the SAVR assembly flag;
may throw -5 "return stack overflow" or -6 "return stack underflow"

___
### C@
_c-addr -- char_

fetch char

___
### @
_addr -- x_

fetch from cell

___
### 2@
_addr -- x1 x2_

fetch from double cell

    : 2@ DUP CELL+ @ SWAP @ ;

___
### C!
_char c-addr --_

store char in c-addr

___
### !
_x addr --_

store in cell

___
### 2!
_x1 x2 addr --_

store in double cell

    : 2! TUCK ! CELL+ ! ;

___
### +!
_n addr --_

increment cell

___
### D+!
_d addr --_

increment double cell

___
### ON
_addr --_

store TRUE (-1) in cell

    : ON -1 SWAP ! ;

___
### OFF
_addr --_

store FALSE (0) in cell

    : OFF 0 SWAP ! ;

___
### +
_n1 n2 -- n3_

sum n1+n2

___
### M+
_d1 n -- d2_

double sum d1+n

___
### D+
_d1 d2 -- d3_

double sum d1+d2

    : D+ >R M+ R> + ;

___
### -
_n1 n2 -- n3_

difference n1-n2

___
### D-
_d1 d2 -- d3_

double difference d1-d2

    : D- DNEGATE D+ ;

___
### UM*
_u1 u2 -- ud_

unsigned double product u1*u2

___
### M*
_n1 n2 -- d_

signed double product n1*n2

    : M*
      2DUP XOR >R
      ABS SWAP ABS
      UM*
      R> 0< IF DNEGATE THEN ;

___
### *
_n1|u1 n2|u2 -- n3|u3_

signed and unsigned product n1*n2

    : * UM* DROP ;

___
### UMD*
_ud1 u -- ud2_

unsigned double product ud1*u

    : UMD*
      DUP>R
      UM* DROP SWAP
      R> UM* ROT + ;

___
### UM/MOD
_ud u1 -- u2 u3_

unsigned remainder and quotient ud/u1;
the result is undefined when u1 = 0

___
### SM/REM
_d1 n1 -- n2 n3_

symmetric remainder and quotient d1/n1 rounded towards zero;
the result is undefined when n1 = 0

    : SM/REM
      2DUP XOR >R
      OVER >R
      ABS -ROT DABS ROT
      UM/MOD
      R> 0< IF SWAP NEGATE SWAP THEN
      R> 0< IF NEGATE THEN ;

___
### FM/MOD
_d1 n1 -- n2 n3_

floored signed modulus and quotient d1/n1 rounded towards negative (floored);
the result is undefined when n1 = 0

    : FM/MOD
      DUP>R
      SM/REM
      DUP 0< IF
        SWAP R> + SWAP 1-
      ELSE
        RDROP
      THEN ;

___
### /MOD
_n1 n2 -- n3 n4_

symmetric remainder and quotient n1/n2;
the result is undefined when n2 = 0

    : /MOD SWAP S>D ROT SM/REM ;

___
### MOD
_n1 n2 -- n3_

symmetric remainder of n1/n2;
the result is undefined when n2 = 0

    : MOD /MOD DROP ;

___
### /
_n1 n2 -- n3_

quotient n1/n2;
the result is undefined when n2 = 0

    : / /MOD NIP ;

___
### */MOD
_n1 n2 n3 -- n4 n5_

product with symmetric remainder and quotient n1*n2/n3;
the result is undefined when n3 = 0

    : */MOD -ROT M* ROT SM/REM ;

___
### */
_n1 n2 n3 -- n4_

product with quotient n1*n2/n3;
the result is undefined when n3 = 0

    : */ */MOD NIP ;

___
### UM*/MOD
_ud1 u1 u2 -- u3 ud2_

unsigned double product and quotient ud1*u1/u2 with single remainder u3,
with intermediate triple-cell product;
the result is undefined when u2 = 0

    \ assume d = dh.dl     = hi.lo parts
    \ assume t = th.tm.tl  = hi.mid.lo parts
    \ then
    \ dl*n -> tm.tl
    \ dh*n+tm -> th.tm
    \ gives d*n -> t
    : UMT*     ( ud u -- ut )
      DUP>R
      ROT UM*
      ROT 0 SWAP R> UM* D+ ;
    \ assume d = dh.dl     = hi.lo parts
    \ assume t = th.tm.tl  = hi.mid.lo parts
    \ then
    \ (th.tm)/n -> dh
    \ (th.tm)%n -> r
    \ (r.tl)/n -> dl
    \ (r.tl)%n -> r
    \ gives t/n -> d remainder r
    : UMT/MOD  ( ut u1 -- u2 ud )
      DUP>R
      UM/MOD R> SWAP >R
      UM/MOD R> ;
    : UM*/MOD >R UMT* R> UMT/MOD ;

___
### M*/
_d1 n1 n2 -- d2_

double product with quotient d1*n1/n2,
with intermediate triple-cell product;
the result is undefined when n2 = 0

    : M*/
      2DUP XOR 3 PICK XOR >R
      ABS SWAP ABS SWAP 2SWAP DABS 2SWAP
      UM*/MOD DROP
      >R 0< IF DNEGATE THEN ;

___
### MD*
_d1 n -- d2_

signed double product d1*n with a single

    : MD*
      2DUP XOR >R
      ABS -ROT DABS ROT
      UMD*
      R> 0< IF DNEGATE THEN ;

___
### D*
_d1|ud1 d2|ud2 -- d3|ud3_

signed and unsigned double product d1*d2;
the result overflows when d1 and d2 are too large;
use MD* for signed double product d*n with a single;
use UMD* for unsigned double product ud*u with a single;

    : D* >R ROT DUP>R -ROT MD* 2R> * 0 SWAP D+ ;

___
### UMD/MOD
_ud1 u1 -- u2 ud2_

unsigned remainder and unsigned double quotient ud1/u1;
the result is undefined when u1 = 0;

    : UMD/MOD DUP>R 0 SWAP UM/MOD -ROT R> UM/MOD ROT ;

___
### UD/MOD
_ud1 ud2 -- ud3 ud4_

unsigned double remainder and quotient ud1/ud2;
the result is undefined when ud2 = 0

___
### D/MOD
_d1 d2 -- d3 d4_

double symmetric remainder and quotient d1/d2;
the result is undefined when d2 = 0

    : D/MOD
      DUP 3 PICK DUP>R XOR >R
      DABS 2SWAP DABS 2SWAP
      UD/MOD
      R> 0< IF DNEGATE THEN
      R> 0< IF 2SWAP DNEGATE 2SWAP THEN ;

___
### DMOD
_d1 d2 -- d3_

double symmetric remainder of d1/d2;
the result is undefined when d2 = 0

    : DMOD D/MOD 2DROP ;

___
### D/
_d1 d2 -- d3_

double quotient d1/d2;
the result is undefined when d2 = 0

    : D/ D/MOD 2SWAP 2DROP ;

___
### AND
_x1 x2 -- x1&x2_

bitwise and x1 with x2

___
### OR
_x1 x2 -- x1|x2_

bitwise or x1 with x2

___
### XOR
_x1 x2 -- x1^x2_

bitwise xor x1 with x2

___
### =
_x1 x2 -- flag_

true if x1 = x2

___
### <>
_x1 x2 -- flag_

true if x1 <> x2

___
### <
_n1 n2 -- flag_

true if n1 < n2 signed

    : <
      2DUP XOR 0< IF
        DROP 0<
        EXIT
      THEN
      - 0< ;

___
### >
_n1 n2 -- flag_

true if n1 > n2 signed

    : > SWAP < ;

___
### U<
_u1 u2 -- flag_

true if u1 < u2 unsigned

    : U<
      2DUP XOR 0< IF
        NIP 0<
        EXIT
      THEN
      - 0< ;

___
### U>
_u1 u2 -- flag_

true if u1 > u2 unsigned

    : U> SWAP U< ;

___
### 0=
_x -- flag_

true if x = 0;
also serves as a logical NOT

___
### 0<
_n -- flag_

true if n < 0

___
### D0=
_dx -- flag_

true if dx = 0

    : D0= OR 0= ;

___
### D0<
_d -- flag_

true if d < 0

    : D0< NIP 0< ;

___
### S>D
_n -- d_

widen single to double

___
### D>S
_d -- n_

narrow double to single;
may throw -11 "result out of range" valid range is -32768 to 65535

___
### D=
_d1 d2 -- flag_

true if d1 = d2

    : D= D- D0= ;

___
### D<
_d1 d2 -- flag_

true if d1 < d2

    : D<
      DUP 3 PICK XOR 0< IF
        2DROP D0<
        EXIT
      THEN
      D- D0< ;

___
### DU<
_du1 du2 -- flag_

true if ud1 < ud2

    : DU<
      DUP 3 PICK XOR 0< IF
        2SWAP 2DROP D0<
        EXIT
      THEN
      D- D0< ;

___
### MAX
_n1 n2 -- n3_

signed max of n1 and n2

    : MAX
      2DUP < IF SWAP THEN
      DROP ;

___
### MIN
_n1 n2 -- n3_

signed min of n1 and n2

    : MIN
      2DUP > IF SWAP THEN
      DROP ;

___
### UMAX
_u1 u2 -- u3_

unsigned max of u1 and u2

    : UMAX
      2DUP U< IF SWAP THEN
      DROP ;

___
### UMIN
_u1 u2 -- u3_

unsigned min of u1 and u2

    : UMIN
      2DUP U> IF SWAP THEN
      DROP ;

___
### DMAX
_d1 d2 -- d3_

signed double max of d1 and d2

    : DMAX
      2OVER 2OVER D< IF 2SWAP THEN
      2DROP ;

___
### DMIN
_d1 d2 -- d3_

signed double min of d1 and d2

    : DMIN
      2OVER 2OVER D< INVERT IF 2SWAP THEN
      2DROP ;

___
### WITHIN
_x1 x2 x3 -- flag_

true if x1 is within x2 up to x3 exclusive

    : WITHIN OVER - >R - R> U< ;

___
### INVERT
_x1 -- x2_

one's complement ~x1

    : INVERT 1+ NEGATE ;
    : INVERT -1 XOR ;

___
### NEGATE
_n1 -- n2_

two's complement -n1

    : NEGATE 0 SWAP - ;
    : NEGATE INVERT 1+ ;

___
### ABS
_n1 -- n2_

absolute value |n1|

    : ABS DUP 0< IF NEGATE THEN ;

___
### DNEGATE
_d1 -- d2_

two's complement -d1

    : DNEGATE SWAP INVERT SWAP INVERT 1 M+ ;

___
### DABS
_d1 -- d2_

absolute value |d1|

    : DABS DUP 0< IF DNEGATE THEN ;

___
### LSHIFT
_x1 u -- x2_

logical shift left x1 << u

___
### RSHIFT
_x1 u -- x2_

logical shift right x1 >> u

___
### 1+
_n1 -- n2_

increment n1+1

    : 1+ 1 + ;

___
### 2+
_n1 -- n2_

increment n1+2

    : 2+ 2 + ;

___
### 1-
_n1 -- n2_

decrement n1-1

    : 1- 1 - ;

___
### 2-
_n1 -- n2_

decrement n1-2

    : 2- 2 - ;

___
### 2*
_n1 -- n2_

arithmetic shift left n1 << 1

    : 2* 2 * ;

___
### 2/
_n1 -- n2_

arithmetic shift right n1 >> 1

    : 2/ 2 / ;

___
### D2*
_d1 -- d2_

arithmetic shift left d1 << 1

    : D2* 2 MD* ;

___
### D2/
_d1 -- d2_

arithmetic shift right d1 >> 1

    : D2/ 1 2 M*/ ;

___
### CELL+
_addr -- addr_

increment to next cell

    : CELL+ 2+ ;

___
### CELLS
_n1 -- n2_

convert to cell unit

    : CELLS 2* ;

___
### CHAR+
_n1 -- n1_

increment to next char

    : CHAR+ 1+ ;

___
### CHARS
_n1 -- n2_

convert to char unit (does nothing as chars are bytes)

    : CHARS ;

___
### COUNT
_c-addr1 -- c-addr2 u_

convert counted string to string

    : COUNT DUP 1+ SWAP C@ ;

___
### COMPARE
_c-addr1 u1 c-addr2 u2 -- -1|0|1_

compare strings, leaves -1 = less or 0 = equal or 1 = greater

___
### SEARCH
_c-addr1 u1 c-addr2 u2 -- c-addr3 u3 flag_

true if the first string contains the second string;
leaves matching address, remaining length, and TRUE;
or leaves the first string and FALSE

___
### CMOVE
_c-addr1 c-addr2 u --_

move u bytes from c-addr1 to c-addr2 (from begin going up)

    : CMOVE
      SWAP >R
      BEGIN DUP WHILE
        NEXT-CHAR R@ C!
        R> 1+ >R
      REPEAT
      RDROP
      2DROP ;

___
### CMOVE>
_c-addr1 c-addr2 u --_

move u bytes from c-addr1 to c-addr2 up (from end going down)

___
### MOVE
_c-addr1 c-addr2 u --_

move u bytes from c-addr1 to c-addr2

    : MOVE
      -ROT
      2DUP U< IF
        ROT CMOVE>
      ELSE
        ROT CMOVE
      THEN ;

___
### FILL
_c-addr u char --_

fill memory with char

___
### ERASE
_c-addr u --_

fill memory with zeros

    : ERASE 0 FILL ;

___
### BLANK
_c-addr u --_

fill memory with 0x20 (BL) chars

    : ERASE BL FILL ;

___
### CHOP
_c-addr u1 char -- c-addr u2_

truncate a string up to a matching char;
leaves the string if char not found;
char = 0x20 (BL) chops 0x00 to 0x20 (white space and control)

___
### TRIM
_c-addr1 u1 char -- c-addr2 u2_

trim initial chars from a string;
char = 0x20 (BL) trims 0x00 to 0x20 (white space and control)

___
### -TRIM
_c-addr u1 char -- c-addr u2_

trim trailing chars from a string;
char = 0x20 (BL) trims 0x00 to 0x20 (white space and control)

___
### -TRAILING
_c-addr u1 -- c-addr u2_

trim trailing white space and control characters from a string

    : -TRAILING BL -TRIM ;

___
### /STRING
_c-addr1 u1 n -- c-addr2 u2_

slice n characters off the start of a string;
if n is larger than u1, then u2 underflows,
which should be avoided with OVER UMIN /STRING

    : /STRING ROT OVER + -ROT - ;

___
### NEXT-CHAR
_c-addr1 u1 -- c-addr2 u2 char_

get next char from a string;
increments the string address and decrements its length by one

    : NEXT-CHAR OVER C@ >R 1- SWAP 1+ SWAP R> ;
    : NEXT-CHAR OVER C@ -ROT 1- SWAP 1+ SWAP ROT ;

___
### WIDTH
_u --_

set text mode screen and its window width

___
### MAX-XY
_-- u1 u2_

leave number of screen columns x as u1 and rows y as u2

___
### CUR-XY
_-- u1 u2_

fetch cursor column x as u1 >= 0 and row y as u2 >= 0

___
### AT-XY
_u1 u2 --_

set column x to u1 >= 0 and row y to u2 >= 0

    : AT-XY Y! X! ;

___
### OUTPUT-ID
_-- 0|fileid_

value with 0 = console output, otherwise fileid to redirect output

___
### EMIT
_char --_

emit char to screen or to OUTPUT-ID when set;
list of control codes for console output:

code | effect
---- | ---------------------------------------------------------
   1 | MSX graphic character header, follow by 0x40 to 0x5f
   7 | BELL
   8 | BS backspace
   9 | TAB
  10 | LF line feed
  11 | HOME cursor home
  12 | FF CLS clear screen and home
  13 | CR carriage return
  27 | ESC escape sequence (see table below)
  28 | cursor right (wraps but does not scroll)
  29 | cursor left (wraps but does not scroll)
  30 | cursor up (does not scroll)
  31 | cursor down (does not scroll)
 127 | DEL delete character

codes | effect
----- | -----------------------------------------------------
ESC j | clear screen and home
ESC E | clear screen and home
ESC K | clear to end of line
ESC J | clear to end of screen
ESC l | clear line
ESC L | insert line
ESC M | delete line
ESC Y | set cursor coordinates, follow by row column bytes
ESC A | cursor up, does not scroll
ESC B | cursor down, does not scroll
ESC C | cursor right, wraps if the logical line of text wraps
ESC D | cursor left, does not wrap
ESC H | cursor home
ESC x | change cursor, follow by '4' (block) or '5' (disable)
ESC y | change cursor, follow by '4' (under) or '5' (enable)

___
### PAGE
_--_

clear console screen

    : PAGE $C EMIT ;

___
### CR
_--_

emit carriage return and line feed

    : CR 13 EMIT 10 EMIT ;

___
### SPACE
_--_

emit a space

    : SPACE BL EMIT ;

___
### SPACES
_n --_

emit n spaces (zero or negative n does nothing)

    : SPACES 0 MAX 0 ?DO SPACE LOOP ;

___
### TYPE
_c-addr u --_

type string to output or to OUTPUT-ID when set;
string may contain control codes, see EMIT

    : TYPE
      BEGIN DUP WHILE
        NEXT-CHAR EMIT
      REPEAT
      2DROP ;

___
### BASE
_-- addr_

variable with numeric base for conversion

    VARIABLE BASE

___
### DECIMAL
_--_

set BASE to 10

    : DECIMAL 10 BASE ! ;

___
### HEX
_--_

set BASE to 16

    : HEX 16 BASE ! ;

___
### HP
_-- addr_

hold pointer

    0 VALUE HP

___
### <#
_--_

begin pictured numeric output

    : <# HERE h_size + TO HP ;

___
### HOLD
_char --_

hold char for pictured numeric output

    : HOLD -1 +TO HP HP C! ;

___
### HOLDS
_c-addr u --_

hold string for pictured numeric output;
the string size should be limited to not exceed the hold space size of 40 bytes

    : HOLDS
      BEGIN DUP WHILE
        1- 2DUP + C@ HOLD
      REPEAT
      2DROP ;

___
### #
_ud1 -- ud2_

hold digit

    : #
      0 BASE @ UM/MOD >R
      BASE @ UM/MOD
      SWAP DUP 9 > IF
        7 +
      THEN
      '0 + HOLD
      R> ;

___
### #S
_ud -- 0 0_

hold all remaining digits

    : #S BEGIN # 2DUP D0= UNTIL ;

___
### SIGN
_n --_

hold minus sign if n < 0

    : SIGN 0< IF '- HOLD THEN ;

___
### #>
_ud -- c-addr u_

end pictured numeric output, leave string

    : #> 2DROP HP HERE h_size + OVER - ;

___
### D.R
_d +n --_

output signed double d right-aligned in field of +n chars wide

    : D.R -ROT TUCK DABS <# #S ROT SIGN #> ROT OVER - SPACES TYPE ;

___
### D.
_d --_

output signed double d with a trailing space

    : D. 0 D.R SPACE ;

___
### U.R
_u +n --_

output unsigned u right-aligned in field of +n chars wide

    : U.R 0 SWAP D.R ;

___
### U.
_u --_

output unsigned u with a trailing space

    : U. 0 D. ;

___
### .R
_n +n --_

output signed n right-aligned in field of +n chars wide

    : .R SWAP S>D ROT D.R ;

___
### .
_n --_

output signed n with a trailing space

    : . S>D D. ;

___
### ?
_addr --_

output signed cell stored at addr

    : ? @ . ;

___
### INKEY
_-- x_

check for key press and return the code of the key;
0 = no key pressed

___
### KEY-CLEAR
_--_

wait until no keys are pressed

    : KEY-CLEAR BEGIN INKEY 0= UNTIL ;

___
### KEY?
_-- flag_

true if a key is pressed

___
### KEY
_-- char_

wait and read key from the console

___
### EDIT
_c-addr +n1 n2 n3 -- c-addr +n4_

edit buffer c-addr;
buffer size +n1;
string in buffer has length n2;
non-editable left margin n3;
leaves c-addr and length +n4 (MSX INLIN strips first n3 characters)

___
### ACCEPT
_c-addr +n1 -- +n2_

accept user input into buffer c-addr +n1;
leaves length +n2

    : ACCEPT 0 0 EDIT NIP ;

___
### #IN
_-- n_

value with line number of the input file being read from SOURCE-ID

    0 VALUE #IN

___
### >IN
_-- addr_

variable with offset into the input buffer

    VARIABLE >IN

___
### SOURCE-ID
_-- 0|-1|fileid_

value with 0 = console input or -1 = string input, otherwise fileid input

    0 VALUE SOURCE-ID

___
### SOURCE
_-- c-addr u_

double value with input source buffer and input length

    TIB 0 2VALUE SOURCE

___
### RESTORE-INPUT
_... n -- flag_

restore input parameters from the stack;
flag is always FALSE (success)

    : RESTORE-INPUT DROP >IN ! TO SOURCE TO SOURCE-ID FALSE ;
    : RESTORE-INPUT DROP TO #IN >IN ! TO SOURCE TO SOURCE-ID FALSE ;

___
### SAVE-INPUT
_-- ... n_

save input parameters on the stack

    : SAVE-INPUT SOURCE-ID SOURCE >IN @ 4 ;
    : SAVE-INPUT SOURCE-ID SOURCE >IN @ #IN 5 ;

___
### REFILL
_-- flag_

attempt to refill the input buffer;
leaves FALSE when the end of input (end of file) is reached

    : REFILL
      SOURCE-ID INVERT DUP IF
        TIB DUP #IB
        ACCEPT
        TO SOURCE
        >IN OFF
      THEN ;

    : REFILL
      SOURCE-ID INVERT DUP IF
        SOURCE-ID ?DUP IF
          GET-LINE 0= AND 0= IF
            2DROP DROP FALSE
            EXIT
          THEN
          1 +TO #IN
        ELSE
          TIB DUP #IB
          ACCEPT
          0 TO #IN
        THEN
        TO SOURCE
        >IN OFF
      THEN ;

___
### SKIP
_char "&lt;chars&gt;" --_

skip chars in input when present, 0x20 (BL) skips 0x00 to 0x20 (white space and control)

    : SKIP SOURCE >IN @ /STRING ROT TRIM DROP SOURCE DROP - >IN ! ;

___
### PARSE
_char "ccc&lt;char&gt;" -- c-addr u_

parse "ccc" up to char when present

    : PARSE SOURCE >IN @ /STRING ROT CHOP DUP 1+ >IN @ + SOURCE NIP UMIN >IN ! ;

___
### PARSE-WORD
_char "&lt;chars&gt;ccc&lt;char&gt;" -- c-addr u_

parse char-delimited word;
may throw -18 "parsed string overflow"

    : PARSE-WORD
      DUP SKIP PARSE
      DUP b_size-1 U> IF -18 THROW THEN ;

___
### WORD
_char "&lt;chars&gt;ccc&lt;char&gt;" -- c-addr_

parse word as a counted string

    : WORD TMP DUP ROT PARSE-WORD ROT 2DUP C! 1+ SWAP CMOVE ;
    : WORD PARSE-WORD SDUP DROP 1- ;

___
### CHECK-NAME
_c-addr u -- c-addr u_

check if name is valid;
may throw -16 "attempt to use a zero-length string as a name";
may throw -19 "definition name too long"

    : CHECK-NAME
      DUP 0= IF -16 THROW THEN
      DUP length_mask U> IF -19 THROW THEN ;

___
### PARSE-NAME
_"&lt;spaces&gt;name&lt;space&gt;" -- c-addr u_

parse space-delimited name;
check if name length is valid

    : PARSE-NAME BL PARSE-WORD CHECK-NAME ;

___
### CHAR
_"&lt;spaces&gt;name&lt;space&gt;" -- char_

parse char

    : CHAR PARSE-NAME DROP C@ ;

___
### >DIGIT
_char -- n_

convert char digit to numeric digit when within BASE;
leaves -1 if char is invalid

___
### >NUMBER
_ud1 c-addr1 u1 -- ud2 c-addr2 u2_

convert string to number;
updates accumulated double ud1 to ud2;
leaves string with the remaining unconvertable chars or empty

    : >NUMBER
      BEGIN DUP WHILE
        NEXT-CHAR >DIGIT
        DUP 0< IF
          DROP -1 /STRING
          EXIT
        THEN
        >R
        2SWAP
        BASE @ UMD*
        R> M+
        2SWAP
      REPEAT ;

___
### DBL
_-- flag_

true if >DOUBLE or NUMBER parsed and produced a double

    0 VALUE DBL

___
### >DOUBLE
_c-addr u -- d true | false_

convert string to signed double;
prefixed with character $ converts hex;
prefixed with character # converts decimal;
prefixed with character % converts binary;
leaves the double and true if string is converted;
leaves false if string is unconvertable;
sets value DBL to -1 when the number is a double;
otherwise sets value DBL to 0

___
### L>NAME
_lfa -- nt_

convert link field address (lfa) to name token or name field address (nfa)

___
### NAME>STRING
_nt -- c-addr u_

convert name token or name field address (nfa) to string

___
### NAME>
_nt -- xt_

convert name token or name field address (nfa) to execution token or call field address (cfa)

___
### >NAME
_xt -- nt_

convert execution token or call field address (cfa) to name token or name field address (nfa);
may throw -24 "invalid numeric argument"

___
### >BODY
_xt -- addr_

convert execution token to parameter field address (pfa)

___
### FIND-WORD
_c-addr u -- c-addr 0 | xt 1 | xt -1_

search dictionary for matching word (case insensitive);
leaves execution token and 1 = immediate or -1 = not immediate;
leaves c-addr and 0 when not found

___
### '
_"&lt;spaces&gt;name&lt;space&gt;" -- xt_

parse name and search the dictionary to leave its execution token;
may throw -13 "undefined word"

    : ' PARSE-NAME FIND-WORD 0= IF -13 THROW THEN ;

___
### FIND
_c-addr -- c-addr 0 | xt 1 | xt -1_

search dictionary for the counted string to match a word (case insensitive);
leaves execution token and 1 = immediate or -1 = not immediate;
leaves c-addr and 0 when not found

    : FIND COUNT FIND-WORD ;

___
### WORDS
_--_

display context vocabulary words one screen full at a time

___
### HERE
_-- addr_

address of free memory after the dictionary;
new definitions are added here;
note that numeric output words use HERE for conversion

___
### UNUSED
_-- u_

unused dictionary space

    : UNUSED top @ HERE - ;

___
### LASTXT
_-- xt_

leaves the last execution token defined

    0 VALUE LASTXT

___
### RECURSE
_... -- ..._

recursively call the currently defined word;
may throw -14 "interpreting a compile-only word";
recursion depth should not exceed available return stack space

    : RECURSE ?COMP LASTXT COMPILE, ; IMMEDIATE
    : RECURSE ?COMP ['] ?RP COMPILE, LASTXT COMPILE, ; IMMEDIATE

___
### STATE
_-- addr_

compilation state;
STATE @ leaves TRUE when compiling;
STATE @ leaves FALSE when interpreting

    VARIABLE STATE

___
### [
_--_

switch state to interpreting

    : [ STATE OFF ;

___
### ]
_--_

switch state to compiling

    : ] STATE ON ;

___
### [']
_"&lt;spaces&gt;name&lt;space&gt;" -- ; -- xt_

compile xt of name as literal;
may throw -14 "interpreting a compile-only word"

    : ['] ?COMP ' LITERAL ; IMMEDIATE

___
### [CHAR]
_"&lt;spaces&gt;char" -- ; -- char_

compile char as literal;
note that the syntax 'char is preferred instead of this legacy word;
may throw -14 "interpreting a compile-only word"

    : [CHAR] ?COMP CHAR LITERAL ; IMMEDIATE

___
### [COMPILE]
_"&lt;space&gt;name&lt;space&gt;" -- ; ... -- ..._

compile name;
note that POSTPONE is preferred instead of this legacy word;
may throw -14 "interpreting a compile-only word"

    : [COMPILE] ?COMP ' COMPILE, ; IMMEDIATE

___
### HIDE
_--_

hide the last definition

    : HIDE CURRENT @ L>NAME DUP C@ smudge_mask OR SWAP C! ;

___
### REVEAL
_--_

reveal the last definition, i.e. unhide it

    : REVEAL CURRENT @ L>NAME DUP C@ ~smudge_mask AND SWAP C! ;

___
### IMMEDIATE
_--_

make the last definition immediate

    : IMMEDIATE CURRENT @ L>NAME DUP C@ immediate_mask OR SWAP C! ;

___
### ?COMP
_--_

check if compiling;
may throw -14 "interpreting a compile-only word"

___
### ?SYS
_-- ; C: x --_

check if compiled control structure matches x;
may throw -14 "interpreting a compile-only word";
may throw -22 "control structure mismatch"

___
### ALLOT
_n --_

allocate n bytes starting from HERE in the dictionary;
undo the last ALLOT with negative n to reclaim memory,
but beware: don't use negative n when new words were defined;
may throw -8 "dictionary overflow"

___
### COMPILE,
_xt --_

append execution token to dictionary;
may throw -8 "dictionary overflow"

    : COMPILE, , ;

___
### ,
_x --_

append cell to dictionary;
may throw -8 "dictionary overflow"

___
### C,
_char --_

append char to dictionary;
may throw -8 "dictionary overflow"

___
### 2,
_x1 x2 --_

append double cell to dictionary;
may throw -8 "dictionary overflow"

    : 2, , , ;

___
### NFA,
_c-addr u --_

append dictionary entry with name string;
set LASTXT to HERE;
may throw -8 "dictionary overflow"

    : NFA, HERE CURRENT @ , CURRENT ! DUP C, HERE SWAP DUP ALLOT CMOVE HERE TO LASTXT ;

___
### CODE
_"&lt;spaces&gt;name&lt;space&gt;" --_

parse name and append dictionary entry with name to execute machine code;
machine code should be appended to CODE definitions;
set LASTXT to HERE;
may throw -8 "dictionary overflow"

    : CODE PARSE-NAME NFA, ;

___
### CFA,
_addr --_

append cfa call addr to dictionary;
may throw -8 "dictionary overflow"

___
### :CFA
_-- addr colon_sys_

append cfa colon definition with cfa call addr to dictionary;
make CONTEXT the CURRENT vocabulary;
start compiling and leave HERE and colon_sys to end compiling with the ; word;
may throw -8 "dictionary overflow"

    : :CFA ] HERE colon_sys ['] (:) CFA, CURRENT TO CONTEXT ;

___
### CFA=
_xt1 xt2 -- flag_

true if xt1 has a cfa equal to a call to addr xt2;
used for introspection of
VARIABLE, 2VARIABLE and CREATE without DOES> that leave a pfa (VAR),
CREATE with DOES> (DOES),
VALUE (VAL), 2VALUE (2VAL),
CONSTANT (CON), 2CONSTANT (2CON),
DEFER (DEF), for example:
    3 VALUE X
    ' X ' (VAL) CFA=
leaves -1 (TRUE) meaning X is a VALUE which calls runtime (VAL)

    : CFA= OVER C@ $CD = -ROT SWAP 1+ @ = AND ;

___
### POSTPONE
_"&lt;spaces&gt;name&lt;space&gt;" --_

postpone compile action of name;
if name is immediate, then compile name instead of executing it;
otherwise compile name into the current colon definition;
can be used to create macros, e.g. : TRUE POSTPONE -1 ; IMMEDIATE;
may throw -13 "undefined word";
may throw -14 "interpreting a compile-only word"

___
### BUFFER:
_n "&lt;spaces&gt;name&lt;space&gt;" -- ; -- addr_

define buffer with n bytes;
executing name leaves address of n bytes

    : BUFFER: CREATE ALLOT ;

___
### :NONAME
_-- xt_

colon definition without name;
leaves the execution token of the definition to be used or saved

    : :NONAME HERE DUP TO LASTXT :CFA ;

___
### :
_-- ; C: "&lt;spaces&gt;name&lt;space&gt;" -- addr colon_sys_

define name and start compiling

    : : CODE HIDE :CFA ;

___
### ;
_-- ; C: addr colon_sys --_

end colon definition and stop compiling;
may throw -14 "interpreting a compile-only word";
may throw -22 "control structure mismatch"

    : ; ?COMP colon_sys <> IF -22 THROW THEN DROP POSTPONE (;) REVEAL [ ; IMMEDIATE

___
### EXIT
_--_

exit colon definition;
to EXIT the definition from within a DO-LOOP,
call UNLOOP first to remove DO-LOOP parameters from the return stack

    : EXIT ?COMP POSTPONE (EXIT) ; IMMEDIATE

___
### CREATE
_"&lt;spaces&gt;name&lt;space&gt;" -- ; -- addr_

create name;
executing name leaves address (HERE address after CREATE)

    : CREATE CODE ['] (VAR) CFA, ;

___
### DOES>
_-- ; ... -- ..._

change CREATE name behavior to execute code after DOES>

    : DOES> ?COMP POSTPONE (;DOES) ['] (DOES) CFA, ; IMMEDIATE

___
### VARIABLE
_"&lt;spaces&gt;name&lt;space&gt;" -- ; -- addr_

define a variable;
executing name leaves address of value (initialized to zero)

    : VARIABLE CREATE 0 , ;

___
### 2VARIABLE
_"&lt;spaces&gt;name&lt;space&gt;" -- ; -- addr_

define a double variable;
executing name leaves address of double value (initialized to zero)

    : 2VARIABLE CREATE 0 0 2, ;

___
### CONSTANT
_x "&lt;spaces&gt;name&lt;space&gt;" -- ; -- x_

define a constant;
executing name leaves x

    : CONSTANT CODE ['] (CON) CFA, , ;
    : CONSTANT CREATE , DOES> @ ;

___
### 2CONSTANT
_x1 x2 "&lt;spaces&gt;name&lt;space&gt;" -- ; -- x1 x2_

define a double constant;
executing name leaves x1 x2

    : 2CONSTANT CODE ['] (2CON) CFA, 2, ;
    : 2CONSTANT CREATE 2, DOES> 2@ ;

___
### VALUE
_x "&lt;spaces&gt;name&lt;space&gt;" -- ; -- x_

define a value;
executing name leaves x

    : VALUE CODE ['] (VAL) CFA, , ;

___
### 2VALUE
_dx "&lt;spaces&gt;name&lt;space&gt;" -- ; -- dx_

define a double value;
executing name leaves dx

    : 2VALUE CODE ['] (2VAL) CFA, 2, ;

___
### TO
_"&lt;spaces&gt;name&lt;space&gt;" -- ; x --_

assign value name;
may throw -32 "invalid name argument"

    : TO
      '
      DUP ['] (VAL) CFA= IF
        >BODY
        STATE @ IF
          POSTPONE (TO)
          ,
          EXIT
        THEN
        !
        EXIT
      THEN
      DUP ['] (2VAL) CFA= IF
        >BODY
        STATE @ IF
          POSTPONE (2TO)
          ,
          EXIT
        THEN
        2!
        EXIT
      THEN
      -32 THROW ; IMMEDIATE

___
### +TO
_"&lt;spaces&gt;name&lt;space&gt;" -- ; n --_

increment value name;
may throw -32 "invalid name argument"

    : +TO
      '
      DUP ['] (VAL) CFA= IF
        >BODY
        STATE @ IF
          POSTPONE (+TO)
          ,
          EXIT
          THEN
        +!
        EXIT
      THEN
      DUP ['] (2VAL) CFA= IF
        >BODY
        STATE @ IF
          POSTPONE (D+TO)
          ,
          EXIT
          THEN
        D+!
        EXIT
      -32 THROW ; IMMEDIATE

___
### DEFER
_"&lt;spaces&gt;name&lt;space&gt;" -- ; ... -- ..._

define a deferred name

    : DEFER CODE ['] (DEF) CFA, ['] UNDEF , ;

___
### DEFER!
_xt1 xt2 --_

store xt1 in deferred xt2

    : DEFER! >BODY ! ;

___
### DEFER@
_xt1 -- xt2_

fetch execution token from deferred xt1

    : DEFER@ >BODY @ ;

___
### IS
_xt "&lt;spaces&gt;name&lt;space&gt;" --_

assign execution token to deferred name;
may throw -32 "invalid name argument"

    : IS
      '
      DUP ['] (DEF) CFA= IF
        STATE @ IF
          LITERAL
          POSTPONE DEFER!
          EXIT
        THEN
        DEFER!
        EXIT
      THEN
      #-32 THROW ; IMMEDIATE

___
### ACTION-OF
_"&lt;spaces&gt;name&lt;space&gt;" -- xt_

fetch execution token of deferred name;
may throw -32 "invalid name argument"

    : ACTION-OF
      '
      DUP ['] (DEF) CFA= IF
        STATE @ IF
          LITERAL
          POSTPONE DEFER@
          EXIT
        THEN
        DEFER@
        EXIT
      THEN
      #-32 THROW ; IMMEDIATE

___
### LITERAL
_x -- ; -- x_

compile a literal

    : LITERAL ?COMP POSTPONE (LIT) , ; IMMEDIATE

___
### 2LITERAL
_x1 x2 -- ; -- x1 x2_

compile a double literal

    : 2LITERAL ?COMP POSTPONE (2LIT) 2, ; IMMEDIATE

___
### SLITERAL
_c-addr u -- ; -- c-addr u_

compile a string literal;
max literal string length is 255

    : SLITERAL
      ?COMP
      DUP 255 U> IF -18 THROW THEN
      POSTPONE (SLIT)
      DUP C,
      HERE OVER ALLOT SWAP CMOVE ; IMMEDIATE

___
### ."
_"ccc&lt;quote&gt;" -- ; --_

type "ccc" (compiled, not interpreted)

    : ." ?COMP '" PARSE SLITERAL POSTPONE TYPE ; IMMEDIATE

___
### C"
_"ccc&lt;quote&gt;" -- ; -- c-addr_

leave counted string "ccc" (compiled, not interpreted);
may throw -18 "parsed string overflow"

    : C" ?COMP POSTPONE S" POSTPONE DROP POSTPONE 1- ; IMMEDIATE

___
### S"
_"ccc&lt;quote&gt;" -- ; -- c-addr u_

leave string "ccc" (compiled and interpreted);
truncates string to 255 characters long when excessive

    : S" '" PARSE SDUP ; IMMEDIATE

___
### S\"
_"ccc&lt;quote&gt;" -- ; -- c-addr u_

leave string "ccc" (compiled and interpreted);
"ccc" may include \-escape codes translated by the S\>S word;
truncates string to 255 characters long when excessive

    : S\" '" PARSE S\>S SDUP ; IMMEDIATE

___
### S\>S
_c-addr u -- c-addr u_

convert string in place by translating \-escape codes

code | corresponding ASCII character code
---- | ----------------------------------------------------------
\\   | \ backslash (92)
\a   | BEL (7)
\b   | BS (8)
\e   | ESC (27)
\f   | FF (12)
\l   | LF (10)
\m   | CR/LF (13 then 10)
\n   | LF (10)
\q   | " quote (34)
\r   | CR (13)
\t   | TAB (9)
\v   | VT (11)
\xhh | hh in hex (0xhh)
\z   | NUL (0)
\G   | MSX graphic character header (1) e.g. \GA

___
### SDUP
_c-addr1 u -- c-addr2 u_

duplicate string to a TMP buffer or to a string literal when compiling;
stores the copy of the string as a counted string at c-addr2 - 1;
truncates the copy to 255 characters long when excessive

    : SDUP
      STATE @ IF
        SLITERAL
        EXIT
      THEN
      255 UMIN
      TMP
      2DUP C!
      1+
      SWAP
      2DUP 2>R
      CMOVE
      2R> ;

___
### I
_-- n_

the loop counter value of innermost do-loop

___
### J
_-- n_

the loop counter value of outer (second) do-loop

___
### K
_-- n_

the loop counter value of outer (third) do-loop

___
### AHEAD
_-- ; C: -- addr orig_

branch ahead to THEN;
may throw -14 "interpreting a compile-only word"

___
### BEGIN
_-- ; C: -- addr dest_

begin WHILE REPEAT;
may throw -14 "interpreting a compile-only word"

___
### AGAIN
_-- ; C: addr dest --_

branch back to BEGIN;
may throw -14 "interpreting a compile-only word";
may throw -22 "control structure mismatch"

___
### UNTIL
_x -- ; C: addr dest --_

branch back to BEGIN if x = 0 (FALSE);
may throw -14 "interpreting a compile-only word";
may throw -22 "control structure mismatch"

___
### IF
_x -- ; C: -- addr orig_

branch to closest ELSE or THEN if x = 0 (FALSE);
may throw -14 "interpreting a compile-only word"

___
### THEN
_-- ; C: addr orig --_

close AHEAD, IF, ELSE;
may throw -14 "interpreting a compile-only word";
may throw -22 "control structure mismatch"

___
### ELSE
_-- ; C: addr orig -- addr orig_

close IF and branch to THEN;
may throw -14 "interpreting a compile-only word";
may throw -22 "control structure mismatch"

___
### WHILE
_x -- ; C: addr sys -- addr orig addr sys_

branch to exit REPEAT if x = 0 (FALSE);
may throw -14 "interpreting a compile-only word"

___
### REPEAT
_-- ; C: addr orig addr dest --_

branch back to BEGIN after WHILE;
may throw -14 "interpreting a compile-only word";
may throw -22 "control structure mismatch"

___
### DO
_n1|u1 n2|u2 -- ; C: -- addr do_sys_

begin loop from initial value n2|u2 to the limit n1|u1;
loop at least once;
pushes do-loop parameters onto the return stack;
may throw -14 "interpreting a compile-only word"

___
### ?DO
_n1|u1 n2|u2 -- ; C: -- addr do_sys_

begin loop from initial value n2|u2 to the limit n1|u1;
pushes do-loop parameters onto the return stack;
skip loop when zero trip loop;
may throw -14 "interpreting a compile-only word"

___
### LOOP
_-- ; C: addr do_sys --_

repeat loop unless loop counter crosses the limit;
may throw -14 "interpreting a compile-only word";
may throw -22 "control structure mismatch"

___
### +LOOP
_n|u -- ; C: addr do_sys --_

increment counter and repeat loop unless counter crosses the limit;
may throw -14 "interpreting a compile-only word";
may throw -22 "control structure mismatch"

___
### UNLOOP
_--_

remove do-loop parameters from the return stack;
may throw -14 "interpreting a compile-only word"

___
### ?LEAVE
_x --_

if x is nonzero (not FALSE) then exit the innermost do-loop;
may throw -14 "interpreting a compile-only word"

___
### LEAVE
_--_

exit the innermost do-loop;
may throw -14 "interpreting a compile-only word"

___
### CASE
_x -- ; C: -- 0_

begin CASE ENDCASE switch;
may throw -14 "interpreting a compile-only word"

___
### OF
_x1 x2 -- x1 or x1 x2 -- ; C: n1 -- orig n2_

take CASE arm if x1 = x2;
otherwise branch to next OF;
may throw -14 "interpreting a compile-only word"

___
### ENDOF
_-- ; C: n -- orig n_

branch to ENDCASE;
may throw -14 "interpreting a compile-only word";
may throw -22 "control structure mismatch"

___
### ENDCASE
_x -- ; C: n*orig n --_

close CASE;
may throw -14 "interpreting a compile-only word";
may throw -22 "control structure mismatch"

___
### ERROR
_n --_

display exception n at the offending location in the input;
n = 0 return without displaying an error message;
n = -1 ABORT and n = -2 ABORT" also clears the parameter stack
               n = -28 break is displayed as <STOP>
n = -56 QUIT stays silent;
other errors n are displayed as <ERR-n> switched to decimal,
a line number is included <ERR-n:line> when loading from files
list of Forth errors:

code | error
---- | ---------------------------------------------------------
  -1 | ABORT
  -2 | ABORT"
  -3 | stack overflow
  -4 | stack underflow
  -5 | return stack overflow
  -6 | return stack underflow
  -8 | dictionary overflow
 -10 | division by zero
 -11 | result out of range
 -13 | undefined word
 -14 | interpreting a compile-only word
 -15 | invalid FORGET
 -16 | attempt to use zero-length string as a name
 -18 | parsed string overflow
 -19 | definition name too long
 -22 | control structure mismatch
 -24 | invalid numeric argument
 -28 | user interrupt (BREAK was pressed)
 -32 | invalid name argument (invalid TO name)
 -36 | invalid file position
 -37 | file I/O exception
 -38 | non-existent file
 -39 | unexpected end of file
 -42 | floating-point divide by zero
 -43 | floating-point result out of range
 -46 | floating-point invalid argument
 -56 | QUIT
-256 | execution of an uninitialized deferred word

code | MSX error = code + 256
---- | ---------------------------------------------------------
-197 | 59 file not open
-198 | 58 sequentual I/O only
-200 | 56 incorrect file name
-202 | 54 file already open
-203 | 53 file not found
-204 | 52 bad file number
-236 | 20 verify error
-231 | 25 line buffer overflow
-237 | 19 device I/O error
-243 | 13 type mismatch
-245 | 11 division by zero
-250 |  6 numeric overflow
-251 |  5 invalid function argument

    : ERROR
      DUP -2 0 WITHIN IF
        CLEAR
        EXIT
      THEN
      DUP -56 = IF
        DROP
        EXIT
      THEN
      DUP -28 = IF
        DROP
        ." <STOP>"
        EXIT
      THEN
      ?DUP IF
        SOURCE >IN @
        2DUP U> + UMIN TYPE
        ." <ERR"
        DECIMAL
        0 .R
        #IN ?DUP IF
          ': EMIT 0 .R
        THEN
        '> EMIT
      THEN ;

___
### HANDLER
_-- addr_

variable with saved return stack pointer

    VARIABLE HANDLER

___
### EXECUTE
_... xt -- ..._

execute execution token xt

___
### CATCH
_... xt -- ... 0 or xt -- n_

execute xt leaving nonzero exception code n or 0 when no exception occurred;
when an exception was caught, the parameter and return stacks are restored
to their state before execution of xt

    : CATCH
      SP@ >R
      HANDLER @ >R
      RP@ HANDLER !
      EXECUTE
      R> HANDLER !
      RDROP
      0 ;

___
### THROW
_0 -- or ... n -- ... n_

throw exception n if nonzero

    : THROW
      ?DUP IF
        HANDLER @ ?DUP IF
          RP!
          R> HANDLER !
          R> SWAP >R
          SP!
          DROP
          R>
          EXIT
        THEN
        >R CLEAR R>
        ERROR
        REPL
      THEN ;

___
### QUIT
_... -- ; R: ... --_

throw -56 "QUIT";
no exception error is displayed;
unlike ABORT, the parameter stack is not cleared

    : QUIT -56 THROW ;

___
### ABORT
_... -- ; R: ... --_

throw -1 "ABORT";
clears the parameter stack unless caught with CATCH

    : ABORT -1 THROW ;

___
### ABORT"
_... flag -- ; C: "ccc&lt;quote&gt;" -- ; R: ... --_

if flag then abort with string message unless an active catch is present;
throw -2 "ABORT"";
clears the parameter stack unless caught with CATCH;
may throw -14 "interpreting a compile-only word"

    : ABORT" ?COMP POSTPONE S" POSTPONE (ABORT") ; IMMEDIATE

___
### BYE
_--_

return to BASIC

___
### (
_"ccc&lt;paren&gt;" --_

start a comment block;
parse and skip input up to the closing )

    : (
      ') PARSE
      BEGIN
        + DROP
        SOURCE + = IF
          DROP REFILL
        ELSE
          C@ ') <> IF
            REFILL
          ELSE
            FALSE
          THEN
        THEN
      0= UNTIL ; IMMEDIATE

___
### \
_"ccc&lt;eol&gt;" --_

start a comment line;
parse and skip input up to the end of line

    : \ $A PARSE 2SROP ;

___
### OK
_"ccc&lt;eol&gt;" --_

start a comment line;
parse and skip input up to the end of line;
same as \ but not immediate,
so that screen editing of Forth output before OK is made possible

    : OK POSTPONE \ ;

___
### .(
_"ccc&lt;paren&gt;" --_

emit CR then type "ccc" up to the closing )

    : .( ') PARSE CR TYPE ; IMMEDIATE

___
### NUMBER
_c-addr u -- n|u|d|ud|r_

convert string to number;
prefixed with character $ converts hex;
prefixed with character # converts decimal;
prefixed with character % converts binary;
sets value DBL to -1 when the number is a double;
sets value DBL to 1 when the number is a float;
otherwise sets value DBL to 0;
may throw -13 "undefined word" when string is not numeric

    : NUMBER
      2DUP >DOUBLE IF
        2SWAP 2DROP
        DBL INVERT IF
          d>S
        THEN
        STATE @ IF
          DBL IF
            2LITERAL
            EXIT
          THEN
          LITERAL
        THEN
        EXIT
      THEN
      >FLOAT IF
        1 TO DBL
        STATE @ IF
          2LITERAL
        THEN
        EXIT
      THEN
      -13 THROW ;

___
### INTERPRET
_--_

interpret input while input is available

    : INTERPRET
      BEGIN BL PARSE-WORD ?DUP WHILE
        2DUP
        FIND-WORD ?DUP IF
          2SWAP 2DROP
          STATE @ = IF
            COMPILE,
          ELSE
            EXECUTE
          THEN
        ELSE
          DROP
          NUMBER
        THEN
      REPEAT
      DROP ;

___
### EVALUATE
_... c-addr u -- ..._

evaluate string

    : EVALUATE
      SAVE-INPUT N>R
      TO SOURCE
      >IN OFF
      -1 TO SOURCE-ID
      ['] INTERPRET CATCH
      DUP ERROR
      NR> RESTORE-INPUT DROP
      THROW ;

___
### REPL
_--_

read-evaluate-print loop

    : REPL
      RP0@ RP!
      HANDLER OFF
      0 TO SOURCE-ID
      CR
      [
      BEGIN
        BEGIN ['] REFILL CATCH ?DUP WHILE
          ERROR CR
        REPEAT
      WHILE
        ['] INTERPRET CATCH ?DUP IF
          ERROR
          REPL
        THEN
        STATE @ INVERT IF
          ."  OK "
          DEPTH ?DUP IF . THEN CR
        THEN
      REPEAT
      BYE ;

___
### MARKER
_"&lt;spaces&gt;name&lt;space&gt;" -- ; --_

define a dictionary marker;
executing the name deletes marker and all definitions made after;
beware of vocabulary definitions crossings
(other vocabulary DEFINITIONS after markers also get deleted and corrupt their vocabulary)

    : MARKER
      CURRENT
      DUP @
      HERE
      CREATE
        , 2,
      DOES>
        DUP CELL+ 2@
        SWAP TO CONTEXT
        DUP CONTEXT !
        DEFINITIONS
        L>NAME NAME> TO LASTXT
        @ HERE - ALLOT ;

___
### FENCE
_-- addr_

only permit FORGET past the dictionary FENCE address

    HERE VALUE FENCE

___
### FORGET
_"&lt;spaces&gt;name&lt;space&gt;" --_

delete name and all following definitions;
may throw -15 "invalid FORGET";
beware of vocabulary definitions crossings
(other vocabulary DEFINITIONS after markers also get deleted and corrupt their vocabulary)

___
### CONTEXT
_-- addr_

leaves address of link of the last vocabulary context definition

    ' FORTH VALUE CONTEXT

___
### CURRENT
_-- addr_

leaves address of link of the last current vocabulary definition

    ' FORTH VALUE CURRENT

___
### DEFINITIONS
_--_

make CURRENT the CONTEXT vocabulary

    : DEFINITIONS CONTEXT TO CURRENT ;

___
### VOCABULARY
_"&lt;spaces&gt;name&lt;space&gt;" --_

define a new vocabulary

    : VOCABULARY CURRENT CREATE , fig_kludge , DOES> TO CONTEXT ;

___
### FORTH
_--_

make FORTH the CONTEXT vocabulary

    VOCABULARY FORTH



File-access words
--------------------------

___
### DRV
_-- c-addr_

last used drive letter, the default drive when none is specified explicitly, initially drive A

___
### FXB
_-- addr_

array of FCB+FIB per open file

    CREATE FXB 37 4 + #IB + FCBN * ALLOT

___
### S>FCB
_c-addr u -- addr_

store the filename string of the form [D:]FILENAME[.EXT] in a new FCB;
wildcard '?' matches any character when used in FILENAME and EXT;
wildcard '*' matches any sequence of characters, at most one '*' may be used in FILENAME and in EXT;
may throw -204 "bad file number" when max files are in use

    : S>FCB
      \ drive letter or use default drive
      S" :" SEARCH IF
        OVER 1- C@ TO DRV
        1 /STRING
      THEN
      (FCB)
      DUP 37 ERASE
      DUP 12 BLANK
      \ c-addr u fcb-addr
      \ set drive number
      DRV $1f AND OVER C!
      \ set filename, expand * into ???...
      -ROT
      \ fcb-addr c-addr u
      8 0 ?DO
        DUP 0= ?LEAVE
        OVER C@ '. = ?LEAVE
        OVER C@ '* = IF
          '?
        ELSE
          NEXT-CHAR
        THEN
        3 PICK 1+ I + C!
      LOOP
      \ set extension, expand * into ???...
      S" ." SEARCH IF
        1 /STRING
        3 0 ?DO
          DUP 0= ?LEAVE
          OVER C@ '* = IF
            '?
          ELSE
            NEXT-CHAR
          THEN
          3 PICK 9 + I + C!
        LOOP
      THEN
      2DROP ;

___
### FIB
_fileid -- c-addr_

the file input buffer associated with fileid;
used by INCLUDE-FILE, INCLUDE, INCLUDED

___
### BIN
_fam -- fam_

CREATE-FILE and OPEN-FILE mode fam;
note: files are always treaded as binary

___
### W/O
_-- fam_

CREATE-FILE and OPEN-FILE mode fam

___
### R/O
_-- fam_

CREATE-FILE and OPEN-FILE mode fam

___
### R/W
_-- fam_

CREATE-FILE and OPEN-FILE mode fam

___
### CREATE-FILE
_c-addr u fam -- fileid ior_

create a new file given by the filename string c-addr u, where fam is R/W, R/O or W/O;
if the file already exists, then it is truncated to zero length;
leaves fileid (a fcb-addr) and ior 0 (success) or -203 (failure)

___
### OPEN-FILE
_c-addr u fam -- fileid ior_

open a file or device given by the filename string c-addr u, where fam is R/W, R/O or W/O;
leaves fileid (a fcb-addr) and ior 0 (success) or -203 (failure);
filename format: [D:]FILENAME[.EXT] where D: becomes the default drive when specified;
device names are AUX, CON, LST, NUL, and PRN without a drive specified

___
### CLOSE-FILE
_fileid -- ior_

close file with fileid (a fcb-addr);
leaves ior 0 (success) or -197 (failure)

___
### CLOSE-FILES
_--_

close all open files

___
### READ-FILE
_c-addr u1 fileid -- u2 ior_

read into buffer c-addr of size u1 from fileid (a fcb-addr);
leaves number u2 of bytes read into the buffer and ior 0 (success) or 1 (u2 is zero and eof) or nz (other failure)
to read a single character to a cell on the stack: 0 SP@ 1 fileid READ-FILE -- char 0|1 ior


___
### GET-LINE
_fileid -- c-addr u flag ior_

sequentially read next line from fileid data buffered in FIB;
leaves c-addr and length u (without terminating CR/LF) and flag TRUE;
leaves flag FALSE when u is zero and EOF is reached;
ior is nonzero when an error occurred, use GET-LINE 0= AND 0= which is TRUE for EOF or error
does not support REPOSITION-FILE except for position zero to rewind

    : GET-LINE
      DUP FILE-POSITION DROP D0= IF \ at start of file
        DUP FIB 2- DUP OFF 2- OFF \ reset len and pos
      THEN
      DUP FIB DUP 2- DUP @ SWAP 2- @ SWAP \ -- fileid fib len pos
      OVER UMIN /STRING        \ -- fileid fib+pos len-pos
      TUCK                     \ -- fileid len-pos fib+pos len-pos
      10 CHOP                  \ -- fileid len-pos fib+pos u
      ROT                      \ -- fileid fib+pos u len-pos
      OVER = IF                \ CHOPed no LF as u=len-pos
        2>R DUP FIB R> 2DUP R> -ROT \ -- fileid fib u fib+pos fib u
        CMOVE                  \ move fib+pos to fib u bytes
        TUCK                   \ -- fileid u fib u
        #IB SWAP               \ -- fileid u fib #ib u
        /STRING                \ -- fileid u fib+u #ib-u
        3 PICK READ-FILE ?DUP IF \ no more data
          >R                   \ -- fileid u u2
          +                    \ -- fileid u+u2
          SWAP FIB SWAP        \ -- fib u+u2
          FALSE R>
          DUP 2/ 0= IF         \ ior is 0 or 1
            2DROP DUP 0= 0= 0  \ -- fib u+u2 flag 0
          THEN
          EXIT
        THEN                   \ -- fileid u u2
        +                      \ -- fileid u+u2
        OVER FIB               \ --fileid u+u2 fib
        2DUP 2- DUP OFF 2- !   \ set len to u+u2 and pos to 0
        SWAP                   \ -- fileid fib len
        10 CHOP
      THEN                     \ -- fileid fib+pos u
      ROT FIB                  \ -- fib+pos u fib
      OVER 1+ SWAP 2- +!       \ increment pos by u+1 -- fib+pos u
      13 -TRIM TRUE 0
    ;

___
### READ-LINE
_c-addr u1 fileid -- u2 flag ior_

sequentially read a line into buffer c-addr of size u1 from fileid;
leaves number u2 of bytes read into the buffer (without terminating CR/LF) and flag TRUE;
leaves flag FALSE when u2 is zero and EOF is reached;
ior is nonzero when an error occurred, use READ-LINE 0= AND 0= which is TRUE for EOF or error;
does not support REPOSITION-FILE except for position zero to rewind

    : READ-LINE
      GET-LINE 2>R R@ IF
        2DROP 2DROP 0
      ELSE
        2SWAP ROT UMIN DUP>R CMOVE R>
      THEN
      2R>
    ;

___
### WRITE-FILE
_c-addr u1 fileid -- ior_

write buffer c-addr of size u1 to fileid (a fcb-addr);
leaves ior 0 (success) or 1 (disk full) or nz (other failure)

___
### WRITE-LINE
_c-addr u1 fileid -- ior_

write buffer c-addr of size u1 to fileid (a fcb-addr) followed by a CR/LF pair;
leaves ior 0 (success) or 1 (disk full)

___
### FILE-POSITION
_fileid -- ud ior_

for the open fileid get the current file position
leaves file position ud and ior (always 0 for success)

___
### REPOSITION-FILE
_ud fileid -- ior_

for the open fileid set the file position to ud;
leaves ior (always 0 for success)

___
### FILE-SIZE
_fileid -- ud ior_

for the open fileid get the size of the file;
leaves file size ud and ior (always 0 for success)

___
### RESIZE-FILE
_ud fileid -- ior_

rfor the open fileid esize the file to ud bytes;
leaves ior 0 (success) or 1 (disk full) or nz (other failure)

___
### DELETE-FILE
_c-addr u -- ior_

delete the file with the name string c-addr u;
leaves ior 0 (success) or nz (failure)

___
### RENAME-FILE
_c-addr1 u1 c-addr2 u2 -- ior_

rename the file with the name string c-addr1 u1 to c-addr2 u2;
leaves ior 0 (success) or nz (failure)

___
### INCLUDE-FILE
_... fileid -- ..._

read and interpret Forth source code from fileid;
fileid is closed afterwards

    : INCLUDE-FILE
      SAVE-INPUT N>R
      TO SOURCE-ID
      BEGIN 0 REFILL WHILE
        DROP
        ['] INTERPRET CATCH
        ?DUP 0= WHILE
      REPEAT THEN
      SOURCE-ID CLOSE-FILE DROP
      DUP ERROR
      NR> RESTORE-INPUT DROP
      THROW ;

___
### INCLUDED
_... c-addr u -- ..._

read and interpret Forth source code from the file named by the string c-addr u

    : INCLUDED
      R/O OPEN-FILE THROW
      INCLUDE-FILE ;

___
### INCLUDE
_... "&lt;spaces&gt;name&lt;space&gt;" -- ..._

read and interpret Forth source code from file "name"

    : INCLUDE PARSE-NAME INCLUDED ;

___
### REQUIRED
_... c-addr u -- ..._

read and interpret Forth source code from the file named by the string c-addr u,
if the file was not already included;
this also adds file name as a MARKER with a leading '~' to the dictionary;
beware of vocabulary definitions crossings
(other vocabulary DEFINITIONS after markers also get deleted and corrupt their vocabulary)

    : REQUIRED
      SDUP -1 /STRING OVER '~ SWAP C!
      2DUP FIND-WORD NIP IF
        2DROP
        EXIT
      THEN
      CURRENT DUP @ 2>R HERE >R
      2DUP NFA, marker_does CFA,  \ create marker with marker DOES> cfa
      R> , 2R> 2,                 \ marker body stores HERE CURRENT CURRENT@
      1 /STRING INCLUDED ;

___
### REQUIRE
_... "&lt;spaces&gt;name&lt;space&gt;" -- ..._

read and interpret Forth source code from file "name",
if the file was not already included;
this also adds file name with a leading '~' to the dictionary to assert inclusion;
beware of vocabulary definitions crossings
(other vocabulary DEFINITIONS after markers also get deleted and corrupt their vocabulary)

    : REQUIRE PARSE-NAME REQUIRED ;

___
### ANEW
_... "&lt;spaces&gt;name&lt;space&gt;" -- ..._

read and interpret Forth source code from file "name",
same as REQUIRE,
but anew by deleting all previously included definitions from the dictionary first

    : ANEW
      PARSE-NAME SDUP -1 /STRING OVER '~ SWAP C!
      2DUP FIND-WORD IF
        EXECUTE
      ELSE
        DROP
      THEN
      1 /STRING REQUIRED ;



MSX Math-Pack BCD floating-point math words
-------------------------------------------

Floating-point literals are specified in scientific notation with an exponent,
even when the exponent is zero.  For example, `1E0` is floating-point 1,
but also `1e` without exponent digits following the lower-case `e`.

Because MSX BASIC floating-point values may be specified with a trailing `!` or
`#`, ForthMSX supports this notation also, e.g. `1#` is floating-point 1.
Likewise, `1d` is floating-point 1, and perhaps strangely, `&h1` also.

Floating point values are stored as doubles on the stack.  Double-moving Words,
such as `2DUP`, can be used to manipulate floats.  Floats can be stored in
`2CONSTANT`, `2VARIABLE`, and in `2VALUE` assigned with `TO` but not with `+TO`.

Beware that `HEX` prevents inputting floats and garbles the output of floats.

___
### FTRUNC
_r1 -- r2_

truncate float towards zero

___
### FLOOR
_r1 -- r2_

floor float towards negative infinity

___
### FNEGATE
_r1 -- r2_

negate float

___
### FABS
_r1 -- r2_

absolute value |r1|

    : FABS 2DUP F0< IF FNEGATE THEN ;

___
### FSQRT
_r1 -- r2_

take the square root of r1

___
### FSIN
_r1 -- r2_

sine of float in radian

___
### FCOS
_r1 -- r2_

cosine of float in radian

___
### FTAN
_r1 -- r2_

tangent of float in radian

___
### FATAN
_r1 -- r2_

arc tangent of float, in radian

___
### FRAND
_r1 -- r2_

if r1 is positive, then leave new random number from 0 to 1 exclusive;
if r1 is zero, then leave the last random number;
if r1 is negative, then seed the random number using r1

___
### F+
_r1 r2 -- r3_

sum r1+r2

___
### F-
_r1 r2 -- r3_

difference r1-r2

___
### F*
_r1 r2 -- r3_

product r1*r2

___
### F/
_r1 r2 -- r3_

quotient r1/r2

___
### F**
_r1 r2 -- r3_

raise r1 to r2

___
### FASIN
_r1 -- r2_

arc sine of float, in radian

    : FASIN
      2DUP F0= IF EXIT THEN
      2DUP FABS 1E0 F= IF                           \ if |x|=1 then
        PI/2 2SWAP F0< IF FNEGATE THEN              \ sign(x)*pi/2
        EXIT
      THEN
      2DUP 2DUP F* 1E0 2SWAP F- FSQRT FATAN2 ;      \ arctan(x/sqrt(1-x^2)) = atan2(x,sqrt(1-x*x))

___
### FACOS
_r1 -- r2_

arc cosine of float, in radian

    : FACOS FASIN PI/2 2SWAP F- ;

___
### FATAN2
_r1 r2 -- r3_

atan2(r1,r2) = atan(r1/r2) but using a more accurate formulation

___
### PI
_-- r_

floating-point constant pi

    3.14159E0 2CONSTANT PI

___
### PI/2
_-- r_

floating-point constant pi/2 (half pi)

    1.57080E0 2CONSTANT PI/2

___
### FLN
_r1 -- r2_

natural log of float

___
### FEXP
_r1 -- r2_

natural exponent of float

___
### FLOG
_r1 -- r2_

base 10 log of float

    : FLOG FLN 0.434294E0 F* ;    \ = ln(x)/ln(10) approx ln(10) such that 10E0 FLOG = 1E0

___
### FALOG
_r1 -- r2_

base 10 exponent of float

    : FALOG 2.30259E0 F* FEXP ;    \ = exp(x*ln(10))

___
### FSINH
_r1 -- r2_

sine hyperbolicus of float

    : FSINH FEXP 2DUP 1E0 2SWAP F/ F- .5E0 F* ;

___
### FCOSH
_r1 -- r2_

cosine hyperbolicus of float

    : FCOSH FEXP 2DUP 1E0 2SWAP F/ F+ .5E0 F* ;

___
### FTANH
_r1 -- r2_

tangent hyperbolicus of float

    : FTANH 2DUP F+ FEXP 2DUP 1E0 F- 2SWAP 1E0 F+ F/ ;

___
### FASINH
_r1 -- r2_

arc sine hyperbolicus of float

    : FASINH 2DUP 2DUP F* 1E0 F+ FSQRT F+ FLN ;

___
### FACOSH
_r1 -- r2_

arc cosine hyperbolicus of float

    : FACOSH 2DUP 2DUP F* 1E0 F- FSQRT F+ FLN ;

___
### FATANH
_r1 -- r2_

arc tangent hyperbolicus of float

    : FATANH 2DUP 1E0 F+ 2SWAP 1E0 2SWAP F- F/ FLN .5E0 F* ;

___
### F=
_r1 r2 -- flag_

true if r1 = r2

___
### F<
_r1 r2 -- flag_

true if r1 < r2

    : F<
      DUP 3 PICK AND 0< IF
        2SWAP
      D< ; ( works for MSX MATH-PACK decimals )

___
### F0=
_r -- flag_

true if r = 0.0e0

    : F0= D0= ; ( works for MSX MATH-PACK decimals )

___
### F0<
_r -- flag_

true if r < 0.0e0

    : F0< D0< ; ( works for MSX MATH-PACK decimals )

___
### FMAX
_r1 r2 -- r3_

max of r1 and r2

    : FMAX
      2OVER 2OVER F< IF 2SWAP THEN
      2DROP ;

___
### FMIN
_r1 r2 -- r3_

min of r1 and r2

    : FMIN
      2OVER 2OVER F< INVERT IF 2SWAP THEN
      2DROP ;

___
### D>F
_d -- r_

widen signed double to float;
this word is much slower than the optimized S>F

    : D>F
      BASE @ -ROT
      DECIMAL
      TUCK DABS <# #S ROT SIGN #> >FLOAT DROP
      ROT BASE ! ;

___
### S>F
_n -- r_

widen signed single to float

    : S>F S>D D>F ;

___
### F>D
_r -- d_

narrow float to a signed double;
may throw -11 "result out of range";
this word is much slower than the optimized F>S

    : F>D
      HERE 1+ 10 REPRESENT DROP SWAP
      DUP 0< IF
        2DROP 0.
        EXIT
      THEN
      DUP 10 > IF
        -11 THROW
      THEN
      '- HERE C!       \ place '-' in here
      OVER -           \ sign exp-sign
      SWAP HERE 1+ +   \ exp-sign here+1+sign
      SWAP             \ here+1+sign exp-sign
      >DOUBLE 0= IF
        -11 THROW
      THEN ;

___
### F>S
_r -- n_

narrow float to a signed single;
may throw -11 "result out of range" or -250 "numeric overflow"

    : F>S F>D D>S ;

___
### >FLOAT
_c-addr u -- r true | false_

convert string to float;
leaves the float and true if string is converted;
leaves false if string is unconvertable;

___
### REPRESENT
_r c-addr u -- n flag true_

convert float to string;
store decimal digits of the float in buffer c-addr with size u > 0;
leaves decimal exponent n+1 and flag = true if negative

___
### PRECISION
_-- +n_

floating-point output precision, the number of decimal digits displayed is 6 by default

    6 VALUE PRECISION

___
### FS.
_r --_

output float in scientific notation with a trailing space

    : FS.
      HERE PRECISION REPRESENT DROP IF
        '- EMIT
      THEN
      HERE C@ DUP EMIT
      '0 <> +
      '. HERE C!
      HERE PRECISION '0 -TRIM TYPE
      'E EMIT . ;

___
### F.
_r --_

output float with a trailing space;
output fixed notation when 1e-1 <= |r| < 1e+7, otherwise output scientific notation;
beware that non-scientific output cannot be copy-pasted back into input,
as floating-point literals require an exponent

    : F.
      HERE PRECISION REPRESENT DROP IF
        '- EMIT
      THEN
      DUP 0 PRECISION 1+ WITHIN IF
        DUP IF
          HERE OVER TYPE
        ELSE
          '0 EMIT
        THEN
        '. EMIT
        HERE OVER +
        PRECISION ROT - '0 -TRIM TYPE SPACE
        EXIT
      THEN
      HERE C@ EMIT
      '. HERE C!
      HERE PRECISION '0 -TRIM TYPE
      'E EMIT 1- . ;



Alternative IEEE-754 floating-point math words (not enabled)
------------------------------------------------------------

Floating-point literals must be specified in scientific notation with an exponent,
even when the exponent is zero.  For example, `1E0` is floating point 1,
but also `1e` without exponent digits following the lower-case `e`.

Floating point values are stored as doubles on the stack.  Double-moving Words,
such as `2DUP`, can be used to manipulate floats.  Floats can be stored in
`2CONSTANT`, `2VARIABLE`, and in `2VALUE` assigned with `TO` but not with `+TO`.

Beware that `HEX` prevents inputting floats and garbles the output of floats.

___
### F+
_r1 r2 -- r3_

sum r1+r2;
may throw -43 "floating-point result out of range"

___
### F-
_r1 r2 -- r3_

difference r1-r2;
may throw -43 "floating-point result out of range"

___
### F*
_r1 r2 -- r3_

product r1*r2;
may throw -43 "floating-point result out of range"

___
### F/
_r1 r2 -- r3_

quotient r1/r2;
may throw -42 "floating-point divide by zero";
may throw -43 "floating-point result out of range"

___
### FTRUNC
_r1 -- r2_

truncate float towards zero

___
### FLOOR
_r1 -- r2_

floor float towards negative infinity;
may throw -43 "floating-point result out of range"

___
### FROUND
_r1 -- r2_

round float to nearest;
may throw -43 "floating-point result out of range"

___
### FNEGATE
_r1 -- r2_

negate float

___
### FABS
_r1 -- r2_

absolute value |r1|

    : FABS 2DUP F0< IF FNEGATE THEN ;

___
### F=
_r1 r2 -- flag_

true if r1 = r2

    : F= D= ; ( works for IEEE 754 floating-point without negative zero and inf/nan )

___
### F<
_r1 r2 -- flag_

true if r1 < r2

    : F<
      DUP 3 PICK AND 0< IF
        2SWAP
      D< ; ( works for IEEE 754 floating-point without negative zero and inf/nan )

___
### F0=
_r -- flag_

true if r = 0.0e0

    : F0= D0= ; ( works for IEEE 754 floating-point without negative zero and inf/nan )

___
### F0<
_r -- flag_

true if r < 0.0e0

    : F0< D0< ; ( works for IEEE 754 floating-point without negative zero and inf/nan )

___
### FMAX
_r1 r2 -- r3_

max of r1 and r2

    : FMAX
      2OVER 2OVER F< IF 2SWAP THEN
      2DROP ;

___
### FMIN
_r1 r2 -- r3_

min of r1 and r2

    : FMIN
      2OVER 2OVER F< INVERT IF 2SWAP THEN
      2DROP ;

___
### D>F
_d -- r_

widen signed double to float

___
### S>F
_n -- r_

widen signed single to float

___
### F>D
_r -- d_

narrow float to a signed double;
may throw -11 "result out of range"

___
### F>S
_r -- n_

narrow float to a signed single;
may throw -11 "result out of range"

___
### >FLOAT
_c-addr u -- r true | false_

convert string to float;
leaves the float and true if string is converted;
leaves false if string is unconvertable

___
### REPRESENT
_r c-addr u -- n flag true_

convert float to string;
store decimal digits of the float in buffer c-addr with size u > 0;
leaves decimal exponent n+1 and flag = true if negative

___
### PRECISION
_-- +n_

floating-point output precision, the number of decimal digits displayed is 7 by default

    7 VALUE PRECISION

___
### FS.
_r --_

output float in scientific notation with a trailing space

    : FS.
      HERE PRECISION REPRESENT DROP IF
        '- EMIT
      THEN
      HERE C@ EMIT
      '. HERE C!
      HERE PRECISION '0 -TRIM TYPE
      'E EMIT 1- . ;

___
### F.
_r --_

output float with a trailing space;
output fixed notation when 1e-1 <= |r| < 1e+7, otherwise output scientific notation

;    : F.
      HERE PRECISION REPRESENT DROP IF
        '- EMIT
      THEN
      DUP 0 PRECISION 1+ WITHIN IF
        DUP IF
          HERE OVER TYPE
        ELSE
          '0 EMIT
        THEN
        '. EMIT
        HERE OVER +
        PRECISION ROT - '0 -TRIM TYPE SPACE
        EXIT
      THEN
      HERE C@ EMIT
      '. HERE C!
      HERE PRECISION '0 -TRIM TYPE
      'E EMIT 1- . ;

___
### FSQRT
_r1 -- r2_

square root of float

    : FSQRT
      2DUP F0< IF -46 THROW THEN
      2DUP F0= IF EXIT THEN
      \ map r1 to [0.5,2) using sqrt(x*2^n) = sqrt(x*2^(n%2))*2^(n/2)
      DUP 8 RSHIFT $3f - -ROT               \ 2^(n/2) = 2^(exponent/2 - bias/2)
      $ff AND $3f00 +                       \ remove exponent 2^(n/2) from x
      2DUP                                  \ initial estimate y is x
      5 0 DO                                \ 5 Newton-Raphson iterations
        2OVER 2OVER F/ F+ .5E0 F*           \ x y -- x (y+x/y)/2
      LOOP
      2SWAP 2DROP                           \ x y -- y
      ROT $7f + 7 LSHIFT 0 SWAP F* ;        \ y times 2^(n/2)

___
### PI
_-- r_

floating-point constant pi

    3.1415928E0 2CONSTANT PI

___
### PI/2
_-- r_

floating-point constant pi/2 (half pi)

    1.5707964E0 2CONSTANT PI/2

___
### FCOSI
_r1 flag -- r2_

if flag is -1 (TRUE) then leave sin(r1) else if flag is 0 (FALSE) then leave cos(r1)

    : FCOSI                                 \ r2=sin(r1) if flag=-1 else r2=cos(r1) if flag=0
      >R                                    \ save flag
      PI/2 F/                               \ map r1 to x in [-pi/4,pi/4]
      2DUP .5E0 F+ F>D                      \ floor(2x/pi+.5)
      \ save (floor(2x/pi+.5)+flag+1) mod 4 = quadrant 0,1,2,3 where flag is -1 (sin) or 0 (cos)
      OVER R> + 1+ >R
      D>F F- PI/2 F*                        \ pi/2 * (2x/pi - floor(2x/pi + .5))
      2DUP 2DUP F* FNEGATE 2SWAP            \ -- -x*x x
      \ quadrant 0:  sin(x) =  x - x^3/3! + x^5/5! - x^7/7! + ...
      \ quadrant 1:  cos(x) =  1 - x^2/2! + x^4/4! - x^6/6! + ...
      \ quadrant 2: -sin(x) = -x + x^3/3! - x^5/5! + x^7/7! - ...
      \ quadrant 3: -cos(x) = -1 + x^2/2! - x^4/4! + x^6/6! - ...
      R@ 1 AND IF 2DROP 1E0 THEN            \ initial term 1 for quadrant 1 and 3
      R@ 2 AND IF FNEGATE THEN              \ negate initial term for quadrant 2 and 4
      2SWAP                                 \ -- x|1|-x|-1 -x*x
      \ Maclaurin series iterations i=2,4,6,8,10,12 (cos) or i=1,3,5,7,9,11 (sin)
      13 2 R> 1 AND - DO                    \ 6 iterations
        2OVER 2OVER F*                      \ -- ... term -x*x -x*x*term
        I DUP 1+ *                          \ -- ... term -x*x -x*x*term i*(i+1)
        S>F F/                              \ -- ... term -x*x -x*x*term/(i*(i+1))
        2SWAP                               \ -- ... term -x*x*term/(i*(i+1)) -x*x
      2 +LOOP
      2DROP
      F+ F+ F+ F+ F+ F+ ;                   \ sum the 7 terms in reverse order for accuracy

___
### FSIN
_r1 -- r2_

sine of float in radian

    : FSIN TRUE FCOSI ;

___
### FCOS
_r1 -- r2_

cosine of float in radian

    : FCOS FALSE FCOSI ;

___
### FTAN
_r1 -- r2_

tangent of float in radian

    : FTAN 2DUP FSIN 2SWAP FCOS F/ ;

___
### FASIN
_r1 -- r2_

arc sine of float, in radian

    : FASIN
      2DUP F0= IF EXIT THEN
      2DUP FABS 1E0 F= IF                           \ if |x|=1 then
        PI/2 2SWAP F0< IF FNEGATE THEN              \ sign(x)*pi/2
___
### ;
_ EXIT_

      THEN
      2DUP 2DUP F* 1E0 2SWAP F- FSQRT FATAN2 ;      \ arctan(x/sqrt(1-x^2)) = atan2(x,sqrt(1-x*x))

___
### FACOS
_r1 -- r2_

arc cosine of float, in radian

    : FACOS FASIN PI/2 2SWAP F- ;

___
### FATAN
_r1 -- r2_

arc tangent of float, in radian

    : FATAN
      \ map r1 to [-1,1] using arctan(x) = sign(x) * (pi/2-arctan(1/abs(x)))
      1E0 2OVER FABS F< IF                  \ if |r1| > 1 then
        2DUP F0< -ROT
        1E0 2SWAP FABS F/
        TRUE
      ELSE
        FALSE
      THEN
      -ROT
      \ map r1 in [-1,1] to [-sqrt(2)+1,sqrt(2)-1] using arctan(x) = 2*arctan(x/(1+sqrt(1+x^2)))
      .41423562E0 2OVER FABS F< IF          \ if |r1| > sqrt(2)-1 then
        2DUP 2DUP F* 1E0 F+ FSQRT 1E0 F+ F/
        TRUE
      ELSE
        FALSE
      THEN
      -ROT
      \ Maclaurin series arctan(x) = x - x^3/3 + x^5/5 - x^7/7 + ... with x in (-1,1)
      2DUP 2DUP 2DUP F* FNEGATE 2SWAP       \ -- x -x*x x
      16 3 DO                               \ 7 iterations
        2OVER F*                            \ -- x -x^3/3 ... -x*x -x*x*term
        2DUP I S>F F/                       \ -- x -x^3/3 ... -x*x -x*x*term -x*x*term/i
        2ROT 2ROT                           \ -- x -x^3/3 ... -x*x*term/i -x*x -x*x*term
      2 +LOOP
      2DROP 2DROP                           \ -- x -x^3/3 ... x^15/15
      F+ F+ F+ F+ F+ F+ F+                  \ sum the 8 terms in reverse order for accuracy
      ROT IF 2E0 F* THEN                    \ 2*arctan(x/(1+sqrt(1+x^2)))
      ROT IF PI/2 2SWAP F-
        ROT IF FNEGATE THEN
      THEN ;                                \ sign(x) * (pi/2-arctan(1/abs(x)))

___
### FATAN2
_r1 r2 -- r3_

atan2(r1,r2) = atan(r1/r2) but using a more accurate formulation

    : FATAN2
      2DUP FNEGATE F0< IF
        F/ FATAN
        EXIT
      THEN
      2SWAP
      2DUP F0= IF
        2DROP F0< IF PI ELSE PI/2 THEN
        EXIT
      THEN
      PI/2 2OVER F0< IF FNEGATE THEN
      2ROT 2ROT F/ FATAN F- ;

___
### FLN
_r1 -- r2_

natural log of float

    : FLN
      2DUP 2DUP F0< -ROT F0= OR IF -46 THROW THEN
      \ map r1 to [0.5,1) using ln(x*2^n) = ln(x) + ln(2^n) = ln(x) + n*ln(2)
      DUP 7 RSHIFT $7e - -ROT               \ 2^(n+1) = 2^(exponent - bias + 1)
      $7f AND $3f00 +                       \ remove exponent 2^(n+1)
      1E0 2SWAP F-                          \ 1-x
      \ Maclaurin series -ln(1-x) = x + x^2/2 + x^3/3 + ... with x in (0,0.5]
      2DUP 2DUP                             \ -- x x x
      22 2 DO                               \ 20 iterations
        2OVER F*                            \ -- x x^2/2 ... x term*x
        2DUP I S>F F/                       \ -- x x^2/2 ... x term*x term*x/i
        2ROT 2ROT                           \ -- x x^2/2 ... term*x/i x term*x
      LOOP
      2DROP 2DROP                           \ -- x x^2/2 ... x^19/19
      20 0 DO F+ LOOP                       \ sum the 21 terms in reverse order
      FNEGATE
      ROT S>F .69314724E0 F* F+ ;           \ + n*ln(2) with approx ln(2) such that 1E0 FLN = 0

___
### FEXP
_r1 -- r2_

natural exponent of float

    : FEXP
      2DUP F0< -ROT
      FABS
      \ map |r1| to [0,ln(2)) using exp(x+k*ln(2)) = exp(x)*2^k
      2DUP .69314724E0 F/ F>S               \ ln(2) = .69314724E0
      DUP $7f > IF -43 THROW THEN           \ multiply by 2^k will overflow
      DUP>R
      S>F .69314724E0 F* F-                 \ ln(2) = .69314724E0
      \ Maclaurin series expm1(x) = exp(x) - 1 = x + x^2/2! + x^3/3! + ...
      2DUP                                  \ -- x x
      10 2 DO                               \ 8 iterations
        2OVER 2OVER F*                      \ -- x x^2/2! ... term x term*x
        I S>F F/                            \ -- x x^2/2! ... term x term*x/i
        2SWAP                               \ -- x x^2/2! ... term term*x/i x
      LOOP
      2DROP                                 \ -- x x^2/2! ... x^9/9!
      F+ F+ F+ F+ F+ F+ F+ F+               \ sum the 9 terms in reverse order
      1E0 F+                                \ exp(x) = expm1(x) + 1
      R> 7 LSHIFT +                         \ multiply exp(x) by 2^k
      ROT IF 1E0 2SWAP F/ THEN ;            \ return reciprocal for negative r1

___
### FLOG
_r1 -- r2_

base 10 log of float

    : FLOG FLN 0.4342945E0 F* ;    \ = ln(x)/ln(10) approx ln(10) such that 10E0 FLOG = 1E0

___
### FALOG
_r1 -- r2_

base 10 exponent of float

    : FALOG 2.3025853E0 F* FEXP ;    \ = exp(x*ln(10))

___
### F^
_r1 r2 -- r3_

raise r1 to r2 using exp(ln(r1)*r2) where r1 > 0

    : F^ 2SWAP FLN F* FEXP ;

___
### F**
_r1 r2 -- r3_

raise r1 to r2

    : F**
      2DUP F0= IF                           \ r2 = 0
        2OVER F0= IF -46 THROW THEN         \ error if r1 = 0 and r2 = 0
        2DROP 2DROP 1E0 EXIT                \ return 1.0E0
      THEN
      2OVER F0= IF                          \ r1 = 0
        2DUP F0< IF -46 THROW THEN          \ error if r1 = 0 and r2 < 0
        2DROP 2DROP 0E0 EXIT                \ return 0.0E0
      THEN
      \ exponentiation r1^n by repeated squaring when n is a small integer |n|<=16
      2DUP 2DUP FTRUNC F= IF                \ r2 has no fractional part
        2DUP ['] F>D CATCH 0= IF            \ r2 is convertable to a double n
          2DUP DABS 17. DU< IF              \ |n| <= 16
            DROP                            \ drop high order of n
            DUP 0< >R                       \ save sign of n
            ABS >R                          \ save |n|
            2DROP                           \ drop old r2
            1E0                             \ -- r1 1.0
            BEGIN
              R@ 1 AND IF 2OVER F* THEN
              R> 1 RSHIFT                   \ -- r1^n product u>>1
            DUP WHILE
              >R
              2SWAP 2DUP F* 2SWAP           \ -- r1^n^2 product u>>1
            REPEAT
            DROP 2SWAP 2DROP                \ -- product
            R> IF 1E0 2SWAP F/ THEN         \ reciprocal when exponent was negative
            EXIT
          THEN
          OVER 1 AND IF                     \ n is odd
            2OVER F0< IF                    \ r1 is negative
              2DROP 2SWAP FABS 2SWAP F^ FNEGATE
              EXIT                          \ return -(|r1|^n)
            THEN
          THEN
          2DROP 2SWAP FABS 2SWAP            \ we want to return |r1|^r2
        ELSE
          2DROP                             \ drop copy of r2
        THEN
      THEN
      F^ ;

___
### FSINH
_r1 -- r2_

sine hyperbolicus of float

    : FSINH FEXP 2DUP 1E0 2SWAP F/ F- .5E0 F* ;

___
### FCOSH
_r1 -- r2_

cosine hyperbolicus of float

    : FCOSH FEXP 2DUP 1E0 2SWAP F/ F+ .5E0 F* ;

___
### FTANH
_r1 -- r2_

tangent hyperbolicus of float

    : FTANH 2DUP F+ FEXP 2DUP 1E0 F- 2SWAP 1E0 F+ F/ ;

___
### FASINH
_r1 -- r2_

arc sine hyperbolicus of float

    : FASINH 2DUP 2DUP F* 1E0 F+ FSQRT F+ FLN ;

___
### FACOSH
_r1 -- r2_

arc cosine hyperbolicus of float

    : FACOSH 2DUP 2DUP F* 1E0 F- FSQRT F+ FLN ;

___
### FATANH
_r1 -- r2_

arc tangent hyperbolicus of float

    : FATANH 2DUP 1E0 F+ 2SWAP 1E0 2SWAP F- F/ FLN .5E0 F* ;



Alphabetic list of words
------------------------

word | stack
---- | -----
[`!`](#!)	|		x addr --
[`#>`](##>)	|		ud -- c-addr u
[`#IB`](##IB)	|		-- u
[`#IN`](##IN)	|		-- n
[`#S`](##S)	|		ud -- 0 0
[`#`](##)	|		ud1 -- ud2
[`'`](#')	|		"&lt;spaces&gt;name&lt;space&gt;" -- xt
[`*/MOD`](#*/MOD)	|		n1 n2 n3 -- n4 n5
[`*/`](#*/)	|		n1 n2 n3 -- n4
[`*`](#*)	|		n1|u1 n2|u2 -- n3|u3
[`+!`](#+!)	|		n addr --
[`+LOOP`](#+LOOP)	|		n|u -- ; C: addr do_sys --
[`+TO`](#+TO)	|		"&lt;spaces&gt;name&lt;space&gt;" -- ; n --
[`+`](#+)	|		n1 n2 -- n3
[`,`](#,)	|		x --
[`-1`](#-1)	|		-- -1
[`-ROT`](#-ROT)	|		x1 x2 x3 -- x3 x1 x2
[`-TRAILING`](#-TRAILING)	|	c-addr u1 -- c-addr u2
[`-TRIM`](#-TRIM)	|		c-addr u1 char -- c-addr u2
[`-`](#-)	|		n1 n2 -- n3
[`."`](#.")	|		"ccc&lt;quote&gt;" -- ; --
[`.(`](#.()	|		"ccc&lt;paren&gt;" --
[`.R`](#.R)	|		n +n --
[`.S`](#.S)	|		--
[`.`](#.)	|		n --
[`/MOD`](#/MOD)	|		n1 n2 -- n3 n4
[`/STRING`](#/STRING)	|	c-addr1 u1 n -- c-addr2 u2
[`/`](#/)	|		n1 n2 -- n3
[`0<`](#0<)	|		n -- flag
[`0=`](#0=)	|		x -- flag
[`0`](#0)	|		-- 0
[`1+`](#1+)	|		n1 -- n2
[`1-`](#1-)	|		n1 -- n2
[`1`](#1)	|		-- 1
[`2!`](#2!)	|		x1 x2 addr --
[`2*`](#2*)	|		n1 -- n2
[`2+`](#2+)	|		n1 -- n2
[`2,`](#2,)	|		x1 x2 --
[`2-`](#2-)	|		n1 -- n2
[`2/`](#2/)	|		n1 -- n2
[`2>R`](#2>R)	|		x1 x2 -- ; R: -- x1 x2
[`2@`](#2@)	|		addr -- x1 x2
[`2CONSTANT`](#2CONSTANT)	|	x1 x2 "&lt;spaces&gt;name&lt;space&gt;" -- ; -- x1 x2
[`2DROP`](#2DROP)	|		xd1 xd2 -- xd1
[`2DUP`](#2DUP)	|		xd -- xd xd
[`2LITERAL`](#2LITERAL)	|	x1 x2 -- ; -- x1 x2
[`2OVER`](#2OVER)	|		xd1 xd2 -- xd1 xd2 xd1
[`2R>`](#2R>)	|		R: x1 x2 -- ; -- x1 x2
[`2R@`](#2R@)	|		R: x1 x2 -- x1 x2 ; -- x1 x2
[`2ROT`](#2ROT)	|		xd1 xd2 xd3 -- xd2 xd3 xd1
[`2SWAP`](#2SWAP)	|		xd1 xd2 -- xd2 xd1
[`2VALUE`](#2VALUE)	|	dx "&lt;spaces&gt;name&lt;space&gt;" -- ; -- dx
[`2VARIABLE`](#2VARIABLE)	|	"&lt;spaces&gt;name&lt;space&gt;" -- ; -- addr
[`2`](#2)	|		-- 2
[`3`](#3)	|		-- 3
[`:CFA`](#:CFA)	|		-- addr colon_sys
[`:NONAME`](#:NONAME)	|	-- xt
[`:`](#:)	|		-- ; C: "&lt;spaces&gt;name&lt;space&gt;" -- addr colon_sys
[`;`](#;)	|		-- ; C: addr colon_sys --
[`<#`](#<#)	|		--
[`<>`](#<>)	|		x1 x2 -- flag
[`<`](#<)	|		n1 n2 -- flag
[`=`](#=)	|		x1 x2 -- flag
[`>BODY`](#>BODY)	|		xt -- addr
[`>DIGIT`](#>DIGIT)	|	char -- n
[`>DOUBLE`](#>DOUBLE)	|	c-addr u -- d true | false
[`>FLOAT`](#>FLOAT)	|	c-addr u -- r true | false
[`>IN`](#>IN)	|		-- addr
[`>NAME`](#>NAME)	|		xt -- nt
[`>NUMBER`](#>NUMBER)	|	ud1 c-addr1 u1 -- ud2 c-addr2 u2
[`>R`](#>R)	|		x -- ; R: -- x
[`>`](#>)	|		n1 n2 -- flag
[`?COMP`](#?COMP)	|		--
[`?DO`](#?DO)	|		n1|u1 n2|u2 -- ; C: -- addr do_sys
[`?DUP`](#?DUP)	|		x -- x x or 0 -- 0
[`?LEAVE`](#?LEAVE)	|	x --
[`?RP`](#?RP)	|		--
[`?SYS`](#?SYS)	|		-- ; C: x --
[`?`](#?)	|		addr --
[`@`](#@)	|		addr -- x
[`ABORT"`](#ABORT")	|	... flag -- ; C: "ccc&lt;quote&gt;" -- ; R: ... --
[`ABORT`](#ABORT)	|		... -- ; R: ... --
[`ABS`](#ABS)	|		n1 -- n2
[`ACCEPT`](#ACCEPT)	|	c-addr +n1 -- +n2
[`ACTION-OF`](#ACTION-OF)	|	"&lt;spaces&gt;name&lt;space&gt;" -- xt
[`AGAIN`](#AGAIN)	|		-- ; C: addr dest --
[`AHEAD`](#AHEAD)	|		-- ; C: -- addr orig
[`ALLOT`](#ALLOT)	|		n --
[`AND`](#AND)	|		x1 x2 -- x1&x2
[`AT-XY`](#AT-XY)	|		u1 u2 --
[`BASE`](#BASE)	|		-- addr
[`BEGIN`](#BEGIN)	|		-- ; C: -- addr dest
[`BLANK`](#BLANK)	|		c-addr u --
[`BL`](#BL)	|		-- 32
[`BUFFER:`](#BUFFER:)	|	n "&lt;spaces&gt;name&lt;space&gt;" -- ; -- addr
[`BYE`](#BYE)	|		--
[`C!`](#C!)	|		char c-addr --
[`C"`](#C")	|		"ccc&lt;quote&gt;" -- ; -- c-addr
[`C,`](#C,)	|		char --
[`C@`](#C@)	|		c-addr -- char
[`CASE`](#CASE)	|		x -- ; C: -- 0
[`CATCH`](#CATCH)	|		... xt -- ... 0 or xt -- n
[`CELL+`](#CELL+)	|		addr -- addr
[`CELLS`](#CELLS)	|		n1 -- n2
[`CFA,`](#CFA,)	|		addr --
[`CFA=`](#CFA=)	|		xt1 xt2 -- flag
[`CHAR+`](#CHAR+)	|		n1 -- n1
[`CHARS`](#CHARS)	|		n1 -- n2
[`CHAR`](#CHAR)	|		"&lt;spaces&gt;name&lt;space&gt;" -- char
[`CHECK-NAME`](#CHECK-NAME)	|	c-addr u -- c-addr u
[`CHOP`](#CHOP)	|		c-addr u1 char -- c-addr u2
[`CLEAR`](#CLEAR)	|		... --
[`CMOVE>`](#CMOVE>)	|	c-addr1 c-addr2 u --
[`CMOVE`](#CMOVE)	|		c-addr1 c-addr2 u --
[`CODE`](#CODE)	|		"&lt;spaces&gt;name&lt;space&gt;" --
[`COMPARE`](#COMPARE)	|	c-addr1 u1 c-addr2 u2 -- -1|0|1
[`COMPILE,`](#COMPILE,)	|	xt --
[`CONSTANT`](#CONSTANT)	|	x "&lt;spaces&gt;name&lt;space&gt;" -- ; -- x
[`CONTEXT`](#CONTEXT)	|	-- addr
[`COUNT`](#COUNT)	|		c-addr1 -- c-addr2 u
[`CREATE`](#CREATE)	|	"&lt;spaces&gt;name&lt;space&gt;" -- ; -- addr
[`CR`](#CR)	|		--
[`CUR-XY`](#CUR-XY)	|	-- u1 u2
[`CURRENT`](#CURRENT)	|	-- addr
[`D*`](#D*)	|		d1|ud1 d2|ud2 -- d3|ud3
[`D+!`](#D+!)	|		d addr --
[`D+`](#D+)	|		d1 d2 -- d3
[`D-`](#D-)	|		d1 d2 -- d3
[`D.R`](#D.R)	|		d +n --
[`D.`](#D.)	|		d --
[`D/MOD`](#D/MOD)	|	d1 d2 -- d3 d4
[`D/`](#D/)	|		d1 d2 -- d3
[`D0<`](#D0<)	|		d -- flag
[`D0=`](#D0=)	|		dx -- flag
[`D2*`](#D2*)	|		d1 -- d2
[`D2/`](#D2/)	|		d1 -- d2
[`D<`](#D<)	|		d1 d2 -- flag
[`D=`](#D=)	|		d1 d2 -- flag
[`D>F`](#D>F)	|		d -- r
[`D>S`](#D>S)	|		d -- n
[`DABS`](#DABS)	|		d1 -- d2
[`DBL`](#DBL)	|		-- flag
[`DECIMAL`](#DECIMAL)	|	--
[`DEFER!`](#DEFER!)	|	xt1 xt2 --
[`DEFER@`](#DEFER@)	|	xt1 -- xt2
[`DEFER`](#DEFER)	|		"&lt;spaces&gt;name&lt;space&gt;" -- ; ... -- ...
[`DEFINITIONS`](#DEFINITIONS)	|	--
[`DEPTH`](#DEPTH)	|		-- u
[`DMAX`](#DMAX)	|		d1 d2 -- d3
[`DMIN`](#DMIN)	|		d1 d2 -- d3
[`DMOD`](#DMOD)	|		d1 d2 -- d3
[`DNEGATE`](#DNEGATE)	|	d1 -- d2
[`DOES>`](#DOES>)	|		-- ; ... -- ...
[`DO`](#DO)	|		n1|u1 n2|u2 -- ; C: -- addr do_sys
[`DROP`](#DROP)	|		x --
[`DU<`](#DU<)	|		du1 du2 -- flag
[`DUP>R`](#DUP>R)	|		x -- x ; R: -- x
[`DUP`](#DUP)	|		x -- x x
[`EDIT`](#EDIT)	|		c-addr +n1 n2 n3 -- c-addr +n4
[`ELSE`](#ELSE)	|		-- ; C: addr orig -- addr orig
[`EMIT`](#EMIT)	|		char --
[`ENDCASE`](#ENDCASE)	|	x -- ; C: n*orig n --
[`ENDOF`](#ENDOF)	|		-- ; C: n -- orig n
[`ERASE`](#ERASE)	|		c-addr u --
[`ERROR`](#ERROR)	|		n --
[`EVALUATE`](#EVALUATE)	|	... c-addr u -- ...
[`EXECUTE`](#EXECUTE)	|	... xt -- ...
[`EXIT`](#EXIT)	|		--
[`F**`](#F**)	|		r1 r2 -- r3
[`F*`](#F*)	|		r1 r2 -- r3
[`F+`](#F+)	|		r1 r2 -- r3
[`F-`](#F-)	|		r1 r2 -- r3
[`F.`](#F.)	|		r --
[`F/`](#F/)	|		r1 r2 -- r3
[`F0<`](#F0<)	|		r -- flag
[`F0=`](#F0=)	|		r -- flag
[`F<`](#F<)	|		r1 r2 -- flag
[`F=`](#F=)	|		r1 r2 -- flag
[`F>D`](#F>D)	|		r -- d
[`F>S`](#F>S)	|		r -- n
[`FABS`](#FABS)	|		r1 -- r2
[`FACOSH`](#FACOSH)	|	r1 -- r2
[`FACOS`](#FACOS)	|	r1 -- r2
[`FALOG`](#FALOG)	|	r1 -- r2
[`FALSE`](#FALSE)	|		-- 0
[`FASINH`](#FASINH)	|	r1 -- r2
[`FASIN`](#FASIN)	|	r1 -- r2
[`FATAN2`](#FATAN2)	|	r1 r2 -- r3
[`FATANH`](#FATANH)	|	r1 -- r2
[`FATAN`](#FATAN)	|	r1 -- r2
[`FCOSH`](#FCOSH)	|	r1 -- r2
[`FCOS`](#FCOS)	|		r1 -- r2
[`FENCE`](#FENCE)	|		-- addr
[`FEXP`](#FEXP)	|		r1 -- r2
[`FILL`](#FILL)	|		c-addr u char --
[`FIND-WORD`](#FIND-WORD)	|	c-addr u -- c-addr 0 | xt 1 | xt -1
[`FIND`](#FIND)	|		c-addr -- c-addr 0 | xt 1 | xt -1
[`FLN`](#FLN)	|		r1 -- r2
[`FLOG`](#FLOG)	|		r1 -- r2
[`FLOOR`](#FLOOR)	|	r1 -- r2
[`FM/MOD`](#FM/MOD)	|	d1 n1 -- n2 n3
[`FMAX`](#FMAX)	|		r1 r2 -- r3
[`FMIN`](#FMIN)	|		r1 r2 -- r3
[`FNEGATE`](#FNEGATE)	|	r1 -- r2
[`FORGET`](#FORGET)	|	"&lt;spaces&gt;name&lt;space&gt;" --
[`FORTH`](#FORTH)	|		--
[`FRAND`](#FRAND)	|	r1 -- r2
[`FS.`](#FS.)	|		r --
[`FSINH`](#FSINH)	|	r1 -- r2
[`FSIN`](#FSIN)	|		r1 -- r2
[`FSQRT`](#FSQRT)	|	r1 -- r2
[`FTANH`](#FTANH)	|	r1 -- r2
[`FTAN`](#FTAN)	|		r1 -- r2
[`FTRUNC`](#FTRUNC)	|	r1 -- r2
[`HANDLER`](#HANDLER)	|	-- addr
[`HERE`](#HERE)	|		-- addr
[`HEX`](#HEX)	|		--
[`HIDE`](#HIDE)	|		--
[`HOLDS`](#HOLDS)	|		c-addr u --
[`HOLD`](#HOLD)	|		char --
[`HP`](#HP)	|		-- addr
[`IF`](#IF)	|		x -- ; C: -- addr orig
[`IMMEDIATE`](#IMMEDIATE)	|	--
[`INKEY`](#INKEY)	|	-- x
[`INTERPRET`](#INTERPRET)	|	--
[`INVERT`](#INVERT)	|	x1 -- x2
[`IS`](#IS)	|		xt "&lt;spaces&gt;name&lt;space&gt;" --
[`I`](#I)	|		-- n
[`J`](#J)	|		-- n
[`KEY-CLEAR`](#KEY-CLEAR)	|	--
[`KEY?`](#KEY?)	|		-- flag
[`KEY`](#KEY)	|		-- char
[`K`](#K)	|		-- n
[`L>NAME`](#L>NAME)	|	lfa -- nt
[`LASTXT`](#LASTXT)	|	-- xt
[`LEAVE`](#LEAVE)	|		--
[`LITERAL`](#LITERAL)	|	x -- ; -- x
[`LOOP`](#LOOP)	|		-- ; C: addr do_sys --
[`LSHIFT`](#LSHIFT)	|	x1 u -- x2
[`M*/`](#M*/)	|		d1 n1 n2 -- d2
[`M*`](#M*)	|		n1 n2 -- d
[`M+`](#M+)	|		d1 n -- d2
[`MARKER`](#MARKER)	|	"&lt;spaces&gt;name&lt;space&gt;" -- ; --
[`MAX-XY`](#MAX-XY)	|	-- u1 u2
[`MAX`](#MAX)	|		n1 n2 -- n3
[`MD*`](#MD*)	|		d1 n -- d2
[`MIN`](#MIN)	|		n1 n2 -- n3
[`MOD`](#MOD)	|		n1 n2 -- n3
[`MOVE`](#MOVE)	|		c-addr1 c-addr2 u --
[`N>R`](#N>R)	|		n*x n -- ; R: -- n*x n
[`NAME>STRING`](#NAME>STRING)	|	nt -- c-addr u
[`NAME>`](#NAME>)	|		nt -- xt
[`NEGATE`](#NEGATE)	|	n1 -- n2
[`NEXT-CHAR`](#NEXT-CHAR)	|	c-addr1 u1 -- c-addr2 u2 char
[`NFA,`](#NFA,)	|		c-addr u --
[`NIP`](#NIP)	|		x1 x2 -- x2
[`NR>`](#NR>)	|		R: n*x n -- ; -- n*x n
[`NUMBER`](#NUMBER)	|	c-addr u -- n|u|d|ud|r
[`OFF`](#OFF)	|		addr --
[`OF`](#OF)	|		x1 x2 -- x1 or x1 x2 -- ; C: n1 -- orig n2
[`OK`](#OK)	|		"ccc&lt;eol&gt;" --
[`ON`](#ON)	|		addr --
[`OR`](#OR)	|		x1 x2 -- x1|x2
[`OUTPUT-ID`](#OUTPUT-ID)	|	-- 0|fileid
[`OVER`](#OVER)	|		x1 x2 -- x1 x2 x1
[`PAD`](#PAD)	|		-- c-addr
[`PAGE`](#PAGE)	|		--
[`PARSE-NAME`](#PARSE-NAME)	|	"&lt;spaces&gt;name&lt;space&gt;" -- c-addr u
[`PARSE-WORD`](#PARSE-WORD)	|	char "&lt;chars&gt;ccc&lt;char&gt;" -- c-addr u
[`PARSE`](#PARSE)	|		char "ccc&lt;char&gt;" -- c-addr u
[`PI/2`](#PI/2)	|		-- r
[`PICK`](#PICK)	|		xu ... x0 u -- xu ... x0 xu
[`PI`](#PI)	|		-- r
[`POSTPONE`](#POSTPONE)	|	"&lt;spaces&gt;name&lt;space&gt;" --
[`PRECISION`](#PRECISION)	|	-- +n
[`QUIT`](#QUIT)	|		... -- ; R: ... --
[`R>`](#R>)	|		R: x -- ; -- x
[`R@`](#R@)	|		R: x -- x ; -- x
[`RDROP`](#RDROP)	|		R: x -- ; --
[`RECURSE`](#RECURSE)	|	... -- ...
[`REFILL`](#REFILL)	|	-- flag
[`REPEAT`](#REPEAT)	|	-- ; C: addr orig addr dest --
[`REPL`](#REPL)	|		--
[`REPRESENT`](#REPRESENT)	|	r c-addr u -- n flag true
[`RESTORE-INPUT`](#RESTORE-INPUT)	|	... n -- flag
[`REVEAL`](#REVEAL)	|	--
[`ROLL`](#ROLL)	|		xu x(u+1) ... x1 x0 u -- x(u+1) ... x1 x0 xu
[`ROT`](#ROT)	|		x1 x2 x3 -- x2 x3 x1
[`RP!`](#RP!)	|		addr --
[`RP@`](#RP@)	|		-- addr
[`RSHIFT`](#RSHIFT)	|	x1 u -- x2
[`S"`](#S")	|		"ccc&lt;quote&gt;" -- ; -- c-addr u
[`S>D`](#S>D)	|		n -- d
[`S>F`](#S>F)	|		n -- r
[`SAVE-INPUT`](#SAVE-INPUT)	|	-- ... n
[`SDUP`](#SDUP)	|		c-addr1 u -- c-addr2 u
[`SEARCH`](#SEARCH)	|	c-addr1 u1 c-addr2 u2 -- c-addr3 u3 flag
[`SIGN`](#SIGN)	|		n --
[`SKIP`](#SKIP)	|		char "&lt;chars&gt;" --
[`SLITERAL`](#SLITERAL)	|	c-addr u -- ; -- c-addr u
[`SM/REM`](#SM/REM)	|	d1 n1 -- n2 n3
[`SOURCE-ID`](#SOURCE-ID)	|	-- 0|-1|fileid
[`SOURCE`](#SOURCE)	|	-- c-addr u
[`SP!`](#SP!)	|		addr --
[`SP@`](#SP@)	|		-- addr
[`SPACES`](#SPACES)	|	n --
[`SPACE`](#SPACE)	|		--
[`STATE`](#STATE)	|		-- addr
[`SWAP`](#SWAP)	|		x1 x2 -- x2 x1
[`S\"`](#S\")	|		"ccc&lt;quote&gt;" -- ; -- c-addr u
[`S\>S`](#S\>S)	|		c-addr u -- c-addr u
[`THEN`](#THEN)	|		-- ; C: addr orig --
[`THROW`](#THROW)	|		0 -- or ... n -- ... n
[`TIB`](#TIB)	|		-- c-addr
[`TMP`](#TMP)	|		-- c-addr
[`TO`](#TO)	|		"&lt;spaces&gt;name&lt;space&gt;" -- ; x --
[`TRIM`](#TRIM)	|		c-addr1 u1 char -- c-addr2 u2
[`TRUE`](#TRUE)	|		-- -1
[`TUCK`](#TUCK)	|		x1 x2 -- x2 x1 x2
[`TYPE`](#TYPE)	|		c-addr u --
[`U.R`](#U.R)	|		u +n --
[`U.`](#U.)	|		u --
[`U<`](#U<)	|		u1 u2 -- flag
[`U>`](#U>)	|		u1 u2 -- flag
[`UD/MOD`](#UD/MOD)	|	ud1 ud2 -- ud3 ud4
[`UM*/MOD`](#UM*/MOD)	|	ud1 u1 u2 -- u3 ud2
[`UM*`](#UM*)	|		u1 u2 -- ud
[`UM/MOD`](#UM/MOD)	|	ud u1 -- u2 u3
[`UMAX`](#UMAX)	|		u1 u2 -- u3
[`UMD*`](#UMD*)	|		ud1 u -- ud2
[`UMD/MOD`](#UMD/MOD)	|	ud1 u1 -- u2 ud2
[`UMIN`](#UMIN)	|		u1 u2 -- u3
[`UNLOOP`](#UNLOOP)	|	--
[`UNTIL`](#UNTIL)	|		x -- ; C: addr dest --
[`UNUSED`](#UNUSED)	|	-- u
[`VALUE`](#VALUE)	|		x "&lt;spaces&gt;name&lt;space&gt;" -- ; -- x
[`VARIABLE`](#VARIABLE)	|	"&lt;spaces&gt;name&lt;space&gt;" -- ; -- addr
[`VOCABULARY`](#VOCABULARY)	|	"&lt;spaces&gt;name&lt;space&gt;" --
[`WHILE`](#WHILE)	|		x -- ; C: addr sys -- addr orig addr sys
[`WIDTH`](#WIDTH)	|		u --
[`WITHIN`](#WITHIN)	|	x1 x2 x3 -- flag
[`WORDS`](#WORDS)	|		--
[`WORD`](#WORD)	|		char "&lt;chars&gt;ccc&lt;char&gt;" -- c-addr
[`XOR`](#XOR)	|		x1 x2 -- x1^x2
[`[']`](#['])	|		"&lt;spaces&gt;name&lt;space&gt;" -- ; -- xt
[`[CHAR]`](#[CHAR])	|	"&lt;spaces&gt;char" -- ; -- char
[`[COMPILE]`](#[COMPILE])	|	"&lt;space&gt;name&lt;space&gt;" -- ; ... -- ...
[`[`](#[)	|		--
[`\`](#\)	|		"ccc&lt;eol&gt;" --
[`]`](#])	|		--
