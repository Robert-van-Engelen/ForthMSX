Forth words
-----------

Forth words operate on the parameter stack, the return stack, and may parse
Forth words or delimited sequences of characters from the input buffer.
Parameter stack changes by a word such as `ROT` are indicated by a `--`:

**ROT**
<br>
_x1 x2 x3 -- x2 x3 x1_

On the left side, three single-cell values are on the parameter stack with _x3_
the top-of-stack (TOS) value.  On the right side three single-cell values are
returned in a rotated order with _x1_ the new TOS, _x3_ the second-on-stack
(2OS) and _x2_ the third-on-stack (3OS).

Return stack changes are indicated by an _R_, for example _x -- ; R: -- x_
moves _x_ from the parameter stack to the return stack.

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
### (:)
_-- ; R: -- ip_
<br>
call colon definition;
runtime of the : compile-only word

___
### (;)
_-- ; R: ip --_
<br>
return to caller from colon definition;
runtime of the ; compile-only word

___
### (EXIT)
_-- ; R: ip --_
<br>
return to caller from colon definition;
runtime of the EXIT compile-only word

___
### (;DOES)
_-- ; R: ip --_
<br>
set LASTXT cfa to ip and return from colon definition;
a runtime word compiled by the DOES&gt; compile-only word

___
### (DOES)
_addr -- addr ; R: -- ip_
<br>
calls the DOES&gt; definition with pfa addr;
runtime word compiled by the DOES&gt; compile-only word coded as call dodoes

___
### (VAR)
_-- addr_
<br>
leave parameter field address (pfa) of variable;
runtime word of a VARIABLE coded as call dovar

___
### (VAL)
_-- x_
<br>
fetch value;
runtime word of a VALUE coded as call doval

___
### (2VAL)
_-- dx_
<br>
fetch double value;
runtime word of a 2VALUE coded as call dotwoval

___
### (CON)
_-- x_
<br>
fetch constant;
runtime word of a CONSTANT coded as call docon

___
### (2CON)
_-- x_
<br>
fetch double constant;
runtime word of a 2CONSTANT coded as call dotwocon

___
### (DEF)
_--_
<br>
execute deferred word;
runtime word of a DEFER coded as call dodef

___
### (LIT)
_-- x_
<br>
fetch literal;
runtime word compiled by EVALUATE, INTERPRET and NUMBER

___
### (2LIT)
_-- x1 x2_
<br>
fetch double literal;
runtime word compiled by EVALUATE, INTERPRET and NUMBER

___
### (SLIT)
_-- c-addr u_
<br>
fetch literal string;
runtime word compiled by S" and ."

___
### 0
_-- 0_
<br>
leave constant 0

    0 CONSTANT 0

___
### 1
_-- 1_
<br>
leave constant 1

    1 CONSTANT 1

___
### 2
_-- 2_
<br>
leave constant 2

    2 CONSTANT 2

___
### 3
_-- 3_
<br>
leave constant 3

    3 CONSTANT 3

___
### -1
_-- -1_
<br>
leave constant -1

    -1 CONSTANT -1

___
### FALSE
_-- 0_
<br>
leave 0

    0 CONSTANT FALSE

___
### TRUE
_-- -1_
<br>
leave -1

    -1 CONSTANT TRUE

___
### BL
_-- 32_
<br>
leave constant 32 (space)

    32 CONSTANT BL

___
### PAD
_-- c-addr_
<br>
leave address of the PAD;
the PAD is a free buffer space of at least 256 bytes not used by Forth

___
### TIB
_-- c-addr u_
<br>
leave c-addr of the terminal input buffer (TIB) and buffer size u;
the terminal input buffer used by Forth of at least 256 bytes

___
### TMP
_-- c-addr_
<br>
leave address of the next temp string buffer;
switches between two string buffers of 256 free bytes each;
used by SDUP, WORD and S" to store a string when interpreting

___
### DROP
_x --_
<br>
drop TOS

___
### DUP
_x -- x x_
<br>
duplicate TOS

___
### ?DUP
_x -- x x or 0 -- 0_
<br>
duplicate TOS if nonzero

___
### SWAP
_x1 x2 -- x2 x1_
<br>
swap TOS with 2OS

___
### OVER
_x1 x2 -- x1 x2 x1_
<br>
copy 2OS over TOS

___
### ROT
_x1 x2 x3 -- x2 x3 x1_
<br>
rotate cells

    : ROT &gt;R SWAP R> SWAP ;

___
### -ROT
_x1 x2 x3 -- x3 x1 x2_
<br>
undo (or back, or left) rotate cells, or ROT twice

    : -ROT ROT ROT ;

___
### NIP
_x1 x2 -- x2_
<br>
nip 2OS

    : NIP SWAP DROP ;

___
### TUCK
_x1 x2 -- x2 x1 x2_
<br>
tuck TOS under 2OS

    : TUCK SWAP OVER ;

___
### 2DROP
_xd1 xd2 -- xd1_
<br>
drop double TOS

    : 2DROP DROP DROP ;

___
### 2DUP
_xd -- xd xd_
<br>
duplicate double TOS

    : 2DUP OVER OVER ;

___
### 2SWAP
_xd1 xd2 -- xd2 xd1_
<br>
swap double TOS with double 2OS

    : 2SWAP ROT &gt;R ROT R> ;
    : 2SWAP 3 ROLL 3 ROLL ;

___
### 2OVER
_xd1 xd2 -- xd1 xd2 xd1_
<br>
copy double 2OS over double TOS

    : 2OVER &gt;R >R 2DUP R> R> 2SWAP ;
    : 2OVER 3 PICK 3 PICK ;

___
### 2ROT
_xd1 xd2 xd3 -- xd2 xd3 xd1_
<br>
rotate double cells

    : 2ROT 5 ROLL 5 ROLL ;

___
### PICK
_xu ... x0 u -- xu ... x0 xu_
<br>
pick u'th cell from the parameter stack;
0 PICK is the same as DUP;
1 PICK is the same as OVER

    : PICK 1+ CELLS SP@ + @ ;

___
### ROLL
_xu x(u+1) ... x1 x0 u -- x(u+1) ... x1 x0 xu_
<br>
roll u cells on the parameter stack,
where u &lt; 128 (u is not checked, using u modulo 128 for safety);
0 ROLL does nothing;
1 ROLL is the same as SWAP;
2 ROLL is the same as ROT

___
### DEPTH
_-- u_
<br>
parameter stack depth

    : DEPTH SP0@ SP@ - 2- 2/ ;

___
### CLEAR
_... --_
<br>
purge parameter stack

    : CLEAR SP0@ SP! ;

___
### SP@
_-- addr_
<br>
fetch stack pointer, leave addr of the TOS cell (the TOS before SP@)

___
### SP!
_addr --_
<br>
store stack pointer

___
### .S
_--_
<br>
display parameter stack

    : .S DEPTH 0 ?DO SP0@ I 2+ CELLS - ? LOOP ;

___
### &gt;R
_x -- ; R: -- x_
<br>
move TOS to the return stack

___
### DUP&gt;R
_x -- x ; R: -- x_
<br>
duplicate TOS to the return stack, a single word for DUP &gt;R

___
### R&gt;
_R: x -- ; -- x_
<br>
move cell from the return stack

___
### RDROP
_R: x -- ; --_
<br>
drop cell from the return stack, a single word for R&gt; DROP

___
### R@
_R: x -- x ; -- x_
<br>
fetch cell from the return stack

___
### 2&gt;R
_x1 x2 -- ; R: -- x1 x2_
<br>
move double TOS to the return stack, a single word for SWAP &gt;R >R

___
### 2R&gt;
_R: x1 x2 -- ; -- x1 x2_
<br>
move double cell from the return stack, a single word for R&gt; R> SWAP

___
### 2R@
_R: x1 x2 -- x1 x2 ; -- x1 x2_
<br>
fetch double cell from the return stack, a single word for R&gt; R@ SWAP DUP R>

___
### N&gt;R
_n*x n -- ; R: -- n*x n_
<br>
move n cells to the return stack;
where n &lt; 127 (u is not checked, using n modulo 128 for safety);
no stack under/overflow checking

___
### NR&gt;
_R: n*x n -- ; -- n*x n_
<br>
move n cells from the return stack;
where n &lt; 127 (u is not checked, using n modulo 128 for safety);
no stack under/overflow checking

___
### RP@
_-- addr_
<br>
fetch return stack pointer

___
### RP!
_addr --_
<br>
store return stack pointer

___
### ?RP
_--_
<br>
check return stack pointer for under- and overflow,
available only when assembled with the SAVR assembly flag;
may throw -5 "return stack overflow" or -6 "return stack underflow"

___
### C@
_c-addr -- char_
<br>
fetch char

___
### @
_addr -- x_
<br>
fetch from cell

___
### 2@
_addr -- x1 x2_
<br>
fetch from double cell

    : 2@ DUP CELL+ @ SWAP @ ;

___
### C!
_char c-addr --_
<br>
store char in c-addr

___
### !
_x addr --_
<br>
store in cell

___
### 2!
_x1 x2 addr --_
<br>
store in double cell

    : 2! TUCK ! CELL+ ! ;

___
### (TO)
_x --_
<br>
store in value;
runtime of the TO compile-only word

___
### (2TO)
_dx --_
<br>
store in double value;
runtime of the TO compile-only word

___
### +!
_n addr --_
<br>
increment cell

___
### D+!
_d addr --_
<br>
increment double cell

___
### (+TO)
_n --_
<br>
increment value;
runtime of the +TO compile-only word

___
### (D+TO)
_d --_
<br>
increment double value;
runtime of the +TO compile-only word

___
### ON
_addr --_
<br>
store TRUE (-1) in cell

    : ON -1 SWAP ! ;

___
### OFF
_addr --_
<br>
store FALSE (0) in cell

    : OFF 0 SWAP ! ;

___
### +
_n1 n2 -- n3_
<br>
sum n1+n2

___
### M+
_d1 n -- d2_
<br>
double sum d1+n

___
### D+
_d1 d2 -- d3_
<br>
double sum d1+d2

    : D+ &gt;R M+ R> + ;

___
### -
_n1 n2 -- n3_
<br>
difference n1-n2

___
### D-
_d1 d2 -- d3_
<br>
double difference d1-d2

    : D- DNEGATE D+ ;

___
### UM*
_u1 u2 -- ud_
<br>
unsigned double product u1*u2

___
### M*
_n1 n2 -- d_
<br>
signed double product n1*n2

    : M*
      2DUP XOR &gt;R
      ABS SWAP ABS
      UM*
      R&gt; 0&lt; IF DNEGATE THEN ;

___
### *
_n1|u1 n2|u2 -- n3|u3_
<br>
signed and unsigned product n1*n2

    : * UM* DROP ;

___
### UMD*
_ud1 u -- ud2_
<br>
unsigned double product ud1*u

    : UMD*
      DUP&gt;R
      UM* DROP SWAP
      R&gt; UM* ROT + ;

___
### UM/MOD
_ud u1 -- u2 u3_
<br>
unsigned remainder and quotient ud/u1;
the result is undefined when u1 = 0

___
### SM/REM
_d1 n1 -- n2 n3_
<br>
symmetric remainder and quotient d1/n1 rounded towards zero;
the result is undefined when n1 = 0

    : SM/REM
      2DUP XOR &gt;R
      OVER &gt;R
      ABS -ROT DABS ROT
      UM/MOD
      R&gt; 0&lt; IF SWAP NEGATE SWAP THEN
      R&gt; 0&lt; IF NEGATE THEN ;

___
### FM/MOD
_d1 n1 -- n2 n3_
<br>
floored signed modulus and quotient d1/n1 rounded towards negative (floored);
the result is undefined when n1 = 0

    : FM/MOD
      DUP&gt;R
      SM/REM
      DUP 0&lt; IF
        SWAP R&gt; + SWAP 1-
      ELSE
        RDROP
      THEN ;

___
### /MOD
_n1 n2 -- n3 n4_
<br>
symmetric remainder and quotient n1/n2;
the result is undefined when n2 = 0

    : /MOD SWAP S&gt;D ROT SM/REM ;

___
### MOD
_n1 n2 -- n3_
<br>
symmetric remainder of n1/n2;
the result is undefined when n2 = 0

    : MOD /MOD DROP ;

___
### /
_n1 n2 -- n3_
<br>
quotient n1/n2;
the result is undefined when n2 = 0

    : / /MOD NIP ;

___
### */MOD
_n1 n2 n3 -- n4 n5_
<br>
product with symmetric remainder and quotient n1*n2/n3;
the result is undefined when n3 = 0

    : */MOD -ROT M* ROT SM/REM ;

___
### */
_n1 n2 n3 -- n4_
<br>
product with quotient n1*n2/n3;
the result is undefined when n3 = 0

    : */ */MOD NIP ;

___
### UM*/MOD
_ud1 u1 u2 -- u3 ud2_
<br>
unsigned double product and quotient ud1*u1/u2 with single remainder u3,
with intermediate triple-cell product;
the result is undefined when u2 = 0

    \ assume d = dh.dl     = hi.lo parts
    \ assume t = th.tm.tl  = hi.mid.lo parts
    \ then
    \ dl*n -&gt; tm.tl
    \ dh*n+tm -&gt; th.tm
    \ gives d*n -&gt; t
    : UMT*     ( ud u -- ut )
      DUP&gt;R
      ROT UM*
      ROT 0 SWAP R&gt; UM* D+ ;
    \ assume d = dh.dl     = hi.lo parts
    \ assume t = th.tm.tl  = hi.mid.lo parts
    \ then
    \ (th.tm)/n -&gt; dh
    \ (th.tm)%n -&gt; r
    \ (r.tl)/n -&gt; dl
    \ (r.tl)%n -&gt; r
    \ gives t/n -&gt; d remainder r
    : UMT/MOD  ( ut u1 -- u2 ud )
      DUP&gt;R
      UM/MOD R&gt; SWAP >R
      UM/MOD R&gt; ;
    : UM*/MOD &gt;R UMT* R> UMT/MOD ;

___
### M*/
_d1 n1 n2 -- d2_
<br>
double product with quotient d1*n1/n2,
with intermediate triple-cell product;
the result is undefined when n2 = 0

    : M*/
      2DUP XOR 3 PICK XOR &gt;R
      ABS SWAP ABS SWAP 2SWAP DABS 2SWAP
      UM*/MOD DROP
      &gt;R 0&lt; IF DNEGATE THEN ;

___
### MD*
_d1 n -- d2_
<br>
signed double product d1*n with a single

    : MD*
      2DUP XOR &gt;R
      ABS -ROT DABS ROT
      UMD*
      R&gt; 0&lt; IF DNEGATE THEN ;

___
### D*
_d1|ud1 d2|ud2 -- d3|ud3_
<br>
signed and unsigned double product d1*d2;
the result overflows when d1 and d2 are too large;
use MD* for signed double product d*n with a single;
use UMD* for unsigned double product ud*u with a single;

    : D* &gt;R ROT DUP>R -ROT MD* 2R> * 0 SWAP D+ ;

___
### UMD/MOD
_ud1 u1 -- u2 ud2_
<br>
unsigned remainder and unsigned double quotient ud1/u1;
the result is undefined when u1 = 0;

    : UMD/MOD DUP&gt;R 0 SWAP UM/MOD -ROT R> UM/MOD ROT ;

___
### UD/MOD
_ud1 ud2 -- ud3 ud4_
<br>
unsigned double remainder and quotient ud1/ud2;
the result is undefined when ud2 = 0

___
### D/MOD
_d1 d2 -- d3 d4_
<br>
double symmetric remainder and quotient d1/d2;
the result is undefined when d2 = 0

    : D/MOD
      DUP 3 PICK DUP&gt;R XOR >R
      DABS 2SWAP DABS 2SWAP
      UD/MOD
      R&gt; 0&lt; IF DNEGATE THEN
      R&gt; 0&lt; IF 2SWAP DNEGATE 2SWAP THEN ;

___
### DMOD
_d1 d2 -- d3_
<br>
double symmetric remainder of d1/d2;
the result is undefined when d2 = 0

    : DMOD D/MOD 2DROP ;

___
### D/
_d1 d2 -- d3_
<br>
double quotient d1/d2;
the result is undefined when d2 = 0

    : D/ D/MOD 2SWAP 2DROP ;

___
### AND
_x1 x2 -- x1&x2_
<br>
bitwise and x1 with x2

___
### OR
_x1 x2 -- x1|x2_
<br>
bitwise or x1 with x2

___
### XOR
_x1 x2 -- x1^x2_
<br>
bitwise xor x1 with x2

___
### =
_x1 x2 -- flag_
<br>
true if x1 = x2

___
### &lt;&gt;
_x1 x2 -- flag_
<br>
true if x1 &lt;&gt; x2

___
### &lt;
_n1 n2 -- flag_
<br>
true if n1 &lt; n2 signed

    : &lt;
      2DUP XOR 0&lt; IF
        DROP 0&lt;
        EXIT
      THEN
      - 0&lt; ;

___
### &gt;
_n1 n2 -- flag_
<br>
true if n1 &gt; n2 signed

    : &gt; SWAP &lt; ;

___
### U&lt;
_u1 u2 -- flag_
<br>
true if u1 &lt; u2 unsigned

    : U&lt;
      2DUP XOR 0&lt; IF
        NIP 0&lt;
        EXIT
      THEN
      - 0&lt; ;

___
### U&gt;
_u1 u2 -- flag_
<br>
true if u1 &gt; u2 unsigned

    : U&gt; SWAP U&lt; ;

___
### 0=
_x -- flag_
<br>
true if x = 0;
also serves as a logical NOT

___
### 0&lt;
_n -- flag_
<br>
true if n &lt; 0

___
### D0=
_dx -- flag_
<br>
true if dx = 0

    : D0= OR 0= ;

___
### D0&lt;
_d -- flag_
<br>
true if d &lt; 0

    : D0&lt; NIP 0< ;

___
### S&gt;D
_n -- d_
<br>
widen single to double

___
### D&gt;S
_d -- n_
<br>
narrow double to single;
may throw -11 "result out of range" valid range is -32768 to 65535

___
### D=
_d1 d2 -- flag_
<br>
true if d1 = d2

    : D= D- D0= ;

___
### D&lt;
_d1 d2 -- flag_
<br>
true if d1 &lt; d2

    : D&lt;
      DUP 3 PICK XOR 0&lt; IF
        2DROP D0&lt;
        EXIT
      THEN
      D- D0&lt; ;

___
### DU&lt;
_du1 du2 -- flag_
<br>
true if ud1 &lt; ud2

    : DU&lt;
      DUP 3 PICK XOR 0&lt; IF
        2SWAP 2DROP D0&lt;
        EXIT
      THEN
      D- D0&lt; ;

___
### MAX
_n1 n2 -- n3_
<br>
signed max of n1 and n2

    : MAX
      2DUP &lt; IF SWAP THEN
      DROP ;

___
### MIN
_n1 n2 -- n3_
<br>
signed min of n1 and n2

    : MIN
      2DUP &gt; IF SWAP THEN
      DROP ;

___
### UMAX
_u1 u2 -- u3_
<br>
unsigned max of u1 and u2

    : UMAX
      2DUP U&lt; IF SWAP THEN
      DROP ;

___
### UMIN
_u1 u2 -- u3_
<br>
unsigned min of u1 and u2

    : UMIN
      2DUP U&gt; IF SWAP THEN
      DROP ;

___
### DMAX
_d1 d2 -- d3_
<br>
signed double max of d1 and d2

    : DMAX
      2OVER 2OVER D&lt; IF 2SWAP THEN
      2DROP ;

___
### DMIN
_d1 d2 -- d3_
<br>
signed double min of d1 and d2

    : DMIN
      2OVER 2OVER D&lt; INVERT IF 2SWAP THEN
      2DROP ;

___
### WITHIN
_x1 x2 x3 -- flag_
<br>
true if x1 is within x2 up to x3 exclusive

    : WITHIN OVER - &gt;R - R> U&lt; ;

___
### INVERT
_x1 -- x2_
<br>
one's complement ~x1

    : INVERT 1+ NEGATE ;
    : INVERT -1 XOR ;

___
### NEGATE
_n1 -- n2_
<br>
two's complement -n1

    : NEGATE 0 SWAP - ;
    : NEGATE INVERT 1+ ;

___
### ABS
_n1 -- n2_
<br>
absolute value |n1|

    : ABS DUP 0&lt; IF NEGATE THEN ;

___
### DNEGATE
_d1 -- d2_
<br>
two's complement -d1

    : DNEGATE SWAP INVERT SWAP INVERT 1 M+ ;

___
### DABS
_d1 -- d2_
<br>
absolute value |d1|

    : DABS DUP 0&lt; IF DNEGATE THEN ;

___
### LSHIFT
_x1 u -- x2_
<br>
logical shift left x1 &lt;< u

___
### RSHIFT
_x1 u -- x2_
<br>
logical shift right x1 &gt;> u

___
### 1+
_n1 -- n2_
<br>
increment n1+1

    : 1+ 1 + ;

___
### 2+
_n1 -- n2_
<br>
increment n1+2

    : 2+ 2 + ;

___
### 1-
_n1 -- n2_
<br>
decrement n1-1

    : 1- 1 - ;

___
### 2-
_n1 -- n2_
<br>
decrement n1-2

    : 2- 2 - ;

___
### 2*
_n1 -- n2_
<br>
arithmetic shift left n1 &lt;< 1

    : 2* 2 * ;

___
### 2/
_n1 -- n2_
<br>
arithmetic shift right n1 &gt;> 1

    : 2/ 2 / ;

___
### D2*
_d1 -- d2_
<br>
arithmetic shift left d1 &lt;< 1

    : D2* 2 MD* ;

___
### D2/
_d1 -- d2_
<br>
arithmetic shift right d1 &gt;> 1

    : D2/ 1 2 M*/ ;

___
### CELL+
_addr -- addr_
<br>
increment to next cell

    : CELL+ 2+ ;

___
### CELLS
_n1 -- n2_
<br>
convert to cell unit

    : CELLS 2* ;

___
### CHAR+
_n1 -- n1_
<br>
increment to next char

    : CHAR+ 1+ ;

___
### CHARS
_n1 -- n2_
<br>
convert to char unit (does nothing as chars are bytes)

    : CHARS ;

___
### COUNT
_c-addr1 -- c-addr2 u_
<br>
convert counted string to string

    : COUNT DUP 1+ SWAP C@ ;

___
### COMPARE
_c-addr1 u1 c-addr2 u2 -- -1|0|1_
<br>
compare strings, leaves -1 = less or 0 = equal or 1 = greater

___
### SEARCH
_c-addr1 u1 c-addr2 u2 -- c-addr3 u3 flag_
<br>
true if the second string is in the first;
leaves matching address, remaining length and true;
or leaves the first string and false

___
### CMOVE
_c-addr1 c-addr2 u --_
<br>
move u bytes from c-addr1 to c-addr2 (from begin going up)

    : CMOVE
      SWAP &gt;R
      BEGIN DUP WHILE
        NEXT-CHAR R@ C!
        R&gt; 1+ >R
      REPEAT
      RDROP
      2DROP ;

___
### CMOVE&gt;
_c-addr1 c-addr2 u --_
<br>
move u bytes from c-addr1 to c-addr2 up (from end going down)

___
### MOVE
_c-addr1 c-addr2 u --_
<br>
move u bytes from c-addr1 to c-addr2

    : MOVE
      -ROT
      2DUP U&lt; IF
        ROT CMOVE&gt;
      ELSE
        ROT CMOVE
      THEN ;

___
### FILL
_c-addr u char --_
<br>
fill memory with char

___
### ERASE
_c-addr u --_
<br>
fill memory with zeros

    : ERASE 0 FILL ;

___
### BLANK
_c-addr u --_
<br>
fill memory with 0x20 (BL) chars

    : ERASE BL FILL ;

___
### CHOP
_c-addr u1 char -- c-addr u2_
<br>
truncate a string up to a matching char;
leaves the string if char not found;
char = 0x20 (BL) chops 0x00 to 0x20 (white space and control)

___
### TRIM
_c-addr1 u1 char -- c-addr2 u2_
<br>
trim initial chars from a string;
char = 0x20 (BL) trims 0x00 to 0x20 (white space and control)

___
### -TRIM
_c-addr u1 char -- c-addr u2_
<br>
trim trailing chars from a string;
char = 0x20 (BL) trims 0x00 to 0x20 (white space and control)

___
### -TRAILING
_c-addr u1 -- c-addr u2_
<br>
trim trailing white space and control characters from a string

    : -TRAILING BL -TRIM ;

___
### /STRING
_c-addr1 u1 n -- c-addr2 u2_
<br>
slice n characters off the start of a string;
if n is larger than u1, then u2 underflows,
which should be avoided with OVER UMIN /STRING

    : /STRING ROT OVER + -ROT - ;

___
### NEXT-CHAR
_c-addr1 u1 -- c-addr2 u2 char_
<br>
get next char from a string;
increments the string address and decrements its length by one

    : NEXT-CHAR OVER C@ &gt;R 1- SWAP 1+ SWAP R> ;
    : NEXT-CHAR OVER C@ -ROT 1- SWAP 1+ SWAP ROT ;

___
### WIDTH
_u --_
<br>
set text mode screen and its window width

___
### MAX-XY
_-- u1 u2_
<br>
leave number of screen columns x as u1 and rows y as u2

___
### CUR-XY
_-- u1 u2_
<br>
fetch cursor column x as u1 &gt;= 0 and row y as u2 >= 0

___
### AT-XY
_u1 u2 --_
<br>
set column x to u1 &gt;= 0 and row y to u2 >= 0

    : AT-XY Y! X! ;

___
### OUTPUT-ID
_-- 0|fileid_
<br>
value with 0 = console output, otherwise fileid to redirect output

___
### EMIT
_char --_
<br>
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

ESC code | effect
-------- | -----------------------------------------------------
ESC j    | clear screen and home
ESC E    | clear screen and home
ESC K    | clear to end of line
ESC J    | clear to end of screen
ESC l    | clear line
ESC L    | insert line
ESC M    | delete line
ESC Y    | set cursor coordinates, follow by row column bytes
ESC A    | cursor up, does not scroll
ESC B    | cursor down, does not scroll
ESC C    | cursor right, wraps if the logical line of text wraps
ESC D    | cursor left, does not wrap
ESC H    | cursor home
ESC x    | change cursor, follow by '4' (block) or '5' (disable)
ESC y    | change cursor, follow by '4' (under) or '5' (Enable)

___
### PAGE
_--_
<br>
clear console screen

    : PAGE $C EMIT ;

___
### CR
_--_
<br>
emit carriage return and line feed

    : CR $D EMIT $A EMIT ;

___
### SPACE
_--_
<br>
emit a space

    : SPACE BL EMIT ;

___
### SPACES
_n --_
<br>
emit n spaces (zero or negative n does nothing)

    : SPACES 0 MAX 0 ?DO SPACE LOOP ;

___
### TYPE
_c-addr u --_
<br>
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
<br>
variable with numeric base for conversion

    VARIABLE BASE

___
### DECIMAL
_--_
<br>
set BASE to 10

    : DECIMAL 10 BASE ! ;

___
### HEX
_--_
<br>
set BASE to 16

    : HEX 16 BASE ! ;

___
### HP
_-- addr_
<br>
hold pointer

    0 VALUE HP

___
### &lt;#
_--_
<br>
begin pictured numeric output

    : &lt;# HERE h_size + TO HP ;

___
### HOLD
_char --_
<br>
hold char for pictured numeric output

    : HOLD -1 +TO HP HP C! ;

___
### HOLDS
_c-addr u --_
<br>
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
<br>
hold digit

    : #
      0 BASE @ UM/MOD &gt;R
      BASE @ UM/MOD
      SWAP DUP 9 &gt; IF
        7 +
      THEN
      '0 + HOLD
      R&gt; ;

___
### #S
_ud -- 0 0_
<br>
hold all remaining digits

    : #S BEGIN # 2DUP D0= UNTIL ;

___
### SIGN
_n --_
<br>
hold minus sign if n &lt; 0

    : SIGN 0&lt; IF '- HOLD THEN ;

___
### #&gt;
_ud -- c-addr u_
<br>
end pictured numeric output, leave string

    : #&gt; 2DROP HP HERE h_size + OVER - ;

___
### D.R
_d +n --_
<br>
output signed double d right-aligned in field of +n chars wide

    : D.R -ROT TUCK DABS &lt;# #S ROT SIGN #&gt; ROT OVER - SPACES TYPE ;

___
### D.
_d --_
<br>
output signed double d with a trailing space

    : D. 0 D.R SPACE ;

___
### U.R
_u +n --_
<br>
output unsigned u right-aligned in field of +n chars wide

    : U.R 0 SWAP D.R ;

___
### U.
_u --_
<br>
output unsigned u with a trailing space

    : U. 0 D. ;

___
### .R
_n +n --_
<br>
output signed n right-aligned in field of +n chars wide

    : .R SWAP S&gt;D ROT D.R ;

___
### .
_n --_
<br>
output signed n with a trailing space

    : . S&gt;D D. ;

___
### ?
_addr --_
<br>
output signed cell stored at addr

    : ? @ . ;

___
### OUT
_u1 u2 --_
<br>
output byte u1 to Z80 port u2

___
### INP
_u1 -- u2_
<br>
input from Z80 port u1

___
### INKEY
_-- x_
<br>
check for key press and return the code of the key;
0x00 = no key pressed

___
### KEY-CLEAR
_--_
<br>
wait until no keys are pressed

    : KEY-CLEAR BEGIN INKEY 0= UNTIL ;

___
### KEY?
_-- flag_
<br>
true if a key is pressed

___
### KEY
_-- char_
<br>
wait and read key from the console

___
### EDIT
_c-addr +n1 n2 n3 -- c-addr +n4_
<br>
edit buffer c-addr;
buffer size +n1;
string in buffer has length n2;
non-editable left margin n3;
leaves c-addr and length +n4 (MSX INLIN strips first n3 characters)

___
### (INLIN)
_-- c-addr u_
<br>
MSX INLIN input a logical line of screen text;
leaves TIB c-addr and text input length u

key          | effect
------------ | -------------------------------------------------
cursor LEFT  | CTRL-] move cursor down
cursor RIGHT | CTRL-\ move cursor down
cursor UP    | CTRL-^ move cursor up
cursor DOWN  | CTRL-_ move cursor down
RETURN       | CTRL-M enter the logical line the cursor is on
HOME         | CTRL-K move cursor to the top left corner
SHIFT-HOME   | CTRL-L clear screen
INS          | CTRL-R toggle insert/overwrite mode
DEL          | delete character under the cursor
BS           | CTRL-H delete character to the left of the cursor
TAB          | CTRL-I insert TAB spacing
CTRL-A       | insert graphic header, follow by A to Z [ \ ] ^ _
CTRL-B       | move cursor backward to the previous word
CTRL-F       | move cursor forward to the next word
CTRL-J       | move cursor down (line feed, scrolls)
CTRL-N       | move cursor to the end of the logical line
CTRL-E       | delete from cursor to the end of the logical line
CTRL-U       | delete the entire logical line the cursor is on
(CTRL-)STOP  | program break

___
### ACCEPT
_c-addr +n1 -- +n2_
<br>
accept user input into buffer c-addr +n1;
leaves length +n2

    : ACCEPT 0 0 EDIT NIP ;

___
### #IN
_-- n_
<br>
value with line number of the input file being read from SOURCE-ID

    0 VALUE #IN

___
### &gt;IN
_-- addr_
<br>
variable with offset into the input buffer

    VARIABLE &gt;IN

___
### SOURCE-ID
_-- 0|-1|fileid_
<br>
value with 0 = console input or -1 = string input, otherwise fileid input

    0 VALUE SOURCE-ID

___
### SOURCE
_-- c-addr u_
<br>
double value with input source

    TIB DROP 0 2VALUE SOURCE

___
### RESTORE-INPUT
_... n -- flag_
<br>
restore input parameters from the stack;
flag is always FALSE (success)

    : RESTORE-INPUT DROP &gt;IN ! TO SOURCE TO SOURCE-ID FALSE ;
    : RESTORE-INPUT DROP TO #IN &gt;IN ! TO SOURCE TO SOURCE-ID FALSE ;

___
### SAVE-INPUT
_-- ... n_
<br>
save input parameters on the stack

    : SAVE-INPUT SOURCE-ID SOURCE &gt;IN @ 4 ;
    : SAVE-INPUT SOURCE-ID SOURCE &gt;IN @ #IN 5 ;

___
### REFILL
_-- flag_
<br>
attempt to refill the input buffer;
leaves FALSE when the end of input (end of file) is reached

    : REFILL
      SOURCE-ID INVERT DUP IF
        TIB OVER SWAP
        ACCEPT
        TO SOURCE
        &gt;IN OFF
      THEN ;

    : REFILL
      SOURCE-ID INVERT DUP IF
        SOURCE-ID ?DUP IF
          FIB OVER SWAP SOURCE-ID
          READ-LINE 0= AND 0= IF
            2DROP DROP FALSE
            EXIT
          THEN
          1 +TO #IN
        ELSE
          TIB OVER SWAP
          ACCEPT
          0 TO #IN
        THEN
        TO SOURCE
        &gt;IN OFF
      THEN ;

___
### SKIP
_char "&lt;chars&gt;" --_
<br>
skip chars in input when present, 0x20 (BL) skips 0x00 to 0x20 (white space and control)

    : SKIP SOURCE &gt;IN @ /STRING ROT TRIM DROP SOURCE DROP - >IN ! ;

___
### PARSE
_char "ccc&lt;char&gt;" -- c-addr u_
<br>
parse "ccc" up to char when present

    : PARSE SOURCE &gt;IN @ /STRING ROT CHOP DUP 1+ >IN @ + SOURCE NIP UMIN >IN ! ;

___
### PARSE-WORD
_char "&lt;chars&gt;ccc<char>" -- c-addr u_
<br>
parse char-delimited word;
may throw -18 "parsed string overflow"

    : PARSE-WORD
      DUP SKIP PARSE
      DUP b_size-1 U&gt; IF -18 THROW THEN ;

___
### WORD
_char "&lt;chars&gt;ccc<char>" -- c-addr_
<br>
parse word as a counted string

    : WORD TMP DUP ROT PARSE-WORD ROT 2DUP C! 1+ SWAP CMOVE ;
    : WORD PARSE-WORD SDUP DROP 1- ;

___
### CHECK-NAME
_c-addr u -- c-addr u_
<br>
check if name is valid;
may throw -16 "attempt to use a zero-length string as a name";
may throw -19 "definition name too long"

    : CHECK-NAME
      DUP 0= IF -16 THROW THEN
      DUP length_mask U&gt; IF -19 THROW THEN ;

___
### PARSE-NAME
_"&lt;spaces&gt;name<space>" -- c-addr u_
<br>
parse space-delimited name;
check if name length is valid

    : PARSE-NAME BL PARSE-WORD CHECK-NAME ;

___
### CHAR
_"&lt;spaces&gt;name<space>" -- char_
<br>
parse char

    : CHAR PARSE-NAME DROP C@ ;

___
### &gt;DIGIT
_char -- n_
<br>
convert char digit to numeric digit when within BASE;
leaves -1 if char is invalid

___
### &gt;NUMBER
_ud1 c-addr1 u1 -- ud2 c-addr2 u2_
<br>
convert string to number;
updates accumulated double ud1 to ud2;
leaves string with the remaining unconvertable chars or empty

    : &gt;NUMBER
      BEGIN DUP WHILE
        NEXT-CHAR &gt;DIGIT
        DUP 0&lt; IF
          DROP -1 /STRING
          EXIT
        THEN
        &gt;R
        2SWAP
        BASE @ UMD*
        R&gt; M+
        2SWAP
      REPEAT ;

___
### DBL
_-- flag_
<br>
true if &gt;DOUBLE or NUMBER parsed and produced a double

    0 VALUE DBL

___
### &gt;DOUBLE
_c-addr u -- d true | false_
<br>
convert string to signed double;
prefixed with character $ converts hex;
prefixed with character # converts decimal;
prefixed with character % converts binary;
leaves the double and true if string is converted;
leaves false if string is unconvertable;
sets value DBL to -1 when the number is a double;
otherwise sets value DBL to 0

___
### L&gt;NAME
_lfa -- nt_
<br>
convert link field address (lfa) to name token or name field address (nfa)

___
### NAME&gt;STRING
_nt -- c-addr u_
<br>
convert name token or name field address (nfa) to string

___
### NAME&gt;
_nt -- xt_
<br>
convert name token or name field address (nfa) to execution token or call field address (cfa)

___
### &gt;NAME
_xt -- nt_
<br>
convert execution token or call field address (cfa) to name token or name field address (nfa);
may throw -24 "invalid numeric argument"

___
### &gt;BODY
_xt -- addr_
<br>
convert execution token to parameter field address (pfa)

___
### FIND-WORD
_c-addr u -- c-addr 0 | xt 1 | xt -1_
<br>
search dictionary for matching word (case insensitive);
leaves execution token and 1 = immediate or -1 = not immediate;
leaves c-addr and 0 when not found

___
### '
_"&lt;spaces&gt;name<space>" -- xt_
<br>
parse name and search the dictionary to leave its execution token;
may throw -13 "undefined word"

    : ' PARSE-NAME FIND-WORD 0= IF -13 THROW THEN ;

___
### FIND
_c-addr -- c-addr 0 | xt 1 | xt -1_
<br>
search dictionary for the counted string to match a word (case insensitive);
leaves execution token and 1 = immediate or -1 = not immediate;
leaves c-addr and 0 when not found

    : FIND COUNT FIND-WORD ;

___
### WORDS
_--_
<br>
display context vocabulary words one screen full at a time

___
### HERE
_-- addr_
<br>
address of free memory after the dictionary;
new definitions are added here;
note that numeric output words use HERE for conversion

___
### UNUSED
_-- u_
<br>
unused dictionary space

    : UNUSED top @ HERE - ;

___
### LASTXT
_-- xt_
<br>
leaves the last execution token defined

    0 VALUE LASTXT

___
### RECURSE
_... -- ..._
<br>
recursively call the currently defined word;
may throw -14 "interpreting a compile-only word";
recursion depth should not exceed available return stack space

    : RECURSE ?COMP LASTXT COMPILE, ; IMMEDIATE
    : RECURSE ?COMP ['] ?RP COMPILE, LASTXT COMPILE, ; IMMEDIATE

___
### STATE
_-- addr_
<br>
compilation state;
STATE @ leaves TRUE when compiling;
STATE @ leaves FALSE when interpreting

    VARIABLE STATE

___
### [
_--_
<br>
switch state to interpreting

    : [ STATE OFF ;

___
### ]
_--_
<br>
switch state to compiling

    : ] STATE ON ;

___
### [']
_"&lt;spaces&gt;name<space>" -- ; -- xt_
<br>
compile xt of name as literal;
may throw -14 "interpreting a compile-only word"

    : ['] ?COMP ' LITERAL ; IMMEDIATE

___
### [CHAR]
_"&lt;spaces&gt;char" -- ; -- char_
<br>
compile char as literal;
note that the syntax 'char is preferred instead of this legacy word;
may throw -14 "interpreting a compile-only word"

    : [CHAR] ?COMP CHAR LITERAL ; IMMEDIATE

___
### [COMPILE]
_"&lt;space&gt;name<space>" -- ; ... -- ..._
<br>
compile name;
note that POSTPONE is preferred instead of this legacy word;
may throw -14 "interpreting a compile-only word"

    : [COMPILE] ?COMP ' COMPILE, ; IMMEDIATE

___
### HIDE
_--_
<br>
hide the last definition

    : HIDE CURRENT @ L&gt;NAME DUP C@ smudge_mask OR SWAP C! ;

___
### REVEAL
_--_
<br>
reveal the last definition, i.e. unhide it

    : REVEAL CURRENT @ L&gt;NAME DUP C@ ~smudge_mask AND SWAP C! ;

___
### IMMEDIATE
_--_
<br>
make the last definition immediate

    : IMMEDIATE CURRENT @ L&gt;NAME DUP C@ immediate_mask OR SWAP C! ;

___
### ?COMP
_--_
<br>
check if compiling;
may throw -14 "interpreting a compile-only word"

___
### ?SYS
_-- ; C: x --_
<br>
check if compiled control structure matches x;
may throw -14 "interpreting a compile-only word";
may throw -22 "control structure mismatch"

___
### ALLOT
_n --_
<br>
allocate n bytes starting from HERE in the dictionary;
undo the last ALLOT with negative n to reclaim memory,
but beware: don't use negative n when new words were defined;
may throw -8 "dictionary overflow"

___
### COMPILE,
_xt --_
<br>
append execution token to dictionary;
may throw -8 "dictionary overflow"

    : COMPILE, , ;

___
### ,
_x --_
<br>
append cell to dictionary;
may throw -8 "dictionary overflow"

___
### C,
_char --_
<br>
append char to dictionary;
may throw -8 "dictionary overflow"

___
### 2,
_x1 x2 --_
<br>
append double cell to dictionary;
may throw -8 "dictionary overflow"

    : 2, , , ;

___
### NFA,
_c-addr u --_
<br>
append dictionary entry with name string;
set LASTXT to HERE;
may throw -8 "dictionary overflow"

    : NFA, HERE CURRENT @ , CURRENT ! DUP C, HERE SWAP DUP ALLOT CMOVE HERE TO LASTXT ;

___
### CODE
_"&lt;spaces&gt;name<space>" --_
<br>
parse name and append dictionary entry with name;
to start compiling code into the dictionary;
set LASTXT to HERE;
may throw -8 "dictionary overflow"

    : CODE PARSE-NAME NFA, ;

___
### CFA,
_addr --_
<br>
append cfa call addr to dictionary;
may throw -8 "dictionary overflow"

___
### CFA:
_-- addr colon_sys_
<br>
append cfa colon definition to dictionary;
make CONTEXT the CURRENT vocabulary;
start compiling;
may throw -8 "dictionary overflow"

    : CFA: ] HERE colon_sys ['] (:) CFA, CURRENT TO CONTEXT ;

___
### CFA=
_xt1 xt2 -- flag_
<br>
true if xt1 has a cfa equal to a call to addr xt2;
used for introspection of
VARIABLE, 2VARIABLE and CREATE without DOES&gt; that leave a pfa (VAR),
CREATE with DOES&gt; (DOES),
VALUE (VAL), 2VALUE (2VAL),
CONSTANT (CON), 2CONSTANT (2CON),
DEFER (DEF), for example:
    3 VALUE X
    ' X ' (VAL) CFA=
leaves -1 (TRUE) meaning X is a VALUE which calls runtime (VAL)

    : CFA= OVER C@ $CD = -ROT SWAP 1+ @ = AND ;

___
### POSTPONE
_"&lt;spaces&gt;name<space>" --_
<br>
postpone compile action of name;
if name is immediate, then compile name instead of executing it;
otherwise compile name into the current colon definition;
can be used to create macros, e.g. : TRUE POSTPONE -1 ; IMMEDIATE;
may throw -13 "undefined word";
may throw -14 "interpreting a compile-only word"

___
### BUFFER:
_n "&lt;spaces&gt;name<space>" -- ; -- addr_
<br>
define buffer with n bytes;
executing name leaves address of n bytes

    : BUFFER: CREATE ALLOT ;

___
### :NONAME
_-- xt_
<br>
colon definition without name;
leaves execution token of definition to be used or saved

___
### :
_-- ; C: "&lt;spaces&gt;name<space>" -- addr colon_sys_
<br>
define name and start compiling

    : : CODE HIDE CFA: ;

___
### ;
_-- ; C: addr colon_sys --_
<br>
end colon definition and stop compiling;
may throw -14 "interpreting a compile-only word";
may throw -22 "control structure mismatch"

    : ; ?COMP colon_sys &lt;&gt; IF -22 THROW THEN DROP POSTPONE (;) REVEAL [ ; IMMEDIATE

___
### EXIT
_--_
<br>
exit colon definition;
to EXIT the definition from within a DO-LOOP, first call UNLOOP

    : EXIT ?COMP POSTPONE (EXIT) ; IMMEDIATE

___
### CREATE
_"&lt;spaces&gt;name<space>" -- ; -- addr_
<br>
create name;
executing name leaves address (HERE address after CREATE)

    : CREATE CODE ['] (VAR) CFA, ;

___
### DOES&gt;
_-- ; ... -- ..._
<br>
change CREATE name behavior to execute code after DOES&gt;

    : DOES&gt; ?COMP POSTPONE (;DOES) ['] (DOES) CFA, ; IMMEDIATE

___
### VARIABLE
_"&lt;spaces&gt;name<space>" -- ; -- addr_
<br>
define a variable;
executing name leaves address of value (initialized to zero)

    : VARIABLE CREATE 0 , ;

___
### 2VARIABLE
_"&lt;spaces&gt;name<space>" -- ; -- addr_
<br>
define a double variable;
executing name leaves address of double value (initialized to zero)

    : 2VARIABLE CREATE 0 0 2, ;

___
### CONSTANT
_x "&lt;spaces&gt;name<space>" -- ; -- x_
<br>
define a constant;
executing name leaves x

    : CONSTANT CODE ['] (CON) CFA, , ;
    : CONSTANT CREATE , DOES&gt; @ ;

___
### 2CONSTANT
_x1 x2 "&lt;spaces&gt;name<space>" -- ; -- x1 x2_
<br>
define a double constant;
executing name leaves x1 x2

    : 2CONSTANT CODE ['] (2CON) CFA, 2, ;
    : 2CONSTANT CREATE 2, DOES&gt; 2@ ;

___
### VALUE
_x "&lt;spaces&gt;name<space>" -- ; -- x_
<br>
define a value;
executing name leaves x

    : VALUE CODE ['] (VAL) CFA, , ;

___
### 2VALUE
_dx "&lt;spaces&gt;name<space>" -- ; -- dx_
<br>
define a double value;
executing name leaves dx

    : 2VALUE CODE ['] (2VAL) CFA, 2, ;

___
### TO
_"&lt;spaces&gt;name<space>" -- ; x --_
<br>
assign value name;
may throw -32 "invalid name argument"

    : TO
      '
      DUP ['] (VAL) CFA= IF
        &gt;BODY
        STATE @ IF
          POSTPONE (TO)
          ,
          EXIT
        THEN
        !
        EXIT
      THEN
      DUP ['] (2VAL) CFA= IF
        &gt;BODY
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
_"&lt;spaces&gt;name<space>" -- ; n --_
<br>
increment value name;
may throw -32 "invalid name argument"

    : +TO
      '
      DUP ['] (VAL) CFA= IF
        &gt;BODY
        STATE @ IF
          POSTPONE (+TO)
          ,
          EXIT
          THEN
        +!
        EXIT
      THEN
      DUP ['] (2VAL) CFA= IF
        &gt;BODY
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
_"&lt;spaces&gt;name<space>" -- ; ... -- ..._
<br>
define a deferred name

    : DEFER CODE ['] (DEF) CFA, ['] UNDEF , ;

___
### DEFER!
_xt1 xt2 --_
<br>
store xt1 in deferred xt2

    : DEFER! &gt;BODY ! ;

___
### DEFER@
_xt1 -- xt2_
<br>
fetch execution token from deferred xt1

    : DEFER@ &gt;BODY @ ;

___
### IS
_xt "&lt;spaces&gt;name<space>" --_
<br>
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
_"&lt;spaces&gt;name<space>" -- xt_
<br>
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
<br>
compile a literal

    : LITERAL ?COMP POSTPONE (LIT) , ; IMMEDIATE

___
### 2LITERAL
_x1 x2 -- ; -- x1 x2_
<br>
compile a double literal

    : 2LITERAL ?COMP POSTPONE (2LIT) 2, ; IMMEDIATE

___
### SLITERAL
_c-addr u -- ; -- c-addr u_
<br>
compile a string literal;
max literal string length is 255

    : SLITERAL
      ?COMP
      DUP 255 U&gt; IF -18 THROW THEN
      POSTPONE (SLIT)
      DUP C,
      HERE OVER ALLOT SWAP CMOVE ; IMMEDIATE

___
### ."
_"ccc&lt;quote&gt;" -- ; --_
<br>
type "ccc" (compiled, not interpreted)

    : ." ?COMP '" PARSE SLITERAL POSTPONE TYPE ; IMMEDIATE

___
### C"
_"ccc&lt;quote&gt;" -- ; -- c-addr_
<br>
leave counted string "ccc" (compiled, not interpreted);
may throw -18 "parsed string overflow"

    : C" ?COMP POSTPONE S" POSTPONE DROP POSTPONE 1- ; IMMEDIATE

___
### S"
_"ccc&lt;quote&gt;" -- ; -- c-addr u_
<br>
leave string "ccc" (compiled and interpreted);
truncates string to 255 characters long when excessive

    : S" '" PARSE SDUP ; IMMEDIATE

___
### S\"
_"ccc&lt;quote&gt;" -- ; -- c-addr u_
<br>
leave string "ccc" (compiled and interpreted);
truncates string to 255 characters long when excessive

    : S\" '" PARSE S\&gt;S SDUP ; IMMEDIATE

___
### S\&gt;S
_c-addr u -- c-addr u_
<br>
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
<br>
duplicate string to a TMP buffer or string literal when compiling;
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
      2DUP 2&gt;R
      CMOVE
      2R&gt; ;

___
### (UNTIL)
_x --_
<br>
branch if x = 0;
runtime of the UNTIL compile-only word

___
### (IF)
_x --_
<br>
branch if x = 0;
runtime of the IF and WHILE compile-only words

___
### (AGAIN)
_--_
<br>
branch;
runtime of the AGAIN and REPEAT compile-only words

___
### (AHEAD)
_--_
<br>
branch;
runtime of the AHEAD, ELSE and ENDOF compile-only words

___
### (OF)
_x1 x2 -- x1 or x1 x2 --_
<br>
branch if x1 &lt;&gt; x2;
runtime of the OF compile-only word

___
### (+LOOP)
_n --_
<br>
increment loop counter by n and repeat loop unless counter crosses the limit;
runtime of the +LOOP compile-only word

___
### (LOOP)
_--_
<br>
increment loop counter by 1 and repeat loop unless loop counter crosses the limit;
runtime of the LOOP compile-only word

___
### (?DO)
_n1|u1 n2|u2 --_
<br>
begin loop with limit n1|u1 and initial value n2|u2;
skip loop when zero trip loop;
runtime of the ?DO compile-only word

___
### (DO)
_n1|u1 n2|u2 --_
<br>
begin loop with limit n1|u1 and initial value n2|u2;
loop at least once;
runtime of the DO compile-only word

___
### (UNLOOP)
_R: ... --_
<br>
remove loop parameters;
runtime of the UNLOOP compile-only word

___
### (?LEAVE)
_x --_
<br>
if x is nonzero (not FALSE) then discard the loop parameters and exit the innermost do-loop;
runtime of the ?LEAVE compile-only word

___
### (LEAVE)
_--_
<br>
discard the loop parameters and exit the innermost do-loop;
runtime of the LEAVE compile-only word

___
### I
_-- n_
<br>
the loop counter value of innermost do-loop

___
### J
_-- n_
<br>
the loop counter value of outer (second) do-loop

___
### K
_-- n_
<br>
the loop counter value of outer (third) do-loop

___
### AHEAD
_-- ; C: -- addr orig_
<br>
branch ahead to THEN;
may throw -14 "interpreting a compile-only word"

___
### BEGIN
_-- ; C: -- addr dest_
<br>
begin WHILE REPEAT;
may throw -14 "interpreting a compile-only word"

___
### AGAIN
_-- ; C: addr dest --_
<br>
branch back to BEGIN;
may throw -14 "interpreting a compile-only word";
may throw -22 "control structure mismatch"

___
### UNTIL
_x -- ; C: addr dest --_
<br>
branch back to BEGIN if x = 0 (FALSE);
may throw -14 "interpreting a compile-only word";
may throw -22 "control structure mismatch"

___
### IF
_x -- ; C: -- addr orig_
<br>
branch to closest ELSE or THEN if x = 0 (FALSE);
may throw -14 "interpreting a compile-only word"

___
### THEN
_-- ; C: addr orig --_
<br>
close AHEAD, IF, ELSE;
may throw -14 "interpreting a compile-only word";
may throw -22 "control structure mismatch"

___
### ELSE
_-- ; C: addr orig -- addr orig_
<br>
close IF and branch to THEN;
may throw -14 "interpreting a compile-only word";
may throw -22 "control structure mismatch"

___
### WHILE
_x -- ; C: addr sys -- addr orig addr sys_
<br>
branch to exit REPEAT if x = 0 (FALSE);
may throw -14 "interpreting a compile-only word"

___
### REPEAT
_-- ; C: addr orig addr dest --_
<br>
branch back to BEGIN after WHILE;
may throw -14 "interpreting a compile-only word";
may throw -22 "control structure mismatch"

___
### DO
_n1|u1 n2|u2 -- ; C: -- addr do_sys_
<br>
begin loop from initial value n2|u2 to the limit n1|u1;
loop at least once;
pushes do-loop parameters onto the return stack;
may throw -14 "interpreting a compile-only word"

___
### ?DO
_n1|u1 n2|u2 -- ; C: -- addr do_sys_
<br>
begin loop from initial value n2|u2 to the limit n1|u1;
pushes do-loop parameters onto the return stack;
skip loop when zero trip loop;
may throw -14 "interpreting a compile-only word"

___
### LOOP
_-- ; C: addr do_sys --_
<br>
repeat loop unless loop counter crosses the limit;
may throw -14 "interpreting a compile-only word";
may throw -22 "control structure mismatch"

___
### +LOOP
_n|u -- ; C: addr do_sys --_
<br>
increment counter and repeat loop unless counter crosses the limit;
may throw -14 "interpreting a compile-only word";
may throw -22 "control structure mismatch"

___
### UNLOOP
_--_
<br>
remove do-loop parameters from the return stack;
may throw -14 "interpreting a compile-only word"

___
### ?LEAVE
_x --_
<br>
if x is nonzero (not FALSE) then exit the innermost do-loop;
may throw -14 "interpreting a compile-only word"

___
### LEAVE
_--_
<br>
exit the innermost do-loop;
may throw -14 "interpreting a compile-only word"

___
### CASE
_x -- ; C: -- 0_
<br>
begin CASE ENDCASE switch;
may throw -14 "interpreting a compile-only word"

___
### OF
_x1 x2 -- x1 or x1 x2 -- ; C: n1 -- orig n2_
<br>
take CASE arm if x1 = x2;
otherwise branch to next OF;
may throw -14 "interpreting a compile-only word"

___
### ENDOF
_-- ; C: n -- orig n_
<br>
branch to ENDCASE;
may throw -14 "interpreting a compile-only word";
may throw -22 "control structure mismatch"

___
### ENDCASE
_x -- ; C: n*orig n --_
<br>
close CASE;
may throw -14 "interpreting a compile-only word";
may throw -22 "control structure mismatch"

___
### ERROR
_n --_
<br>
display exception n at the offending location in the input;
n = 0 return without displaying an error message;
n = -1 ABORT and n = -2 ABORT" also clears the parameter stack
               n = -28 break is displayed as &lt;STOP&gt;
n = -56 QUIT stays silent;
other errors n are displayed as &lt;ERR-n&gt; switched to decimal,
a line number is included &lt;ERR-n:line&gt; when loading from files
list of Forth errors:

code | error
---- | ---------------------------------------------------------
-1   | ABORT
-2   | ABORT"
-3   | stack overflow
-4   | stack underflow
-5   | return stack overflow
-6   | return stack underflow
-8   | dictionary overflow
-10  | division by zero
-11  | result out of range
-13  | undefined word
-14  | interpreting a compile-only word
-15  | invalid FORGET
-16  | attempt to use zero-length string as a name
-18  | parsed string overflow
-19  | definition name too long
-22  | control structure mismatch
-24  | invalid numeric argument
-28  | user interrupt (BREAK was pressed)
-32  | invalid name argument (invalid TO name)
-36  | invalid file position
-37  | file I/O exception
-38  | non-existent file
-39  | unexpected end of file
-42  | floating-point divide by zero
-43  | floating-point result out of range
-46  | floating-point invalid argument
-56  | QUIT
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
        ." &lt;STOP&gt;"
        EXIT
      THEN
      ?DUP IF
        SOURCE &gt;IN @
        2DUP U&gt; + UMIN TYPE
        ." &lt;ERR"
        DECIMAL
        0 .R
        #IN ?DUP IF
          ': EMIT 0 .R
        THEN
        '&gt; EMIT
      THEN ;

___
### HANDLER
_-- addr_
<br>
variable with saved return stack pointer

    VARIABLE HANDLER

___
### EXECUTE
_... xt -- ..._
<br>
execute execution token xt

___
### CATCH
_... xt -- ... 0 or xt -- n_
<br>
execute xt leaving nonzero exception code n or 0 when no exception occurred;
when an exception was caught, the parameter and return stacks are restored
to their state before execution of xt

    : CATCH
      SP@ &gt;R
      HANDLER @ &gt;R
      RP@ HANDLER !
      EXECUTE
      R&gt; HANDLER !
      RDROP
      0 ;

___
### THROW
_0 -- or ... n -- ... n_
<br>
throw exception n if nonzero

    : THROW
      ?DUP IF
        HANDLER @ ?DUP IF
          RP!
          R&gt; HANDLER !
          R&gt; SWAP >R
          SP!
          DROP
          R&gt;
          EXIT
        THEN
        &gt;R CLEAR R>
        ERROR
        REPL
      THEN ;

___
### QUIT
_... -- ; R: ... --_
<br>
throw -56 "QUIT";
no exception error is displayed;
unlike ABORT, the parameter stack is not cleared

    : QUIT -56 THROW ;

___
### ABORT
_... -- ; R: ... --_
<br>
throw -1 "ABORT";
clears the parameter stack unless caught with CATCH

    : ABORT -1 THROW ;

___
### (ABORT")
_... flag c-addr u -- ; R: ... --_
<br>
if flag then abort with string message unless an active catch is present;
runtime of the ABORT" compile-only word;
throw -2 "ABORT""

    : (ABORT")
      ROT IF
        HANDLER @ IF
          2DROP
        ELSE
          TYPE
        THEN
        -2 THROW
      THEN
      2DROP ;

___
### ABORT"
_... flag -- ; C: "ccc&lt;quote&gt;" -- ; R: ... --_
<br>
if flag then abort with string message unless an active catch is present;
throw -2 "ABORT"";
clears the parameter stack unless caught with CATCH;
may throw -14 "interpreting a compile-only word"

    : ABORT" ?COMP POSTPONE S" POSTPONE (ABORT") ; IMMEDIATE

___
### BYE
_--_
<br>
return to BASIC

___
### (
_"ccc&lt;paren&gt;" --_
<br>
start a comment block;
parse and skip input up to the closing )

    : (
      ') PARSE
      BEGIN
        + DROP
        SOURCE + = IF
          DROP REFILL
        ELSE
          C@ ') &lt;&gt; IF
            REFILL
          ELSE
            FALSE
          THEN
        THEN
      0= UNTIL ; IMMEDIATE

___
### \
_"ccc&lt;eol&gt;" --_
<br>
start a comment line;
parse and skip input up to the end of line

    : \ $A PARSE 2SROP ;

___
### OK
_"ccc&lt;eol&gt;" --_
<br>
start a comment line;
parse and skip input up to the end of line;
same as \ but not immediate,
so that screen editing of Forth output before OK is made possible

    : OK POSTPONE \ ;

___
### .(
_"ccc&lt;paren&gt;" --_
<br>
emit CR then type "ccc" up to the closing )

    : .( ') PARSE CR TYPE ; IMMEDIATE

___
### NUMBER
_c-addr u -- n|u|d|ud|r_
<br>
convert string to number;
prefixed with character $ converts hex;
prefixed with character # converts decimal;
prefixed with character % converts binary;
sets value DBL to -1 when the number is a double;
sets value DBL to 1 when the number is a float;
otherwise sets value DBL to 0;
may throw -13 "undefined word" when string is not numeric

    : NUMBER
      2DUP &gt;DOUBLE IF
        2SWAP 2DROP
        DBL INVERT IF
          d&gt;S
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
      &gt;FLOAT IF
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
<br>
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
<br>
evaluate string

    : EVALUATE
      SAVE-INPUT N&gt;R
      TO SOURCE
      &gt;IN OFF
      -1 TO SOURCE-ID
      ['] INTERPRET CATCH
      DUP ERROR
      NR&gt; RESTORE-INPUT DROP
      THROW ;

___
### REPL
_--_
<br>
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
_"&lt;spaces&gt;name<space>" -- ; --_
<br>
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
      DOES&gt;
        DUP CELL+ 2@
        SWAP TO CONTEXT
        DUP CONTEXT !
        DEFINITIONS
        L&gt;NAME NAME> TO LASTXT
        @ HERE - ALLOT ;

___
### FENCE
_-- addr_
<br>
only permit FORGET past the dictionary FENCE address

    HERE VALUE FENCE

___
### FORGET
_"&lt;spaces&gt;name<space>" --_
<br>
delete name and all following definitions;
may throw -15 "invalid FORGET";
beware of vocabulary definitions crossings
(other vocabulary DEFINITIONS after markers also get deleted and corrupt their vocabulary)

___
### CONTEXT
_-- addr_
<br>
leaves address of link of the last vocabulary context definition

    ' FORTH VALUE CONTEXT

___
### CURRENT
_-- addr_
<br>
leaves address of link of the last current vocabulary definition

    ' FORTH VALUE CURRENT

___
### DEFINITIONS
_--_
<br>
make CURRENT the CONTEXT vocabulary

    : DEFINITIONS CONTEXT TO CURRENT ;

___
### VOCABULARY
_"&lt;spaces&gt;name<space>" --_
<br>
define a new vocabulary

    : VOCABULARY CURRENT CREATE , fig_kludge , DOES&gt; TO CONTEXT ;

___
### FORTH
_--_
<br>
make FORTH the CONTEXT vocabulary

    VOCABULARY FORTH



File-access words
--------------------------

___
### DRV
_-- c-addr_
<br>
last used drive letter, the default drive when none is specified explicitly, initially drive A

___
### FCX
_-- addr_
<br>
array of FCB+FIB per open file

    CREATE FCX 37 FCBN * ALLOT

___
### (FCB)
_-- addr_
<br>
allocate a new FCB;
may throw -204 "bad file number" when max files are in use

___
### S&gt;FCB
_c-addr u -- addr_
<br>
store the filename string of the form [D:]FILENAME[.EXT] in a new FCB;
wildcard '?' matches any character when used in FILENAME and EXT;
wildcard '*' matches any sequence of characters, at most one '*' may be used in FILENAME and in EXT;
may throw -204 "bad file number" when max files are in use

    : S&gt;FCB
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
_fileid -- c-addr u_
<br>
the file input buffer of size u associated with fileid;
used by INCLUDE-FILE, INCLUDE, INCLUDED, otherwise free to use

___
### (BDOS)
_addr u1 n -- u2 u3 0|1..255_
<br>
execute CP/M style MSX-DOS command C=n with DE=addr and HL=u1;
leaves u2=HL and u3=DE returned and register A for 0 (success) or 1 to 255 (failure)

___
### {DOSX}
_addr u1 n -- u2 ior_
<br>
execute CP/M style MSX-DOS command C=n with DE=addr and HL=u1;
leaves u2=HL returned and 0 (success) or nz (failure);
catches BDOS exceptions and then leaves ior = 1 (failure)

___
### BIN
_fam -- fam_
<br>
CREATE-FILE and OPEN-FILE mode fam;
note: files are always treaded as binary

___
### W/O
_-- fam_
<br>
CREATE-FILE and OPEN-FILE mode fam

___
### R/O
_-- fam_
<br>
CREATE-FILE and OPEN-FILE mode fam

___
### R/W
_-- fam_
<br>
CREATE-FILE and OPEN-FILE mode fam

___
### CREATE-FILE
_c-addr u fam -- fileid ior_
<br>
create a new file given by the string filename, where fam is R/W, R/O or W/O;
if the file already exists, then it is truncated to zero length;
leaves fileid (a fcb-addr) and ior 0 (success) or -203 (failure)

___
### OPEN-FILE
_c-addr u fam -- fileid ior_
<br>
open a file given by the string filename, where fam is R/W, R/O or W/O;
leaves fileid (a fcb-addr) and ior 0 (success) or -203 (failure);
filename format: [D:]FILENAME[.EXT] where drive D: becomes the default drive when specified;
device names: AUX CON LST NUL PRN

___
### CLOSE-FILE
_fileid -- ior_
<br>
___
### close file with
_fileid (a fcb-addr);_
<br>
leaves ior 0 (success) or -197 (failure)

___
### CLOSE-FILES
_--_
<br>
close all open files

___
### READ-FILE
_c-addr u1 fileid -- u2 ior_
<br>
read into buffer c-addr of size u1 from fileid (a fcb-addr);
leaves number u2 of bytes read into the buffer and ior 0 (success) or 1 (u2 is zero and eof) or may throw an MSX error
to read a single character to a cell on the stack: 0 SP@ 1 fileid READ-FILE -- char 0|1 ior


___
### READ-LINE
_c-addr u1 fileid -- u2 flag ior_
<br>
read a line into buffer c-addr of size u1 from fileid;
leaves number u2 of bytes read into the buffer (without terminating CR/LF) and flag TRUE;
leaves flag FALSE when u2 is zero and EOF is reached;
ior is nonzero when an error occurred, use READ-LINE 0= AND 0= which is TRUE for EOF or error

    : READ-LINE
      DUP FILE-POSITION DROP   \ c-addr u1 fileid ud
      2&gt;R                      \ c-addr u1 fileid
      2&gt;R R@ SWAP DUP 2R>      \ fileid c-addr c-addr u1 fileid
      READ-FILE IF             \ fileid c-addr u2 ior
        2DROP DROP RDROP RDROP
        0 FALSE 0
        EXIT
      THEN                     \ fileid c-addr u2
      10 CHOP                  \ fileid c-addr u3
      DUP 1+                   \ fileid c-addr u3 u3+1
      2R&gt; ROT M+               \ fileid c-addr u3 ud+u3+1
      2&gt;R                      \ fileid c-addr u3
      ROT                      \ c-addr u3 fileid
      2R&gt; ROT                  \ c-addr u3 ud+u3+1 fileid
      REPOSITION-FILE DROP     \ c-addr u3
      13 -TRIM                 \ c-addr u4
      NIP TRUE 0 ;

___
### WRITE-FILE
_c-addr u1 fileid -- ior_
<br>
write buffer c-addr of size u1 to fileid (a fcb-addr);
leaves ior 0 (success) or 1 (disk full) or may throw an MSX error

___
### WRITE-LINE
_c-addr u1 fileid -- ior_
<br>
write buffer c-addr of size u1 to fileid (a fcb-addr) followed by a CR/LF pair;
leaves ior 0 (success) or 1 (disk full)

___
### FILE-POSITION
_fileid -- ud ior_
<br>
the fileid (a fcb-addr);
leaves file position ud and ior (always 0 for success)

___
### REPOSITION-FILE
_ud fileid -- ior_
<br>
for the open fileid (a fcb-addr) set the file position to ud;
leaves ior (always 0 for success)

___
### FILE-SIZE
_fileid -- ud ior_
<br>
the fileid (a fcb-addr);
leaves file size ud and ior (always 0 for success)

___
### RESIZE-FILE
_ud fileid -- ior_
<br>
the fileid (a fcb-addr);
leaves ior 0 (success) or 1 (disk full) or may throw an MSX error

___
### DELETE-FILE
_c-addr u -- ior_
<br>
delete the file with the name specified by the string c-addr u;
leaves ior 0 (success) or 255 (failure) or may throw an MSX error

___
### RENAME-FILE
_c-addr1 u1 c-addr2 u2 -- ior_
<br>
rename the file with the name specified by the string c-addr1 u1 to the name c-addr2 u2
leaves ior 0 (success) or 255 (failure) or may throw an MSX error

___
### INCLUDE-FILE
_... fileid -- ..._
<br>
read and interpret Forth source code from fileid;
fileid is closed afterwards

    : INCLUDE-FILE
      SAVE-INPUT N&gt;R
      TO SOURCE-ID
      BEGIN
        0
        REFILL
      WHILE
        DROP
        ['] INTERPRET CATCH
        ?DUP 0=
      WHILE
      REPEAT
      SOURCE-ID CLOSE-FILE DROP
      DUP ERROR
      NR&gt; RESTORE-INPUT DROP
      THROW ;

___
### INCLUDED
_... c-addr u -- ..._
<br>
read and interpret Forth source code from the file named by the string c-addr u

    : INCLUDED
      R/O OPEN-FILE THROW
      INCLUDE-FILE ;

___
### INCLUDE
_... "&lt;spaces&gt;name" -- ..._
<br>
read and interpret Forth source code from file "name"

    : INCLUDE PARSE-NAME INCLUDED ;

___
### REQUIRED
_... c-addr u -- ..._
<br>
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
      CURRENT DUP @ 2&gt;R HERE >R
      2DUP NFA, marker_does CFA,  \ create marker with marker DOES&gt; cfa
      R&gt; , 2R> 2,                 \ marker body stores HERE CURRENT CURRENT@
      1 /STRING INCLUDED ;

___
### REQUIRE
_... "&lt;spaces&gt;name" -- ..._
<br>
read and interpret Forth source code from file "name",
if the file was not already included;
this also adds file name with a leading space to the dictionary to assert inclusion;
beware of vocabulary definitions crossings
(other vocabulary DEFINITIONS after markers also get deleted and corrupt their vocabulary)

    : REQUIRE PARSE-NAME REQUIRED ;

___
### ANEW
_... "&lt;spaces&gt;name" -- ..._
<br>
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
<br>
truncate float towards zero

___
### FLOOR
_r1 -- r2_
<br>
floor float towards negative infinity

___
### FNEGATE
_r1 -- r2_
<br>
negate float

___
### FABS
_r1 -- r2_
<br>
absolute value |r1|

    : FABS 2DUP F0&lt; IF FNEGATE THEN ;

___
### FSQRT
_r1 -- r2_
<br>
take the square root of r1

___
### FSIN
_r1 -- r2_
<br>
sine of float in radian

___
### FCOS
_r1 -- r2_
<br>
cosine of float in radian

___
### FTAN
_r1 -- r2_
<br>
tangent of float in radian

___
### FATAN
_r1 -- r2_
<br>
arc tangent of float, in radian

___
### FRAND
_r1 -- r2_
<br>
if r1 is positive, then leave new random number from 0 to 1 exclusive;
if r1 is zero, then leave the last random number;
if r1 is negative, then seed the random number using r1

___
### F+
_r1 r2 -- r3_
<br>
sum r1+r2

___
### F-
_r1 r2 -- r3_
<br>
difference r1-r2

___
### F*
_r1 r2 -- r3_
<br>
product r1*r2

___
### F/
_r1 r2 -- r3_
<br>
quotient r1/r2

___
### F**
_r1 r2 -- r3_
<br>
raise r1 to r2

___
### FASIN
_r1 -- r2_
<br>
arc sine of float, in radian

    : FASIN
      2DUP F0= IF EXIT THEN
      2DUP FABS 1E0 F= IF                           \ if |x|=1 then
        PI/2 2SWAP F0&lt; IF FNEGATE THEN              \ sign(x)*pi/2
___
### ;
_ EXIT_
<br>
      THEN
      2DUP 2DUP F* 1E0 2SWAP F- FSQRT FATAN2 ;      \ arctan(x/sqrt(1-x^2)) = atan2(x,sqrt(1-x*x))

___
### FACOS
_r1 -- r2_
<br>
arc cosine of float, in radian

    : FACOS FASIN PI/2 2SWAP F- ;

___
### FATAN2
_r1 r2 -- r3_
<br>
atan2(r1,r2) = atan(r1/r2) but using a more accurate formulation

___
### PI
_-- r_
<br>
floating-point constant pi

    3.14159E0 2CONSTANT PI

___
### PI/2
_-- r_
<br>
floating-point constant pi/2 (half pi)

    1.57080E0 2CONSTANT PI/2

___
### FLN
_r1 -- r2_
<br>
natural log of float

___
### FEXP
_r1 -- r2_
<br>
natural exponent of float

___
### FLOG
_r1 -- r2_
<br>
base 10 log of float

    : FLOG FLN 0.434294E0 F* ;    \ = ln(x)/ln(10) approx ln(10) such that 10E0 FLOG = 1E0

___
### FALOG
_r1 -- r2_
<br>
base 10 exponent of float

    : FALOG 2.30259E0 F* FEXP ;    \ = exp(x*ln(10))

___
### FSINH
_r1 -- r2_
<br>
sine hyperbolicus of float

    : FSINH FEXP 2DUP 1E0 2SWAP F/ F- .5E0 F* ;

___
### FCOSH
_r1 -- r2_
<br>
cosine hyperbolicus of float

    : FCOSH FEXP 2DUP 1E0 2SWAP F/ F+ .5E0 F* ;

___
### FTANH
_r1 -- r2_
<br>
tangent hyperbolicus of float

    : FTANH 2DUP F+ FEXP 2DUP 1E0 F- 2SWAP 1E0 F+ F/ ;

___
### FASINH
_r1 -- r2_
<br>
arc sine hyperbolicus of float

    : FASINH 2DUP 2DUP F* 1E0 F+ FSQRT F+ FLN ;

___
### FACOSH
_r1 -- r2_
<br>
arc cosine hyperbolicus of float

    : FACOSH 2DUP 2DUP F* 1E0 F- FSQRT F+ FLN ;

___
### FATANH
_r1 -- r2_
<br>
arc tangent hyperbolicus of float

    : FATANH 2DUP 1E0 F+ 2SWAP 1E0 2SWAP F- F/ FLN .5E0 F* ;

___
### F=
_r1 r2 -- flag_
<br>
true if r1 = r2

___
### F&lt;
_r1 r2 -- flag_
<br>
true if r1 &lt; r2

    : F&lt;
      DUP 3 PICK AND 0&lt; IF
        2SWAP
      D&lt; ; ( works for MSX MATH-PACK decimals )

___
### F0=
_r -- flag_
<br>
true if r = 0.0e0

    : F0= D0= ; ( works for MSX MATH-PACK decimals )

___
### F0&lt;
_r -- flag_
<br>
true if r &lt; 0.0e0

    : F0&lt; D0< ; ( works for MSX MATH-PACK decimals )

___
### FMAX
_r1 r2 -- r3_
<br>
max of r1 and r2

    : FMAX
      2OVER 2OVER F&lt; IF 2SWAP THEN
      2DROP ;

___
### FMIN
_r1 r2 -- r3_
<br>
min of r1 and r2

    : FMIN
      2OVER 2OVER F&lt; INVERT IF 2SWAP THEN
      2DROP ;

___
### D&gt;F
_d -- r_
<br>
widen signed double to float;
this word is much slower than the optimized S&gt;F

    : D&gt;F
      BASE @ -ROT
      DECIMAL
      TUCK DABS &lt;# #S ROT SIGN #&gt; >FLOAT DROP
      ROT BASE ! ;

___
### S&gt;F
_n -- r_
<br>
widen signed single to float

    : S&gt;F S>D D>F ;

___
### F&gt;D
_r -- d_
<br>
narrow float to a signed double;
may throw -11 "result out of range";
this word is much slower than the optimized F&gt;S

    : F&gt;D
      HERE 1+ 10 REPRESENT DROP SWAP
      DUP 0&lt; IF
        2DROP 0.
        EXIT
      THEN
      DUP 10 &gt; IF
        -11 THROW
      THEN
___
###       '- HERE C!
_\ place '-' in here_
<br>
___
###       OVER -
_\ sign exp-sign_
<br>
___
###       SWAP HERE 1+ +
_\ exp-sign here+1+sign_
<br>
___
###       SWAP
_\ here+1+sign exp-sign_
<br>
      &gt;DOUBLE 0= IF
        -11 THROW
      THEN ;

___
### F&gt;S
_r -- n_
<br>
narrow float to a signed single;
may throw -11 "result out of range" or -250 "numeric overflow"

    : F&gt;S F>D D>S ;

___
### &gt;FLOAT
_c-addr u -- r true | false_
<br>
convert string to float;
leaves the float and true if string is converted;
leaves false if string is unconvertable;

___
### REPRESENT
_r c-addr u -- n flag true_
<br>
convert float to string;
store decimal digits of the float in buffer c-addr with size u &gt; 0;
leaves decimal exponent n+1 and flag = true if negative

___
### PRECISION
_-- +n_
<br>
floating-point output precision, the number of decimal digits displayed is 6 by default

    6 VALUE PRECISION

___
### FS.
_r --_
<br>
output float in scientific notation with a trailing space

    : FS.
      HERE PRECISION REPRESENT DROP IF
        '- EMIT
      THEN
      HERE C@ DUP EMIT
      '0 &lt;&gt; +
      '. HERE C!
      HERE PRECISION '0 -TRIM TYPE
      'E EMIT . ;

___
### F.
_r --_
<br>
output float with a trailing space;
output fixed notation when 1e-1 &lt;= |r| < 1e+7, otherwise output scientific notation;
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
<br>
sum r1+r2;
may throw -43 "floating-point result out of range"

___
### F-
_r1 r2 -- r3_
<br>
difference r1-r2;
may throw -43 "floating-point result out of range"

___
### F*
_r1 r2 -- r3_
<br>
product r1*r2;
may throw -43 "floating-point result out of range"

___
### F/
_r1 r2 -- r3_
<br>
quotient r1/r2;
may throw -42 "floating-point divide by zero";
may throw -43 "floating-point result out of range"

___
### FTRUNC
_r1 -- r2_
<br>
truncate float towards zero

___
### FLOOR
_r1 -- r2_
<br>
floor float towards negative infinity;
may throw -43 "floating-point result out of range"

___
### FROUND
_r1 -- r2_
<br>
round float to nearest;
may throw -43 "floating-point result out of range"

___
### FNEGATE
_r1 -- r2_
<br>
negate float

___
### FABS
_r1 -- r2_
<br>
absolute value |r1|

    : FABS 2DUP F0&lt; IF FNEGATE THEN ;

___
### F=
_r1 r2 -- flag_
<br>
true if r1 = r2

    : F= D= ; ( works for IEEE 754 floating-point without negative zero and inf/nan )

___
### F&lt;
_r1 r2 -- flag_
<br>
true if r1 &lt; r2

    : F&lt;
      DUP 3 PICK AND 0&lt; IF
        2SWAP
      D&lt; ; ( works for IEEE 754 floating-point without negative zero and inf/nan )

___
### F0=
_r -- flag_
<br>
true if r = 0.0e0

    : F0= D0= ; ( works for IEEE 754 floating-point without negative zero and inf/nan )

___
### F0&lt;
_r -- flag_
<br>
true if r &lt; 0.0e0

    : F0&lt; D0< ; ( works for IEEE 754 floating-point without negative zero and inf/nan )

___
### FMAX
_r1 r2 -- r3_
<br>
max of r1 and r2

    : FMAX
      2OVER 2OVER F&lt; IF 2SWAP THEN
      2DROP ;

___
### FMIN
_r1 r2 -- r3_
<br>
min of r1 and r2

    : FMIN
      2OVER 2OVER F&lt; INVERT IF 2SWAP THEN
      2DROP ;

___
### D&gt;F
_d -- r_
<br>
widen signed double to float

___
### S&gt;F
_n -- r_
<br>
widen signed single to float

___
### F&gt;D
_r -- d_
<br>
narrow float to a signed double;
may throw -11 "result out of range"

___
### F&gt;S
_r -- n_
<br>
narrow float to a signed single;
may throw -11 "result out of range"

___
### &gt;FLOAT
_c-addr u -- r true | false_
<br>
convert string to float;
leaves the float and true if string is converted;
leaves false if string is unconvertable

___
### REPRESENT
_r c-addr u -- n flag true_
<br>
convert float to string;
store decimal digits of the float in buffer c-addr with size u &gt; 0;
leaves decimal exponent n+1 and flag = true if negative

___
### PRECISION
_-- +n_
<br>
floating-point output precision, the number of decimal digits displayed is 7 by default

    7 VALUE PRECISION

___
### FS.
_r --_
<br>
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
<br>
output float with a trailing space;
output fixed notation when 1e-1 &lt;= |r| < 1e+7, otherwise output scientific notation

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
<br>
square root of float

    : FSQRT
      2DUP F0&lt; IF -46 THROW THEN
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
<br>
floating-point constant pi

    3.1415928E0 2CONSTANT PI

___
### PI/2
_-- r_
<br>
floating-point constant pi/2 (half pi)

    1.5707964E0 2CONSTANT PI/2

___
### FCOSI
_r1 flag -- r2_
<br>
if flag is -1 (TRUE) then leave sin(r1) else if flag is 0 (FALSE) then leave cos(r1)

    : FCOSI                                 \ r2=sin(r1) if flag=-1 else r2=cos(r1) if flag=0
      &gt;R                                    \ save flag
      PI/2 F/                               \ map r1 to x in [-pi/4,pi/4]
      2DUP .5E0 F+ F&gt;D                      \ floor(2x/pi+.5)
      \ save (floor(2x/pi+.5)+flag+1) mod 4 = quadrant 0,1,2,3 where flag is -1 (sin) or 0 (cos)
      OVER R&gt; + 1+ >R
      D&gt;F F- PI/2 F*                        \ pi/2 * (2x/pi - floor(2x/pi + .5))
      2DUP 2DUP F* FNEGATE 2SWAP            \ -- -x*x x
      \ quadrant 0:  sin(x) =  x - x^3/3! + x^5/5! - x^7/7! + ...
      \ quadrant 1:  cos(x) =  1 - x^2/2! + x^4/4! - x^6/6! + ...
      \ quadrant 2: -sin(x) = -x + x^3/3! - x^5/5! + x^7/7! - ...
      \ quadrant 3: -cos(x) = -1 + x^2/2! - x^4/4! + x^6/6! - ...
      R@ 1 AND IF 2DROP 1E0 THEN            \ initial term 1 for quadrant 1 and 3
      R@ 2 AND IF FNEGATE THEN              \ negate initial term for quadrant 2 and 4
      2SWAP                                 \ -- x|1|-x|-1 -x*x
      \ Maclaurin series iterations i=2,4,6,8,10,12 (cos) or i=1,3,5,7,9,11 (sin)
      13 2 R&gt; 1 AND - DO                    \ 6 iterations
        2OVER 2OVER F*                      \ -- ... term -x*x -x*x*term
        I DUP 1+ *                          \ -- ... term -x*x -x*x*term i*(i+1)
        S&gt;F F/                              \ -- ... term -x*x -x*x*term/(i*(i+1))
        2SWAP                               \ -- ... term -x*x*term/(i*(i+1)) -x*x
      2 +LOOP
      2DROP
      F+ F+ F+ F+ F+ F+ ;                   \ sum the 7 terms in reverse order for accuracy

___
### FSIN
_r1 -- r2_
<br>
sine of float in radian

    : FSIN TRUE FCOSI ;

___
### FCOS
_r1 -- r2_
<br>
cosine of float in radian

    : FCOS FALSE FCOSI ;

___
### FTAN
_r1 -- r2_
<br>
tangent of float in radian

    : FTAN 2DUP FSIN 2SWAP FCOS F/ ;

___
### FASIN
_r1 -- r2_
<br>
arc sine of float, in radian

    : FASIN
      2DUP F0= IF EXIT THEN
      2DUP FABS 1E0 F= IF                           \ if |x|=1 then
        PI/2 2SWAP F0&lt; IF FNEGATE THEN              \ sign(x)*pi/2
___
### ;
_ EXIT_
<br>
      THEN
      2DUP 2DUP F* 1E0 2SWAP F- FSQRT FATAN2 ;      \ arctan(x/sqrt(1-x^2)) = atan2(x,sqrt(1-x*x))

___
### FACOS
_r1 -- r2_
<br>
arc cosine of float, in radian

    : FACOS FASIN PI/2 2SWAP F- ;

___
### FATAN
_r1 -- r2_
<br>
arc tangent of float, in radian

    : FATAN
      \ map r1 to [-1,1] using arctan(x) = sign(x) * (pi/2-arctan(1/abs(x)))
      1E0 2OVER FABS F&lt; IF                  \ if |r1| &gt; 1 then
        2DUP F0&lt; -ROT
        1E0 2SWAP FABS F/
        TRUE
      ELSE
        FALSE
      THEN
      -ROT
      \ map r1 in [-1,1] to [-sqrt(2)+1,sqrt(2)-1] using arctan(x) = 2*arctan(x/(1+sqrt(1+x^2)))
      .41423562E0 2OVER FABS F&lt; IF          \ if |r1| &gt; sqrt(2)-1 then
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
        2DUP I S&gt;F F/                       \ -- x -x^3/3 ... -x*x -x*x*term -x*x*term/i
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
<br>
atan2(r1,r2) = atan(r1/r2) but using a more accurate formulation

    : FATAN2
      2DUP FNEGATE F0&lt; IF
        F/ FATAN
        EXIT
      THEN
      2SWAP
      2DUP F0= IF
        2DROP F0&lt; IF PI ELSE PI/2 THEN
        EXIT
      THEN
      PI/2 2OVER F0&lt; IF FNEGATE THEN
      2ROT 2ROT F/ FATAN F- ;

___
### FLN
_r1 -- r2_
<br>
natural log of float

    : FLN
      2DUP 2DUP F0&lt; -ROT F0= OR IF -46 THROW THEN
      \ map r1 to [0.5,1) using ln(x*2^n) = ln(x) + ln(2^n) = ln(x) + n*ln(2)
      DUP 7 RSHIFT $7e - -ROT               \ 2^(n+1) = 2^(exponent - bias + 1)
      $7f AND $3f00 +                       \ remove exponent 2^(n+1)
      1E0 2SWAP F-                          \ 1-x
      \ Maclaurin series -ln(1-x) = x + x^2/2 + x^3/3 + ... with x in (0,0.5]
      2DUP 2DUP                             \ -- x x x
      22 2 DO                               \ 20 iterations
        2OVER F*                            \ -- x x^2/2 ... x term*x
        2DUP I S&gt;F F/                       \ -- x x^2/2 ... x term*x term*x/i
        2ROT 2ROT                           \ -- x x^2/2 ... term*x/i x term*x
      LOOP
      2DROP 2DROP                           \ -- x x^2/2 ... x^19/19
      20 0 DO F+ LOOP                       \ sum the 21 terms in reverse order
      FNEGATE
      ROT S&gt;F .69314724E0 F* F+ ;           \ + n*ln(2) with approx ln(2) such that 1E0 FLN = 0

___
### FEXP
_r1 -- r2_
<br>
natural exponent of float

    : FEXP
      2DUP F0&lt; -ROT
      FABS
      \ map |r1| to [0,ln(2)) using exp(x+k*ln(2)) = exp(x)*2^k
      2DUP .69314724E0 F/ F&gt;S               \ ln(2) = .69314724E0
      DUP $7f &gt; IF -43 THROW THEN           \ multiply by 2^k will overflow
      DUP&gt;R
      S&gt;F .69314724E0 F* F-                 \ ln(2) = .69314724E0
      \ Maclaurin series expm1(x) = exp(x) - 1 = x + x^2/2! + x^3/3! + ...
      2DUP                                  \ -- x x
      10 2 DO                               \ 8 iterations
        2OVER 2OVER F*                      \ -- x x^2/2! ... term x term*x
        I S&gt;F F/                            \ -- x x^2/2! ... term x term*x/i
        2SWAP                               \ -- x x^2/2! ... term term*x/i x
      LOOP
      2DROP                                 \ -- x x^2/2! ... x^9/9!
      F+ F+ F+ F+ F+ F+ F+ F+               \ sum the 9 terms in reverse order
      1E0 F+                                \ exp(x) = expm1(x) + 1
      R&gt; 7 LSHIFT +                         \ multiply exp(x) by 2^k
      ROT IF 1E0 2SWAP F/ THEN ;            \ return reciprocal for negative r1

___
### FLOG
_r1 -- r2_
<br>
base 10 log of float

    : FLOG FLN 0.4342945E0 F* ;    \ = ln(x)/ln(10) approx ln(10) such that 10E0 FLOG = 1E0

___
### FALOG
_r1 -- r2_
<br>
base 10 exponent of float

    : FALOG 2.3025853E0 F* FEXP ;    \ = exp(x*ln(10))

___
### F^
_r1 r2 -- r3_
<br>
raise r1 to r2 using exp(ln(r1)*r2) where r1 &gt; 0

    : F^ 2SWAP FLN F* FEXP ;

___
### F**
_r1 r2 -- r3_
<br>
raise r1 to r2

    : F**
      2DUP F0= IF                           \ r2 = 0
        2OVER F0= IF -46 THROW THEN         \ error if r1 = 0 and r2 = 0
        2DROP 2DROP 1E0 EXIT                \ return 1.0E0
      THEN
      2OVER F0= IF                          \ r1 = 0
        2DUP F0&lt; IF -46 THROW THEN          \ error if r1 = 0 and r2 < 0
        2DROP 2DROP 0E0 EXIT                \ return 0.0E0
      THEN
      \ exponentiation r1^n by repeated squaring when n is a small integer |n|&lt;=16
      2DUP 2DUP FTRUNC F= IF                \ r2 has no fractional part
        2DUP ['] F&gt;D CATCH 0= IF            \ r2 is convertable to a double n
          2DUP DABS 17. DU&lt; IF              \ |n| <= 16
            DROP                            \ drop high order of n
            DUP 0&lt; &gt;R                       \ save sign of n
            ABS &gt;R                          \ save |n|
            2DROP                           \ drop old r2
            1E0                             \ -- r1 1.0
            BEGIN
              R@ 1 AND IF 2OVER F* THEN
              R&gt; 1 RSHIFT                   \ -- r1^n product u>>1
            DUP WHILE
              &gt;R
              2SWAP 2DUP F* 2SWAP           \ -- r1^n^2 product u&gt;>1
            REPEAT
            DROP 2SWAP 2DROP                \ -- product
            R&gt; IF 1E0 2SWAP F/ THEN         \ reciprocal when exponent was negative
            EXIT
          THEN
          OVER 1 AND IF                     \ n is odd
            2OVER F0&lt; IF                    \ r1 is negative
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
<br>
sine hyperbolicus of float

    : FSINH FEXP 2DUP 1E0 2SWAP F/ F- .5E0 F* ;

___
### FCOSH
_r1 -- r2_
<br>
cosine hyperbolicus of float

    : FCOSH FEXP 2DUP 1E0 2SWAP F/ F+ .5E0 F* ;

___
### FTANH
_r1 -- r2_
<br>
tangent hyperbolicus of float

    : FTANH 2DUP F+ FEXP 2DUP 1E0 F- 2SWAP 1E0 F+ F/ ;

___
### FASINH
_r1 -- r2_
<br>
arc sine hyperbolicus of float

    : FASINH 2DUP 2DUP F* 1E0 F+ FSQRT F+ FLN ;

___
### FACOSH
_r1 -- r2_
<br>
arc cosine hyperbolicus of float

    : FACOSH 2DUP 2DUP F* 1E0 F- FSQRT F+ FLN ;

___
### FATANH
_r1 -- r2_
<br>
arc tangent hyperbolicus of float

    : FATANH 2DUP 1E0 F+ 2SWAP 1E0 2SWAP F- F/ FLN .5E0 F* ;



Alphabetic list of words
------------------------

word | stack
---- | -----
[`!`](#!)	|		x addr --
[`#&gt;`](##&gt;)	|		ud -- c-addr u
[`#IN`](##IN)	|		-- n
[`#S`](##S)	|		ud -- 0 0
[`#`](##)	|		ud1 -- ud2
[`&gt;BODY`](#&gt;BODY)	|		xt -- addr
[`&gt;DIGIT`](#&gt;DIGIT)	|	char -- n
[`&gt;DOUBLE`](#&gt;DOUBLE)	|	c-addr u -- d true | false
[`&gt;FLOAT`](#&gt;FLOAT)	|	c-addr u -- r true | false
[`&gt;IN`](#&gt;IN)	|		-- addr
[`&gt;NAME`](#&gt;NAME)	|		xt -- nt
[`&gt;NUMBER`](#&gt;NUMBER)	|	ud1 c-addr1 u1 -- ud2 c-addr2 u2
[`&gt;R`](#&gt;R)	|		x -- ; R: -- x
[`&gt;`](#&gt;)	|		n1 n2 -- flag
[`&lt;#`](#&lt;#)	|		--
[`&lt;&gt;`](#&lt;&gt;)	|		x1 x2 -- flag
[`&lt;`](#&lt;)	|		n1 n2 -- flag
[`'`](#')	|		"&lt;spaces&gt;name<space>" -- xt
[`(+LOOP)`](#(+LOOP))	|	n --
[`(+TO)`](#(+TO))	|		n --
[`(2CON)`](#(2CON))	|	-- x
[`(2LIT)`](#(2LIT))	|	-- x1 x2
[`(2TO)`](#(2TO))	|		dx --
[`(2VAL)`](#(2VAL))	|	-- dx
[`(:)`](#(:))	|		-- ; R: -- ip
[`(;)`](#(;))	|		-- ; R: ip --
[`(;DOES)`](#(;DOES))	|	-- ; R: ip --
[`(?DO)`](#(?DO))	|		n1|u1 n2|u2 --
[`(?LEAVE)`](#(?LEAVE))	|	x --
[`(ABORT")`](#(ABORT"))	|	... flag c-addr u -- ; R: ... --
[`(AGAIN)`](#(AGAIN))	|	--
[`(AHEAD)`](#(AHEAD))	|	--
[`(CON)`](#(CON))	|		-- x
[`(D+TO)`](#(D+TO))	|	d --
[`(DEF)`](#(DEF))	|		--
[`(DO)`](#(DO))	|		n1|u1 n2|u2 --
[`(DOES)`](#(DOES))	|	addr -- addr ; R: -- ip
[`(EXIT)`](#(EXIT))	|	-- ; R: ip --
[`(IF)`](#(IF))	|		x --
[`(INLIN)`](#(INLIN))	|	-- c-addr u
[`(LEAVE)`](#(LEAVE))	|	--
[`(LIT)`](#(LIT))	|		-- x
[`(LOOP)`](#(LOOP))	|	--
[`(OF)`](#(OF))	|		x1 x2 -- x1 or x1 x2 --
[`(SLIT)`](#(SLIT))	|	-- c-addr u
[`(TO)`](#(TO))	|		x --
[`(UNLOOP)`](#(UNLOOP))	|	R: ... --
[`(UNTIL)`](#(UNTIL))	|	x --
[`(VAL)`](#(VAL))	|		-- x
[`(VAR)`](#(VAR))	|		-- addr
[`(`](#()	|		"ccc&lt;paren&gt;" --
[`*/MOD`](#*/MOD)	|		n1 n2 n3 -- n4 n5
[`*/`](#*/)	|		n1 n2 n3 -- n4
[`*`](#*)	|		n1|u1 n2|u2 -- n3|u3
[`+!`](#+!)	|		n addr --
[`+LOOP`](#+LOOP)	|		n|u -- ; C: addr do_sys --
[`+TO`](#+TO)	|		"&lt;spaces&gt;name<space>" -- ; n --
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
[`0&lt;`](#0&lt;)	|		n -- flag
[`0=`](#0=)	|		x -- flag
[`0`](#0)	|		-- 0
[`1+`](#1+)	|		n1 -- n2
[`1-`](#1-)	|		n1 -- n2
[`1`](#1)	|		-- 1
[`2!`](#2!)	|		x1 x2 addr --
[`2&gt;R`](#2&gt;R)	|		x1 x2 -- ; R: -- x1 x2
[`2*`](#2*)	|		n1 -- n2
[`2+`](#2+)	|		n1 -- n2
[`2,`](#2,)	|		x1 x2 --
[`2-`](#2-)	|		n1 -- n2
[`2/`](#2/)	|		n1 -- n2
[`2@`](#2@)	|		addr -- x1 x2
[`2CONSTANT`](#2CONSTANT)	|	x1 x2 "&lt;spaces&gt;name<space>" -- ; -- x1 x2
[`2DROP`](#2DROP)	|		xd1 xd2 -- xd1
[`2DUP`](#2DUP)	|		xd -- xd xd
[`2LITERAL`](#2LITERAL)	|	x1 x2 -- ; -- x1 x2
[`2OVER`](#2OVER)	|		xd1 xd2 -- xd1 xd2 xd1
[`2R&gt;`](#2R&gt;)	|		R: x1 x2 -- ; -- x1 x2
[`2R@`](#2R@)	|		R: x1 x2 -- x1 x2 ; -- x1 x2
[`2ROT`](#2ROT)	|		xd1 xd2 xd3 -- xd2 xd3 xd1
[`2SWAP`](#2SWAP)	|		xd1 xd2 -- xd2 xd1
[`2VALUE`](#2VALUE)	|	dx "&lt;spaces&gt;name<space>" -- ; -- dx
[`2VARIABLE`](#2VARIABLE)	|	"&lt;spaces&gt;name<space>" -- ; -- addr
[`2`](#2)	|		-- 2
[`3`](#3)	|		-- 3
[`:NONAME`](#:NONAME)	|	-- xt
[`:`](#:)	|		-- ; C: "&lt;spaces&gt;name<space>" -- addr colon_sys
[`;`](#;)	|		-- ; C: addr colon_sys --
[`=`](#=)	|		x1 x2 -- flag
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
[`ACTION-OF`](#ACTION-OF)	|	"&lt;spaces&gt;name<space>" -- xt
[`AGAIN`](#AGAIN)	|		-- ; C: addr dest --
[`AHEAD`](#AHEAD)	|		-- ; C: -- addr orig
[`ALLOT`](#ALLOT)	|		n --
[`AND`](#AND)	|		x1 x2 -- x1&x2
[`AT-XY`](#AT-XY)	|		u1 u2 --
[`BASE`](#BASE)	|		-- addr
[`BEGIN`](#BEGIN)	|		-- ; C: -- addr dest
[`BLANK`](#BLANK)	|		c-addr u --
[`BL`](#BL)	|		-- 32
[`BUFFER:`](#BUFFER:)	|	n "&lt;spaces&gt;name<space>" -- ; -- addr
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
[`CFA:`](#CFA:)	|		-- addr colon_sys
[`CFA=`](#CFA=)	|		xt1 xt2 -- flag
[`CHAR+`](#CHAR+)	|		n1 -- n1
[`CHARS`](#CHARS)	|		n1 -- n2
[`CHAR`](#CHAR)	|		"&lt;spaces&gt;name<space>" -- char
[`CHECK-NAME`](#CHECK-NAME)	|	c-addr u -- c-addr u
[`CHOP`](#CHOP)	|		c-addr u1 char -- c-addr u2
[`CLEAR`](#CLEAR)	|		... --
[`CMOVE&gt;`](#CMOVE&gt;)	|	c-addr1 c-addr2 u --
[`CMOVE`](#CMOVE)	|		c-addr1 c-addr2 u --
[`CODE`](#CODE)	|		"&lt;spaces&gt;name<space>" --
[`COMPARE`](#COMPARE)	|	c-addr1 u1 c-addr2 u2 -- -1|0|1
[`COMPILE,`](#COMPILE,)	|	xt --
[`CONSTANT`](#CONSTANT)	|	x "&lt;spaces&gt;name<space>" -- ; -- x
[`CONTEXT`](#CONTEXT)	|	-- addr
[`COUNT`](#COUNT)	|		c-addr1 -- c-addr2 u
[`CREATE`](#CREATE)	|	"&lt;spaces&gt;name<space>" -- ; -- addr
[`CR`](#CR)	|		--
[`CUR-XY`](#CUR-XY)	|	-- u1 u2
[`CURRENT`](#CURRENT)	|	-- addr
[`D&gt;F`](#D&gt;F)	|		d -- r
[`D&gt;S`](#D&gt;S)	|		d -- n
[`D&lt;`](#D&lt;)	|		d1 d2 -- flag
[`D*`](#D*)	|		d1|ud1 d2|ud2 -- d3|ud3
[`D+!`](#D+!)	|		d addr --
[`D+`](#D+)	|		d1 d2 -- d3
[`D-`](#D-)	|		d1 d2 -- d3
[`D.R`](#D.R)	|		d +n --
[`D.`](#D.)	|		d --
[`D/MOD`](#D/MOD)	|	d1 d2 -- d3 d4
[`D/`](#D/)	|		d1 d2 -- d3
[`D0&lt;`](#D0&lt;)	|		d -- flag
[`D0=`](#D0=)	|		dx -- flag
[`D2*`](#D2*)	|		d1 -- d2
[`D2/`](#D2/)	|		d1 -- d2
[`D=`](#D=)	|		d1 d2 -- flag
[`DABS`](#DABS)	|		d1 -- d2
[`DBL`](#DBL)	|		-- flag
[`DECIMAL`](#DECIMAL)	|	--
[`DEFER!`](#DEFER!)	|	xt1 xt2 --
[`DEFER@`](#DEFER@)	|	xt1 -- xt2
[`DEFER`](#DEFER)	|		"&lt;spaces&gt;name<space>" -- ; ... -- ...
[`DEFINITIONS`](#DEFINITIONS)	|	--
[`DEPTH`](#DEPTH)	|		-- u
[`DMAX`](#DMAX)	|		d1 d2 -- d3
[`DMIN`](#DMIN)	|		d1 d2 -- d3
[`DMOD`](#DMOD)	|		d1 d2 -- d3
[`DNEGATE`](#DNEGATE)	|	d1 -- d2
[`DOES&gt;`](#DOES&gt;)	|		-- ; ... -- ...
[`DO`](#DO)	|		n1|u1 n2|u2 -- ; C: -- addr do_sys
[`DROP`](#DROP)	|		x --
[`DU&lt;`](#DU&lt;)	|		du1 du2 -- flag
[`DUP&gt;R`](#DUP&gt;R)	|		x -- x ; R: -- x
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
[`F&gt;D`](#F&gt;D)	|		r -- d
[`F&gt;S`](#F&gt;S)	|		r -- n
[`F&lt;`](#F&lt;)	|		r1 r2 -- flag
[`F**`](#F**)	|		r1 r2 -- r3
[`F*`](#F*)	|		r1 r2 -- r3
[`F+`](#F+)	|		r1 r2 -- r3
[`F-`](#F-)	|		r1 r2 -- r3
[`F.`](#F.)	|		r --
[`F/`](#F/)	|		r1 r2 -- r3
[`F0&lt;`](#F0&lt;)	|		r -- flag
[`F0=`](#F0=)	|		r -- flag
[`F=`](#F=)	|		r1 r2 -- flag
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
[`FORGET`](#FORGET)	|	"&lt;spaces&gt;name<space>" --
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
[`INP`](#INP)	|		u1 -- u2
[`INTERPRET`](#INTERPRET)	|	--
[`INVERT`](#INVERT)	|	x1 -- x2
[`IS`](#IS)	|		xt "&lt;spaces&gt;name<space>" --
[`I`](#I)	|		-- n
[`J`](#J)	|		-- n
[`KEY-CLEAR`](#KEY-CLEAR)	|	--
[`KEY?`](#KEY?)	|		-- flag
[`KEY`](#KEY)	|		-- char
[`K`](#K)	|		-- n
[`L&gt;NAME`](#L&gt;NAME)	|	lfa -- nt
[`LASTXT`](#LASTXT)	|	-- xt
[`LEAVE`](#LEAVE)	|		--
[`LITERAL`](#LITERAL)	|	x -- ; -- x
[`LOOP`](#LOOP)	|		-- ; C: addr do_sys --
[`LSHIFT`](#LSHIFT)	|	x1 u -- x2
[`M*/`](#M*/)	|		d1 n1 n2 -- d2
[`M*`](#M*)	|		n1 n2 -- d
[`M+`](#M+)	|		d1 n -- d2
[`MARKER`](#MARKER)	|	"&lt;spaces&gt;name<space>" -- ; --
[`MAX-XY`](#MAX-XY)	|	-- u1 u2
[`MAX`](#MAX)	|		n1 n2 -- n3
[`MD*`](#MD*)	|		d1 n -- d2
[`MIN`](#MIN)	|		n1 n2 -- n3
[`MOD`](#MOD)	|		n1 n2 -- n3
[`MOVE`](#MOVE)	|		c-addr1 c-addr2 u --
[`N&gt;R`](#N&gt;R)	|		n*x n -- ; R: -- n*x n
[`NAME&gt;STRING`](#NAME&gt;STRING)	|	nt -- c-addr u
[`NAME&gt;`](#NAME&gt;)	|		nt -- xt
[`NEGATE`](#NEGATE)	|	n1 -- n2
[`NEXT-CHAR`](#NEXT-CHAR)	|	c-addr1 u1 -- c-addr2 u2 char
[`NFA,`](#NFA,)	|		c-addr u --
[`NIP`](#NIP)	|		x1 x2 -- x2
[`NR&gt;`](#NR&gt;)	|		R: n*x n -- ; -- n*x n
[`NUMBER`](#NUMBER)	|	c-addr u -- n|u|d|ud|r
[`OFF`](#OFF)	|		addr --
[`OF`](#OF)	|		x1 x2 -- x1 or x1 x2 -- ; C: n1 -- orig n2
[`OK`](#OK)	|		"ccc&lt;eol&gt;" --
[`ON`](#ON)	|		addr --
[`OR`](#OR)	|		x1 x2 -- x1|x2
[`OUTPUT-ID`](#OUTPUT-ID)	|	-- 0|fileid
[`OUT`](#OUT)	|		u1 u2 --
[`OVER`](#OVER)	|		x1 x2 -- x1 x2 x1
[`PAD`](#PAD)	|		-- c-addr
[`PAGE`](#PAGE)	|		--
[`PARSE-NAME`](#PARSE-NAME)	|	"&lt;spaces&gt;name<space>" -- c-addr u
[`PARSE-WORD`](#PARSE-WORD)	|	char "&lt;chars&gt;ccc<char>" -- c-addr u
[`PARSE`](#PARSE)	|		char "ccc&lt;char&gt;" -- c-addr u
[`PI/2`](#PI/2)	|		-- r
[`PICK`](#PICK)	|		xu ... x0 u -- xu ... x0 xu
[`PI`](#PI)	|		-- r
[`POSTPONE`](#POSTPONE)	|	"&lt;spaces&gt;name<space>" --
[`PRECISION`](#PRECISION)	|	-- +n
[`QUIT`](#QUIT)	|		... -- ; R: ... --
[`R&gt;`](#R&gt;)	|		R: x -- ; -- x
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
[`S&gt;D`](#S&gt;D)	|		n -- d
[`S&gt;F`](#S&gt;F)	|		n -- r
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
[`S\&gt;S`](#S\&gt;S)	|		c-addr u -- c-addr u
[`THEN`](#THEN)	|		-- ; C: addr orig --
[`THROW`](#THROW)	|		0 -- or ... n -- ... n
[`TIB`](#TIB)	|		-- c-addr u
[`TMP`](#TMP)	|		-- c-addr
[`TO`](#TO)	|		"&lt;spaces&gt;name<space>" -- ; x --
[`TRIM`](#TRIM)	|		c-addr1 u1 char -- c-addr2 u2
[`TRUE`](#TRUE)	|		-- -1
[`TUCK`](#TUCK)	|		x1 x2 -- x2 x1 x2
[`TYPE`](#TYPE)	|		c-addr u --
[`U&gt;`](#U&gt;)	|		u1 u2 -- flag
[`U&lt;`](#U&lt;)	|		u1 u2 -- flag
[`U.R`](#U.R)	|		u +n --
[`U.`](#U.)	|		u --
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
[`VALUE`](#VALUE)	|		x "&lt;spaces&gt;name<space>" -- ; -- x
[`VARIABLE`](#VARIABLE)	|	"&lt;spaces&gt;name<space>" -- ; -- addr
[`VOCABULARY`](#VOCABULARY)	|	"&lt;spaces&gt;name<space>" --
[`WHILE`](#WHILE)	|		x -- ; C: addr sys -- addr orig addr sys
[`WIDTH`](#WIDTH)	|		u --
[`WITHIN`](#WITHIN)	|	x1 x2 x3 -- flag
[`WORDS`](#WORDS)	|		--
[`WORD`](#WORD)	|		char "&lt;chars&gt;ccc<char>" -- c-addr
[`XOR`](#XOR)	|		x1 x2 -- x1^x2
[`[']`](#['])	|		"&lt;spaces&gt;name<space>" -- ; -- xt
[`[CHAR]`](#[CHAR])	|	"&lt;spaces&gt;char" -- ; -- char
[`[COMPILE]`](#[COMPILE])	|	"&lt;space&gt;name<space>" -- ; ... -- ...
[`[`](#[)	|		--
[`\`](#\)	|		"ccc&lt;eol&gt;" --
[`]`](#])	|		--
