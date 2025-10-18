#!/bin/sh
#
# ./words.sh

echo 'Forth words'
echo '-----------'
echo
cat <<'END'
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
END
echo
awk '/^;\+? ([^(][^ ]*|\()\t/,/^$/ { print }' forth.asm \
      | sed -E 's/^;    /    /' \
      | sed -E 's/^;(\+? |		)//' \
      | sed -E 's/"<([a-z]+)>/"\&lt;\1\&gt;/' \
      | sed -E 's/<([a-z]+)>"/\&lt;\1\&gt;"/' \
      | sed -E 's/^([^	]+)[	]+(.*)/___\n### \1\n_\2_\n/' \
      | sed -E 's/^;//'
#echo
#echo 'Non-standard extra words'
#echo '------------------------'
#echo
#awk '/^;\+ [^ ]+\t/,/^$/ { print }' forth.asm \
#      | sed -E 's/^;    /    /' \
#      | sed -E 's/^;(\+ |		)//' \
#      | sed -E 's/"<([a-z]+)>/"\&lt;\1\&gt;/' \
#      | sed -E 's/<([a-z]+)>"/\&lt;\1\&gt;"/' \
#      | sed -E 's/^([^	]+)[	]+(.*)/___\n### \1\n_\2_\n\n/' \
#      | sed -E 's/^;//'
echo
echo
echo 'File-access words'
echo '--------------------------'
echo
awk '/^;\/ [^(][^ ]*\t/,/^$/ { print }' forth.asm \
      | sed -E 's/^;    /    /' \
      | sed -E 's/^;(\/ |		)//' \
      | sed -E 's/"<([a-z]+)>/"\&lt;\1\&gt;/' \
      | sed -E 's/<([a-z]+)>"/\&lt;\1\&gt;"/' \
      | sed -E 's/^([^	]+)[	]+(.*)/___\n### \1\n_\2_\n/' \
      | sed -E 's/^;//'
echo
echo
echo 'MSX Math-Pack BCD floating-point math words'
echo '-------------------------------------------'
echo
echo 'Floating-point literals are specified in scientific notation with an exponent,'
echo 'even when the exponent is zero.  For example, `1E0` is floating-point 1,'
echo 'but also `1e` without exponent digits following the lower-case `e`.'
echo
echo 'Because MSX BASIC floating-point values may be specified with a trailing `!` or'
echo '`#`, ForthMSX supports this notation also, e.g. `1#` is floating-point 1.'
echo 'Likewise, `1d` is floating-point 1, and perhaps strangely, `&h1` also.'
echo
echo 'Floating point values are stored as doubles on the stack.  Double-moving Words,'
echo 'such as `2DUP`, can be used to manipulate floats.  Floats can be stored in'
echo '`2CONSTANT`, `2VARIABLE`, and in `2VALUE` assigned with `TO` but not with `+TO`.'
echo
echo 'Beware that `HEX` prevents inputting floats and garbles the output of floats.'
echo
awk '/^;\. [^(][^ ]*\t/,/^$/ { print }' forth.asm \
      | sed -E 's/^;    /    /' \
      | sed -E 's/^;(\. |		)//' \
      | sed -E 's/"<([a-z]+)>/"\&lt;\1\&gt;/' \
      | sed -E 's/<([a-z]+)>"/\&lt;\1\&gt;"/' \
      | sed -E 's/^([^	]+)[	]+(.*)/___\n### \1\n_\2_\n/' \
      | sed -E 's/^;//'
echo
echo
echo 'Alternative IEEE-754 floating-point math words (not enabled)'
echo '------------------------------------------------------------'
echo
echo 'Floating-point literals must be specified in scientific notation with an exponent,'
echo 'even when the exponent is zero.  For example, `1E0` is floating point 1,'
echo 'but also `1e` without exponent digits following the lower-case `e`.'
echo
echo 'Floating point values are stored as doubles on the stack.  Double-moving Words,'
echo 'such as `2DUP`, can be used to manipulate floats.  Floats can be stored in'
echo '`2CONSTANT`, `2VARIABLE`, and in `2VALUE` assigned with `TO` but not with `+TO`.'
echo
echo 'Beware that `HEX` prevents inputting floats and garbles the output of floats.'
echo
awk '/^;= [^(][^ ]*\t/,/^$/ { print }' forth.asm \
      | sed -E 's/^;    /    /' \
      | sed -E 's/^;(= |		)//' \
      | sed -E 's/"<([a-z]+)>/"\&lt;\1\&gt;/' \
      | sed -E 's/<([a-z]+)>"/\&lt;\1\&gt;"/' \
      | sed -E 's/^([^	]+)[	]+(.*)/___\n### \1\n_\2_\n/' \
      | sed -E 's/^;//'

echo
echo
echo 'Alphabetic list of words'
echo '------------------------'
echo
echo 'word | stack'
echo '---- | -----'
awk '/^;[+.]? [^(][^ ]*\t/ { print }' forth.asm \
      | sed -E 's/^;[+.]? //' \
      | sed -E 's/"<([a-z]+)>/"\&lt;\1\&gt;/' \
      | sed -E 's/<([a-z]+)>"/\&lt;\1\&gt;"/' \
      | sed -E 's/^[^	]+/[`&`](#&)\t|/' \
      | sort -u
