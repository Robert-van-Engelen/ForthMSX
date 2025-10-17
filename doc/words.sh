#!/bin/sh
#
# ./words.sh

echo 'Forth words'
echo '-----------'
echo
cat <<'END'
Forth words operate on the parameter stack, the return stack, and may parse
Forth words or delimited sequences of characters from the input buffer.
Parameter stack changes by a word such as `ROT` are indicated by a `--`:

**ROT**
<br>
_x1 x2 x3 -- x2 x3 x1_

On the left side of `--` we have three single-cell values are on the parameter
stack with _x3_ the top-of-stack (TOS) value.  On the right side we have three
single-cell values that are returned by ROT, replacing the input values on the
stack in a rotated order with _x1_ the new TOS, _x3_ the second-on-stack (2OS)
and _x2_ the third-on-stack (3OS).

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
END
echo
awk '/^;\+? [^ ]+\t/,/^$/ { print }' forth.asm \
      | sed -E 's/^;    /    /' \
      | sed -E 's/^;(\+? |		)//' \
      | sed -E 's/^([^	]+)[	]+(.*)/___\n### \1\n_\2_\n<br>/' \
      | sed -E 's/^;//'
#echo
#echo 'Non-standard extra words'
#echo '------------------------'
#echo
#awk '/^;\+ [^ ]+\t/,/^$/ { print }' forth.asm \
#      | sed -E 's/^;    /    /' \
#      | sed -E 's/^;(\+ |		)//' \
#      | sed -E 's/^([^	]+)[	]+(.*)/___\n### \1\n_\2_\n<br>/' \
#      | sed -E 's/^;//'
echo
echo
echo 'File-access words'
echo '--------------------------'
echo
awk '/^;\/ [^ ]+\t/,/^$/ { print }' forth.asm \
      | sed -E 's/^;    /    /' \
      | sed -E 's/^;(\/ |		)//' \
      | sed -E 's/^([^	]+)[	]+(.*)/___\n### \1\n_\2_\n<br>/' \
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
awk '/^;\. [^ ]+\t/,/^$/ { print }' forth.asm \
      | sed -E 's/^;    /    /' \
      | sed -E 's/^;(\. |		)//' \
      | sed -E 's/^([^	]+)[	]+(.*)/___\n### \1\n_\2_\n<br>/' \
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
awk '/^;= [^ ]+\t/,/^$/ { print }' forth.asm \
      | sed -E 's/^;    /    /' \
      | sed -E 's/^;(= |		)//' \
      | sed -E 's/^([^	]+)[	]+(.*)/___\n### \1\n_\2_\n<br>/' \
      | sed -E 's/^;//'

echo
echo
echo 'Alphabetic list of words'
echo '------------------------'
echo
echo 'word | stack'
echo '---- | -----'
awk '/^;[+.]? [^ ]+\t/ { print }' forth.asm \
      | sed -E 's/^;[+.]? //' \
      | sed -E 's/^[^	]+/[`&`](#&)\t|/' \
      | sort -u
