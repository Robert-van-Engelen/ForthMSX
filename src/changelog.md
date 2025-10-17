# Changelog and TODO

## Changelog

### v0.9

- this project extends Forth850 located in my GitHub repo at <https://github.com/Robert-van-Engelen/Forth850> 

- add new `RPIX` assembly flag to use the IX register as RP to save code space, making it optional in case the IX register is used by the host system; rewrite all Forth words that use RP in assembly

- add new `JPIY` assembly flag to use the IY register for `jp (iy)` to the `next` routine as before for speed, but making it optional in case the IY register is used by the host system

- add new `AUXR` assembly flag to use auxiliary registers by the interpreter, making it optional in case the aux registers are used by the host system

- add new `SAFE` assembly flag to perform parameter stack under- and overflow checks in Forth loops as before, but making it optional to increase speed

- add new `SAFR` assembly flag to include the non-standard word `?RP` that performs a return stack under- and overflow check, used in `RECURSE`

- add new `STOP` assembly flag to check MSX (CTRL-) STOP key and break to the REPL, making it optional

- add new `UPHI` assembly flag to use the maximum free memory space up to `HIMEM` for the dictionary and stacks (adds only a mere 48 more bytes when combined with `CLEAR100 : MAXFILES=0`), but this prevents returning to BASIC with `BYE` because of BASIC instability by colliding memory usage

- add new `MATH` assembly flag to include single precision floating-point words that call the MSX Math Pack ROM, add Forth 2012 standard Floating-Point words; because floating-point values are stored as doubles, words that move double numbers can be used to operate with floating point values (saves a lot of dictionay memory space)

- add new `REPL` assembly flag to include the Forth interpreter with REPL (read-eval-print loop); disabling this flag makes the Forth system "headless" without interpreter, creating a small compiled-only Forth library without the dictionary; use `MAIN` assembly flag to include a `MAIN` Forth routine as a stand-alone application to run

- add new `FCBN` assembly flag and value to include Forth 2012 standard File Access words that call the MSX-DOS (when available); the number of concurrent open files is limited by the `FCBN` value that pre-allocates a FCB (file control block) and FIB (file input buffer) per open file

- add new `EDIT` assembly flag to use the built-in MSX screen editor (like BASIC) to enter Forth from the console

- add new `XTRA` assembly flag to include Forth Double-Number words `D*` `UMD/MOD` `UD/MOD` `D/MOD` `DMOD` `D/`, include Forth Core words `N>R` `NR>` `S\"` and include `INKEY` `KEY-CLEAR` `S\>S`

- add new `S\>S` to convert a string with `\`-escapes by embedding the codes in place

- add new `S\"` word

- expand assembly macros to include common Forth stack operations amd register saves/restores

- make `REQUIRE` and `REQUIRED` words add a marker word to the dictionary of the form `~filename.ext` when `filename.ext` is loaded to indicate its presence and to conveniently delete the imported definitions when `~filename.ext` is executed (viz. C++ destructor name with leading `~`)

- add new `ANEW` word that executes the `~filename.ext` marker when present in the dictionary to delete all its definitions and then reloads the file with `REQUIRE`; this is useful when the file is changed and the updated definitions should be reloaded from the file

- update `+TO` to also work with `2VALUE`

- add new `OK` word that behaves like a Forth `\`-comment so that typing ENTER at an OK line does not cause errors but allows Forth output to be input again; rewrite `OK` output with nonzero cell depth

- add new `WIDTH` word to switch text mode screen width, supports 80 columns (MSX2)

- add new `MAX-XY` word to return the current screen size

- add new `CUR-XY` word to get the current cursor position

- add new `?LEAVE` word

- add new `CFA=` word for introspecting CFA definitions of execution tokens

- add new `CODE` word to create dictionary entry which must be followed by Z80 code

- add new `SDUP` word to populate a `TMP` counted string copy

- add `OUTPUT-ID` word (a `VALUE` word) to redirect all output to a file or to a device (counter part of `SOURCE-ID`); update `EMIT` and `TYPE` to redirect output so that all output-related words are covered

- add `FILE.FTH` source file with Forth File-Access Extension and `FILES` to list files with size

- add `FACILITY.FTH` source file with Forth Facility words

- add `TOOLS.FTH` source file with Forth Programming Tools words `[DEFINED` `[UNDEFINED]` `[IF]` `[ELSE` `[THEN]`

- add `SEE.FTH` source file to decompile word definitions with the Forth Programming Tools `SEE` word

- add `DUMP.FTH` source file for memory dumping with the Forth Programming-Tools `DUMP` word

- add `#IN` input line number to report parse errors with line numbers; update `REFILL` to reset/increment `#IN`; update `SAVE-INPUT` and `RESTORE-INPUT` to include `#IN`; update `ERROR` to report line number

- improve `N>R` and `NR>` code

- fix missing `D2*` and `D2/` standard words

- fix `M*/` word to be standard compliant, using new `UM*/MOD` with triple cell intermediate result

### v1.0

## TODO

- `READ-LINE` on "CON" device can't work with `READ-FILE` byte-by-byte, because MSX-DOS apparently refuses to cooperate and produce a byte stream; plan use CP/M "console input" 01h instead and do the same for "AUX" input 03h

- add words for MSX VDP graphics

- add words for MSX PSG sound


