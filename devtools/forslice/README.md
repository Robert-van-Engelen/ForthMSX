# Headless Forth program slicer

**forslice** is a headless Forth program slicer to optimize, obfuscate, shrink,
and relocate Forth compiled binary programs.  Slicing a Forth program removes
all unused Forth words from the binary.  A headless Forth program slice is a
binary with no interactive REPL (no read-eval-print-loop) and no dictionary
with identifyable Forth words.  

## Build

```
$ cc -o forslice forslice.c
```

## Usage

```
forslice [options] INFILE.BIN [OUTFILE.BIN]
```

Slice ForthMSX saved-state binary INFILE.BIN by removing unused words to create
a headless program slice saved to OUTFILE.BIN or to SLICE.BIN when OUTFILE.BIN
is not specified.  Unused words are those that cannot be reached on any
execution path from MAIN or from the word specified with option `-main` instead
of MAIN.  Forslice saves a log file forslice.log in the working directory.
Forslice requires the forth.rel file located in the working directory to
relocate the Forth internals.  When warnings are given, it may be necessary to
specify a words info file with option `-words`.

The options are:

* `-repl`              include REPL interpreter and dictionary (not headless)
* `-break`             disable (CTRL-)STOP break
* `-main <name>`       run `<name>` as the main program, default name is MAIN
* `-start <addr>`      new binary start address to bload the program slice
* `-words <file.inf>`  use words info file to assist slicing and relocation

If MAIN or the word specified by `-main` is missing, then REPL will be run.

A words info file contains a list of Forth words with an optional info field
separated by spaces and/or newlines.  For example, we can specify some words
that we want to include in the slice as well as all words that those words
depend on to run:

```
REPL
words .s PAGE
```

Including the first line is the same as specifying option `-repl`.  ForthMSX
word names are case insensitive.

An info field of the form `{...}` is used for `VAR`, `VAL` and `2VAL` words and
for words defined with `CREATE` `DOES>`.  The info field must not contain
spacing and specifies a list of offsets into the data of those words where
Forth addresses (pointers to data) are located that must be relocated properly
for the program slice to run reliably.

For example, words created by `MARKER` and `VOCABULARY` have pointers in their
data at certain byte-aligned offsets from their pfa (parameter field address
given by `>BODY`):

```
MARKER {0,2,4}
VOCABULARY {0,4}
```

These pointers at the specified offsets must remain valid after program
relocation.  Therefore, these two lines are required to properly slice and
relocate all `MARKER`-defined words and `VOCABULARY`-defined words in the
sliced saved state, when these are used in the slice.  This is not necessary
When none are used in the slice.  Check the saved forslice.log for details.

## Example

When using [WebMSX](http://webmsx.org), load and run FORTH.BIN in Drive A from
BASIC as follows:

```
clear 100,&h8400
bload "forth.bin",r
```

Add `examples/STARS.FTH` to Disk A and load the `STARS.FTH` example:

```
require stars.fth
```

Save the program state as a binary `STARS.BIN` to Drive A:


```
require save.fth
save stars.bin
```

When using WebMSX, click Save Disk Image of Drive A to save the A disk as a
.dsk image to your computer and rename to Disk.dsk.  Extract `STARS.BIN` using
`dsktool` (conveniently located in devtools/dsktool):

```
./dsktool e Disk.dsk STARS.BIN
```

Slice the `STARS.BIN` saved state to run `twinkle` as the main program:

```
./forslice -main twinkle STARS.BIN NEWSTARS.BIN
```

The `NEWSTARS.BIN` binary file is a headless self-contained small binary that
runs `twinkle`.  It is loaded with `bload "newstars.bin",r` and run at address
`&H8400`.  Space to run it should be allocated with `clear 100,&h8400`.
Because it is so small with only about 1200 bytes of code and needing only 1K
memory on top of that for the stacks and buffers, we may want to move it up to
a higher address to run it there.  Typical is to use `&Hc000` as the start
address:

```
./forslice -start 0xc000 -main twinkle STARS.BIN NEWSTARS.BIN
```

Note that `0xc000` in C is `&hc000` in BASIC.

Pressing CTRL-STOP while the program runs terminates the program to return to
BASIC.  To disable CTRL-STOP, use option `-break`:

```
./forslice -break -start 0xc000 -main twinkle STARS.BIN NEWSTARS.BIN
```

This necessitates a reset to terminate the program.

There are some important caveats to create headless program slices.  We should
not ignore `forslice` warnings:

```
analyzing core... 5 warnings logged in forslice.log
```

These warnings are present because the slicer could not figure out if certain
Forth addresses (pointers) are used that must be relocated.  The forslice.log
shows warnigns for all `MARKER` and `VOCABULARY` words and for `seed` and
`star`.  Both `seed` and `star` don't have pointers in their data.  But
`MARKER` and `VOCABULARY` words do.  The warnings are resolved by creatign a
new fle `STARS.INF` with four lines:

```
MARKER {0,2,4}
VOCABULARY {0,4}
seed {}
star {}
```

Now the warnings are gone when we slice with option `-words` with the word info
file specified:

```
./forslice -break -start 0xc000 -main twinkle -words STARS.INF STARS.BIN NEWSTARS.BIN
```

What if we want to keep some words and the REPL, for example to debug
interactively?

The sliced binary includes only the Forth words required by the program and by
the REPL.  Most other words are removed, such as `require`.  To retain When
developing headless stand-alone programs, it may be desirable to interactively
debug the program which can be done before saving it as a saved state with
`save` to slice the binary with `forslice`.  However, it is also possible to
slice the binary while keeping the REPL and Forth dictionary.  This allows you
to interactively run the program from the REPL.  To keep the REPL and
dictionary, use option `-repl` without option `-main`:

```
./forslice -repl -start 0xc000 -words STARS.INF STARS.BIN NEWSTARS.BIN
```

Other words such as `require` can be added to the words info file.  To debug
we want to use a different `BUGSTARS.INF` file to specifically create a binary
file for interactive debugging, where `BUGSTARS.INF` contains:

```
MARKER {0,2,4}
VOCABULARY {0,4}
seed {}
star {}
require
repl
```

Adding `REPL` to the words info file is the same as option `-repl`.

```
./forslice -start 0xc000 -words BUGSTARS.INF STARS.BIN NEWSTARS.BIN
```

## Limitations

The following limitations should be taken into account when writing ForthMSX
programs to slice:

problem | warning? | example | solution
------- | -------- | ------- | --------
words created with `CONSTANT` and `2CONSTANT` cannot contain pointers to or into Forth words | no | `HERE CONSTANT top` | use `VALUE` and `2VALUE` instead, then add an entry to the words info file, e.g. `top {0}`
words created with `VALUE` and `2VALUE` that contain pointers to or into Forth words | yes | `HERE VALUE top` | add an entry to the words info file, e.g. `top {0}` means that `top` has a pointer at offset 0 in its body, whereas `top {}` means that it has not
words created with `VARIABLE`,  `2VARIABLE` and `CREATE` without `DOES>` that contain pointers to or into Forth words | only for the first cell | `CREATE data addr1 , 0 , addr2 ,` | add an entry to the words info file, e.g. `data {0,4}` means that when `CREATE data` is populated with values it has pointers at offsets 0 and 4 in its body, whereas `data {}` means it has no pointers
words created with `CREATE` with `DOES>` that contain pointers to or into Forth words | only for the first cell | `VOCABULARY project` | add an entry to the words info file for the *creating word*, i.e. the word that executes `CREATE ... DOES>`, e.g. `MARKER` and `VOCABULARY` are creating words so the entries `MARKER {0,2,4}` and `VROCABULARY {0,4}` suffice to add to the words info file
evaluated literals in word definitions that are pointers | yes | `: foo [ ' bar ] LITERAL CATCH ERROR ;` | use `[']` instead, e.g. `: foo ['] bar CATCH ERROR ;`

Machine `CODE` words should not use `jp` jumps or `call` calls to other Forth
words.  However, for the ForthMSX assembly code internals the situation is
different, because the assembler's forth.rel file has all of the necessary
relocation information about absolute jumps and calls to relocate; in addition,
forslice also supports a final last unconditional `jr` relative jump to another
Forth word that will be analyzed and adjusted during relocation.

