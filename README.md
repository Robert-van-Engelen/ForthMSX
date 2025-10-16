# ForthMSX - a Forth 2012 standard system for MSX

The goal of this ongoing project is to implement a "wicked fast" and complete Forth 2012 standard system for MSX.

This repo will also include a complete Forth manual in the near future, similar to the Forth manual I wrote for [Forth500](https://github.com/Robert-van-Engelen/Forth500).

If you don't have an MSX machine or just want to try out ForthMSX, then visit [WebMSX](http://webmsx.org).  To run ForthMSX in WebMSX, click the WebMSX floppy drive icon on the bottom-left, then "Import Files to Disk" and select the `forth.bin` file or small `forthx.bin` file located in your forked copy of this project:

![ForthMSX](img/forthmsx1.png)

To run ForthMSX in WebMSX:

    clear 100,&h8400
    bload "forth.bin",r

Once you've been greeted by the Forth system, type `80 width` and ENTER to switch to 80 characters per line.  Type `words` and ENTER to list all Forth words, one screenful at a time (press a key to continue).

![ForthMSX](img/forthmsx2.png)

The Forth screen editor is the same as the BASIC screen editor.  So you can use the same key combinations and ways to enter Forth commands, edit them, and rerun them as desired.

ForthMSX uses the available 32K BASIC RAM without additional RAM slots.  This supports all 64K MSX systems.  In the future, it makes perhaps more sense to release a version that runs Forth in certain RAM slots concurrently to BASIC/MSX-DOS/Nextor.

With the full version `forth.bin` you can extend Forth by loading Forth source code and try some examples.  Locate the `FILES.FTH` source [`src/FILES.FTH`](src/FILES.FTH) and import this file to disk in WebMSX.  Then type `require files.fth` in ForthMSX to load and compile Forth files definitions (takes about a second).  Then type `files`:

![ForthMSX](img/forthmsx3.png)

The [examples](examples) directory of this project includes a prime number sieve.  Import `PRIMES.FTH` to disk.  Type `require primes.fth` and `1000 sieve`:

![ForthMSX](img/forthmsx3.png)

To delete all definitions loaded from a file, such as `PRIMES.FTH`, type `~primes.fth` (like a C++ destructor uses `~`).

If a Forth source file is changed, then reload it with `anew primes.fth` to delete all its definitions first and compile it again.
