# Disk Tools

**dsktool**: command line tool to manage MSX DSK image files.  Supports FAT12 and ADVH filesystem.  Based on *Ricardo Bittencourt* and *Tony Cruise* work.

Sourced from: <https://github.com/nataliapc/MSX_devs>

Updated to compile on MacOS and Linux withotu errors.

## Build


```
$ make
```

Then copy `dsktool` to your bin directory or to a location to run it.

## Usage

```
DskTool v1.40 (C) 1998 by Ricardo Bittencourt
Utility to manage MSX DOS 1.0 diskette images (3.5"360/720Kb).
(2010) Updated by Tony Cruise
(2017-2019) Updated by NataliaPC
This file is under GNU GPL, read COPYING for details

Usage: dsktool <command> [option] <DSK_file> [files]

Commands:
        c N   Create a floppy image [where N:360,720,1440,2880]
        i     Show floppy info
        l[h]  List contents of .DSK
        e[h]  Extract files from .DSK
        a[h]  Add files to .DSK
        d     Delete files from .DSK
        f     File clusters info
        o[h]  Get file info for a raw disk offset

    Note: optional [H] suffix change to ADVH filesystem mode.

Examples:
        dsktool c 360 TALKING.DSK
        dsktool i TALKING.DSK
        dsktool l TALKING.DSK
        dsktool lh DRAGON.DSK
        dsktool e TALKING.DSK FUZZ*.*
        dsktool a TALKING.DSK MSXDOS.SYS COMMAND.COM
        dsktool ah DRAGON.DSK M*.COM
        dsktool d TALKING.DSK *.BAS *.BIN
        dsktool f TALKING.DSK FILE.EXT
        dsktool o TALKING.DSK 307712
```

## Manual

See [readme.txt](readme.txt)
