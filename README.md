
## TRS-80 Color Computer ROM source code

This is a collection of 6809 assembly language sources (and the source code for a 6809 assembler) which allow one to fully build all "rom" images for variations of the [TRS-80 Color Computer](http://en.wikipedia.org/wiki/TRS-80_Color_Computer).  The built rom images can be used with emulators such as [mame](http://www.mamedev.org) that can fully simulate a running TRS-80 Color Computer.

Most of this has been collected from internet sources.  Of particular note is the classic "Unravelled" series of [books](http://techheap.packetizer.com/computers/coco/unravelled_series/) which was the origin of most of the assembly code.


### Provided Files

| file         | description                              |                                                                      |
| ------------ | ---------------------------------------- | -------------------------------------------------------------------- |
| a09.c        | a 6809 assembler                         | slightly modified from the original found [here](http://www.hermannseib.com/english/opensource.htm) |
| Makefile     | instructions for building via make       | (just run "make" to build all rom version permutations)              |
| cocodefs.asm | symbol definitions for Color Computer    | (memory map and constants - used during assembly)                    |
| bas.asm      | assembly source for Color Basic          | (can build versions 1.0, 1.1, 1.2, 1.3, and 2.0)                     |
| extbas.asm   | assembly source for Exended Color Basic  | (can build versions 1.0, 1.1, and 2.0)                               |
| disk.asm     | assembly source for Disk Basic           | (can build versions 1.0 and 1.1)                                     |
| supbas.asm   | assembly source for Super Color Basic    | (can build version 2.0)                                              |
| README.md    | this README file                         | you are here.                                                        |


This was all put together under linux, but it should be straightforward enough to build everything on other platforms.  For windows, [msys](http://www.mamedev.org/tools/), [cygwin](http://www.cygwin.com), or [wsl](http://www.microsoft.com/en-us/p/ubuntu/9nblggh4msv6) may be helpful.

When you run `make` the a09 assembler is compiled and is then used to build the rom files.  Afterwards, you should have all of the following files (named as specified for use with [mame](http://www.mamedev.org)).  The Makefile includes expected SHA-1 hashes for the roms and compares them all (via "sha1sum") after assembling to ensure they match bit-for-bit the original contents of the machine ROMs.


### Built Files

| file                 | description                              |                                                                      |
| -------------------- | ---------------------------------------- | -------------------------------------------------------------------- |
| bas10.rom            | Color Basic 1.0                          | $A000 to $BFFF in TRS-80 rom                                         |
| bas11.rom            | Color Basic 1.1                          | $A000 to $BFFF in TRS-80 rom                                         |
| bas12.rom            | Color Basic 1.2                          | $A000 to $BFFF in TRS-80 rom                                         |
| bas13.rom            | Color Basic 1.3                          | $A000 to $BFFF in TRS-80 rom                                         |
| bas20.rom            | Color Basic 2.0                          | $A000 to $BFFF in TRS-80 rom                                         |
| extbas10.rom         | Extended Color Basic 1.0                 | $8000 to $9FFF in TRS-80 rom                                         |
| extbas11.rom         | Extended Color Basic 1.1                 | $8000 to $9FFF in TRS-80 rom                                         |
| extbas20.rom         | Extended Color Basic 2.0                 | $8000 to $9FFF in TRS-80 rom                                         |
| disk10.rom           | Disk Basic 1.0                           | $C000 to $DFFF in TRS-80 rom                                         |
| disk11.rom           | Disk Basic 1.1                           | $C000 to $DFFF in TRS-80 rom                                         |
| supbas20.rom         | Super Basic 2.0                          | $C000 to $FFFF in TRS-80 rom                                         |
| supbas20p.rom        | Super Basic 2.0 (pal)                    | $C000 to $FFFF in TRS-80 rom                                         |
| coco3.rom            | Full Color Computer 3 ROM image          | (concatenation of extbas20.rom, bas20.rom, supbas20.rom)             |
| coco3p.rom           | Full Color Computer 3 (PAL) ROM image    | (concatenation of extbas20.rom, bas20.rom, supbas20p.rom)            |
| \*.lst               | assembly listings                        | full listings for each assembled rom (for information only)          |
| auto\_\*             | preprocessed files                       | (just used during assembly - you can delete them)                    |
| a09                  | the compiled 6809 assembler itself       |                                                                      |


Running `make clean` will remove all the built files.
