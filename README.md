
## TRS-80 Color Computer ROM source code

This is a collection of 6809 assembly language sources (and the source code for a 6809 assembler) which allow one to fully build all "rom" images for variations of the TRS-80 Color Computer.  The built rom images can be used with emulators such as mame [mame](http://www.mamedev.org) that can fully simulate a running TRS-80 Color computer.  The Makefile includes expected SHA-1 hashes for the roms and compares them all after assembling to ensure they match bit-for-bit the original contents of the machine ROMs.

Most of this has been collected from internet sources.  Of particular note is the ["Unravelled" series of books](http://techheap.packetizer.com/computers/coco/unravelled_series/) which was the original source (pun intended) for most of the assembly code.


The files here are:

### Files

| file         | description                              |                                                                      |
| ------------ | ---------------------------------------- | -------------------------------------------------------------------- |
|              |                                          |                                                                      |
| a09.c        | a 6809 assembler                         | slightly modified from the original found [here](http://www.hermannseib.com/english/opensource.htm) |
| Makefile     | instructions for building via make       | (just run "make" to build all rom version permutations)              |
|              |                                          |                                                                      |
| cocodefs.asm | symbol definitions for Color Computer    | (memory map and constants - used with assembling all other files)    |
|              |                                          |                                                                      |
| bas.asm      | assembly source for Color Basic          | (VERBAS can be set to 10,11,12,13,20 to assemble different versions) |
| extbas.asm   | assembly source for Exended Color Basic  | (VEREXTBAS can be set to 10,11,20  to assemble different versions)   |
| disk.asm     | assembly source for Disk Basic           | (VERDISK can be set to 10,11  to assemble different versions)        |
| supbas.asm   | assembly source for Super Color Basic    | (version 2.0)                                                        |
|              |                                          |                                                                      |
| README.md    | this README file                         | you are here.                                                        |



When you run make, by default the a09 assembler is compiled and is then used to build the rom files.  After this has been done, you should have all of the following files (named as specified for use with [mame](http://www.mamedev.org)).  The Makefile includes expected SHA-1 hashes for the roms and compares them all after assembling.


### Built Files

| file                 | description                              |                                                                      |
| -------------------- | ---------------------------------------- | -------------------------------------------------------------------- |
|                      |                                          |                                                                      |
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
| coco3p.rom           | Full Color Computer 3 (PAL) ROM image    | (concatenation of extbas20.rom, bas20.rom, supbas20.rom)             |
|                      |                                          |                                                                      |
| \*.lst               | assembly listings                        | for each rom, the full listing wih bytecode is provided (for information only) |
|                      |                                          |                                                                      |
| auto\_\*             | preprocessed files                       | (just used during assembly - you can delete them)                    |
|                      |                                          |                                                                      |
| a09                  | the compiled 6809 assembler itself       |                                                                      |
