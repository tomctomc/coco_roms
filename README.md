## Custom Tandy Color Computer 3 ROM source code

Originally forked from [tomctomc/coco_roms](https://github.com/tomctomc/coco_roms), all non-coco3 code has been stripped and the sources modified to be compatible with [LWASM](http://www.lwtools.ca/manual/manual.html#AEN62).

This repo is more of an 8-bit assembly playground for me. I doubt these mods will be generally useful, but feel free to give it a try if you're interested. I've tried to keep existing code intact as much as possible to hopefully avoid breaking compatibility with other software that mucks around in the standard ROMs, but I make no guarantees.

### Modifications so far

- Custom keymapping! You can switch between QWERTY and Dvorak by poking a 0 (QWERTY) or a 2 (DVORAK) to address `0xFB6F`. For example:

```text
POKE &HFB6F,2 ' switch to Dvorak
POKE &HFB6F,0 ' switch to QWERTY
```

This address will change as I clean up the code. I've duplicated much of the original key input logic in this first pass, and I'll be clearing that out at some point.
