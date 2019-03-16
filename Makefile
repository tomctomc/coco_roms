


# =============================================================================
# A09 assembler (modified version included here).  stock version available
#               from: http://www.hermannseib.com/english/opensource.htm
# =============================================================================

A09=./a09 -oNOW -oTXT -oEXP -oNCL

  AUTO_COCO_BUILD= ${A09} -Bauto_tmp -Y$@       cocodefs.asm
   AUTO_BAS_BUILD= ${A09} -Bauto_tmp -Y$@       auto_symbols_coco.asm   bas.asm      -DVERBAS=20
AUTO_EXTBAS_BUILD= ${A09} -Bauto_tmp -Y$@       auto_symbols_bas.asm    extbas.asm   -DVEREXTBAS=20

      BAS10_BUILD= ${A09} -B$@ -L${@:.rom=.lst} auto_symbols_coco.asm   bas.asm      -DVERBAS=10
      BAS11_BUILD= ${A09} -B$@ -L${@:.rom=.lst} auto_symbols_coco.asm   bas.asm      -DVERBAS=11
      BAS12_BUILD= ${A09} -B$@ -L${@:.rom=.lst} auto_symbols_coco.asm   bas.asm      -DVERBAS=12
      BAS13_BUILD= ${A09} -B$@ -L${@:.rom=.lst} auto_symbols_coco.asm   bas.asm      -DVERBAS=13
      BAS20_BUILD= ${A09} -B$@ -L${@:.rom=.lst} auto_symbols_coco.asm   bas.asm      -DVERBAS=20
   EXTBAS10_BUILD= ${A09} -B$@ -L${@:.rom=.lst} auto_symbols_bas.asm    extbas.asm   -DVEREXTBAS=10
   EXTBAS11_BUILD= ${A09} -B$@ -L${@:.rom=.lst} auto_symbols_bas.asm    extbas.asm   -DVEREXTBAS=11
   EXTBAS20_BUILD= ${A09} -B$@ -L${@:.rom=.lst} auto_symbols_bas.asm    extbas.asm   -DVEREXTBAS=20
     DISK10_BUILD= ${A09} -B$@ -L${@:.rom=.lst} auto_symbols_extbas.asm disk.asm     -DVERDISK=10
     DISK11_BUILD= ${A09} -B$@ -L${@:.rom=.lst} auto_symbols_extbas.asm disk.asm     -DVERDISK=11
   SUPBAS20_BUILD= ${A09} -B$@ -L${@:.rom=.lst} auto_symbols_extbas.asm supbas.asm   -DCOCOPAL=0
  SUPBAS20P_BUILD= ${A09} -B$@ -L${@:.rom=.lst} auto_symbols_extbas.asm supbas.asm   -DCOCOPAL=1



# =============================================================================
# mamou assembler
# (available from http://sourceforge.net/projects/toolshed)
# =============================================================================

#MAMOU=mamou -q -mr

#  AUTO_COCO_BUILD= ${MAMOU} cocodefs.asm                                        -sa > tmp && mv tmp $@
#   AUTO_BAS_BUILD= ${MAMOU} auto_symbols_coco.asm   bas.asm      -aVERBAS=20    -sa > tmp && mv tmp $@
#AUTO_EXTBAS_BUILD= ${MAMOU} auto_symbols_bas.asm    extbas.asm   -aVEREXTBAS=20 -sa > tmp && mv tmp $@

#      BAS10_BUILD= ${MAMOU} auto_symbols_coco.asm   bas.asm      -aVERBAS=10    -o$@ -l >${@:.rom=.lst}
#      BAS11_BUILD= ${MAMOU} auto_symbols_coco.asm   bas.asm      -aVERBAS=11    -o$@ -l >${@:.rom=.lst}
#      BAS12_BUILD= ${MAMOU} auto_symbols_coco.asm   bas.asm      -aVERBAS=12    -o$@ -l >${@:.rom=.lst}
#      BAS13_BUILD= ${MAMOU} auto_symbols_coco.asm   bas.asm      -aVERBAS=13    -o$@ -l >${@:.rom=.lst}
#      BAS20_BUILD= ${MAMOU} auto_symbols_coco.asm   bas.asm      -aVERBAS=20    -o$@ -l >${@:.rom=.lst}
#   EXTBAS10_BUILD= ${MAMOU} auto_symbols_bas.asm    extbas.asm   -aVEREXTBAS=10 -o$@ -l >${@:.rom=.lst}
#   EXTBAS11_BUILD= ${MAMOU} auto_symbols_bas.asm    extbas.asm   -aVEREXTBAS=11 -o$@ -l >${@:.rom=.lst}
#   EXTBAS20_BUILD= ${MAMOU} auto_symbols_bas.asm    extbas.asm   -aVEREXTBAS=20 -o$@ -l >${@:.rom=.lst}
#     DISK10_BUILD= ${MAMOU} auto_symbols_extbas.asm disk.asm     -aVERDISK=10   -o$@ -l >${@:.rom=.lst}
#     DISK11_BUILD= ${MAMOU} auto_symbols_extbas.asm disk.asm     -aVERDISK=11   -o$@ -l >${@:.rom=.lst}
#   SUPBAS20_BUILD= ${MAMOU} auto_symbols_extbas.asm supbas.asm   -aCOCOPAL=0    -o$@ -l >${@:.rom=.lst}
#  SUPBAS20P_BUILD= ${MAMOU} auto_symbols_extbas.asm supbas.asm   -aCOCOPAL=1    -o$@ -l >${@:.rom=.lst}
# =============================================================================

all: verify

clean:
	rm -f *.lst auto_* *.rom a09 joe* xaa xab xac xad

a09: a09.c
	gcc -o a09 a09.c

cocodefs.asm: a09
	
auto_symbols_coco.asm: cocodefs.asm
	${AUTO_COCO_BUILD}

auto_symbols_bas.asm: auto_symbols_coco.asm
	${AUTO_BAS_BUILD}

auto_symbols_extbas.asm: auto_symbols_bas.asm
	${AUTO_EXTBAS_BUILD}

bas10.rom: auto_symbols_coco.asm bas.asm
	${BAS10_BUILD}

bas11.rom: auto_symbols_coco.asm bas.asm
	${BAS11_BUILD}

bas12.rom: auto_symbols_coco.asm bas.asm
	${BAS12_BUILD}

bas13.rom: auto_symbols_coco.asm bas.asm
	${BAS13_BUILD}

bas20.rom: auto_symbols_coco.asm bas.asm
	${BAS20_BUILD}

extbas10.rom: auto_symbols_bas.asm extbas.asm
	${EXTBAS10_BUILD}

extbas11.rom: auto_symbols_bas.asm extbas.asm
	${EXTBAS11_BUILD}

extbas20.rom: auto_symbols_bas.asm extbas.asm
	${EXTBAS20_BUILD}

disk10.rom: auto_symbols_extbas.asm extbas.asm
	${DISK10_BUILD}

disk11.rom: auto_symbols_extbas.asm extbas.asm
	${DISK11_BUILD}

supbas20.rom: auto_symbols_extbas.asm supbas.asm
	${SUPBAS20_BUILD}

supbas20p.rom: auto_symbols_extbas.asm supbas.asm
	${SUPBAS20P_BUILD}

#
# coco3.rom is just a single 32k concatentation of:
#
#     extbas20 8000-9FFF  (8k)
#        bas20 A000-BFFF  (8k)
#     supbas20 C000-FFFF (16k)
#
coco3.rom: bas20.rom extbas20.rom supbas20.rom
	cat extbas20.rom bas20.rom supbas20.rom >$@

#
# likewise for coco3p.rom (just substitute supbas20p.rom)
#
coco3p.rom: bas20.rom extbas20.rom supbas20p.rom
	cat extbas20.rom bas20.rom supbas20p.rom >$@


#
# compare our assembled rom binaries with known SHA1 hashes from
# http://www.mess.org (mess -listxml output)
#
verify: bas10.rom bas11.rom bas12.rom bas13.rom extbas10.rom extbas11.rom disk10.rom disk11.rom coco3.rom coco3p.rom
	@echo "verifying all roms..."
	@# ============== "official" mess hashes (taken directly from `mess -listxml`)
	@sha1sum bas10.rom       | grep "1f08455cd48ce6a06132aea15c4778f264e19539" >/dev/null && echo "bas10.rom      OK" || echo "bas10.rom      ***** BAD SHA1 *****"
	@sha1sum bas11.rom       | grep "cecb7c24ff1e0ab5836e4a7a8eb1b8e01f1fded3" >/dev/null && echo "bas11.rom      OK" || echo "bas11.rom      ***** BAD SHA1 *****"
	@sha1sum bas12.rom       | grep "0f14dc46c647510eb0b7bd3f53e33da07907d04f" >/dev/null && echo "bas12.rom      OK" || echo "bas12.rom      ***** BAD SHA1 *****"
	@sha1sum bas13.rom       | grep "28b92bebe35fa4f026a084416d6ea3b1552b63d3" >/dev/null && echo "bas13.rom      OK" || echo "bas13.rom      ***** BAD SHA1 *****"
	@sha1sum extbas10.rom    | grep "8aa58f2eb3e8bcfd5470e3e35e2b359e9a72848e" >/dev/null && echo "extbas10.rom:  OK" || echo "extbas10.rom:  ***** BAD SHA1 *****"
	@sha1sum extbas11.rom    | grep "ad927fb4f30746d820cb8b860ebb585e7f095dea" >/dev/null && echo "extbas11.rom:  OK" || echo "extbas11.rom:  ***** BAD SHA1 *****"
	@sha1sum disk10.rom      | grep "04115be3f97952b9d9310b52f806d04f80b40d03" >/dev/null && echo "disk10.rom:    OK" || echo "disk10.rom:    ***** BAD SHA1 *****"
	@sha1sum disk11.rom      | grep "10bdc5aa2d7d7f205f67b47b19003a4bd89defd1" >/dev/null && echo "disk11.rom:    OK" || echo "disk11.rom:    ***** BAD SHA1 *****"
	@sha1sum coco3.rom       | grep "e0d82953fb6fd03768604933df1ce8bc51fc427d" >/dev/null && echo "coco3.rom      OK" || echo "coco3.rom      ***** BAD SHA1 *****"
	@sha1sum coco3p.rom      | grep "631e383068b1f52a8f419f4114b69501b21cf379" >/dev/null && echo "coco3p.rom     OK" || echo "coco3p.rom     ***** BAD SHA1 *****"
	@# ============== self-generated hashes (these files are actually just portions of coco3.rom and coco3p.rom above)
	@sha1sum bas20.rom       | grep "09015df53738be4ae9e50a84f26a19b8582c05ff" >/dev/null && echo "bas20.rom      OK" || echo "bas20.rom      ***** BAD SHA1 *****"
	@sha1sum extbas20.rom    | grep "d322cbd9f959bdc5f2cba7baac37071450946b5e" >/dev/null && echo "extbas20.rom:  OK" || echo "extbas20.rom:  ***** BAD SHA1 *****"
	@sha1sum supbas20.rom    | grep "9a05652fd80ab87219102800fe6a44a4ecacd8c3" >/dev/null && echo "supbas20.rom:  OK" || echo "supbas20.rom:  ***** BAD SHA1 *****"
	@sha1sum supbas20p.rom   | grep "1953f7c33f0588ac975212d8ec3c6faee4ebad39" >/dev/null && echo "supbas20p.rom: OK" || echo "supbas20p.rom: ***** BAD SHA1 *****"



