# =============================================================================
# A09 assembler (modified version included here).  stock version available
#               from: http://www.hermannseib.com/english/opensource.htm
# =============================================================================

BUILD_DIR := build

A09=$(BUILD_DIR)/a09 -oNOW -oTXT -oEXP -oNCL

  AUTO_COCO_BUILD= ${A09} -B$(BUILD_DIR)/auto_tmp -Y$@       cocodefs.asm
   AUTO_BAS_BUILD= ${A09} -B$(BUILD_DIR)/auto_tmp -Y$@       $(BUILD_DIR)/auto_symbols_coco.asm   bas.asm
AUTO_EXTBAS_BUILD= ${A09} -B$(BUILD_DIR)/auto_tmp -Y$@       $(BUILD_DIR)/auto_symbols_bas.asm    extbas.asm

       BAS20_BUILD= ${A09} -B$@ -L${@:.rom=.lst} $(BUILD_DIR)/auto_symbols_coco.asm   bas.asm
    EXTBAS20_BUILD= ${A09} -B$@ -L${@:.rom=.lst} $(BUILD_DIR)/auto_symbols_bas.asm    extbas.asm
      DISK11_BUILD= ${A09} -B$@ -L${@:.rom=.lst} $(BUILD_DIR)/auto_symbols_extbas.asm disk.asm       -DVERDISK=11
    SUPBAS20_BUILD= ${A09} -B$@ -L${@:.rom=.lst} $(BUILD_DIR)/auto_symbols_extbas.asm supbas.asm     -DCOCOPAL=0
#   SUPBAS20P_BUILD= ${A09} -B$@ -L${@:.rom=.lst} $(BUILD_DIR)/auto_symbols_extbas.asm supbas.asm     -DCOCOPAL=1

all: $(BUILD_DIR) verify

$(BUILD_DIR):
	mkdir -p $@

clean:
	rm -rf $(BUILD_DIR)

$(BUILD_DIR)/a09: a09.c
	gcc -o $@ $<

cocodefs.asm: $(BUILD_DIR)/a09

$(BUILD_DIR)/auto_symbols_coco.asm: cocodefs.asm
	${AUTO_COCO_BUILD}

$(BUILD_DIR)/auto_symbols_bas.asm: $(BUILD_DIR)/auto_symbols_coco.asm
	${AUTO_BAS_BUILD}

$(BUILD_DIR)/auto_symbols_extbas.asm: $(BUILD_DIR)/auto_symbols_bas.asm
	${AUTO_EXTBAS_BUILD}

$(BUILD_DIR)/bas10.rom: $(BUILD_DIR)/auto_symbols_coco.asm bas.asm
	${BAS10_BUILD}

$(BUILD_DIR)/bas11.rom: $(BUILD_DIR)/auto_symbols_coco.asm bas.asm
	${BAS11_BUILD}

$(BUILD_DIR)/bas12.rom: $(BUILD_DIR)/auto_symbols_coco.asm bas.asm
	${BAS12_BUILD}

$(BUILD_DIR)/bas13.rom: $(BUILD_DIR)/auto_symbols_coco.asm bas.asm
	${BAS13_BUILD}

$(BUILD_DIR)/bas20.rom: $(BUILD_DIR)/auto_symbols_coco.asm bas.asm
	${BAS20_BUILD}

$(BUILD_DIR)/extbas10.rom: $(BUILD_DIR)/auto_symbols_bas.asm extbas.asm
	${EXTBAS10_BUILD}

$(BUILD_DIR)/extbas11.rom: $(BUILD_DIR)/auto_symbols_bas.asm extbas.asm
	${EXTBAS11_BUILD}

$(BUILD_DIR)/extbas20.rom: $(BUILD_DIR)/auto_symbols_bas.asm extbas.asm
	${EXTBAS20_BUILD}

$(BUILD_DIR)/disk10.rom: $(BUILD_DIR)/auto_symbols_extbas.asm extbas.asm
	${DISK10_BUILD}

$(BUILD_DIR)/disk11.rom: $(BUILD_DIR)/auto_symbols_extbas.asm extbas.asm
	${DISK11_BUILD}

$(BUILD_DIR)/supbas20.rom: $(BUILD_DIR)/auto_symbols_extbas.asm supbas.asm
	${SUPBAS20_BUILD}

$(BUILD_DIR)/supbas20p.rom: $(BUILD_DIR)/auto_symbols_extbas.asm supbas.asm
	${SUPBAS20P_BUILD}

#
# coco3.rom is just a single 32k concatentation of:
#
#     extbas20 8000-9FFF  (8k)
#        bas20 A000-BFFF  (8k)
#     supbas20 C000-FFFF (16k)
#
$(BUILD_DIR)/coco3.rom: $(BUILD_DIR)/bas20.rom $(BUILD_DIR)/extbas20.rom $(BUILD_DIR)/supbas20.rom
	cat $(BUILD_DIR)/extbas20.rom $(BUILD_DIR)/bas20.rom $(BUILD_DIR)/supbas20.rom >$@

#
# likewise for coco3p.rom (just substitute supbas20p.rom)
#
#$(BUILD_DIR)/coco3p.rom: $(BUILD_DIR)/bas20.rom $(BUILD_DIR)/extbas20.rom $(BUILD_DIR)/supbas20p.rom
#	cat $(BUILD_DIR)/extbas20.rom $(BUILD_DIR)/bas20.rom $(BUILD_DIR)/supbas20p.rom >$@


#
# compare our assembled rom binaries with known SHA1 hashes from
# http://www.mess.org (mess -listxml output)
#
verify: $(BUILD_DIR)/disk11.rom $(BUILD_DIR)/coco3.rom # $(BUILD_DIR)/coco3p.rom
	@echo "verifying all roms..."
	@# ============== "official" mess hashes (taken directly from `mess -listxml`)
	@sha1sum $(BUILD_DIR)/disk11.rom      | grep "10bdc5aa2d7d7f205f67b47b19003a4bd89defd1" >/dev/null && echo "disk11.rom:    OK" || echo "disk11.rom:    ***** BAD SHA1 *****"
	@sha1sum $(BUILD_DIR)/coco3.rom       | grep "e0d82953fb6fd03768604933df1ce8bc51fc427d" >/dev/null && echo "coco3.rom      OK" || echo "coco3.rom      ***** BAD SHA1 *****"
# @sha1sum $(BUILD_DIR)/coco3p.rom      | grep "631e383068b1f52a8f419f4114b69501b21cf379" >/dev/null && echo "coco3p.rom     OK" || echo "coco3p.rom     ***** BAD SHA1 *****"
	@# ============== self-generated hashes (these files are actually just portions of coco3.rom and coco3p.rom above)
	@sha1sum $(BUILD_DIR)/bas20.rom       | grep "09015df53738be4ae9e50a84f26a19b8582c05ff" >/dev/null && echo "bas20.rom      OK" || echo "bas20.rom      ***** BAD SHA1 *****"
	@sha1sum $(BUILD_DIR)/extbas20.rom    | grep "d322cbd9f959bdc5f2cba7baac37071450946b5e" >/dev/null && echo "extbas20.rom:  OK" || echo "extbas20.rom:  ***** BAD SHA1 *****"
	@sha1sum $(BUILD_DIR)/supbas20.rom    | grep "9a05652fd80ab87219102800fe6a44a4ecacd8c3" >/dev/null && echo "supbas20.rom:  OK" || echo "supbas20.rom:  ***** BAD SHA1 *****"
# @sha1sum $(BUILD_DIR)/supbas20p.rom   | grep "1953f7c33f0588ac975212d8ec3c6faee4ebad39" >/dev/null && echo "supbas20p.rom: OK" || echo "supbas20p.rom: ***** BAD SHA1 *****"

install: $(BUILD_DIR)/coco3.rom
	cp $< $(HOME)/Library/XRoar/roms


