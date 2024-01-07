# =============================================================================
# A09 assembler (modified version included here).  stock version available
#               from: http://www.hermannseib.com/english/opensource.htm
# =============================================================================

BUILD_DIR := build

A09=$(BUILD_DIR)/a09 -oNOW -oTXT -oEXP -oNCL

all: $(BUILD_DIR) $(BUILD_DIR)/coco3.rom

$(BUILD_DIR):
	mkdir -p $@

clean:
	rm -rf $(BUILD_DIR)

$(BUILD_DIR)/a09: a09.c $(BUILD_DIR)
	gcc -o $@ $<

cocodefs.asm: $(BUILD_DIR)/a09

$(BUILD_DIR)/auto_symbols_coco.asm: cocodefs.asm
	${A09} -B$(BUILD_DIR)/auto_tmp -Y$@ $^

verify: $(BUILD_DIR)/coco3.rom
	@sha1sum $(BUILD_DIR)/coco3.rom | grep "e0d82953fb6fd03768604933df1ce8bc51fc427d" >/dev/null && echo "coco3.rom      OK" || echo "coco3.rom      ***** BAD SHA1 *****"

$(BUILD_DIR)/coco3.rom: $(BUILD_DIR)/auto_symbols_coco.asm coco3bas.asm
	${A09} -B$@ -L${@:.rom=.lst} $^ -DCOCOPAL=0

install: $(BUILD_DIR)/coco3.rom
	cp $< $(HOME)/Library/XRoar/roms

run: install
	xroar -machine coco3 -force-crc-match &


