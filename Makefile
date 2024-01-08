# =============================================================================
# A09 assembler (modified version included here).  stock version available
#               from: http://www.hermannseib.com/english/opensource.htm
# =============================================================================

BUILD_DIR := build

LWASM=lwasm --6809 --format=raw --list-nofiles --pragma=noindex0tonone

all: $(BUILD_DIR) $(BUILD_DIR)/coco3.rom

$(BUILD_DIR):
	mkdir -p $@

clean:
	rm -rf $(BUILD_DIR)

$(BUILD_DIR)/auto_symbols_coco.asm: $(BUILD_DIR) cocodefs.asm
	${LWASM} --no-output --symbol-dump=$@ $(filter-out $<,$^)

$(BUILD_DIR)/coco3.rom: $(BUILD_DIR) $(BUILD_DIR)/auto_symbols_coco.asm coco3bas.asm
	${LWASM} --output=$@ --pragma=cd --list=${@:.rom=.lst} $(filter-out $<,$^) -DCOCOPAL=0

verify: $(BUILD_DIR)/coco3.rom
	@sha1sum $(BUILD_DIR)/coco3.rom | grep "e0d82953fb6fd03768604933df1ce8bc51fc427d" >/dev/null && echo "coco3.rom      OK" || echo "coco3.rom      ***** BAD SHA1 *****"

install: $(BUILD_DIR)/coco3.rom
	cp $< $(HOME)/Library/XRoar/roms

run: install
	xroar -machine coco3 -force-crc-match &


