# tools

ASM=asz80
ASMFLAGS=-lops

LNK=aslink
LNKFLAGS=-imwu

IHX2BIN=ihx/ihx2bin -g

RM=rm -f

# files

BIN=forth.bin
HEX=forth.ihx
REL=forth.rel

# rules

all:	$(BIN)

$(BIN): $(HEX)
	$(IHX2BIN) $(HEX) $(BIN)

$(HEX): $(REL)
	$(LNK) $(LNKFLAGS) $(REL)

%.rel: %.asm math.asm mathr.asm main.asm
	$(ASM) $(ASMFLAGS) $<

clean:
	$(RM) *.rel *.sym *.hlr *.map *.lst *.rst

distlean:
	$(RM) *.rel *.sym *.hlr *.map *.lst *.rst $(HEX) $(BIN) $(WAV)

.PHONY: all clean distclean
