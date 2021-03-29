.PHONY: all clean

all: niccc.gbc

PYTHON = python
OBJS = src/main.o src/bss.o src/SoundSystem.o \
	src/parts/notgba.o src/parts/dotplotter.o \
	src/parts/polystream.o src/parts/credits.o \
	data/music_main.o data/music_credits.o \
    src/parts/introscreens.o

data/scene1_16k.bin:
	$(PYTHON) data/repad.py

src/main.asm: src/header.asm src/init.asm;
src/parts/polystream.asm: data/scene1_16k.bin;
%.asm: ;

%.o: %.asm
	rgbasm -o $@ $<

# MBC5 + 8KB RAM
niccc.gbc: $(OBJS)
	rgblink -n niccc.sym -o niccc.gbc $^
	rgbfix -Cjv -m 0x1a -p 0xff -r 2 -t "GBC NICCC" niccc.gbc

clean:
	rm -f niccc.gbc niccc.sym
	rm -f $(OBJS)
	rm -f data/scene1_16k.bin
