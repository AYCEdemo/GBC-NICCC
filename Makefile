.PHONY: all clean

all: niccc.gbc

PYTHON = python
OBJS = src/main.o src/notgba.o src/bss.o src/SoundSystem.o \
	src/polystream.o src/credits.o \
	data/music_main.o

data/scene1_16k.bin:
	$(PYTHON) data/repad.py

src/polystream.asm: data/scene1_16k.bin;
%.asm: ;

src/notgba.asm:
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
