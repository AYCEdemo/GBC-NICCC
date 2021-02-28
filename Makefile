.PHONY: all clean

all: niccc.gbc

OBJS = src/main.o src/bss.o src/polystream.o src/credits.o

%.asm: ;

%.o: %.asm
	rgbasm -o $@ $<

# MBC5 + 8KB RAM
niccc.gbc: $(OBJS)
	rgblink -n niccc.sym -o niccc.gbc $^
	rgbfix -Cjv -m 0x1a -p 0 -r 2 -t "GBC NICCC" niccc.gbc

clean:
	rm niccc.gbc niccc.sym
	rm $(OBJS)
