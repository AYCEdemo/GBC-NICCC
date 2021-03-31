import pygame_sdl2 as pygame
import os
import sys

DUMP_PATH = "niccc.dump"
SYM_PATH = "niccc.sym"
SIZE = WIDTH, HEIGHT = 128, 128*2
SCALE = 3

DELTA_COLORS = [
    (0,0,0), (255,255,255), (127,127,255), (127,255,127)
]
COL_NEW = (127,255,127)
COL_NOT = (255,127,127)

sym = {}
for line in open(SYM_PATH, "r"):
    pars = line.split(";")[0].split(" ")
    if len(pars) < 2:
        continue
    # ignore bank for now
    ofs = int(pars[0][3:], 16) - 0xa000
    if ofs < 0 or ofs >= 0x6000:
        continue
    sym[pars[1].strip()] = ofs

init = True
pygame.init()
screen = pygame.display.set_mode((WIDTH * SCALE, HEIGHT * SCALE))

last_dump = None

vert_tab = None
stroke_tab = [0]*128*16
render_buf = [0]*128*16

def read_dump():
    global init
    surf = pygame.Surface(SIZE)
    dump = open(DUMP_PATH, "rb").read(0x6000)
    base_x, base_y = sym["wVertArrayX"], sym["wVertArrayY"]
    vert_tab = [(dump[i + base_x], dump[i + base_y]) for i in range(256)]
    # verts
    for i in vert_tab:
        if 0 <= i[0] < 128 and 0 <= i[1] < 128:
            surf.set_at(i, 0x7f7f7f)

    # expected face
    cnt = dump[sym["wVertCount"]]
    base = sym["hVertTabX"]
    verts = [(dump[i+base], dump[i+base+16]) for i in range(cnt)]
    prevs = [surf.get_at(i) for i in verts]
    pygame.draw.polygon(surf, (255, 255, 255), verts)
    for i in range(cnt):
        pos = verts[i]
        ref = prevs[i]
        col = 0x00ff00
        if ref == col:
            col = 0xff0000
        elif ref == 0:
            col = 0x0000ff
        surf.set_at(pos, col)

    # render buffer
    base = sym["sRenderBuf"]
    cur_col = dump[sym["wCurColor"]]
    addr = 0
    for i in range(16):
        for j in range(128):
            base2 = base + i*256 + j*2
            bits = int.from_bytes(dump[base2:base2+2]+dump[base2+0x1000:base2+0x1002], "little", signed=False)
            delta = 0 if init else render_buf[addr] ^ bits
            render_buf[addr] = bits
            for k in range(8):
                sh = 7 - k
                pix = bits >> sh
                pix = (pix & 1) | ((pix & 0x100) >> 7) | ((pix & 0x10000) >> 14) | ((pix & 0x1000000) >> 21)
                col = (pix*0x11, pix*0x11, pix*0x11)
                if (delta >> sh) & 0x01010101 != 0:
                    col = COL_NEW if pix == cur_col else COL_NOT
                surf.set_at((i*8+k, j+128), col)
            addr += 1

    pygame.transform.scale(surf, (WIDTH*SCALE, HEIGHT*SCALE), screen)
    # screen.blit(surf, (0, 0))
    init = False

while True:
    for event in pygame.event.get():
        if event.type == pygame.locals.QUIT:
            sys.exit()

    # examine if the dump was modified
    stat = os.stat(DUMP_PATH)
    size = stat.st_size
    mtime = stat.st_mtime_ns
    if size >= 0x6000 and mtime != last_dump:
        read_dump()
        last_dump = mtime

    pygame.display.flip()
    pygame.time.wait(500)
