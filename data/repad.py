TARGET_BANK_SIZE = 16384

def num(a):
    return int.from_bytes(a, "big", signed=False)

def writebank(bank, data):
    fou = open("scene1/{}.bin".format(bank), "wb")
    fou.write(data)
    fou.close()

fin = open("scene1.bin", "rb")

bank = 0
data = bytearray()
frame = bytearray()
running = True
while running:
    bflag = fin.read(1)
    frame += bflag
    flag = num(bflag)
    indexed = False
    if flag & 2:
        # palette data
        bmask = fin.read(2)
        frame += bmask
        mask = num(bmask)
        for i in range(16):
            if mask & 1:
                frame += fin.read(2)
            mask >>= 1
    if flag & 4:
        # vert table
        indexed = True
        bnum = fin.read(1)
        frame += bnum + fin.read(num(bnum) * 2)
    while True:
        bpoly = fin.read(1)
        poly = num(bpoly)
        if poly >= 253:
            # flush
            if poly == 254:
                fin.seek(((fin.tell() - 1) // 65536 + 1) * 65536)
            if len(data) + len(frame) >= TARGET_BANK_SIZE - 1:
                writebank(bank, data[:-1] + b"\xfe")
                bank += 1
                data.clear()
            data += frame + b"\xff"
            frame.clear()
            if poly == 253:
                # end
                writebank(bank, data[:-1] + b"\xfd")
                running = False
            break
        else:
            poly &= 15
            dlen = poly if indexed else poly * 2
            frame += bpoly + fin.read(dlen)
