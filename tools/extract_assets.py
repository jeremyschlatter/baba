#!/usr/bin/env -S uv run --script
# /// script
# dependencies = ["pillow"]
# ///
# Extract images from Baba Is You's Assets.dat (Chowdren engine, archive v2,
# image entry v1, chowimg/LZ4 compression). Format reverse-engineered with
# reference to github.com/Priw8/chowdren-extractor.
import struct, sys, os
from PIL import Image

data = open(sys.argv[1], "rb").read()
outdir = sys.argv[2]
os.makedirs(outdir, exist_ok=True)

def lz4_hunks(buf, start, end):
    out = bytearray()
    pos = start
    while pos < end:
        hunk_size, = struct.unpack_from("<I", buf, pos)
        pos += 4
        hunk_end = pos + hunk_size
        hunk_start = len(out)
        while pos < hunk_end:
            control = buf[pos]; pos += 1
            lit_len = control >> 4
            match_len = control & 0xF
            if lit_len == 0xF:
                while True:
                    b = buf[pos]; pos += 1
                    lit_len += b
                    if b != 0xFF: break
            out += buf[pos:pos+lit_len]; pos += lit_len
            if pos >= hunk_end: break
            dist, = struct.unpack_from("<H", buf, pos); pos += 2
            if match_len == 0xF:
                while True:
                    b = buf[pos]; pos += 1
                    match_len += b
                    if b != 0xFF: break
            match_len += 4
            src = hunk_start + (len(out) - hunk_start) - dist
            for _ in range(match_len):  # may overlap; copy byte-wise
                out.append(out[src]); src += 1
        pos = hunk_end
    return bytes(out)

# header: (offset, size) pairs until the first asset offset
first_offset, = struct.unpack_from("<I", data, 0)
entries = [struct.unpack_from("<II", data, i) for i in range(0, first_offset, 8)]
print(f"{len(entries)} entries; first at 0x{first_offset:x}")

ok = bad = 0
index = []
for i, (off, size) in enumerate(entries):
    try:
        w, h, hx, hy = struct.unpack_from("<HHHH", data, off)
        csize, = struct.unpack_from("<I", data, off + 13)
        if 13 + 4 + csize != size or not (0 < w <= 4096 and 0 < h <= 4096):
            bad += 1
            continue
        rgba = lz4_hunks(data, off + 17, off + 17 + csize)
        assert len(rgba) == w * h * 4, f"#{i}: got {len(rgba)}, want {w*h*4}"
        Image.frombytes("RGBA", (w, h), rgba).save(f"{outdir}/img{i:04}_{w}x{h}_hot{hx},{hy}.png")
        index.append((i, w, h, hx, hy))
        ok += 1
    except Exception as e:
        bad += 1
        if bad < 10: print(f"#{i} (0x{off:x}, {size}): {e}")
print(f"extracted {ok}, skipped {bad}")
with open(f"{outdir}/index.txt", "w") as f:
    for i, w, h, hx, hy in index:
        f.write(f"{i} {w}x{h} hot {hx},{hy}\n")
