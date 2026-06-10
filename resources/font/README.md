# UI font glyphs

These are the original game's UI font bitmaps (used by `writetext` in
`resources/original/Data/menu.lua` for level names, menus, etc), recovered
from the Chowdren engine pack `resources/original/Assets.dat` with
`tools/extract_assets.py`. They are NOT the same drawings as the
`text_<char>_0_*.png` tile sprites in `Data/Sprites` — the UI font is a
separate, smaller set that includes punctuation.

Each glyph is an 8x24 RGBA image with its hotspot at (4,12) (the engine
draws letters centered on the hotspot). `writetext` advances a fixed 10
units per character, 1 unit = 1/24 of a grid square.

## How these were located (in case it has to be done again)

- The engine renders text as animation frames of an internal letter object.
  `lookup_table` in `resources/original/Data/values.lua` (~line 2805) maps
  codepoints to frame indices: a-z = frames 0-25, 0-9 = 26-35, then
  punctuation `- . ? ! , ' : _ > < ( ) & + ...` = 36+, umlauts, cyrillic,
  and capitals (which share bitmaps with lowercase, so the packer dedupes
  them into one image).
- In the extracted archive the font frames are the 8x24 images with hotspot
  (4,12), scattered non-contiguously because of that dedup. Filter the
  extractor's output by that size and eyeball the strips.
- Indices in this build's Assets.dat (may differ in other builds):
  a=1991 b=769 c=1490 d=1491 e=18 f=403 g=404 h=141 i=109 j=405 k=1438
  l=407 m=409 n=408 o=410 p=411 q=412 r=413 s=414 t=415 u=416 v=417 w=418
  x=419 y=420 z=421; 0=58 1=423 2=424 3=425 4=427 5=428 6=429 7=430 8=431
  9=432; .=433 -=434 ?=435 !=436 ,=437 '=438 :=439
- Other treasures in there: a tiny 3x12 font (indices ~150, 293-305,
  353-384), the big 24x48 credits font (~743-807, 2059+), the
  CONGRATULATIONS! write-on animation (604-626, final frames 624-626), and
  AREA COMPLETE! (1474-1504).
