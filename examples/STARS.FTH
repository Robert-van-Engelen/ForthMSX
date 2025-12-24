\ MSX VDP and VRAM direct access and control example
\ The "MSX Red Book" has info on MSX VDP IO ports

require VDP.FTH
require RAND.FTH

.( Loading STARS...)

DECIMAL

\ define four star character bitmaps from dim to bright
CREATE star
  %00000000 C,
  %00000000 C,
  %00000000 C,
  %00100000 C,
  %00000000 C,
  %00000000 C,
  %00000000 C,
  %00000000 C,

  %00000000 C,
  %00000000 C,
  %01010000 C,
  %00100000 C,
  %01010000 C,
  %00000000 C,
  %00000000 C,
  %00000000 C,

  %00000000 C,
  %00000000 C,
  %01110000 C,
  %01010000 C,
  %01110000 C,
  %00000000 C,
  %00000000 C,
  %00000000 C,

  %00100000 C,
  %10101000 C,
  %01110000 C,
  %11011000 C,
  %01110000 C,
  %10101000 C,
  %00100000 C,
  %00000000 C,

\ twinkle bitmapped stars in 40x24 text mode
: twinkle
  40 WIDTH	\ 40x24 text mode
  15 1 1 VCOLOR	\ white on black
  star 32 $0808 VWRITE	\ redefine char 1,2,3,4 in the VRAM pattern table
  randomize
  201 0 DO
    I 3 AND 1+		\ 1,2,3,4
    SP@ 1		\ SP@ is address of TOS with 1,2,3,4
    rand 1600 MOD	\ random VRAM address $0000 to $037F
    VWRITE
    DROP		\ remove char 1,2,3,4
  LOOP
  0
  BEGIN
    1+		\ twinkle counter
    4 0 DO	\ cycle through 4 bitmaps
      DUP
      I + 3 AND 3 LSHIFT star +
      8
      $0808 I 3 LSHIFT +
      VWRITE	\ redefine star bitmap
    LOOP
    1000 0 DO LOOP	\ slow down
  AGAIN
  DROP
;
