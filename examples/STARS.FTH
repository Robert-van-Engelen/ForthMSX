\ MSX VDP and VRAM direct access and control example
\ The "MSX Red Book" has info on MSX VDP IO ports

.( Loading STARS...)

require VDP.FTH
require RAND.FTH

DECIMAL

\ display 1000 stars
: stars
  80 WIDTH
  $F1 7 VDP!	\ white $F on black $1
  randomize
  1001 0 DO
    rand 3200 MOD VRAM	\ random VRAM address $0000 to $0C7F
    S" *" >VRAM		\ display *
  LOOP
;

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
  \
  %00000000 C,
  %00000000 C,
  %01010000 C,
  %00100000 C,
  %01010000 C,
  %00000000 C,
  %00000000 C,
  %00000000 C,
  \
  %00000000 C,
  %00000000 C,
  %01110000 C,
  %01010000 C,
  %01110000 C,
  %00000000 C,
  %00000000 C,
  %00000000 C,
  \
  %00100000 C,
  %10101000 C,
  %01110000 C,
  %11011000 C,
  %01110000 C,
  %10101000 C,
  %00100000 C,
  %00000000 C,

\ twinkle bitmapped stars
: twinkle
  40 WIDTH
  $F1 7 VDP!	\ white $F on black $1
  $0808 VRAM	\ redefine char 1,2,3,4 in the VRAM pattern table
  star 32 >VRAM
  randomize
  251 0 DO
    rand 1600 MOD VRAM	\ random VRAM address $0000 to $037F
    rand 3 AND 1+	\ random 1,2,3,4
    SP@ 1 >VRAM		\ SP@ is address of TOS
    DROP
  LOOP
  0
  BEGIN
    1+		\ twinkle counter
    4 0 DO			\ cycle through 4 bitmaps
      $0808 I 8 * + VRAM
      DUP I + 3 AND
      8 * star + 8 >VRAM	\ redefine star bitmap
    LOOP
    1000 0 DO LOOP		\ slow down
  AGAIN
  DROP
;
