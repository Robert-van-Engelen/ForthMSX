To run Forth in MSX:

  clear 100,&h8400
  bload "forth.bin",r

Enter `bye` in Forth to exit back to BASIC. You can return to Forth where you left off from BASIC with:

  defusr0=&h8400:?usr0(0)

Available Forth versions:

- `forthx.bin`: a minimal Forth without extras, no floating-ppint math, and no file-access words
- `forth.bin`: full Forth
