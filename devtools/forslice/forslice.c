/******************************************************************************

 NAME
     forslice - a headless Forth program slicer

 SYNOPSIS
     forslice [options] INFILE.BIN [OUTFILE.BIN]

 DESCRIPTION
     Slice ForthMSX saved-state binary INFILE.BIN by removing unused words to
     create a headless program slice saved to OUTFILE.BIN or to SLICE.BIN when
     OUTFILE.BIN is not specified.  Unused words are those that cannot be
     reached on any execution path from MAIN or from the word specified with
     option -main instead of MAIN.  Forslice saves a log file forslice.log in
     the working directory.  Forslice requires the forth.rel file located in the
     working directory to relocate the Forth internals.  When warnings are
     given, it may be necessary to specify a words info file with option
     -words.

 OPTIONS
     -repl              include REPL interpreter and dictionary (not headless)
     -break             disable (CTRL-)STOP break
     -main <name>       run <name> as the main program, default name is MAIN
     -start <addr>      new binary start address to bload the program slice
     -words <file.inf>  use words info file to assist slicing and relocation

     If MAIN or the word specified by -main is missing, then REPL will be run.

     A words info file contains a list of Forth words with an optional info
     field separated by spaces and/or newlines.  For example, we can specify
     some words that we want to include in the slice as well as all words that
     those words depend on to run:

     REPL
     words .s PAGE

     The first line is the same as option -repl.  ForthMSX word names are case
     insensitive.

     An info field of the form {...} is used for VAR, VAL and 2VAL words and
     for words defined with CREATE DOES>.  The info field must not contain
     spacing and specifies a list of offsets into the data of those words where
     Forth addresses (pointers to data) are located that must be relocated
     properly for the program slice to run reliably.

     For example, words created by MARKER and VOCABULARY have pointers in their
     data at certain byte-aligned offsets from their pfa (parameter field
     address given by >BODY):

     MARKER {0,2,4}
     VOCABULARY {0,4}

     These pointers at the specified offsets must remain valid after program
     relocation.  Therefore, these two lines are required to properly slice and
     relocate all `MARKER`-defined words and `VOCABULARY`-defined words in the
     sliced saved state, when these are used in the slice.  This is not
     necessary When none are used in the slice.  Check the saved forslice.log
     for details.

 ******************************************************************************

 BSD 3-Clause License

 Copyright (c) 2025, Robert van Engelen
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice, this
    list of conditions and the following disclaimer.

 2. Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

 3. Neither the name of the copyright holder nor the names of its
    contributors may be used to endorse or promote products derived from
    this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

 ******************************************************************************/

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys/stat.h>

// ForthMSX max dictionary TOP address
#define TOP (0xde3f - 256 - 256 - 2*256 - 40)

int flag_repl = 0;
int flag_break = 0;
int flag_start = 0;
const char *flag_main = "MAIN"; // default word to run when present

typedef uint8_t Byte;
typedef uint16_t Addr;

struct Binary {
  const char *infile;
  const char *outfile;
  const char *logfile;
  Byte *data;   // raw binary data from bin file
  Byte *mark;   // mark word lfa with 1 to analyze a word, then 2 to mark used, 0 unusud
  Addr size;    // binary size including the header
  Addr base;    // binary base address without header
  Addr boot;    // binary boot address
};

struct Reloc {
  const char *file;
  Byte *relo;   // relocation info for address, 0 = none, 1 = LSB, 2 = MSB, 3 = address, 4 = jr offset
  Addr *addr;   // relocation address from rel file
  Addr *move;   // relocated address
  Addr base;    // base address of relocatable binary with header
  Addr last;    // last address of relocation info
  Addr boot;    // cold boot address
  Addr warm;    // warm boot address
  Addr link;    // vocabulary link (voc_link) address
};

struct Words {
  const char *file;
  char *data;   // word info file content
  Addr *addr;   // lfa of each word in the dictionary
  Addr size;    // number of words in the dictionary
  Addr docol;   // cfa of (:)
  Addr doret;   // cfa of (;) etc.
  Addr dovar;
  Addr doval;
  Addr dotwoval;
  Addr docon;
  Addr dotwocon;
  Addr dodef;
  Addr dotick;
  Addr dolit;
  Addr dotwolit;
  Addr doquote;
  Addr doto;
  Addr doplusto;
  Addr dotwoto;
  Addr dodplusto;
  Addr doif;
  Addr doof;
  Addr doahead;
  Addr doagain;
  Addr dountil;
  Addr dodo;
  Addr doqdo;
  Addr doloop;
  Addr doplusloop;
  Addr dosemicolondoes;
  Addr repl;
  Addr error;
  Addr handler;
  Addr throw;
  Addr bye;
  Addr main;
};

static struct Binary bin;
static struct Reloc rel;
static struct Words wds;

// parse a byte from a string
Byte parse_byte(const char *s)
{
  return strtoul(s, NULL, 16);
}

// parse a address from a string
Addr parse_addr(const char *s)
{
  return strtoul(s, NULL, 16);
}

// parse a LSB<space>MSB address from a string
Addr parse_lsb_msb(const char *s)
{
  return parse_byte(s) + (parse_byte(s + 3) << 8);
}

// convert a binary LSB MSB byte pair to a 16 bit address
Addr u16(const Byte *data)
{
  return data[0] + (data[1] << 8);
}

// fetch a byte from the binary file data located at addr
Addr bin_byte(const struct Binary *bin, Addr addr)
{
  return bin->data[addr - bin->base];
}

// fetch an address from the binary file data located at addr
Addr bin_addr(const struct Binary *bin, Addr addr)
{
  return u16(bin->data + addr - bin->base);
}

// compare a name with length to the name field in the binary file data at address nfa
int bin_name_len(const struct Binary *bin, Addr nfa, const char *name, size_t len)
{
  Byte n = bin_byte(bin, nfa) & 0x3f;
  return n == len && strncasecmp((const char*)(bin->data + nfa - bin->base + 1), name, n) == 0;
}

// compare a name to the name field in the binary file data at address nfa
int bin_name(const struct Binary *bin, Addr nfa, const char *name)
{
  return bin_name_len(bin, nfa, name, strlen(name));
}

// find the name field address (nfa) of the word corresponding to the execution token xt using the context vocabulary
Addr bin_nt(const struct Binary *bin, Addr context, Addr xt)
{
  Addr link = context;
  for (link = bin_addr(bin, link); link; link = bin_addr(bin, link))
    if (link + 3 + (bin_byte(bin, link + 2) & 0x3f) == xt)
      return link + 2;
  return 0;
}

// check if addr is an address in the rel file (0 no or 1 yes) or could possibly be a valid Forth address (2)
int test_addr(const struct Binary *bin, const struct Reloc *rel, Addr addr)
{
  if (addr >= bin->base && addr <= bin->base + bin->size)
  {
    if (addr <= rel->last)
      return rel->relo[addr - rel->base] != 0;
    addr = bin_addr(bin, addr);
    if (addr >= rel->warm && addr <= bin->base + bin->size)
      return 2;
  }
  return 0;
}

// set orignal relocation address to adjust when relocating
void rel_relo_addr(struct Reloc *rel, Addr addr, Addr orig)
{
  rel->relo[addr - rel->base] = 3;
  rel->addr[addr - rel->base] = orig;
}

// set orignal relative jump relocation address to adjust when relocating
void rel_jr_addr(struct Reloc *rel, Addr addr, Addr orig)
{
  rel->relo[addr - rel->base] = 4;
  rel->addr[addr - rel->base] = orig;
}

// write the name of the name token nt to screen or file fp
void print_nt(FILE *fp, const struct Binary *bin, Addr nt)
{
  Byte len = bin_byte(bin, nt) & 0x3f;
  while (len--)
    fputc(bin_byte(bin, ++nt), fp);
}

// store byte x in the binary file at addr
void bin_set_byte(struct Binary *bin, Addr addr, Byte x)
{
  bin->data[addr - bin->base] = x;
}

// store address dest in the binary file at addr
void bin_set_addr(struct Binary *bin, Addr addr, Addr dest)
{
  bin->data[addr - bin->base + 0] = dest & 0xff;
  bin->data[addr - bin->base + 1] = dest >> 8;
}

// fetch relocation info byte for addr (0=none, 1=LSB, 2=MSB, 3=address, 4=jr)
Byte rel_relo(struct Reloc *rel, Addr addr)
{
  return rel->relo[addr - rel->base];
}

// fetch relocation info original address at addr
Addr rel_addr(struct Reloc *rel, Addr addr)
{
  return rel->addr[addr - rel->base];
}

// relocate the address stored at addr to the dest address
void rel_move(struct Reloc *rel, Addr addr, Addr dest)
{
  rel->move[addr - rel->base] = dest;
}

// fetch relocated dest address for the address stored at addr
Addr rel_dest(struct Reloc *rel, Addr addr)
{
  Addr dest = rel->move[addr - rel->base];
  if (dest == 0)
  {
    printf("ERROR: no relocation destination for address %0.4X\n", addr);
    exit(EXIT_FAILURE);
  }
  return dest;
}

// read binary file
void read_bin(struct Binary *bin)
{
  struct stat buf;
  FILE *fp = fopen(bin->infile, "rb");
  printf("reading bin file %s\n", bin->infile);
  bin->data = NULL;
  bin->mark = NULL;
  bin->size = 0;
  if (fp == NULL)
  {
    perror("cannot open bin file for reading");
    exit(EXIT_FAILURE);
  }
  if (fstat(fileno(fp), &buf) != 0)
  {
    perror("cannot stat bin file");
    exit(EXIT_FAILURE);
  }
  if (buf.st_size > 65535)
  {
    fprintf(stderr, "bin file exceeds 64K\n");
    exit(EXIT_FAILURE);
  }
  bin->size = (uint16_t)buf.st_size;
  bin->data = malloc(bin->size);
  if (fread(bin->data, 1, bin->size, fp) < bin->size)
  {
    perror("cannot read bin file");
    exit(EXIT_FAILURE);
  }
  fclose(fp);
}

// read relocation file
void read_rel(struct Reloc *rel, Addr size)
{
  Addr addr, skip;
  size_t i;
  unsigned lineno = 2;
  char line[256], text[256];
  FILE *fp = fopen(rel->file, "r");
  printf("reading rel file %s\n", rel->file);
  rel->relo = malloc(size);
  rel->addr = malloc(size * sizeof(Addr));
  rel->move = malloc(size * sizeof(Addr));
  memset(rel->relo, 0, size);
  memset(rel->addr, 0, size * sizeof(Addr));
  memset(rel->move, 0, size * sizeof(Addr));
  rel->base = 0;
  rel->boot = 0;
  rel->warm = 0;
  rel->link = 0;
  if (fp == NULL)
  {
    perror("cannot open rel file for reading");
    exit(EXIT_FAILURE);
  }
  if (fgets(line, sizeof(line), fp) == NULL || strcmp(line, "XL2\n") != 0)
  {
    perror("not a rel file");
    exit(EXIT_FAILURE);
  }
  while (fgets(line, sizeof(line), fp) != NULL)
  {
    switch (*line)
    {
      case 'S':
        if (strncmp(line + 2, "boot Def", 8) == 0)
          rel->boot = parse_addr(line + 10);
        else if (strncmp(line + 2, "warm Def", 8) == 0)
          rel->warm = parse_addr(line + 10);
        else if (strncmp(line + 2, "voc_link Def", 12) == 0)
          rel->link = parse_addr(line + 14);
        break;
      case 'T':
        addr = parse_lsb_msb(line + 2);
        if (rel->base == 0)
          rel->base = addr;
        strcpy(text, line);
        break;
      case 'R':
        if (rel->base == 0)
        {
          fprintf(stderr, "rel file stray R line at %u\n%s\n", lineno, line);
        }
        else
        {
          skip = 2;
          for (i = 4; 3 * i + 3 < strlen(line); i += 4)
          {
            Byte relo = parse_byte(line + 2 + 3 * i);
            Byte offset = parse_byte(line + 5 + 3 * i) & 0x0f;
            rel->addr[addr - rel->base + offset - skip] = parse_lsb_msb(text + 2 + 3 * offset);
            if (relo == 0x00)
            {
              rel->relo[addr - rel->base + offset - skip] = 1;
              ++skip;
            }
            else if (relo == 0x0d)
            {
              rel->relo[addr - rel->base + offset - skip] = 2;
              ++skip;
            }
            else if (relo == 0x01)
            {
              rel->relo[addr - rel->base + offset - skip] = 3;
            }
            else
            {
              fprintf(stderr, "unsupported relocation n1 = %.2X at line %u\n%s\n", relo, lineno, line);
              exit(EXIT_FAILURE);
            }
          }
        }
        break;
    }
    ++lineno;
  }
  rel->last = addr;
  fclose(fp);
}

// return the marker for addr (0=none, 1=analyze, 2=used)
Byte bin_marked(struct Binary *bin, Addr addr)
{
  return bin->mark[addr - bin->base];
}

// find the link field address (lfa) of the word given addr pointing anywhere in its body
Addr bin_lfa(struct Words *wds, Addr addr)
{
  Addr lfa;
  int i;
  for (i = 1; i < wds->size; ++i)
    if (addr < wds->addr[i])
      break;
  lfa = wds->addr[i - 1];
  return lfa;
}

// convert link field address (lfa) to name field address (nfa)
Addr bin_lfa_to_nfa(struct Binary *bin, Addr lfa)
{
  Addr nfa = lfa + 2;
  return nfa;
}

// convert link field address (lfa) to code field address (cfa or execution token of a word)
Addr bin_lfa_to_cfa(struct Binary *bin, Addr lfa)
{
  Addr nfa = bin_lfa_to_nfa(bin, lfa);
  Byte len = bin_byte(bin, nfa) & 0x3f;
  Addr cfa = nfa + 1 + len;
  return cfa;
}

// convert link field address (lfa) to parameter field address (pfa or body of a word)
Addr bin_lfa_to_pfa(struct Binary *bin, Addr lfa)
{
  Addr cfa = bin_lfa_to_cfa(bin, lfa);
  Addr pfa = cfa + 3;
  return pfa;
}

// mark the word corresponding to addr pointing anywhere in its body
void bin_mark(struct Binary *bin, struct Words *wds, Addr addr, Byte marker)
{
  if (addr >= bin->base && addr < bin->base + bin->size)
  {
    Addr lfa = bin_lfa(wds, addr);
    if (marker > bin_marked(bin, lfa))
    {
      Addr cfa = bin_lfa_to_cfa(bin, lfa);
      bin->mark[lfa - bin->base] = marker;
      bin->mark[cfa - bin->base] = marker;
    }
  }
}

// read words info file
void read_wds(struct Words *wds)
{
  wds->data = NULL;
  if (wds->file != NULL)
  {
    struct stat buf;
    FILE *fp = fopen(wds->file, "r");
    printf("reading words info file %s\n", wds->file);
    if (fp == NULL)
    {
      perror("cannot open words info file for reading");
      exit(EXIT_FAILURE);
    }
    if (fstat(fileno(fp), &buf) != 0)
    {
      perror("cannot stat words info file");
      exit(EXIT_FAILURE);
    }
    wds->data = malloc(buf.st_size + 1);
    if (fread(wds->data, 1, buf.st_size, fp) < buf.st_size)
    {
      perror("cannot read words info file");
      exit(EXIT_FAILURE);
    }
    wds->data[buf.st_size] = '\0';
    fclose(fp);
  }
}

// find lfa in the words info file (1=found) or find lfa and populate relocation info for nonzero pfa and end (2)
int wds_info(struct Binary *bin, struct Reloc *rel, struct Words *wds, Addr lfa, Addr pfa, Addr end)
{
  if (wds->data)
  {
    const char *ptr = wds->data;
    Addr nfa = bin_lfa_to_nfa(bin, lfa);
    while (*ptr)
    {
      const char *word;
      while (*ptr != '\0' && !isgraph((unsigned char)*ptr))
        ++ptr;
      word = ptr;
      while (*ptr != '\0' && isgraph((unsigned char)*ptr))
        ++ptr;
      if (bin_name_len(bin, nfa, word, ptr - word))
      {
        while (*ptr != '\0' && !isgraph((unsigned char)*ptr))
          ++ptr;
        if (*ptr == '{')
        {
          const char *offsets = ptr;
          while (*ptr != '\0' && isgraph((unsigned char)*ptr))
            ++ptr;
          if (*(ptr - 1) == '}')
          {
            if (pfa != 0)
            {
              // clear relocation info for this word from pfa to its end
              Addr addr;
              for (addr = pfa; addr < end; ++addr)
                rel->relo[addr - rel->base] = 0;
              // overwrite and add address-based pfa offsets to the relocation info for this word
              while (*offsets != '}')
              {
                char *next;
                Addr addr = pfa + strtoul(offsets + 1, &next, 0);
                if (addr < end)
                {
                  Addr orig = bin_addr(bin, addr);
                  if (orig >= rel->warm && orig <= bin->base + bin->size)
                    rel_relo_addr(rel, addr, orig);
                }
                offsets = next;
              }
              return 2;
            }
            return 0;
          }
        }
        return 1;
      }
    }
  }
  return 0;
}

// slice the ForthMSX program
void slice(struct Binary *bin, struct Reloc *rel, struct Words *wds)
{
  int i, errs, warn, keep, changes, sliced;
  Addr move, addr, link, here, dict;
  FILE *fp = NULL;
  bin->mark = malloc(bin->size);
  memset(bin->mark, 0, bin->size);
  bin->base = u16(bin->data + 1) - 7;
  if (bin->base != rel->base)
  {
    fprintf(stderr, "rel file base address undefined or does not match bin file base address\n");
    exit(EXIT_FAILURE);
  }
  bin->boot = u16(bin->data + 5);
  if (bin->boot != rel->boot)
  {
    fprintf(stderr, "rel file boot address undefined or does not match bin file boot address\n");
    exit(EXIT_FAILURE);
  }
  if (rel->link == 0)
  {
    fprintf(stderr, "rel file voc_link address undefined\n");
    exit(EXIT_FAILURE);
  }
  // log the analysis and results, warn with some details about potential issues
  fp = fopen(bin->logfile, "w");
  if (fp == NULL)
  {
    perror("cannot open log file for writing");
    exit(EXIT_FAILURE);
  }
  // do we have a new bload start address to relatively move the binary to?
  move = (flag_start != 0 ? flag_start - bin->base - 7 : 0);
  // check rel file relocation info against the bin file data (won't work with a saved state)
  printf("bin file spans %0.4X to %0.4X (%u bytes)\n", bin->base, bin->base + bin->size, bin->size);
  printf("rel file spans %0.4X to %0.4X (%u bytes)\n", rel->base, rel->last, rel->last - rel->base);
  // iterate over linked list of vocabularies
  wds->addr = malloc((bin->size / 8) * sizeof(Addr));
  wds->size = 0;
  for (link = rel->link - 4; link; link = bin_addr(bin, link + 4))
  {
    Addr lfa = link;
    Addr nfa = bin_nt(bin, lfa, link - 3);
    if (nfa)
    {
      // iterate over words
      for (lfa = bin_addr(bin, lfa); lfa; lfa = bin_addr(bin, lfa))
      {
        nfa = lfa + 2;
        // insert lfa into the word list sorted by address low to high
        if (wds->size == 0 || lfa > wds->addr[wds->size - 1])
        {
          wds->addr[wds->size++] = lfa;
        }
        else
        {
          for (i = 0; i < wds->size && lfa != wds->addr[i]; ++i)
          {
            if (lfa < wds->addr[i])
            {
              memmove(wds->addr + i + 1, wds->addr + i, (wds->size - i) * sizeof(Addr));
              wds->addr[i] = lfa;
              wds->size++;
              break;
            }
          }
        }
      }
    }
  }
  // last Words wds->addr lfa entry is the end of the binary
  wds->addr[wds->size] = bin->base + bin->size;
  printf("\nwords:");
  for (i = 0; i < wds->size; ++i)
  {
    printf(" ");
    print_nt(stdout, bin, wds->addr[i] + 2);
  }
  printf("\n\nanalyzing core... ");
  fprintf(fp, "CHECKING\n\n");
  warn = 0;
  for (addr = rel->base; addr < rel->base + bin->size; ++addr)
  {
    int ok = 1;
    Byte relo = rel_relo(rel, addr);
    switch (relo)
    {
      case 1: ok = (bin_byte(bin, addr) == (rel->addr[addr - rel->base] & 0xff)); break;
      case 2: ok = (bin_byte(bin, addr) == (rel->addr[addr - rel->base] >> 8));   break;
      case 3: ok = (bin_addr(bin, addr) == rel->addr[addr - rel->base]);          break;
    }
    if (!ok)
    {
      Addr lfa = bin_lfa(wds, addr);
      Addr nfa = bin_lfa_to_nfa(bin, lfa);
      // (:) HERE LASTXT FORTH have modified literal addresses in a saved state; warnings can be safely ignored
      if (bin_name(bin, nfa, "(:)") == 0 &&
          bin_name(bin, nfa, "HERE") == 0 &&
          bin_name(bin, nfa, "LASTXT") == 0 &&
          bin_name(bin, nfa, "FORTH") == 0)
      {
        Addr pfa = bin_lfa_to_pfa(bin, lfa);
        fprintf(fp, "warning: bin file literal address does not match rel file for ");
        print_nt(fp, bin, nfa);
        fprintf(fp, " at %0.4X (%u bytes offset from pfa)\n", addr, pfa - addr);
        ++warn;
      }
    }
  }
  wds->docol = 0;
  wds->doret = 0;
  wds->dovar = 0;
  wds->doval = 0;
  wds->dotwoval = 0;
  wds->docon = 0;
  wds->dotwocon = 0;
  wds->dodef = 0;
  wds->dotick = 0;
  wds->dolit = 0;
  wds->dotwolit = 0;
  wds->doquote = 0;
  wds->doto = 0;
  wds->doplusto = 0;
  wds->dotwoto = 0;
  wds->dodplusto = 0;
  wds->doif = 0;
  wds->doof = 0;
  wds->doahead = 0;
  wds->doagain = 0;
  wds->dountil = 0;
  wds->dodo = 0;
  wds->doqdo = 0;
  wds->doloop = 0;
  wds->doplusloop = 0;
  wds->dosemicolondoes = 0;
  wds->repl = 0;
  wds->error = 0;
  wds->handler = 0;
  wds->throw = 0;
  wds->bye = 0;
  wds->main = 0;
  for (i = 0; i < wds->size; ++i)
  {
    Addr lfa = wds->addr[i];
    Addr nfa = lfa + 2;
    Byte len = bin_byte(bin, nfa) & 0x3f;
    Addr cfa = nfa + 1 + len;
    if (bin_name(bin, nfa, "(:)"))
      wds->docol = cfa;
    else if (bin_name(bin, nfa, "(;)"))
      wds->doret = cfa;
    else if (bin_name(bin, nfa, "(VAR)"))
      wds->dovar = cfa;
    else if (bin_name(bin, nfa, "(VAL)"))
      wds->doval = cfa;
    else if (bin_name(bin, nfa, "(2VAL)"))
      wds->dotwoval = cfa;
    else if (bin_name(bin, nfa, "(CON)"))
      wds->docon = cfa;
    else if (bin_name(bin, nfa, "(2CON)"))
      wds->dotwocon = cfa;
    else if (bin_name(bin, nfa, "(DEF)"))
      wds->dodef = cfa;
    else if (bin_name(bin, nfa, "(')"))
      wds->dotick = cfa;
    else if (bin_name(bin, nfa, "(LIT)"))
      wds->dolit = cfa;
    else if (bin_name(bin, nfa, "(2LIT)"))
      wds->dotwolit = cfa;
    else if (bin_name(bin, nfa, "(\")"))
      wds->doquote = cfa;
    else if (bin_name(bin, nfa, "(TO)"))
      wds->doto = cfa;
    else if (bin_name(bin, nfa, "(+TO)"))
      wds->doplusto = cfa;
    else if (bin_name(bin, nfa, "(2TO)"))
      wds->dotwoto = cfa;
    else if (bin_name(bin, nfa, "(D+TO)"))
      wds->dodplusto = cfa;
    else if (bin_name(bin, nfa, "(IF)"))
      wds->doif = cfa;
    else if (bin_name(bin, nfa, "(OF)"))
      wds->doof = cfa;
    else if (bin_name(bin, nfa, "(AHEAD)"))
      wds->doahead = cfa;
    else if (bin_name(bin, nfa, "(AGAIN)"))
      wds->doagain = cfa;
    else if (bin_name(bin, nfa, "(UNTIL)"))
      wds->dountil = cfa;
    else if (bin_name(bin, nfa, "(DO)"))
      wds->dodo = cfa;
    else if (bin_name(bin, nfa, "(?DO)"))
      wds->doqdo = cfa;
    else if (bin_name(bin, nfa, "(LOOP)"))
      wds->doloop = cfa;
    else if (bin_name(bin, nfa, "(+LOOP)"))
      wds->doplusloop = cfa;
    else if (bin_name(bin, nfa, "(;DOES)"))
      wds->dosemicolondoes = cfa;
    else if (bin_name(bin, nfa, "REPL"))
      wds->repl = cfa;
    else if (bin_name(bin, nfa, "ERROR"))
      wds->error = cfa;
    else if (bin_name(bin, nfa, "HANDLER"))
      wds->handler = cfa;
    else if (bin_name(bin, nfa, "THROW"))
      wds->throw = cfa;
    else if (bin_name(bin, nfa, "BYE"))
      wds->bye = cfa;
    else if (bin_name(bin, nfa, flag_main))
      wds->main = cfa;
  }
  fflush(stdout);
  errs = 0;
  if (wds->docol == 0)
  {
    fprintf(stderr, "(:) not found\n");
    ++errs;
  }
  if (wds->doret == 0)
  {
    fprintf(stderr, "(;) not found\n");
    ++errs;
  }
  if (wds->dovar == 0)
  {
    fprintf(stderr, "(VAR) not found\n");
    ++errs;
  }
  if (wds->doval == 0)
  {
    fprintf(stderr, "(VAL) not found\n");
    ++errs;
  }
  if (wds->dotwoval == 0)
  {
    fprintf(stderr, "(2VAL) not found\n");
    ++errs;
  }
  if (wds->docon == 0)
  {
    fprintf(stderr, "(CON) not found\n");
    ++errs;
  }
  if (wds->dotwocon == 0)
  {
    fprintf(stderr, "(2CON) not found\n");
    ++errs;
  }
  if (wds->dodef == 0)
  {
    fprintf(stderr, "(DEF) not found\n");
    ++errs;
  }
  if (wds->dotick == 0)
  {
    fprintf(stderr, "(') not found\n");
    ++errs;
  }
  if (wds->dolit == 0)
  {
    fprintf(stderr, "(LIT) not found\n");
    ++errs;
  }
  if (wds->dotwolit == 0)
  {
    fprintf(stderr, "(2LIT) not found\n");
    ++errs;
  }
  if (wds->doquote == 0)
  {
    fprintf(stderr, "(\") not found\n");
    ++errs;
  }
  if (wds->doto == 0)
  {
    fprintf(stderr, "(TO) not found\n");
    ++errs;
  }
  if (wds->doplusto == 0)
  {
    fprintf(stderr, "(+TO) not found\n");
    ++errs;
  }
  if (wds->dotwoto == 0)
  {
    fprintf(stderr, "(2TO) not found\n");
    ++errs;
  }
  if (wds->dodplusto == 0)
  {
    fprintf(stderr, "(D+TO) not found\n");
    ++errs;
  }
  if (wds->doif == 0)
  {
    fprintf(stderr, "(IF) not found\n");
    ++errs;
  }
  if (wds->doof == 0)
  {
    fprintf(stderr, "(OF) not found\n");
    ++errs;
  }
  if (wds->doahead == 0)
  {
    fprintf(stderr, "(AHEAD) not found\n");
    ++errs;
  }
  if (wds->doagain == 0)
  {
    fprintf(stderr, "(AGAIN) not found\n");
    ++errs;
  }
  if (wds->dountil == 0)
  {
    fprintf(stderr, "(UNTIL) not found\n");
    ++errs;
  }
  if (wds->dodo == 0)
  {
    fprintf(stderr, "(DO) not found\n");
    ++errs;
  }
  if (wds->doqdo == 0)
  {
    fprintf(stderr, "(?DO) not found\n");
    ++errs;
  }
  if (wds->doloop == 0)
  {
    fprintf(stderr, "(LOOP) not found\n");
    ++errs;
  }
  if (wds->doloop == 0)
  {
    fprintf(stderr, "(+LOOP) not found\n");
    ++errs;
  }
  if (wds->dosemicolondoes == 0)
  {
    fprintf(stderr, "(;DOES) not found\n");
    ++errs;
  }
  if (wds->repl == 0)
  {
    fprintf(stderr, "REPL not found\n");
    ++errs;
  }
  if (wds->error == 0)
  {
    fprintf(stderr, "ERROR not found\n");
    ++errs;
  }
  if (wds->handler == 0)
  {
    fprintf(stderr, "HANDLER not found\n");
    ++errs;
  }
  if (wds->throw == 0)
  {
    fprintf(stderr, "THROW not found\n");
    ++errs;
  }
  if (wds->bye == 0)
  {
    fprintf(stderr, "BYE not found\n");
    ++errs;
  }
  if (errs > 0)
    exit(EXIT_FAILURE);
  // check if REPL is defined in the words file to set flag -repl
  if (wds_info(bin, rel, wds, bin_lfa(wds, wds->repl), 0, 0) == 1)
    flag_repl = 1;
  // analyze words and find addresses not in rel file to target for relocation
  fprintf(fp, "\nANALYZING\n\n");
  for (i = 0; i < wds->size; ++i)
  {
    Addr lfa = wds->addr[i];
    Addr nfa = bin_lfa_to_nfa(bin, lfa);
    Addr cfa = bin_lfa_to_cfa(bin, lfa);
    Addr end = wds->addr[i + 1];
    fprintf(fp, "%0.4X ", lfa);
    if (bin_byte(bin, cfa) == 0xcd)
    {
      Addr call = bin_addr(bin, cfa + 1);
      if (cfa + 1 >= rel->last)
        rel_relo_addr(rel, cfa + 1, call);
      if (call == wds->docol)
      {
        Addr addr;
        fprintf(fp, "     : ");
        print_nt(fp, bin, nfa);
        for (addr = cfa + 3; ; addr += 2)
        {
          Addr xt = bin_addr(bin, addr);
          fprintf(fp, "\n            ");
          print_nt(fp, bin, bin_lfa_to_nfa(bin, bin_lfa(wds, xt)));
          fprintf(fp, " %0.4X ", xt);
          if (addr >= rel->last)
            rel_relo_addr(rel, addr, xt);
          if (xt == wds->doret)
            break;
          if (xt == wds->dotick)
          {
            Addr lit = bin_addr(bin, addr + 2);
            Addr lfa = bin_lfa(wds, lit);
            Addr nfa = bin_lfa_to_nfa(bin, lfa);
            fprintf(fp, "= ");
            print_nt(fp, bin, nfa);
            fprintf(fp, " %0.4X", lit);
            if (addr >= rel->last)
              rel_relo_addr(rel, addr + 2, lit);
            addr += 2;
          }
          else if (xt == wds->dolit)
          {
            Addr lit = bin_addr(bin, addr + 2);
            int test = test_addr(bin, rel, addr + 2);
            if (test == 2)
            {
              fprintf(fp, "warning: possible address %0.4X ignored and not relocated", lit);
              ++warn;
            }
            else
            {
              fprintf(fp, "= %d (%0.4X)", lit, lit);
            }
            addr += 2;
          }
          else if (xt == wds->dotwolit)
          {
            Addr litlo, lithi = bin_addr(bin, addr + 2);
            int test = test_addr(bin, rel, addr + 2);
            if (test == 2)
            {
              fprintf(fp, "warning: first cell possible address %0.4X ignored and not relocated,", lithi);
              ++warn;
            }
            else
            {
              fprintf(fp, "= %d,", lithi);
            }
            litlo = bin_addr(bin, addr + 4);
            test = test_addr(bin, rel, addr + 4);
            if (test == 2)
            {
              fprintf(fp, "warning: second cell possible address %0.4X ignored and not relocated", litlo);
              ++warn;
            }
            else
            {
              fprintf(fp, "%d (%0.8X)", litlo, (lithi << 16) + litlo);
            }
            addr += 4;
          }
          else if (xt == wds->doquote)
          {
            Byte len = bin_byte(bin, addr + 2);
            fprintf(fp, "%u chars", len);
            if (addr >= rel->last)
              rel_relo_addr(rel, addr, xt);
            addr += 1 + len;
          }
          else if (xt == wds->doto || xt == wds->doplusto)
          {
            Addr lit = bin_addr(bin, addr + 2);
            Addr lfa = bin_lfa(wds, lit);
            Addr nfa = bin_lfa_to_nfa(bin, lfa);
            fprintf(fp, "VAL ");
            print_nt(fp, bin, nfa);
            fprintf(fp, " %0.4X", lit);
            if (addr >= rel->last)
              rel_relo_addr(rel, addr + 2, lit);
            addr += 2;
          }
          else if (xt == wds->dotwoto || xt == wds->dodplusto)
          {
            Addr lit = bin_addr(bin, addr + 2);
            Addr lfa = bin_lfa(wds, lit);
            Addr nfa = bin_lfa_to_nfa(bin, lfa);
            fprintf(fp, "2VAL ");
            print_nt(fp, bin, nfa);
            fprintf(fp, " %0.4X", lit);
            if (addr >= rel->last)
              rel_relo_addr(rel, addr + 2, lit);
            addr += 2;
          }
          else if (xt == wds->doif || xt == wds->doof || xt == wds->doahead
              || xt == wds->doagain || xt == wds->dountil || xt == wds->dodo
              || xt == wds->doqdo || xt == wds->doloop || xt == wds->doplusloop)
          {
            Addr jmp = bin_addr(bin, addr + 2);
            fprintf(fp, "-> %0.4X", jmp);
            if (addr >= rel->last)
              rel_relo_addr(rel, addr + 2, jmp);
            addr += 2;
          }
          else if (xt == wds->dosemicolondoes)
          {
            Addr jmp = bin_addr(bin, addr + 3);
            fprintf(fp, "CALL (DOES) %0.4X", jmp);
            if (addr >= rel->last)
              rel_relo_addr(rel, addr + 3, jmp);
            addr += 3;
          }
          else if (xt == wds->repl || xt == wds->error)
          {
            // if headless MAIN, replace ERROR and REPL with BYE in THROW
            if (wds->main && !flag_repl)
            {
              if (cfa == wds->throw)
              {
                xt = wds->bye;
                bin->data[addr - bin->base + 0] = xt & 0xff;
                bin->data[addr - bin->base + 1] = xt >> 8;
                rel_relo_addr(rel, addr, xt);
              }
            }
          }
        }
      }
      else if (call == wds->dovar)
      {
        Addr pfa = cfa + 3;
        Addr lit;
        int test = test_addr(bin, rel, pfa);
        // reset HANDLER when set
        if (cfa == wds->handler)
          bin_set_addr(bin, pfa, 0);
        fprintf(fp, "   VAR ");
        print_nt(fp, bin, nfa);
        lit = bin_addr(bin, pfa);
        if (wds_info(bin, rel, wds, lfa, pfa, end) == 2)
        {
          Addr addr;
          fprintf(fp, " {");
          for (addr = pfa; addr < end; ++addr)
            if (rel_relo(rel, addr))
              fprintf(fp, " %u:%0.4X", addr - pfa, rel_addr(rel, addr));
          fprintf(fp, " }");
        }
        else if (pfa < end && end > rel->last)
        {
          fprintf(fp, " warning: %u bytes of data ignored", end - pfa);
          if (test == 2)
            fprintf(fp, " with possible address %0.4X", lit);
          ++warn;
        }
      }
      else if (call == wds->doval)
      {
        Addr pfa = cfa + 3;
        Addr lit = bin_addr(bin, pfa);
        int test = test_addr(bin, rel, pfa);
        fprintf(fp, "   VAL ");
        print_nt(fp, bin, nfa);
        if (wds_info(bin, rel, wds, lfa, pfa, end) == 2)
        {
          fprintf(fp, " {");
          for (addr = pfa; addr < end; ++addr)
            if (rel_relo(rel, pfa))
              fprintf(fp, " 0:%0.4X", rel_addr(rel, pfa));
          fprintf(fp, " }");
        }
        else if (test == 1)
        {
          fprintf(fp, " address %0.4X", lit);
        }
        else if (test == 2)
        {
          fprintf(fp, " warning: possible address %0.4X utilized and relocated", lit);
          rel_relo_addr(rel, pfa, lit);
          ++warn;
        }
        else
        {
          fprintf(fp, " %d", lit);
        }
      }
      else if (call == wds->dotwoval)
      {
        Addr pfa = cfa + 3;
        Addr litlo, lithi = bin_addr(bin, pfa);
        int test = test_addr(bin, rel, pfa);
        fprintf(fp, "  2VAL ");
        print_nt(fp, bin, nfa);
        if (wds_info(bin, rel, wds, lfa, pfa, end) == 2)
        {
          fprintf(fp, " {");
          for (addr = pfa; addr < end; addr += 2)
            if (rel_relo(rel, pfa))
              fprintf(fp, " 0:%0.4X", rel_addr(rel, pfa));
          fprintf(fp, " }");
        }
        else
        {
          if (test == 1)
          {
            fprintf(fp, " address %0.4X", lithi);
          }
          else if (test == 2)
          {
            fprintf(fp, " warning: first cell possible address %0.4X utilized and relocated", lithi);
            rel_relo_addr(rel, pfa, lithi);
            ++warn;
          }
          else
          {
            fprintf(fp, " %d", lithi);
          }
          litlo = bin_addr(bin, pfa + 2);
          test = test_addr(bin, rel, pfa + 2);
          if (test == 1)
          {
            fprintf(fp, " address %0.4X", litlo);
          }
          else if (test == 2)
          {
            fprintf(fp, " warning: second cell possible address %0.4X utilized and relocated", litlo);
            rel_relo_addr(rel, pfa + 2, litlo);
            ++warn;
          }
          else
          {
            fprintf(fp, ",%d (%0.8X)", litlo, (lithi << 16) + litlo);
          }
        }
      }
      else if (call == wds->docon)
      {
        Addr pfa = cfa + 3;
        Addr lit = bin_addr(bin, pfa);
        int test = test_addr(bin, rel, pfa);
        fprintf(fp, " CONST ");
        print_nt(fp, bin, nfa);
        if (test == 1)
        {
          fprintf(fp, " address %0.4X", lit);
        }
        else if (test == 2)
        {
          fprintf(fp, " warning: possible address %0.4X ignored and not relocated", lit);
          ++warn;
        }
        else
        {
          fprintf(fp, " %d", lit);
        }
      }
      else if (call == wds->dotwocon)
      {
        Addr pfa = cfa + 3;
        Addr litlo, lithi = bin_addr(bin, pfa);
        int test = test_addr(bin, rel, pfa);
        fprintf(fp, "2CONST ");
        print_nt(fp, bin, nfa);
        if (test == 1)
        {
          fprintf(fp, " address %0.4X,", lithi);
        }
        else if (test == 2)
        {
          fprintf(fp, " warning: first cell possible address %0.4X ignored and not relocated,", lithi);
          ++warn;
        }
        else
        {
          fprintf(fp, " %d", lithi);
        }
        litlo = bin_addr(bin, pfa + 2);
        test = test_addr(bin, rel, pfa + 2);
        if (test == 1)
        {
          fprintf(fp, "address %0.4X", litlo);
        }
        else if (test == 2)
        {
          fprintf(fp, "warning: second cell possible address %0.4X ignored and not relocated", litlo);
          ++warn;
        }
        else
        {
          fprintf(fp, ",%d (%0.8X)", litlo, (lithi << 16) + litlo);
        }
      }
      else if (call == wds->dodef)
      {
        Addr pfa = cfa + 3;
        Addr jmp = bin_addr(bin, pfa);
        fprintf(fp, " DEFER ");
        print_nt(fp, bin, nfa);
        fprintf(fp, " IS ");
        lfa = bin_lfa(wds, jmp);
        nfa = bin_lfa_to_nfa(bin, lfa);
        print_nt(fp, bin, nfa);
        fprintf(fp, " %0.4X", jmp);
        if (addr >= rel->last)
          rel_relo_addr(rel, pfa, jmp);
      }
      else
      {
        Addr call = bin_addr(bin, cfa + 1);
        int test = test_addr(bin, rel, cfa + 1);
        if (cfa >= rel->last || test > 0)
        {
          Addr pfa = cfa + 3;
          Addr lit = bin_addr(bin, pfa);
          Addr lfx = bin_lfa(wds, call);
          Addr nfx = bin_lfa_to_nfa(bin, lfx);
          fprintf(fp, "CREATE ");
          print_nt(fp, bin, nfa);
          fprintf(fp, " DOES> ");
          print_nt(fp, bin, nfx);
          fprintf(fp, " %0.4X", call);
          if (cfa + 1 >= rel->last)
            rel_relo_addr(rel, cfa + 1, call);
          if (wds_info(bin, rel, wds, lfx, pfa, end) == 2)
          {
            Addr addr;
            fprintf(fp, " {");
            for (addr = pfa; addr < end; ++addr)
              if (rel_relo(rel, addr))
                fprintf(fp, " %u:%0.4X", addr - pfa, rel_addr(rel, addr));
            fprintf(fp, " }");
          }
          else if (pfa < end && end > rel->last)
          {
            fprintf(fp, " warning: %u bytes of data ignored", end - pfa);
            if (test == 2)
              fprintf(fp, " with possible address %0.4X", lit);
            ++warn;
          }
        }
        else
        {
          fprintf(fp, "  CODE ");
          print_nt(fp, bin, nfa);
        }
      }
    }
    else if (cfa == wds->docol && bin_byte(bin, cfa + 10) == 0x3a && bin_byte(bin, cfa + 17) == 0x0a && flag_break)
    {
      // in (:) copy EXEC 7 bytes to remove STOP detection, when RPIX=0
      fprintf(fp, "  CODE ");
      print_nt(fp, bin, nfa);
      fprintf(fp, " (with break on STOP removed)");
      memmove(bin->data + cfa + 10 - bin->base, bin->data + cfa + 10 + 7 - bin->base, 7);
    }
    else if (cfa == wds->docol && bin_byte(bin, cfa + 11) == 0x3a && bin_byte(bin, cfa + 18) == 0x0a && flag_break)
    {
      // in (:) copy EXEC 7 bytes to remove STOP detection, when RPIX=1
      fprintf(fp, "  CODE ");
      print_nt(fp, bin, nfa);
      fprintf(fp, " (with break on STOP removed)");
      memmove(bin->data + cfa + 11 - bin->base, bin->data + cfa + 11 + 7 - bin->base, 7);
    }
    else
    {
      fprintf(fp, "  CODE ");
      print_nt(fp, bin, nfa);
      // unconditional jr in Forth internals at the end of the CODE word?
      if (end <= rel->last && bin_byte(bin, end - 3) != 0xc3 && bin_byte(bin, end - 2) == 0x18)
      {
        Addr jmp = end + (int8_t)bin_byte(bin, end - 1);
        if (jmp < lfa || jmp >= end)
        {
          Addr lfx = bin_lfa(wds, jmp);
          Addr nfx = bin_lfa_to_nfa(bin, lfx);
          rel_jr_addr(rel, end - 1, jmp);
        }
      }
    }
    fprintf(fp, "\n");
  }
  if (warn)
    printf("%d warnings logged in %s\n", warn, bin->logfile);
  else
    printf("no warnings in %s\n", bin->logfile);
  // mark cold boot area
  for (addr = rel->boot; addr < rel->warm; ++addr)
    if (rel_relo(rel, addr) == 3)
      bin_mark(bin, wds, bin_addr(bin, addr), 1);
  // mark MAIN when defined, otherwise mark REPL
  if (wds->main)
    bin_mark(bin, wds, wds->main, 1);
  if (!wds->main || flag_repl)
    bin_mark(bin, wds, wds->repl, 1);
  // mark words in the words info file
  if (wds->data != NULL)
  {
    for (i = 0; i < wds->size; ++i)
    {
      Addr lfa = wds->addr[i];
      Addr pfa = bin_lfa_to_pfa(bin, lfa);
      Addr end = wds->addr[i + 1];
      if (wds_info(bin, rel, wds, lfa, 0, 0) == 1)
        bin_mark(bin, wds, lfa, 1);
    }
  }
  // mark dictionary words recursively
  fprintf(fp, "\nSLICING\n\n");
  changes = 1;
  while (changes)
  {
    changes = 0;
    for (i = 0; i < wds->size; ++i)
    {
      Addr lfa = wds->addr[i];
      if (bin_marked(bin, lfa) == 1)
      {
        Addr nfa = bin_lfa_to_nfa(bin, lfa);
        Addr cfa = bin_lfa_to_cfa(bin, lfa);
        Addr end = wds->addr[i + 1];
        print_nt(fp, bin, nfa);
        fprintf(fp, " ->");
        for (addr = cfa + 1; addr < end; ++addr)
        {
          Byte relo = rel_relo(rel, addr);
          if (relo >= 3)
          {
            Addr xt = (relo == 3 ? bin_addr(bin, addr) : rel_addr(rel, addr));
            Addr lfx = bin_lfa(wds, xt);
            Addr nfx = bin_lfa_to_nfa(bin, lfx);
            Addr cfx = bin_lfa_to_cfa(bin, lfx);
            if (cfx != cfa)
            {
              fprintf(fp, " ");
              if (lfx)
                print_nt(fp, bin, nfx);
              else
                fprintf(fp, "%0.4X", xt);
            }
            bin_mark(bin, wds, xt, 1);
          }
        }
        fprintf(fp, "\n");
        bin_mark(bin, wds, lfa, 2);
        changes = 1;
      }
    }
  }
  // show slice
  printf("\nslice:");
  for (i = 0; i < wds->size; ++i)
  {
    Addr lfa = wds->addr[i];
    if (bin_marked(bin, lfa))
    {
      Addr nfa = bin_lfa_to_nfa(bin, lfa);
      printf(" ");
      print_nt(stdout, bin, nfa);
    }
  }
  // overwrite warm start REPL with MAIN
  if (wds->main)
    here = rel->warm + 7; // call docol MAIN BYE
  else
    here = rel->warm + 3; // jp REPL
  dict = here;
  // when REPL is used, then keep REPL and the dictionary links for words that are used
  keep = bin_marked(bin, wds->repl);
  printf("\n\nsliced headless or with interpreter: %s\n", keep ? "interpreter" : "headless");
  // determine relocated address of cfa and relink the sliced words
  fprintf(fp, "\nRELOCATING\n\n");
  for (i = 0; i < wds->size; ++i)
  {
    Addr lfa = wds->addr[i];
    if (bin_marked(bin, lfa))
    {
      Addr cfa = bin_lfa_to_cfa(bin, lfa);
      Addr nfa = bin_lfa_to_nfa(bin, lfa);
      Addr end = wds->addr[i + 1];
      Addr from = (keep ? lfa : cfa);
      Addr dist = from - here;
      print_nt(fp, bin, nfa);
      fprintf(fp, " %0.4X -> %0.4X\n", lfa, move + lfa - dist);
      for (addr = lfa; addr < end; ++addr)
        rel_move(rel, addr, move + addr - dist);
      if (keep)
      {
        Addr link = bin_addr(bin, lfa);
        while (link != 0 && !bin_marked(bin, link))
          link = bin_addr(bin, link);
        if (link != 0)
          bin_set_addr(bin, lfa, rel_dest(rel, link));
      }
      here += end - from;
    }
  }
  // relocate end address
  rel_move(rel, wds->addr[wds->size], move + here);
  // move start address too high?
  if (move >= TOP - here)
  {
    printf("ERROR: new start address %0.4X too high >= %0.4X by %u bytes\n", flag_start, TOP, move + here - TOP);
    exit(EXIT_FAILURE);
  }
  // new start address
  if (flag_start != 0)
    printf("relocate to new bload start address %0.4X\n", flag_start);
  // overwrite warm start REPL with MAIN
  if (wds->main)
  {
    printf("modifying boot up to run %s%s\n", (keep ? "" : "headless "), flag_main);
    // call docol
    bin_set_byte(bin, rel->warm, 0xcd);
    bin_set_addr(bin, rel->warm + 1, rel_dest(rel, wds->docol));
    // MAIN
    bin_set_addr(bin, rel->warm + 3, rel_dest(rel, wds->main));
    // BYE
    bin_set_addr(bin, rel->warm + 5, rel_dest(rel, wds->bye));
  }
  else
  {
    printf("modifying boot up for interactive REPL interpreter\n");
    // jp REPL
    bin_set_byte(bin, rel->warm, 0xc3);
    bin_set_addr(bin, rel->warm + 1, rel_dest(rel, wds->repl));
  }
  // break is modified
  if (flag_break)
    printf("modifying runtime to disable break\n");
  // relocate cold boot guts
  printf("relocating boot area at %0.4X to %0.4X (%u bytes)\n", rel->boot, move + rel->boot, rel->warm - rel->boot);
  for (addr = rel->boot; addr < rel->warm; ++addr)
    if (rel_relo(rel, addr) == 3)
      bin_set_addr(bin, addr, rel_dest(rel, bin_addr(bin, addr)));
  // relocate sliced words guts
  printf("relocating dictionary words at %0.4X to %0.4X (%u bytes)\n", wds->addr[0], move + dict, here - rel->warm);
  for (i = 0; i < wds->size; ++i)
  {
    Addr lfa = wds->addr[i];
    if (bin_marked(bin, lfa))
    {
      Addr cfa = bin_lfa_to_cfa(bin, lfa);
      Addr end = wds->addr[i + 1];
      for (addr = cfa; addr < end; ++addr)
      {
        Byte relo = rel_relo(rel, addr);
        if (relo == 1)
        {
          // update LSB address in bin file to relocated address dest
          Addr dest = rel_dest(rel, rel_addr(rel, addr));
          bin_set_byte(bin, addr, dest & 0xff);
        }
        else if (relo == 2)
        {
          // update MSB address in bin file to relocated address dest
          Addr dest = rel_dest(rel, rel_addr(rel, addr));
          bin_set_byte(bin, addr, dest >> 8);
        }
        else if (relo == 3)
        {
          // update address in bin file to relocated address dest
          Addr dest = rel_dest(rel, bin_addr(bin, addr));
          bin_set_addr(bin, addr, dest);
        }
        else if (relo == 4)
        {
          // update relative distance of jr in bin file (can only get closer)
          Addr dest = rel_dest(rel, rel_addr(rel, addr));
          Addr jr_dest = rel_dest(rel, cfa) + addr - cfa - 1; // relocated address of jr
          Byte rel = (int8_t)(dest - jr_dest - 2);
          bin_set_byte(bin, addr, rel);
        }
      }
    }
  }
  fprintf(fp, "\nCOMPLETED\n");
  fclose(fp);
  // update MSX bin header to the new end address
  printf("creating new header (7 bytes)\n");
  bin_set_addr(bin, bin->base + 1, move + bin->base + 7);
  bin_set_addr(bin, bin->base + 3, move + here);
  bin_set_addr(bin, bin->base + 5, move + bin->boot);
  // save new bin file
  printf("saving Forth program slice to %s\n", bin->outfile);
  fp = fopen(bin->outfile, "wb");
  if (fp == NULL)
  {
    perror("cannot open bin file for writing");
    exit(EXIT_FAILURE);
  }
  if (wds->main)
    addr = rel->warm + 7; // call docol MAIN BYE
  else
    addr = rel->warm + 3; // jp REPL
  if (fwrite(bin->data, 1, addr - rel->base, fp) < addr - rel->base)
  {
    perror("cannot write bin file");
    exit(EXIT_FAILURE);
  }
  sliced = 0;
  for (i = 0; i < wds->size; ++i)
  {
    Addr lfa = wds->addr[i];
    if (bin_marked(bin, lfa))
    {
      Addr cfa = bin_lfa_to_cfa(bin, lfa);
      Addr end = wds->addr[i + 1];
      Addr from = (keep ? lfa : cfa);
      Addr size = end - from;
      if (fwrite(bin->data + from - bin->base, 1, size, fp) < size)
      {
        perror("cannot write bin file");
        exit(EXIT_FAILURE);
      }
      ++sliced;
    }
  }
  fclose(fp);
  printf("original words total = %u\n", wds->size);
  printf("retained words total = %u (%.1f%%)\n", sliced, 100.0 * sliced / wds->size);
  printf("original binary size = %u bytes\n", bin->size);
  printf("retained binary size = %u bytes (%.1f%%)\n", here - bin->base, 100.0 * (here - bin->base) / bin->size);
}

void usage()
{
  puts("\
SYNOPSIS\n\
     forslice [options] INFILE.BIN [OUTFILE.BIN]\n\
\n\
DESCRIPTION\n\
     Slice ForthMSX saved-state binary INFILE.BIN by removing unused words to\n\
     create a headless program slice saved to OUTFILE.BIN or to SLICE.BIN when\n\
     OUTFILE.BIN is not specified.  Unused words are those that cannot be\n\
     reached on any execution path from MAIN or from the word specified with\n\
     option -main instead of MAIN.  Forslice saves a log file forslice.log in\n\
     the working directory.  Forslice requires the forth.rel file located in the\n\
     working directory to relocate the Forth internals.  When warnings are\n\
     given, it may be necessary to specify a words info file with option\n\
     -words.\n\
\n\
OPTIONS\n\
     -repl              include REPL interpreter and dictionary (not headless)\n\
     -break             disable (CTRL-)STOP break\n\
     -main <name>       run <name> as the main program, default name is MAIN\n\
     -start <addr>      new binary start address to bload the program slice\n\
     -words <file.inf>  use words info file to assist slicing and relocation\n\
\n\
     If MAIN or the word specified by -main is missing, then REPL will be run.\n\
\n\
     A words info file contains a list of Forth words with an optional info\n\
     field separated by spaces and/or newlines.  For example, we can specify\n\
     some words that we want to include in the slice as well as all words that\n\
     those words depend on to run:\n\
\n\
     REPL\n\
     words .s PAGE\n\
\n\
     The first line is the same as option -repl.  ForthMSX word names are case\n\
     insensitive.\n\
\n\
     An info field of the form {...} is used for VAR, VAL and 2VAL words and\n\
     for words defined with CREATE DOES>.  The info field must not contain\n\
     spacing and specifies a list of offsets into the data of those words where\n\
     Forth addresses (pointers to data) are located that must be relocated\n\
     properly for the program slice to run reliably.\n\
\n\
     For example, words created by MARKER and VOCABULARY have pointers in their\n\
     data at certain byte-aligned offsets from their pfa (parameter field\n\
     address given by >BODY) into their data:\n\
\n\
     MARKER {0,2,4}\n\
     VOCABULARY {0,4}\n\
\n\
     These pointers at the specified offsets must remain valid after program\n\
     relocation.  Therefore, these two lines are required to properly slice and\n\
     relocate all `MARKER`-defined words and `VOCABULARY`-defined words in the\n\
     sliced saved state, when these are used in the slice.  This is not\n\
     necessary When none are used in the slice.  Check the saved forslice.log\n\
     for details.\n\
");
}

int main(int argc, char **argv)
{
  while (argc > 1 && *argv[1] == '-')
  {
    if (strcmp(argv[1], "-repl") == 0)
    {
      flag_repl = 1;
    }
    else if (strcmp(argv[1], "-break") == 0)
    {
      flag_break = 1;
    }
    else if (argc > 2 && strcmp(argv[1], "-start") == 0)
    {
      flag_start = strtoul(argv[2], NULL, 0);
      --argc;
      ++argv;
    }
    else if (argc > 2 && strcmp(argv[1], "-main") == 0)
    {
      flag_main = argv[2];
      --argc;
      ++argv;
    }
    else if (argc > 2 && strcmp(argv[1], "-words") == 0)
    {
      wds.file = argv[2];
      --argc;
      ++argv;
    }
    else
    {
      printf("unknown option %s\n", argv[1]);
      exit(EXIT_FAILURE);
    }
    --argc;
    ++argv;
  }
  if (argc <= 1)
  {
    usage();
    exit(EXIT_SUCCESS);
  }
  bin.infile = argv[1];
  bin.outfile = argc > 2 ? argv[2] : "SLICE.BIN";
  bin.logfile = "forslice.log";
  rel.file = "forth.rel";
  read_bin(&bin);
  read_rel(&rel, bin.size);
  read_wds(&wds);
  slice(&bin, &rel, &wds);
}
