
```nasm
; after enjoying sectorforth and milliforth, I wondered:
; how much useful (and flexible!) forth can I cram into
; 510 bytes while being fun to read and hack on?
```

nictoforth
==========

Nick's 16-bit x86 bootsector Forth.

If you're impatient: `nix-shell` then
`make run` to jump in. Though:

**Heads up:** this is more an art piece exploring a
constrained problem space than something you'd
wanna write software for.

...the hell is an "x86 bootsector Forth"?
-----------------------------------------

### Forth ⛰️

Is an old and grumpy programming language.
I love it to death.

It has a reputation for being impenetrable because you
must mentally track a stack of values, **but the real
sauce is the dictionary:** you're given the tools to
extend the compiler and interpreter in the language
itself. More collaborator than consumer. I could talk
your ear off.

**📚 Learn more:** | [Wikipedia][wik] | the beloved
[Starting Forth][sta] | the dense [ANS Forth glossary][ans]

[wik]: https://en.wikipedia.org/wiki/Forth_(programming_language)
[sta]: https://www.forth.com/starting-forth/
[ans]: https://forth-standard.org/standard/alpha

Forth was made to be a practical tool to solve problems.
nictoforth, however, is more of an exploration of how much
readable language can fit entirely within the:

### Boot sector 💾

Of a floppy disk. A BIOS loads the sector into memory
and jumps to the code when you turn your PC on.
Honestly I don't know the details but it looked like
fun. Only 510 bytes! Written in:

### x86 ⚡

An instruction set. Intel and AMD processors run
programs encoded in x86 at their lowest-ish level. Most
phones run ARM programs. The NES, the Commodore 64, and
Bender Bending Rodríguez run 6502 programs. There's
lots.

### "nictoforth"?

You'll have to [read the source][asm].
Can't spoil all the surprises.

*Inspired by the lovely [sectorforth][sec],
[milliforth][mil], and [durexforth][dur].*

[asm]: nicto.asm
[fs]:  hello.fs
[sec]: https://github.com/cesarblum/sectorforth
[mil]: https://github.com/fuzzballcat/milliForth
[dur]: https://github.com/jkotlinski/durexforth

What can it do? 🔍
------------------

After [proper bootstrap][fs] the following words are
available:

```
[nix-shell]$ make words
2+ 2u/ nand invert 0= + drop dup swap >r r>
>in dp sp@ rp@ @ ! key emit line lex find execute
abort quit head, , ] compile, ; exit immediate ;
```

- `line` gets input, so to a first approximation is
  `0 4096 accept`,
- `lex` is nonstandard `parse-name`: it rewinds `>in`
  onto the delimiter,
- `find` is very nonstandard (see below),
- The second `;` is the bootstrapper, but note:
- There is **no builtin number parser!** You'll have to
  calculate numbers until you can write one in Forth.

But even before a number parser:

```
lex 3 drop @ \ "lex 3" gives an ( addr len ) in the
      \ input buffer, len is dropped and the '3' digit
      \ and space are fetched: ( $2033 )
2+    \ ( $2035 )
emit  \ prints: 5. See hello.fs for lots more.
```

For source reading aides try:

```
[nix-shell]$ make targets
(... others omitted ...)
# -- INFO PHONIES.
words:          # system capabilities: the what.
outline:        # with sections and stack effects.
terse:          # implementation details: the how.
story: clean    # design narrative: the why.
targets:        # this list.

[nix-shell]$ make outline | tee o/ol.txt
; -- [0] ARCHITECTURE.
; -- [1] ARITHMETIC, STACK.
plus2:  ; 2+ ( n -- n+2 )
udiv2:  ; 2u/ ( u -- u/2 )
nand:   ; nand ( n1 n2 -- ~(n1&n2) )
invert: ; invert ( n -- ~n )
equal0: ; 0= ( n -- flag )
plus:   ; + ( n1 n2 -- n1+n2 )
drop:   ; drop ( n -- ) free tail word!
(... etc etc ...)
```

How do I use it? 🚀
-------------------

```bash
nix-shell    # get assembler and qemu.
make run     # assemble and enter serial session.
```

The `run` target points you towards
[code to copypaste][fs], and gives details about the
quirky input. Only backspace and return, other controls
put garbage in the buffer.

My `shell.nix` gets native-only `qemu_test` to save
install time and space. If you're not on x86 then try
`nix-shell -p nasm qemu` for the whole shebang. If you
don't have nix, I'm sure you can figure out how
to get `nasm` and `qemu`. We're adults here.

What's the status?
------------------

```nasm
; my biggest win: almost [8e] every byte of kernel code
; is reusable from forth. proud of that.
```

The core assembly feels pretty done I think. A few
tradeoffs are illustrated with `%if 0/1` macros but a
lot more live in the comment prose. A few still in my
head.

You could:

- Read the [detailed narrative][asm] of the assembly.
- Do a `make terse | less` to see just the code please, 
  thanks. (My kinda reading!)
- Explore [the forth code][fs] beyond the race to hello
  world.
- Try to find more bytes while staying "fun to read and hack
  on." Which is extremely subjective but hey, throw something
  at me.

Moving forward I'll probably continue to polish prose,
I'm never satisfied with it.

Uh, actually. About that:

LLM disclosure 🤖
-----------------

I made extensive use of LLMs as personal copyeditor to
polish the comment copy in nicto.asm. However I want
you to understand that 99.9% of the words (and 99% of
the code) are still my own (or milliforth's or
sectorforth's). The emojis in this README have AI
stink but I decided to add them myself because there's
not much else pretty to look at here.

25% of the shit it came up with was nonsense, another
70% was kinda bad, but the real value I extracted was
the feedback; brainstorming places and directions to
improve. I have strong mixed feelings. This is
professional labor that I didn't pay for.

I showed it the last paragraph. "...having someone to
bounce ideas off, even if that someone was a language
model." Haha, no. Stay in your lane.

At LLM suggestion, though, I migrated from BIOS I/O to
serial for better DX. It guided me through the specific
routines. I guess I could have looked it up. I didn't.

It cost me bytes. And struggles. Fought with the
backspace. That was an adventure. Had to sell `swap`,
after spending lots of effort on its bittersweet return
story. I don't wanna talk about it.

When I felt overcommenting dread it showed me my code
without them, which was super helpful psychologically
so I got the idea for `make terse`.

That's all. I figured you should know.

---

Oh geez, that's an awful way to end the README. Go back
to [What's the status?](#whats-the-status) and we'll
pretend this never happened, ok?

<!-- *** end of README. *** -->

