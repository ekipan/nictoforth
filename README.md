
# Nictoforth

Nick's 16-bit x86 bootsector Forth. See the bottom of
this README if you [don't know what that means][wha].

Like its progenitors [Milliforth][mil] and
[Sectorforth][sec], this is an **art Forth** exploring
a constrained problem space, rather than a system you'd
wanna write software for. All three **lack a builtin
numbers parser,** for example.

## How to use it?
<!-------------->

I've tested with `nasm 3.01`, `qemu 10.1.2`, and gnu
`make 4.4.1`, but recentish versions will probably be
fine. Then `awk cat wc xargs` etc for info like
`make outline`. You can get them however you like,
but if you have [Nix]:

```bash
nix-shell  # get assembler and qemu.
make run   # assemble and enter serial session.
# try copypasting hello.fs.
# press ctrl-a, x to quit qemu.
```

The `run` target tells details about the quirky input.
Only backspace and return, other controls put garbage
in the buffer.

My `shell.nix` gets native-only `qemu_test` to save
install time and space. If you're not on x86 then try
`nix-shell -p nasm qemu` for the whole shebang.

## What can it do?
<!--------------->

To save name bytes there's only one word at boot,
which gives names to the rest of the builtins.

Type `; 2+ <return>` to name the first word `2+`.
Other inputs reply with `?` to let you know there
was an error and the stacks were reset. See the
Forth source for the full [wacky bootstrap][fs].

You can ask the Makefile for more info:

```txt
[nix-shell]$ make targets
(...)
# -- INFO PHONIES.
words:         # system capabilities: the what.
outline:       # with stack effects, as a reading aide.
terse:         # implementation details: the how.
(...)

[nix-shell]$ make words
2+ 2u/ nand invert 0= + drop dup swap >r r> >in
dp sp@ rp@ @ ! key emit line lex find execute abort
quit head, , ] compile, ; exit immediate ;
```

- `line` gets input, so to a first approximation is
  `0 4096 accept`,
- `lex` is nonstandard `parse-name`: it rewinds `>in`
  onto the delimiter,
- `find` is very nonstandard (see `make outline`),
- The second `;` is the bootstrapper, but note:

Above I mentioned the lack of a numbers parser.
You can write one in Forth, but even before that:

```forth
lex 3 drop @ \ "lex 3" gives an ( addr len ) in the
      \ input buffer, len is dropped and the '3' digit
      \ and space are fetched: ( $2033 )
2+    \ ( $2035 )
emit  \ prints: 5. See hello.fs for lots more.
```

## Why make this?
<!-------------->

Reading DuskOS's [Tumble Forth][tum] blog led me to
Milliforth, which crushes Sectorforth's code into as
few bytes as possible, but I was inspired to go a
different direction: spending those bytes decoupling
and expanding to see just how much it could resemble a
real Forth.

The creative constraint: **never touch the disk again**
after BIOS first jumps to the kernel. Just 510 bytes
and a user across a serial line.

## What's the status? 📌
<!--------------------->

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
- Do a `make terse | bat -l nasm` or `| less` to see
  just the code please, thanks. (My kinda reading!)
- Explore [the forth code][fs] beyond the race to hello
  world.
- Try to find more bytes while staying "fun to read and hack
  on." Which is extremely subjective but hey, throw something
  at me.

Moving forward I'll probably continue to polish prose,
I'm never satisfied with it.

Uh, actually. About that:

## How was it made? 🤖
<!------------------->

I started with [Milliforth's][mil] code and developed
most of this thing in my head over a couple feverish
weeks before finally testing or committing any of it.
I've dug up some earlier drafts but haven't done the
work of reconnecting the history back to Milliforth.

I used LLMs as personal copyeditor to polish the
comment copy in nicto.asm. However I want you to
understand that 99.9% of the words (and 99% of the
code) are still my own (or Milliforth's or
Sectorforth's). The emojis have AI stink but there's
not much else pretty to look at here.

The copy it comes up with is shit but the feedback is
valuable: finding places that need improvements in
clarity or examples or whatever. I have strong mixed
feelings. This is professional labor that I didn't pay
for.

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

## ...the hell is an "x86 bootsector Forth"? 😵‍💫
<!-------------------------------------------->

### Forth

Is an old and grumpy programming language.
I love it to death.

It has a reputation for being impenetrable because you
must mentally track a stack of values, **but the real
sauce is the dictionary:** you're given the tools to
extend the compiler and interpreter in the language
itself. More collaborator than consumer. I could talk
your ear off.

**📚 Learn more:** | [Wikipedia][wik]
| the beloved [Starting Forth][sta]
| the dense [ANS Forth glossary][ans]

Forth was made to be a practical tool to solve
problems. I think [Miniforth][min] and [DuskOS][dus]
are good examples in a similar space, though I haven't
read much of them. As said at the top, however,
[Nictoforth is an art Forth][top] exploring how much
readable language can fit entirely within the:

### Boot sector

Of a floppy disk 💾. A BIOS loads the sector into
memory and jumps to the code when you turn your PC on.
Honestly I don't know the details but it looked like
fun. Only 510 bytes! Written in:

### x86

An instruction set. Intel and AMD processors run
programs encoded in x86 at their lowest-ish level. Most
phones run ARM programs. The NES, the Commodore 64, and
Bender Bending Rodríguez run 6502 programs. There's
lots.

### "Nictoforth"?

You'll have to [read the source][asm].
Can't spoil all the surprises.

<!-- References -->

[top]: #nictoforth
[wha]: #the-hell-is-an-x86-bootsector-forth-%E2%80%8D
[asm]: nicto.asm
[fs]:  hello.fs
[wik]: https://en.wikipedia.org/wiki/Forth_(programming_language)
[ans]: https://forth-standard.org/standard/alpha
[sta]: https://www.forth.com/starting-forth/
[dus]: https://duskos.org/
[tum]: https://tumbleforth.hardcoded.net/
[sec]: https://github.com/cesarblum/sectorforth
[mil]: https://github.com/fuzzballcat/milliForth
[min]: https://github.com/meithecatte/miniforth
[nix]: https://nixos.org/

<!-- kate: remove-trailing-spaces all; -->
<!-- *** end of README. *** -->
