<!-- markdownlint-disable blanks-around-headings no-inline-html -->

# Nictoforth
[top]: #nictoforth

A 510-byte x86 bootsector Forth that wants to be:

```asm
; (...) fun to read and hack on.
```

["Bootsector"? "Forth"?][wtf] <- follow that to the
bottom of this README.

Like its progenitors [Milliforth][mil] and
[Sectorforth][sec], this is an **art Forth** exploring
a constrained problem space, rather than a system you'd
wanna write software for. All three **lack a builtin
numbers parser,** for example.

[The source][asm] is full of cross-reference. Search
for [5c] [6b] etc to jump around. Include a semicolon
"; [5c]" to go straight to an anchor, skipping refs.

## How to use it?
<!-------------->

I've tested with `nasm 3.01`, `qemu 10.1.2`, and gnu
`make 4.4.1`, but recentish versions will probably be
fine. Then `awk cat wc xargs` etc for info like
`make glossary`. You can get them however you like,
but if you have [Nix]:

```bash
nix-shell  # get assembler and qemu.
make run   # assemble and enter serial session.
# try copypasting hello.fs.
# press ctrl-a, x to quit qemu.
```

Here's a [demo session log][dem] if you want to see it
working.

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
[nix-shell]$ make help
(...)
# -- INFO PHONIES.
words:    # compact list of the implemented forth words.
glossary: # word list with stack effects. [!]
design:   # example, memory map, registers, control flow, dict format. [!]
reading:  # source format conventions.
terse:    # just the code, no asides.
names:    # labels, variables, macros.
graph:    # control flow: labels, jumps, calls, rets.
(...)

[nix-shell]$ make words
2+ 2u/ and invert 0= + drop dup swap >r r> >in
dp sp@ rp@ @ ! key emit line lex find execute abort
quit head, , ] compile, ; exit immediate ;
```

- `line` gets input, so to a first approximation is
  `0 4096 accept`,
- `lex` is nonstandard `parse-name`: it rewinds `>in`
  onto the delimiter,
- `find` is very nonstandard (see `make glossary`),
- The second `;` is the bootstrapper, but note:
- It lacks a numbers parser. You can compute numbers
  and write a parser in Forth, but even before that:

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

```nasm
; the constraint: never touch the disk again after bios
; jumps in. just 510 bytes and a user over serial.
```

## What's the status?
<!------------------>

```nasm
; my biggest win: almost every byte of kernel code
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
- Try to find more bytes while staying "fun to read
  and hack on." Which is extremely subjective but hey,
  throw something at me.
- Try to improve readability and cross-reference. Tell
  me if something confuses you.

Moving forward I'll probably continue to polish prose,
I'm never satisfied with it.

Uh, actually. About that:

## How was it made? 🤖
<!---------------->

I started with [Milliforth's][mil] code and developed
most of this thing in my head over **a couple feverish
weeks** before finally testing or committing any of it.
I've dug up earlier drafts but haven't done the work to
reconnect history back to Milliforth.

---

<details><summary>
Bigass LLM rant, should probably be its own document.
</summary>

As an experiment, **I consulted LLMs** to polish the
docs copy. First time touching the things, I was
curious.

I didn't keep an audit trail, I'm not willing to let
the bot touch my hobby repo, but I'll still take
ownership of all the words and especially the code.
You've read this far, maybe you know my voice.

### Benefits I've reaped

- If it hallucinates an explanation then maybe I can
  make the text clearer. Add examples, concretize.
- When I felt overcommenting dread it showed me my code
  without them. Refreshed, I got the idea for
  `make terse`.
- I wanted paste, so it suggested I migrate to serial.
  The fat routines it gave me were painful to shove in.
  Now that I've read the docs and squeezed them I'm
  comfortable calling them my own (~10 instructions).
- Occasional brainstorming for saved bytes, though
  hallucinations make this mostly a wash. The
  frustration has made me read the docs more though!
  Maybe one day I will understand x86 instruction
  encoding syntax.
- It led me to references like [Cloutier's][clo]!

### The harm

The usual stuff:

- Alarmingly addictive. Warm words get the endorphins
  flowing, but sometimes the sycophancy makes me sick.
- The hallucinations seem designed to be part of the
  pull-the-lever addiction loop.
- The slop tsunami will never go away. Every word I see
  now I have to wonder if a person wrote it or not.
  "You're right that X!" "Not just X but Y!" If you
  didn't care to write it then I don't care to read it,
  and now I have to convince you that I _did care_ to
  write my docs, even if I used this _thing_ to help me.

And the stuff that I have the luxury of feeling
disconnected to:

- I'm using professional labor I didn't pay for.
  Laborers are getting devalued.
- Burning energy in an energy crisis.

I can't expect Joe Developer to figure out if my use is
"responsible," and I don't blame him. Spending these
paragraphs apologizing for the dirtiness benefits
nobody. _Fuck,_ man.

</details>

---

Oh, when I posted to the Forth discord, a very helpful
person immediately bought me [an easy 24 bytes][24b]!
He also suggested I put TOS in bx, which will probably
buy me more, but the complexity spreads throughout.
Maybe one day I'll experiment.

## ...the hell is an "x86 bootsector Forth"? 😵‍💫
[wtf]: #the-hell-is-an-x86-bootsector-forth-%E2%80%8D
<!----------------------------------------->

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

[asm]: nicto.asm
[fs]:  hello.fs

[24b]: https://github.com/ekipan/nictoforth/commit/a837d57050404c17f21fc27df223048c91511b0e
[ans]: https://forth-standard.org/standard/alpha
[clo]: https://www.felixcloutier.com/x86/
[dem]: https://gist.github.com/ekipan/641c40be1b86beba628d5d00b05da583/798ecbeeed430b453c3dd5288330a1f44ea5fa72
[dus]: https://duskos.org/
[mil]: https://github.com/fuzzballcat/milliForth
[min]: https://github.com/meithecatte/miniforth
[nix]: https://nixos.org/
[sec]: https://github.com/cesarblum/sectorforth
[sta]: https://www.forth.com/starting-forth/
[tum]: https://tumbleforth.hardcoded.net/
[wik]: https://en.wikipedia.org/wiki/Forth_(programming_language)

<!-- kate: remove-trailing-spaces all; -->
<!-- *** end of README. *** -->
