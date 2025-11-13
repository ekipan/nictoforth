[1]: ./nicto.asm
[2]: ./nicto.fs

# nictoforth

If you're impatient: `nix-shell` then `make run` to jump in.

Heads up though: this is more an art piece exploring a crazy constrained problem space than a practical thing you'd wanna write software for.

    ; after enjoying sectorforth and milliforth, I wondered:
    ; how much useful (and flexible!) forth can I cram into
    ; 510 bytes while being fun to read and hack on?
    ;
    ; -- nictoforth: nick's 16-bit x86 bootsector forth. --

## ...the hell is an "x86 bootsector forth"?

### Forth

An old and grumpy programming language. I love it to death.

It has a reputation for being impenetrable because you must mentally track a stack of values, **but the real sauce is the dictionary:** you're given the tools to extend the compiler and interpreter in the language itself. More collaborator than consumer. I could talk your ear off.

**Learn more:** | [Wikipedia][3] | the beloved [Starting Forth][4] | the dense [ANS Forth glossary][5]

[3]: https://en.wikipedia.org/wiki/Forth_(programming_language)
[4]: https://www.forth.com/starting-forth/
[5]: https://forth-standard.org/standard/alpha

### Boot sector

Of a floppy disk. A BIOS loads the sector into memory and jumps to the code when you turn your PC on. Honestly I don't know the details but it looked like fun. Only 510 bytes!

### x86

An instruction set. Intel and AMD processors run programs encoded in x86 at their lowest-ish level. Most phones run ARM programs. The NES, the Commodore 64, and Bender Bending Rodríguez run 6502 programs. There's lots.

### "nictoforth"?

You'll have to [read the source][1]. Can't spoil all the surprises.

*Inspired by the lovely [sectorforth][6], [milliforth][7], and [durexforth][8].*

[6]: https://github.com/cesarblum/sectorforth
[7]: https://github.com/fuzzballcat/milliForth
[8]: https://github.com/jkotlinski/durexforth

## What can it do?

Available Forth words (after [proper bootstrap][2]):

    [nix-shell:~/nicto]$ make outline
    ; for an outline: grep -- -- nicto.asm.
    ; -- [0] INTRODUCTION --
    ; -- nictoforth: nick's 16-bit x86 bootsector forth. --
    ; -- [1] ARITHMETIC, STACK --
    plus2:  ; 2+ ( n -- n+2 )
    udiv2:  ; 2u/ ( u -- u/2 )
    nand:   ; nand ( n1 n2 -- ~(n1&n2) )
    invert: ; invert ( n -- ~n )
    equal0: ; 0= ( n -- flag )
    plus:   ; + ( n1 n2 -- n1+n2 )
    drop:   ; drop ( n -- ) free tail word!
    dup:    ; dup ( n -- n n )
    rpush:  ; >r ( n -- r:n )
    rpop:   ; r> ( r:n -- n )
    ; -- [2] MEMORY --
    cin:    ; >in ( -- addr )
    dptr:   ; dp ( -- addr ) address of `here`.
    sptr:   ; sp@ ( -- addr )
    rptr:   ; rp@ ( -- addr )
    fetch:  ; @ ( addr -- n )
    store:  ; ! ( n addr -- )
    ; -- [3] INPUT/OUTPUT --
    key:    ; key ( -- c )
    emit:   ; emit ( c -- )
    line:   ; line ( -- ) reset `>in`, fill buffer.
    ; -- [4] PARSING --
    lex:    ; parse-name ( "name" -- addr len )
    ; -- [5] TEXT INTERPRETER --
    find:   ; find ( addr len -- xt nt | addr 0 )
    interpret: ; ( ... "name" -- ... )
    execute: ; execute ( ... xt -- ... )
    ; -- [6] INITIALIZATION, MAIN LOOP --
    abort:  ; abort ( -- ) reset param stack and:
    quit:   ; quit ( -- ) everything else, then loop.
    ; -- [7] COMPILER --
    .head:  ; head, ( addr len -- )
    .comma: ; , ( n -- )
    .on:    ; ] ( -- )
    .call:  ; compile, ( xt -- )
    .semi:  ; ; ( -- ) immediate
    .ret:   ; exit ( -- ) immediate
    .immed: ; immediate ( -- )
    ; -- [8] BOOTSTRAP --
    .prim:  ; ; ( "name" -- )

## How do I use it?

    nix-shell  # get yasm and qemu.
    make run   # assemble and enter serial session.

The `run` target points you towards [code to copypaste][2], and gives details about the quirky input. Only backspace and return, other controls put garbage in the buffer.

If you don't have nix, I'm sure you can figure out how to get `yasm` and `qemu`. We're adults here.

## What's the status?

    ; my biggest win: almost [8e] every byte of kernel code
    ; is reusable from forth. proud of that.

The core assembly feels pretty done I think. A few tradeoffs are illustrated with `%if 0/1` macros but a lot more live in the comment prose. A few still in my head.

You could:

- Read the [detailed narrative][1] of the assembly.
- Do a `make terse` to see just the code please, thanks. (My kinda reading!)
- Explore [the forth code][2] beyond the race to hello world.
- Try to find more bytes while staying "fun to read and hack on." (Tall ask, I'd be floored!)

Moving forward I'll probably continue to polish prose, I'm never satisfied with it.

Uh, actually. About that:

## Some messy human shit.

A disclosure: I made extensive use of LLMs as personal copyeditor to polish the comment copy in nicto.asm. 25% of the shit it came up with was nonsense, another 70% was kinda bad, but the real value I extracted was the back-and-forth, bouncing ideas off, getting feedback to jump off from. It's... a complicated feeling. This is professional labor that I didn't pay for.

I showed it this section and you should see what it gave me back, ha ha. "...having someone to bounce ideas off, even if that someone was a language model." Yeah, no. Stay in your box.

At LLM suggestion, though, I migrated from BIOS I/O to serial for better DX. It guided me through the specific routines. I guess I could have looked it up. I didn't.

It cost me bytes. And struggles. Fought with the backspace. That was an adventure. Had to sell `swap`, after spending lots of effort on its bittersweet return story. I don't wanna talk about it.

When I struggled with impostor syndrome vs the other two Forths it showed me my code without all the comments, which was super helpful psychologically so I got the idea for `make terse`.

That's all. I figured you should know.

---

Oh geez, that's an awful way to end the README. Go back to [What's the status?](#whats-the-status) and we'll pretend this never happened, ok?

<!-- *** end of README. *** -->

