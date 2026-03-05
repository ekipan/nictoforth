; 2+ ; 2u/ ; nand ; invert ; 0= ; +
; drop ; dup ; swap ; >r ; r>
; >in ; dp ; sp@ ; rp@ ; @ ; !
; key ; emit ; \ ; lex
; find ; execute ; abort ; quit
; head, ; , ; ] ; compile,
; immediate ; exit immediate ; ; immediate

lex 3 drop @ \ some smoke tests. ( $2033 )
dup 2+ emit \ honoring the jackson:
dup >in 0= + emit  >in 0= 0= + emit \ 32:

\ (c) 2025, see LICENSE.

\ nictoforth: nick's 16-bit x86 bootsector forth.

\ the initial bootstrapping word `;` gives a name to a
\ builtin word from an internal list. the list above
\ includes `\` which lets us write comments, and the
\ last word was the usual forth `;`, shadowing the
\ bootstrapping word underneath it. some notes:

\ lex ( -- addr len ) like standard `parse-name`.
\ head, ( addr len -- ) need to `,` an xt after it.
\ find ( addr len -- xt nt | addr 0 ) very nonstandard.

\ -----

\ bootstrapping the compiler is a bit circular.

lex : head,
\ the string `:` is parsed from the input buffer, then
\ `head,` compiles a link to the previous word and
\ copies the name characters. saying `:` now will crash
\ the interpreter; the dictionary format expects a
\ code pointer.
dp
\ this is the address of the compiler pointer `here`.
@ 2+ ,
\ we compile a pointer to the next free addr here+2.
\ now we switch on `]` the internal compiler and it
\ compiles calls to each of these words into the new
\ definition:
] lex head, dp @ 2+ , ] ;
\ executing `:` now will follow the same procedure:
\ parse and define another new name then switch the
\ compiler on. no smudging yet; be careful not to
\ write accidental recursion.

\ a confession: `\` isn't actually a comment word. like
\ standard `refill` it gets new input so the side-effect
\ is the same but only if there's no "ok" prompt, which
\ gets skipped. it's also not immediate. let's rename:
: ' lex find drop ; \ ( "name" -- xt ) no error check.
: alias lex head, , ; \ ( xt "name" -- )
' \ alias line
line testing, this should still be ignored.

\ `>in` is right after the buffer and its low address
\ byte is a zero so we'll reuse it as end-of-input.
\ it's hacky but hey it beats having to scan for a zero.
: \ >in >in ! ; immediate

\ alright, it's time to roll up your sleeves,
\ let's get this thing going.

\ -----

\ for more testing. gets two bytes from the buffer,
\ so good enough to compute and emit with.
: x lex drop @ ; \ ( "name" -- 'a'*256+'n' )
x o emit x k emit \ annie are you:

\ no builtin literals, sadly. need code bytes!
\ compute zero from known nonzero address `>in`.
: 0 >in 0= ;   : -1 0 invert ;
: 1 -1 2+ ;   : 2 0 2+ ;
x 3 0 + emit x 3 1 + emit \ 34:
x 3 2 + emit x 3 2 2u/ + emit \ 54:

\ variables:   >in \ addr of next unparsed character.
: state >in 2+ ;   \ /!\ must be exactly 1! see asm.
: here dp @ ;      \ next free byte to `,` compile to.
: latest dp 2+ @ ; \ dict entry of last defined word.
\ main at dp+4     \ vector: replace the interpreter.
here x 3 2+ , @ 2+ emit \ 7:

\ core stack. some of these come and go from the
\ builtins list above as I squeeze and spend bytes
\ developing nictoforth. definitions below will be
\ uncommented if needed.
\ : dup sp@ @ ;
: over sp@ 2+ @ ;
: nip sp@ 2+ ! ;
: 2dup over over ;
: 2drop drop drop ;
: 3rd sp@ 2+ 2+ ;
\ : swap dup 3rd @ 3rd ! 3rd ! ;
\ prefer `over ... nip` etc vs this slow `swap`.
x a x b x c nip swap emit emit \ ac:

\ runtime literals. breathe. it'll be okay.
: lit r> dup 2+ >r @ ;
: [ 0 state ! ; immediate

\ some arithmetic.
: and nand invert ;
: or invert swap invert nand ;
: - -1 + invert + ;
: = - 0= ;
: 2* dup + ;
: 16* 2* 2* 2* 2* ;
: 16u/ 2u/ 2u/ 2u/ 2u/ ;
x 3 x 6 and emit \ 2:
x a 1 16* + emit \ q:
x a x b = x 5 + emit \ 5:

\ non-recursive `:` so words may extend themselves.
: flags latest 2+ ;
: bl lit [ 2 16* , ] ;
: smudge bl flags @ or flags ! ;
: reveal lit [ bl invert , ] flags @ and flags ! ;
' : : : [ compile, ] smudge ; \ don't think too hard.
' ; : ; [ compile, ] reveal ; reveal immediate

\ control flow. runtime 0branch is a bit hairy.
: branch r> @ >r ;
: 0branch 0= rp@ @ @ and r> 2+ over 0= and or >r ;
: branch, lit [ ' branch , ] compile, ;
: 0branch, lit [ ' 0branch , ] compile, ;
: mark, here 0 , ;
: resolve here swap ! ;
: if 0branch, mark, ; immediate
: then resolve ; immediate
: begin here ; immediate
: again branch, , ; immediate
: until 0branch, , ; immediate
: while 0branch, mark, swap ; immediate
: repeat branch, , resolve ; immediate

\ counted loop.
: 2>r swap r> swap >r swap >r >r ;
: (loop) r> r> 1 + r> 2dup =
  if 2drop 2+ >r exit then >r >r @ >r ;
: do lit [ ' 2>r , ] compile, here ; immediate
: loop lit [ ' (loop) , ] compile, , ; immediate
: i rp@ 2+ @ ; \ or if r@ were defined: ' r@ alias i

\ string typer.
: $ff lit [ -1 16u/ 16u/ , ] ;
: char x $ff and ;
: c@ @ $ff and ;
: bounds over + swap ;
: type dup 0= if 2drop exit then
  bounds do i c@ emit loop ;
lex testing type

\ parsed ( start delim -- start len )
: parsed drop >in @ over - ;
: in+ 1 >in @ + >in ! ;
: in@ >in @ c@ ;
: parse in+ >in @ swap begin
    in@ 0= if parsed exit then
    dup in@ = if parsed in+ exit then
  in+ again ;

\ this is it!
: ')' lit [ char ) , ] ;
: ( ')' parse 2drop ; immediate
( paren comments! )
: .( ')' parse type ; immediate

.( hello, forth )

\ -----

\ so where to next?

\ we can parse and type strings. that's exciting, but
\ it'd be nice to compile them too so we could use them
\ at runtime. though it's tedious without builtin `c,`
\ or `move`.

\ then the next glaring hole is numbers. we have `lit`
\ and enough arithmetic to implement a hex parser and
\ printer but again it's quite tedious.

\ all of this bootstrap tedium should honestly live in
\ source blocks on disk, but once nicto.asm gives access
\ to disk it doesn't really have *any* constraints any
\ more and just becomes another typical, useful, forth.

\ I haven't read much of it myself but apparently
\ miniforth is exactly that. I'm sure there's lots of
\ others.

\ sectorforth has example source that gets to fizzbuzz
\ and milliforth to a brainfuck interpreter. but for now,
\ nictoforth is good enough.
