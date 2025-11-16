; 2+ ; 2u/ ; nand ; invert ; 0= ; +
; drop ; dup ; >r ; r>
; >in ; dp ; sp@ ; rp@ ; @ ; !
; key ; emit ; \ ; lex
; find ; execute ; abort ; quit
; head, ; , ; ] ; compile,
; immediate ; exit immediate ; ; immediate

\ (c) 2025, see LICENSE. -*- mode: forth -*-

\ nictoforth: nick's 16-bit x86 bootsector forth.

\ the initial bootstrapping word `;` gives a name to a
\ builtin word from an internal list. the list above
\ includes `\` which lets us write comments, and the
\ last word was the usual forth `;`, shadowing the
\ bootstrapping word underneath it. some notes:

\ lex ( -- addr len ) standard `parse-name`.
\ head, ( addr len -- ) need to `,` an xt after it.
\ find ( addr len -- xt nt | addr 0 ) very nonstandard.

\ some smoke tests to make sure things are working:
lex 3 drop @ 2+ emit \ honoring the jackson:
lex 3 drop @ >in 0= + emit \ 3:
lex 3 drop @ >in 0= 0= + emit \ 2:

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
\ is the same but that's only because there's no "ok"
\ prompt, which would be skipped. also it's not
\ immediate, which we could fix if we care enough:
\ : line \ ;   : \ line ; immediate

\ time to roll up your sleeves,
\ let's get this thing going.

\ -----

\ for more testing. gets two bytes from the buffer,
\ so good enough to compute and emit with.
: x lex drop @ ;
x o emit x k emit \ annie are you:

\ no builtin literals, sadly. need code bytes!
\ compute zero from known nonzero address `>in`.
: 0 >in 0= ;   : -1 0 invert ;
: 1 -1 2+ ;   : 2 0 2+ ;
x 3 0 + emit x 3 1 + emit \ 34:
x 3 2 + emit x 3 2 2u/ + emit \ 54:

\ >in  (variables) \ addr of next unparsed character.
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
: swap dup 3rd @ 3rd ! 3rd ! ;
\ prefer `over ... nip` etc vs this slow `swap`.
x a x b x c nip swap emit emit \ ac:

\ runtime literals. breathe. it'll be okay.
: lit r> dup 2+ >r @ ;
: [ 0 state ! ; immediate

\ some arithmetic.
: and nand invert ;
: or invert over invert nand nip ;
: - -1 + invert + ;
: = - 0= ;
: 2* dup + ;
: 16* 2* 2* 2* 2* ;
: 16u/ 2u/ 2u/ 2u/ 2u/ ;
x 3 x 6 and emit \ 2:
x a 1 16* + emit \ q:
x a x b = x 5 + emit \ 5:

\ TODO non-recursive `:` so words may extend themselves.
: ' lex find drop ; \ missing word gives a buffer addr.
: flags latest 2+ ;
: smudge lit [ 2 16* , ] flags @ or flags ! ;
: reveal lit [ 2 16* invert , ] flags @ and flags ! ;
\ ' : : : [ compile, ] smudge ; \ don't think too hard.
\ : ; ; reveal ; immediate reveal

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
: i rp@ 2+ @ ; \ or if r@ were defined:
\ ' r@ lex i head, , \ ' r@ alias i

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
: in+ >in @ 1 + >in ! ;
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
