; (c) 2025-2026, see LICENSE (it's MIT).

; nictoforth: nick's 16-bit x86 bootsector forth.

;   preamble [0] design [1] basics [2] memory [3] i/o
;   [4] parse [5] interp [6] init [7] compile [8] boot

; after enjoying sectorforth and milliforth, I wondered:
; how much useful (and flexible!) forth can I cram into
; 510 bytes while being fun to read and hack on?
;
; the constraint: never touch the disk again after bios
; jumps in. just 510 bytes and a user over serial.
;
; the name squishes 'nick's sector' into five characters,
; a nod to the filename limit that gave us 'forth'.
; I started with milliforth's code but after a good
; hacksawing it's probably more sectorforthy.
;
; my biggest win: almost [8h] every byte of kernel code
; is reusable from forth. proud of that.

; [0] ARCHITECTURE -----------------------------------

; subroutine threaded because I like how it reads and
; writes (shoutouts to durexforth!). this forth code:
;
;   : double dup + ; \ compiles to:
;
;   dw link | db 6,'double' | dw double ; dict data.
;   double: call dup | call plus | ret  ; instructions.

        bits 16
        cpu 186 ; need `push imm16`
        org 0x2000 ; 0x05c0:0x2000 = 0x07c00, bios boot.
        jmp 0x05c0:abort ; cs ds es ss = 0x05c0 [6].

; both my parentforths park in segment 0x50 and put the
; tib at 0, saving parse code but losing bios vars.
; I chose a higher segment for more dictionary space.
;
; [0a] segment 0x05c0 memory map:
;   0000-0fff  [text->0.......]  input buffer.
;   1000-1003  [ToIn][State]     variables.
;   1004-1fff  [.....sp<-addrs]  return stack.
;   2000-....  [code->Here....]  kernel, dictionary.
;   ....-ffff  [......bp<-data]  parameter stack.
;
; registers:
;   subroutine threaded so x86 ip = forth ip.
;   bp = param stack pointer, sp = return stack pointer.
;   ax bx cx dx si di = scratch for code words.
;
; one exception: ah couples find -> dispatch [5c].
; [0b] lex and find set flags for compactness.

ToIn    equ 0x1000    ; next unparsed [4] character.
State   equ 0x1002    ; low byte nonzero = compile [5c].
Here:   dw c.Here     ; next free byte to compile [7] to.
Latest: dw Dictionary ; head of find [5] linked list.
Main:   dw interpret  ; custom interpreter vector [6b].

; milliforth groups the variables and keeps their base
; address in bx, saving instruction bytes at complexity
; cost. I split variables to simplify init [6] and
; built a hairy bootstrap [8] that buys me more bytes.
; bytes aren't cheap.
;
; FUTURE: to try, or nice-to-haves, budget willing:
; bx tos. numbers parser. c, c! c@ move bye xor rp! sp!
; case insens. c.ret tco, infeasible tbh.
;
; time to dive in. good luck and happy reading!

%define INC2 times 2 inc ; every byte is sacred.
%define DEC2 times 2 dec ; every byte is good.
%define B byte           ; every byte is needed,
%define W word           ; in your neighborhood. ~*

; (B/W are mainly taste, but they do allow you to search
; byte/word in these comments and get less noise.)

; [1] ARITHMETIC, STACK ------------------------------

plus2:  ; 2+ ( n -- n+2 )
        add W[bp],2
        ret

udiv2:  ; 2u/ ( u -- u/2 )
        shr W[bp],1
        ret

and:    ; and ( n1 n2 -- n1&n2 )
        mov ax,W[bp]
        and W[bp+2],ax
        jmp drop        ; [1a]

invert: ; invert ( n -- ~n )
        not W[bp]
        ret

; [1a] an earlier version fell through into invert,
; implementing `nand` which was both delightfully silly
; and homage to my parentforths. I'll miss it.

equal0: ; 0= ( n -- flag )
        xor ax,ax
        cmp W[bp],ax
        jnz putax       ; not zero? put zero [2a].
        dec ax
        jmp putax

plus:   ; + ( n1 n2 -- n1+n2 )
        mov ax,W[bp]
        add W[bp+2],ax
drop:   ; drop ( n -- ) free tail word!
        INC2 bp
        ret

%if 1 ; 5 bytes, plus 1 in c.List [8].
dup:    ; dup ( n -- n n )
        mov ax,W[bp]
        jmp pushax
%endif

%if 1 ; 8 plus 1 bytes [1b].
swap:   ; swap ( x y -- y x )
        mov ax,W[bp]
        xchg W[bp+2],ax
        jmp putax
%endif

; [1b] you can define all stack words in terms of
; `sp@ 2+ @ !`, so `swap` comes and goes a lot for bytes.
; (yeah this hurts. or bittersweet win. whichever
; currently applies, I'm tired of editing.)

rpush:  ; >r ( n -- r:n )
        pop dx
        push W[bp]
        push dx
        jmp drop

rpop:   ; r> ( r:n -- n )
        pop dx
        pop ax
        push dx
        jmp pushax

; (I bet you're curious about the lack of dictionary
; headers. better keep your boots [8] on.)

; [2] MEMORY -----------------------------------------

; [2a] shared tails pushax/putax live here so
; surrounding code saves bytes with short jumps.

cin:    ; >in ( -- addr )
        mov ax,ToIn
pushax: DEC2 bp         ; [2a]
putax:  mov W[bp],ax    ; [2a]
        ret

dptr:   ; dp ( -- addr ) address of `here`.
        mov ax,Here
        jmp pushax

sptr:   ; sp@ ( -- addr )
        mov ax,bp
        jmp pushax

rptr:   ; rp@ ( -- addr )
        mov ax,sp
        INC2 ax         ; skip own return address.
        jmp pushax

fetch:  ; @ ( addr -- n )
        mov bx,W[bp]
        mov ax,W[bx]
        jmp putax

store:  ; ! ( n addr -- )
        mov bx,W[bp]
        mov ax,W[bp+2]
        add bp,4        ; 3 bytes `add` < 4 `inc`s.
        mov W[bx],ax
        ret

; [3] INPUT/OUTPUT -----------------------------------

key:    ; key ( -- c )
        push pushax     ; defer pstack push after:
.al:    mov ah,2        ; serial receive.
        call com1
        shl ah,1        ; [3a]
        jc .al          ; receive error?
        mov ah,0
        ret

; [3a] `shr ah,8` puts error into carry *and* 0 into ah,
; saving a byte vs mov, but 8-shifting reg8 is UB.

emit:   ; emit ( c -- )
        mov al,B[bp]
        INC2 bp
.al:    mov ah,1        ; serial transmit.
com1:   xor dx,dx       ; clobbers dx.
        int 0x14
        ret

%macro DEBUG 1 ; 7 bytes per use.
        push ax
        mov al,%1
        call emit.al    ; emit DEBUG char.
        pop ax
%endmacro ; places to plunder bytes: %ifs, [1-2,7-8].

line:   ; line ( -- ) reset `>in`, fill buffer.
        mov al,10
        call emit.al    ; move to next line.
        xor di,di       ; buffer at addr 0 [0a].
        mov W[ToIn],di  ; parse [4] from there later.
        jmp .wait
.store: stosb           ; store and loop.
.echo:  call emit.al
.wait:  call key.al
%if 1 ; 0 or 12 or 22 bytes. pick your ux.
        cmp al,127      ; [3b]
        jne .check      ; not delete?
        dec di
        jns .bsp        ; di >= 0, still in buffer?
        inc di
.bsp:
    %if 0 ; 10 bytes. users expect this.
        mov al,8
        call emit.al    ; move cursor back.
        mov al,32
        call emit.al    ; erase character.
    %endif
        mov al,8
        jmp .echo       ; move cursor back.
%endif
.check: cmp al,13
        jne .store      ; not a carriage return?
        mov ax,32       ; ah = zero terminator.
        stosw           ; [3c]
        jmp emit.al     ; friendly space.

; [3b] should check 8 too. I'm tired of this routine.
; `test di,di | jz .wait | dec di` would be simpler and
; avoid the extra 8 emit but costs another byte.

; [4] PARSING ----------------------------------------

; `lex` is just my quirky name for standard `parse-name`.
; it's short and more precise imo. lemme have this.

lex:    ; lex ( "name" -- addr len )
        ;DEBUG 'L'
        mov di,W[ToIn]
        xor cx,cx
        mov al,32       ; space.
.skip:  ;DEBUG '.'
        cmp B[di],0     ; zero terminator.
        je .eob         ; end-of-buffer?
        scasb ; cmp 32,B[di]
        jae .skip       ; space or control?
.scan:  ;DEBUG '!'
        inc cx          ; cx = len.
        scasb ; cmp 32,B[di]
        jb .scan        ; name character?
        dec di          ; [4a] di = end addr.
.eob:   mov W[ToIn],di
        sub di,cx       ; di = start addr.
        sub bp,4
        mov W[bp+2],di
        mov W[bp+0],cx
        ret             ; cxz if eob. [0b]

; [4a] well, almost standard. `line` always stores a
; space [3c] before the zero terminator but a custom
; interpreter [6b] might not, so either: assume it does
; anyway (fragile), recheck (costly), or rewind
; (nonstandard) as above.

; could also recover standard `parse-name` after
; defining `+!` and `1`:   : parse-name lex 1 >in +! ;

; a numbers parser, even single digits, costs tens of
; bytes of code. I'd rather spend them on `swap`.

; [5] TEXT INTERPRETER -------------------------------

ImmedFlag  equ 0x80 ; execute even in compile mode.
HiddenFlag equ 0x20 ; ignore when `find`ing words.
LenMask    equ 0x1f ; max 31 characters.

Dictionary: ; starts with only one word. the format:
        dw 0      ; link: 0 marks end of dictionary.
        db 1,';'  ; name: len+flags byte then characters.
        dw c.prim ; xt: execution token, a code address.
        ; nt: a name token is a link field address.

; the xt field is mainly for byte savings [8].
; it looks like indirect threading but don't be fooled:
; `find` fetches direct addresses [5a] for dispatch.

find:   ; find ( addr len -- xt nt | addr 0 )
        ;DEBUG 'F'
        mov bx,Latest
.prev:  mov bx,W[bx]    ; bx = nt (or 0).
        test bx,bx
        jz .eod         ; end-of-dictionary?
        mov si,bx
        lodsw           ; skip link.
        lodsb           ; al = len+flags.
        mov ah,al       ; needed for dispatch [5c].
        and al,LenMask|HiddenFlag
        cmp al,B[bp+0]
        jne .prev       ; wrong length or hidden?
        mov di,W[bp+2]
        mov cx,W[bp+0]
        repe cmpsb
        jne .prev       ; name characters differ?
        mov dx,W[si]    ; [5a] dx = xt.
        mov W[bp+2],dx
.eod:   mov W[bp+0],bx
        test bx,bx
        ret             ; nz if found. [0b]

; (a bit of fluff: as I've spent bytes decoupling bits
; of the interpreter I've watched its design converge
; towards durexforth's, whose source I didn't quite get
; before. it's kinda magical. go implement a forth, it
; opens your eyes!)

ok:     ;DEBUG 'K'
        add bp,4        ; drop empty lex.
        jg error        ; [5b] underflow?
%if 1 ; 10 bytes. *the* iconic forth ux.
        mov al,'o'
        call emit.al
        mov al,'k'
        call emit.al
%endif
        call line
interpret: ; ( ... "name" -- ... ) default Main [6b].
        call lex
        jcxz ok         ; end of line? [0b]
        call find
        ; [5b] possible underflow self-correction.
        jz error        ; didn't find a word? [0b]
        ; [5c] dispatch coupled to find: ah = len+flags.
        INC2 bp         ; ( xt nt ) drop
        shl ah,1        ; rely on ImmedFlag = 0x80.
        jc execute      ; immediate word?
        cmp B[State],0
        jne c.call      ; compile mode?
execute: ; execute ( ... xt -- ... )
        INC2 bp
        jmp W[bp-2]

; [5b] underflowing the stack wraps bp to low addresses,
; see map [0a]. `jg` corrects it (bp > 0), but pushing
; values there corrupts the in buffer, so it may also
; self-correct in the middle of a line if bp and ToIn
; happen to collide!

; [5c] could reuse from forth if the flags were taken
; from the nt on the stack, but it costs 3 bytes.
; sectorforth has a *wildly* dense dispatch routine I
; adore. sadly this simple one costs the same and has
; a milder gotcha: State high byte is ignored.

; [6] INITIALIZATION, MAIN LOOP ----------------------

; variables: (a) ToIn State (b) Here Latest Main.
; either: all at 0x1000, but need (b) inits before abort.
; or: current split, but need two words to give addrs
; to forth. same code cost, I like this better.

error:  mov al,'?'
        call emit.al
abort:  ; abort ( -- ) reset param stack and:
        xor bp,bp       ; first push wraps to 0xfffe [0a].
quit:   ; quit ( -- ) everything else, then loop.
        cld             ; standard stuff:
        times 3 push cs
        pop ds
        pop es
        pop ss          ; [6a]
        mov sp,$$       ; rstack under the kernel [0a].
        ; serial init omitted.
        ; seabios seems to take care of it idk.
        mov B[State],0  ; start in execute mode [5c].
        call line
        push abort      ; in case user types `r>` etc.
.loop:  push .loop
        jmp [Main]      ; swappable [6b] interpreter.

; control flow (see example [8a] with data flow):
;   boot[0] -> abort -> line[3] -> .loop
;   -> [Main]interpret -> lex[4] (-> ok -> line -> lex)
;   -> find[5] -> error | c.call[7] | execute -> .loop

; [6a] apparently setting ss disables interrupts briefly
; so it makes the sp load safer. sure, I'll have it.

; [6b] vectored Main costs 4 bytes, enabling interpreter
; hotswap: define a new interpreter in forth, reuse
; `line lex find abort c.call execute`, maybe add number
; parsing or whatever, then store it into Main and it
; becomes the new main loop:  ' my-interpret Main !

; [7] COMPILER ---------------------------------------

; format[5]:  dw link | db len,'name' | dw xt
; shared tails c.ax/al/done sync di and W[Here].

c: ; the story of a typical colon word:

; 1. first compile the link and name fields:
.head:  ; head, ( addr len -- )
        mov ax,W[Here]
        xchg ax,W[Latest] ; update latest.
        call .ax        ; link to old latest.
        mov si,W[bp+2]  ; si = addr.
        mov cx,W[bp]    ; cx = len.
        add bp,4
        mov al,cl
        stosb           ; length. not bounds checked!
        rep movsb       ; name characters.
        jmp .done

; 2. then add an xt of here+2 (it's complicated [8]):
.comma: ; , ( n -- )
        mov ax,W[bp]
        INC2 bp
        jmp .ax

; 3. switch the compiler on:
.on:    ; ] ( -- )
        mov B[State],1  ; for dispatch [5c].
        ret

; 4. dispatch [5c] compiles words into the definition:
.call:  ; compile, ( xt -- )
        mov al,0xe8
        call .al
        mov ax,W[bp]
        INC2 bp
        DEC2 ax
        sub ax,di       ; relative address.
.ax:    mov di,W[Here]
        stosw
        jmp .done

; 5. then switch off and tie it up:
.semi:  ; ; ( -- ) immediate
        mov B[State],0
.ret:   ; exit ( -- ) immediate
        mov al,0xc3
.al:    mov di,W[Here]
        stosb
.done:  mov W[Here],di
        ret

; 6. and optionally immediafy.
.immed: ; immediate ( -- )
        mov bx,W[Latest]
        or B[bx+2],ImmedFlag
        ret

; [8] BOOTSTRAP --------------------------------------

; okay lean the fuck in, this is unbelievably complex.
; the core idea is straightforward enough:
;
; the xt field in the dictionary format [5] lets me
; split code from names, so I omit precious name bytes
; from the kernel. after boot, c.prim (named `;`) will
; name the builtins one at a time, constructing their
; xts from a list of offsets.
;
; [8a] read that once more then take a second to gawk
; at the code. cross-ref control (and data) flow:
;
;   ... -> line [input "; 2+"] -> ... -> lex (";")
;   -> find (c.prim) -> execute -> c.prim -> lex ("2+")
;   -> c.head, c.ax [compile `2+`] -> quit.loop[6]

%define XT plus2 ; first word in this file.

.prim:  ; ; ( "name" -- )
        call lex
        call .head
.8b:    mov al,B[.List] ; [8b] load xt offset byte.
        cbw             ; decompress.
        xchg dx,ax      ; -128 <= dx <= 127.
        inc W[.8b+1]    ; [8c] point to next byte.
.8d:    mov ax,XT       ; [8d] load xt.
        add W[.8d+1],dx ; [8e] mutate into next xt.
        jmp .ax

; the first time through:
;   1. compile link and name: lex ("2+") -> c.head
;   2. [8b] compute dx = udiv2-plus2.
;   3. complete entry with xt: load plus2 [8d] -> c.ax.
;   4. [8e] mutate [8d] into udiv2 for next time.
;
; `cbw` [8b] negative offsets support final c.semi [8g]
; for shadowing and `c.semi -> c.ret` fallthru, which
; saves 2 bytes jmp. 1 byte `xchg` < 2 byte `mov`.
; self-modifying code [8c][8e] saves variable bytes.
; code *is* data, anyways.

%macro DBO 1-* ; data byte offsets, to compress xt list.
    %rep %0
        %if %1-XT < -128 || 127 < %1-XT
            %error DBO %1 <- out of range
        %endif
        db %1-XT        ; [8b] loads, [8e] adds into [8d].
        %define XT %1   ; remember for next byte.
        %rotate 1
    %endrep
%endmacro

.List:  ; db udiv2-plus2, and-udiv2, invert-and, ...
        DBO udiv2, and, invert, equal0, plus
        DBO drop, dup, swap, rpush, rpop
        DBO cin, dptr, sptr, rptr, fetch, store
        DBO key, emit, line, lex ; [8f]
        DBO find, execute, abort, quit
        DBO .head, .comma, .on, .call
        DBO .immed, .ret, .semi ; [8g]
        ; see full boostrap example in hello.fs.

; [8f] enough for a quick smoke test:
;
;   ; 2+ ; 2u/ ; and ; invert ( ... ) ; line ; lex
;   lex 3 drop @ 2+ emit \ test, should print 5.
;   ( ... ) ; immediate ; exit immediate ; ; immediate
;
; [8g] c.ret becomes forth `exit`, but immediate. then
; c.semi becomes `;`, shadowing c.prim. c.prim and
; c.List become dead code.
;
; [8h] besides c.prim, c.List, and dispatch [5c], every
; byte of kernel code is available. `interpret` you can
; fetch from Main. most words from then on will have xt
; fields that point to their next address. waste later
; to save now.

.Here: ; be dragons! and future dictionary entries [0a].

%ifndef NOPAD ; for `make count` size check.
        times 510-($-$$) db 0 ; (what would YOU build
        dw 0xaa55             ; with 510 bytes?)
%endif

; kate: hl Intel x86 (NASM); word-wrap-column 55
; *** end of assembly program file. ***
