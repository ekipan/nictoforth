; (c) 2025-2026, see LICENSE (it's MIT).

; contents: [0] design [1] basics [2] memory [3] i/o
;   [4] parse [5] interp [6] init [7] compile [8] boot
;
; diagrams: [0a] memory map, registers [5a] dict format
;   [6a] control flow [8a] with data [8f] boot excerpt
;
; awk this source with: `make design` for diagrams,
; `make skel` calls/rets, `make terse` no asides,
; `make help` see all targets.

; nictoforth: nick's 16-bit x86 bootsector forth
; ==============================================

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
; writes (shoutouts to durexforth!). after defining
; colon `:` (see hello.fs), this forth code:
;
;   : double dup + ; \ compiles to:
;
;   dw link | db 6,'double' | dw double ; dict data.
;   double: call dup | call plus | ret  ; instructions.

        bits 16
        cpu 186 ; need: push imm16; imul a,b,c
        org 0x0c00 ; 0x700:0xc00 = 0x7c00, bios boot.
        jmp 0x0700:abort ; cs ds es ss = 0x0700 [6].

; both my parentforths park in segment 0x50 and put the
; tib at 0, saving parse code but losing bios vars.
; I chose a higher segment for more dictionary space.
;
; [0a] memory map (and growth), segment 0x700:
;   0000-03ff [text->0.......] input buffer.
;   0400-0403 [ToIn][State]    variables.
;   0404-0bff [.....sp<-addrs] return stack (down).
;   0c00-.... [code->Here....] kernel, dictionary (up).
;   ....-ffff [....bp<-values] parameter stack (down).
;   text input buffer is zero-terminated [3c].
;
; registers:
;   subroutine threaded so x86 ip = forth ip.
;   bp = param stack pointer, sp = return stack pointer.
;   ax bx cx dx si di = scratch for code words, except:
;   ah, couples `find -> dispatch` [5e].
;   flags, couple `lex | find -> interpret` [5d].

ToIn    equ 0x400     ; next unparsed [4] character.
State   equ 0x402     ; low byte nonzero = compile [5f].
Here:   dw c.Here     ; next free byte to compile [7] to.
Latest: dw Dictionary ; head of find [5] linked list.
Main:   dw interpret  ; custom interpreter vector [6c].

; milliforth groups the variables and keeps their base
; address in bx, saving instruction bytes at complexity
; cost. I split variables to simplify init [6] and
; built a hairy bootstrap [8] that buys me more bytes.
; bytes aren't cheap.
;
; FUTURE: to try, or nice-to-haves, budget willing:
; bx tos. lit c, c! c@ move bye xor rp! sp!
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

; [1a] (`INC2 bp` here instead would fall into invert,
; implementing `nand` for same code cost. delightfully
; goofy and an homage to my parentforths. I miss it.)

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

toin:   ; >in ( -- addr )
        mov ax,ToIn
pushax: DEC2 bp         ; [2a]
putax:  mov W[bp],ax    ; [2a]
        ret

dp:     ; dp ( -- addr ) address of `here`.
        mov ax,Here
        jmp pushax

spfch:  ; sp@ ( -- addr )
        mov ax,bp
        jmp pushax

rpfch:  ; rp@ ( -- addr )
        mov ax,sp
        INC2 ax         ; skip own return address.
        jmp pushax

fetch:  ; @ ( addr -- n )
        mov si,W[bp]
        lodsw
        jmp putax

store:  ; ! ( n addr -- )
        mov di,W[bp]
        mov ax,W[bp+2]
        stosw
        add bp,4        ; 3 bytes `add` < 4 `inc`s.
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

; [3a] `shr ah,8` might put error into carry *and* zero
; ah, saving a byte vs mov, but 8-shifting reg8 is UB.

emit:   ; emit ( c -- )
        mov al,B[bp]
        INC2 bp
.al:    mov ah,1        ; serial transmit.
com1:   xor dx,dx       ; clobber.
        int 0x14
        ret

%macro DEBUG 1 ; 7 bytes per use.
        push ax
        mov al,%1
        call emit.al    ; (part of DEBUG macro.)
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
%if 0 ; 4 bytes, press escape to re-input.
        cmp al,27
        je line         ; escape?
%endif
%if 1 ; 0 or 12 or 22 bytes. pick your backspace ux.
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
        cmp B[di],0     ; zero terminator [3c].
        je .eob         ; end-of-buffer?
        scasb ; cmp al,B[di++]
        jae .skip       ; space or control?
.scan:  ;DEBUG '!'
        inc cx          ; cx = len.
        scasb ; cmp al,B[di++]
        jb .scan        ; name character?
        dec di          ; [4a] di = end of word.
.eob:   mov W[ToIn],di
        sub di,cx       ; di = start of word.
        sub bp,4
        mov W[bp+2],di
        mov W[bp+0],cx
        ret             ; cxz if eob. [5d]

; the DEBUG macro exists because of the `scasb`
; instruction. why tf is the memory load on the rhs??

; [4a] well, almost standard. `line` always stores a
; space [3c] before the zero terminator but a custom
; interpreter [6c] might not, so either: assume it does
; anyway (fragile), recheck (costly), or rewind
; (nonstandard) as above.

; could also recover standard `parse-name` after
; defining `+!` and `1`:   : parse-name lex 1 >in +! ;

%if 0 ; 26 bytes. immediates only, no lit.
number: ; >number ( addr len 0 == n ) no error check.
        mov si,W[bp+4]
        mov cx,W[bp+2]
        xor dx,dx
.digit: lodsb
        cbw             ; 1 byte < 2 bytes `mov ah,0`.
        sub al,'0'      ; no range or minus check.
        imul dx,dx,10   ; only decimal.
        add dx,ax
        loop .digit
        xchg ax,dx      ; 1 byte < 2 bytes `mov`.
        add bp,4
        jmp putax

missing equ number
%else
missing equ error       ; might correct underflow [5c].
%endif

; [5] TEXT INTERPRETER -------------------------------

Immediate equ 0x80 ; flag: execute even in compile mode.
Hidden    equ 0x20 ; flag: ignore when `find`ing words.
Length    equ 0x1f ; mask: max 31 characters.

Dictionary: ; [5a] starts with only one entry. format:
        dw 0      ; link: 0 marks end of dictionary.
        db 1,';'  ; name: flags+len byte then characters.
        dw c.prim ; xt: execution token, a code address.
        ; nt: a name token is a link field address.

; [5b] the xt field is mainly for byte savings [8].
; it looks like indirect threading but don't be fooled:
; `find` fetches direct addresses for dispatch.

find:   ; find ( addr len -- xt nt | addr len 0 )
        ;DEBUG 'F'
        DEC2 bp         ; add slot in case of 0.
        mov bx,Latest
.prev:  mov bx,W[bx]    ; bx = nt (or 0).
        test bx,bx
        jz .eod         ; end-of-dictionary?
        mov si,bx
        lodsw           ; skip link.
        lodsb           ; al = flags+len.
        mov ah,al       ; needed for dispatch [5e].
        and al,Hidden|Length
        cmp al,B[bp+2]
        jne .prev       ; hidden or wrong length?
        mov di,W[bp+4]
        mov cx,W[bp+2]
        repe cmpsb
        jne .prev       ; name characters differ?
        INC2 bp         ; found, drop slot and:
        dec cx          ; clear z.
        mov dx,W[si]    ; [5b] dx = xt.
        mov W[bp+2],dx
.eod:   mov W[bp+0],bx
        ret             ; nz if found. [5d]

; (a bit of fluff: as I've spent bytes decoupling bits
; of the interpreter I've watched its design converge
; towards durexforth's, whose source I didn't quite get
; before. it's kinda magical. go implement a forth, it
; opens your eyes!)

ok:     ;DEBUG 'K'
        add bp,4        ; drop empty lex.
        jg error        ; [5c] underflow?
%if 1 ; 10 bytes. *the* iconic forth ux.
        mov al,'o'
        call emit.al
        mov al,'k'
        call emit.al
%endif
        call line
interpret: ; ( ... "name" -- ... ) default Main [6c].
        call lex
        jcxz ok         ; [5d] end of line?
        call find
        ; [5c] possible underflow self-correction.
        jz missing      ; [5d] didn't find a word?
dispatch: ; [5e] coupled to find: ah = flags+len.
        INC2 bp         ; ( xt nt ) drop
        shl ah,1        ; rely on Immediate = 0x80.
        jc execute      ; immediate word?
        cmp B[State],0  ; [5f]
        jne c.call      ; compile mode?
execute: ; execute ( ... xt -- ... )
        INC2 bp
        jmp W[bp-2]

; [5c] underflowing the stack wraps bp > 0 (see [0a]),
; which `ok` corrects (limiting to 0x8000-ffff). pushing
; values there corrupts the in buffer tho, so it may
; also self-correct if bp and ToIn happen to collide!

; [5e] costs 3 bytes to decouple, taking the flags from
; the nt on the stack to be reusable from forth.

; [5f] State high byte is ignored, a milder gotcha
; than sectorforth's *wildly* dense routine I adored,
; at same code cost. check it out!  $ git show bf7b6fb

; [6] INITIALIZATION, MAIN LOOP ----------------------

; variables: (a) ToIn State (b) Here Latest Main.
; either: all at 0x400, but need (b) inits pre-abort.
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
        pop ss          ; [6b]
        mov sp,$$       ; rstack under the kernel [0a].
        push abort      ; in case user types `r>` etc.
        ; serial init omitted.
        ; seabios seems to take care of it idk.
        mov B[State],0  ; start in execute mode [5f].
        call line
.loop:  push .loop
        jmp [Main]      ; swappable [6c] interpreter.

; [6a] control flow (see example [8a] with data flow):
;   boot[0] -> abort -> line[3] -> .loop
;   -> [Main]interpret -> lex[4] (-> ok -> line -> lex)
;   -> find[5] -> error | c.call[7] | execute -> .loop

; [6b] apparently setting ss disables interrupts briefly
; so it makes the sp load safer. sure, I'll have it.

; [6c] 4 bytes vectored Main buys hotswap: define a new
; interpreter in forth with `line lex find abort c.call
; execute`, add number parsing or whatever, then store
; into Main to switch:   ' my-interpret Main !

; [7] COMPILER ---------------------------------------

; format[5a]:  dw link | db len,'name' | dw xt
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
        mov B[State],1  ; for dispatch [5f].
        ret

; 4. dispatch [5e] compiles words into the definition:
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
        or B[bx+2],Immediate
        ret

; [8] BOOTSTRAP --------------------------------------

; okay lean the fuck in, this is unbelievably complex.
; the core idea is straightforward enough:
;
; the xt field in the dictionary format [5a] lets me
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
        DBO toin, dp, spfch, rpfch, fetch, store
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
; [8h] besides c.prim, c.List, and dispatch [5e], every
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
