; (c) 2025, see LICENSE (it's MIT).

; * contents: preamble fluff, then [0] architecture,
;   [1-4] the bits, [5-6] the heart, [7-8] the tools.

; nictoforth: nick's 16-bit x86 bootsector forth.
;
; after enjoying sectorforth and milliforth, I wondered:
; how much useful (and flexible!) forth can I cram into
; 510 bytes while being fun to read and hack on?
;
; the name squishes 'nick's sector' into five characters,
; a nod to the filename limit that gave us 'forth'.
; I started with milliforth's code but after a good
; hacksawing it's probably more sectorforthy.
;
; my biggest win: almost [8g] every byte of kernel code
; is reusable from forth. proud of that.

; -- [0] ARCHITECTURE.

; subroutine threaded because I like how it reads and
; writes (shoutouts to durexforth!). this forth code:
;
;   : double dup + ; \ compiles to:
;
;   dw link | db 6,'double' | dw double ; dict data.
;   double: call dup | call plus | ret  ; instructions.
;
; I chose an unconventional segment. implications:
;   1. I can store the tib at 0 to save addr calc code,
;   2. but lose access to bios variables.
;   3. but! possible underflow self-correction [5b].
;   4. more space to grow the dictionary,
;   5. without needing to move code.

        bits 16
        cpu 386
        org 0x2000 ; 0x05c0:0x2000 = 0x07c00, bios boot.
        jmp 0x05c0:abort ; set cs = 0x05c0.

; memory map (cs ds es ss = 0x05c0):
;   0000 [tib->0........] text buffer, zero terminated.
;   1000 [CIN][STATE]     interpreter variables.
;   1004 [....sp<-rstack] return addresses.
;   2000 [dict->here....] kernel and dictionary.
;    top [....bp<-pstack] parameter data.
;
; registers:
;   bp = param stack pointer, sp = return stack pointer.
;   ax bx cx dx si di = scratch for code words.
;   subroutine threaded so forth ip = x86 ip.

CIN     equ 0x1000    ; next unparsed character.
STATE   equ 0x1002    ; /!\ MUST EQUAL 1! [5c]
HERE:   dw c.here     ; next free byte to compile to.
LATEST: dw dictionary ; start of interpreter search list.
MAIN:   dw interpret  ; custom interpreter vector. [6b]

; milliforth groups the variables and keeps their base
; address in bx, saving instruction bytes at complexity
; cost. I split variables to simplify init [6] and
; built a hairy bootstrap [8] that buys me more bytes.
; bytes aren't cheap.
;
; FUTURE: nice-to-haves if I can find bytes for them:
; words: rp! sp! xor move.
; case insens. c.ret tco, infeasible tbh.
;
; time to dive in. good luck and happy reading!

%define INC2 times 2 inc ; every byte is sacred.
%define DEC2 times 2 dec ; every byte is good.
%define B byte           ; every byte is needed,
%define W word           ; in your neighborhood. ~*

; (B/W are mainly taste, but they do allow you to search
; byte/word in these comments and get less noise.)

; -- [1] ARITHMETIC, STACK.

plus2:  ; 2+ ( n -- n+2 )
        add W[bp],2
        ret

udiv2:  ; 2u/ ( u -- u/2 )
        shr W[bp],1
        ret

nand:   ; nand ( n1 n2 -- ~(n1&n2) )
        mov ax,W[bp]
        and W[bp+2],ax
        INC2 bp         ; [1a]
invert: ; invert ( n -- ~n )
        not W[bp]
        ret

; [1a] `jmp drop` here would decouple `nand` into `and`.
; good silly energy tho. and homage to my parentforths.
; honestly I'm really torn. might decouple.

equal0: ; 0= ( n -- flag )
        xor ax,ax
        cmp W[bp],ax
        jnz putax
        dec ax
        jmp putax

plus:   ; + ( n1 n2 -- n1+n2 )
        mov ax,W[bp]
        add W[bp+2],ax
drop:   ; drop ( n -- ) free tail word!
        INC2 bp
        ret

%if 1 ; 5 bytes, plus 1 in c.list [8].
dup:    ; dup ( n -- n n )
        mov ax,W[bp]
        jmp pushax
%endif

%if 1 ; 8 plus 1 bytes.
swap:   ; swap ( x y -- y x )
        mov ax,W[bp]
        xchg W[bp+2],ax
        jmp putax
%endif

; you can define all stack words in terms of `sp@ 2+ @ !`,
; so `swap` comes and goes a lot for bytes. (yeah this
; hurts. or bittersweet win. whichever currently applies,
; I'm tired of editing.)

rpush:  ; >r ( n -- r:n )
        pop bx
        push W[bp]
        push bx
        jmp drop

rpop:   ; r> ( r:n -- n )
        pop bx
        pop ax
        push bx
        jmp pushax

; (I bet you're curious about the lack of dictionary
; headers. better keep your boots on.)

; -- [2] MEMORY.

; shared tails pushax/putax live here so surrounding
; code saves bytes with short jumps.

cin:    ; >in ( -- addr )
        mov ax,CIN
pushax: DEC2 bp
putax:  mov W[bp],ax
        ret

dptr:   ; dp ( -- addr ) address of `here`.
        mov ax,HERE
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

; -- [3] INPUT/OUTPUT.

key:    ; key ( -- c )
        call .al
        mov ah,0
        jmp pushax
.al:    mov ah,2        ; serial recieve.
        xor dx,dx       ; com1.
        int 0x14
        test ah,1
        jz .al
        ret

emit:   ; emit ( c -- )
        mov al,B[bp]
        INC2 bp
.al:    mov ah,1        ; serial transmit.
        xor dx,dx       ; com1.
        int 0x14
        ; could spend bytes converting cr -> cr lf.
        ret

%macro DEBUG 1
        push ax         ; to use this you'll
        mov al,%1       ; need bytes. good
        call emit.al    ; places to plunder:
        pop ax          ; [1] [2] [7] [8].
%endmacro

line:   ; line ( -- ) reset `>in`, fill buffer.
        mov al,10
        call emit.al    ; move to next line.
        xor di,di       ; buffer at addr 0.
        mov W[CIN],di
        jmp .wait
.store: stosb           ; store and loop.
.echo:  call emit.al
.wait:  call key.al
%if 1 ; 0 or 12 or 22 bytes. pick your ux.
        cmp al,127      ; delete? (should check 8 too.
        jne .nobsp      ; I'm tired of this routine.)
        dec di
        jns .bsp        ; didn't go negative?
        inc di          ; whoops, passed start-of-line.
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
.nobsp: cmp al,13       ; carriage return?
        jne .store
        mov ax,32       ; ah = zero terminator.
        stosw           ; [3a]
        jmp emit.al     ; friendly space.

; -- [4] PARSING.

; `lex` is just my quirky name for standard `parse-name`.
; it's short and more precise imo. lemme have this.

lex:    ; lex ( "name" -- addr len )
        ;DEBUG 'L'
        mov di,W[CIN]
        xor cx,cx
        mov al,32       ; ascii space.
.skip:  ;DEBUG '.'
        cmp B[di],0
        je .eob         ; end-of-buffer.
        scasb ; cmp 32,B[di]
        jae .skip       ; skip space/ctls.
.scan:  ;DEBUG '!'
        inc cx          ; cx = len.
        scasb ; cmp 32,B[di]
        jb .scan        ; scan characters.
        dec di          ; di = end addr. [4a]
.eob:   mov W[CIN],di
        sub di,cx       ; di = start addr.
        sub bp,4
        mov W[bp+2],di
        mov W[bp+0],cx
        ret             ; cxz if eob.

; [4a] well, almost standard. can't skip the zero
; terminator so either: recheck (costly), rely on
; trailing space (fragile), or rewind (nonstandard)
; as [4a]. `line` always stores a space [3a] but a
; custom interpreter [6b] might not.

; could also recover standard `parse-name`:
;   : parse-name lex 1 >in +! ;
; after defining `+!` and `1`, or possibly:

; a numbers parser, even single digits, costs tens of
; bytes of code. I'd rather spend them on `swap`.

; -- [5] TEXT INTERPRETER.

immed_flag  equ 0x80 ; execute even in compile mode.
hidden_flag equ 0x20 ; ignore when `find`ing words.
len_mask    equ 0x1f ; max 31 characters.

dictionary: ; starts with only one word. the format:
        dw 0      ; link: 0 marks end of dictionary.
        db 1,';'  ; name: len+flags byte then characters.
        dw c.prim ; xt: execution token, a code address.
        ; a name token (nt) is a dictionary address.

; the xt field is mainly for byte savings [8].
; it looks like indirect threading but don't be fooled:
; `find` fetches direct addresses [5a] for dispatch.

find:   ; find ( addr len -- xt nt | addr 0 )
        ;DEBUG 'F'
        mov bx,LATEST
.prev:  mov bx,W[bx]    ; bx = nt (or 0).
        test bx,bx
        jz .eod         ; end-of-dictionary.
        mov si,bx
        lodsw           ; skip link.
        lodsb           ; al = len+flags.
        mov ah,al       ; for dispatch. [5d]
        and al,len_mask|hidden_flag
        cmp al,B[bp+0]  ; same length and not hidden?
        jne .prev
        mov di,W[bp+2]
        mov cx,W[bp+0]
        repe cmpsb      ; name characters match?
        jne .prev
        mov dx,W[si]    ; dx = xt. [5a]
        mov W[bp+2],dx
.eod:   mov W[bp+0],bx
        test bx,bx      ; nz if found.
        ret

; (a bit of fluff: as I've spent bytes decoupling bits
; of the interpreter I've watched its design converge
; towards durexforth's, whose source I didn't quite get
; before. it's kinda magical. go implement a forth, it
; opens your eyes!)

ok:     ;DEBUG 'K'
        add bp,4        ; drop empty lex.
        jg error        ; underflow?
%if 1 ; 10 bytes. *the* iconic forth ux.
        mov al,'o'
        call emit.al
        mov al,'k'
        call emit.al
%endif
        call line
interpret: ; ( ... "name" -- ... ) default MAIN. [6b]
        call lex
        jcxz ok         ; end of line?
        call find
        ; possible underflow self-correction. [5b]
        jz error        ; didn't find a word?
        INC2 bp         ; ( xt nt ) drop
        ; [5d] dispatch coupled to `find`: ah = len+flags.
        ; word type:      immediate | nonimmediate
        ; current state:    exe com | exe com
        and ah,immed_flag ;  80  80 |  0   0
        or ah,B[STATE]  ;    80  81 |  0   1  [5c]
        dec ah          ;    7f  80 | ff  *0*
        jz c.call       ; compile nonimmediate word.
execute: ; execute ( ... xt -- ... )
        INC2 bp
        jmp W[bp-2]     ; execute other cases.

; [5b] underflowing the stack wraps bp to low addresses.
; pushing values there corrupts the in buffer, and a
; malformed name causes an abort, correcting underflow.
; but bp and CIN have to collide *just so*.

; [5c] /!\ `and or dec` dispatch needs STATE low byte of
; exactly 1 to compile! it's a sharp edge, but it's code
; dense. (thanks, sectorforth!)

; [5d] could reuse from forth if the flags were taken
; from the nt on the stack. costs instructions though.
; maybe it's okay to keep the sharp edge in the drawer.

; -- [6] INITIALIZATION, MAIN LOOP.

; variables: (a) CIN STATE (b) HERE LATEST MAIN. either:
; all five at 0x1000, but need (b) inits before abort.
; or: current split design, but need two words to give
; addrs to forth. same code cost, this feels better imo.

error:  mov al,'?'
        call emit.al
abort:  ; abort ( -- ) reset param stack and:
        xor bp,bp       ; first push wraps to 0xfffe.
quit:   ; quit ( -- ) everything else, then loop.
        cld             ; standard stuff:
        times 3 push cs
        pop ds
        pop es
        pop ss          ; [6a]
        mov sp,$$       ; return stack under the kernel.
        ; serial init omitted.
        ; seabios seems to take care of it idk.
        mov B[STATE],0
        call line
        push abort      ; in case user types `r>` etc.
.loop:  push .loop
        jmp [MAIN]      ; jump through vector. [6b]

; [6a] apparently setting ss disables interrupts briefly
; so it makes the sp load safer. sure, I'll have it.

; [6b] vectored MAIN enables runtime interpreter
; swapping: define a new interpeter in forth, reusing all
; the pieces `lex find execute` etc, maybe add number
; parsing or whatever, then store it into MAIN and it
; becomes the new main loop:  ' my-interpret MAIN !

; -- [7] COMPILER.

; format[5]:  dw link | db len,'name' | dw xt
; shared tails c.ax/al/done sync di and W[HERE].

c: ; the story of a typical colon word:

; 1. first compile the link and name fields:
.head:  ; head, ( addr len -- )
        mov ax,W[HERE]
        xchg ax,W[LATEST] ; update latest.
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
        mov B[STATE],1
        ret

; 4. dispatch [5d] compiles words into the definition:
.call:  ; compile, ( xt -- )
        mov al,0xe8
        call .al
        mov ax,W[bp]
        INC2 bp
        DEC2 ax
        sub ax,di       ; relative address.
.ax:    mov di,W[HERE]
        stosw
        jmp .done

; 5. then switch off and tie it up:
.semi:  ; ; ( -- ) immediate
        mov B[STATE],0
.ret:   ; exit ( -- ) immediate
        mov al,0xc3
.al:    mov di,W[HERE]
        stosb
.done:  mov W[HERE],di
        ret

; 6. and optionally immediafy.
.immed: ; immediate ( -- )
        mov bx,W[LATEST]
        or B[bx+2],immed_flag
        ret

; -- [8] BOOTSTRAP.

; okay lean the fuck in, this is unbelievably complex.
; the core idea is straightforward enough:
;
; the xt field in the dictionary format [5] lets me
; split code from names, so I omit precious name bytes
; from the kernel. after boot, c.prim (named `;`) will
; name the builtins one at a time, constructing their
; xts from a list of offsets.
;
; read that a couple more times then take a second to
; gawk at the code:

%define XT plus2 ; first word in this file.

.prim:  ; ; ( "name" -- )
        call lex
        call .head      ; compile link and name.
.8a:    mov al,B[.list] ; [8a] load offset.
        inc W[.8a+1]    ; [8b] prepare next offset.
        cbw             ; -128 <= offset <= 127.
        xchg bx,ax      ; bx = offset.
.8c:    mov ax,XT       ; [8c] load xt.
        add W[.8c+1],bx ; [8d] prepare next xt.
        jmp .ax         ; compile xt.

; ignore most of it the first time through: first call
; lex and c.head, load plus2 [8c], then jump to c.ax,
; making a complete entry. the rest changes the load at
; [8c] into udiv2 from c.list below. I'll leave *how*
; it does this as an exercise. the self-modifying code
; saves extra variable bytes. code *is* data, anyways.

%macro DBO 1-* ; data byte offsets, each from previous.
    %rep %0
        db %1-XT
        %define XT %1
        %rotate 1
    %endrep
%endmacro

.list:  ; db udiv2-plus2, nand-udiv2, invert-nand, ...
        DBO udiv2, nand, invert, equal0, plus
        DBO drop, dup, swap, rpush, rpop
        DBO cin, dptr, sptr, rptr, fetch, store
        DBO key, emit, line, lex ; [8e]
        DBO find, execute, abort, quit
        DBO .head, .comma, .on, .call
        DBO .immed, .ret, .semi ; [8f]

; [8e] enough for a quick smoke test:
;
;   ; 2+ ; 2u/ ; nand ; invert ; 0= ; +
;   ( ... etc until: ... ) ; lex
;   lex 3 drop @ 2+ emit \ test, should print 5.
;   ( ... see hello.fs for full bootstrap ... )
;   ; immediate ; exit immediate ; ; immediate
;
; [8f] c.ret becomes forth `exit`, but immediate. then
; c.semi becomes `;`, shadowing c.prim. c.prim and
; c.list become dead code.
;
; [8g] besides c.prim, c.list, and dispatch [5d], every
; byte of kernel code is available. `interpret` you can
; fetch from MAIN. most words from then on will have xt
; fields that point to their next address. waste later
; to save now.

.here: ; be dragons! and future dictionary entries.

%ifndef NOPAD
        times 510-($-$$) db 0 ; (what would YOU build
        dw 0xaa55             ; with 510 bytes?)
%endif

; kate: hl Intel x86 (NASM); word-wrap-column 55
; *** end of assembly program file. ***

