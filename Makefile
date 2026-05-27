
## nictoforth is an x86 bootsector forth that:
##   - wants to be fun to read and hack on.
##   - doesn't touch the disk after bios jumps in.
##     just 510 bytes and you, across a serial line.
##   - is unconcerned with being practical.
##
## Makefile variables you can override:

SRC ?= nicto.asm
ASM ?= nasm # yasm also works fine.
QEMU ?= qemu-system-i386 # or: qemu-kvm

##
## target files you can make:

o/boot: $(SRC) o/.dir # (default)
	$(ASM) -f bin -o $@ $<

o/nopad o/list: $(SRC) o/.dir
	$(ASM) -f bin -o o/nopad -l o/list -D NOPAD $<

o/.dir:
	#
	#   [!] New here? Do "make help" next. *Tons* of info!
	#
	mkdir -p $(@D); touch $@

##
## development phonies:

clean:         # remove o directory.
	rm -rf o

all: o/boot o/nopad

count: o/nopad # print assembled size.
	wc -c <$< # assembled size, out of 510 max:

RUN = $(QEMU) -drive if=floppy,format=raw,file=$< \
  -display none -no-reboot

r: o/boot      # run w/o usage banner.
	# "make run" instead for help. ctrl-a, x to quit qemu. [!]
	$(RUN) -serial mon:stdio

run: o/boot    # qemu serial session. [!]
	#
	#   nictoforth, across an emulated serial line.
	#   ux is hostile, it's more intended for source study.
	#
	#   - errors give "?" and reset the stacks.
	#   - only backspace and return, other controls put garbage.
	#   - make sure your terminal sends backspace 127's.
	#     it does delete from the buffer but not your screen.
	#
	#   the boot dictionary only has one word named `;`, and
	#   it's a weirdo. you should copypaste  hello.fs  to define
	#   the builtins then `:` and the rest. [!]
	#
	#   - ctrl-a, c to swap serial<->monitor.
	#   - ctrl-a, x to quit qemu. [!]
	#   - "make help", back in your shell, to explore.
	#   - "make r" skips this bigass banner.
	#
	$(RUN) -serial mon:stdio

# my maintainer phonies, caret ^ hides from "make help":

list: o/list # ^ requires bat.
	grep -P '^.{40}[^;]' $< | LESS=SFMR bat -pl nasm

PHONIES = awk '/^[a-z][^/]/ {print$$1}' Makefile | tr -d :

phonies: # ^ to update this Makefile.
	@$(PHONIES) | sort | xargs -n 8 echo '.PHONY:'

demo1: # ^ to update the gist for the README.
	@clear; echo '$$ make demo'
	#
	#   https://github.com/ekipan/nictoforth
	#
	#   below I copypaste an abridged source from kwrite into qemu
	#   (I haven't figured out how to automate it). you should look
	#   at the full 'hello.fs' for detailed explanations though.
	#
	git rev-parse @ # current commit:
	git status --short # current state, should be clean:

demo2: # ^ so I can 'make demo2 r' to test w/o clearing.
	awk '/^[^\\]/' hello.fs | kwrite -i &>/dev/null & # to copy.
	@printf '\nmake clean all count r # r is abridged run\n'

demo: demo1 demo2 clean all count r # ^

# ; format conventions you can expect in the
# ; asm source (and which are used to parse it):
#
# ; [0] SECTION HEADER ------------------
#
# %define MACRO_NAME 1234
# EquateName  equ 1234
# DataName:   dw 1234
#
# ; asides have semicolon on column 1.
#
# code_name: ; forth-word ( stack -- effect ) remark.
# .local_code_name:
#         instruction   ; [0a] anchored instruction.
#         jz label      ; question? (branch if yes)
#         ; remark about the state in this context.
#         instruction   ; cross-reference [0b].
#         ret           ; describe out flags.
# .LocalDataName:
#         dw 1234
#
# ; [0b] anchored note, marker after semicolon.
# ; might include cross-references [0a].
#
# ; (background or playful aside.)

##
## these phonies filter slices of the source:

# '||:' silences SIGPIPE. 'cat -s' squeezes blanks.

words:    # compact list of the implemented forth words. [!]
	@awk '/ -- / && !/^interp/ {print$$3}' $(SRC) | xargs -n 12 ||:

teaser1:  # the interpreter routine. the heart of a forth. [!]
	@awk '/^ok/,/jmp/' $(SRC) ||:

teaser2:  # the bootstrap. *terrifying* and heavily documented.
	@awk '/^; \[8\]/,/4\./' $(SRC) ||:

DESIGN = /^; subrou|^; \[(0a|6a)\]/,/^$$/

design:   # example, mem map, regs, dict format, control flow. [!]
	@awk '$(DESIGN); /; \[5a\]/,/^$$/' $(SRC) ||:

reading:  # source format conventions.
	@awk '/^# ;/,/^$$/' Makefile ||:

glossary: # labels that implement words, with stack effects. [!]
	@awk '/--/' $(SRC) ||:

names:    # all labels, variables, macros.
	@awk '/---|^\.|^\w|^%(def|mac)/' $(SRC) ||:

skel:     # control flow: labels, jumps, calls, rets. [!]
	@awk '/---|^%[eim]|^\.?[a-z]|^ +(j|loop|call|ret|push (pu|ab))/' $(SRC) ||:

terse:    # just the code, no asides.
	@echo '; see $(SRC) for notes [5c] [6b] etc.'
	@awk '/---/; !/^;/; $(DESIGN)' $(SRC) | cat -s ||:

notes:    # anchored note contents, tricky highlights.
	@awk '/^; \[/,/^$$/' $(SRC) ||:

doc:      # the main comment text, no code.
	@awk '/^;|^$$/' $(SRC) | cat -s ||:

xrefs:    # inventory cross-ref anchors, for maintenance.
	@awk '/; \[/' $(SRC) ||:

help:     # this list. [!] marks important targets.
	@awk '/^## |^[a-z]|\?=/ && !/\^/; /^##$$/ {print""}' Makefile ||:

h:        # just the phony target names.
	@$(PHONIES) | xargs -n 8 ||:

##
## try:
##  $ make words # or teaser1 or design or ...
##  $ make skel >o/skel.asm  # or: ... | less
##  $ make terse | vim - -c 'set ft=nasm'

.PHONY: all clean count demo demo1 demo2 design doc glossary
.PHONY: h help list names notes phonies r reading
.PHONY: run skel teaser1 teaser2 terse words xrefs
