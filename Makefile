
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

o/boot: $(SRC) o/stub # (default)
	$(ASM) -f bin -l $@l -o $@ $<

o/nopad: $(SRC) o/stub
	$(ASM) -f bin -l $@l -o $@ -D NOPAD $<

o/stub:
	#
	#   [!] New here? Do "make help" next. *Tons* of info!
	#
	mkdir -p $@

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
	#   "make help", back in your shell, to explore.
	#   "make r" skips this bigass banner.
	#   ctrl-a, c to swap serial<->monitor.
	#   ctrl-a, x to quit qemu. [!]
	#
	$(RUN) -serial mon:stdio

# my maintainer phonies, caret ^ hides from "make help":

PHONIES = awk '/^[a-z][^/]/ {print$$1}' Makefile | tr -d :

phonies: # ^ to update this Makefile.
	@$(PHONIES) | sort | xargs -n 10 echo '.PHONY:'

status: # ^ for demo, to update the README.
	git rev-parse @
	git status --short

demo: status clean count run # ^ needs manual paste.
# I've tried to automate this but attempts at piping
# things into qemu have been met with frustration.

# ; format conventions you can expect in the
# ; asm source (and which are used to parse it):
#
# ; [0] SECTION HEADER ------------------
#
# %define MACRO_NAME 1234
# EquateName  equ 1234
# DataName:   dw 1234
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
# ; [0b] anchored note, immediately after semicolon.
# ; might include cross-references [0a].
#
# ; unanchored note.
#
# ; (background or playful aside.)

##
## these phonies filter slices of the source:

# '||:' silence SIGPIPEs. 'cat -s' squeeze blanks.

words:    # compact list of the implemented forth words. [!]
	@awk '/ -- / && !/^interp/ {print$$3}' $(SRC) | xargs -n 12 ||:

teaser1:  # the interpreter routine. the heart of a forth. [!]
	@awk '/^ok/,/jmp/' $(SRC) ||:

teaser2:  # the bootstrap. *terrifying* and heavily documented.
	@awk '/^; \[8\]/,/4\./' $(SRC) ||:

DESIGN = /: double/,/^$$/; /^; \[0a\]/,/^$$/; /^; control flow/,/^$$/

design:   # example, memory map, registers, control flow. [!]
	@printf '; see  hello.fs  to define `:`, then:\n;\n'
	@awk '$(DESIGN)' $(SRC) ||:

reading:  # source format conventions.
	@awk '/^# ;/,/^$$/' Makefile ||:

glossary: # labels that implement words, with stack effects.
	@awk '/--/' $(SRC) ||:

names:    # all labels, variables, macros.
	@awk '/---|^\.|^\w|^%(def|mac)/' $(SRC) ||:

skel:     # control flow: labels, jumps, calls, rets. [!]
	@awk '/---|^\.?[a-z]|^ +(j|call|ret)/' $(SRC) ||:

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
##  $ make skel | bat -l nasm # or: | less
##  $ make terse >o/terse.asm

.PHONY: all clean count demo design doc glossary h help names notes
.PHONY: phonies r reading run skel status teaser1 teaser2 terse usage
.PHONY: words xrefs
