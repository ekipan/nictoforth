
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

o/boot: $(SRC) o/dir # (default)
	$(ASM) -f bin -l $@l -o $@ $<

o/nopad: $(SRC) o/dir
	$(ASM) -f bin -l $@l -o $@ -D NOPAD $<

o/dir: # (blank stub)
	#
	#   [!] New here? Do "make help" next. *Tons* of info!
	#
	mkdir -p o; touch $@

##
## development phonies:

clean:
	rm -rf o

all: o/boot o/nopad

count: o/nopad      # print assembled size.
	wc -c <$< # assembled size, out of 510 max:

usage:              # help banner.
	#
	#   nictoforth, across an emulated serial line.
	#   ux is hostile, it's more intended for source study.
	#
	#   - try "make help" back in your shell.
	#   - see  hello.fs  for some code to paste. [!]
	#   - errors give "?" and reset the stacks.
	#   - only backspace and return, other controls put garbage.
	#   - make sure your terminal sends backspace 127's.
	#     it does delete from the buffer but not your screen.
	#

r: o/boot           # run w/o usage.
	#   ctrl-a, x to quit qemu. [!]
	$(QEMU) -no-reboot -display none -serial mon:stdio \
	  -drive if=floppy,format=raw,file=$<

run: o/boot usage r # qemu serial session. [!]

# my maintainer phonies, caret ^ hides from "make help":

phonies: # ^ to update this Makefile.
	@awk '/^[a-z][^/]/{print$$1}' Makefile | \
	  tr -d : | sort | xargs -n 10 echo '.PHONY:'

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
#         ret           ; cross-reference [0b].
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
## these phonies awk-filter slices of the source.
## pipe the output to less or bat, redirect to a file,
## whatever you like:

# '||:' silence SIGPIPEs. 'cat -s' squeeze blanks.

words:    # compact list of the implemented forth words.
	@awk '/--/ && !/^;|^interp/ {print $$3}' $(SRC) | xargs -n 12 ||:

teaser1:  # the interpreter. the heart of a forth. [!]
	@awk '/^ok/,/jmp/' $(SRC)

teaser2:  # the bootstrap. *terrifying* [!]
	@awk '/^; \[8\]/,/4\./' $(SRC) ||:

glossary: # labels that implement words, with stack effects. [!]
	@awk '/--/' $(SRC) ||:

DESIGN = /: double/,/^$$/; /^; \[0a\]/,/^$$/; /^; control flow/,/^$$/

design:   # example, memory map, registers, control flow. [!]
	@awk '$(DESIGN)' $(SRC) ||:
	@echo '; see full boostrap example in hello.fs.'

reading:  # source format conventions.
	@awk '/^# ;/,/^$$/' Makefile ||:

terse:    # just the code, no asides.
	@echo '; see $(SRC) for notes [5c] [6b] etc.'
	@awk '/--$$/; !/^;/; $(DESIGN)' $(SRC) | cat -s ||:

names:    # all labels, variables, macros.
	@awk '/--$$|^\.|^\w|^%(def|mac)/' $(SRC) ||:

skel:     # control flow: labels, jumps, calls, rets.
	@awk '/--$$|^\.?[a-z]|^ +(j|call|ret)/' $(SRC) ||:

notes:    # anchored note contents, tricky highlights.
	@awk '/^; \[/,/^$$/' $(SRC) ||:

doc:      # the main comment text, no code.
	@awk '/^;|^$$/' $(SRC) | cat -s ||:

xrefs:    # inventory cross-ref anchors, for maintenance.
	@awk '/; \[/' $(SRC) ||:

help:     # this list. [!] marks important targets.
	@awk '/^## |^\w/ && !/\^/; /^##$$/{print""}' Makefile ||:

.PHONY: all clean count demo design doc glossary graph help names
.PHONY: notes phonies reading run serial status terse usage words xrefs
