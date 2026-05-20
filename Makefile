
## nictoforth is an x86 bootsector forth that:
##   - wants to be fun to read and hack on.
##   - doesn't touch the disk after bios jumps in.
##     just 510 bytes and you, across a serial line.
##   - is unconcerned with being a practical forth.
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

count: o/nopad    # print assembled size.
	wc -c <$< # assembled size, out of 510 max:

usage:            # basic help banner.
	#
	#  nictoforth, across an emulated serial line.
	#
	#  - see  hello.fs  for some code to paste. [!]
	#  - errors give "?" and reset the stacks.
	#  - only backspace and return, other controls put garbage.
	#  - make sure your terminal sends backspace 127's.
	#    it does delete from the buffer but not your screen.
	#
	#  - ctrl-a, c to swap serial<->monitor.

serial: o/boot    # run w/o usage, not recommended.
	#  - ctrl-a, x to quit qemu. [!]
	#  - see "make help".
	#
	$(QEMU) -no-reboot -display none -serial mon:stdio \
	  -drive if=floppy,format=raw,file=$<

run: usage serial # qemu serial session. [!]

phonies: # ^ to update this Makefile.
	@awk '/^[a-z][^/]/{print$$1}' Makefile | \
	  tr -d : | sort | xargs -n 10 echo '.PHONY:'

status: # ^ for demo, to update the README.
	git rev-parse @
	git status --short

demo: status clean count run # ^ needs manual paste.
# I've tried to automate this but attempts at piping
# things into qemu have been met with frustration.

##
## info phonies awk'd from the source. pipe the output to
## less or bat, redirect into a file, whatever you like:

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

# '||:' silence SIGPIPEs. 'cat -s' squeeze blanks.

words:    # compact list of the implemented forth words.
	@awk '/--/ && !/^;|^interp/ {print $$3}' $(SRC) | xargs -n 12 ||:

glossary: # word list with stack effects. [!]
	@awk '/--/' $(SRC) ||:

DESIGN = /: double/,/^$$/; /^; \[0a\]/,/^$$/; \
 /^; control flow/,/^$$/; /^; format\[5\]/,/^$$/
# "make help" parses ^ to hide this and some targets.

design:   # example, memory map, registers, control flow, dict format. [!]
	@awk '$(DESIGN)' $(SRC) ||:

reading:  # source format conventions.
	@awk '/^# ;/,/^$$/' Makefile ||:

terse:    # just the code, no asides.
	@echo '; see $(SRC) for notes [5c] [6b] etc.'
	@awk '/--$$/; !/^;/; $(DESIGN)' $(SRC) | cat -s ||:

names:    # labels, variables, macros.
	@awk '/--$$|^\.|^\w|^%(def|mac)/' $(SRC) ||:

notes:    # anchored note contents, a dense spec.
	@awk '/^; \[/,/^$$/' $(SRC) ||:

flow:     # control flow graph: labels, jumps, calls, rets.
	@awk '/--$$|^\.?[a-z]|^ +(j|call|ret)/' $(SRC) ||:

doc:      # all names and asides. cut most code.
	@awk '/^;|^\.|^\w|^%(def|mac)|^$$/' $(SRC) | cat -s ||:

xrefs:    # inventory cross-ref anchors, for maintenance.
	@awk '/; \[/' $(SRC) ||:

help:     # this list. [!] marks important targets.
	@awk '/^## |^\w/ && !/\^/; /^##$$/{print""}' Makefile ||:

.PHONY: all clean count demo design doc glossary graph help names
.PHONY: notes phonies reading run serial status terse usage words xrefs
