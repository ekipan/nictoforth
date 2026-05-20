
# run "make targets" for an overview! it's at the bottom.

# -- VARIABLES.

SRC ?= nicto.asm
ASM ?= nasm # yasm also works fine.
QEMU ?= qemu-system-i386 # or: qemu-kvm

# -- TARGET FILES.

o/boot: $(SRC) o/dir # (default)
	# try: make run, make glossary, make first, make targets.
	$(ASM) -f bin -l $@l -o $@ $<

o/nopad: $(SRC) o/dir
	$(ASM) -f bin -l $@l -o $@ -D NOPAD $<

o/dir:
	mkdir -p o; touch $@

# -- DEVEL PHONIES.

clean:
	rm -rf o

all: o/boot o/nopad

count: o/nopad # print assembled size.
	wc -c <$< # assembled size, out of 510 max:

run: o/boot    # qemu serial session. [!]
	#
	#  nictoforth, across an emulated serial line.
	#
	#  - see hello.fs for some code to paste.
	#  - errors give "?" and reset the stacks.
	#  - only backspace and return, other controls put garbage.
	#  - make sure your terminal sends backspace 127's.
	#    it does delete from the buffer but not your screen.
	#  - ctrl-a, c to swap serial<->monitor.
	#  - ctrl-a, x to quit qemu. [!]
	#
	$(QEMU) -no-reboot -display none -serial mon:stdio \
	  -drive if=floppy,format=raw,file=$<

status:        # query git for demo, which requires manual paste,
	git rev-parse @
	git status --short

demo: status clean count run # so it's intended for myself.

.PHONY: all clean count demo run status

# -- INFO PHONIES.

### how to use the Makefile:
# run "make targets" to list all available info. pipe info
# phonies into less or bat, put them in a file, whatev.

### source formatting conventions:
#
# ; [0] SECTION HEADER ------------------
#
# %define MACRO_NAME 123
# EquateName  equ 345
# DataName:   dw 234
#
# code_name: ; forth-word ( stack -- effect ) remark.
# .local_code_name:
#         instruction   ; [0a] anchored instruction.
#         ret           ; cross-reference [0b].
# .LocalDataName:
#         dw 456
#
# ; [0b] anchored note, immediately after semicolon.
# ; might include cross-references [0a].
#
# ; unanchored note.
#
# ; (background or playful aside.)

# '||:' silence SIGPIPEs. 'cat -s' squeeze blanks.

first:    # how to read, including name and format conventions. [!]
	@awk '/^###/,/^$$/' Makefile ||:

glossary: # word list with stack effects. [!]
	@awk '/--/' $(SRC) ||:

terse:    # just the code, no asides. [!]
	@echo '; see $(SRC) for notes [5c] [6b] etc.'
	@awk '/--$$/; !/^;/; /: double/,/^$$/; /^; \[0a\]/,/^$$/; \
	  /^; control flow/,/^$$/; /^; format\[5\]/,/^$$/' \
	  $(SRC) | cat -s ||:

words:    # compact list of the implemented forth words.
	@awk '/--/ && !/^;|^interp/ {print $$3}' $(SRC) | xargs -n 12 ||:

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

targets:  # this list. important targets: [!]
	@awk '/^# --|^\w/' Makefile ||:

.PHONY: doc first flow glossary names notes targets terse words xrefs
