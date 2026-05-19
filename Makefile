# -- VARIABLES.

ASM ?= nasm # yasm also works fine.
QEMU ?= qemu-system-i386 # or: qemu-kvm

# -- TARGET FILES.

o/boot: nicto.asm o/dir # (default)
	# try: make outline, make run, make targets.
	$(ASM) -f bin -l $@l -o $@ $<

o/nopad: nicto.asm o/dir
	$(ASM) -f bin -l $@l -o $@ -D NOPAD $<

o/dir:
	mkdir -p o; touch $@

.PHONY: all run count clean words outline terse status demo targets

# -- DEVEL PHONIES.

all: o/boot o/nopad

count: o/nopad # print assembled size.
	wc -c <$< # assembled size, out of 510 max:

run: o/boot    # qemu serial session.
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

clean:         # remove o directory.
	rm -rf o

# -- INFO PHONIES.

words:         # system capabilities: the what.
	@awk '/--/ && !/^;|^interp/ {print $$3}' nicto.asm | xargs -n 12

outline:       # with stack effects, as a reading aide.
	@awk '/--/' nicto.asm

terse:         # implementation details: the how.
	@echo '; see nicto.asm for notes [5c] [6b] etc.'
	@awk '!/^;/; /--$$/; /: doub/,/ret /; /map:$$/,/0b/' \
	  nicto.asm | cat -s || :
# !/^;/ code /--$$/ section heads //,// example, map, registers.
# 'cat -s' squeeze blanks, ':' silence 'make terse | head' error.

status:        # query git, leading into demo:
	git rev-parse @
	git status --short

demo: status clean count run

targets:       # this list.
	@awk '/^# --|^\w/' Makefile
