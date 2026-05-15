# -- VARIABLES.

ASM ?= nasm # yasm also works fine.
QEMU ?= qemu-system-i386 # or: qemu-kvm
QOPT ?= -no-reboot -display none -drive format=raw,file=

# -- TARGET FILES.

o/boot o/list &: nicto.asm o # (default)
	# try: make outline, make run, make targets.
	$(ASM) -f bin -o o/boot -l o/list $<

o/nopad: nicto.asm o
	$(ASM) -f bin -D NOPAD -o $@ $<

o: # (directory)
	mkdir -p o

.PHONY: all run count clean words outline terse story targets

# -- DEVEL PHONIES.

all: o/boot o/nopad

count: o/nopad # print assembled size.
	wc -c <$<

run: o/boot    # qemu serial session.
	#
	#  nictoforth, across an emulated serial line.
	#  see hello.fs for some code to paste.
	#
	#  using qemu:
	#    ctrl-a, x to quit.
	#    ctrl-a, c to swap serial<->monitor.
	#
	#  using nictoforth:
	#    only bksp and return, other controls put garbage.
	#    errors give "?" and reset the stacks.
	#    make sure your terminal sends backspace 127's.
	#    it does delete from the buffer but not your screen.
	#
	$(QEMU) $(QOPT)$< -serial mon:stdio

clean:         # remove o directory.
	rm -rf o

# -- INFO PHONIES.

words:         # system capabilities: the what.
	@awk '/--/ && !/^;|^interp/ {print $$3}' nicto.asm | xargs -n 12

outline:       # with stack effects, as a reading aide.
	@awk '/--/' nicto.asm

terse:         # implementation details: the how.
	@echo '; see nicto.asm for notes [5c] [6b] etc.'
	@awk '!/^;/; /^; --/; /: doub/,/ret /; /map:$$/,/x86/' \
	  nicto.asm | cat -s || :
# !/^;/ code /^; --/ section heads //,// example, map+registers.
# 'cat -s' squeeze blanks, ':' silence 'make terse | head' error.

status:         # query git, leading into demo:
	git rev-parse @
	git status --short

demo: status clean count run

targets:       # this list.
	@awk '/^# --|^\w/' Makefile
