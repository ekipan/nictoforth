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
	#  ctrl-a, x to quit qemu.
	#  ctrl-a, c to swap serial<->monitor.
	#  see hello.fs for some code to paste.
	#
	#  make sure your terminal sends backspace 127's.
	#  it does delete from the buffer but not your screen.
	#  an unknown word gives "?".
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

story:         # design narrative: the why.
	git rev-parse @
	git status --short
	#
	# I present nictoforth: a space-and-pedagogy-constrained
	# art Forth, in make target format. From README setup to
	# x86 implementation to QEMU serial session, it's
	# carefully crafted to be read top-to-bottom.
	#
	# Here's the repo if you just want the code instead of my
	# blathering. Try "make terse | less" or "make targets".
	#
	#     https://github.com/ekipan/nictoforth
	#
	# Strap in. Shit gets messy.
	#
	cat README.md nicto.asm
	make clean count run
	#
	#     ~fin~
	#
	#     https://github.com/ekipan/nictoforth

targets:       # this list.
	@awk '/^# --|^\w/' Makefile
