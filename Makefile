# -- VARIABLES.

ASM ?= nasm #   # yasm also works fine.
QEMU ?= qemu-system-i386
I ?= nicto.asm
O ?= nicto.bin

# -- TARGET FILES.

o/$(O): $(I) o  # bootable sector bin. (default)
	# try: make outline, make run, make targets.
	$(ASM) -f bin -o $@ $<
o/nopad-$(O): $(I) o
	$(ASM) -f bin -D NOPAD -o $@ $<
o:              # build outputs directory.
	mkdir -p o

# -- BUILD PHONIES.
.PHONY: all run count clean

all: o/$(O) o/nopad-$(O)
run: o/$(O)     # qemu serial session.
	#
	#  ctrl-a, x to quit qemu.
	#  ctrl-a, c to swap serial<->monitor.
	#  see hello.fs for some code to paste.
	#
	#  make sure your terminal sends backspace 127's.
	#  it does delete from the buffer but not your screen.
	#  an unknown word gives "?".
	#
	$(QEMU) -no-reboot -display none -serial mon:stdio \
	  -drive if=floppy,format=raw,file=$<
count: o/nopad-$(O) # print assembled size.
	wc -c <$<
clean:          # remove o directory.
	rm -rf o

# -- INFO PHONIES.
.PHONY: words outline terse story targets

words:          # system capabilities: the what.
	@awk '/--/ && !/^(;|inte)/ {print $$3}' $(I) | xargs
outline:        # with sections and stack effects.
	@awk '/--/' $(I)

terse:          # implementation details: the how.
	@echo '; (see $(I) for tradeoffs and tricky bits.)'
	@echo '; subroutine-threaded. bp=params, sp=returns, tib=0.'
	@awk '/^; --/ || !/^;/' $(I) | cat -s

story: clean    # design narrative: the why.
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
	cat README.md $(I)
	make count run
	#
	#   ~fin~
	#
	# Ctrl-F "ekipan" for the link back at the top.

targets:        # this list.
	@awk '/^(#|\w)/' Makefile
