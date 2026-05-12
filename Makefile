# -- VARIABLES.

ASM ?= nasm # yasm also works fine.
QEMU ?= qemu-system-i386 # or: qemu-kvm

# -- TARGET FILES.

o/nicto.bin o/nicto.lst &: nicto.asm o # bootable image. (default)
	# try: make outline, make run, make targets.
	$(ASM) -f bin -o o/nicto.bin -l o/nicto.lst $<

o/nopad.bin: nicto.asm o
	$(ASM) -f bin -D NOPAD -o $@ $<

o:                 # build outputs directory.
	mkdir -p o

.PHONY: all run count clean words outline terse show story targets

# -- DEVEL PHONIES.

all: o/nicto.bin o/nopad.bin

count: o/nopad.bin # print assembled size.
	wc -c <$<

run: o/nicto.bin   # qemu serial session.
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

clean:             # remove o directory.
	rm -rf o

# -- INFO PHONIES.

words:             # system capabilities: the what.
	@awk '/--/ && !/^;|^interp/ {print $$3}' nicto.asm | xargs -n 12

outline:           # with sections and stack effects.
	@awk '/--/' nicto.asm

terse:             # implementation details: the how.
	@echo '; (see nicto.asm for tradeoffs and tricky bits.)'
	@echo '; subroutine-threaded. bp=params, sp=returns, tib=0.'
	@echo '; dict fmt: dw link | db len,'\''name'\'' | dw xt'
	@awk '/^; --/ || !/^;/' nicto.asm | cat -s # squeeze blanks.

show:              # design narrative: the why.
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

story: clean show count run # and demonstration.
	#
	#     ~fin~
	#
	#     https://github.com/ekipan/nictoforth

targets:           # this list.
	@awk '/^# --|^\w/' Makefile
