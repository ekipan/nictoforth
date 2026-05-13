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

.PHONY: all run count clean words outline terse show story targets

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

outline:       # with sections and stack effects.
	@awk '/--/' nicto.asm

terse:         # implementation details: the how.
	@printf "\
	; (see nicto.asm for tradeoffs and tricky bits.)\n\
	; subroutine-threaded. bp=params sp=returns tib=0.\n\
	;   : double dup + ; \ this compiles to:\n\
	; dw prev | db 6,'double' | dw double ; dict data\n\
	; double: call dup | call plus | ret  ; instructions\n"
	@awk '/^; --/ || !/^;/' nicto.asm | cat -s # squeeze blanks.

show:          # design narrative: the why.
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

targets:       # this list.
	@awk '/^# --|^\w/' Makefile
