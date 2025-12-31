ASM ?= yasm # nasm probably works, haven't tested tho.
QEMU ?= qemu-system-i386
I ?= nicto.asm
O ?= nicto.bin

o/$(O): $(I) o
	# try: make outline, make count, make run.
	$(ASM) -f bin -o $@ $<
o/unpadded-$(O): $(I) o
	$(ASM) -f bin -D NOPAD -o $@ $<
o:
	mkdir -p o

.PHONY: clean count run

clean:
	rm -rf o
count: o/unpadded-$(O)
	wc -c $<
run: o/$(O)
	#
	#  ctrl-a, x to quit qemu.
	#  ctrl-a, c to swap serial<->monitor.
	#  see hello.fs for some code to paste.
	#
	#  make sure your terminal sends backspace 127's.
	#  it does delete from the buffer but not your screen.
	#  no "ok" prompt, but an unknown word gives "?".
	#
	$(QEMU) -drive file=$<,format=raw,if=floppy \
	  -no-reboot -display none -serial mon:stdio

.PHONY: words outline terse story

words: # system capabilities: the what.
	@awk '/--/ && !/^(;|inte)/ {print $$3}' $(I) | xargs
outline: # with sections and stack effects.
	@awk '/--/' $(I)

terse: # implementation details: the how.
	@echo '; (see $(I) for tradeoffs and tricky bits.)'
	@echo '; subroutine-threaded. bp=params, sp=returns, tib=0.'
	@awk '/^; --/ || !/^;/' $(I) | cat -s

story: clean # design narrative: the why.
	# I present nictoforth: a space-and-pedagogy-constrained
	# art Forth, in make target format. From README setup to
	# x86 implementation to QEMU serial session, it's
	# carefully crafted to be read top-to-bottom.
	#
	# Here's the repo if you just want the code instead of my
	# blathering. Try "make terse | less", it's cool.
	#
	#     https://github.com/ekipan/nictoforth
	#
	# I haven't shared it widely yet. Butted heads with a
	# Forth Discord person over design philosophy that led to
	# better expectations management and then to a huge size
	# win (credit in git log). He also suggested I store TOS
	# in BX. I'm still mulling it over.
	#
	# I'm hesitant to expand its audience and possibly lose
	# fleetfootedness, I'm a perfectionist who likes to
	# rewrite histories, despite the rudeness.
	#
	# Strap in. Shit gets messy.
	#
	cat README.md $(I)
	make count run
	#
	#   ~fin~
	#
	# Ctrl-F "github" for the link back at the top.
