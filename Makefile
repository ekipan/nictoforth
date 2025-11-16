ASM ?= yasm # nasm probably works, haven't tested tho.
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
	#  ctrl-a, x to quit.
	#  ctrl-a, c to swap serial<->monitor.
	#  see hello.fs for some code to paste.
	#
	#  make sure your terminal sends backspace 127's.
	#  it does delete from the buffer but not your screen.
	#  no "ok" prompt, but an unknown word gives "?".
	#
	qemu-system-i386 \
	  -drive file=$<,format=raw,if=floppy \
	  -no-reboot -display none -serial mon:stdio

.PHONY: outline terse read story

outline: # system capabilities: the what.
	grep -- -- $(I)
terse: # implementation details: the how.
	@echo '; (see $(I) for tradeoffs and tricky bits.)'
	@echo '; subroutine-threaded. bp=params, sp=returns, tib=0.'
	@awk '/^; --/ || !/^;/' $(I) | cat -s
read: # design narrative: the why.
	cat README.md $(I)
story: clean read count run
	#
	#   ~fin~
	#
	#   https://github.com/ekipan/nictoforth
	#
