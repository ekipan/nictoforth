ASM ?= yasm # nasm probably works, haven't tested tho.
I ?= nicto.asm
O ?= nicto.bin

$(O): $(I)
	# try: make outline, make count, make run.
	$(ASM) -f bin -o $@ $<
unpadded-$(O): $(I)
	$(ASM) -f bin -D NOPAD -o $@ $<

.PHONY: clean count run

clean:
	rm -f *.bin
count: unpadded-$(O)
	wc -c $< && rm $<
run: $(O)
	#
	#  ctrl-a, x to quit.
	#  ctrl-a, c to swap serial<->monitor.
	#  see nicto.fs for some code to paste.
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
	@grep -- -- $(I)
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
