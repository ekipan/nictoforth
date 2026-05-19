
# -- VARIABLES. important targets: [!]

SRC ?= nicto.asm
ASM ?= nasm # yasm also works fine.
QEMU ?= qemu-system-i386 # or: qemu-kvm

# -- TARGET FILES.

o/boot: $(SRC) o/dir # (default)
	# try: make run, make outline, make targets.
	$(ASM) -f bin -l $@l -o $@ $<

o/nopad: $(SRC) o/dir
	$(ASM) -f bin -l $@l -o $@ -D NOPAD $<

o/dir:
	mkdir -p o; touch $@

# -- DEVEL PHONIES.

.PHONY: clean all count run status demo

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

status:        # query git, leading into demo:
	git rev-parse @
	git status --short

demo: status clean count run

# -- INFO PHONIES.
# pipe these into less or bat, put them in a file, whatev.
# '||:' silence SIGPIPEs. 'cat -s' squeeze blanks.

.PHONY: words outline xrefs notes doc terse targets notes

words:    # compact list of the implemented words.
	@awk '/--/ && !/^;|^interp/ {print $$3}' $(SRC) | xargs -n 12 ||:

outline:  # with stack effects, as a reading aide. [!]
	@awk '/--/' $(SRC) ||:

xrefs:    # inventory cross-ref anchors, for maintenance.
	@awk '/; \[/' $(SRC) ||:

notes:    # anchored note contents, a dense spec.
	@awk '/^; \[/,/^$$/' $(SRC) ||:

doc:      # or the entirety of the asides.
	@awk '/^;/; /^$$/' $(SRC) | cat -s ||:

terse:    # just the code, no asides. [!]
	@echo '; see $(SRC) for notes [5c] [6b] etc.'
	@awk '!/^;/; /--$$/; /: doub/,/ret /; /^; \[0a/,/0b/' \
	  $(SRC) | cat -s ||:

targets:  # this list.
	@awk '/^# --|^\w/' Makefile ||:
