# zero-to-compiler -
# Copyright (C) 2025 nineties

ASM_EXAMPLE_SOURCES := $(wildcard asm/examples/*.s)
ASM_EXAMPLE_TARGETS := $(ASM_EXAMPLE_SOURCES:%.s=%)

default: pforth $(ASM_EXAMPLE_TARGETS)

pforth: pforth.xxd
	xxd -r -c 8 $< > pforth
	chmod +x pforth

%: %.s pforth plisp.fs $(shell find . -name "*.lisp")
	./pforth < plisp.fs asm.lisp $< $@
	chmod +x $@

.PHONY: clean test
clean:
	rm -f pforth
	rm -f $(ASM_EXAMPLE_TARGETS)

test: pforth plisp.fs $(shell find plisp -name "*.lisp")
	./pforth < plisp.fs plisp/test.lisp
