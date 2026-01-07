# zero-to-compiler -
# Copyright (C) 2025 nineties

ASM_SOURCES := asm.lisp $(wildcard asm/*.lisp)
ASM_EXAMPLE_SOURCES := $(wildcard asm/examples/*.s)
ASM_EXAMPLE_TARGETS := $(ASM_EXAMPLE_SOURCES:%.s=%)

VASM_SOURCES := vasm.lisp $(wildcard vasm/*.lisp)
VASM_EXAMPLE_SOURCES := $(wildcard vasm/examples/*.v)
VASM_EXAMPLE_TARGETS := $(VASM_EXAMPLE_SOURCES:%.v=%)

default:\
	pforth\
	$(ASM_EXAMPLE_TARGETS)\
	$(VASM_EXAMPLE_TARGETS)

pforth: pforth.xxd
	xxd -r -c 8 $< > pforth
	chmod +x pforth

%: %.s pforth plisp.fs $(ASM_SOURCES)
	./pforth < plisp.fs asm.lisp $< $@
	chmod +x $@

%: %.v pforth plisp.fs $(VASM_SOURCES)
	./pforth < plisp.fs vasm.lisp $< $@
	chmod +x $@

.PHONY: clean test
clean:
	rm -f pforth
	rm -f $(ASM_EXAMPLE_TARGETS)
	rm -f $(VASM_EXAMPLE_TARGETS)

test: pforth plisp.fs $(shell find plisp -name "*.lisp")
	./pforth < plisp.fs plisp/test.lisp
