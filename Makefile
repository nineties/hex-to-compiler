# zero-to-compiler -
# Copyright (C) 2025 nineties

ASM_SOURCES := asm.lisp $(wildcard asm/*.lisp)
ASM_EXAMPLE_SOURCES := $(wildcard asm/examples/*.s)
ASM_EXAMPLE_TARGETS := $(ASM_EXAMPLE_SOURCES:%.s=%)

SASM_SOURCES := sasm.lisp $(wildcard sasm/*.lisp)
SASM_LIBS	:= $(wildcard sasm/lib/*.sv)
SASM_EXAMPLE_SOURCES := $(wildcard sasm/examples/*.sv)
SASM_EXAMPLE_TARGETS := $(SASM_EXAMPLE_SOURCES:%.sv=%)

default:\
	pforth\
	plisp2

example:\
	$(ASM_EXAMPLE_TARGETS)\
	$(SASM_EXAMPLE_TARGETS)

pforth: pforth.xxd
	xxd -r -c 8 $< > pforth
	chmod +x pforth

%: %.s pforth plisp.fs $(ASM_SOURCES)
	-time ./pforth < plisp.fs asm.lisp $< $@
	-chmod +x $@

%: %.sv pforth plisp.fs $(SASM_SOURCES) $(ASM_SOURCES) $(SASM_LIBS)
	-time ./pforth < plisp.fs sasm.lisp $< $@
	-chmod +x $@

.PHONY: clean test
clean:
	rm -f pforth
	rm -f plisp2
	rm -f $(ASM_EXAMPLE_TARGETS)
	rm -f $(SASM_EXAMPLE_TARGETS)

test: pforth plisp.fs $(shell find plisp -name "*.lisp")
	./pforth < plisp.fs plisp/test.lisp
