# zero-to-compiler -
# Copyright (C) 2025 nineties

ASM_SOURCES := asm.lisp $(wildcard asm/*.lisp)
ASM_EXAMPLE_SOURCES := $(wildcard asm/examples/*.s)
ASM_EXAMPLE_TARGETS := $(ASM_EXAMPLE_SOURCES:%.s=%)

PCC_SOURCES := pcc.lisp $(wildcard pcc/*.lisp)
PCC_LIBS	:= $(wildcard pcc/lib/*.pc)
PCC_EXAMPLE_SOURCES := $(wildcard pcc/examples/*.pc)
PCC_EXAMPLE_TARGETS := $(PCC_EXAMPLE_SOURCES:%.pc=%)

default:\
	pforth\
	plisp2

example:\
	$(ASM_EXAMPLE_TARGETS)\
	$(PCC_EXAMPLE_TARGETS)

pforth: pforth.xxd
	xxd -r -c 8 $< > pforth
	chmod +x pforth

%: %.s pforth plisp.fs $(ASM_SOURCES)
	-./pforth < plisp.fs asm.lisp $< $@
	-chmod +x $@
	-./$@

%: %.pc pforth plisp.fs $(PCC_SOURCES) $(ASM_SOURCES) $(PCC_LIBS)
	-./pforth < plisp.fs pcc.lisp $< $@
	-chmod +x $@
	-./$@

.PHONY: clean test
clean:
	rm -f pforth
	rm -f $(ASM_EXAMPLE_TARGETS)
	rm -f $(PCC_EXAMPLE_TARGETS)

test: pforth plisp.fs $(shell find plisp -name "*.lisp")
	./pforth < plisp.fs plisp/test.lisp
