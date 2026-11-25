# zero-to-compiler -
# Copyright (C) 2025 nineties

ASM_SOURCES := bootstrap/asm.lisp $(wildcard bootstrap/asm/*.lisp)
ASM_EXAMPLE_SOURCES := $(wildcard bootstrap/asm/examples/*.s)
ASM_EXAMPLE_TARGETS := $(ASM_EXAMPLE_SOURCES:%.s=%)

SASM_SOURCES := bootstrap/sasm.lisp $(wildcard bootstrap/sasm/*.lisp)
SASM_LIBS	:= $(wildcard bootstrap/sasm/lib/*.sv)
SASM_EXAMPLE_SOURCES := $(wildcard bootstrap/sasm/examples/*.sv)
SASM_EXAMPLE_TARGETS := $(SASM_EXAMPLE_SOURCES:%.sv=%)

PLISP := $(shell if [ -f ./plisp2 ]; then echo "./plisp2"; else echo "./pforth < bootstrap/plisp.fs"; fi)

default:\
	pforth\
	plisp2\
	planck0

example:\
	$(ASM_EXAMPLE_TARGETS)\
	$(SASM_EXAMPLE_TARGETS)

pforth: bootstrap/pforth.xxd
	xxd -r -c 8 $< > pforth
	chmod +x pforth

plisp2: bootstrap/plisp2.sv pforth bootstrap/plisp.fs $(SASM_SOURCES) $(ASM_SOURCES) $(SASM_LIBS)
	-time $(PLISP) bootstrap/sasm.lisp $< tmp-$@
	-mv tmp-$@ $@
	-chmod +x $@

planck0: bootstrap/planck0.sv plisp2 $(SASM_LIBS)
	-time ./plisp2 bootstrap/sasm.lisp $< $@
	-chmod +x $@

%: %.s pforth bootstrap/plisp.fs $(ASM_SOURCES)
	-time $(PLISP) bootstrap/asm.lisp $< $@
	-chmod +x $@

%: %.sv pforth bootstrap/plisp.fs $(SASM_SOURCES) $(ASM_SOURCES) $(SASM_LIBS)
	@mkdir -p $(dir tmp-$@)
	-time $(PLISP) bootstrap/sasm.lisp $< tmp-$@
	-mv tmp-$@ $@
	-chmod +x $@


.PHONY: clean test test-sasm test-planck0
clean:
	rm -f pforth
	rm -f plisp2
	rm -f planck0
	rm -f $(ASM_EXAMPLE_TARGETS)
	rm -f $(SASM_EXAMPLE_TARGETS)

test: pforth bootstrap/plisp.fs $(shell find bootstrap/plisp -name "*.lisp")
	./pforth < bootstrap/plisp.fs bootstrap/plisp/test.lisp

test-sasm: bootstrap/sasm/examples/switch
	./bootstrap/sasm/examples/switch

test-planck0: planck0
	./planck0 /dev/null
