# zero-to-compiler -
# Copyright (C) 2025 nineties

ASM_SOURCES := bootstrap/asm.lisp $(wildcard bootstrap/asm/*.lisp)
ASM_EXAMPLE_SOURCES := $(wildcard bootstrap/asm/examples/*.s)
ASM_EXAMPLE_TARGETS := $(ASM_EXAMPLE_SOURCES:%.s=%)

SASM_SOURCES := bootstrap/sasm.lisp $(wildcard bootstrap/sasm/*.lisp)
SASM_LIBS	:= $(wildcard bootstrap/sasm/lib/*.sv)
SASM_EXAMPLE_SOURCES := $(wildcard bootstrap/sasm/examples/*.sv)
SASM_EXAMPLE_TARGETS := $(SASM_EXAMPLE_SOURCES:%.sv=%)

PLANCK_EXAMPLES := $(wildcard planck/examples/*.pk)

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


.PHONY: clean test test-sasm test-planck0 test-all preprocess
clean:
	rm -f planck/lib/*.pkm planck/lib/*/*.pkm
	rm -f pforth
	rm -f plisp2
	rm -f planck0
	rm -f $(ASM_EXAMPLE_TARGETS)
	rm -f $(SASM_EXAMPLE_TARGETS)

test: pforth bootstrap/plisp.fs $(shell find bootstrap/plisp -name "*.lisp")
	./pforth < bootstrap/plisp.fs bootstrap/plisp/test.lisp

preprocess: planck0
	./planck0 tools/preprocess.pk pretty list string

test-sasm: bootstrap/sasm/examples/switch
	./bootstrap/sasm/examples/switch

# run the core tests of planck0: tests of the language and the examples in tests/errors
test-planck0: planck0
	./planck0 planck/tests/core.pk
	./planck0 planck/tests/syntax.pk
	./planck0 planck/tests/macro.pk
	./planck0 planck/tests/define.pk
	./planck0 planck/tests/module.pk
	./planck0 planck/tests/import.pk
	./planck0 planck/tests/builtins.pk one "two words"
	./planck0 planck/tests/bigint.pk
	@for f in planck/tests/errors/*.pk; do \
		./planck0 $$f > tmp-error.out 2> tmp-error.err && { echo "$$f: should stop"; exit 1; }; \
		diff -u $${f%.pk}.out tmp-error.out || exit 1; \
		diff -u $${f%.pk}.err tmp-error.err || exit 1; \
		echo "$$f: stops"; \
	done; rm -f tmp-error.out tmp-error.err

test-all: test-planck0
	./planck0 planck/tests/lib.pk
	$(MAKE) preprocess
	tools/test_preprocess.sh
	@for f in $(PLANCK_EXAMPLES); do \
		./planck0 $$f > tmp-example.out || exit 1; \
		diff -u $${f%.pk}.out tmp-example.out || exit 1; \
		echo "$$f: ok"; \
	done; rm -f tmp-example.out
