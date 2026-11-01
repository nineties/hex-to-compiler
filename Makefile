# zero-to-compiler -
# Copyright (C) 2025 nineties

ASM_SOURCES := asm.lisp $(wildcard asm/*.lisp)
ASM_EXAMPLE_SOURCES := $(wildcard asm/examples/*.s)
ASM_EXAMPLE_TARGETS := $(ASM_EXAMPLE_SOURCES:%.s=%)

SASM_SOURCES := sasm.lisp $(wildcard sasm/*.lisp)
SASM_LIBS	:= $(wildcard sasm/lib/*.sv)
SASM_EXAMPLE_SOURCES := $(wildcard sasm/examples/*.sv)
SASM_EXAMPLE_TARGETS := $(SASM_EXAMPLE_SOURCES:%.sv=%)

PLANCK_EXAMPLES := $(wildcard planck/examples/*.pk)

PLISP := $(shell if [ -f ./plisp2 ]; then echo "./plisp2"; else echo "./pforth < plisp.fs"; fi)

default:\
	pforth\
	plisp2\
	planck0

example:\
	$(ASM_EXAMPLE_TARGETS)\
	$(SASM_EXAMPLE_TARGETS)

pforth: pforth.xxd
	xxd -r -c 8 $< > pforth
	chmod +x pforth

planck0: planck0.sv plisp2 $(SASM_LIBS)
	-time ./plisp2 sasm.lisp $< $@
	-chmod +x $@

%: %.s pforth plisp.fs $(ASM_SOURCES)
	-time $(PLISP) asm.lisp $< $@
	-chmod +x $@

%: %.sv pforth plisp.fs $(SASM_SOURCES) $(ASM_SOURCES) $(SASM_LIBS)
	-time $(PLISP) sasm.lisp $< tmp-$@
	-mv tmp-$@ $@
	-chmod +x $@

.PHONY: clean test test-planck0
clean:
	rm -f pforth
	rm -f plisp2
	rm -f planck0
	rm -f $(ASM_EXAMPLE_TARGETS)
	rm -f $(SASM_EXAMPLE_TARGETS)

test: pforth plisp.fs $(shell find plisp -name "*.lisp")
	./pforth < plisp.fs plisp/test.lisp

# run tests of planck0 and compare outputs of examples with *.out
test-planck0: planck0
	./planck0 planck/test.pk
	./planck0 planck/test_syntax.pk
	./planck0 planck/test_define.pk
	./planck0 planck/test_builtins.pk one "two words"
	./planck0 planck/test_lib.pk
	./planck0 planck/test_unparse.pk
	./planck0 planck/test_unparse_pattern.pk
	./planck0 planck/test_unparse_file.pk
	@for f in $(PLANCK_EXAMPLES); do \
		./planck0 $$f > tmp-example.out || exit 1; \
		diff -u $${f%.pk}.out tmp-example.out || exit 1; \
		echo "$$f: ok"; \
	done; rm -f tmp-example.out
	@for f in planck/errors/*.pk; do \
		./planck0 $$f > tmp-error.out 2> tmp-error.err && { echo "$$f: should stop"; exit 1; }; \
		diff -u $${f%.pk}.out tmp-error.out || exit 1; \
		diff -u $${f%.pk}.err tmp-error.err || exit 1; \
		echo "$$f: stops"; \
	done; rm -f tmp-error.out tmp-error.err
