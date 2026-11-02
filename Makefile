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


VASM_EXAMPLES := $(wildcard planck/vasm/*.vasm)
BACKEND_EXAMPLES := $(wildcard planck/backend/*.pk)

.PHONY: clean test test-planck0 preprocess
clean:
	rm -f planck/lib/*.pkm
	rm -f pforth
	rm -f plisp2
	rm -f planck0
	rm -f $(ASM_EXAMPLE_TARGETS)
	rm -f $(SASM_EXAMPLE_TARGETS)

test: pforth plisp.fs $(shell find plisp -name "*.lisp")
	./pforth < plisp.fs plisp/test.lisp

# parse the libraries that planck/vasmc.pk uses in advance (planck/lib/*.pkm)
preprocess: planck0
	./planck0 planck/preprocess.pk vasm_build

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
	./planck0 planck/test_highlight.pk
	$(MAKE) preprocess
	tools/test_preprocess.sh
	./planck0 planck/test_vasm.pk
	./planck0 planck/test_vasm2.pk
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
	./planck0 planck/test_backend.pk
	@for f in $(BACKEND_EXAMPLES); do \
		./planck0 $$f tmp-backend || exit 1; \
		./tmp-backend > tmp-backend.out || exit 1; \
		diff -u $${f%.pk}.out tmp-backend.out || exit 1; \
		echo "$$f: ok"; \
	done; rm -f tmp-backend tmp-backend.out
	@for f in $(VASM_EXAMPLES); do \
		./planck0 planck/vasmc.pk $$f tmp-vasm || exit 1; \
		./tmp-vasm > tmp-vasm.out; s=$$?; \
		[ "$$s" = "$$(cat $${f%.vasm}.status)" ] || { echo "$$f: exit status $$s"; exit 1; }; \
		if [ -f $${f%.vasm}.out ]; then diff -u $${f%.vasm}.out tmp-vasm.out || exit 1; fi; \
		echo "$$f: ok"; \
	done; rm -f tmp-vasm tmp-vasm.out
