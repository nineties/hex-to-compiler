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
	-time $(PLISP) bootstrap/sasm.lisp $< tmp-$@
	-mv tmp-$@ $@
	-chmod +x $@


VASM_EXAMPLES := $(wildcard planck/examples/vasm/*.vasm)
PSYS_EXAMPLES := $(wildcard planck/examples/psys/*.psys)
VASM_TARGETS := x86 x86_64
BACKEND_EXAMPLES := $(wildcard planck/examples/backend/*.pk)
PE_EXAMPLES := $(wildcard planck/examples/pe/*.pk)

.PHONY: clean test test-planck0 preprocess
clean:
	rm -f planck/lib/*.pkm planck/lib/*/*.pkm
	rm -f pforth
	rm -f plisp2
	rm -f planck0
	rm -f $(ASM_EXAMPLE_TARGETS)
	rm -f $(SASM_EXAMPLE_TARGETS)

test: pforth bootstrap/plisp.fs $(shell find bootstrap/plisp -name "*.lisp")
	./pforth < bootstrap/plisp.fs bootstrap/plisp/test.lisp

# parse the libraries that planck/vasmc.pk uses in advance (planck/lib/*/*.pkm)
preprocess: planck0
	./planck0 tools/preprocess.pk vasm/build psys/expand

# run tests of planck0 and compare outputs of examples with *.out
test-planck0: planck0
	./planck0 planck/tests/core.pk
	./planck0 planck/tests/syntax.pk
	./planck0 planck/tests/define.pk
	./planck0 planck/tests/builtins.pk one "two words"
	./planck0 planck/tests/lib.pk
	./planck0 planck/tests/unparse.pk
	./planck0 planck/tests/unparse_pattern.pk
	./planck0 planck/tests/unparse_file.pk
	./planck0 planck/tests/highlight.pk
	$(MAKE) preprocess
	tools/test_preprocess.sh
	./planck0 planck/tests/vasm.pk
	./planck0 planck/tests/vasm2.pk
	./planck0 planck/tests/psys.pk
	./planck0 planck/tests/pe.pk
	@for f in $(PLANCK_EXAMPLES); do \
		./planck0 $$f > tmp-example.out || exit 1; \
		diff -u $${f%.pk}.out tmp-example.out || exit 1; \
		echo "$$f: ok"; \
	done; rm -f tmp-example.out
	@for f in planck/tests/errors/*.pk; do \
		./planck0 $$f > tmp-error.out 2> tmp-error.err && { echo "$$f: should stop"; exit 1; }; \
		diff -u $${f%.pk}.out tmp-error.out || exit 1; \
		diff -u $${f%.pk}.err tmp-error.err || exit 1; \
		echo "$$f: stops"; \
	done; rm -f tmp-error.out tmp-error.err
	./planck0 planck/tests/backend.pk
	./planck0 planck/tests/x64.pk
	./planck0 planck/tests/target.pk
	./planck0 planck/tests/unit.pk
	@for f in $(BACKEND_EXAMPLES); do \
		./planck0 $$f tmp-backend || exit 1; \
		./tmp-backend > tmp-backend.out || exit 1; \
		diff -u $${f%.pk}.out tmp-backend.out || exit 1; \
		echo "$$f: ok"; \
	done; rm -f tmp-backend tmp-backend.out
	@for t in $(VASM_TARGETS); do for f in $(VASM_EXAMPLES) $(PSYS_EXAMPLES); do \
		./planck0 planck/vasmc.pk $$f tmp-vasm $$t || exit 1; \
		./tmp-vasm > tmp-vasm.out; s=$$?; \
		[ "$$s" = "$$(cat $${f%.*}.status)" ] || { echo "$$f ($$t): exit status $$s"; exit 1; }; \
		if [ -f $${f%.*}.out ]; then diff -u $${f%.*}.out tmp-vasm.out || exit 1; fi; \
		echo "$$f ($$t): ok"; \
	done; done; rm -f tmp-vasm tmp-vasm.out
	@for f in $(PE_EXAMPLES); do \
		./planck0 $$f tmp-pe > tmp-pe.out || exit 1; \
		diff -u $${f%.pk}.out tmp-pe.out || exit 1; \
		./tmp-pe tmp-pe > tmp-pe.run; s=$$?; \
		[ "$$s" = "$$(cat $${f%.pk}.status)" ] || { echo "$$f: exit status $$s"; exit 1; }; \
		diff -u $${f%.pk}.run tmp-pe.run || exit 1; \
		echo "$$f: ok"; \
	done; rm -f tmp-pe tmp-pe.out tmp-pe.run tmp-pe-file
