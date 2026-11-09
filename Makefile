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
NATIVE_EXAMPLES := $(wildcard planck/examples/native/*.native)
VASM_TARGETS := x86_64
VM_EXAMPLES := $(wildcard planck/examples/vm/*.pk)
BACKEND_EXAMPLES := $(wildcard planck/examples/backend/*.pk)

.PHONY: clean test test-planck0 test-all test-vm test-udiv preprocess
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
	./planck0 tools/preprocess.pk vasm/build native/compile pvm/image

# run the core tests of planck0: tests of the language and the examples in tests/errors
test-planck0: planck0
	./planck0 planck/tests/core.pk
	./planck0 planck/tests/syntax.pk
	./planck0 planck/tests/macro.pk
	./planck0 planck/tests/define.pk
	./planck0 planck/tests/module.pk
	./planck0 planck/tests/import.pk
	./planck0 planck/tests/builtins.pk one "two words"
	@for f in planck/tests/errors/*.pk; do \
		./planck0 $$f > tmp-error.out 2> tmp-error.err && { echo "$$f: should stop"; exit 1; }; \
		diff -u $${f%.pk}.out tmp-error.out || exit 1; \
		diff -u $${f%.pk}.err tmp-error.err || exit 1; \
		echo "$$f: stops"; \
	done; rm -f tmp-error.out tmp-error.err

# build planck/vm as units and run the programs of planck/examples/vm on it; the standard input is the file of the same name with .in, or empty
VM_UNITS := mem gc big flt dec num obj bit trans nb arr eq builtin hsh ctl ops vm
test-vm: planck0
	@done=; for u in $(VM_UNITS); do \
		./planck0 planck/vasmc.pk unit planck/vm/$$u.native tmp-vm-$$u.unit x86_64 $$done || exit 1; \
		done="$$done tmp-vm-$$u.unit"; \
	done; ./planck0 planck/vasmc.pk link tmp-vm x86_64 $$done || exit 1
	@for f in $(VM_EXAMPLES); do \
		./planck0 $$f tmp-vm.img || exit 1; \
		in=/dev/null; if [ -f $${f%.pk}.in ]; then in=$${f%.pk}.in; fi; \
		./tmp-vm tmp-vm.img < $$in > tmp-vm.out || exit 1; \
		diff -u $${f%.pk}.out tmp-vm.out || exit 1; \
		echo "$$f: ok"; \
	done; rm -f tmp-vm tmp-vm-*.unit tmp-vm.img tmp-vm.out

# check the division by the reciprocal of planck/vm/udiv.native against its definition; the status of the program is the number of wrong results
test-udiv: planck0
	./planck0 planck/vasmc.pk unit planck/vm/udiv.native tmp-udiv-lib.unit x86_64
	./planck0 planck/vasmc.pk unit planck/tests/udiv/main.native tmp-udiv-main.unit x86_64 tmp-udiv-lib.unit
	./planck0 planck/vasmc.pk link tmp-udiv x86_64 tmp-udiv-lib.unit tmp-udiv-main.unit
	./tmp-udiv; s=$$?; rm -f tmp-udiv tmp-udiv-*.unit; exit $$s

# run everything: test-planck0 and then the libraries, the tools, the vasm, native and backend tests, and the examples
test-all: test-planck0
	./planck0 planck/tests/lib.pk
	./planck0 planck/tests/unparse.pk
	./planck0 planck/tests/unparse_pattern.pk
	./planck0 planck/tests/unparse_file.pk
	./planck0 planck/tests/highlight.pk
	$(MAKE) preprocess
	tools/test_preprocess.sh
	./planck0 planck/tests/vasm.pk
	./planck0 planck/tests/native.pk
	$(MAKE) test-udiv
	$(MAKE) test-vm
	@for f in $(PLANCK_EXAMPLES); do \
		./planck0 $$f > tmp-example.out || exit 1; \
		diff -u $${f%.pk}.out tmp-example.out || exit 1; \
		echo "$$f: ok"; \
	done; rm -f tmp-example.out
	./planck0 planck/tests/backend.pk
	./planck0 planck/tests/x64.pk
	./planck0 planck/tests/target.pk
	./planck0 planck/tests/unit.pk
	./planck0 tools/test.pk backend/cfg backend/x64enc backend/x64 backend/target
	@for f in $(BACKEND_EXAMPLES); do \
		./planck0 $$f tmp-backend || exit 1; \
		./tmp-backend > tmp-backend.out || exit 1; \
		diff -u $${f%.pk}.out tmp-backend.out || exit 1; \
		echo "$$f: ok"; \
	done; rm -f tmp-backend tmp-backend.out
	@for t in $(VASM_TARGETS); do for f in $(VASM_EXAMPLES) $(NATIVE_EXAMPLES); do \
		./planck0 planck/vasmc.pk $$f tmp-vasm $$t || exit 1; \
		./tmp-vasm > tmp-vasm.out; s=$$?; \
		[ "$$s" = "$$(cat $${f%.*}.status)" ] || { echo "$$f ($$t): exit status $$s"; exit 1; }; \
		if [ -f $${f%.*}.out ]; then diff -u $${f%.*}.out tmp-vasm.out || exit 1; fi; \
		echo "$$f ($$t): ok"; \
	done; done; rm -f tmp-vasm tmp-vasm.out
	@for t in $(VASM_TARGETS); do \
		./planck0 planck/vasmc.pk unit planck/examples/units/lib.native tmp-lib.unit $$t || exit 1; \
		./planck0 planck/vasmc.pk unit planck/examples/units/main.native tmp-main.unit $$t tmp-lib.unit || exit 1; \
		./planck0 planck/vasmc.pk link tmp-units $$t tmp-lib.unit tmp-main.unit || exit 1; \
		./tmp-units; s=$$?; \
		[ "$$s" = "$$(cat planck/examples/units/main.status)" ] || { echo "planck/examples/units ($$t): exit status $$s"; exit 1; }; \
		echo "planck/examples/units ($$t): ok"; \
	done; rm -f tmp-lib.unit tmp-main.unit tmp-units
	@./planck0 planck/vasmc.pk unit planck/examples/units/lib.native tmp-lib.unit x86_64 || exit 1
	@for f in planck/tests/units/*.native; do \
		./planck0 planck/vasmc.pk unit $$f tmp-unit.unit x86_64 tmp-lib.unit > tmp-unit.out && { echo "$$f: should stop"; exit 1; }; \
		diff -u $${f%.native}.out tmp-unit.out || exit 1; \
		echo "$$f: stops"; \
	done; rm -f tmp-lib.unit tmp-unit.unit tmp-unit.out
