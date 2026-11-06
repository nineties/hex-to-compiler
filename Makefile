# zero-to-compiler -
# Copyright (C) 2025 nineties

default:\
	pforth\
	plisp2\
	planck0

include bootstrap/plisp2.mk
include bootstrap/planck0.mk

PLANCK_EXAMPLES := $(wildcard planck/examples/*.pk)

example:\
	$(ASM_EXAMPLE_TARGETS)\
	$(SASM_EXAMPLE_TARGETS)

VASM_EXAMPLES := $(wildcard planck/examples/vasm/*.vasm)
NATIVE_EXAMPLES := $(wildcard planck/examples/native/*.native)
VASM_TARGETS := x86_64
VM_EXAMPLES := $(wildcard planck/examples/vm/*.pk)
BACKEND_EXAMPLES := $(wildcard planck/examples/backend/*.pk)
PARTIAL_EVAL_EXAMPLES := $(wildcard planck/examples/partial_eval/*.pk)

.PHONY: clean test test-planck0 test-all test-vm preprocess
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

# build planck/vm as units and run the programs of planck/examples/vm on it
VM_UNITS := mem big flt dec num obj bit trans nb arr builtin hsh ctl ops vm
test-vm: planck0
	@done=; for u in $(VM_UNITS); do \
		./planck0 planck/vasmc.pk unit planck/vm/$$u.native tmp-vm-$$u.unit x86_64 $$done || exit 1; \
		done="$$done tmp-vm-$$u.unit"; \
	done; ./planck0 planck/vasmc.pk link tmp-vm x86_64 $$done || exit 1
	@for f in $(VM_EXAMPLES); do \
		./planck0 $$f tmp-vm.img || exit 1; \
		./tmp-vm tmp-vm.img > tmp-vm.out || exit 1; \
		diff -u $${f%.pk}.out tmp-vm.out || exit 1; \
		echo "$$f: ok"; \
	done; rm -f tmp-vm tmp-vm-*.unit tmp-vm.img tmp-vm.out

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
	$(MAKE) test-vm
	./planck0 planck/tests/partial_eval.pk
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
	@for f in $(PARTIAL_EVAL_EXAMPLES); do \
		./planck0 $$f tmp-pe > tmp-pe.out || exit 1; \
		diff -u $${f%.pk}.out tmp-pe.out || exit 1; \
		./tmp-pe tmp-pe > tmp-pe.run; s=$$?; \
		[ "$$s" = "$$(cat $${f%.pk}.status)" ] || { echo "$$f: exit status $$s"; exit 1; }; \
		diff -u $${f%.pk}.run tmp-pe.run || exit 1; \
		echo "$$f: ok"; \
	done; rm -f tmp-pe tmp-pe.out tmp-pe.run tmp-pe-file
