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
VM_ALL_EXAMPLES := $(wildcard planck/examples/vm/*.pk)
# the programs that use a float are left out of the examples of VM0, the planckvm without floating point
VM0_EXAMPLES := $(filter-out planck/examples/vm/float.pk planck/examples/vm/plain_float.pk planck/examples/vm/trans.pk planck/examples/vm/bits.pk planck/examples/vm/equality.pk planck/examples/vm/plain_int.pk,$(wildcard planck/examples/vm/*.pk))
BACKEND_EXAMPLES := $(wildcard planck/examples/backend/*.pk)

.PHONY: clean test test-planck0 test-all test-vm0 test-vm test-udiv test-fm preprocess
clean:
	rm -f planck/lib/*.pkm planck/lib/*/*.pkm
	rm -f pforth
	rm -f plisp2
	rm -f planck0
	rm -rf tmp-vm tmp-vm-build tmp-vmf tmp-vmf-build
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

# planck/vm is built as units. tmp-vm is VM0, the planckvm without floating point: its flt, dec and trans are those of planck/vm0, which stop on a float.
# tmp-vmf is the planckvm with floating point: it has the flt, dec and trans of planck/vm, and fm with the coefficients that VM0 made (fmk).
# The other units are the same in both, so they are compiled once. The standard input of an example is the file of the same name with .in, or empty.
VM_UNITS := mem gc udiv big flt dec num obj bit trans nb arr eq builtin hsh ctl ops vm
VM_BUILD := tmp-vm-build
VM_UNIT_FILES := $(addprefix $(VM_BUILD)/,$(addsuffix .unit,$(VM_UNITS)))
# everything that a unit depends on besides its source: the compiler, which is planck/vasmc.pk and the libraries that it uses
VASMC_SOURCES := planck/vasmc.pk planck/init.pk planck/peg.pk planck/syntax.pk $(shell find planck/lib -name "*.pk")
# the unit files of the units that planck/vm/$(1).native imports
vm_src = $(if $(filter flt dec trans,$(1)),planck/vm0,planck/vm)/$(1).native
vm_imports = $(addprefix $(VM_BUILD)/,$(addsuffix .unit,$(shell sed -n 's/^import \([A-Za-z0-9_]*\).*/\1/p' $(call vm_src,$(1)))))
# a unit is compiled once the units that it imports are, so make -j compiles the units that do not depend on each other at the same time
define VM_UNIT_RULE
$(VM_BUILD)/$(1).unit: $(call vm_src,$(1)) $(call vm_imports,$(1)) $(VASMC_SOURCES) planck0 | $(VM_BUILD)
	./planck0 planck/vasmc.pk unit $$< $$@ x86_64 $(call vm_imports,$(1))
endef
$(foreach u,$(VM_UNITS),$(eval $(call VM_UNIT_RULE,$(u))))
$(VM_BUILD):
	mkdir -p $@
tmp-vm: $(VM_UNIT_FILES) planck0
	./planck0 planck/vasmc.pk link $@ x86_64 $(VM_UNIT_FILES)

# the coefficients and the constants of the floating point of planck/vm are written by tools/gen_fm_vm0.pk, which runs on tmp-vm (the planckvm without floating point)
$(VM_BUILD)/fmk.img: tools/gen_fm_vm0.pk planck/lib/pvm/cas.pk planck/lib/pvm/compile.pk planck0 | $(VM_BUILD)
	./planck0 tools/gen_fm_vm0.pk $@
$(VM_BUILD)/fmk.native: $(VM_BUILD)/fmk.img tmp-vm
	./tmp-vm $< > $@
$(VM_BUILD)/fmk.unit: $(VM_BUILD)/fmk.native $(VASMC_SOURCES) planck0
	./planck0 planck/vasmc.pk unit $< $@ x86_64

# check exp, log, pow, sin, cos and atan2 of planck/vm/fm.native; the status of the program is the number of results more than 1 ulp apart
test-fm: $(VM_BUILD)/fmk.unit planck0
	./planck0 planck/vasmc.pk unit planck/vm/fm.native $(VM_BUILD)/fm.unit x86_64 $(VM_BUILD)/fmk.unit
	./planck0 planck/vasmc.pk unit planck/tests/fm/main.native $(VM_BUILD)/fmtest.unit x86_64 $(VM_BUILD)/fm.unit
	./planck0 planck/vasmc.pk link $(VM_BUILD)/fmtest x86_64 $(VM_BUILD)/fmk.unit $(VM_BUILD)/fm.unit $(VM_BUILD)/fmtest.unit
	$(VM_BUILD)/fmtest

# the units of tmp-vmf: flt, dec and trans of planck/vm, and fm, which needs the units that VM0 made
VMF_BUILD := tmp-vmf-build
VMF_REAL := flt dec trans
VMF_UNIT_FILES := $(filter-out $(addprefix $(VM_BUILD)/,$(addsuffix .unit,$(VMF_REAL))),$(VM_UNIT_FILES)) $(VM_BUILD)/fmk.unit $(VM_BUILD)/fm.unit $(addprefix $(VMF_BUILD)/,$(addsuffix .unit,$(VMF_REAL)))
$(VM_BUILD)/fm.unit: planck/vm/fm.native $(VM_BUILD)/fmk.unit $(VASMC_SOURCES) planck0
	./planck0 planck/vasmc.pk unit $< $@ x86_64 $(VM_BUILD)/fmk.unit
$(VMF_BUILD):
	mkdir -p $@
$(VMF_BUILD)/flt.unit: planck/vm/flt.native $(VM_BUILD)/fm.unit $(VM_BUILD)/mem.unit $(VM_BUILD)/big.unit $(VASMC_SOURCES) planck0 | $(VMF_BUILD)
	./planck0 planck/vasmc.pk unit $< $@ x86_64 $(VM_BUILD)/fm.unit $(VM_BUILD)/mem.unit $(VM_BUILD)/big.unit
$(VMF_BUILD)/dec.unit: planck/vm/dec.native $(VMF_BUILD)/flt.unit $(VM_BUILD)/mem.unit $(VASMC_SOURCES) planck0 | $(VMF_BUILD)
	./planck0 planck/vasmc.pk unit $< $@ x86_64 $(VM_BUILD)/mem.unit $(VMF_BUILD)/flt.unit
$(VMF_BUILD)/trans.unit: planck/vm/trans.native $(VMF_BUILD)/flt.unit $(VM_BUILD)/fm.unit $(VM_BUILD)/mem.unit $(VASMC_SOURCES) planck0 | $(VMF_BUILD)
	./planck0 planck/vasmc.pk unit $< $@ x86_64 $(VM_BUILD)/fm.unit $(VM_BUILD)/mem.unit $(VMF_BUILD)/flt.unit
tmp-vmf: $(VMF_UNIT_FILES) planck0
	./planck0 planck/vasmc.pk link $@ x86_64 $(VMF_UNIT_FILES)

test-vm: tmp-vmf
	@for f in $(VM_ALL_EXAMPLES); do \
		./planck0 $$f tmp-vmf.img || exit 1; \
		in=/dev/null; if [ -f $${f%.pk}.in ]; then in=$${f%.pk}.in; fi; \
		./tmp-vmf tmp-vmf.img < $$in > tmp-vmf.out || exit 1; \
		diff -u $${f%.pk}.out tmp-vmf.out || exit 1; \
		echo "$$f: ok"; \
	done; rm -f tmp-vmf.img tmp-vmf.out

test-vm0: tmp-vm
	@for f in $(VM0_EXAMPLES); do \
		./planck0 $$f tmp-vm.img || exit 1; \
		in=/dev/null; if [ -f $${f%.pk}.in ]; then in=$${f%.pk}.in; fi; \
		./tmp-vm tmp-vm.img < $$in > tmp-vm.out || exit 1; \
		diff -u $${f%.pk}.out tmp-vm.out || exit 1; \
		echo "$$f: ok"; \
	done; rm -f tmp-vm.img tmp-vm.out

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
	$(MAKE) test-vm0
	$(MAKE) test-vm
	$(MAKE) test-fm
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
