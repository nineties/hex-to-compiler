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


# the libraries as M-expressions (planck/mkmx.pk), which planck/mx.pk loads at a small part of the memory of the text
MX_LIBS := base list vector table string error bitset bytes ir cfg sched regalloc obj elf link x86enc x86 wide vasm vasm_ir vasm_rt vasm_build
MX_FILES := $(MX_LIBS:%=planck/lib/%.mx)
VASM_EXAMPLES := $(wildcard planck/vasm/*.vasm)
BACKEND_EXAMPLES := $(wildcard planck/backend/*.pk)

.PHONY: mx
mx: $(MX_FILES)

planck/lib/%.mx: planck/lib/%.pk planck/mkmx.pk planck/mx.pk planck0
	./planck0 planck/mkmx.pk $< > $@.tmp
	mv $@.tmp $@

planck/lib/list.mx: planck/lib/base.mx
planck/lib/vector.mx: planck/lib/list.mx
planck/lib/table.mx: planck/lib/list.mx
planck/lib/string.mx: planck/lib/vector.mx
planck/lib/error.mx: planck/lib/string.mx
planck/lib/bitset.mx: planck/lib/list.mx
planck/lib/bytes.mx: planck/lib/vector.mx
planck/lib/ir.mx: planck/lib/table.mx planck/lib/string.mx
planck/lib/cfg.mx: planck/lib/bitset.mx planck/lib/ir.mx
planck/lib/sched.mx: planck/lib/cfg.mx
planck/lib/regalloc.mx: planck/lib/cfg.mx
planck/lib/obj.mx: planck/lib/table.mx planck/lib/bytes.mx
planck/lib/elf.mx: planck/lib/bytes.mx
planck/lib/link.mx: planck/lib/obj.mx planck/lib/elf.mx
planck/lib/x86enc.mx: planck/lib/obj.mx planck/lib/ir.mx
planck/lib/x86.mx: planck/lib/sched.mx planck/lib/regalloc.mx planck/lib/x86enc.mx
planck/lib/wide.mx: planck/lib/ir.mx
planck/lib/vasm.mx: planck/lib/table.mx planck/lib/error.mx
planck/lib/vasm_ir.mx: planck/lib/ir.mx
planck/lib/vasm_rt.mx: planck/lib/ir.mx planck/lib/vasm.mx planck/lib/bytes.mx
planck/lib/vasm_build.mx: planck/lib/x86.mx planck/lib/wide.mx planck/lib/link.mx planck/lib/vasm_ir.mx planck/lib/vasm_rt.mx

.PHONY: clean test test-planck0
clean:
	rm -f pforth
	rm -f plisp2
	rm -f planck0
	rm -f $(ASM_EXAMPLE_TARGETS)
	rm -f $(SASM_EXAMPLE_TARGETS)
	rm -f $(MX_FILES)

test: pforth plisp.fs $(shell find plisp -name "*.lisp")
	./pforth < plisp.fs plisp/test.lisp

# run tests of planck0 and compare outputs of examples with *.out
test-planck0: planck0 mx
	./planck0 planck/test.pk
	./planck0 planck/test_syntax.pk
	./planck0 planck/test_define.pk
	./planck0 planck/test_builtins.pk one "two words"
	./planck0 planck/test_lib.pk
	./planck0 planck/test_unparse.pk
	./planck0 planck/test_unparse_pattern.pk
	./planck0 planck/test_unparse_file.pk
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
