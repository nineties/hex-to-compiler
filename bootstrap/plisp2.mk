ASM_SOURCES := bootstrap/asm.lisp $(wildcard bootstrap/asm/*.lisp)
ASM_EXAMPLE_SOURCES := $(wildcard bootstrap/asm/examples/*.s)
ASM_EXAMPLE_TARGETS := $(ASM_EXAMPLE_SOURCES:%.s=%)

SASM_SOURCES := bootstrap/sasm.lisp $(wildcard bootstrap/sasm/*.lisp)
SASM_LIBS	:= $(wildcard bootstrap/sasm/lib/*.sv)
SASM_EXAMPLE_SOURCES := $(wildcard bootstrap/sasm/examples/*.sv)
SASM_EXAMPLE_TARGETS := $(SASM_EXAMPLE_SOURCES:%.sv=%)

PLISP := $(shell if [ -f ./plisp2 ]; then echo "./plisp2"; else echo "./pforth < bootstrap/plisp.fs"; fi)

pforth: bootstrap/pforth.xxd
	xxd -r -c 8 $< > pforth
	chmod +x pforth

plisp2: bootstrap/plisp2.sv pforth bootstrap/plisp.fs $(SASM_SOURCES) $(ASM_SOURCES) $(SASM_LIBS)
	-time $(PLISP) bootstrap/sasm.lisp $< tmp-$@
	-mv tmp-$@ $@
	-chmod +x $@

%: %.s pforth bootstrap/plisp.fs $(ASM_SOURCES)
	-time $(PLISP) bootstrap/asm.lisp $< $@
	-chmod +x $@

%: %.sv pforth bootstrap/plisp.fs $(SASM_SOURCES) $(ASM_SOURCES) $(SASM_LIBS)
	-time $(PLISP) bootstrap/sasm.lisp $< tmp-$@
	-mv tmp-$@ $@
	-chmod +x $@
