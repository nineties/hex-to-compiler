# wasm.mk: makes docs/assets/vm.wasm, the PlanckVM for the wasm32 target, and docs/assets/planck.img, the image that it runs in the browser.
# usage (in the root of the repository, after make planck0 and make tmp-vm-build/fmk.native):
#   make -f docs/tools/wasm.mk
# The units of planck/vm are compiled for the word of 4 bytes. The coefficients of fm are those that VM0 made (tmp-vm-build/fmk.native).
B := tmp-wasm-build
UNITS := mem gc udiv big flt dec num obj bit trans nb arr eq builtin hsh ctl ops vm
VASMC_SOURCES := planck/vasmc.pk planck/init.pk planck/peg.pk planck/syntax.pk $(shell find planck/lib -name "*.pk")
imports = $(addprefix $(B)/,$(addsuffix .unit,$(shell sed -n 's/^import \([A-Za-z0-9_]*\).*/\1/p' planck/vm/$(1).native)))
define RULE
$(B)/$(1).unit: planck/vm/$(1).native $(call imports,$(1)) $(VASMC_SOURCES) planck0 | $(B)
	./planck0 planck/vasmc.pk unit $$< $$@ wasm32 $(call imports,$(1))
endef
$(foreach u,$(UNITS) fm,$(eval $(call RULE,$(u))))

all: docs/assets/vm.wasm docs/assets/planck.img

$(B):
	mkdir -p $(B)
$(B)/fmk.unit: tmp-vm-build/fmk.native $(VASMC_SOURCES) planck0 | $(B)
	./planck0 planck/vasmc.pk unit $< $@ wasm32
docs/assets/vm.wasm: $(addprefix $(B)/,$(addsuffix .unit,fmk fm $(UNITS)))
	./planck0 planck/vasmc.pk link $@ wasm32 $^
docs/assets/planck.img: docs/tools/playground.pk docs/tools/playground_run.pk $(VASMC_SOURCES) planck/lib/pvm/boot.pk planck0
	./planck0 docs/tools/playground.pk $@
