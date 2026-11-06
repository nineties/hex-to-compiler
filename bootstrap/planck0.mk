planck0: bootstrap/planck0.sv plisp2 $(SASM_LIBS)
	-time ./plisp2 bootstrap/sasm.lisp $< $@
	-chmod +x $@
