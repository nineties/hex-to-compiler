; plisp2 -- simple lisp interpreter
; Copyright (C) 2026 nineties

(include "std.sv")

(fun read_file (path)
    (var fd (open path O_RDONLY))
    (if (< fd 0) (do
        (eputs "open failed: ") (eputs path) (eputs "\n")
        (exit 1)))
    (fprint_int STDOUT (fsize fd) 10)
    (close fd)
    )

(fun read_sexp_list (path)
    (var text (read_file path))
    )

(fun main (argc argv)
    (if (<= argc 1) (do
        (puts "no input file")
        (exit 1)
        ))
    (var sexp_list (read_sexp_list (get argv 1)))
    )
