; plisp2 -- simple lisp interpreter
; Copyright (C) 2026 nineties

(include "std.sv")

(def HEAP_BLOCK_SIZE (* 128 (* 1024 1024))) ; 128 MB
(long heap_block)
(fun init_heap ()
    (var addr (mmap2 0 HEAP_BLOCK_SIZE
        (| PROT_READ PROT_WRITE)
        (| MAP_PRIVATE MAP_ANONYMOUS)
        -1 0))
    (fprint_int STDOUT addr 16)
    )

(fun read_file (path)
    (var fd (open path O_RDONLY))
    (if (< fd 0) (do
        (eputs "open failed: ") (eputs path) (eputs "\n")
        (exit 1)))

    (var file_size (fsize fd))
    (if (< file_size 0) (do
        (eputs "fstat failed: ") (eputs path) (eputs "\n")
        (exit 1)))

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

    (init_heap)


    (var sexp_list (read_sexp_list (get argv 1)))
    )
