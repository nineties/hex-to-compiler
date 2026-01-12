; plisp2 -- simple lisp interpreter
; Copyright (C) 2026 nineties

(include "std.sv")

(def HEAP_BLOCK_SIZE (* 128 (* 1024 1024))) ; 128 MB
(long heap_root)
(long heap_end)
(long heap_pos)
(fun init_heap ()
    (var addr (mmap2 0 HEAP_BLOCK_SIZE
        (| PROT_READ PROT_WRITE)
        (| MAP_PRIVATE MAP_ANONYMOUS)
        -1 0))
    (if (u>= addr 0xfffff001) (do
        (eputs "mmap2 failed\n")
        (exit 1)
        ))
    (= heap_root addr)
    (= heap_pos addr)
    (= heap_end (+ addr HEAP_BLOCK_SIZE))
    )

(fun align (n) ; align n to 4-byte boundary
    (return (& (+ n 3) 0xfffffffe))
    )

(fun allocate (size)
    (= size (align size))
    (if (>= (+ heap_pos size) heap_end) (do
        (eputs "memory allocation error\n")
        (exit 1)
        ))
    (var addr heap_pos)
    (+= heap_pos size)
    (return addr)
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

    (var buf (allocate file_size))
    (var r (read fd buf file_size))
    (if (< r file_size) (do
        (eputs "read failed: ") (eputs path) (eputs "\n")
        (exit 1)
        ))

    (close fd)
    (return buf)
    )

(fun read_sexp_list (path)
    (var text (read_file path))
    (puts text)
    )

(fun main (argc argv)
    (if (<= argc 1) (do
        (puts "no input file")
        (exit 1)
        ))

    (init_heap)

    (var sexp_list (read_sexp_list (get argv 1)))
    )
