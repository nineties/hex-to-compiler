; plisp2 -- simple lisp interpreter
; Copyright (C) 2026 nineties

(include "std.sv")

; === Memory Allocation

(fun align (n) ; align n to 4-byte boundary
    (return (& (+ n 7) 0xfffffff8))
    )

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
    (= heap_pos (align addr))
    (= heap_end (+ addr HEAP_BLOCK_SIZE))
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

; === Nodes

(fun box (val)
    (var cell (allocate 4))
    (set cell 0 val)
    (return cell)
    )
(fun unbox (cell) (get cell))

; last 3 bit of address is used to detect node types
; 000  : int
; 001  : nil
; 010  : cons
; 011  : symbol
; 100  : str
; 101  : lambda
; 110  : macro
; 111  : prim

(def Nint 0)
(def Nnil 1)
(def Ncons 2)
(def Nsymbol 3)
(def Nstr 4)
(def Nlambda 5)
(def Nmacro 6)
(def Nprim 7)

(def nil 1)

(fun node_type (node) (return (& 0x7 node)))
(fun make_int (n) (return (<< n 3)))
(fun make_cons (a b)
    (var cons (allocate 8))
    (set cons 0 a)
    (set cons 1 b)
    (return (| cons Ncons))
    )
(fun make_symbol (s)
    (var sym (allocate 4))
    (set sym 0 s)
    (return (| sym Nsymbol))
    )
(fun make_str (s)
    (var str (allocate 4))
    (set str 0 s)
    (return (| str Nstr))
    )
(fun make_lambda (env params body)
    (var lam (allocate 12))
    (set lam 0 env)
    (set lam 1 params)
    (set lam 2 body)
    (return (| lam Nlambda))
    )
(fun make_macro (env params body)
    (var mac (allocate 12))
    (set mac 0 env)
    (set mac 1 params)
    (set mac 2 body)
    (return (| mac Nmacro))
    )
(fun make_prim (name fun)
    (var prim (allocate 8))
    (set prim 0 name)
    (set prim 1 fun)
    (return (| prim Nprim))
    )

; === Parser

(fun read_file (path)
    (var fd (open path O_RDONLY))
    (if (< fd 0) (do
        (eputs "open failed: ") (eputs path) (eputs "\n")
        (exit 1)))

    (var file_size (fsize fd))
    (if (< file_size 0) (do
        (eputs "fstat failed: ") (eputs path) (eputs "\n")
        (exit 1)))

    (var buf (allocate (+ file_size 1))) ; +1 for \0
    (var r (read fd buf file_size))
    (if (< r file_size) (do
        (eputs "read failed: ") (eputs path) (eputs "\n")
        (exit 1)
        ))
    (setb buf file_size 0)

    (close fd)
    (return buf)
    )

(fun is_blank (c)
    (if (|| (== c (char " ")) (|| (== c (char "\t")) (== c (char "\n")))) (return 1) (return 0))
    )

(fun skip_spaces_and_commets (text_buf)
    (var addr (unbox text_buf))
    (while (getb addr) (do
        (var c (getb addr))
        (if (== c (char ";"))
            (while (&& (!= (getb addr) (char "\n")) (getb addr)) (+= addr 1))
        (if (is_blank c)
            (+= addr 1)
            (do
                (set text_buf addr)
                (return)
            )))
        ))
    (set text_buf addr)
    )

(fun read_sexp_list (path)
    (var text_buf (box (read_file path)))
    (skip_spaces_and_commets text_buf)
    (if (== (getb (unbox text_buf)) 0) (return nil))
    (puts (unbox text_buf))
    )

(fun main (argc argv)
    (if (<= argc 1) (do
        (puts "no input file")
        (exit 1)
        ))

    (init_heap)

    (var sexp_list (read_sexp_list (get argv 1)))
    )
