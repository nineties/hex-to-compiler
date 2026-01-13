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

; === Utilities

(fun strndup (from size)
    (var to (allocate (+ size 1)))
    (memcpy to from size)
    (setb to size 0)
    (return to)
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

(fun fprint_tag (fd tag)
    (if (== tag Nint) (fputs fd "int")
    (if (== tag Nnil) (fputs fd "()")
    (if (== tag Ncons) (fputs fd "cons")
    (if (== tag Nsymbol) (fputs fd "symbol")
    (if (== tag Nstr) (fputs fd "string")
    (if (== tag Nlambda) (fputs fd "lambda")
    (if (== tag Nmacro) (fputs fd "macro")
    (if (== tag Nprim) (fputs fd "prim")
        )))))))))

(fun print_tag (tag) (fprint_tag STDOUT tag))
(fun eprint_tag (tag) (fprint_tag STDERR tag))

(fun tag (node) (return (& 0x7 node)))
(fun untag (node) (return (& 0xfffffff8 node)))
(fun check_tag (t node)
    (if (!= t (tag node)) (do
        (eprint_tag t)
        (eputs " is expected\n")
        (exit 1)
        )))

(fun make_int (n) (return (<< n 3)))
(fun to_int (node)
    (check_tag Nint node)
    (return (asr node 3))
    )
(fun make_cons (a b)
    (var cons (allocate 8))
    (set cons 0 a)
    (set cons 1 b)
    (return (| cons Ncons))
    )
(fun cons_p (node) (if (== (tag node) Ncons) (return 1) (return 0)))
(fun car (node)
    (check_tag Ncons node)
    (return (get (untag node) 0))
    )
(fun cdr (node)
    (check_tag Ncons node)
    (return (get (untag node) 1))
    )
(fun make_symbol (s)
    (var sym (allocate 4))
    (set sym 0 s)
    (return (| sym Nsymbol))
    )
(fun symbol_name (s)
    (check_tag Nsymbol s)
    (return (get (untag s) 0))
    )
(fun make_str (s)
    (var str (allocate 4))
    (set str 0 s)
    (return (| str Nstr))
    )
(fun to_str (s)
    (check_tag Nstr s)
    (return (get (untag s) 0))
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

; === Parser and Printer

(fun print_str (str)
    (puts "\"")
    (while (getb str) (do
        (var c (getb str))
        (var v (escape_char c))
        (if (< v 0)
            (putc c)
            (do
                (puts "\\")
                (putc v)
            ))
        (+= str 1)
        ))
    (puts "\"")
    )

(fun print_sexp (sexp)
    (var t (tag sexp))
    (if (== t Nint) (puti (to_int sexp))
    (if (== t Nnil) (puts "()")
    (if (== t Ncons) (do
        (puts "(")
        (print_sexp (car sexp))
        (= sexp (cdr sexp))
        (while (!= sexp nil) (do
            (puts " ")
            (print_sexp (car sexp))
            (= sexp (cdr sexp))
            ))
        (puts ")")
        )
    (if (== t Nsymbol) (puts (symbol_name sexp))
    (if (== t Nstr) (print_str (to_str sexp))
    (if (== t Nlambda) (not_implemented "print_sexp:lambda")
    (if (== t Nmacro) (not_implemented "print_sexp:macro")
    (if (== t Nprim) (not_implemented "print_sexp:prim")
        ))))))))
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

(fun skip_spaces_and_commets (textbuf)
    (var addr (unbox textbuf))
    (while (getb addr) (do
        (var c (getb addr))
        (if (== c (char ";"))
            (while (&& (!= (getb addr) (char "\n")) (getb addr)) (+= addr 1))
        (if (is_blank c)
            (+= addr 1)
            (do
                (set textbuf addr)
                (return)
            )))
        ))
    (set textbuf addr)
    )

(fun nextchar (textbuf) (return (getb (unbox textbuf))))

(fun succ (textbuf n)
    (set textbuf (+ (unbox textbuf) n))
    (return textbuf)
    )

(fun is_atom_char (c)
    ; 0-9, a-z, A-Z, +-*/<>=?!_:$%&|^~@[]
    (if (== (getb "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxoxxoooxxxooxoxooooooooooooxooooooooooooooooooooooooooooooooxoooxooooooooooooooooooooooooooxoxoxx" c) (char "o"))
        (return 1)
        (return 0)
        )
    )

(fun parse_sym (textbuf)
    (var start (unbox textbuf))
    (while (is_atom_char (nextchar textbuf)) (succ textbuf 1))
    (var end (unbox textbuf))
    (var str (strndup start (- end start)))

    (var c (nextchar textbuf))
    (if (&& (! (is_blank c)) (!= c (char ")"))) (do
        (eputs "junk letters after symbol: ")
        (eputs str)
        (exit 1)
        ))
    (return (make_symbol str))
    )

(fun escape_char (c)
    (if (== c 0) (return (char "0"))
    (if (== c 7) (return (char "a"))
    (if (== c 8) (return (char "b"))
    (if (== c 9) (return (char "t"))
    (if (== c 10) (return (char "n"))
    (if (== c 11) (return (char "v"))
    (if (== c 12) (return (char "f"))
    (if (== c 13) (return (char "r"))
    (if (== c (char "\"")) (return (char "\""))
    (if (== c (char "'")) (return (char "'"))
    (if (== c (char "\\")) (return (char "\\"))
        (return -1)
        ))))))))))))

(fun unescape_char (c)
    (if (== c (char "0")) (return 0)
    (if (== c (char "a")) (return 7)
    (if (== c (char "b")) (return 8)
    (if (== c (char "t")) (return 9)
    (if (== c (char "n")) (return 10)
    (if (== c (char "v")) (return 11)
    (if (== c (char "f")) (return 12)
    (if (== c (char "r")) (return 13)
    (if (== c (char "\"")) (return (char "\"")))
    (if (== c (char "'")) (return (char "'")))
    (if (== c (char "\\")) (return (char "\\"))
        (do
            (eputs "invalid escaped character\n")
            (exit 1)
        )))))))))))

(char[] 4096 parse_str_buf)
(fun parse_str (textbuf)
    (var end parse_str_buf)
    (succ textbuf 1) ; skip '"'
    (while (nextchar textbuf) (do
        (var c (nextchar textbuf))
        (if (== c (char "\"")) (do
            (succ textbuf 1)
            (return (make_str (strndup parse_str_buf (- end parse_str_buf))))
            )
        (if (== c (char "\\")) (do
            (succ textbuf 1)
            (setb end (unescape_char (nextchar textbuf)))
            (+= end 1)
            (succ textbuf 1)
            )
        (do
            (setb end c)
            (+= end 1)
            (succ textbuf 1)
            )))
        ))
    (eputs "invalid string literal: ")
    (eputs parse_str_buf)
    (eputs "\n")
    (exit 1)
    )

(fun parse_atom (textbuf)
    (var c (nextchar textbuf))
    (if (== c (char "\""))
        (return (parse_str textbuf))
    (if (is_atom_char c)
        (return (parse_sym textbuf))
        (not_implemented "parse_atom")
        ))
    )

(fun parse_sexp_list (textbuf)
    (skip_spaces_and_commets textbuf)
    (var c (nextchar textbuf))
    (if (== c 0) (do
        (eputs "syntax error")
        (exit 1)
        ))
    (if (== c (char ")")) (do
        (succ textbuf 1)
        (return nil)
        ))
    (var sexp (parse_sexp textbuf))
    (var list (parse_sexp_list textbuf))
    (return (make_cons sexp list))
    )

(fun parse_sexp (textbuf)
    (var addr (unbox textbuf))
    (var c (getb addr))
    (if (== c (char "("))
        (return (parse_sexp_list (succ textbuf 1)))
        (return (parse_atom textbuf))
        )
    )

(fun read_sexp_list (path)
    (var textbuf (box (read_file path)))
    (skip_spaces_and_commets textbuf)
    (if (== (getb (unbox textbuf)) 0) (return nil))
    (var sexp (parse_sexp textbuf))
    (print_sexp sexp)
    )

(fun main (argc argv)
    (if (<= argc 1) (do
        (puts "no input file")
        (exit 1)
        ))

    (init_heap)

    (var sexp_list (read_sexp_list (get argv 1)))
    )
