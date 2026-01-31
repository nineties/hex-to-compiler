; planck0 - An interpreted language dedicated to symbolic and language processing.
; Copyright (C) 2026 nineties

; planck0 serves as a minimal but fully functional subset of Planck,
; acting as an interim implementation for bootstrapping.

(include "std.sv")

; === Utilities

(fun not_implemented (name)
    (eputs "not implemented: ")
    (eputs name)
    (eputs "\n")
    (exit 1)
    )

(fun not_reachable (name)
    (eputs "not reachable: ")
    (eputs name)
    (eputs "\n")
    (exit 1)
    )

; === Memory Allocation

(fun align (n) ; align n to 4-byte boundary
    (return (& (+ n 7) 0xfffffff8))
    )

(long heap_root)
(long heap_end)
(long heap_pos)
(fun init_heap ()
    (var heap_block_size 1024)  ; 1GB
    (*= heap_block_size 1024)
    (*= heap_block_size 1024)
    (var addr (mmap2 0 heap_block_size
        (| PROT_READ PROT_WRITE)
        (| MAP_PRIVATE MAP_ANONYMOUS)
        -1 0))
    (if (u>= addr 0xfffff001) (do
        (eputs "mmap2 failed\n")
        (exit 1)
        ))
    (= heap_root addr)
    (= heap_pos (align addr))
    (= heap_end (+ addr heap_block_size))
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

; === data layout (planck0)
; Small integers: tagged with LSB=1. Value is encoded as (val << 1) | 1.
; Heap objects: Identified by LSB=0.
; These objects are preceded by a one-word header containing object-specific information.

; Layout of heap objects.
; m: 1 if the object is mutable
; 
;         |        header        |
;         |  28bit | 1bit | 3bit |
; symbol  |        |    0 |  000 | text |
; string  | length |    0 |  001 | text |
; mexpr   |  arity |    0 |  010 | sym  | mexpr1 | ... |
; array   | length |    m |  011 |
; struct  |        |    m |  100 |
; closure |        |    0 |  101 |
; prim    |  arity |    0 |  110 | ptr | pat1 | pat2 | ... |
;
;
; Both symbols and mexpr undergo interning,
; enabling O(1) equality testing via pointer comparison.

; === Hash table
; entry:
;     key: data
;     val: data
;     next: entry
;
; table:
;     size      (exponent of 2)
;     hash-func
;     equal-func
;     buckets[]

(fun make_table (size hash equal)
    (var table (allocate (* 4 (+ 3 size))))
    (var i 0)
    (set table 0 size)
    (set table 1 hash)
    (set table 2 equal)
    (while (< i size) (do
        (set table (+ 3 i) 0)
        (+= i 1)
        ))
    (return table)
    )

(fun table_lookup (table key) 
    (var size (get table 0))
    (var hash_func (get table 1))
    (var equal_func (get table 2))

    (var idx (& (hash_func key) (- size 1)))
    (var ent (get table (+ 3 idx)))
    (while ent (do
        (if (equal_func (get ent 0) key) (return (get ent 1)))
        (= ent (get ent 2))
        ))
    (return 0)
    )

(fun table_insert (table key val)
    (var size (get table 0))
    (var hash_func (get table 1))
    (var equal_func (get table 2))

    (var idx (& (hash_func key) (- size 1)))
    (var ent (get table (+ 3 idx)))
    (var new_ent (allocate (* 4 3)))
    (set new_ent 0 key)
    (set new_ent 1 val)
    (set new_ent 2 ent)
    (set table (+ 3 idx) new_ent)
    )

(fun table_update (table key val)
    (var size (get table 0))
    (var hash_func (get table 1))
    (var equal_func (get table 2))

    (var idx (& (hash_func key) (- size 1)))
    (var ent (get table (+ 3 idx)))
    (while ent (do
        (if (equal_func (get ent 0) key) (do
            (set ent 1 val)
            (return 1)
            ))
        (= ent (get ent 2))
        ))
    (return 0)
    )

; === Nodes
(def SymbolT  0x0)
(def StringT  0x1)
(def MexprT   0x2)
(def ArrayT   0x3)
(def StructT  0x4)
(def ClosureT 0x5)
(def PrimT    0x6)
(def IntT     0x8)

(fun print_tag (chan tag)
    (if (== tag SymbolT) (fputs chan "Symbol")
    (if (== tag StringT) (fputs chan "String")
    (if (== tag MexprT) (fputs chan "Mexpr")
    (if (== tag ArrayT) (fputs chan "Array")
    (if (== tag StructT) (fputs chan "Struct")
    (if (== tag ClosureT) (fputs chan "Closure")
    (if (== tag IntT) (fputs chan "Int")
        )))))))
    )


(fun expect (val tag)
    (if (!= (gettag val) tag) (do
        (print_tag STDERR tag)
        (eputs " is expected\n")
        (exit 1)
        ))
    )

(fun gettag (node)
    (if (& node 1)
        (return IntT)
        (& (get node 0) 0x7)
        )
    )

(fun is_fixnum (n)
    (return (& n 1))
    )

(fun is_mutable (node)
    (if (is_fixnum node) (return 1))
    (var header (get node 0))
    (return (& 0x8 header))
    )

(fun make_header (tag mutable arg)
    (return (| tag (| (<< mutable 3) (<< arg 4))))
    )

(fun get_header_arg (node)
    (var header (get node 0))
    (return (>> header 4))
    )

(fun fixnum (n)
    (return (| (<< n 1) 1))
    )
(fun fixnum_to_int (n)
    (return (>> n 1))
    )

(fun str (text)
    (var data (allocate 8))
    (set data 0 (| (<< (strlen text) 4) StringT))
    (set data 1 text)
    (return data)
    )

(fun str_text (str)
    (expect str StringT)
    (return (get str 1))
    )

(fun strhash (str)
    (var hash 5381)
    (while (getb str) (do
        (= hash (+ (+ (<< hash 5) hash) (getb str)))
        (+= str 1)
        ))
    (return hash)
    )

(long symtable)     ; table for interning symbols
(fun sym (text)
    (var data (table_lookup symtable text))
    (if data (return data))
    (var sym (allocate 8))
    (set sym 0 0)
    (set sym 1 text)
    (table_insert symtable text sym)
    (return sym)
    )

(fun sym_name (sym)
    (expect sym SymbolT)
    (return (get sym 1))
    )

(fun symhash (sym)
    (var hash (>> sym 3)) ; last 3 bit is always zero
    (return (* hash 536870909))
    )

(fun symeq (sym1 sym2)
    (if (== sym1 sym2) (return 1) (return 0))
    )

(fun mexprhash (mexpr)
    (var h 5381)
    (var arity (get_header_arg mexpr))
    (var i 1)
    (var e (+ arity 2))
    (while (< i e) (do
        (= h (+ (+ (<< h 5) h) (hash (get mexpr i))))
        (+= i 1)
        ))
    (return h)
    )

(fun mexpreq (mexpr1 mexpr2)
    (var arity1 (get_header_arg mexpr1))
    (var arity2 (get_header_arg mexpr2))
    (if (!= arity1 arity2) (return 0))
    (var i 1)
    (var e (+ arity1 2))
    (while (< i e) (do
        (if (!= (get mexpr1 i) (get mexpr2 i)) (return 0))
        (+= i 1)
        ))
    (return 1)
    )

(long mexprtable)   ; table for interning mexprs

(fun mexpr1 (h a)
    (char[] 12 tmpobj)  ; 3 word temporary objet
    (set tmpobj 0 (make_header MexprT 0 1))
    (set tmpobj 1 h)
    (set tmpobj 2 a)
    (var obj (table_lookup mexprtable tmpobj))
    (if obj (return obj))
    (= obj (allocate 12))
    (set obj 0 (make_header MexprT 0 1))
    (set obj 1 h)
    (set obj 2 a)
    (table_insert mexprtable obj obj)
    (return obj)
    )

(fun mexpr2 (h a b)
    (char[] 16 tmpobj)  ; 4 word temporary objet
    (set tmpobj 0 (make_header MexprT 0 2))
    (set tmpobj 1 h)
    (set tmpobj 2 a)
    (set tmpobj 3 b)
    (var obj (table_lookup mexprtable tmpobj))
    (if obj (return obj))
    (= obj (allocate 16))
    (set obj 0 (make_header MexprT 0 2))
    (set obj 1 h)
    (set obj 2 a)
    (set obj 3 b)
    (table_insert mexprtable obj obj)
    (return obj)
    )

(fun mexpr3 (h a b c)
    (char[] 20 tmpobj)  ; 5 word temporary objet
    (set tmpobj 0 (make_header MexprT 0 3))
    (set tmpobj 1 h)
    (set tmpobj 2 a)
    (set tmpobj 3 b)
    (set tmpobj 4 c)
    (var obj (table_lookup mexprtable tmpobj))
    (if obj (return obj))
    (= obj (allocate 20))
    (set obj 0 (make_header MexprT 0 3))
    (set obj 1 h)
    (set obj 2 a)
    (set obj 3 b)
    (set obj 4 c)
    (table_insert mexprtable obj obj)
    (return obj)
    )

(fun hash (e)
    (var t (gettag e))
    (if (== t IntT) (return (* (fixnum_to_int e) 536870909))
    (if (== t SymbolT) (return (symhash e))
    (if (== t StringT) (return (strhash (str_text e)))
    (if (== t MexprT) (return (mexprhash e))
        (not_implemented "print")
        ))))
    )

(long global_env)   ; variable table (sym -> data)

(fun make_env (size parent)
    (var table (make_table size symhash symeq))
    (var env (allocate 8))
    (set env 0 table)
    (set env 1 parent)
    (return env)
    )

(fun env_lookup (env sym)
    (var table (get env 0))
    (var data 0)
    (while table (do
        (= data (table_lookup table sym))
        (if data (return data))
        (= table (get env 1))
        ))
    (return 0)
    )

(fun env_insert (env sym val)
    (table_insert (get env 0) sym val)
    )

(fun env_update (env sym val)
    (var table (get env 0))
    (while table (do
        (if (table_update table sym val) (return 1))
        (= table (get env 1))
        ))
    (return 0)
    )

(fun value_of (sym)
    (var data (env_lookup global_env sym))
    (if (! data) (do
        (eputs "undefined variable: ")
        (eputs (sym_name sym))
        (eputs "\n")
        (exit 1)
        ))
    )

(fun init_tables ()
    (= symtable (make_table 0x10000 strhash streq))
    (= mexprtable (make_table 0x10000 mexprhash mexpreq))
    (= global_env (make_env 0x1000 0))
    )

(long Sparse)
(long Sprint)
(long Seval)
(long S_)
(long S_TypeOf)
(long Sint)
(long Sstring)

(fun init_symbols ()
    (= Sparse   (sym "parse"))
    (= Sprint   (sym "print"))
    (= Seval    (sym "eval"))
    (= S_       (sym "_"))
    (= S_TypeOf (sym "_TypeOf"))
    (= Sint     (sym "int"))
    (= Sstring  (sym "string"))
    )

; === Pattern Matching
; a pattern is represetned by mexprs.
; wildcard pattern (_): matches any value without binding.
; variable pattern (x, y, ..): matches any value and binds it to the variable.
; literal pattern (23, "hello"): matches only if the value is equal to the constant.
; m-expr pattern (User{name, age}): matches an m-expr with the tag User and two fields
; type or pattern (_TypeOf{x, type}): matches any value with the type

; ==== Printing
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

(fun print_str (str)
    (var c 0)
    (var v 0)
    (= str (str_text str))
    (puts "\"")
    (while (getb str 0) (do
        (= c (getb str 0))
        (= v (escape_char c))
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

(fun print_mexpr (e)
    (var i 0)
    (var arity  (get_header_arg e))
    (print (get e 1))
    (puts "{")
    (while (< i arity) (do
        (print (get e (+ i 2)))
        (+= i 1)
        (if (< i arity) (puts ", "))
        ))
    (puts "}")
    )

(fun print (e)
    (var t (gettag e))
    (if (== t IntT) (puti (fixnum_to_int e))
    (if (== t SymbolT) (puts (sym_name e))
    (if (== t StringT) (print_str e)
    (if (== t MexprT) (print_mexpr e)
        (not_implemented "print")
        ))))
    )

(fun eval (e)
    (puts "eval\n")
    )

; === Primitive Functions
(fun allocate_prim (arity)
    (var prim (allocate (* 4 (+ 2 arity))))
    (set prim 0 (make_header PrimT 0 arity))
    (return prim)
    )

(fun add_prim1 (name p1 ptr)
    (var prim (allocate_prim 1))
    (set prim 1 ptr)
    (set prim 2 p1)
    (env_insert global_env name prim)
    )

(fun init_prims ()
    (add_prim1 Sprint (sym "e") print)
    (add_prim1 Seval (sym "e") eval)
    )

; === Interpreter

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


(fun interpret (path)
    (var text (read_file path))
    (print (fixnum 123)) (puts "\n")
    (print (sym "abc")) (puts "\n")
    (print (str "hello")) (puts "\n")
    (var e1 (mexpr2 (sym "Add") (fixnum 123) (fixnum 456)))
    )

(fun main (argc argv)
    ;(if (<= argc 1) (do
    ;    (puts "no input file")
    ;    (exit 1)
    ;    ))

    (init_heap)
    (init_tables)
    (init_symbols)
    (init_prims)

    (interpret "planck/init.pk")
    )
