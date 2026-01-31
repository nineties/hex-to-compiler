; planck0 - An interpreted language dedicated to symbolic and language processing.
; Copyright (C) 2026 nineties

; planck0 serves as a minimal but fully functional subset of Planck,
; acting as an interim implementation for bootstrapping.

(include "std.sv")

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
; prim    |  arity |    0 |  110 |
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

(fun get_header_arg (node)
    (var header (get node 0))
    (return (>> header 4))
    )

(fun int_to_fixnum (n)
    (return (| (<< n 1) 1))
    )
(fun fixnum_to_int (n)
    (return (>> n 1))
    )

(fun make_string (text)
    (var data (allocate 8))
    (set data 0 (| (<< (strlen text) 4) StructT))
    (set data 1 text)
    (return data)
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
(fun make_symbol (text)
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
    (var hash 5381)
    (var arity (get_header_arg mexpr))
    (var i 1)
    (var e (+ arity 2))
    (while (< i e) (do
        ; use address of args as the hash value
        ; since header symbol and mexpr arguments
        ; are globally unique and their address are also unique.
        (= hash (+ (+ (<< hash 5) hash) (get mexpr i)))
        (+= i 1)
        ))
    (return hash)
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
(long Seval)

(fun init_symbols ()
    (= Sparse (make_symbol "parse"))
    (= Seval  (make_symbol "eval"))
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


(fun interpret (path)
    (var text (read_file path))
    (value_of Sparse)

    (puts text)
    )

(fun main (argc argv)
    ;(if (<= argc 1) (do
    ;    (puts "no input file")
    ;    (exit 1)
    ;    ))

    (init_heap)
    (init_tables)
    (init_symbols)

    (interpret "planck/init.pk")
    )
