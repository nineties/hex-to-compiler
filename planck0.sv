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

(fun strndup (from size)
    (var to (allocate (+ size 1)))
    (memcpy to from size)
    (setb to size 0)
    (return to)
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
; mexpr   |  arity |    0 |  010 | sym  | arg1 | .... |
; array   | length |    m |  011 |
; struct  |        |    m |  100 |
; closure |  arity |    0 |  101 | pat1 | pat2 | ...  |  env | body |
; prim    |  arity |    0 |  110 | pat1 | pat2 | .... |  ptr |
; union   |        |    0 |  111 | fun1 | fun2 |
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
(def UnionT   0x7)
(def IntT     0x8)

(fun fprint_tag (chan tag)
    (if (== tag SymbolT) (fputs chan "Symbol")
    (if (== tag StringT) (fputs chan "String")
    (if (== tag MexprT) (fputs chan "Mexpr")
    (if (== tag ArrayT) (fputs chan "Array")
    (if (== tag StructT) (fputs chan "Struct")
    (if (== tag ClosureT) (fputs chan "Closure")
    (if (== tag IntT) (fputs chan "Int")
        )))))))
    )


(fun expect (tag val)
    (if (!= (gettag val) tag) (do
        (fprint_tag STDERR tag)
        (eputs " is expected\n")
        (exit 1)
        ))
    )

(fun expect_mexpr (head val)
    (expect MexprT val)
    (if (!= (get val 1) head) (do
        (eputs "M-expr with head ")
        (eprint head)
        (eputs " is expected\n")
        (exit 1)
        ))
    )

(fun has_head (head expr)
    (if (&& (== (gettag expr) MexprT)
            (== (get 1 expr) head))
        (return 1)
        (return 0)
    ))

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
    (expect StringT str)
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
    (expect SymbolT sym)
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

(fun mexpr (expr)
    (var obj (table_lookup mexprtable expr))
    (if obj (return obj))
    (var arity (get_header_arg expr))
    (= obj (allocate (* 4 (+ 2 arity))))
    (memcpy obj expr (* 4 (+ 2 arity)))
    (table_insert mexprtable obj obj)
    (return obj)
    )

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

; === builtin symbols

(long parseS)
(long printS)
(long evalS)
(long _S)
(long _TypeOfS)
(long intS)
(long stringS)
(long DefS)
(long SetS)
(long CallS)
(long DoS)
(long IfS)
(long WhileS)
(long LambdaS)
(long UnionS)
(long QuoteS)
(long QuasiQuoteS)
(long UnQuoteS)
(long HandleS)
(long PerformS)
(long TupleS)
(long SyntaxErrorS)
(long noneS)

(fun tup2 (a b) (return (mexpr2 TupleS a b)))
(fun tup3 (a b c) (return (mexpr3 TupleS a b c)))

(fun tup_get (e i)
    (expect_mexpr TupleS e)
    (var arity (get_header_arg e))
    (if (>= i arity) (not_reachable "tup_get"))
    (return (get e (+ 2 i)))
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

(fun value_of (env sym)
    (var data (env_lookup env sym))
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

(fun init_symbols ()
    (= parseS   (sym "parse"))
    (= printS   (sym "print"))
    (= evalS    (sym "eval"))
    (= _S       (sym "_"))
    (= _TypeOfS (sym "_TypeOf"))
    (= intS     (sym "int"))
    (= stringS  (sym "string"))
    (= DefS         (sym "Def"))
    (= SetS         (sym "Set"))
    (= CallS        (sym "Call"))
    (= DoS          (sym "Do"))
    (= IfS          (sym "If"))
    (= WhileS       (sym "While"))
    (= LambdaS      (sym "Lambda"))
    (= UnionS       (sym "Union"))
    (= QuoteS       (sym "Quote"))
    (= QuasiQuoteS  (sym "QuasiQuote"))
    (= UnQuoteS     (sym "UnQuote"))
    (= HandleS      (sym "Handle"))
    (= PerformS     (sym "Perform"))
    (= TupleS       (sym "Tuple"))
    (= SyntaxErrorS (sym "SyntaxError"))
    (= noneS        (sym "none"))
    )

; === Pattern Matching
; a pattern is represetned by mexprs.
; wildcard pattern (_): matches any value without binding.
; variable pattern (x, y, ..): matches any value and binds it to the variable.
; literal pattern (23, "hello"): matches only if the value is equal to the constant.
; m-expr pattern (User{name, age}): matches an m-expr with the tag User and two fields
; type or pattern (_TypeOf{x, type}): matches any value with the type

(fun match_arg (binds offs pat arg)
    (var i 0)
    (var t (gettag pat))
    (if (== pat _S) (do ; wildcard
        (return 1)
        )
    (if (== t SymbolT) (do ; variable pattern
        (= i (get offs))
        (set binds (* 2 i) pat)
        (set binds (+ (* 2 i) 1) arg)
        (set offs (+ i 1))
        (return 1)
        )))
    (not_implemented "match_arg")
    )

(fun match (matched_fn binds fn e args)
    (var t (gettag fn))
    (var arity 0)
    (var i 0)
    (var nbinds 0)
    (char[] 4 offs)
    (if (== t UnionT)
        (if (match matched_fn binds (get fn 1) e args)
            (return 1)
            (return (match matched_fn binds (get fn 2) e args))
            )
    (if (&& (!= t PrimT) (!= t ClosureT)) (do
        (eputs "not a function: ")
        (eprint fn)
        (exit 1)
        )))

    (= arity (get_header_arg fn))
    (if (!= (+ arity 1) (get_header_arg e)) (return 0))
    (set offs 0 0)
    (while (< i arity) (do
        (if (! (match_arg binds offs (get fn (+ 1 i)) (get args i)))
            (return 0)
            )
        (+= i 1)
        ))
    ; Found the function!
    (set matched_fn fn)
    (set binds (* 2 (get offs)) 0)
    (return 1)
    )

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
        (return -1)
        ))))))))))

(fun fprint_str (chan str)
    (var c 0)
    (var v 0)
    (= str (str_text str))
    (fputs chan "\"")
    (while (getb str 0) (do
        (= c (getb str 0))
        (= v (escape_char c))
        (if (< v 0)
            (fputc chan c)
            (do
                (fputs chan "\\")
                (fputc chan v)
            ))
        (+= str 1)
        ))
    (fputs chan "\"")
    )

(fun fprint_mexpr (chan e)
    (var i 0)
    (var arity  (get_header_arg e))
    (fprint chan (get e 1))
    (fputs chan "{")
    (while (< i arity) (do
        (fprint chan (get e (+ i 2)))
        (+= i 1)
        (if (< i arity) (fputs chan ", "))
        ))
    (fputs chan "}")
    )

(fun fprint_prim (chan e)
    (expect PrimT e)
    (var i 0)
    (var arity (get_header_arg e))
    (fputs chan "(")
    (while (< i arity) (do
        (fprint chan (get e (+ 1 i)))
        (+= i 1)
        (if (< i arity) (fputs chan ", "))
        ))
    (fputs chan ") -> ...")
    )

(fun fprint (chan e)
    (var t (gettag e))
    (if (== t IntT) (fputi chan (fixnum_to_int e))
    (if (== t SymbolT) (fputs chan (sym_name e))
    (if (== t StringT) (fprint_str chan e)
    (if (== t MexprT) (fprint_mexpr chan e)
    (if (== t PrimT) (fprint_prim chan e)
        (not_implemented "print")
        )))))
    )
(fun print (e)
    (fprint STDOUT e)
    (return noneS)
    )
(fun eprint (e) (fprint STDERR e))

; === Parsing
(fun syntax_error (msg)
    (return (mexpr1 SyntaxErrorS (str msg)))
    )

(fun is_blank (c)
    (if (|| (== c (char " "))
        (|| (== c (char "\t"))
            (== c (char "\n"))))
        (return 1)
        (return 0)
        )
    )

(fun skip_spaces (raw)
    (var c 0)
    (while (getb raw) (do
        (= c (getb raw))
        (if (! (is_blank c)) (return raw))
        (+= raw 1)
        ))
    (return raw)
    )

(fun is_symbol_leading_char (c)
    ; a-z, A-Z, _
    (if (== (getb "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxooooooooooooooooooooooooooxxxxoxooooooooooooooooooooooooooxxxxx" c) (char "o"))
        (return 1)
        (return 0)
        )
    )

(fun is_symbol_following_char (c)
    ; a-z, A-Z, 0-9, _
    (if (== (getb "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxooooooooooxxxxxxxooooooooooooooooooooooooooxxxxoxooooooooooooooooooooooooooxxxxx" c) (char "o"))
        (return 1)
        (return 0)
        )
    )

(fun parse_symbol (ret raw)
    (var start raw)
    (+= raw 1)
    (while (is_symbol_following_char (getb raw)) (+= raw 1))
    (var name (strndup start (- raw start)))
    (set ret (sym name))
    (return raw)
    )

(fun parse_int (ret raw)
    (var value 0)
    (var d 0)
    (var base 10)
    (var c 0)
    (if (== (getb raw) (char "0")) (do
        (+= raw 1)
        (= base 8)
        (= c (getb raw))
        (if (|| (== c (char "x")) (== c (char "X"))) (do
            (+= raw 1)
            (= base 16)
            )
        (if (|| (== c (char "o")) (== c (char "O"))) (do
            (+= raw 1)
            (= base 8)
            )
        (if (|| (== c (char "b")) (== c (char "B"))) (do
            (+= raw 1)
            (= base 2)
            ))))
        ))
    (while 1 (do
        (= c (getb raw))
        (if (&& (<= (char "0") c) (<= c (char "9")))
            (= d (- c (char "0")))
        (if (&& (<= (char "a") c) (<= c (char "f")))
            (= d (+ (- c (char "a")) 10))
        (if (&& (<= (char "A") c) (<= c (char "F")))
            (= d (+ (- c (char "A")) 10))
            (do
                (set ret (fixnum value))
                (return raw)
            ))))
        (if (>= d base) (do
            (set ret (syntax_error "malformed integer literal"))
            (return (+ raw 1))
            ))
        (= value (+ (* value base) d))
        (+= raw 1)
        ))
    (not_reachable "parse_int")
    )

(char[] 4096 parse_str_buf)
(fun parse_str (ret raw)
    (var end parse_str_buf)
    (var c 0)
    (+= raw 1)  ; skip '"'
    (while (getb raw) (do
        (= c (getb raw))
        (if (== c (char "\"")) (do
            (+= raw 1)
            (set ret (str (strndup parse_str_buf (- end parse_str_buf))))
            (return raw)
            )
        (if (== c (char "\\")) (do
            (+= raw 1)
            (= c (unescape_char (getb raw)))
            (if (< c 0) (do
                (set ret (syntax_error "invalid escaped character"))
                (return raw)
                ))
            (setb end c)
            (+= end 1)
            (+= raw 1)
            )
        (if (== c (char "\n")) (do
            (set ret (syntax_error "unterminated string literal"))
            (return (+ raw 1))
            )
        (do
            (setb end c)
            (+= end 1)
            (+= raw 1)
            ))))
        ))
    (set ret (syntax_error "unterminated string literal"))
    (return raw)
    )


(fun parse_mexpr (ret head raw)
    (char[] 4 tmp)
    (char[] 72 mexpr_buf) ; buffer for m-expr with 16 arity at maximum
    (var arity 0)
    (+= raw 1)  ; skip {

    (set mexpr_buf 1 head)

    (= raw (skip_spaces raw))
    (if (== (getb raw) (char "}")) (do
        (set mexpr_buf 0 (make_header MexprT 0 arity))
        (set ret (mexpr mexpr_buf))
        (return (+ raw 1))
        ))

    (= raw (parse_ tmp raw))
    (if (has_head SyntaxErrorS (get tmp)) (do
        (set ret (get tmp))
        (return raw)
        ))
    (set mexpr_buf (+ 2 arity) (get tmp))
    (+= arity 1)

    (while (== (getb raw) (char ",")) (do
        (+= raw 1) ; skip ,
        (= raw (parse_ tmp raw))
        (if (has_head SyntaxErrorS (get tmp)) (do
            (set ret (get tmp))
            (return raw)
            ))
        (set mexpr_buf (+ 2 arity) (get tmp))
        (+= arity 1)
        (if (>= arity 16) (do
            (set ret (syntax_error "too many arguments"))
            (return raw)
            ))
        ))
    (if (== (getb raw) (char "}")) (do
        (set mexpr_buf 0 (make_header MexprT 0 arity))
        (set ret (mexpr mexpr_buf))
        (return (+ raw 1))
        ))
    (set ret (syntax_error "unterminated M-expr"))
    (return raw)
    )

(fun parse_ (ret raw)
    (= raw (skip_spaces raw))

    (var c (getb raw 0))
    (if (is_symbol_leading_char c) (do
        ; symbol or m-expr
        (= raw (parse_symbol ret raw))
        (= raw (skip_spaces raw))
        (if (== (getb raw) (char "{"))
            (= raw (parse_mexpr ret (get ret) raw))
            )
        )
    (if (&& (<= (char "0") c) (<= c (char "9")))
        (= raw (parse_int ret raw))
    (if (== c (char "\""))
        (= raw (parse_str ret raw))
        (not_implemented "parse")
        )))
    (= raw (skip_spaces raw))
    (return raw)
    )

(fun parse (text)
    (char[] 4 ret)
    (expect StringT text)
    (var raw (get text 1))
    (= raw (parse_ ret raw))
    (return (tup2 (get ret) (str raw)))
    )

; === Evaluator

(fun eval_call (env e)
    (var arity (- (get_header_arg e) 1))
    (var fn 0)
    (var i 0)
    (char[] 4 matched_fn)

    ; local array for storing args and variable bindings
    ; from pattern matching. The size is sufficient
    ; for the temporary implementation of planck.
    (char[] 64 args)    ; 16 args
    (char[] 128 binds)  ; 16 binds

    (if (< arity 0) (do
        (eputs "malformed Call expr: ")
        (eprint e)
        (eputs "\n")
        (exit 1)
        ))
    (= fn (eval env (get e 2)))
    ; eval args
    (while (< i arity) (do
        (set args i (eval env (get e (+ 3 i))))
        (+= i 1)
        ))

    (if (! (match matched_fn binds fn e args)) (do
        (eputs "matching failed: ")
        (eprint e)
        (eputs "\n")
        (exit 1)
        ))

    (= fn (get matched_fn))
    (if (== (gettag fn) PrimT) (do
        (= fn (get fn (+ arity 1)))
        (if (== arity 0)
            (return (fn env))
        (if (== arity 1)
            (return (fn (get args 0) env))
        (if (== arity 2)
            (return (fn (get args 0) (get args 1) env))
        (if (== arity 3)
            (return (fn (get args 0) (get args 1) (get args 2) env))
            (not_implemented "call prim")
            ))))
        ))

    (not_implemented "eval_call")
    )

(fun eval_mexpr (env e)
    (puts "eval: ") (print e) (puts "\n")
    (var head (get e 1))
    (var arity (get_header_arg e))
    (var v 0)
    (if (== head CallS) (return (eval_call env e))
    (if (== head DefS) (do
        (if (|| (!= arity 2) (!= (gettag (get e 2)) SymbolT)) (do
            (eputs "malfoemd Def expr: ") (eprint e) (eputs "\n") (exit 1)
            ))
        (= v (eval env (get e 3)))
        (env_insert env (get e 2) v)
        (return v)
        )
    (if (== head SetS) (do
        (if (|| (!= arity 2) (!= (gettag (get e 2)) SymbolT)) (do
            (eputs "malfoemd Set expr: ") (eprint e) (eputs "\n") (exit 1)
            ))
        (= v (eval env (get e 3)))
        (if (! (env_update env (get e 2) v)) (do
            (eputs "undefined variable: ") (eprint (get e 2)) (eputs "\n") (exit 1)
            ))
        (return v)
        )
    (if (== head QuoteS) (do
        (if (!= arity 1) (do
            (eputs "malformed Quote expr: ") (eprint e) (eputs "\n") (exit 1)
            ))
        (return (get e 2))
        )
        ))))
    (not_implemented "eval_mexpr")
    )

(fun eval (env e)
    (var t (gettag e))
    (if (== t IntT) (return e)
    (if (== t SymbolT) (return (value_of env e))
    (if (== t StringT) (return e)
    (if (== t MexprT) (return (eval_mexpr env e))
        ))))
    (fprint_tag STDERR t) (eputs "\n")
    (not_implemented "eval")
    )

; === Primitive Functions
(fun allocate_prim (arity)
    (var prim (allocate (* 4 (+ 2 arity))))
    (set prim 0 (make_header PrimT 0 arity))
    (return prim)
    )

(fun add_prim1 (name p1 ptr)
    (var prim (allocate_prim 1))
    (set prim 1 p1)
    (set prim 2 ptr)
    (env_insert global_env name prim)
    )

(fun init_prims ()
    (add_prim1 printS (sym "e") print)
    (add_prim1 evalS (sym "e") eval)
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
    (return (str buf))
    )


(fun interpret (path)
    (var text (read_file path))
    (var ret 0)
    (var e 0)
    (var v 0)
    (while 1 (do
        (= ret (parse text))
        (= e (tup_get ret 0))
        (puts "parsed:") (print e) (puts "\n")
        (= v (eval global_env e))
        (puts "result:") (print v) (puts "\n")
        (= text (tup_get ret 1))
        (if (== (get_header_arg text) 0) (return ))
        ))
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
    (puts "total memory used: ")
    (puti (/ (- heap_pos heap_root) 0x100000))
    (puts "MB\n")
    )
