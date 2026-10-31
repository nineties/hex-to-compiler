; planck0 - An interpreted language dedicated to symbolic and language processing.
; Copyright (C) 2026 nineties

; planck0 serves as a minimal but fully functional subset of Planck,
; acting as an interim implementation for bootstrapping.
;
; Expressions (M-exprs) are compiled to bytecode and executed by a small
; virtual machine. Frames of the VM live in the heap, not on the machine
; stack, so that continuations for algebraic effects are just pointers
; to a chain of frames. See "Bytecode VM" below for details.

(include "std.sv")

; === Utilities

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

(fun align (n) ; align n to 8-byte boundary
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

; There is no GC in planck0. Memory obtained by mmap is zero-filled
; and never reused, so newly allocated memory is always zero.
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
;         |  27bit | 1bit | 4bit |
; symbol  |      0 |    0 | 0000 | text |
; string  | length |    0 | 0001 | text |
; mexpr   |  arity |    0 | 0010 | sym  | arg1 | .... |
; array   | length |    m | 0011 |
; lambda  |  arity |    0 | 0101 | code | env  |
; prim    |  arity |    0 | 0110 | ptr  | name | kind |
; cont    |      0 |    0 | 0111 | top frame | boundary frame |
; fusion  |      0 |    0 | 1000 | fun1 | fun2 |
; code    |      0 |    0 | 1010 | (see Bytecode VM)
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
(def LambdaT  0x5)
(def PrimT    0x6)
(def ContT    0x7)
(def FusionT  0x8)
(def IntT     0x9)
(def CodeT    0xa)

(fun fprint_tag (chan tag)
    (if (== tag SymbolT) (fputs chan "Symbol")
    (if (== tag StringT) (fputs chan "String")
    (if (== tag MexprT) (fputs chan "Mexpr")
    (if (== tag ArrayT) (fputs chan "Array")
    (if (== tag LambdaT) (fputs chan "Lambda")
    (if (== tag PrimT) (fputs chan "Prim")
    (if (== tag ContT) (fputs chan "Cont")
    (if (== tag FusionT) (fputs chan "Fusion")
    (if (== tag IntT) (fputs chan "Int")
        )))))))))
    )

(fun gettag (node)
    (if (& node 1) (return IntT))
    (return (& (get node 0) 0xf))
    )

(fun expect (tag val)
    (if (!= (gettag val) tag) (do
        (fprint_tag STDERR tag)
        (eputs " is expected: ")
        (eprint val)
        (eputs "\n")
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
            (== (get expr 1) head))
        (return 1)
        (return 0)
    ))

(fun is_fixnum (n)
    (return (& n 1))
    )

(fun is_mutable (node)
    (if (is_fixnum node) (return 0))
    (var header (get node 0))
    (return (& 0x10 header))
    )

(fun is_function (obj)
    (var t (gettag obj))
    (if (|| (== t PrimT) (|| (== t LambdaT) (|| (== t ContT) (== t FusionT))))
        (return 1)
        (return 0)
        )
    )

(fun make_header (tag mutable arg)
    (return (| tag (| (<< mutable 4) (<< arg 5))))
    )

(fun get_header_arg (node)
    (var header (get node 0))
    (return (>> header 5))
    )

(fun fixnum (n)
    (return (| (<< n 1) 1))
    )
(fun fixnum_to_int (n)
    (return (asr n 1))
    )

(fun str (text)
    (var data (allocate 8))
    (set data 0 (make_header StringT 0 (strlen text)))
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

; intern a temporary m-expr object
(fun mexpr (expr)
    (var obj (table_lookup mexprtable expr))
    (if obj (return obj))
    (var arity (get_header_arg expr))
    (= obj (allocate (* 4 (+ 2 arity))))
    (memcpy obj expr (* 4 (+ 2 arity)))
    (table_insert mexprtable obj obj)
    (return obj)
    )

(fun mexpr0 (h)
    (char[] 8 tmpobj)
    (set tmpobj 0 (make_header MexprT 0 0))
    (set tmpobj 1 h)
    (return (mexpr tmpobj))
    )

(fun mexpr1 (h a)
    (char[] 12 tmpobj)  ; 3 word temporary objet
    (set tmpobj 0 (make_header MexprT 0 1))
    (set tmpobj 1 h)
    (set tmpobj 2 a)
    (return (mexpr tmpobj))
    )

(fun mexpr2 (h a b)
    (char[] 16 tmpobj)  ; 4 word temporary objet
    (set tmpobj 0 (make_header MexprT 0 2))
    (set tmpobj 1 h)
    (set tmpobj 2 a)
    (set tmpobj 3 b)
    (return (mexpr tmpobj))
    )

; make an m-expr from an array of n values
; (a local buffer of the stack must be smaller than 128 bytes, so the
; buffer is global. mexprn is not reentrant.)
(def MAX_ARITY 64)
(char[] 264 mexprn_buf) ; m-expr with 64 arity at maximum
(fun mexprn (h n vals)
    (var tmpobj mexprn_buf)
    (var i 0)
    (if (> n MAX_ARITY) (do
        (eputs "too many arguments for M-expr\n")
        (exit 1)
        ))
    (set tmpobj 0 (make_header MexprT 0 n))
    (set tmpobj 1 h)
    (while (< i n) (do
        (set tmpobj (+ i 2) (get vals i))
        (+= i 1)
        ))
    (return (mexpr tmpobj))
    )

(fun hash (e)
    (var t (gettag e))
    (var h 0)
    (if (== t IntT) (do
        ; the bucket is taken from the low bits, so mix the high bits into them
        (= h (* (fixnum_to_int e) 536870909))
        (return (^ h (>> h 15)))
        ))
    (if (== t SymbolT) (return (symhash e)))
    (if (== t StringT) (return (strhash (str_text e))))
    (if (== t MexprT) (return (mexprhash e)))
    ; other objects are hashed by their addresses
    (return (symhash e))
    )

(fun fusion (f1 f2)
    (var obj (allocate 12))
    (if (|| (! (is_function f1)) (! (is_function f2))) (do
        (eputs "functions are expected for Fusion expr: ")
        (eprint f1)
        (eputs ", ")
        (eprint f2)
        (eputs "\n")
        (exit 1)
        ))
    (set obj 0 (make_header FusionT 0 0))
    (set obj 1 f1)
    (set obj 2 f2)
    (return obj)
    )

; === builtin symbols

(long trueS)
(long falseS)
(long noneS)
(long _S)
(long _TypeOfS)
(long intS)
(long stringS)
(long symbolS)
(long mexprS)
(long functionS)
(long arrayS)
(long DefS)
(long SetS)
(long ApplyS)
(long DoS)
(long IfS)
(long WhileS)
(long LambdaS)
(long FusionS)
(long QuoteS)
(long QuasiQuoteS)
(long UnQuoteS)
(long HandleS)
(long PerformS)
(long TupleS)
(long SyntaxErrorS)
(long EofS)
(long parseS)
(long evalS)
(long emptyTuple)

(fun tup2 (a b) (return (mexpr2 TupleS a b)))

(fun tup_get (e i)
    (expect_mexpr TupleS e)
    (var arity (get_header_arg e))
    (if (>= i arity) (not_reachable "tup_get"))
    (return (get e (+ 2 i)))
    )

(long global_table) ; global variables (sym -> data)

(fun init_tables ()
    (= symtable (make_table 0x10000 strhash streq))
    (= mexprtable (make_table 0x10000 mexprhash mexpreq))
    (= global_table (make_table 0x1000 symhash symeq))
    )

(fun init_symbols ()
    (= trueS    (sym "true"))
    (= falseS   (sym "false"))
    (= noneS    (sym "none"))
    (= _S       (sym "_"))
    (= _TypeOfS (sym "_TypeOf"))
    (= intS     (sym "int"))
    (= stringS  (sym "string"))
    (= symbolS  (sym "symbol"))
    (= mexprS   (sym "mexpr"))
    (= functionS (sym "function"))
    (= arrayS   (sym "array"))
    (= DefS         (sym "Def"))
    (= SetS         (sym "Set"))
    (= ApplyS       (sym "Apply"))
    (= DoS          (sym "Do"))
    (= IfS          (sym "If"))
    (= WhileS       (sym "While"))
    (= LambdaS      (sym "Lambda"))
    (= FusionS      (sym "Fusion"))
    (= QuoteS       (sym "Quote"))
    (= QuasiQuoteS  (sym "QuasiQuote"))
    (= UnQuoteS     (sym "UnQuote"))
    (= HandleS      (sym "Handle"))
    (= PerformS     (sym "Perform"))
    (= TupleS       (sym "Tuple"))
    (= SyntaxErrorS (sym "SyntaxError"))
    (= EofS         (sym "Eof"))
    (= parseS       (sym "parse"))
    (= evalS        (sym "eval"))
    (= emptyTuple   (mexpr0 TupleS))
    )

; Interned m-exprs without strings are equal iff they are identical.
; Strings are not interned, so m-exprs containing strings are compared
; structurally.
(fun values_equal (a b)
    (var i 0)
    (var n 0)
    (if (== a b) (return 1))
    (if (&& (== (gettag a) StringT) (== (gettag b) StringT))
        (return (streq (str_text a) (str_text b))))
    (if (&& (== (gettag a) MexprT) (== (gettag b) MexprT)) (do
        (if (!= (get a 1) (get b 1)) (return 0))
        (= n (get_header_arg a))
        (if (!= (get_header_arg b) n) (return 0))
        (while (< i n) (do
            (if (! (values_equal (get a (+ i 2)) (get b (+ i 2)))) (return 0))
            (+= i 1)
            ))
        (return 1)
        ))
    (return 0)
    )

(fun typeof (v)
    (var t (gettag v))
    (if (== t IntT) (return intS))
    (if (== t StringT) (return stringS))
    (if (== t SymbolT) (return symbolS))
    (if (== t MexprT) (return mexprS))
    (if (is_function v) (return functionS))
    (if (== t ArrayT) (return arrayS))
    (not_reachable "typeof")
    )

; ==== Printing
(fun escape_char (c)
    (if (== c 0) (return (char "0")))
    (if (== c 7) (return (char "a")))
    (if (== c 8) (return (char "b")))
    (if (== c 9) (return (char "t")))
    (if (== c 10) (return (char "n")))
    (if (== c 11) (return (char "v")))
    (if (== c 12) (return (char "f")))
    (if (== c 13) (return (char "r")))
    (if (== c (char "\"")) (return (char "\"")))
    (if (== c (char "\\")) (return (char "\\")))
    (return -1)
    )

(fun unescape_char (c)
    (if (== c (char "0")) (return 0))
    (if (== c (char "a")) (return 7))
    (if (== c (char "b")) (return 8))
    (if (== c (char "t")) (return 9))
    (if (== c (char "n")) (return 10))
    (if (== c (char "v")) (return 11))
    (if (== c (char "f")) (return 12))
    (if (== c (char "r")) (return 13))
    (if (== c (char "\"")) (return (char "\"")))
    (if (== c (char "'")) (return (char "'")))
    (if (== c (char "\\")) (return (char "\\")))
    (return -1)
    )

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

(fun fprint_lambda (chan e)
    (var pats (get (get e 1) 2)) ; patterns of the code object
    (var i 0)
    (var arity (get_header_arg e))
    (fputs chan "<lambda:(")
    (while (< i arity) (do
        (fprint chan (get pats (+ 2 i)))
        (+= i 1)
        (if (< i arity) (fputs chan ", "))
        ))
    (fputs chan ")>")
    )

(fun fprint (chan e)
    (var t (gettag e))
    (if (== t IntT) (fputi chan (fixnum_to_int e))
    (if (== t SymbolT) (fputs chan (sym_name e))
    (if (== t StringT) (fprint_str chan e)
    (if (== t MexprT) (fprint_mexpr chan e)
    (if (== t LambdaT) (fprint_lambda chan e)
    (if (== t PrimT) (do
        (fputs chan "<prim:")
        (fprint chan (get e 2))
        (fputs chan ">")
        )
    (if (== t ContT) (fputs chan "<cont>")
    (if (== t ArrayT) (fputs chan "<array>")
    (if (== t FusionT) (do
        (fprint chan (get e 1))
        (fputs chan "|")
        (fprint chan (get e 2))
        )
        (fputs chan "<?>")
        )))))))))
    )
(fun eprint (e) (fprint STDERR e))

; === Parsing
(fun syntax_error (msg)
    (return (mexpr1 SyntaxErrorS (str msg)))
    )

(fun is_blank (c)
    (if (|| (== c (char " "))
        (|| (== c (char "\t"))
        (|| (== c (char "\r"))
            (== c (char "\n")))))
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

(fun is_digit (c)
    (if (&& (<= (char "0") c) (<= c (char "9"))) (return 1))
    (return 0)
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
        (if (>= arity 16) (do
            (set ret (syntax_error "too many arguments"))
            (return raw)
            ))
        (= raw (parse_ tmp raw))
        (if (has_head SyntaxErrorS (get tmp)) (do
            (set ret (get tmp))
            (return raw)
            ))
        (set mexpr_buf (+ 2 arity) (get tmp))
        (+= arity 1)
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
    (if (is_digit c)
        (= raw (parse_int ret raw))
    (if (&& (== c (char "-")) (is_digit (getb raw 1))) (do
        ; negative integer literal
        (= raw (parse_int ret (+ raw 1)))
        (if (== (gettag (get ret)) IntT)
            (set ret (- 2 (get ret))) ; fixnum(-n) = 2 - fixnum(n)
            )
        )
    (if (== c (char "\""))
        (= raw (parse_str ret raw))
    (if (== c 0)
        (set ret (syntax_error "unexpected end of input"))
        (do
            (set ret (syntax_error "unexpected character"))
            (+= raw 1)
            )
        )))))
    (= raw (skip_spaces raw))
    (return raw)
    )

(fun parse (text)
    (char[] 4 ret)
    (expect StringT text)
    (var raw (skip_spaces (get text 1)))
    (if (== (getb raw) 0)
        (return (tup2 (mexpr0 EofS) (str raw)))
        )
    (= raw (parse_ ret raw))
    (return (tup2 (get ret) (str raw)))
    )

; === Bytecode VM
;
; planck0 compiles each top-level expression into a code object and runs it
; on a stack-based virtual machine. Every Lambda becomes its own code object.
;
; code object:
;   | header | arity | patterns | nslots | maxstack | ncode | code words ... |
;   patterns: Tuple of parameter patterns (0 for top-level code)
;   nslots:   number of variable slots of the environment
;   maxstack: maximum depth of the operand stack
;   Jump targets are word offsets from the first code word.
;
; environment (internal):
;   | parent env | slot0 | slot1 | ... |
;   Variables are resolved at compile time to (depth, slot) where depth is
;   the number of parent links to follow. An empty slot holds 0, which is
;   never a valid value. Variables are shared locations: closures and
;   continuations refer to the same environment.
;
;   Variables defined by top-level Def, and names that are not bound
;   lexically, are global variables looked up in global_table at run time.
;
; frame (internal, allocated in the heap):
;   | caller | code | pc | env | sp | handler | size | operand stack ... |
;   caller:  the frame which receives the return value (0 for top-level)
;   pc:      address of the next instruction (saved when not running)
;   sp:      byte offset of the stack top from the frame (saved)
;   handler: effect handler if this frame is the boundary of Handle, or 0
;   size:    size of the frame in words
;
; Since frames are not on the machine stack, the VM never calls itself
; recursively and the control state of a computation is just the chain
; of frames from the current frame to the top-level frame.
;
; Instructions (operands follow the opcode):
;   CONST v         push v
;   LOAD d i sym    push slot i of the d-th parent environment
;   STORE d i       store the stack top to slot i of the d-th parent env
;   GLOAD sym       push the global variable sym
;   GDEF sym        define the global variable sym with the stack top
;   GSET sym        update the global variable sym with the stack top
;   POP             discard the stack top
;   JMP off         jump
;   JF off          pop, jump if it is false
;   CALL n          call f with n arguments: [f a1 .. an] -> [result]
;   TCALL n         same as CALL but reuses the caller (tail call)
;   RET             return the stack top to the caller
;   CLOSURE code    push a closure of code and the current environment
;   TUPLE n         [a1 .. an] -> [Tuple{a1, .., an}]
;   MEXPR h n       [a1 .. an] -> [h{a1, .., an}]
;   FUSION          [f g] -> [f|g]
;   HANDLE          [body handler] -> [result]
;                   call the closure body in a new frame marked with handler
;   PERFORM         [v] -> [value given by resume]
;
; Function call:
;   The arguments are matched with the parameter patterns of the callee.
;   Matched pattern variables are written to the slots of a new environment
;   in the order of appearance. For f|g, f is tried first and then g.
;   A frame which is a boundary of Handle is never dropped by TCALL.
;
; Algebraic effects (deep handlers):
;   Handle{body, handler} evaluates body in a boundary frame B with handler.
;   Perform{v} searches the nearest boundary frame B from the current frame
;   and calls handler(v, k) whose result is returned to the caller of B,
;   i.e. it becomes the value of the Handle expression.
;   k is a continuation object (ContT) holding the current frame and B.
;   If the arguments do not match the handler, the next outer handler is
;   tried (effect forwarding).
;   Calling k with a value x copies the frames from the current frame to B
;   (B keeps its handler), links the copy of B to the caller of k, pushes x
;   as the result of Perform and runs the copied frames. Since captured
;   frames are never executed themselves, k can be resumed multiple times.
;   When the resumed body finishes, its value is returned to the caller of k.

(def C_ARITY    1)
(def C_PATS     2)
(def C_NSLOTS   3)
(def C_MAXSTACK 4)
(def C_NCODE    5)
(def C_CODE     6)

(def F_CALLER   0)
(def F_CODE     1)
(def F_PC       2)
(def F_ENV      3)
(def F_SP       4)
(def F_HANDLER  5)
(def F_SIZE     6)
(def F_STACK    7)

(def OP_CONST   0)
(def OP_LOAD    1)
(def OP_STORE   2)
(def OP_GLOAD   3)
(def OP_GDEF    4)
(def OP_GSET    5)
(def OP_POP     6)
(def OP_JMP     7)
(def OP_JF      8)
(def OP_CALL    9)
(def OP_TCALL   10)
(def OP_RET     11)
(def OP_CLOSURE 12)
(def OP_TUPLE   13)
(def OP_MEXPR   14)
(def OP_FUSION  15)
(def OP_HANDLE  16)
(def OP_PERFORM 17)
(def NUM_OPS    18)

(def PRIM_NATIVE 0)
(def PRIM_EVAL   1)

; === Compiler
;
; compile context:
;   | parent | level | start | nslots | depth | maxdepth | block | toplevel |
;   level:  nesting level of Lambda (0 for top-level)
;   start:  address of the first code word in the code buffer
;   block:  nesting level of blocks (If, While) in the function
;
; Code objects are emitted to a single code buffer. Since compilation of
; nested Lambdas is LIFO, the code of an inner Lambda is placed after the
; code of the outer one, copied to a code object when finished, and then
; the buffer is rewound.
;
; compile-time environment entry:
;   | sym | level | slot | next |

(def CX_PARENT   0)
(def CX_LEVEL    1)
(def CX_START    2)
(def CX_NSLOTS   3)
(def CX_DEPTH    4)
(def CX_MAXDEPTH 5)
(def CX_BLOCK    6)
(def CX_TOPLEVEL 7)

(long cbuf_pos)
(long cbuf_end)
(long cctx)
(long cenv)

(fun init_compiler ()
    (var size 0x400000) ; 4MB
    (= cbuf_pos (allocate size))
    (= cbuf_end (+ cbuf_pos size))
    (= cctx 0)
    (= cenv 0)
    )

(fun compile_error (msg e)
    (eputs "compile error: ")
    (eputs msg)
    (eputs ": ")
    (eprint e)
    (eputs "\n")
    (exit 1)
    )

(fun malformed (e)
    (compile_error "malformed expression" e)
    )

(fun emit (w)
    (if (u>= cbuf_pos cbuf_end) (do
        (eputs "code buffer overflow\n")
        (exit 1)
        ))
    (set cbuf_pos w)
    (+= cbuf_pos 4)
    )

(fun stack_effect (d)
    (var x (+ (get cctx CX_DEPTH) d))
    (set cctx CX_DEPTH x)
    (if (> x (get cctx CX_MAXDEPTH)) (set cctx CX_MAXDEPTH x))
    )

(fun code_offset ()
    (return (/ (- cbuf_pos (get cctx CX_START)) 4))
    )

(fun patch (off val)
    (set (get cctx CX_START) off val)
    )

(fun new_context (toplevel)
    (var ctx (allocate 32))
    (set ctx CX_PARENT cctx)
    (if cctx
        (set ctx CX_LEVEL (+ (get cctx CX_LEVEL) 1))
        (set ctx CX_LEVEL 0)
        )
    (set ctx CX_START cbuf_pos)
    (set ctx CX_NSLOTS 0)
    (set ctx CX_DEPTH 0)
    (set ctx CX_MAXDEPTH 0)
    (set ctx CX_BLOCK 0)
    (set ctx CX_TOPLEVEL toplevel)
    (return ctx)
    )

; copy the code of the current context to a new code object
(fun finish_code (arity pats)
    (var start (get cctx CX_START))
    (var n (/ (- cbuf_pos start) 4))
    (var code (allocate (* 4 (+ C_CODE n))))
    (set code 0 (make_header CodeT 0 0))
    (set code C_ARITY arity)
    (set code C_PATS pats)
    (set code C_NSLOTS (get cctx CX_NSLOTS))
    (set code C_MAXSTACK (get cctx CX_MAXDEPTH))
    (set code C_NCODE n)
    (memcpy (+ code (* 4 C_CODE)) start (* 4 n))
    (return code)
    )

(fun cenv_add (sym)
    (var slot (get cctx CX_NSLOTS))
    (var ent (allocate 16))
    (set cctx CX_NSLOTS (+ slot 1))
    (set ent 0 sym)
    (set ent 1 (get cctx CX_LEVEL))
    (set ent 2 slot)
    (set ent 3 cenv)
    (= cenv ent)
    (return slot)
    )

(fun cenv_lookup (sym)
    (var ent cenv)
    (while ent (do
        (if (== (get ent 0) sym) (return ent))
        (= ent (get ent 3))
        ))
    (return 0)
    )

(fun is_global_scope ()
    (if (&& (get cctx CX_TOPLEVEL) (== (get cctx CX_BLOCK) 0)) (return 1))
    (return 0)
    )

(fun begin_block ()
    (set cctx CX_BLOCK (+ (get cctx CX_BLOCK) 1))
    (return cenv)
    )

(fun end_block (saved)
    (set cctx CX_BLOCK (- (get cctx CX_BLOCK) 1))
    (= cenv saved)
    )

(fun emit_const (v)
    (emit OP_CONST)
    (emit v)
    (stack_effect 1)
    )

(fun compile_var (sym)
    (var ent (cenv_lookup sym))
    (if ent
        (do
            (emit OP_LOAD)
            (emit (- (get cctx CX_LEVEL) (get ent 1)))
            (emit (get ent 2))
            (emit sym)
            )
        (do
            (emit OP_GLOAD)
            (emit sym)
            ))
    (stack_effect 1)
    )

; allocate slots for variables in a pattern, in the order of appearance
(fun bind_pattern (p)
    (var t (gettag p))
    (var i 0)
    (var n 0)
    (if (== p _S) (return 0))
    (if (== t SymbolT) (do
        (cenv_add p)
        (return 0)
        ))
    (if (== t MexprT) (do
        (if (== (get p 1) QuoteS) (return 0))
        (if (== (get p 1) _TypeOfS) (do
            (if (!= (get_header_arg p) 2) (compile_error "malformed _TypeOf pattern" p))
            (bind_pattern (get p 2))
            (return 0)
            ))
        (= n (get_header_arg p))
        (while (< i n) (do
            (bind_pattern (get p (+ i 2)))
            (+= i 1)
            ))
        (return 0)
        ))
    (if (|| (== t IntT) (== t StringT)) (return 0))
    (compile_error "invalid pattern" p)
    )

; compile Lambda{pats, body} to a code object
(fun compile_lambda (pats body)
    (var saved_env cenv)
    (var saved_pos cbuf_pos)
    (var i 0)
    (var n (get_header_arg pats))
    (var code 0)
    (= cctx (new_context 0))
    (while (< i n) (do
        (bind_pattern (get pats (+ i 2)))
        (+= i 1)
        ))
    (compile body 1)
    (emit OP_RET)
    (= code (finish_code n pats))
    (= cctx (get cctx CX_PARENT))
    (= cenv saved_env)
    (= cbuf_pos saved_pos)
    (return code)
    )

(fun has_unquote (e nest)
    (var i 0)
    (var n 0)
    (if (!= (gettag e) MexprT) (return 0))
    (if (== (get e 1) UnQuoteS) (do
        (if (== nest 0) (return 1))
        (-= nest 1)
        ))
    (if (== (get e 1) QuasiQuoteS) (+= nest 1))
    (= n (get_header_arg e))
    (while (< i n) (do
        (if (has_unquote (get e (+ i 2)) nest) (return 1))
        (+= i 1)
        ))
    (return 0)
    )

(fun compile_qquote (e nest)
    (var i 0)
    (var n 0)
    (var head 0)
    (if (! (has_unquote e nest)) (do
        (emit_const e)
        (return 0)
        ))
    (= head (get e 1))
    (= n (get_header_arg e))
    (if (|| (== head UnQuoteS) (== head QuasiQuoteS))
        (if (!= n 1) (malformed e)))
    (if (== head UnQuoteS) (do
        (if (== nest 0) (do
            (compile (get e 2) 0)
            (return 0)
            ))
        (-= nest 1)
        ))
    (if (== head QuasiQuoteS) (+= nest 1))
    (while (< i n) (do
        (compile_qquote (get e (+ i 2)) nest)
        (+= i 1)
        ))
    (emit OP_MEXPR)
    (emit head)
    (emit n)
    (stack_effect (- 1 n))
    )

; compile args of e as values and emit an instruction which builds an m-expr
(fun compile_construct (e)
    (var i 0)
    (var head (get e 1))
    (var n (get_header_arg e))
    (while (< i n) (do
        (compile (get e (+ i 2)) 0)
        (+= i 1)
        ))
    (if (== head TupleS)
        (do
            (emit OP_TUPLE)
            (emit n)
            )
        (do
            (emit OP_MEXPR)
            (emit head)
            (emit n)
            ))
    (stack_effect (- 1 n))
    )

(fun compile_mexpr (e tail)
    (var head (get e 1))
    (var arity (get_header_arg e))
    (var i 0)
    (var n 0)
    (var x 0)
    (var ent 0)
    (var slot 0)
    (var saved 0)
    (var l0 0)
    (var l1 0)
    (var l2 0)
    (var args 0)

    (if (== head ApplyS) (do
        (if (|| (!= arity 2) (! (has_head TupleS (get e 3)))) (malformed e))
        (= args (get e 3))
        (= n (get_header_arg args))
        (compile (get e 2) 0)
        (while (< i n) (do
            (compile (get args (+ i 2)) 0)
            (+= i 1)
            ))
        (if (&& tail (! (get cctx CX_TOPLEVEL)))
            (emit OP_TCALL)
            (emit OP_CALL)
            )
        (emit n)
        (stack_effect (- n))
        (return 0)
        ))
    (if (== head DefS) (do
        (if (|| (!= arity 2) (!= (gettag (get e 2)) SymbolT)) (malformed e))
        (= x (get e 2))
        (if (is_global_scope)
            (do
                (compile (get e 3) 0)
                (emit OP_GDEF)
                (emit x)
                )
            (do
                ; The variable is visible from its definition
                ; so that recursive functions can be defined.
                (= slot (cenv_add x))
                (compile (get e 3) 0)
                (emit OP_STORE)
                (emit 0)
                (emit slot)
                ))
        (return 0)
        ))
    (if (== head SetS) (do
        (if (|| (!= arity 2) (!= (gettag (get e 2)) SymbolT)) (malformed e))
        (= x (get e 2))
        (compile (get e 3) 0)
        (= ent (cenv_lookup x))
        (if ent
            (do
                (emit OP_STORE)
                (emit (- (get cctx CX_LEVEL) (get ent 1)))
                (emit (get ent 2))
                )
            (do
                (emit OP_GSET)
                (emit x)
                ))
        (return 0)
        ))
    (if (== head DoS) (do
        (if (== arity 0) (do
            (emit_const noneS)
            (return 0)
            ))
        (while (< i arity) (do
            (if (== i (- arity 1))
                (compile (get e (+ i 2)) tail)
                (do
                    (compile (get e (+ i 2)) 0)
                    (emit OP_POP)
                    (stack_effect -1)
                    ))
            (+= i 1)
            ))
        (return 0)
        ))
    (if (== head IfS) (do
        (if (!= arity 3) (malformed e))
        (compile (get e 2) 0)
        (emit OP_JF)
        (= l1 (code_offset))
        (emit 0)
        (stack_effect -1)

        (= saved (begin_block))
        (compile (get e 3) tail)
        (end_block saved)
        (emit OP_JMP)
        (= l2 (code_offset))
        (emit 0)
        (stack_effect -1)

        (patch l1 (code_offset))
        (= saved (begin_block))
        (compile (get e 4) tail)
        (end_block saved)
        (patch l2 (code_offset))
        (return 0)
        ))
    (if (== head WhileS) (do
        (if (!= arity 2) (malformed e))
        (= l0 (code_offset))
        (compile (get e 2) 0)
        (emit OP_JF)
        (= l1 (code_offset))
        (emit 0)
        (stack_effect -1)

        (= saved (begin_block))
        (compile (get e 3) 0)
        (end_block saved)
        (emit OP_POP)
        (stack_effect -1)
        (emit OP_JMP)
        (emit l0)

        (patch l1 (code_offset))
        (emit_const noneS)
        (return 0)
        ))
    (if (== head LambdaS) (do
        (if (|| (!= arity 2) (! (has_head TupleS (get e 2)))) (malformed e))
        (emit OP_CLOSURE)
        (emit (compile_lambda (get e 2) (get e 3)))
        (stack_effect 1)
        (return 0)
        ))
    (if (== head FusionS) (do
        (if (!= arity 2) (malformed e))
        (compile (get e 2) 0)
        (compile (get e 3) 0)
        (emit OP_FUSION)
        (stack_effect -1)
        (return 0)
        ))
    (if (== head QuoteS) (do
        (if (!= arity 1) (malformed e))
        (emit_const (get e 2))
        (return 0)
        ))
    (if (== head QuasiQuoteS) (do
        (if (!= arity 1) (malformed e))
        (compile_qquote (get e 2) 0)
        (return 0)
        ))
    (if (== head UnQuoteS) (compile_error "UnQuote outside of QuasiQuote" e))
    (if (== head HandleS) (do
        (if (!= arity 2) (malformed e))
        (emit OP_CLOSURE)
        (emit (compile_lambda emptyTuple (get e 2)))
        (stack_effect 1)
        (compile (get e 3) 0)
        (emit OP_HANDLE)
        (stack_effect -1)
        (return 0)
        ))
    (if (== head PerformS) (do
        (if (!= arity 1) (malformed e))
        (compile (get e 2) 0)
        (emit OP_PERFORM)
        (return 0)
        ))
    ; Tuple and other m-exprs: evaluate arguments and construct an m-expr
    (compile_construct e)
    )

(fun compile (e tail)
    (var t (gettag e))
    (if (|| (== t IntT) (== t StringT)) (do
        (emit_const e)
        (return 0)
        ))
    (if (== t SymbolT) (do
        (compile_var e)
        (return 0)
        ))
    (if (== t MexprT) (do
        (compile_mexpr e tail)
        (return 0)
        ))
    (compile_error "cannot compile" e)
    )

(fun compile_toplevel (e)
    (var saved_ctx cctx)
    (var saved_env cenv)
    (var saved_pos cbuf_pos)
    (var code 0)
    (= cctx 0)
    (= cctx (new_context 1))
    (= cenv 0)
    (compile e 0)
    (emit OP_RET)
    (= code (finish_code 0 0))
    (= cctx saved_ctx)
    (= cenv saved_env)
    (= cbuf_pos saved_pos)
    (return code)
    )

; === Virtual Machine

(long vm_frame)     ; current frame (0 when the VM stops)
(long vm_pc)        ; address of the next instruction
(long vm_sp)        ; address of the next free stack slot
(long vm_env)       ; current environment
(long vm_code)      ; current code object
(long vm_result)    ; result of the top-level code
(long sel_env)      ; environment created by select()

(char[] 128 optable) ; opcode -> function

(fun vm_push (v)
    (set vm_sp v)
    (+= vm_sp 4)
    )

(fun vm_pop ()
    (-= vm_sp 4)
    (return (get vm_sp))
    )

(fun fetch ()
    (var w (get vm_pc))
    (+= vm_pc 4)
    (return w)
    )

(fun save_regs ()
    (set vm_frame F_PC vm_pc)
    (set vm_frame F_SP (- vm_sp vm_frame))
    )

(fun load_regs (f)
    (= vm_frame f)
    (= vm_code (get f F_CODE))
    (= vm_pc (get f F_PC))
    (= vm_env (get f F_ENV))
    (= vm_sp (+ f (get f F_SP)))
    )

(fun new_frame (code env caller handler)
    (var size (+ F_STACK (+ (get code C_MAXSTACK) 2)))
    (var f (allocate (* 4 size)))
    (set f F_CALLER caller)
    (set f F_CODE code)
    (set f F_PC (+ code (* 4 C_CODE)))
    (set f F_ENV env)
    (set f F_SP (* 4 F_STACK))
    (set f F_HANDLER handler)
    (set f F_SIZE size)
    (return f)
    )

(fun copy_frame (f)
    (var size (get f F_SIZE))
    (var c (allocate (* 4 size)))
    (memcpy c f (* 4 size))
    (return c)
    )

(fun new_env (code parent)
    (var env (allocate (* 4 (+ 1 (get code C_NSLOTS)))))
    (set env 0 parent)
    (return env)
    )

; make the frame f current (regs of the current frame must be saved)
(fun enter (code env caller handler)
    (load_regs (new_frame code env caller handler))
    )

; give the value v to the frame caller and continue it
(fun deliver (caller v)
    (if (! caller) (do
        (= vm_result v)
        (= vm_frame 0)
        (return 0)
        ))
    (load_regs caller)
    (vm_push v)
    )

(fun runtime_error (msg e)
    (eputs msg)
    (eprint e)
    (eputs "\n")
    (exit 1)
    )

; --- pattern matching

(fun match_pat (p v env idx)
    (var t (gettag p))
    (var i 0)
    (var n 0)
    (var h 0)
    (if (== p _S) (return 1)) ; wildcard
    (if (== t SymbolT) (do ; variable pattern
        (set env (+ 1 (get idx)) v)
        (set idx (+ (get idx) 1))
        (return 1)
        ))
    (if (|| (== t IntT) (== t StringT)) (return (values_equal p v)))
    (if (== t MexprT) (do
        (= h (get p 1))
        (if (== h QuoteS) (return (values_equal (get p 2) v)))
        (if (== h _TypeOfS) (do
            (if (!= (typeof v) (get p 3)) (return 0))
            (return (match_pat (get p 2) v env idx))
            ))
        (if (!= (gettag v) MexprT) (return 0))
        (if (!= (get v 1) h) (return 0))
        (= n (get_header_arg p))
        (if (!= (get_header_arg v) n) (return 0))
        (while (< i n) (do
            (if (! (match_pat (get p (+ i 2)) (get v (+ i 2)) env idx)) (return 0))
            (+= i 1)
            ))
        (return 1)
        ))
    (return 0)
    )

(fun match_args (pats args nargs env)
    (char[] 4 idx)
    (var i 0)
    (set idx 0 0)
    (while (< i nargs) (do
        (if (! (match_pat (get pats (+ i 2)) (get args i) env idx)) (return 0))
        (+= i 1)
        ))
    (return 1)
    )

; select the function to be called for args.
; returns 0 if no function matches.
(fun select (f args nargs)
    (var t (gettag f))
    (var r 0)
    (var code 0)
    (var env 0)
    (if (== t FusionT) (do
        (= r (select (get f 1) args nargs))
        (if r (return r))
        (return (select (get f 2) args nargs))
        ))
    (if (== t LambdaT) (do
        (= code (get f 1))
        (if (!= (get code C_ARITY) nargs) (return 0))
        (= env (new_env code (get f 2)))
        (if (! (match_args (get code C_PATS) args nargs env)) (return 0))
        (= sel_env env)
        (return f)
        ))
    (if (== t PrimT) (do
        (if (!= (get_header_arg f) nargs) (return 0))
        (return f)
        ))
    (if (== t ContT) (do
        (if (!= nargs 1) (return 0))
        (return f)
        ))
    (runtime_error "not a function: " f)
    )

; resume the continuation k with x. the result goes to caller.
(fun resume (k x caller)
    (var top (get k 1))
    (var b (get k 2))
    (var f top)
    (var first (copy_frame top))
    (var last first)
    (var c 0)
    (while (!= f b) (do
        (= f (get f F_CALLER))
        (= c (copy_frame f))
        (set last F_CALLER c)
        (= last c)
        ))
    (set last F_CALLER caller)
    (load_regs first)
    (vm_push x)
    )

; call callee (selected by select()) with args. the result goes to caller.
(fun invoke (callee args nargs caller)
    (var t (gettag callee))
    (var fn 0)
    (var v 0)
    (var code 0)
    (if (== t LambdaT) (do
        (enter (get callee 1) sel_env caller 0)
        (return 0)
        ))
    (if (== t PrimT) (do
        (if (== (get callee 3) PRIM_EVAL) (do
            (= code (compile_toplevel (get args 0)))
            (enter code (new_env code 0) caller 0)
            (return 0)
            ))
        (= fn (get callee 1))
        (if (== nargs 0) (= v (fn))
        (if (== nargs 1) (= v (fn (get args 0)))
        (if (== nargs 2) (= v (fn (get args 0) (get args 1)))
        (if (== nargs 3) (= v (fn (get args 0) (get args 1) (get args 2)))
            (not_reachable "invoke")
            ))))
        (deliver caller v)
        (return 0)
        ))
    (if (== t ContT) (do
        (resume callee (get args 0) caller)
        (return 0)
        ))
    (not_reachable "invoke")
    )

(fun match_error (f args nargs)
    (var i 0)
    (eputs "matching failed: ")
    (eprint f)
    (eputs " <-> (")
    (while (< i nargs) (do
        (eprint (get args i))
        (+= i 1)
        (if (< i nargs) (eputs ", "))
        ))
    (eputs ")\n")
    (exit 1)
    )

(fun vm_call (nargs tail)
    (var args (- vm_sp (* 4 nargs)))
    (var f (get (- args 4)))
    (var callee (select f args nargs))
    (var caller 0)
    (if (! callee) (match_error f args nargs))
    (= vm_sp (- args 4))
    (save_regs)
    (= caller vm_frame)
    (if (&& tail (! (get vm_frame F_HANDLER)))
        (= caller (get vm_frame F_CALLER)))
    (invoke callee args nargs caller)
    )

; --- instructions

(fun op_const () (vm_push (fetch)))

(fun op_load ()
    (var d (fetch))
    (var i (fetch))
    (var s (fetch))
    (var env vm_env)
    (var v 0)
    (while (> d 0) (do
        (= env (get env 0))
        (-= d 1)
        ))
    (= v (get env (+ i 1)))
    (if (! v) (runtime_error "variable used before definition: " s))
    (vm_push v)
    )

(fun op_store ()
    (var d (fetch))
    (var i (fetch))
    (var env vm_env)
    (while (> d 0) (do
        (= env (get env 0))
        (-= d 1)
        ))
    (set env (+ i 1) (get (- vm_sp 4)))
    )

(fun op_gload ()
    (var s (fetch))
    (var v (table_lookup global_table s))
    (if (! v) (runtime_error "undefined variable: " s))
    (vm_push v)
    )

(fun op_gdef ()
    (var s (fetch))
    (var v (get (- vm_sp 4)))
    (if (! (table_update global_table s v))
        (table_insert global_table s v))
    )

(fun op_gset ()
    (var s (fetch))
    (if (! (table_update global_table s (get (- vm_sp 4))))
        (runtime_error "undefined variable: " s))
    )

(fun op_pop () (-= vm_sp 4))

(fun jump (off)
    (= vm_pc (+ vm_code (* 4 (+ C_CODE off))))
    )

(fun op_jmp () (jump (fetch)))

(fun op_jf ()
    (var off (fetch))
    (if (== (vm_pop) falseS) (jump off))
    )

(fun op_call () (vm_call (fetch) 0))
(fun op_tcall () (vm_call (fetch) 1))

(fun op_ret ()
    (var v (vm_pop))
    (deliver (get vm_frame F_CALLER) v)
    )

(fun op_closure ()
    (var code (fetch))
    (var clo (allocate 12))
    (set clo 0 (make_header LambdaT 0 (get code C_ARITY)))
    (set clo 1 code)
    (set clo 2 vm_env)
    (vm_push clo)
    )

(fun op_tuple ()
    (var n (fetch))
    (-= vm_sp (* 4 n))
    (vm_push (mexprn TupleS n vm_sp))
    )

(fun op_mexpr ()
    (var h (fetch))
    (var n (fetch))
    (-= vm_sp (* 4 n))
    (vm_push (mexprn h n vm_sp))
    )

(fun op_fusion ()
    (var g (vm_pop))
    (var f (vm_pop))
    (vm_push (fusion f g))
    )

(fun op_handle ()
    (var handler (vm_pop))
    (var body (vm_pop))
    (var code (get body 1))
    (if (! (is_function handler)) (runtime_error "handler is not a function: " handler))
    (save_regs)
    (enter code (new_env code (get body 2)) vm_frame handler)
    )

(fun make_cont (top b)
    (var k (allocate 12))
    (set k 0 (make_header ContT 0 0))
    (set k 1 top)
    (set k 2 b)
    (return k)
    )

(fun op_perform ()
    (char[] 8 hargs)
    (var v (vm_pop))
    (var top vm_frame)
    (var b vm_frame)
    (var h 0)
    (var callee 0)
    (save_regs)
    (while b (do
        (= h (get b F_HANDLER))
        (if h (do
            (set hargs 0 v)
            (set hargs 1 (make_cont top b))
            (= callee (select h hargs 2))
            (if callee (do
                (invoke callee hargs 2 (get b F_CALLER))
                (return 0)
                ))
            ))
        (= b (get b F_CALLER))
        ))
    (runtime_error "unhandled effect: " v)
    )

(fun init_vm ()
    (set optable OP_CONST   op_const)
    (set optable OP_LOAD    op_load)
    (set optable OP_STORE   op_store)
    (set optable OP_GLOAD   op_gload)
    (set optable OP_GDEF    op_gdef)
    (set optable OP_GSET    op_gset)
    (set optable OP_POP     op_pop)
    (set optable OP_JMP     op_jmp)
    (set optable OP_JF      op_jf)
    (set optable OP_CALL    op_call)
    (set optable OP_TCALL   op_tcall)
    (set optable OP_RET     op_ret)
    (set optable OP_CLOSURE op_closure)
    (set optable OP_TUPLE   op_tuple)
    (set optable OP_MEXPR   op_mexpr)
    (set optable OP_FUSION  op_fusion)
    (set optable OP_HANDLE  op_handle)
    (set optable OP_PERFORM op_perform)
    )

(fun run_toplevel (code)
    (var op 0)
    (var h 0)
    (load_regs (new_frame code (new_env code 0) 0 0))
    (while vm_frame (do
        (= op (fetch))
        (if (u>= op NUM_OPS) (do
            (eputs "invalid opcode\n")
            (exit 1)
            ))
        (= h (get optable op))
        (h)
        ))
    (return vm_result)
    )

; === Primitive Functions

(fun bool (b)
    (if b (return trueS))
    (return falseS)
    )

(fun check_int (name v)
    (if (!= (gettag v) IntT) (do
        (eputs name)
        (eputs ": integer is expected: ")
        (eprint v)
        (eputs "\n")
        (exit 1)
        ))
    )

; signed division truncated toward zero
(fun sdiv (a b)
    (var neg 0)
    (var q 0)
    (if (< a 0) (do
        (= a (- a))
        (= neg (^ neg 1))
        ))
    (if (< b 0) (do
        (= b (- b))
        (= neg (^ neg 1))
        ))
    (= q (/ a b))
    (if neg (return (- q)))
    (return q)
    )

(fun p_print (e)
    (fprint STDOUT e)
    (return noneS)
    )

(fun p_println (e)
    (fprint STDOUT e)
    (puts "\n")
    (return noneS)
    )

(fun p_put (s)
    (puts (str_text s))
    (return noneS)
    )

(fun p_add (a b)
    (check_int "add" a)
    (check_int "add" b)
    (return (+ a (- b 1)))
    )

(fun p_sub (a b)
    (check_int "sub" a)
    (check_int "sub" b)
    (return (+ (- a b) 1))
    )

(fun p_mul (a b)
    (check_int "mul" a)
    (check_int "mul" b)
    (return (fixnum (* (fixnum_to_int a) (fixnum_to_int b))))
    )

(fun p_div (a b)
    (check_int "div" a)
    (check_int "div" b)
    (if (== b 1) (runtime_error "division by zero: " a))
    (return (fixnum (sdiv (fixnum_to_int a) (fixnum_to_int b))))
    )

(fun p_mod (a b)
    (var x 0)
    (var y 0)
    (check_int "mod" a)
    (check_int "mod" b)
    (if (== b 1) (runtime_error "division by zero: " a))
    (= x (fixnum_to_int a))
    (= y (fixnum_to_int b))
    (return (fixnum (- x (* y (sdiv x y)))))
    )

(fun p_lt (a b)
    (check_int "lt" a)
    (check_int "lt" b)
    (if (< a b) (return trueS))
    (return falseS)
    )

(fun p_le (a b)
    (check_int "le" a)
    (check_int "le" b)
    (if (<= a b) (return trueS))
    (return falseS)
    )

(fun p_gt (a b)
    (check_int "gt" a)
    (check_int "gt" b)
    (if (> a b) (return trueS))
    (return falseS)
    )

(fun p_ge (a b)
    (check_int "ge" a)
    (check_int "ge" b)
    (if (>= a b) (return trueS))
    (return falseS)
    )

(fun p_eq (a b) (return (bool (values_equal a b))))

(fun p_ne (a b)
    (if (values_equal a b) (return falseS))
    (return trueS)
    )

(fun p_head (m)
    (expect MexprT m)
    (return (get m 1))
    )

(fun p_arity (m)
    (expect MexprT m)
    (return (fixnum (get_header_arg m)))
    )

(fun p_arg (m i)
    (var n 0)
    (expect MexprT m)
    (check_int "arg" i)
    (= n (fixnum_to_int i))
    (if (|| (< n 0) (>= n (get_header_arg m))) (runtime_error "arg: index out of range: " i))
    (return (get m (+ n 2)))
    )

(fun p_mexpr (h args)
    (expect SymbolT h)
    (expect_mexpr TupleS args)
    (return (mexprn h (get_header_arg args) (+ args 8)))
    )

(fun p_typeof (v) (return (typeof v)))

; --- strings, symbols, tables and files (used by planck/peg.pk)

; a string object for text of length n (text is not copied)
(fun str_n (text n)
    (var data (allocate 8))
    (set data 0 (make_header StringT 0 n))
    (set data 1 text)
    (return data)
    )

(fun str_len (s) (return (get_header_arg s)))

(fun p_strlen (s)
    (expect StringT s)
    (return (fixnum (str_len s)))
    )

; ord(s, i): the byte at i, or -1 when i is out of range
(fun p_ord (s i)
    (var n 0)
    (expect StringT s)
    (check_int "ord" i)
    (= n (fixnum_to_int i))
    (if (|| (< n 0) (>= n (str_len s))) (return (fixnum -1)))
    (return (fixnum (getb (get s 1) n)))
    )

; substr(s, i, j): bytes from i to j-1. The text is shared when j is the end.
(fun p_substr (s i j)
    (var len 0)
    (expect StringT s)
    (check_int "substr" i)
    (check_int "substr" j)
    (= len (str_len s))
    (= i (fixnum_to_int i))
    (= j (fixnum_to_int j))
    (if (|| (< i 0) (|| (> j len) (> i j))) (do
        (eputs "substr: index out of range: ")
        (eprint s)
        (eputs " ")
        (eputi i)
        (eputs " ")
        (eputi j)
        (eputs "\n")
        (exit 1)
        ))
    (if (== j len) (return (str_n (+ (get s 1) i) (- j i))))
    (return (str_n (strndup (+ (get s 1) i) (- j i)) (- j i)))
    )

(fun p_strcat (a b)
    (var n 0)
    (var m 0)
    (var text 0)
    (expect StringT a)
    (expect StringT b)
    (= n (str_len a))
    (= m (str_len b))
    (= text (allocate (+ (+ n m) 1)))
    (memcpy text (get a 1) n)
    (memcpy (+ text n) (get b 1) m)
    (setb text (+ n m) 0)
    (return (str_n text (+ n m)))
    )

(fun p_intern (s)
    (expect StringT s)
    (return (sym (strndup (get s 1) (str_len s))))
    )

; symname(s): the name of the symbol s as a string (the inverse of intern)
(fun p_symname (s)
    (return (str (sym_name s)))
    )

; push(m, x): the m-expr m with x appended
(char[] 256 push_buf)
(fun p_push (m x)
    (var buf push_buf)
    (var n 0)
    (expect MexprT m)
    (= n (get_header_arg m))
    (if (>= n MAX_ARITY) (runtime_error "push: too many arguments: " m))
    (memcpy buf (+ m 8) (* 4 n))
    (set buf n x)
    (return (mexprn (get m 1) (+ n 1) buf))
    )

; array(n): a mutable array of n elements, all none
(fun p_array (n)
    (var a 0)
    (var i 0)
    (check_int "array" n)
    (= i (fixnum_to_int n))
    (if (|| (< i 0) (> i 0x7ffffff)) (runtime_error "array: bad length: " n))
    (= a (allocate (* 4 (+ i 1))))
    (set a 0 (make_header ArrayT 1 i))
    (while (> i 0) (do
        (-= i 1)
        (set a (+ i 1) noneS)
        ))
    (return a)
    )

(fun array_index (name a i)
    (var n 0)
    (expect ArrayT a)
    (check_int name i)
    (= n (fixnum_to_int i))
    (if (|| (< n 0) (>= n (get_header_arg a)))
        (runtime_error "array index out of range: " i))
    (return n)
    )

(fun p_array_get (a i)
    (var n 0)
    (= n (array_index "array_get" a i))
    (return (get a (+ n 1)))
    )

(fun p_array_set (a i v)
    (var n 0)
    (= n (array_index "array_set" a i))
    (set a (+ n 1) v)
    (return v)
    )

(fun p_array_len (a)
    (expect ArrayT a)
    (return (fixnum (get_header_arg a)))
    )

; hash(x): a non-negative integer that is equal for values equal by eq
(fun p_hash (x)
    (return (fixnum (& (hash x) 0xfffffff)))
    )

(fun p_read_file (path)
    (expect StringT path)
    (return (read_file (get path 1)))
    )

(fun p_parse (s) (return (parse s)))

(fun p_exit (n)
    (check_int "exit" n)
    (exit (fixnum_to_int n))
    )

(fun defprim (name arity fn kind)
    (var prim (allocate 16))
    (var s (sym name))
    (set prim 0 (make_header PrimT 0 arity))
    (set prim 1 fn)
    (set prim 2 s)
    (set prim 3 kind)
    (table_insert global_table s prim)
    )

(fun init_prims ()
    (defprim "print"   1 p_print   PRIM_NATIVE)
    (defprim "println" 1 p_println PRIM_NATIVE)
    (defprim "put"     1 p_put     PRIM_NATIVE)
    (defprim "add"     2 p_add     PRIM_NATIVE)
    (defprim "sub"     2 p_sub     PRIM_NATIVE)
    (defprim "mul"     2 p_mul     PRIM_NATIVE)
    (defprim "div"     2 p_div     PRIM_NATIVE)
    (defprim "mod"     2 p_mod     PRIM_NATIVE)
    (defprim "lt"      2 p_lt      PRIM_NATIVE)
    (defprim "le"      2 p_le      PRIM_NATIVE)
    (defprim "gt"      2 p_gt      PRIM_NATIVE)
    (defprim "ge"      2 p_ge      PRIM_NATIVE)
    (defprim "eq"      2 p_eq      PRIM_NATIVE)
    (defprim "ne"      2 p_ne      PRIM_NATIVE)
    (defprim "head"    1 p_head    PRIM_NATIVE)
    (defprim "arity"   1 p_arity   PRIM_NATIVE)
    (defprim "arg"     2 p_arg     PRIM_NATIVE)
    (defprim "mexpr"   2 p_mexpr   PRIM_NATIVE)
    (defprim "typeof"  1 p_typeof  PRIM_NATIVE)
    (defprim "parse"   1 p_parse   PRIM_NATIVE)
    (defprim "strlen"  1 p_strlen  PRIM_NATIVE)
    (defprim "ord"     2 p_ord     PRIM_NATIVE)
    (defprim "substr"  3 p_substr  PRIM_NATIVE)
    (defprim "strcat"  2 p_strcat  PRIM_NATIVE)
    (defprim "intern"  1 p_intern  PRIM_NATIVE)
    (defprim "symname" 1 p_symname PRIM_NATIVE)
    (defprim "push"    2 p_push    PRIM_NATIVE)
    (defprim "array"   1 p_array   PRIM_NATIVE)
    (defprim "array_get" 2 p_array_get PRIM_NATIVE)
    (defprim "array_set" 3 p_array_set PRIM_NATIVE)
    (defprim "array_len" 1 p_array_len PRIM_NATIVE)
    (defprim "hash"    1 p_hash    PRIM_NATIVE)
    (defprim "read_file" 1 p_read_file PRIM_NATIVE)
    (defprim "exit"    1 p_exit    PRIM_NATIVE)
    (defprim "eval"    1 0         PRIM_EVAL)
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

; The loop looks up the global variables `parse` and `eval` every time, so
; a program can replace them (Def{parse, ...}, Def{eval, ...}) and the next
; top-level expression is parsed/evaluated by the new definitions. Apply{eval, ...}
; in a program also calls the replaced eval.
;   parse: String -> Tuple{m-expr, rest String}   (initially the builtin parse)
;     When only white space is left, parse returns Tuple{Eof{}, rest} and the
;     loop stops. A replacement parse should pass Eof{} through.
;   eval: m-expr -> any value                     (initially the builtin eval)
; Inside the VM, the builtin eval (PRIM_EVAL) is called through the global
; variable like any other function.
(fun call_global (name arg)
    (return (run_toplevel (compile_toplevel
        (mexpr2 ApplyS name (mexpr1 TupleS arg)))))
    )

(fun interpret_file (path)
    (var text (read_file path))
    (var ret 0)
    (var e 0)
    (var more 1)
    (while more (do
        (= ret (call_global parseS text))
        (if (|| (! (has_head TupleS ret))
            (|| (!= (get_header_arg ret) 2)
                (!= (gettag (tup_get ret 1)) StringT))) (do
            (eputs path)
            (eputs ": read must return Tuple{m-expr, string}, got ")
            (eprint ret)
            (eputs "\n")
            (exit 1)
            ))
        (= e (tup_get ret 0))
        (if (has_head EofS e) (= more 0) (do
            (if (has_head SyntaxErrorS e) (do
                (eputs path)
                (eputs ": syntax error: ")
                (eprint e)
                (eputs "\n")
                (exit 1)
                ))
            (call_global evalS (mexpr1 QuoteS e))
            (= text (tup_get ret 1))
            ))
        ))
    )

(fun main (argc argv)
    (var i 1)
    (init_heap)
    (init_tables)
    (init_symbols)
    (init_compiler)
    (init_vm)
    (init_prims)

    (interpret_file "planck/init.pk")
    (while (< i argc) (do
        (interpret_file (get argv i))
        (+= i 1)
        ))
    )
