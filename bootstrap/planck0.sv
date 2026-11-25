; planck0 - An interpreted language dedicated to symbolic and language processing.
; Copyright (C) 2026 nineties

; planck0 serves as a minimal but fully functional subset of Planck,
; acting as an interim implementation for bootstrapping.
;
; Expressions (M-exprs) are compiled to bytecode and executed by a small
; virtual machine. Frames of the VM live on a frame stack separate from the
; machine stack and the heap, and are dropped when the function returns.
; A continuation for algebraic effects is a copy of the frames in the heap.
; See "Bytecode VM" below for details.

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

; The heap is a fixed area obtained by mmap2. It is a sequence of chunks.
; A chunk is a multiple of 8 bytes and begins with a header word:
; the size of the chunk with the flags GC_MARK (bit 0) and GC_FREE (bit 1).
; An object is the part of its chunk after the header, so a pointer to an
; object is 4 bytes after the beginning of its chunk. The beginnings of the
; chunks of the objects are recorded in a bitmap (one bit for 8 bytes).
; The memory is taken from the free chunks and then from the end of the heap.
; A collector (see "Garbage Collection") frees the objects which cannot be reached.
(def GC_MARK 1)
(def GC_FREE 2)
(def GC_STACK_WORDS 16777216)
(def GC_MIN_BYTES 67108864)

(long heap_root)
(long heap_end)
(long heap_pos)
(long gc_bitmap)     ; bit i is 1 if a chunk of an object begins at heap_root + 8 * i
(long gc_stack)      ; stack of the objects which are marked and are not scanned yet
(long gc_sp)
(long gc_stack_end)
(long gc_lists)      ; free chunks: word n (1..32) is the list of the chunks of 8 * n bytes, word 0 is the list of the larger chunks
(long gc_since)      ; bytes allocated since the last collection
(long gc_limit)      ; a collection starts when gc_since reaches this
(long gc_live)       ; bytes of the objects marked by the collection
(long gc_runs)
(long alloc_hi)
(long alloc_lo)

(fun map_zero (bytes)
    (var addr (mmap2 0 bytes
        (| PROT_READ PROT_WRITE) (| MAP_PRIVATE MAP_ANONYMOUS) -1 0))
    (if (u>= addr 0xfffff001) (do
        (eputs "mmap2 failed\n")
        (exit 1)
        ))
    (return addr)
    )

(fun init_heap ()
    (var mb 1024)  ; 1GB
    (= heap_root (map_zero (* mb 1048576)))
    (= heap_pos (align heap_root))
    (= heap_end (+ heap_root (* mb 1048576)))
    (= gc_bitmap (map_zero (+ (>> (* mb 1048576) 6) 4096)))
    (= gc_stack (map_zero (* 4 GC_STACK_WORDS)))
    (= gc_sp gc_stack)
    (= gc_stack_end (+ gc_stack (* 4 GC_STACK_WORDS)))
    (= gc_lists (map_zero 4096))
    (= gc_limit GC_MIN_BYTES)
    )

(fun bm_set (a)
    (var g (>> (- a heap_root) 3))
    (var i (>> g 3))
    (setb gc_bitmap i (| (getb gc_bitmap i) (<< 1 (& g 7))))
    )

(fun bm_clear (a)
    (var g (>> (- a heap_root) 3))
    (var i (>> g 3))
    (setb gc_bitmap i (& (getb gc_bitmap i) (^ 255 (<< 1 (& g 7)))))
    )

; put the free chunk c of n bytes (n >= 8) to its list
(fun free_chunk (c n)
    (var idx (>> n 3))
    (if (u> idx 32) (= idx 0))
    (set c (| n GC_FREE))
    (set c 1 (get gc_lists idx))
    (set gc_lists idx c)
    )

; take a chunk of n bytes from the free chunks or from the end of the heap (0 if there is none)
(fun take_chunk (n)
    (var idx (>> n 3))
    (var c 0)
    (var prev 0)
    (var size 0)
    (var rest 0)
    (if (u<= idx 32) (do
        (= c (get gc_lists idx))
        (if c (do
            (set gc_lists idx (get c 1))
            (return c)
            ))
        ))
    (= c (get gc_lists 0))
    (while c (do
        (= size (& (get c) 0xfffffff8))
        (if (u>= size n) (do
            (if prev (set prev 1 (get c 1)) (set gc_lists 0 (get c 1)))
            (= rest (- size n))
            (if rest (free_chunk (+ c n) rest))
            (return c)
            ))
        (= prev c)
        (= c (get c 1))
        ))
    (if (u> (+ heap_pos n) heap_end) (return 0))
    (= c heap_pos)
    (+= heap_pos n)
    (return c)
    )

; Memory is allocated by a first fit on the free chunks. A new object is zero-filled.
(fun try_allocate (size)
    (var n (align (+ size 4)))
    (var c 0)
    (var p 0)
    (var e 0)
    (if (u>= (+ gc_since n) gc_limit) (gc_collect))
    (+= gc_since n)
    (= c (take_chunk n))
    (if (! c) (do
        (gc_collect)
        (= c (take_chunk n))
        (if (! c) (return 0))
        ))
    (set c n)
    (bm_set c)
    (= p (+ c 4))
    (= e (+ c n))
    (while (u< p e) (do
        (set p 0)
        (+= p 4)
        ))
    (return (+ c 4))
    )

(fun allocate (size)
    (var p (try_allocate size))
    (if (! p) (do
        (eputs "memory allocation error\n")
        (exit 1)
        ))
    (return p)
    )

; === data layout (planck0)
; Small integers: tagged with LSB=1. Value is encoded as (val << 1) | 1.
; Heap objects: Identified by LSB=0.
; These objects are preceded by a one-word header containing object-specific information.

; Layout of heap objects.
; m: 1 if the object is mutable
; a: 1 if all bytes of the string are ASCII (set when the string is made)
;
;         |        header        |
;         |  27bit | 1bit | 4bit |
; symbol  |      0 |    0 | 0000 | text |
; string  | length |    a | 0001 | text |
; mexpr   |  arity |    0 | 0010 | sym  | arg1 | .... |
; array   | length |    m | 0011 |
; lambda  |  arity |    0 | 0101 | code | env  |
; prim    |  arity |    0 | 0110 | ptr  | name | kind |
; cont    |      0 |    0 | 0111 | top frame | boundary frame |
; fusion  |      0 |    0 | 1000 | fun1 | fun2 |
; code    |      0 |    0 | 1010 | (see Bytecode VM)
; type    | fields |    0 | 1011 | name | Tuple of the field names |
; data    | fields |    0 | 1100 | type | field1 | .... |
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
(def PArrayT   0x4)
(def LambdaT  0x5)
(def PrimT    0x6)
(def ContT    0x7)
(def FusionT  0x8)
(def IntT     0x9)
(def CodeT    0xa)
(def TypeT    0xb)
(def DataT    0xc)
(def BigT     0xd)

(fun fprint_tag (chan tag)
    (if (== tag SymbolT) (fputs chan "Symbol")
    (if (== tag StringT) (fputs chan "String")
    (if (== tag MexprT) (fputs chan "Mexpr")
    (if (== tag ArrayT) (fputs chan "Array")
    (if (== tag PArrayT) (fputs chan "PArray")
    (if (== tag LambdaT) (fputs chan "Lambda")
    (if (== tag PrimT) (fputs chan "Prim")
    (if (== tag ContT) (fputs chan "Cont")
    (if (== tag FusionT) (fputs chan "Fusion")
    (if (== tag IntT) (fputs chan "Int")
    (if (== tag TypeT) (fputs chan "Type")
    (if (== tag DataT) (fputs chan "Data")
        ))))))))))))
    )

(fun gettag (node)
    (if (& node 1) (return IntT))
    (return (& (get node 0) 0xf))
    )

(fun expect (name tag val)
    (if (!= (gettag val) tag) (do
        (eputs name)
        (eputs ": ")
        (fprint_tag STDERR tag)
        (eputs " is expected: ")
        (eprint val)
        (eputs "\n")
        (exit 1)
        ))
    )

(fun expect_mexpr (name head val)
    (expect name MexprT val)
    (if (!= (get val 1) head) (do
        (eputs name)
        (eputs ": M-expr with head ")
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

(fun ascii_text (p n)
    (while (> n 0) (do
        (if (>= (getb p) 128) (return 0))
        (+= p 1)
        (-= n 1)
        ))
    (return 1)
    )

(fun str (text)
    (var data (allocate 8))
    (set data 0 (make_header StringT (ascii_text text (strlen text)) (strlen text)))
    (set data 1 text)
    (return data)
    )

(fun str_text (str)
    (expect "str_text" StringT str)
    (return (get str 1))
    )

; hash and equality of the strings with a length (the text may hold \0)
(fun strhash_n (str n)
    (var hash 5381)
    (while (> n 0) (do
        (= hash (+ (+ (<< hash 5) hash) (getb str)))
        (+= str 1)
        (-= n 1)
        ))
    (return hash)
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
    (expect "sym_name" SymbolT sym)
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

(fun mexpr3 (h a b c)
    (char[] 20 tmpobj3)
    (set tmpobj3 0 (make_header MexprT 0 3))
    (set tmpobj3 1 h)
    (set tmpobj3 2 a)
    (set tmpobj3 3 b)
    (set tmpobj3 4 c)
    (return (mexpr tmpobj3))
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

(fun bytes_equal (pa pb n)
    (while (> n 0) (do
        (if (!= (getb pa) (getb pb)) (return 0))
        (+= pa 1)
        (+= pb 1)
        (-= n 1)
        ))
    (return 1)
    )

(fun arrayhash (a)
    (var h 5381)
    (var n (get_header_arg a))
    (var i 1)
    (while (<= i n) (do
        (= h (+ (+ (<< h 5) h) (hash (get a i))))
        (+= i 1)
        ))
    (return h)
    )

(fun parrayhash (a)
    (return (+ (* (strhash_n (parray_data a) (* (get_header_arg a) (parray_size (get a 1)))) 31) (get a 1)))
    )

(fun hash (e)
    (var t (gettag e))
    (var h 0)
    (if (== t IntT) (do
        ; the bucket is taken from the low bits, so mix the high bits into them
        (= h (* (fixnum_to_int e) 536870909))
        (return (^ h (>> h 15)))
        ))
    (if (== t BigT) (return (big_hash e)))
    (if (== t SymbolT) (return (symhash e)))
    (if (== t StringT) (return (strhash_n (get e 1) (get_header_arg e))))
    (if (== t MexprT) (return (mexprhash e)))
    (if (== t ArrayT) (return (arrayhash e)))
    (if (== t PArrayT) (return (parrayhash e)))
    (if (== t DataT) (return (+ (* (hash (get e 2)) 31) (symhash (get e 1)))))
    ; other objects are hashed by their addresses
    (return (symhash e))
    )

(fun fusion (f1 f2)
    (var obj (allocate 12))
    (if (! (is_function f1)) (named_error "Fusion" "function is expected: " f1))
    (if (! (is_function f2)) (named_error "Fusion" "function is expected: " f2))
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
(long TypeSlotS)
(long _RestS)
(long IntT_)
(long StringT_)
(long SymbolT_)
(long ExprT_)
(long FunctionT_)
(long ArrayT_)
(long PArrayT_)
(long TypeT_)
(long DefS)
(long DefFunS)
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
(long OkS)
(long ErrS)
(long IoErrorS)
(long SyntaxErrorS)
(long EofS)
(long evalS)
(long emptyTuple)

(fun tup2 (a b) (return (mexpr2 TupleS a b)))

(fun tup_get (e i)
    (expect_mexpr "tup_get" TupleS e)
    (var arity (get_header_arg e))
    (if (>= i arity) (not_reachable "tup_get"))
    (return (get e (+ 2 i)))
    )

(long global_table) ; global variables (sym -> cell of the latest definition)

(fun define_global (sym)
    (var cell (allocate 8))
    (set cell 0 0)
    (set cell 1 sym)
    (if (! (table_update global_table sym cell))
        (table_insert global_table sym cell))
    (return cell)
    )

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
    (= TypeSlotS (sym "_TypeSlot"))
    (= _RestS   (sym "_Rest"))
    (= DefS         (sym "Def"))
    (= DefFunS      (sym "DefFun"))
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
    (= OkS          (sym "Ok"))
    (= ErrS         (sym "Err"))
    (= IoErrorS     (sym "IoError"))
    (= SyntaxErrorS (sym "SyntaxError"))
    (= EofS         (sym "Eof"))
    (= evalS        (sym "eval"))
    (= emptyTuple   (mexpr0 TupleS))
    )

(fun str_values_equal (a b)
    (var n (get_header_arg a))
    (var pa (get a 1))
    (var pb (get b 1))
    (if (!= n (get_header_arg b)) (return 0))
    (while (> n 0) (do
        (if (!= (getb pa) (getb pb)) (return 0))
        (+= pa 1)
        (+= pb 1)
        (-= n 1)
        ))
    (return 1)
    )

; Interned m-exprs without strings are equal iff they are identical.
; Strings are not interned, so m-exprs containing strings are compared
; structurally.
(fun values_equal (a b)
    (var i 0)
    (var n 0)
    (if (== a b) (return 1))
    (if (&& (== (gettag a) StringT) (== (gettag b) StringT))
        (return (str_values_equal a b)))
    (if (&& (== (gettag a) BigT) (== (gettag b) BigT)) (do
        (if (== (num_cmp a b) 0) (return 1))
        (return 0)
        ))
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
    (if (&& (== (gettag a) ArrayT) (== (gettag b) ArrayT)) (do
        (= n (get_header_arg a))
        (if (!= (get_header_arg b) n) (return 0))
        (while (< i n) (do
            (if (! (values_equal (get a (+ i 1)) (get b (+ i 1)))) (return 0))
            (+= i 1)
            ))
        (return 1)
        ))
    (if (&& (== (gettag a) PArrayT) (== (gettag b) PArrayT)) (do
        (if (!= (get a 1) (get b 1)) (return 0))
        (if (!= (get_header_arg a) (get_header_arg b)) (return 0))
        (= n (* (get_header_arg a) (parray_size (get a 1))))
        (return (bytes_equal (parray_data a) (parray_data b) n))
        ))
    (if (&& (== (gettag a) DataT) (== (gettag b) DataT)) (do
        (if (!= (get a 1) (get b 1)) (return 0))
        (return (values_equal (get a 2) (get b 2)))
        ))
    (return 0)
    )

(fun typeof (v)
    (var t (gettag v))
    (if (== t IntT) (return IntT_))
    (if (== t BigT) (return IntT_))
    (if (== t StringT) (return StringT_))
    (if (== t SymbolT) (return SymbolT_))
    (if (== t MexprT) (return ExprT_))
    (if (is_function v) (return FunctionT_))
    (if (== t ArrayT) (return ArrayT_))
    (if (== t PArrayT) (return PArrayT_))
    (if (== t DataT) (return (get v 1)))
    (if (== t TypeT) (return TypeT_))
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
    (var n (str_len str))
    (= str (str_text str))
    (fputs chan "\"")
    (while (> n 0) (do
        (-= n 1)
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

;  the pattern p with each _TypeSlot{slot} replaced by the type in the slot of env, the environment where the function was made
(fun restore_types (p env)
    (var i 0)
    (var n 0)
    (var vals 0)
    (if (!= (gettag p) MexprT) (return p))
    (if (== (get p 1) QuoteS) (return p))
    (if (== (get p 1) TypeSlotS) (return (get env (+ (fixnum_to_int (get p 2)) 1))))
    (= n (get_header_arg p))
    (= vals (allocate (* 4 (+ n 1))))
    (while (< i n) (do
        (set vals i (restore_types (get p (+ i 2)) env))
        (+= i 1)
        ))
    (return (mexprn (get p 1) n vals))
    )

(fun fprint_lambda (chan e)
    (var pats (get (get e 1) 2)) ; patterns of the code object
    (var i 0)
    (var arity (get_header_arg e))
    (fputs chan "<lambda:(")
    (while (< i arity) (do
        (fprint chan (restore_types (get pats (+ 2 i)) (get e 2)))
        (+= i 1)
        (if (< i arity) (fputs chan ", "))
        ))
    (fputs chan ")>")
    )

; <u8[n]>, <u16[n]> or <i32[n]>
(fun fprint_parray (chan e)
    (var kind (get e 1))
    (fputs chan "<")
    (if (== kind 0) (fputs chan "u8")
    (if (== kind 1) (fputs chan "u16")
        (fputs chan "i32")))
    (fputs chan "[")
    (fputi chan (get_header_arg e))
    (fputs chan "]>")
    )

(fun fprint (chan e)
    (var t (gettag e))
    (if (== t IntT) (fputi chan (fixnum_to_int e))
    (if (== t BigT) (big_print chan e)
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
    (if (== t PArrayT) (fprint_parray chan e)
    (if (== t DataT) (do
        (fputs chan "#<")
        (fprint chan (get (get e 1) 1))
        (fputs chan ">")
        )
    (if (== t TypeT) (fprint chan (get e 1))
    (if (== t FusionT) (do
        (fprint chan (get e 1))
        (fputs chan "|")
        (fprint chan (get e 2))
        )
        (fputs chan "<?>")
        )))))))))))))
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
    ; a-z, A-Z, _, %
    (if (== c (char "%")) (return 1))
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

(fun is_hex_digit (c)
    (if (&& (<= (char "0") c) (<= c (char "9"))) (return 1))
    (if (&& (<= (char "a") c) (<= c (char "f"))) (return 1))
    (if (&& (<= (char "A") c) (<= c (char "F"))) (return 1))
    (return 0)
    )

; limit is the largest value of the digits: 2^30 - 1, or 2^30 after a minus sign
(fun parse_int (ret raw limit)
    (var start raw)
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
        (if (> value (/ (- limit d) base)) (do
            (while (is_hex_digit (getb raw)) (+= raw 1))
            (set ret (parse_big (str_n (strndup start (- raw start)) (- raw start)) 0))
            (return raw)
            ))
        (= value (+ (* value base) d))
        (+= raw 1)
        ))
    (not_reachable "parse_int")
    )

(def PARSE_STR_MAX 65536)
(long parse_str_buf)
(fun parse_str (ret raw)
    (var end 0)
    (var c 0)
    (if (! parse_str_buf) (= parse_str_buf (allocate PARSE_STR_MAX)))
    (= end parse_str_buf)
    (+= raw 1)  ; skip '"'
    (while (getb raw) (do
        (= c (getb raw))
        (if (>= (- end parse_str_buf) (- PARSE_STR_MAX 1)) (do
            (set ret (syntax_error "string literal is too long"))
            (return raw)
            ))
        (if (== c (char "\"")) (do
            (+= raw 1)
            (set ret (str_n (strndup parse_str_buf (- end parse_str_buf)) (- end parse_str_buf)))
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


; The arguments of the M-exprs being read are kept in parse_arena: a frame of
; PARSE_FRAME bytes (a header, the head and MAX_ARITY arguments) for each nesting level.
(def PARSE_FRAME 264)
(def PARSE_DEPTH 4096)
(long parse_arena)
(long parse_sp)

(fun parse_mexpr (ret head raw)
    (var buf parse_sp)
    (var r 0)
    (if (! parse_arena) (do
        (= parse_arena (allocate (* PARSE_FRAME PARSE_DEPTH)))
        (= buf parse_arena)
        ))
    (if (u>= (+ buf PARSE_FRAME) (+ parse_arena (* PARSE_FRAME PARSE_DEPTH))) (do
        (set ret (syntax_error "M-exprs are nested too deep"))
        (return (+ raw 1))
        ))
    (= parse_sp (+ buf PARSE_FRAME))
    (= r (parse_mexpr_in ret head raw buf))
    (= parse_sp buf)
    (return r)
    )

(fun parse_mexpr_in (ret head raw mexpr_buf)
    (char[] 4 tmp)
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
        (if (>= arity MAX_ARITY) (do
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
        (= raw (parse_int ret raw (- (<< 1 30) 1)))
    (if (&& (== c (char "-")) (is_digit (getb raw 1))) (do
        ; negative integer literal
        (= raw (parse_int ret (+ raw 1) (<< 1 30)))
        (if (== (gettag (get ret)) IntT)
            (set ret (- 2 (get ret))) ; fixnum(-n) = 2 - fixnum(n)
            )
        (if (== (gettag (get ret)) BigT)
            (set ret (num_sub (fixnum 0) (get ret)))
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
    (expect "mexpr_parse" StringT text)
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
;   Variables defined by top-level Def are global variables. A global variable
;   is a cell (| value | sym |). The compiler finds the cell of a name in
;   global_table, which holds the latest definition at that time, and puts it
;   in the instruction. Def makes a new cell, so the code compiled before it
;   keeps the old one. A name which is neither bound lexically nor defined at
;   the top level is an error at compile time.
;
; frame (internal, placed on the frame stack):
;   | caller | code | pc | env | sp | handler | size | operand stack ... |
;   caller:  the frame which receives the return value (0 for top-level)
;   pc:      address of the next instruction (saved when not running)
;   sp:      byte offset of the stack top from the frame (saved)
;   handler: effect handler if this frame is the boundary of Handle, or 0
;   size:    size of the frame in words
;
; The frame stack is a 1GB area of its own. A frame is placed just above its
; caller, so the free area starts at the end of the current frame, and a
; frame is dropped by returning to its caller. TCALL places the callee where
; the dropped caller was. When the frame stack is full, the VM stops with
; "stack overflow".
;
; Since frames are not on the machine stack, the VM never calls itself
; recursively and the control state of a computation is just the chain
; of frames from the current frame to the top-level frame.
;
; Instructions (operands follow the opcode):
;   CONST v         push v
;   LOAD d i sym    push slot i of the d-th parent environment
;   STORE d i       store the stack top to slot i of the d-th parent env
;   GLOAD cell      push the value of the global variable cell
;   GDEF cell       define the global variable cell with the stack top
;   GSET cell       update the global variable cell with the stack top
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
;   k is a continuation object (ContT) holding a copy in the heap of the
;   frames from the current frame to B.
;   If the arguments do not match the handler, the next outer handler is
;   tried (effect forwarding).
;   Calling k with a value x copies the frames of k onto the frame stack
;   (B keeps its handler), links the copy of B to the caller of k, pushes x
;   as the result of Perform and runs the copied frames. Since the frames of
;   k are never executed themselves, k can be resumed multiple times.
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

(def PRIM_NATIVE 0)
(def PRIM_EVAL   1)
(def PRIM_NEW    2)
(def PRIM_UNWRAP 3)

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

;  the cell of the global variable sym, or stop if there is none
(fun global_cell (sym)
    (var cell (table_lookup global_table sym))
    (if (! cell) (do
        (eputs "undefined variable: ")
        (eprint sym)
        (eputs "\n")
        (exit 1)
        ))
    (return cell)
    )

(fun compile_var (sym)
    (var ent (cenv_lookup sym))
    (var cell 0)
    (if ent
        (do
            (emit OP_LOAD)
            (emit (- (get cctx CX_LEVEL) (get ent 1)))
            (emit (get ent 2))
            (emit sym)
            )
        (do
            (= cell (global_cell sym))
            (emit OP_GLOAD)
            (emit cell)
            ))
    (stack_effect 1)
    )

; allocate slots for variables in a pattern, in the order of appearance
(long param_base)

(fun check_param_unique (sym)
    (var ent cenv)
    (while (!= ent param_base) (do
        (if (== (get ent 0) sym) (compile_error "not implemented: duplicate parameter name" sym))
        (= ent (get ent 3))
        ))
    )

(fun bind_pattern (p)
    (var t (gettag p))
    (var i 0)
    (var n 0)
    (if (== p _S) (return 0))
    (if (== t SymbolT) (do
        (check_param_unique p)
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
        (if (== (get p 1) _RestS) (compile_error "a rest pattern x... must be the last argument" p))
        (= n (get_header_arg p))
        (while (< i n) (do
            (bind_pattern_arg (get p (+ i 2)) (+ i 1) n)
            (+= i 1)
            ))
        (return 0)
        ))
    (if (|| (== t IntT) (|| (== t BigT) (== t StringT))) (return 0))
    (compile_error "invalid pattern" p)
    )

; true when p is a rest pattern _Rest{x}, which takes the remaining arguments
(fun is_rest_pat (p)
    (if (!= (gettag p) MexprT) (return 0))
    (if (== (get p 1) _RestS) (return 1))
    (return 0)
    )

; bind the pattern of the argument number pos (counted from 1) of n. A rest pattern is allowed only as the last one.
(fun bind_pattern_arg (p pos n)
    (if (is_rest_pat p) (do
        (if (|| (!= pos n) (!= (get_header_arg p) 1)) (compile_error "a rest pattern x... must be the last argument" p))
        (bind_pattern (get p 2))
        (return 0)
        ))
    (bind_pattern p)
    )

; compile Lambda{pats, body} to a code object
(fun compile_lambda (pats body)
    (var saved_env cenv)
    (var saved_pos cbuf_pos)
    (var i 0)
    (var n (get_header_arg pats))
    (var code 0)
    (= cctx (new_context 0))
    (= param_base cenv)
    (while (< i n) (do
        (bind_pattern_arg (get pats (+ i 2)) (+ i 1) n)
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

; define x with the stack top where the code is
(fun compile_define (x)
    (var slot 0)
    (if (is_global_scope)
        (do
            (emit OP_GDEF)
            (emit (define_global x))
            )
        (do
            (= slot (cenv_add x))
            (emit OP_STORE)
            (emit 0)
            (emit slot)
            ))
    )

; whether x is a variable of the place where the code defines variables
(fun defined_here (x)
    (var ent 0)
    (if (is_global_scope) (do
        (if (table_lookup global_table x) (return 1))
        (return 0)
        ))
    (= ent (cenv_lookup x))
    (if (&& ent (== (get ent 1) (get cctx CX_LEVEL))) (return 1))
    (return 0)
    )

; whether the pattern p has _TypeOf{q, t}
(fun has_type_pattern (p)
    (var i 0)
    (var n 0)
    (if (!= (gettag p) MexprT) (return 0))
    (if (== (get p 1) QuoteS) (return 0))
    (= n (get_header_arg p))
    (if (&& (== (get p 1) _TypeOfS) (== n 2)) (return 1))
    (while (< i n) (do
        (if (has_type_pattern (get p (+ i 2))) (return 1))
        (+= i 1)
        ))
    (return 0)
    )

; The type t of _TypeOf{q, t} in a pattern of a function is an expression, which is evaluated where the function is made. This emits the code that
; evaluates t and stores it in a new slot of the environment, and gives p with t replaced by _TypeSlot{slot}.
(fun hoist_types (p)
    (var i 0)
    (var n 0)
    (var slot 0)
    (var vals 0)
    (var q 0)
    (if (! (has_type_pattern p)) (return p))
    (= n (get_header_arg p))
    (= vals (allocate (* 4 (+ n 1))))
    (while (< i n) (do
        (set vals i (hoist_types (get p (+ i 2))))
        (+= i 1)
        ))
    (if (&& (== (get p 1) _TypeOfS) (== n 2)) (do
        (compile (get p 3) 0)
        (= slot (get cctx CX_NSLOTS))
        (set cctx CX_NSLOTS (+ slot 1))
        (emit OP_STORE)
        (emit 0)
        (emit slot)
        (emit OP_POP)
        (stack_effect -1)
        (set vals 1 (mexpr1 TypeSlotS (fixnum slot)))
        ))
    (return (mexprn (get p 1) n vals))
    )

(fun compile_mexpr (e tail)
    (var head (get e 1))
    (var arity (get_header_arg e))
    (var i 0)
    (var n 0)
    (var x 0)
    (var ent 0)
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
        ; The right-hand side refers to the previous variable of the same
        ; name. The new variable is made after it is compiled.
        (compile (get e 3) 0)
        (compile_define (get e 2))
        (return 0)
        ))
    (if (== head DefFunS) (do
        (if (|| (!= arity 2) (!= (gettag (get e 2)) SymbolT)) (malformed e))
        (if (! (defined_here (get e 2))) (do
            (emit_const noneS)
            (compile_define (get e 2))
            (emit OP_POP)
            (stack_effect -1)
            ))
        (compile (mexpr2 SetS (get e 2) (mexpr2 FusionS (get e 2) (get e 3))) 0)
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
                (emit (global_cell x))
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
        (if (&& (!= arity 2) (!= arity 3)) (malformed e))
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
        (if (== arity 2) (emit_const noneS) (compile (get e 4) tail))
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
        (= args (hoist_types (get e 2)))
        (emit OP_CLOSURE)
        (emit (compile_lambda args (get e 3)))
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
    (if (== head _RestS) (compile_error "x... is allowed only as the last parameter of a function without a guard, or as the last argument of a pattern" e))
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
    (if (|| (== t IntT) (|| (== t BigT) (== t StringT))) (do
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
(char[] 128 code_data_gap)

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

(long frame_base)   ; bottom of the frame stack
(long frame_end)    ; end of the frame stack

(fun init_frame_stack ()
    (var stack_size 1024)  ; 1GB
    (*= stack_size 1024)
    (*= stack_size 1024)
    (var addr (mmap2 0 stack_size
        (| PROT_READ PROT_WRITE)
        (| MAP_PRIVATE MAP_ANONYMOUS)
        -1 0))
    (if (u>= addr 0xfffff001) (do
        (eputs "mmap2 failed\n")
        (exit 1)
        ))
    (= frame_base (align addr))
    (= frame_end (+ addr stack_size))
    )

; address just above the frame f, where the frame called by f is placed
(fun frame_top (f)
    (if (! f) (return frame_base))
    (return (+ f (* 4 (get f F_SIZE))))
    )

(fun check_frame_stack (top)
    (if (u>= top frame_end) (do
        (eputs "stack overflow\n")
        (exit 1)
        ))
    )

(fun new_frame (code env caller handler)
    (var size (+ F_STACK (+ (get code C_MAXSTACK) 2)))
    (var f (frame_top caller))
    (check_frame_stack (+ f (* 4 size)))
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

(fun named_error (name msg e)
    (eputs name)
    (eputs ": ")
    (runtime_error msg e)
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
    (var tp 0)
    (if (== p _S) (return 1)) ; wildcard
    (if (== t SymbolT) (do ; variable pattern
        (set env (+ 1 (get idx)) v)
        (set idx (+ (get idx) 1))
        (return 1)
        ))
    (if (|| (== t IntT) (|| (== t BigT) (== t StringT))) (return (values_equal p v)))
    (if (== t MexprT) (do
        (= h (get p 1))
        (if (== h QuoteS) (return (values_equal (get p 2) v)))
        (if (== h _TypeOfS) (do
            (= tp (get p 3))
            ; the type is in a slot of the environment where the function was made
            (if (&& (== (gettag tp) MexprT) (== (get tp 1) TypeSlotS))
                (= tp (get (get env 0) (+ (fixnum_to_int (get tp 2)) 1))))
            (if (!= (typeof v) tp) (return 0))
            (return (match_pat (get p 2) v env idx))
            ))
        (if (!= (gettag v) MexprT) (return 0))
        (if (!= (get v 1) h) (return 0))
        (= n (get_header_arg p))
        (if (&& (> n 0) (is_rest_pat (get p (+ n 1)))) (do
            ; Head{p1, ..., rest...}: the arguments after the fixed ones are a Tuple
            (-= n 1)
            (if (< (get_header_arg v) n) (return 0))
            (while (< i n) (do
                (if (! (match_pat (get p (+ i 2)) (get v (+ i 2)) env idx)) (return 0))
                (+= i 1)
                ))
            (return (match_pat (get (get p (+ n 2)) 2)
                (mexprn TupleS (- (get_header_arg v) n) (+ v (* 4 (+ n 2)))) env idx))
            ))
        (if (!= (get_header_arg v) n) (return 0))
        (while (< i n) (do
            (if (! (match_pat (get p (+ i 2)) (get v (+ i 2)) env idx)) (return 0))
            (+= i 1)
            ))
        (return 1)
        ))
    (return 0)
    )

; pats is the Tuple of the parameter patterns of a function. When the last one
; is a rest pattern _Rest{x}, x is matched with the Tuple of the remaining arguments.
(fun match_args (pats args nargs env)
    (char[] 4 idx)
    (var i 0)
    (var n (get_header_arg pats))
    (set idx 0 0)
    (if (&& (> n 0) (is_rest_pat (get pats (+ n 1)))) (do
        (-= n 1)
        (if (< nargs n) (return 0))
        (while (< i n) (do
            (if (! (match_pat (get pats (+ i 2)) (get args i) env idx)) (return 0))
            (+= i 1)
            ))
        (return (match_pat (get (get pats (+ n 2)) 2)
            (mexprn TupleS (- nargs n) (+ args (* 4 n))) env idx))
        ))
    (if (!= n nargs) (return 0))
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
        (= r (get code C_ARITY))
        (if (!= r nargs) (do
            ; only a function whose last parameter is a rest pattern takes other numbers of arguments
            (if (! (&& (> r 0) (is_rest_pat (get (get code C_PATS) (+ r 1))))) (return 0))
            ))
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
; The frames of k are copied onto the frame stack just above caller.
(fun resume (k x caller)
    (var top (get k 1))
    (var b (get k 2))
    (var f top)
    (var total (get top F_SIZE))
    (while (!= f b) (do
        (= f (get f F_CALLER))
        (+= total (get f F_SIZE))
        ))
    (var dst (+ (frame_top caller) (* 4 total)))
    (check_frame_stack dst)
    (= f top)
    (-= dst (* 4 (get f F_SIZE)))
    (memcpy dst f (* 4 (get f F_SIZE)))
    (var first dst)
    (var last dst)
    (while (!= f b) (do
        (= f (get f F_CALLER))
        (-= dst (* 4 (get f F_SIZE)))
        (memcpy dst f (* 4 (get f F_SIZE)))
        (set last F_CALLER dst)
        (= last dst)
        ))
    (set last F_CALLER caller)
    (load_regs first)
    (vm_push x)
    )

(fun new_data (type value)
    (var d (allocate 12))
    (set d 0 (make_header DataT 0 1))
    (set d 1 type)
    (set d 2 value)
    (return d)
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
        (if (== (get callee 3) PRIM_NEW) (do
            (= v (new_data (get callee 2) (get args 0)))
            (deliver caller v)
            (return 0)
            ))
        (if (== (get callee 3) PRIM_UNWRAP) (do
            (= v (get args 0))
            (if (|| (!= (gettag v) DataT) (!= (get v 1) (get callee 2)))
                (runtime_error "unwrap: not a value of the type: " v))
            (deliver caller (get v 2))
            (return 0)
            ))
        (= fn (get callee 1))
        (if (== nargs 0) (= v (fn))
        (if (== nargs 1) (= v (fn (get args 0)))
        (if (== nargs 2) (= v (fn (get args 0) (get args 1)))
        (if (== nargs 3) (= v (fn (get args 0) (get args 1) (get args 2)))
        (if (== nargs 4) (= v (fn (get args 0) (get args 1) (get args 2) (get args 3)))
        (if (== nargs 5) (= v (fn (get args 0) (get args 1) (get args 2) (get args 3) (get args 4)))
            (not_reachable "invoke")
            ))))))
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
    ; none | g is g, so that the first f(p) = e after fun f is the function itself.
    (if (== f noneS) (vm_push g) (vm_push (fusion f g)))
    )

(fun op_handle ()
    (var handler (vm_pop))
    (var body (vm_pop))
    (var code (get body 1))
    (if (! (is_function handler)) (named_error "Handle" "function is expected for the handler: " handler))
    (save_regs)
    (enter code (new_env code (get body 2)) vm_frame handler)
    )

; copy the frames from top to the boundary b into the heap and make a
; continuation object of the copy.
(fun make_cont (top b)
    (var k (allocate 12))
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
    (set last F_CALLER 0)
    (set k 0 (make_header ContT 0 0))
    (set k 1 first)
    (set k 2 last)
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

(fun run_toplevel (code)
    (var op 0)
    (var d 0)
    (var i 0)
    (var s 0)
    (var env 0)
    (var v 0)
    (var cell 0)
    (load_regs (new_frame code (new_env code 0) 0 0))
    (while vm_frame (do
        (= op (get vm_pc))
        (+= vm_pc 4)
        (switch op
            (OP_CONST (do
                (set vm_sp (get vm_pc))
                (+= vm_pc 4)
                (+= vm_sp 4)
                ))
            (OP_LOAD (do
                (= d (get vm_pc 0))
                (= i (get vm_pc 1))
                (= s (get vm_pc 2))
                (+= vm_pc 12)
                (= env vm_env)
                (while (> d 0) (do
                    (= env (get env 0))
                    (-= d 1)
                    ))
                (= v (get env (+ i 1)))
                (if (! v) (runtime_error "variable used before definition: " s))
                (set vm_sp v)
                (+= vm_sp 4)
                ))
            (OP_STORE (do
                (= d (get vm_pc 0))
                (= i (get vm_pc 1))
                (+= vm_pc 8)
                (= env vm_env)
                (while (> d 0) (do
                    (= env (get env 0))
                    (-= d 1)
                    ))
                (set env (+ i 1) (get (- vm_sp 4)))
                ))
            (OP_GLOAD (do
                (= cell (get vm_pc))
                (+= vm_pc 4)
                (= v (get cell 0))
                (if (! v) (runtime_error "undefined variable: " (get cell 1)))
                (set vm_sp v)
                (+= vm_sp 4)
                ))
            (OP_GDEF (do
                (= cell (get vm_pc))
                (+= vm_pc 4)
                (set cell 0 (get (- vm_sp 4)))
                ))
            (OP_GSET (do
                (= cell (get vm_pc))
                (+= vm_pc 4)
                (set cell 0 (get (- vm_sp 4)))
                ))
            (OP_POP (-= vm_sp 4))
            (OP_JMP (do
                (= d (get vm_pc))
                (= vm_pc (+ vm_code (* 4 (+ C_CODE d))))
                ))
            (OP_JF (do
                (= d (get vm_pc))
                (+= vm_pc 4)
                (-= vm_sp 4)
                (if (== (get vm_sp) falseS)
                    (= vm_pc (+ vm_code (* 4 (+ C_CODE d)))))
                ))
            (OP_CALL (op_call))
            (OP_TCALL (op_tcall))
            (OP_RET (op_ret))
            (OP_CLOSURE (op_closure))
            (OP_TUPLE (op_tuple))
            (OP_MEXPR (op_mexpr))
            (OP_FUSION (op_fusion))
            (OP_HANDLE (op_handle))
            (OP_PERFORM (op_perform))
            (default (do
                (eputs "invalid opcode\n")
                (exit 1)
                ))
            )
        ))
    (return vm_result)
    )

; === Primitive Functions

(fun bool (b)
    (if b (return trueS))
    (return falseS)
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

(fun p_put (s)
    (expect "os_put" StringT s)
    (write STDOUT (str_text s) (str_len s))
    (return noneS)
    )

(fun fits_fixnum (x)
    (if (&& (>= x (- 0 (<< 1 30))) (<= x (- (<< 1 30) 1))) (return 1))
    (return 0)
    )

(def BIG_BASE 32768)
(def BIG_MASK 32767)

(fun is_num (v)
    (if (& v 1) (return 1))
    (if (== (gettag v) BigT) (return 1))
    (return 0)
    )

(fun check_num (name v)
    (if (! (is_num v)) (do
        (eputs name)
        (eputs ": integer is expected: ")
        (eprint v)
        (eputs "\n")
        (exit 1)
        ))
    )

(fun index_arg (name v)
    (check_num name v)
    (if (& v 1) (return (fixnum_to_int v)))
    (if (big_neg v) (return (- 0 1)))
    (return 0x7fffffff)
    )

(fun ranged_arg (name v max)
    (var k (index_arg name v))
    (if (|| (< k 0) (> k max)) (named_error name "out of range: " v))
    (return k)
    )

(fun big_try_new (n neg)
    (var b 0)
    (if (> n 0x3ffffff) (return 0))
    (= b (try_allocate (* 4 (+ n 1))))
    (if (! b) (return 0))
    (set b 0 (make_header BigT 0 (| (<< n 1) neg)))
    (return b)
    )

(fun big_new (n neg)
    (var b (allocate (* 4 (+ n 1))))
    (set b 0 (make_header BigT 0 (| (<< n 1) neg)))
    (return b)
    )

(fun big_len (b) (return (>> (get_header_arg b) 1)))

(fun big_neg (b) (return (& (get_header_arg b) 1)))

(fun big_ulen (b)
    (var n (big_len b))
    (while (> n 0) (do
        (if (get b n) (return n))
        (-= n 1)
        ))
    (return 0)
    )

(fun big_from_fix (v)
    (var x (fixnum_to_int v))
    (var neg 0)
    (var b 0)
    (if (< x 0) (do
        (= neg 1)
        (= x (- x))
        ))
    (= b (big_new 3 neg))
    (set b 1 (& x BIG_MASK))
    (set b 2 (& (>> x 15) BIG_MASK))
    (set b 3 (>> x 30))
    (return b)
    )

(fun big_of (v)
    (if (& v 1) (return (big_from_fix v)))
    (return v)
    )

(fun big_norm (b neg)
    (var n (big_ulen b))
    (var v 0)
    (var d2 0)
    (var limit 0)
    (if (<= n 3) (do
        (if (>= n 1) (= v (get b 1)))
        (if (>= n 2) (= v (| v (<< (get b 2) 15))))
        (if (>= n 3) (= d2 (get b 3)))
        (if (<= d2 1) (do
            (= v (| v (<< d2 30)))
            (= limit (+ (- (<< 1 30) 1) neg))
            (if (<= v limit) (do
                (if neg (return (fixnum (- v))))
                (return (fixnum v))
                ))
            ))
        ))
    (set b 0 (make_header BigT 0 (| (<< n 1) neg)))
    (return b)
    )

(fun mag_cmp (a b)
    (var la (big_ulen a))
    (var lb (big_ulen b))
    (var i la)
    (if (< la lb) (return -1))
    (if (> la lb) (return 1))
    (while (> i 0) (do
        (if (< (get a i) (get b i)) (return -1))
        (if (> (get a i) (get b i)) (return 1))
        (-= i 1)
        ))
    (return 0)
    )

(fun mag_add (a b)
    (var la (big_ulen a))
    (var lb (big_ulen b))
    (var n (+ la 1))
    (var r 0)
    (var i 1)
    (var t 0)
    (var carry 0)
    (if (> lb la) (= n (+ lb 1)))
    (= r (big_new n 0))
    (while (<= i n) (do
        (= t carry)
        (if (<= i la) (+= t (get a i)))
        (if (<= i lb) (+= t (get b i)))
        (set r i (& t BIG_MASK))
        (= carry (>> t 15))
        (+= i 1)
        ))
    (return r)
    )

(fun mag_sub (a b)
    (var la (big_ulen a))
    (var lb (big_ulen b))
    (var r (big_new la 0))
    (var i 1)
    (var t 0)
    (var borrow 0)
    (while (<= i la) (do
        (= t (- (get a i) borrow))
        (if (<= i lb) (-= t (get b i)))
        (if (< t 0) (do
            (+= t BIG_BASE)
            (= borrow 1)
            )
            (= borrow 0)
            )
        (set r i t)
        (+= i 1)
        ))
    (return r)
    )

(fun mag_mul (a b)
    (var la (big_ulen a))
    (var lb (big_ulen b))
    (var r (big_new (+ la lb) 0))
    (var i 1)
    (var j 1)
    (var t 0)
    (var carry 0)
    (while (<= i la) (do
        (= carry 0)
        (= j 1)
        (while (<= j lb) (do
            (= t (+ (+ (get r (- (+ i j) 1)) (* (get a i) (get b j))) carry))
            (set r (- (+ i j) 1) (& t BIG_MASK))
            (= carry (>> t 15))
            (+= j 1)
            ))
        (set r (+ i lb) carry)
        (+= i 1)
        ))
    (return r)
    )

(fun mag_divsmall (a dv q)
    (var i (big_ulen a))
    (var rem 0)
    (var cur 0)
    (while (> i 0) (do
        (= cur (| (<< rem 15) (get a i)))
        (set q i (/ cur dv))
        (= rem (% cur dv))
        (-= i 1)
        ))
    (return rem)
    )

(fun mag_scale (a d n r)
    (var i 1)
    (var carry 0)
    (var t 0)
    (while (<= i n) (do
        (= t (+ (* (get a i) d) carry))
        (set r i (& t BIG_MASK))
        (= carry (>> t 15))
        (+= i 1)
        ))
    (set r (+ n 1) carry)
    )

(fun mag_divmod (a b q r)
    (var la (big_ulen a))
    (var lb (big_ulen b))
    (var d 0)
    (var un 0)
    (var vn 0)
    (var j 0)
    (var i 0)
    (var top 0)
    (var vt 0)
    (var vs 0)
    (var qhat 0)
    (var rhat 0)
    (var go 0)
    (var borrow 0)
    (var carry 0)
    (var p 0)
    (var t 0)
    (var rem 0)
    (if (< la lb) (do
        (= i 1)
        (while (<= i la) (do
            (set r i (get a i))
            (+= i 1)
            ))
        (return 0)
        ))
    (if (== lb 1) (do
        (set r 1 (mag_divsmall a (get b 1) q))
        (return 0)
        ))
    (= d (/ BIG_BASE (+ (get b lb) 1)))
    (= un (big_new (+ la 1) 0))
    (= vn (big_new (+ lb 1) 0))
    (mag_scale a d la un)
    (mag_scale b d lb vn)
    (= vt (get vn lb))
    (= vs (get vn (- lb 1)))
    (= j (- la lb))
    (while (>= j 0) (do
        (= top (| (<< (get un (+ (+ j lb) 1)) 15) (get un (+ j lb))))
        (= qhat (/ top vt))
        (= rhat (% top vt))
        (= go 1)
        (while go (do
            (= go 0)
            (if (|| (>= qhat BIG_BASE)
                    (> (* qhat vs) (+ (<< rhat 15) (get un (- (+ j lb) 1))))) (do
                (-= qhat 1)
                (+= rhat vt)
                (if (< rhat BIG_BASE) (= go 1))
                ))
            ))
        (= borrow 0)
        (= carry 0)
        (= i 0)
        (while (< i lb) (do
            (= p (+ (* qhat (get vn (+ i 1))) carry))
            (= carry (>> p 15))
            (= t (- (- (get un (+ (+ i j) 1)) (& p BIG_MASK)) borrow))
            (if (< t 0) (do
                (+= t BIG_BASE)
                (= borrow 1)
                )
                (= borrow 0)
                )
            (set un (+ (+ i j) 1) t)
            (+= i 1)
            ))
        (= t (- (- (get un (+ (+ j lb) 1)) carry) borrow))
        (if (< t 0) (do
            (+= t BIG_BASE)
            (= borrow 1)
            )
            (= borrow 0)
            )
        (set un (+ (+ j lb) 1) t)
        (if borrow (do
            (-= qhat 1)
            (= carry 0)
            (= i 0)
            (while (< i lb) (do
                (= t (+ (+ (get un (+ (+ i j) 1)) (get vn (+ i 1))) carry))
                (set un (+ (+ i j) 1) (& t BIG_MASK))
                (= carry (>> t 15))
                (+= i 1)
                ))
            (set un (+ (+ j lb) 1) (& (+ (get un (+ (+ j lb) 1)) carry) BIG_MASK))
            ))
        (set q (+ j 1) qhat)
        (-= j 1)
        ))
    (= i lb)
    (= rem 0)
    (while (> i 0) (do
        (= t (| (<< rem 15) (get un i)))
        (set r i (/ t d))
        (= rem (% t d))
        (-= i 1)
        ))
    (return 0)
    )

(fun big_add_signed (a b flip)
    (var sa (big_neg a))
    (var sb (^ (big_neg b) flip))
    (var c 0)
    (if (== sa sb) (return (big_norm (mag_add a b) sa)))
    (= c (mag_cmp a b))
    (if (== c 0) (return (fixnum 0)))
    (if (> c 0) (return (big_norm (mag_sub a b) sa)))
    (return (big_norm (mag_sub b a) sb))
    )

(fun num_add (a b) (return (big_add_signed (big_of a) (big_of b) 0)))

(fun num_sub (a b) (return (big_add_signed (big_of a) (big_of b) 1)))

(fun num_mul (a b)
    (var x (big_of a))
    (var y (big_of b))
    (return (big_norm (mag_mul x y) (^ (big_neg x) (big_neg y))))
    )

(fun num_div (a b mode)
    (var x (big_of a))
    (var y (big_of b))
    (var q (big_new (+ (big_len x) 1) 0))
    (var r (big_new (+ (big_len y) 1) 0))
    (var sx (big_neg x))
    (var sy (big_neg y))
    (var rzero 0)
    (mag_divmod x y q r)
    (if (== (big_ulen r) 0) (= rzero 1))
    (if (== mode 0) (return (big_norm q (^ sx sy))))
    (if (== mode 1) (do
        (if (|| rzero (== sx sy)) (return (big_norm q (^ sx sy))))
        (return (big_norm (mag_add q (big_from_fix (fixnum 1))) 1))
        ))
    (if rzero (return (fixnum 0)))
    (if (== sx sy) (return (big_norm r sx)))
    (return (big_norm (mag_sub y r) sy))
    )

(fun num_cmp (a b)
    (var x 0)
    (var y 0)
    (var c 0)
    (if (&& (& a 1) (& b 1)) (do
        (if (< a b) (return -1))
        (if (> a b) (return 1))
        (return 0)
        ))
    (= x (big_of a))
    (= y (big_of b))
    (if (!= (big_neg x) (big_neg y)) (do
        (if (big_neg x) (return -1))
        (return 1)
        ))
    (= c (mag_cmp x y))
    (if (big_neg x) (return (- 0 c)))
    (return c)
    )

(fun big_hash (b)
    (var h (+ (big_neg b) 17))
    (var i (big_ulen b))
    (while (> i 0) (do
        (= h (+ (* h 31) (get b i)))
        (-= i 1)
        ))
    (return h)
    )

(fun big_to_str (b)
    (var n (big_ulen b))
    (var t (big_new n 0))
    (var buf (allocate (+ (* n 5) 8)))
    (var end (+ buf (+ (* n 5) 4)))
    (var pos end)
    (var rem 0)
    (var i 1)
    (var k 0)
    (while (<= i n) (do
        (set t i (get b i))
        (+= i 1)
        ))
    (while (> (big_ulen t) 0) (do
        (= rem (mag_divsmall t 10000 t))
        (= k 0)
        (while (< k 4) (do
            (-= pos 1)
            (setb pos (+ (char "0") (% rem 10)))
            (/= rem 10)
            (+= k 1)
            ))
        ))
    (while (== (getb pos) (char "0")) (+= pos 1))
    (if (== pos end) (do
        (-= pos 1)
        (setb pos (char "0"))
        ))
    (if (big_neg b) (do
        (-= pos 1)
        (setb pos (char "-"))
        ))
    (setb end 0)
    (return (str_n pos (- end pos)))
    )

(fun big_print (chan b)
    (var s (big_to_str b))
    (write chan (get s 1) (str_len s))
    )

(fun big_to_str_base (b base)
    (var n (big_ulen b))
    (var t (big_new n 0))
    (var buf (allocate (+ (* n 15) 8)))
    (var end (+ buf (+ (* n 15) 4)))
    (var pos end)
    (var rem 0)
    (var i 1)
    (while (<= i n) (do
        (set t i (get b i))
        (+= i 1)
        ))
    (while (> (big_ulen t) 0) (do
        (= rem (mag_divsmall t base t))
        (-= pos 1)
        (if (< rem 10) (setb pos (+ (char "0") rem)) (setb pos (+ (char "a") (- rem 10))))
        ))
    (if (== pos end) (do
        (-= pos 1)
        (setb pos (char "0"))
        ))
    (if (big_neg b) (do
        (-= pos 1)
        (setb pos (char "-"))
        ))
    (setb end 0)
    (return (str_n pos (- end pos)))
    )

(fun p_int_to_str (n base)
    (var k 0)
    (check_num "int_to_str" n)
    (= k (index_arg "int_to_str" base))
    (if (|| (< k 2) (> k 36)) (runtime_error "int_to_str: base out of range: " base))
    (if (== k 10) (return (big_to_str (big_of n))))
    (return (big_to_str_base (big_of n) k))
    )

(fun p_add (a b)
    (var r 0)
    (check_num "int_add" a)
    (check_num "int_add" b)
    (if (&& (& a 1) (& b 1)) (do
        (= r (+ a (- b 1)))
        (if (>= (& (^ a r) (^ (- b 1) r)) 0) (return r))
        ))
    (return (num_add a b))
    )

(fun p_sub (a b)
    (var d 0)
    (check_num "int_sub" a)
    (check_num "int_sub" b)
    (if (&& (& a 1) (& b 1)) (do
        (= d (- a b))
        (if (>= (& (^ a b) (^ a d)) 0) (return (+ d 1)))
        ))
    (return (num_sub a b))
    )

(fun p_mul (a b)
    (var x 0)
    (var y 0)
    (var p 0)
    (check_num "int_mul" a)
    (check_num "int_mul" b)
    (if (&& (& a 1) (& b 1)) (do
        (= x (fixnum_to_int a))
        (= y (fixnum_to_int b))
        (= p (* x y))
        (if (== x 0) (return (fixnum 0)))
        (if (== (sdiv p x) y) (do
            (if (fits_fixnum p) (return (fixnum p)))
            ))
        ))
    (return (num_mul a b))
    )

(fun parse_big (text neg)
    (var n (str_len text))
    (var s (get text 1))
    (var i 0)
    (var base 10)
    (var d 0)
    (var c 0)
    (var r 0)
    (var j 0)
    (var carry 0)
    (var t 0)
    (if (== (getb s 0) (char "-")) (do
        (= neg 1)
        (= i 1)
        ))
    (if (== (getb s 0) (char "+")) (= i 1))
    (if (== (getb s i) (char "0")) (do
        (= c (getb s (+ i 1)))
        (if (|| (== c (char "x")) (== c (char "X"))) (= base 16))
        (if (|| (== c (char "o")) (== c (char "O"))) (= base 8))
        (if (|| (== c (char "b")) (== c (char "B"))) (= base 2))
        (if (!= base 10) (+= i 2))
        ))
    (if (>= i n) (runtime_error "str_to_int: invalid integer: " text))
    (= r (big_new (+ n 1) 0))
    (while (< i n) (do
        (= c (getb s i))
        (= d (- 0 1))
        (if (&& (>= c (char "0")) (<= c (char "9"))) (= d (- c (char "0"))))
        (if (&& (>= c (char "a")) (<= c (char "f"))) (= d (- c (- (char "a") 10))))
        (if (&& (>= c (char "A")) (<= c (char "F"))) (= d (- c (- (char "A") 10))))
        (if (|| (< d 0) (>= d base)) (runtime_error "str_to_int: invalid integer: " text))
        (= carry d)
        (= j 1)
        (while (<= j (+ n 1)) (do
            (= t (+ (* (get r j) base) carry))
            (set r j (& t BIG_MASK))
            (= carry (>> t 15))
            (+= j 1)
            ))
        (+= i 1)
        ))
    (return (big_norm r neg))
    )

(fun p_bigint (text)
    (expect "str_to_int" StringT text)
    (return (parse_big text 0))
    )

(fun p_div (a b)
    (var q 0)
    (check_num "int_div" a)
    (check_num "int_div" b)
    (if (== b 1) (runtime_error "division by zero: " a))
    (if (&& (& a 1) (& b 1)) (do
        (= q (sdiv (fixnum_to_int a) (fixnum_to_int b)))
        (if (fits_fixnum q) (return (fixnum q)))
        ))
    (return (num_div a b 0))
    )

(fun fdiv (a b)
    (var q 0)
    (= q (sdiv a b))
    (if (!= (- a (* b q)) 0) (do
        (if (< a 0) (if (>= b 0) (= q (- q 1))))
        (if (>= a 0) (if (< b 0) (= q (- q 1))))
        ))
    (return q)
    )

(fun p_quo (a b)
    (var q 0)
    (check_num "int_quo" a)
    (check_num "int_quo" b)
    (if (== b 1) (runtime_error "division by zero: " a))
    (if (&& (& a 1) (& b 1)) (do
        (= q (fdiv (fixnum_to_int a) (fixnum_to_int b)))
        (if (fits_fixnum q) (return (fixnum q)))
        ))
    (return (num_div a b 1))
    )

(fun p_mod (a b)
    (var x 0)
    (var y 0)
    (check_num "int_mod" a)
    (check_num "int_mod" b)
    (if (== b 1) (runtime_error "division by zero: " a))
    (if (&& (& a 1) (& b 1)) (do
        (= x (fixnum_to_int a))
        (= y (fixnum_to_int b))
        (return (fixnum (- x (* y (fdiv x y)))))
        ))
    (return (num_div a b 2))
    )

(fun p_lt (a b)
    (check_num "int_lt" a)
    (check_num "int_lt" b)
    (if (< (num_cmp a b) 0) (return trueS))
    (return falseS)
    )

(fun p_le (a b)
    (check_num "int_le" a)
    (check_num "int_le" b)
    (if (<= (num_cmp a b) 0) (return trueS))
    (return falseS)
    )

(fun p_gt (a b)
    (check_num "int_gt" a)
    (check_num "int_gt" b)
    (if (> (num_cmp a b) 0) (return trueS))
    (return falseS)
    )

(fun p_ge (a b)
    (check_num "int_ge" a)
    (check_num "int_ge" b)
    (if (>= (num_cmp a b) 0) (return trueS))
    (return falseS)
    )

(fun p_eq (a b) (return (bool (values_equal a b))))

(fun p_identical (a b)
    (if (== a b) (return trueS))
    (if (&& (== (gettag a) BigT) (== (gettag b) BigT)) (do
        (if (== (num_cmp a b) 0) (return trueS))
        ))
    (return falseS)
    )

(fun p_ne (a b)
    (if (values_equal a b) (return falseS))
    (return trueS)
    )

(fun p_head (m)
    (expect "mexpr_head" MexprT m)
    (return (get m 1))
    )

(fun p_arity (m)
    (expect "mexpr_arity" MexprT m)
    (return (fixnum (get_header_arg m)))
    )

(fun p_arg (m i)
    (var n 0)
    (expect "mexpr_arg" MexprT m)
    (= n (index_arg "mexpr_arg" i))
    (if (|| (< n 0) (>= n (get_header_arg m))) (runtime_error "expr_arg: index out of range: " i))
    (return (get m (+ n 2)))
    )

(fun p_mexpr (h args)
    (expect "mexpr_new" SymbolT h)
    (expect_mexpr "mexpr_new" TupleS args)
    (return (mexprn h (get_header_arg args) (+ args 8)))
    )

(fun p_typeof (v) (return (typeof v)))

(fun p_type_name (t)
    (expect "type_name" TypeT t)
    (return (get t 1))
    )

(fun p_make_type (name)
    (var type 0)
    (var wrap (allocate 16))
    (var unwrap (allocate 16))
    (expect "make_type" SymbolT name)
    (= type (new_type name))
    (set wrap 0 (make_header PrimT 0 1))
    (set wrap 1 0)
    (set wrap 2 type)
    (set wrap 3 PRIM_NEW)
    (set unwrap 0 (make_header PrimT 0 1))
    (set unwrap 1 0)
    (set unwrap 2 type)
    (set unwrap 3 PRIM_UNWRAP)
    (return (mexpr3 TupleS type wrap unwrap))
    )

; --- strings, symbols, tables and files (used by planck/peg.pk)

; a string object for text of length n (text is not copied)
(fun str_n (text n)
    (var data (allocate 8))
    (set data 0 (make_header StringT (ascii_text text n) n))
    (set data 1 text)
    (return data)
    )

(fun str_n_ascii (text n)
    (var data (allocate 8))
    (set data 0 (make_header StringT 1 n))
    (set data 1 text)
    (return data)
    )

(fun str_len (s) (return (get_header_arg s)))

(fun ascii_only (name s)
    (if (! (& (get s 0) 16)) (do
        (eputs name)
        (eputs ": not implemented for a string with multibyte characters: ")
        (eprint s)
        (eputs "\n")
        (exit 1)
        ))
    )

(fun p_strlen (s)
    (expect "str_len" StringT s)
    (ascii_only "str_len" s)
    (return (fixnum (str_len s)))
    )

; str_ord(s, i): the byte at i
(fun p_ord (s i)
    (var n 0)
    (expect "str_ord" StringT s)
    (ascii_only "str_ord" s)
    (= n (index_arg "str_ord" i))
    (if (|| (< n 0) (>= n (str_len s))) (named_error "str_ord" "index out of range: " i))
    (return (fixnum (getb (get s 1) n)))
    )

; str_sub(s, i, j): bytes from i to j-1. The text is shared when j is the end.
(fun p_substr (s i j)
    (var len 0)
    (var oi i)
    (var oj j)
    (expect "str_sub" StringT s)
    (ascii_only "str_sub" s)
    (= i (index_arg "str_sub" i))
    (= j (index_arg "str_sub" j))
    (= len (str_len s))
    (if (|| (< i 0) (|| (> j len) (> i j))) (do
        (runtime_error "str_sub: index out of range: " (mexpr3 TupleS s oi oj))
        ))
    (if (== j len) (return (str_n_ascii (+ (get s 1) i) (- j i))))
    (return (str_n_ascii (strndup (+ (get s 1) i) (- j i)) (- j i)))
    )

(fun p_strcat (a b)
    (var n 0)
    (var m 0)
    (var text 0)
    (expect "str_cat" StringT a)
    (expect "str_cat" StringT b)
    (= n (str_len a))
    (= m (str_len b))
    (= text (allocate (+ (+ n m) 1)))
    (memcpy text (get a 1) n)
    (memcpy (+ text n) (get b 1) m)
    (setb text (+ n m) 0)
    (return (str_n text (+ n m)))
    )

(fun expect_no_null (name s)
    (var p (get s 1))
    (var n (str_len s))
    (while (> n 0) (do
        (if (== (getb p) 0) (named_error name "string contains NULL: " s))
        (+= p 1)
        (-= n 1)
        ))
    )

(fun p_intern (s)
    (expect "sym_intern" StringT s)
    (expect_no_null "sym_intern" s)
    (return (sym (strndup (get s 1) (str_len s))))
    )

; sym_name(s): the name of the symbol s as a string (the inverse of sym_intern)
(fun p_symname (s)
    (return (str (sym_name s)))
    )

(fun p_chr (c)
    (var n (index_arg "str_chr" c))
    (var text 0)
    (if (|| (< n 0) (> n 0x10ffff)) (runtime_error "str_chr: out of range: " c))
    (if (> n 127) (runtime_error "str_chr: not implemented for a multibyte character: " c))
    (= text (allocate 2))
    (setb text 0 n)
    (setb text 1 0)
    (return (str_n text 1))
    )

; array_new(n): a mutable array of n elements, all none
(fun p_array (n)
    (var a 0)
    (var i 0)
    (= i (index_arg "array_new" n))
    (if (|| (< i 0) (> i 0x7ffffff)) (runtime_error "array_new: bad length: " n))
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
    (expect name ArrayT a)
    (= n (index_arg name i))
    (if (|| (< n 0) (>= n (get_header_arg a)))
        (named_error name "index out of range: " i))
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
    (return noneS)
    )

(fun p_array_len (a)
    (expect "array_len" ArrayT a)
    (return (fixnum (get_header_arg a)))
    )

(fun p_array_copy (dst di src si n)
    (var d 0)
    (var s 0)
    (var cnt 0)
    (var i 0)
    (expect "array_copy" ArrayT dst)
    (expect "array_copy" ArrayT src)
    (= d (index_arg "array_copy" di))
    (= s (index_arg "array_copy" si))
    (= cnt (index_arg "array_copy" n))
    (if (|| (< cnt 0) (|| (< d 0) (|| (< s 0)
        (|| (> d (get_header_arg dst)) (|| (> s (get_header_arg src))
        (|| (> cnt (- (get_header_arg dst) d)) (> cnt (- (get_header_arg src) s))))))))
        (runtime_error "array_copy: index out of range: " n))
    (if (&& (== dst src) (> d s)) (do
        (= i cnt)
        (while (> i 0) (do
            (= i (- i 1))
            (set dst (+ (+ d i) 1) (get src (+ (+ s i) 1)))
            ))
        (return noneS)
        ))
    (while (< i cnt) (do
        (set dst (+ (+ d i) 1) (get src (+ (+ s i) 1)))
        (= i (+ i 1))
        ))
    (return noneS)
    )

; hash(x): a non-negative integer that is equal for values equal by equal
(fun p_hash (x)
    (return (fixnum (& (hash x) 0xfffffff)))
    )

(fun io_ok (v) (return (mexpr1 OkS v)))

(fun io_err (stage path) (return (mexpr1 ErrS (mexpr2 IoErrorS (sym stage) path))))

(fun p_read_file (path)
    (var fd 0)
    (var size 0)
    (var buf 0)
    (var r 0)
    (expect "os_read_file" StringT path)
    (expect_no_null "os_read_file" path)
    (= fd (open (get path 1) O_RDONLY))
    (if (< fd 0) (return (io_err "open" path)))
    (= size (fsize fd))
    (if (< size 0) (do
        (close fd)
        (return (io_err "fstat" path))
        ))
    (= buf (allocate (+ size 1))) ; +1 for \0
    (= r (read fd buf size))
    (close fd)
    (if (< r size) (return (io_err "read" path)))
    (setb buf size 0)
    (return (io_ok (str_n buf size)))
    )

; --- parray: arrays of unboxed u8, u16 and i32 elements
; layout: | header (length, mutable, PArrayT) | kind | elements |
; kind: 0 = u8, 1 = u16, 2 = i32. The elements are zero at first.

(fun parray_size (kind)
    (if (== kind 0) (return 1))
    (if (== kind 1) (return 2))
    (return 4)
    )

(fun parray_data (a) (return (+ a 8)))

(fun parray_new (name kind n)
    (var a 0)
    (var i 0)
    (= i (index_arg name n))
    (if (|| (< i 0) (> i 0x7ffffff)) (runtime_error "bad parray length: " n))
    (= a (allocate (+ 8 (* i (parray_size kind)))))
    (set a 0 (make_header PArrayT 1 i))
    (set a 1 kind)
    (return a)
    )

(fun p_u8_array (n) (return (parray_new "parray_new_u8" 0 n)))
(fun p_u16_array (n) (return (parray_new "parray_new_u16" 1 n)))
(fun p_i32_array (n) (return (parray_new "parray_new_i32" 2 n)))

(fun parray_index (name a i)
    (var n 0)
    (expect name PArrayT a)
    (= n (index_arg name i))
    (if (|| (< n 0) (>= n (get_header_arg a)))
        (named_error name "index out of range: " i))
    (return n)
    )

(fun p_parray_len (a)
    (expect "parray_len" PArrayT a)
    (return (fixnum (get_header_arg a)))
    )

(fun p_parray_kind (a)
    (var kind 0)
    (expect "parray_kind" PArrayT a)
    (= kind (get a 1))
    (if (== kind 0) (return (sym "u8")))
    (if (== kind 1) (return (sym "u16")))
    (return (sym "i32"))
    )

(fun num_from_i32 (x)
    (var neg 0)
    (var b 0)
    (if (fits_fixnum x) (return (fixnum x)))
    (if (< x 0) (do
        (= neg 1)
        (= x (- 0 x))
        ))
    (= b (big_new 3 neg))
    (set b 1 (& x BIG_MASK))
    (set b 2 (& (>> x 15) BIG_MASK))
    (set b 3 (>> x 30))
    (return b)
    )

(fun i32_arg (v)
    (var mag 0)
    (check_num "parray_set" v)
    (if (& v 1) (return (fixnum_to_int v)))
    (= mag (| (| (get v 1) (<< (get v 2) 15)) (<< (get v 3) 30)))
    (if (== (big_ulen v) 3) (do
        (if (== (get v 3) 1) (do
            (if (big_neg v) (return (- 0 mag)))
            (return mag)
            ))
        (if (&& (&& (== (get v 3) 2) (big_neg v)) (== (| (get v 1) (get v 2)) 0))
            (return mag))
        ))
    (runtime_error "parray_set: not an i32: " v)
    (return 0)
    )

(fun p_parray_get (a i)
    (var n 0)
    (var kind 0)
    (var d 0)
    (= n (parray_index "parray_get" a i))
    (= kind (get a 1))
    (= d (parray_data a))
    (if (== kind 0) (return (fixnum (getb d n))))
    (if (== kind 1) (return (fixnum (| (getb d (* 2 n)) (<< (getb d (+ (* 2 n) 1)) 8)))))
    (return (num_from_i32 (get d n)))
    )

(fun p_parray_set (a i v)
    (var n 0)
    (var kind 0)
    (var d 0)
    (var x 0)
    (= n (parray_index "parray_set" a i))
    (= kind (get a 1))
    (= d (parray_data a))
    (if (== kind 2) (= x (i32_arg v))
        (= x (index_arg "parray_set" v)))
    (if (== kind 0) (do
        (if (|| (< x 0) (> x 255)) (runtime_error "parray_set: not a u8: " v))
        (setb d n x)
        (return noneS)
        ))
    (if (== kind 1) (do
        (if (|| (< x 0) (> x 65535)) (runtime_error "parray_set: not a u16: " v))
        (setb d (* 2 n) (& x 255))
        (setb d (+ (* 2 n) 1) (>> x 8))
        (return noneS)
        ))
    (set d n x)
    (return noneS)
    )

; parray_copy(dst, di, src, si, n): copy n elements of src from si to dst from di
(fun p_parray_copy (dst di src si n)
    (var size 0)
    (var d 0)
    (var s 0)
    (var cnt 0)
    (var tmp 0)
    (expect "parray_copy" PArrayT dst)
    (expect "parray_copy" PArrayT src)
    (if (!= (get dst 1) (get src 1)) (runtime_error "parray_copy: kinds differ: " src))
    (= d (index_arg "parray_copy" di))
    (= s (index_arg "parray_copy" si))
    (= cnt (index_arg "parray_copy" n))
    (if (|| (< cnt 0) (|| (< d 0) (|| (< s 0)
        (|| (> (+ d cnt) (get_header_arg dst)) (> (+ s cnt) (get_header_arg src))))))
        (runtime_error "parray_copy: index out of range: " n))
    (= size (parray_size (get dst 1)))
    ; an overlapping copy in one array goes through a temporary copy
    (if (== dst src) (do
        (= tmp (allocate (+ (* cnt size) 1)))
        (memcpy tmp (+ (parray_data src) (* s size)) (* cnt size))
        (memcpy (+ (parray_data dst) (* d size)) tmp (* cnt size))
        (return noneS)
        ))
    (memcpy (+ (parray_data dst) (* d size)) (+ (parray_data src) (* s size)) (* cnt size))
    (return noneS)
    )

; --- files

(def SYS_CHMOD 15)

(fun write_all (path p n)
    (var fd 0)
    (var r 0)
    (= fd (open (get path 1) (| O_WRONLY (| O_CREAT O_TRUNC))))
    (if (< fd 0) (return (io_err "open" path)))
    (while (> n 0) (do
        (= r (write fd p n))
        (if (<= r 0) (do
            (close fd)
            (return (io_err "write" path))
            ))
        (+= p r)
        (-= n r)
        ))
    (close fd)
    (return (io_ok noneS))
    )

(fun p_write_file (path x)
    (expect "os_write_file" StringT path)
    (expect_no_null "os_write_file" path)
    (expect "os_write_file" StringT x)
    (return (write_all path (get x 1) (str_len x)))
    )

(fun p_write_bytes (path x)
    (expect "os_write_bytes" StringT path)
    (expect_no_null "os_write_bytes" path)
    (expect "os_write_bytes" PArrayT x)
    (if (!= (get x 1) 0) (runtime_error "os_write_bytes: u8 array is expected: " x))
    (return (write_all path (parray_data x) (get_header_arg x)))
    )

; os_read_bytes(path): the contents of the file as a u8 array
(fun p_read_bytes (path)
    (var fd 0)
    (var size 0)
    (var a 0)
    (var r 0)
    (var m 0)
    (expect "os_read_bytes" StringT path)
    (expect_no_null "os_read_bytes" path)
    (= fd (open (get path 1) O_RDONLY))
    (if (< fd 0) (return (io_err "open" path)))
    (= size (fsize fd))
    (if (< size 0) (do
        (close fd)
        (return (io_err "fstat" path))
        ))
    (= a (allocate (+ 8 size)))
    (set a 0 (make_header PArrayT 1 size))
    (set a 1 0)
    (while (< r size) (do
        (= m (read fd (+ (parray_data a) r) (- size r)))
        (if (<= m 0) (do
            (close fd)
            (return (io_err "read" path))
            ))
        (+= r m)
        ))
    (close fd)
    (return (io_ok a))
    )

(fun p_chmod (path mode)
    (expect "os_chmod" StringT path)
    (expect_no_null "os_chmod" path)
    (if (< (syscall SYS_CHMOD (get path 1) (ranged_arg "os_chmod" mode 0xfff)) 0)
        (return (io_err "chmod" path)))
    (return (io_ok noneS))
    )

; --- bit operations on integers (31-bit, two's complement)

(fun check_shift (name k)
    (check_num name k)
    (if (& k 1) (do
        (if (< (fixnum_to_int k) 0)
            (runtime_error "shift count must not be negative: " k))
        (return k)
        ))
    (if (big_neg k) (runtime_error "shift count must not be negative: " k))
    (return (fixnum 0x3fffffff))
    )

(fun big_tc (b n)
    (var r (big_new n 0))
    (var u (big_ulen b))
    (var i 1)
    (var t 0)
    (var carry 1)
    (while (<= i u) (do
        (set r i (get b i))
        (+= i 1)
        ))
    (if (big_neg b) (do
        (= i 1)
        (while (<= i n) (do
            (= t (+ (^ (get r i) BIG_MASK) carry))
            (set r i (& t BIG_MASK))
            (= carry (>> t 15))
            (+= i 1)
            ))
        ))
    (return r)
    )

(fun big_from_tc (r n)
    (var i 1)
    (var t 0)
    (var carry 1)
    (if (& (get r n) 16384) (do
        (while (<= i n) (do
            (= t (+ (^ (get r i) BIG_MASK) carry))
            (set r i (& t BIG_MASK))
            (= carry (>> t 15))
            (+= i 1)
            ))
        (return (big_norm r 1))
        ))
    (return (big_norm r 0))
    )

(fun big_bitop (op a b)
    (var x (big_of a))
    (var y (big_of b))
    (var n (+ (big_ulen x) 1))
    (var tx 0)
    (var ty 0)
    (var i 1)
    (if (> (big_ulen y) (big_ulen x)) (= n (+ (big_ulen y) 1)))
    (= tx (big_tc x n))
    (= ty (big_tc y n))
    (while (<= i n) (do
        (if (== op 0) (set tx i (& (get tx i) (get ty i))))
        (if (== op 1) (set tx i (| (get tx i) (get ty i))))
        (if (== op 2) (set tx i (^ (get tx i) (get ty i))))
        (+= i 1)
        ))
    (return (big_from_tc tx n))
    )

(fun mag_shl (b k)
    (var n (big_ulen b))
    (var s (/ k 15))
    (var r (% k 15))
    (var out (big_try_new (+ (+ n s) 1) 0))
    (var i 1)
    (var t 0)
    (if (! out) (return 0))
    (while (<= i n) (do
        (= t (<< (get b i) r))
        (set out (+ i s) (| (get out (+ i s)) (& t BIG_MASK)))
        (set out (+ (+ i s) 1) (>> t 15))
        (+= i 1)
        ))
    (return out)
    )

(fun mag_shr (b k)
    (var n (big_ulen b))
    (var s (/ k 15))
    (var r (% k 15))
    (var m (- n s))
    (var out 0)
    (var i 1)
    (var t 0)
    (if (< m 1) (= m 1))
    (= out (big_new m 0))
    (while (<= i m) (do
        (= t (>> (get b (+ i s)) r))
        (if (< (+ i s) n)
            (= t (| t (& (<< (get b (+ (+ i s) 1)) (- 15 r)) BIG_MASK))))
        (set out i (& t BIG_MASK))
        (+= i 1)
        ))
    (return out)
    )

(fun mag_lost (b k)
    (var n (big_ulen b))
    (var s (/ k 15))
    (var r (% k 15))
    (var i 1)
    (while (&& (<= i s) (<= i n)) (do
        (if (get b i) (return 1))
        (+= i 1)
        ))
    (if (&& (< s n) (& (get b (+ s 1)) (- (<< 1 r) 1))) (return 1))
    (return 0)
    )

(fun p_bit_and (a b)
    (check_num "bit_and" a)
    (check_num "bit_and" b)
    (if (&& (& a 1) (& b 1)) (return (& a b)))
    (return (big_bitop 0 a b))
    )

(fun p_bit_or (a b)
    (check_num "bit_or" a)
    (check_num "bit_or" b)
    (if (&& (& a 1) (& b 1)) (return (| a b)))
    (return (big_bitop 1 a b))
    )

(fun p_bit_xor (a b)
    (check_num "bit_xor" a)
    (check_num "bit_xor" b)
    (if (&& (& a 1) (& b 1)) (return (| (^ a b) 1)))
    (return (big_bitop 2 a b))
    )

(fun p_bit_not (a)
    (check_num "bit_not" a)
    (if (& a 1) (return (^ a 0xfffffffe)))
    (return (big_bitop 2 a (fixnum -1)))
    )

; the result must stay within 31 bits
(fun p_bit_shl (a k)
    (var x 0)
    (var r 0)
    (var y 0)
    (check_num "bit_shl" a)
    (= k (check_shift "bit_shl" k))
    (if (&& (& a 1) (<= (fixnum_to_int k) 31)) (do
        (= x (fixnum_to_int a))
        (= r (<< x (fixnum_to_int k)))
        (if (&& (== (asr r (fixnum_to_int k)) x) (fits_fixnum r)) (return (fixnum r)))
        ))
    (= y (big_of a))
    (= r (mag_shl y (fixnum_to_int k)))
    (if (! r) (named_error "bit_shl" "out of memory: " k))
    (return (big_norm r (big_neg y)))
    )

; arithmetic shift: the sign is kept
(fun p_bit_sar (a k)
    (var y 0)
    (var r 0)
    (check_num "bit_sar" a)
    (= k (check_shift "bit_sar" k))
    (if (& a 1) (do
        (if (> (fixnum_to_int k) 31) (return (fixnum (asr (fixnum_to_int a) 31))))
        (return (fixnum (asr (fixnum_to_int a) (fixnum_to_int k))))
        ))
    (= r (mag_shr a (fixnum_to_int k)))
    (if (&& (big_neg a) (mag_lost a (fixnum_to_int k)))
        (= r (mag_add r (big_from_fix (fixnum 1)))))
    (return (big_norm r (big_neg a)))
    )

; --- command-line arguments: argv() is the array of the strings on the command line after planck0;
; the first is the program file, the rest are its arguments
(long cmd_argc)
(long cmd_argv)


(def SYS_FORK 2)
(def SYS_CHDIR 12)
(def SYS_EXECVE 11)
(def SYS_WAIT4 114)
(def SYS_DUP2 63)
(def SYS_POLL 168)
(def SYS_PIPE2 331)
(def SYS_SIGNAL 48)
(def O_CLOEXEC 0x80000)

(fun spawn_room (a len)
    (var cap 0)
    (var b 0)
    (if a (= cap (get_header_arg a)))
    (if (&& a (>= (- cap len) 4096)) (return a))
    (= cap (* 2 cap))
    (if (< cap 8192) (= cap 8192))
    (= b (allocate (+ 8 cap)))
    (set b 0 (make_header PArrayT 1 cap))
    (set b 1 0)
    (if a (memcpy (parray_data b) (parray_data a) len))
    (return b)
    )

(fun spawn_text (a len)
    (var data (allocate (+ len 1)))
    (if (> len 0) (memcpy data (parray_data a) len))
    (setb data len 0)
    (return (str_n data len))
    )

(fun spawn_pipe (p flags)
    (if (< (syscall SYS_PIPE2 p flags) 0) (runtime_error "os_run: cannot create a pipe" noneS))
    )

(fun p_run_program (path args input dir)
    (var n 0)
    (var i 0)
    (var argvp 0)
    (var envp 0)
    (var pid 0)
    (var r 0)
    (var ob 0)
    (var olen 0)
    (var eb 0)
    (var elen 0)
    (var in_pos 0)
    (var in_len 0)
    (var fd0 0)
    (var fd1 0)
    (var fd2 0)
    (var chunk 0)
    (var st 0)
    (var res 0)
    (var rv 0)
    (char[] 8 inp)
    (char[] 8 outp)
    (char[] 8 errp)
    (char[] 8 failp)
    (char[] 24 pf)
    (char[] 4 status)
    (char[] 4 one)
    (expect "os_run" StringT path)
    (expect_no_null "os_run" path)
    (expect "os_run" ArrayT args)
    (expect "os_run" StringT input)
    (expect "os_run" StringT dir)
    (expect_no_null "os_run" dir)
    (= n (get_header_arg args))
    (= argvp (allocate (* 4 (+ n 2))))
    (set argvp 0 (get path 1))
    (while (< i n) (do
        (expect "os_run" StringT (get args (+ i 1)))
        (expect_no_null "os_run" (get args (+ i 1)))
        (set argvp (+ i 1) (get (get args (+ i 1)) 1))
        (+= i 1)
        ))
    (= envp (+ cmd_argv (* 4 (+ cmd_argc 1))))
    (spawn_pipe inp 0)
    (spawn_pipe outp 0)
    (spawn_pipe errp 0)
    (spawn_pipe failp O_CLOEXEC)
    (= pid (syscall SYS_FORK))
    (if (< pid 0) (return (io_err "fork" path)))
    (if (== pid 0) (do
        (syscall SYS_DUP2 (get inp 0) 0)
        (syscall SYS_DUP2 (get outp 1) 1)
        (syscall SYS_DUP2 (get errp 1) 2)
        (close (get inp 0))
        (close (get inp 1))
        (close (get outp 0))
        (close (get outp 1))
        (close (get errp 0))
        (close (get errp 1))
        (close (get failp 0))
        (syscall SYS_SIGNAL 13 0)
        (if (> (str_len dir) 0) (do
            (if (< (syscall SYS_CHDIR (get dir 1)) 0) (do
                (setb one 0 1)
                (write (get failp 1) one 1)
                (exit 127)
                ))
            ))
        (syscall SYS_EXECVE (get path 1) argvp envp)
        (setb one 0 1)
        (write (get failp 1) one 1)
        (exit 127)
        ))
    (close (get inp 0))
    (close (get outp 1))
    (close (get errp 1))
    (close (get failp 1))
    (syscall SYS_SIGNAL 13 1)
    (= in_len (str_len input))
    (= fd0 (get inp 1))
    (= fd1 (get outp 0))
    (= fd2 (get errp 0))
    (if (== in_len 0) (do
        (close fd0)
        (= fd0 -1)
        ))
    (while (|| (>= fd0 0) (|| (>= fd1 0) (>= fd2 0))) (do
        (set pf 0 fd0)
        (set pf 1 4)
        (set pf 2 fd1)
        (set pf 3 1)
        (set pf 4 fd2)
        (set pf 5 1)
        (= r (syscall SYS_POLL pf 3 -1))
        (if (< r 0) (do
            (if (!= r -4) (return (io_err "poll" path)))
            ))
        (if (>= r 0) (do
            (if (&& (>= fd0 0) (!= (| (getb pf 6) (getb pf 7)) 0)) (do
                (= chunk (- in_len in_pos))
                (if (> chunk 4096) (= chunk 4096))
                (= r (write fd0 (+ (get input 1) in_pos) chunk))
                (if (> r 0) (+= in_pos r))
                (if (|| (<= r 0) (>= in_pos in_len)) (do
                    (close fd0)
                    (= fd0 -1)
                    ))
                ))
            (if (&& (>= fd1 0) (!= (| (getb pf 14) (getb pf 15)) 0)) (do
                (= ob (spawn_room ob olen))
                (= r (read fd1 (+ (parray_data ob) olen) (- (get_header_arg ob) olen)))
                (if (<= r 0) (do
                    (close fd1)
                    (= fd1 -1)
                    ))
                (if (> r 0) (+= olen r))
                ))
            (if (&& (>= fd2 0) (!= (| (getb pf 22) (getb pf 23)) 0)) (do
                (= eb (spawn_room eb elen))
                (= r (read fd2 (+ (parray_data eb) elen) (- (get_header_arg eb) elen)))
                (if (<= r 0) (do
                    (close fd2)
                    (= fd2 -1)
                    ))
                (if (> r 0) (+= elen r))
                ))
            ))
        ))
    (syscall SYS_WAIT4 pid status 0 0)
    (= r (read (get failp 0) one 1))
    (close (get failp 0))
    (if (> r 0) (return (io_err "exec" path)))
    (= st (get status 0))
    (= rv -1)
    (if (== (& st 127) 0) (= rv (& (>> st 8) 255)))
    (= res (allocate 16))
    (set res 0 (make_header ArrayT 1 3))
    (set res 1 (fixnum rv))
    (set res 2 (spawn_text ob olen))
    (set res 3 (spawn_text eb elen))
    (return (io_ok res))
    )

(def SYS_GETDENTS64 220)
(def O_DIRECTORY 0x10000)

(fun name_array (cap)
    (var a (allocate (* 4 (+ cap 1))))
    (var i 0)
    (set a 0 (make_header ArrayT 1 cap))
    (while (< i cap) (do
        (set a (+ i 1) noneS)
        (+= i 1)
        ))
    (return a)
    )

(fun name_less (a b)
    (var i 0)
    (while (&& (!= (getb a i) 0) (== (getb a i) (getb b i))) (+= i 1))
    (if (< (getb a i) (getb b i)) (return 1))
    (return 0)
    )

(fun p_list_dir (path)
    (var fd 0)
    (var n 0)
    (var off 0)
    (var reclen 0)
    (var len 0)
    (var j 0)
    (var data 0)
    (var names 0)
    (var bigger 0)
    (var count 0)
    (var cap 64)
    (var cur 0)
    (var res 0)
    (char[] 4096 buf)
    (expect "os_list_dir" StringT path)
    (expect_no_null "os_list_dir" path)
    (= fd (open (get path 1) (| O_RDONLY O_DIRECTORY)))
    (if (< fd 0) (return (io_err "open" path)))
    (= names (name_array cap))
    (= n (syscall SYS_GETDENTS64 fd buf 4096))
    (while (> n 0) (do
        (= off 0)
        (while (< off n) (do
            (= reclen (| (getb buf (+ off 16)) (<< (getb buf (+ off 17)) 8)))
            (= len 0)
            (while (!= (getb buf (+ off (+ 19 len))) 0) (+= len 1))
            (if (! (|| (&& (== len 1) (== (getb buf (+ off 19)) 46))
                       (&& (== len 2) (&& (== (getb buf (+ off 19)) 46) (== (getb buf (+ off 20)) 46))))) (do
                (= data (allocate (+ len 1)))
                (= j 0)
                (while (< j len) (do
                    (setb data j (getb buf (+ off (+ 19 j))))
                    (+= j 1)
                    ))
                (setb data len 0)
                (if (== count cap) (do
                    (= bigger (name_array (* 2 cap)))
                    (= j 0)
                    (while (< j count) (do
                        (set bigger (+ j 1) (get names (+ j 1)))
                        (+= j 1)
                        ))
                    (= names bigger)
                    (= cap (* 2 cap))
                    ))
                (set names (+ count 1) (str_n data len))
                (+= count 1)
                ))
            (+= off reclen)
            ))
        (= n (syscall SYS_GETDENTS64 fd buf 4096))
        ))
    (close fd)
    (if (< n 0) (return (io_err "read" path)))
    (= j 1)
    (while (< j count) (do
        (= cur (get names (+ j 1)))
        (= off (- j 1))
        (while (&& (>= off 0) (name_less (get cur 1) (get (get names (+ off 1)) 1))) (do
            (set names (+ off 2) (get names (+ off 1)))
            (-= off 1)
            ))
        (set names (+ off 2) cur)
        (+= j 1)
        ))
    (= res (name_array count))
    (= j 0)
    (while (< j count) (do
        (set res (+ j 1) (get names (+ j 1)))
        (+= j 1)
        ))
    (return (io_ok res))
    )

(fun p_argv ()
    (var n (- cmd_argc 1))
    (var a 0)
    (var i 0)
    (if (< n 0) (= n 0))
    (= a (allocate (* 4 (+ n 1))))
    (set a 0 (make_header ArrayT 1 n))
    (while (< i n) (do
        (set a (+ i 1) (str (get cmd_argv (+ i 1))))
        (+= i 1)
        ))
    (return a)
    )

(fun planck_version_text () (return "dev"))

(fun init_sysinfo ()
    (var features (allocate 4))
    (set features 0 (make_header ArrayT 1 0))
    (set (define_global (sym "planck_target")) 0 (str "i386-linux"))
    (set (define_global (sym "planck_version")) 0 (str (planck_version_text)))
    (set (define_global (sym "planck_features")) 0 features)
    )

(fun print_version ()
    (puts "planck0 ")
    (puts (planck_version_text))
    (puts "
")
    )

(fun print_info ()
    (puts "target: i386-linux
")
    (puts "version: ")
    (puts (planck_version_text))
    (puts "
features:
")
    )

(fun p_parse (s) (return (parse s)))

(fun p_exit (n)
    (exit (ranged_arg "os_exit" n 255))
    )

(def SYS_CLOCK_GETTIME64 403)
(def CLOCK_REALTIME 0)
(char[] 16 clock_ts)

(fun p_time_ns ()
    (if (< (syscall SYS_CLOCK_GETTIME64 CLOCK_REALTIME clock_ts) 0)
        (runtime_error "os_time_ns failed" noneS))
    (var lo (get clock_ts 0))
    (var hi (get clock_ts 1))
    (var nsec (fixnum (get clock_ts 2)))
    (var upper (p_add (p_mul (fixnum hi) (fixnum 65536)) (fixnum (>> lo 16))))
    (var sec (p_add (p_mul upper (fixnum 65536)) (fixnum (& lo 65535))))
    (var micro (p_mul sec (fixnum 1000000)))
    (var whole (p_mul micro (fixnum 1000)))
    (return (p_add whole nsec))
    )

(fun p_allocated_bytes ()
    (var t (+ alloc_lo gc_since))
    (return (p_add (p_mul (fixnum (+ alloc_hi (>> t 20))) (fixnum 1048576)) (fixnum (& t 1048575))))
    )

(fun p_gc_count ()
    (return (fixnum gc_runs))
    )

(fun defprim (name arity fn kind)
    (var prim (allocate 16))
    (var s (sym name))
    (set prim 0 (make_header PrimT 0 arity))
    (set prim 1 fn)
    (set prim 2 s)
    (set prim 3 kind)
    (set (define_global s) 0 prim)
    )

(fun new_type (name)
    (var type (allocate 8))
    (set type 0 (make_header TypeT 0 0))
    (set type 1 name)
    (return type)
    )

; the type that is bound to the global variable of the name
(fun defines_type (name)
    (var s (sym name))
    (var t (new_type s))
    (set (define_global s) 0 t)
    (return t)
    )

(fun init_prims ()
    (= IntT_      (defines_type "Int"))
    (= StringT_   (defines_type "String"))
    (= SymbolT_   (defines_type "Symbol"))
    (= ExprT_     (defines_type "Expr"))
    (= FunctionT_ (defines_type "Function"))
    (= ArrayT_    (defines_type "Array"))
    (= PArrayT_    (defines_type "PArray"))
    (= TypeT_     (defines_type "Type"))
    (defprim "os_put"     1 p_put     PRIM_NATIVE)
    (defprim "int_add"     2 p_add     PRIM_NATIVE)
    (defprim "int_sub"     2 p_sub     PRIM_NATIVE)
    (defprim "int_mul"     2 p_mul     PRIM_NATIVE)
    (defprim "str_to_int"  1 p_bigint  PRIM_NATIVE)
    (defprim "int_to_str"  2 p_int_to_str  PRIM_NATIVE)
    (defprim "int_div"     2 p_div     PRIM_NATIVE)
    (defprim "int_quo"     2 p_quo     PRIM_NATIVE)
    (defprim "int_mod"     2 p_mod     PRIM_NATIVE)
    (defprim "int_lt"      2 p_lt      PRIM_NATIVE)
    (defprim "int_le"      2 p_le      PRIM_NATIVE)
    (defprim "int_gt"      2 p_gt      PRIM_NATIVE)
    (defprim "int_ge"      2 p_ge      PRIM_NATIVE)
    (defprim "equal"        2 p_eq      PRIM_NATIVE)
    (defprim "not_equal"    2 p_ne      PRIM_NATIVE)
    (defprim "identical"    2 p_identical PRIM_NATIVE)
    (defprim "mexpr_head"    1 p_head    PRIM_NATIVE)
    (defprim "mexpr_arity"   1 p_arity   PRIM_NATIVE)
    (defprim "mexpr_arg"     2 p_arg     PRIM_NATIVE)
    (defprim "mexpr_new"   2 p_mexpr   PRIM_NATIVE)
    (defprim "type"         1 p_typeof  PRIM_NATIVE)
    (defprim "type_name" 1 p_type_name PRIM_NATIVE)
    (defprim "make_type"      1 p_make_type PRIM_NATIVE)
    (defprim "mexpr_parse" 1 p_parse PRIM_NATIVE)
    (defprim "str_len"  1 p_strlen  PRIM_NATIVE)
    (defprim "str_ord"     2 p_ord     PRIM_NATIVE)
    (defprim "str_chr"     1 p_chr     PRIM_NATIVE)
    (defprim "str_sub"  3 p_substr  PRIM_NATIVE)
    (defprim "str_cat"  2 p_strcat  PRIM_NATIVE)
    (defprim "sym_intern"  1 p_intern  PRIM_NATIVE)
    (defprim "sym_name" 1 p_symname PRIM_NATIVE)
    (defprim "array_new"   1 p_array   PRIM_NATIVE)
    (defprim "array_get" 2 p_array_get PRIM_NATIVE)
    (defprim "array_set" 3 p_array_set PRIM_NATIVE)
    (defprim "array_len" 1 p_array_len PRIM_NATIVE)
    (defprim "array_copy" 5 p_array_copy PRIM_NATIVE)
    (defprim "hash"         1 p_hash    PRIM_NATIVE)
    (defprim "os_read_file" 1 p_read_file PRIM_NATIVE)
    (defprim "os_write_file" 2 p_write_file PRIM_NATIVE)
    (defprim "os_write_bytes" 2 p_write_bytes PRIM_NATIVE)
    (defprim "os_read_bytes" 1 p_read_bytes PRIM_NATIVE)
    (defprim "os_chmod"   2 p_chmod   PRIM_NATIVE)
    (defprim "parray_new_u8" 1 p_u8_array PRIM_NATIVE)
    (defprim "parray_new_u16" 1 p_u16_array PRIM_NATIVE)
    (defprim "parray_new_i32" 1 p_i32_array PRIM_NATIVE)
    (defprim "parray_len" 1 p_parray_len PRIM_NATIVE)
    (defprim "parray_kind" 1 p_parray_kind PRIM_NATIVE)
    (defprim "parray_get" 2 p_parray_get PRIM_NATIVE)
    (defprim "parray_set" 3 p_parray_set PRIM_NATIVE)
    (defprim "parray_copy" 5 p_parray_copy PRIM_NATIVE)
    (defprim "bit_and" 2 p_bit_and PRIM_NATIVE)
    (defprim "bit_or"  2 p_bit_or  PRIM_NATIVE)
    (defprim "bit_xor" 2 p_bit_xor PRIM_NATIVE)
    (defprim "bit_not" 1 p_bit_not PRIM_NATIVE)
    (defprim "bit_shl" 2 p_bit_shl PRIM_NATIVE)
    (defprim "bit_sar" 2 p_bit_sar PRIM_NATIVE)
    (defprim "os_argv" 0 p_argv PRIM_NATIVE)
    (defprim "os_exit"    1 p_exit    PRIM_NATIVE)
    (defprim "os_time_ns" 0 p_time_ns PRIM_NATIVE)
    (defprim "gc_allocated_bytes" 0 p_allocated_bytes PRIM_NATIVE)
    (defprim "gc_count" 0 p_gc_count PRIM_NATIVE)
    (defprim "os_run" 4 p_run_program PRIM_NATIVE)
    (defprim "os_list_dir" 1 p_list_dir PRIM_NATIVE)
    (defprim "eval"    1 0         PRIM_EVAL)
    (init_sysinfo)
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
    (return (str_n buf file_size))
    )

; The loop reads each top-level expression with the builtin parser and looks up
; the global variable eval every time, so a program can replace it (Def{eval, ...}
; or Set) and the next top-level expression is evaluated by the new definition.
; Apply{eval, ...} in the code compiled after the replacement also calls the
; replaced eval; the code compiled before keeps the old one unless the
; replacement is Set{eval, ...}.
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
        (= ret (parse text))
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

; === Garbage Collection
;
; A stop-the-world mark and sweep collector which never moves objects.
; It is conservative: every word of a root or of a marked object which points
; into an object (not only to its beginning, and not only at a multiple of 4,
; as a string points to its bytes) keeps the object. The roots are the global variables of this program, the frame
; stack and the machine stack. The tables of symbols and m-expressions are
; roots, so the symbols and m-expressions are never freed.
; The sweep walks over all chunks, frees the unmarked objects and merges the
; adjacent free chunks. A collection starts when the memory allocated since
; the last one reaches the live memory of the last one (at least 64MB).

(long stack_hi)     ; the highest address of the machine stack that is scanned

; the beginning of the chunk of the object that contains the address p (0 if there is none)
(fun gc_find_chunk (p)
    (var g (>> (- p heap_root) 3))
    (var i 0)
    (var b 0)
    (var k 0)
    (while (>= g 0) (do
        (= i (>> g 3))
        (= k (& g 7))
        (= b (& (getb gc_bitmap i) (- (<< 2 k) 1)))
        (if b (do
            (while (! (& b (<< 1 k))) (-= k 1))
            (return (+ heap_root (<< (+ (<< i 3) k) 3)))
            ))
        (= g (- (<< i 3) 1))
        ))
    (return 0)
    )

(fun gc_mark (p)
    (var c 0)
    (var h 0)
    (var n 0)
    (if (u< p (+ heap_root 4)) (return 0))
    (if (u>= p heap_pos) (return 0))
    (= c (gc_find_chunk p))
    (if (! c) (return 0))
    (= h (get c))
    (if (& h 3) (return 0))
    (= n (& h 0xfffffff8))
    (if (u>= p (+ c n)) (return 0))
    (set c (| h GC_MARK))
    (+= gc_live n)
    (if (u>= gc_sp gc_stack_end) (do
        (eputs "gc: mark stack overflow\n")
        (exit 1)
        ))
    (set gc_sp c)
    (+= gc_sp 4)
    )

(fun gc_scan (from to)
    (while (u< from to) (do
        (gc_mark (get from))
        (+= from 4)
        ))
    )

(fun gc_drain ()
    (var c 0)
    (while (u> gc_sp gc_stack) (do
        (-= gc_sp 4)
        (= c (get gc_sp))
        (gc_scan (+ c 4) (+ c (& (get c) 0xfffffff8)))
        ))
    )

(fun gc_sweep ()
    (var i 0)
    (var c heap_root)
    (var h 0)
    (var n 0)
    (var run 0)
    (var run_size 0)
    (while (<= i 32) (do
        (set gc_lists i 0)
        (+= i 1)
        ))
    (while (u< c heap_pos) (do
        (= h (get c))
        (= n (& h 0xfffffff8))
        (if (& h GC_MARK) (do
            (set c (& h 0xfffffffe))
            (if run_size (free_chunk run run_size))
            (= run_size 0)
            ) (do
            (if (! (& h GC_FREE)) (bm_clear c))
            (if (! run_size) (= run c))
            (+= run_size n)
            ))
        (+= c n)
        ))
    (if run_size (do
        (if (== (+ run run_size) heap_pos)
            (= heap_pos run)
            (free_chunk run run_size))
        ))
    )

(fun gc_collect ()
    (var p 0)
    (var t 0)
    (char[] 4 here)
    (= gc_live 0)
    (= gc_sp gc_stack)
    (gc_mark symtable)
    (gc_mark mexprtable)
    (gc_mark emptyTuple)
    (gc_mark global_table)
    (gc_mark parse_str_buf)
    (gc_mark parse_arena)
    (gc_mark (- cbuf_end 4))
    (gc_mark cctx)
    (gc_mark cenv)
    (gc_mark vm_pc)
    (gc_mark vm_env)
    (gc_mark vm_code)
    (gc_mark vm_result)
    (gc_mark sel_env)
    (gc_scan frame_base (frame_top vm_frame))
    (gc_scan here stack_hi)
    (gc_drain)
    (gc_sweep)
    (= t (+ alloc_lo gc_since))
    (+= alloc_hi (>> t 20))
    (= alloc_lo (& t 1048575))
    (+= gc_runs 1)
    (= gc_since 0)
    (= gc_limit gc_live)
    (if (u< gc_limit GC_MIN_BYTES) (= gc_limit GC_MIN_BYTES))
    )

; The frame stack and the heap are each a fixed 1GB area obtained by mmap2.
(fun main (argc argv)
    (if (> argc 1) (do
        (if (streq (get argv 1) "--version") (do (print_version) (exit 0)))
        (if (streq (get argv 1) "--info") (do (print_info) (exit 0)))
        ))
    (= stack_hi argv)
    (init_frame_stack)
    (init_heap)
    (init_tables)
    (init_symbols)
    (init_compiler)
    (init_prims)

    (interpret_file "planck/init.pk")
    (= cmd_argc argc)
    (= cmd_argv argv)
    (if (> argc 1) (interpret_file (get argv 1)))
    )
