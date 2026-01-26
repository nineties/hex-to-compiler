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

; === header layout
; m: 1 if the object is mutable
; 
;         |  28bit | 1bit | 3bit |
; symbol  |        |    0 |  000 |
; string  | length |    0 |  001 |
; mexp    |  arity |    0 |  010 |
; array   | length |    m |  011 |
; struct  |        |    m |  100 |
; closure |        |    0 |  101 |

(fun main (argc argv)
    (if (<= argc 1) (do
        (puts "no input file")
        (exit 1)
        ))

    (init_heap)
    )
