; Simple x86 assembler written in plancklisp
; Copyright (C) 2026 nineties

; This assembler generates directly executable ELF files.

(def reg8 '(%al %cl %dl %bl %ah %ch %dh %bh %bpl %spl %dil %sil))
(def reg16 '(%ax %cx %dx %bx %sp %bp %si %di))
(def reg32 '(%eax %ecx %edx %ebx %esp %ebp %esi %edi))

(def x86-instructions '(
    ((int "imm8")           (0xcd "ib"))
    ((mov "r32" "imm32")    ((+ 0xb8 reg) "id"))
    ((xor "r/m32" "r32")    (0x31 "/r"))
    ))

(define match-operand (opd pat) (cond
    ((= opd pat)   true)
    ((= "r8" opd)  (member? pat '("r8" "r16" "r32" "r/m8" "r/m16" "r/m32")))
    ((= "r16" opd) (member? pat '("r16" "r32" "r/m16" "r/m32")))
    ((= "r32" opd) (member? pat '("r32" "r/m32")))
    ((= "imm" opd) (member? pat '("imm8" "imm16" "imm32")))
    ((= "sym" opd) (member? pat '("imm8" "imm16" "imm32" "rel8" "rel16" "rel32")))
    (true ())
    ))

(define match-insn (insn pat) (cond
    ((!= (car insn) (car pat))       ())
    ((!= (length insn) (length pat)) ())
    (true (do
        (define iter (ts ps) (cond
            ((nil? ts) true)
            ((match-operand (car ts) (car ps))  (iter (cdr ts) (cdr ps)))
            (true ())
            ))
        (iter (cdr insn) (cdr pat))
    ))))

(define to-operand-type (opd) (cond
    ((int? opd) "imm")
    ((member? opd reg8)  "r8")
    ((member? opd reg16) "r16")
    ((member? opd reg32) "r32")
    ((sym? opd) "sym") ; addr or offset or const
    ((&& (cons? opd) (= (car opd) '+))  "m")
    (true   (not-implemented "to-operand-type"))
    ))

(define lookup-instruction (insn)
    (if (member? (car insn) '(ascii asciz byte short long quad octa))
        insn
        (do
            (def type (cons (car insn) (map to-operand-type (cdr insn))))
            (def encoding (assoc-find (lambda (pat) (match-insn type pat)) x86-instructions))
            (when (= encoding 'error) (do
                (put "invalid or unsupported instruction: ")
                (println insn)
                (exit 1)
                ))
            (list 'insn insn encoding)
        )
    ))

(define compute-bytes (line) (switch (car line)
    ('ascii (strlen (cadr line)))
    ('asciz (+ 1 (strlen (cadr line))))
    ('byte  (length (cdr line)))
    ('short (* 2 (length (cdr line))))
    ('long  (* 4 (length (cdr line))))
    ('quad  (* 8 (length (cdr line))))
    ('insn  (do
        (def bytes 0)
        (defvar (insn operands) (cdr line))
        (for opd operands (cond
            ((int? opd)   (+= bytes 1))
            ((= opd "ib") (+= bytes 1))
            ((= opd "iw") (+= bytes 2))
            ((= opd "id") (+= bytes 4))
            ((= opd "/r") (+= bytes 1))
            ((&& (cons? opd) (= '+ (car opd)))   (+= bytes 1))
            (true (not-implemented "compute-bytes"))
            ))
        bytes
        ))
    ; default case
    (not-reachable "compute-bytes")
    ))

(define assemble (asm-code) (do
    (def load_base 0x400000)
    (def page_size 0x1000)

    (def entry ())
    (def current_section 'text)
    (def current_locs '(
        (text 0)
        (data 0)
        (rodata 0)
        (bss 0)
        ))
    (def sections '(
        (text ())
        (data ())
        (rodata ())
        (bss ())
        ))
    (def label_offsets ())    ; label -> (section offs)

    ; pass1: scan asm code to compute address of labels
    (for line asm-code (do
        (def head (car line))
        (switch head
          ('entry   (set entry (cadr line)))
          ('section (set current_section (cadr line)))
          ('label   (do
                (def addr (assoc current_section current_locs))
                (acons! (cadr line) (list current_section addr) label_offsets)
                ))
          ('const   ())  ; do notthing
          (do
                (def insn-def (lookup-instruction line))

                ; increment location counter
                (def len (compute-bytes insn-def))
                (def loc (assoc current_section current_locs))
                (assoc-set current_section (+ loc len) current_locs)

                ; append new line to corresponding section
                (def lines (assoc current_section sections))
                (assoc-set current_section (cons insn-def lines) sections)
                )
          )))
    (println entry)
    (println label_offsets)
    (println current_locs)
    (when (nil? entry) (abort "entry point is not defined"))

    (not-implemented "assemble")
    ))
