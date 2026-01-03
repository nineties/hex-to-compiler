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

(define compute-insn-len (line) (switch (car line)
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
            (true (not-implemented "compute-insn-len"))
            ))
        bytes
        ))
    ; default case
    (not-reachable "compute-insn-len")
    ))

(define assemble (asm-code) (do
    (def load_base 0x400000)
    (def page_size 0x1000)

    (def entry ())
    (def current_loc 0)
    (def insns ())
    (def label_offsets ())    ; label -> offs

    ; pass1: scan asm code to compute address of labels
    (for line asm-code (do
        (def head (car line))
        (switch head
          ('entry   (set entry (cadr line)))
          ('label   (acons! (cadr line) current_loc label_offsets))
          ('const   ())  ; do notthing
          (do
                (def insn (lookup-instruction line))

                (+= current_loc (compute-insn-len insn))
                (set insns (cons insn insns))
                )
          )))
    (set insns (reverse insns)) ; here, insns are stored in reversed order

    (when (nil? entry) (abort "entry point is not defined"))

    (def elf_hdr_size 52)
    (def phent_size 32)     ; program header entry size
    (def segment_size current_loc)
    (def filesize (+ elf_hdr_size (+ phent_size segment_size)))
    (def entry_addr (+ load_base (assoc entry label_offsets)))

    (def buf (allocate filesize))
    (def buf_pos 0)

    (define emit (i) (do (setb buf buf_pos i) (+= buf_pos 1)))
    (define emit_i16 (i) (do
        (emit (& 0xff i))
        (emit (& 0xff (>> i 8)))
        ))
    (define emit_i32 (i) (do
        (emit (& 0xff i))
        (emit (& 0xff (>> i 8)))
        (emit (& 0xff (>> i 16)))
        (emit (& 0xff (>> i 24)))
        ))
    (define emit_bytes bytes (while bytes (do (emit (car bytes)) (set bytes (cdr bytes)))))

    ; === write ELF header ===
    ; e_ident
    (emit_bytes
      0x7f (char "E") (char "L") (char "F")
      1 ; EI_CLASS=32bit
      1 ; EI_DATA=little endian
      1 ; EI_VERSION=EV_CURRENT
      0 ; EI_OSABI=unspecified
      0 ; EI_ABIVERSION=0
      0 0 0 0 0 0 0 ; pad
      )
    (emit_i16 2) ; e_type=ET_EXEC
    (emit_i16 3) ; e_machine=EM_386
    (emit_i32 1) ; e_version=EV_CURRENT
    (emit_i32 entry_addr)   ; e_entry
    (emit_i32 elf_hdr_size) ; e_phoff
    (emit_i32 0)            ; e_shoff (no section header)
    (emit_i32 0)            ; e_flags
    (emit_i16 elf_hdr_size) ; e_ehsize
    (emit_i16 phent_size)   ; e_phentsize
    (emit_i16 1)            ; e_phnum (use single program header)
    (emit_i16 0)            ; e_shentsize
    (emit_i16 0)            ; e_shnum
    (emit_i16 0)            ; e_shstrndx

    (assert (= buf_pos elf_hdr_size))

    ; === write Program header ===
    (emit_i32 1)    ; p_type=PT_LOAD
    (emit_i32 (+ elf_hdr_size phent_size)) ; p_offset
    (emit_i32 load_base)    ; p_vaddr
    (emit_i32 0)            ; p_paddr
    (emit_i32 segment_size) ; p_filesz
    (emit_i32 segment_size) ; p_memsz (XXX: bss should supported)
    (emit_i32 0x7)          ; p_flags=RWX
    (emit_i32 page_size)    ; p_align

    (assert (= buf_pos (+ elf_hdr_size phent_size)))

    (list buf filesize)
    ))
