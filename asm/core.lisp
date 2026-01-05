; Simple x86 assembler written in plancklisp
; Copyright (C) 2026 nineties

; This assembler generates directly executable ELF files.

(define assemble (asm-code) (do
    (def reg8 '(%al %cl %dl %bl %ah %ch %dh %bh %bpl %spl %dil %sil))
    (def reg16 '(%ax %cx %dx %bx %sp %bp %si %di))
    (def reg32 '(%eax %ecx %edx %ebx %esp %ebp %esi %edi))

    (def x86_instructions '(
        ((int "imm8")           ("I"  0xcd "ib"))
        ((mov "r32" "imm32")    ("OI" 0xb8 "id"))
        ((xor "r/m32" "r32")    ("MR" 0x31 "/r"))
        ))

    (define match_operand (opd pat) (cond
        ((= opd pat)   true)
        ((= "r8" opd)  (member? pat '("r8" "r16" "r32" "r/m8" "r/m16" "r/m32")))
        ((= "r16" opd) (member? pat '("r16" "r32" "r/m16" "r/m32")))
        ((= "r32" opd) (member? pat '("r32" "r/m32")))
        ((= "imm" opd) (member? pat '("imm8" "imm16" "imm32")))
        ((= "sym" opd) (member? pat '("imm8" "imm16" "imm32" "rel8" "rel16" "rel32")))
        (true ())
        ))

    (define match_insn (insn pat) (cond
        ((!= (car insn) (car pat))       ())
        ((!= (length insn) (length pat)) ())
        (true (do
            (define iter (ts ps) (cond
                ((nil? ts) true)
                ((match_operand (car ts) (car ps))  (iter (cdr ts) (cdr ps)))
                (true ())
                ))
            (iter (cdr insn) (cdr pat))
        ))))

    (define to_operand_type (opd) (cond
        ((int? opd) "imm")
        ((member? opd reg8)  "r8")
        ((member? opd reg16) "r16")
        ((member? opd reg32) "r32")
        ((sym? opd) "sym") ; addr or offset or const
        ((&& (cons? opd) (= (car opd) '+))  "m")
        (true   (not-implemented "to_operand_type"))
        ))

    (define lookup_insn (insn)
        (if (member? (car insn) '(ascii asciz byte short long))
            insn
            (do
                (def type (cons (car insn) (map to_operand_type (cdr insn))))
                (def fmt (assoc-find (lambda (pat) (match_insn type pat)) x86_instructions))
                (when (= fmt 'error) (do
                    (put "invalid or unsupported instruction: ")
                    (println insn)
                    (exit 1)
                    ))
                (list 'insn insn fmt)
            )
        ))

    (define compute_insn_len (line) (switch (car line)
        ('ascii (strlen (cadr line)))
        ('asciz (+ 1 (strlen (cadr line))))
        ('byte  (length (cdr line)))
        ('short (* 2 (length (cdr line))))
        ('long  (* 4 (length (cdr line))))
        ('quad  (* 8 (length (cdr line))))
        ('insn  (do
            (def bytes 0)
            (defvar (insn fmt) (cdr line))
            (def operands (cdr fmt))
            (for opd operands (cond
                ((int? opd)   (+= bytes 1))
                ((= opd "ib") (+= bytes 1))
                ((= opd "iw") (+= bytes 2))
                ((= opd "id") (+= bytes 4))
                ((= opd "/r") (+= bytes 1))
                ((&& (cons? opd) (= '+ (car opd)))   (+= bytes 1))
                (true (not-implemented "compute_insn_len"))
                ))
            bytes
            ))
        ; default case
        (not-reachable "compute_insn_len")
        ))

    ; assembler main
    ;(def load_base 0x400000)
    (def load_base 0x8048000)
    (def page_size 0x1000)
    (def elf_hdr_size 52)
    (def phent_size 32)     ; program header entry size

    (def entry ())

    ; This program loads the entire file, including headers,
    ; as a single segment. Since the program body follows
    ; immediately after the headers, current_loc starts from the header size.
    (def current_loc (+ elf_hdr_size phent_size))

    (def insns ())
    (def label_offsets ())    ; label -> offs

    ; pass1: scan asm code to compute address of labels
    (for line asm-code (do
        (def head (car line))
        (switch head
          ('entry   (set entry (cadr line)))
          ('label   (acons! (cadr line) current_loc label_offsets))
          ('const   (set insns (cons line insns)))
          (do
                (def insn (lookup_insn line))

                (+= current_loc (compute_insn_len insn))
                (set insns (cons insn insns))
                )
          )))
    (set insns (reverse insns)) ; here, insns are stored in reversed order

    (when (nil? entry) (abort "entry point is not defined"))

    (def segment_size current_loc)

    (define compute_addr (label)
        (+ load_base (assoc label label_offsets)))
    (def entry_addr (compute_addr entry))

    (def buf (allocate segment_size))
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
    (define emit_ascii (str) (do
        (def i 0)
        (while (!= (getb str i) 0) (do
            (emit (getb str i))
            (+= i 1)
            ))
        ))

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
    ; (emit_i32 (+ elf_hdr_size phent_size)) ; p_offset
    (emit_i32 0)
    (emit_i32 load_base)    ; p_vaddr
    (emit_i32 0)            ; p_paddr
    (emit_i32 segment_size) ; p_filesz
    (emit_i32 segment_size) ; p_memsz (XXX: bss should supported)
    (emit_i32 0x7)          ; p_flags=RWX
    (emit_i32 page_size)    ; p_align

    (assert (= buf_pos (+ elf_hdr_size phent_size)))

    ; === pass2: write the segment ===
    (define here () (+ load_base buf_pos))
    (def consts ())

    (define eval (e) (cond
        ((int? e)           e)
        ((= e 'here)        (here))
        ((member? e reg8)   e)
        ((member? e reg16)  e)
        ((member? e reg32)  e)
        ((sym? e)        (do
            (def off (assoc e label_offsets))
            (def val (assoc e consts))
            (when (&& (= off 'error) (= val 'error)) (do
                (put "undefined label or const: ")
                (println e)
                (exit 1)
                ))
            (if (!= off 'error)
                (compute_addr e)
                val
                )
            ))
        ((= (car e) '+) (+ (eval (cadr e)) (eval (caddr e))))
        ((= (car e) '-) (- (eval (cadr e)) (eval (caddr e))))
        (true               (not-implemented "eval"))
        ))

    (define reg? (r)
      (|| (member? r reg32)
      (|| (member? r reg8)
          (member? r reg16))))

    (define encode_reg (r) (cond
        ((member? r '(%al %ax %eax))    0)
        ((member? r '(%cl %cx %ecx))    1)
        ((member? r '(%dl %dx %edx))    2)
        ((member? r '(%bl %bx %ebx))    3)
        ((member? r '(%ah %sp %esp))    4)
        ((member? r '(%ch %bp %ebp))    5)
        ((member? r '(%dh %si %esi))    6)
        ((member? r '(%bh %di %edi))    7)
        (true   (not-reachable "encode_reg"))
        ))

    (define emit_imm (ty v) (switch ty
        ("ib" (emit v))
        ("iw" (emit_i16 v))
        ("id" (emit_i32 v))
        (not-reachable "emit_imm")
        ))

    (define emit_modrm (reg r/m) (do
        (define enc (mod reg r/m)
            (| (<< mod 6) (| (<< reg 3) r/m))
               )
        (cond
            ((reg? r/m)  (do
                (def r1 (encode_reg reg))
                (def r2 (encode_reg r/m))
                (emit (enc 0x3 r1 r2))
                ))
            (true   (do
                (not-implemented "ncode_modrm")
                ))
        )))


    (define emit_insn (insn fmt) (switch (car fmt)
        ("I"    (do
            (emit (nth 1 fmt))
            (emit_imm (nth 2 fmt) (cadr insn))
            ))
        ("OI"   (do
            (emit (+ (nth 1 fmt) (encode_reg (cadr insn)))) ; opcode + reg
            (emit_imm (nth 2 fmt) (caddr insn)) ; imm
            ))
        ("MR"   (do
            (emit (nth 1 fmt))
            (emit_modrm (nth 2 insn) (nth 1 insn))
            ))
        (not-implemented "emit_insn")
        ))

    (for insn insns (switch (car insn)
        ('const (acons! (cadr insn) (eval (caddr insn)) consts))
        ('ascii (emit_ascii (cadr insn)))
        ('asciz (do (emit_ascii (cadr insn)) (emit 0)))
        ('byte  (emit (eval (cadr insn))))
        ('short (emit_i16 (eval (cadr insn))))
        ('long  (emit_i32 (eval (cadr insn))))
        ('insn (do
            (defvar (code fmt) (cdr insn))
            (set code (cons (car code) (map eval (cdr code))))
            (emit_insn code fmt)
            ))
        ()
        ))

    (list buf segment_size)
    ))
