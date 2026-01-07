; Simple virtual machine code assembler written in plancklisp
; Copyright (C) 2026 nineties

(import "asm/core.lisp")

(define vasm-to-asm (code) (do
    (def asm-code ())

    (define emit-asm (line)
        (set asm-code (cons line asm-code)))

    (define compile-func (decl) (do
        (def label (cadr decl))
        (def params (caddr decl))
        (def body (cdddr decl))

        (emit-asm `(label ,label))
        (for insn body (switch (car insn)
            ('syscall (do
                (def regs '(%eax %ebx %ecx %edx %esi %edi %ebp))
                (def args (cdr insn))
                (while args (do
                    (emit-asm `(mov ,(car regs) ,(car args)))
                    (set regs (cdr regs))
                    (set args (cdr args))
                    ))
                (emit-asm '(int 0x80))
                ))
            (not-implemented "compile-func")
            ))
        ))

    (define compile-data (decl) (do
        (def label (cadr decl))
        (def data (caddr decl))

        (emit-asm `(label ,label))
        (cond
            ((str? data) (emit-asm `(ascii ,data)))
            (true (not-implemented "compile-data"))
            )
        ))

    (for decl code (switch (car decl)
        ('entry     (emit-asm decl))
        ('function  (compile-func decl))
        ('=         (compile-data decl))
        (println decl)
        ))

    (reverse asm-code)
    ))

(define vasm-compile (code) (do
    (def asm-code (vasm-to-asm code))
    (println asm-code)
    (assemble asm-code)
    ))
