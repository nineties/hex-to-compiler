; Simple compiler for C-like language written in plancklisp
; Copyright (C) 2026 nineties

(import "asm/core.lisp")

(define compile (decls) (do
    (def asm-code ())
    (define emit-asm (line)
        (set asm-code (cons line asm-code)))

    (define push (val) (emit-asm `(push ,val)))
    (define reg (r) (lambda (val)
        (emit-asm `(mov ,r ,val))))

    (define compile-call (ret expr) (do
        (def fun (car expr))
        (def args (cdr expr))
        (for arg (reverse args) (compile-expr push arg))
        (emit-asm `(call ,fun))
        (when args (emit-asm `(add %esp ,(* 4 (length args)))))
        ))

    (define compile-expr (ret expr) (cond
        ((int? expr)    (ret expr))
        ((cons? expr)   (switch (car expr)
            (compile-call ret expr)
            ))
        ))

    (define compile-stmt (stmt) (switch (car stmt)
        ('return    (emit-asm '(ret)))
        ('syscall   (do
            (def regs '(%eax %ebx %ecx %edx %esi %edi %ebp))
            (def args (cdr stmt))
            (while args (do
                (compile-expr (reg (car regs)) (car args))
                (set regs (cdr regs))
                (set args (cdr args))
                ))
            (emit-asm '(int 0x80))
            ))
        (compile-expr '%eax stmt)
        ))

    (define compile-fundecl (decl) (do
        (def label (cadr decl))
        (def params (caddr decl))
        (def body (cdddr decl))

        (emit-asm `(label ,label))
        (for stmt body (compile-stmt stmt))
        ))

    ; entry point
    (emit-asm '(entry _start))
    (compile-fundecl '(fun _start ()
        (main)
        (syscall 1 0)
        ))

    ; compile declarations
    (for decl decls (switch (car decl)
        ('fun   (compile-fundecl decl))
        (not-implemented "compile")
        ))

    (set asm-code (reverse asm-code))

    (println asm-code)
    (assemble asm-code)
    ))
