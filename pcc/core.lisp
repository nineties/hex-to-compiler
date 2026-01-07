; Simple compiler for C-like language written in plancklisp
; Copyright (C) 2026 nineties

(import "asm/core.lisp")

(define compile (decls) (do
    (def asm-code ())
    (define emit-asm (line)
        (set asm-code (cons line asm-code)))

    (define push (val) (emit-asm `(push ,val)))
    (define pop (reg)  (emit-asm `(pop ,reg)))

    (def compile-expr ())

    (define compile-call (expr) (do
        (def fun (car expr))
        (def args (cdr expr))
        (for arg (reverse args) (compile-expr push arg))
        (emit-asm `(call ,fun))
        (when args (emit-asm `(add %esp ,(* 4 (length args)))))
        (push '%eax)
        ))

    (define compile-binop (op expr) (do
        (compile-expr (cadr expr))  ; lhs
        (compile-expr (caddr expr)) ; rhs
        (pop '%ecx) ; rhs 
        (pop '%eax) ; lhs
        (emit-asm `(,op %eax %ecx))
        (push '%eax)
        ))

    (define! compile-expr (expr) (cond
        ((int? expr)    (push expr))
        ((cons? expr)   (switch (car expr)
            ('+     (compile-binop 'add expr))
            ('-     (compile-binop 'sub expr))
            ('*     (compile-binop 'imul expr))
            ('/     (do
                (compile-expr (cadr expr))
                (compile-expr (caddr expr))
                (emit-asm '(xor %edx %edx))
                (pop '%ecx)
                (pop '%eax)
                (emit-asm '(idiv %ecx))
                (push '%eax)
                ))
            ('%     (do
                (compile-expr (cadr expr))
                (compile-expr (caddr expr))
                (emit-asm '(xor %edx %edx))
                (pop '%ecx)
                (pop '%eax)
                (emit-asm '(idiv %ecx))
                (push '%edx)
                ))
            (compile-call expr)
            ))
        ))

    (define compile-stmt (stmt) (switch (car stmt)
        ('return    (emit-asm '(ret)))
        ('syscall   (do
            (def regs '(%eax %ebx %ecx %edx %esi %edi %ebp))
            (def args (cdr stmt))
            (for arg (reverse args) (compile-expr arg))
            (while args (do
                (pop (car regs))
                (set regs (cdr regs))
                (set args (cdr args))
                ))
            (emit-asm '(int 0x80))
            ))
        (do
            (compile-expr '%eax stmt)
            (emit-asm '(add %esp 4))
            )
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
        (syscall 1 (% 7 4))
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
