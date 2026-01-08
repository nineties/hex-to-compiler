; Simple compiler for C-like language written in plancklisp
; Copyright (C) 2026 nineties

(import "asm/core.lisp")

(define compile (decls) (do
    (def asm-code ())
    (def asm-pre ())
    (def env ())    ; variable table
    (def nlocal 0)  ; number of local variables

    (def fresh_label_cnt 0)
    (define fresh-label () (do
        (+= fresh_label_cnt 1)
        (str2sym (strcat ".L." (int2str 10 fresh_label_cnt)))
        ))

    (define emit-asm (line)
        (set asm-code (cons line asm-code)))

    (define push (val) (emit-asm `(push ,val)))
    (define pop (reg)  (emit-asm `(pop ,reg)))

    (def compile-expr ())

    (define compile-call (expr env) (do
        (def fun (car expr))
        (def args (cdr expr))
        (for arg (reverse args) (compile-expr arg env))
        (emit-asm `(call ,fun))
        (when args (emit-asm `(add %esp ,(* 4 (length args)))))
        (push '%eax)
        ))

    (define compile-binop (op expr env) (do
        (compile-expr (cadr expr) env)  ; lhs
        (compile-expr (caddr expr) env) ; rhs
        (pop '%ecx) ; rhs 
        (pop '%eax) ; lhs
        (emit-asm `(,op %eax %ecx))
        (push '%eax)
        ))

    (define! compile-expr (expr env) (cond
        ((int? expr)    (push expr))
        ((str? expr) (do
            (def label (fresh-label))
            (set asm-pre (cons `(label ,label) asm-pre))
            (set asm-pre (cons `(ascii ,expr) asm-pre))
            (push label)
            ))
        ((sym? expr)    (do
            (def pos (assoc expr env))
            (when (= pos 'error) (do
                (put "undefined variable: ")
                (println expr)
                (exit 1)))
            (switch (car pos)
                ('local     (do
                    (def offs (cadr pos))
                    (push `(mem %ebp ,(negate (* 4 (+ offs 1)))))
                    ))
                (not-implemented "compile-car:var")
                )
            ))
        ((cons? expr)   (switch (car expr)
            ('+     (compile-binop 'add expr env))
            ('-     (compile-binop 'sub expr env))
            ('*     (compile-binop 'imul expr env))
            ('/     (do
                (compile-expr (cadr expr) env)
                (compile-expr (caddr expr) env)
                (emit-asm '(xor %edx %edx))
                (pop '%ecx)
                (pop '%eax)
                (emit-asm '(idiv %ecx))
                (push '%eax)
                ))
            ('%     (do
                (compile-expr (cadr expr) env)
                (compile-expr (caddr expr) env)
                (emit-asm '(xor %edx %edx))
                (pop '%ecx)
                (pop '%eax)
                (emit-asm '(idiv %ecx))
                (push '%edx)
                ))
            (compile-call expr env)
            ))
        (true
            (not-implemented "compile-expr"))
        ))

    (define compile-stmt (stmt env) (switch (car stmt)
        ('return    (do
            (when (cdr stmt) (do
                (compile-expr (cadr stmt) env)
                (pop '%eax)
                ))
            (when (> nlocal 0)
                (emit-asm `(add %esp ,(* 4 nlocal))))
            (emit-asm '(ret))
            env))
        ('syscall   (do
            (def regs '(%eax %ebx %ecx %edx %esi %edi %ebp))
            (def args (cdr stmt))
            (for arg (reverse args) (compile-expr arg env))
            (while args (do
                (pop (car regs))
                (set regs (cdr regs))
                (set args (cdr args))
                ))
            (emit-asm '(int 0x80))
            env
            ))
        ('var   (do
            (def x (cadr stmt))
            (def e (caddr stmt))
            (compile-expr e env)
            (set env (acons x `(local ,nlocal) env))
            (+= nlocal 1)
            env
            ))
        ('asm   (do
            (emit-asm (cadr stmt))
            env
            ))
        (do
            (compile-expr stmt env)
            (emit-asm '(add %esp 4))
            env
            )
        ))

    (define compile-fundecl (decl) (do
        (def label (cadr decl))
        (def params (caddr decl))
        (def body (cdddr decl))

        (emit-asm `(label ,label))
        (set nlocal 0)
        (for stmt body (set env (compile-stmt stmt env)))
        ))

    ; entry point
    (emit-asm '(entry _start))
    (compile-fundecl '(fun _start ()
        (asm (mov %ebp %esp))   ; initialize %ebp
        (syscall 1 (main))
        ))

    ; compile declarations
    (for decl decls (switch (car decl)
        ('fun   (compile-fundecl decl))
        (not-implemented "compile")
        ))

    (set asm-code (reverse asm-code))
    (set asm-pre  (reverse asm-pre))

    (assemble (append asm-pre asm-code))
    ))
