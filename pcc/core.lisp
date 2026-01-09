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

    (define to-ifnot-jump (op) (switch op
        ('<     'jge)
        ('>     'jle)
        ('<=    'jg)
        ('>=    'jl)
        ('==    'jne)
        ('!=    'je)
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
                ('param (do
                    (def offs (cadr pos))
                    (push `(mem %ebp ,(+ 8 (* 4 offs))))
                    ))
                ('const (push (cadr pos)))
                (not-implemented "compile-expr:var")
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
            ('<     (not-implemented "<"))
            ('>     (not-implemented ">"))
            ('<=    (not-implemented "<="))
            ('>=    (not-implemented ">="))
            ('==    (not-implemented "=="))
            ('!=    (not-implemented "!="))
            ('load  (do
                (compile-expr (cadr expr) env)
                (compile-expr `(* 4 ,(caddr expr)) env)
                (pop '%ecx)
                (pop '%eax)
                (emit-asm '(add %eax %ecx))
                (emit-asm '(mov %eax (mem %eax)))
                (push '%eax)
                ))
            ('store (do
                (compile-expr (nth 3 expr) env)         ; val
                (compile-expr `(* 4 ,(nth 2 expr)) env) ; offs
                (compile-expr (nth 1 expr) env)         ; arr
                (pop '%eax)
                (pop '%ecx)
                (emit-asm '(add %eax %ecx))
                (pop '%ecx)
                (emit-asm '(mov (mem %eax) %ecx))
                (push '%ecx)
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

            ; function epilogue
            (emit-asm '(mov %esp %ebp))
            (emit-asm '(pop %ebp))

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
        ('if    (cond
            ((= (length stmt) 3)    (compile-stmt `(if ,(nth 1 stmt) ,(nth 2 stmt), (do) env)))
            (true
                (do
                    (defvar (op lhs rhs) (nth 1 stmt))
                    (def t (nth 2 stmt))
                    (def e (nth 3 stmt))
                    (def Le (fresh-label))
                    (def Ljoin (fresh-label))
                    (compile-expr lhs env)
                    (compile-expr rhs env)
                    (pop '%ecx)
                    (pop '%eax)
                    (emit-asm '(cmp %eax %ecx))
                    (emit-asm `(,(to-ifnot-jump op) ,Le))
                    (compile-stmt t env)
                    (emit-asm  `(jmp ,Ljoin))
                    (emit-asm `(label ,Le))
                    (compile-stmt e env)
                    (emit-asm `(label ,Ljoin))
                    env
                ))
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

        (def newenv env)
        (def i 0)
        (for param params (do
            (set newenv (acons param `(param ,i) newenv))
            (+= i 1)
            ))

        (emit-asm `(label ,label))

        ; function prologue
        (emit-asm '(push %ebp))
        (emit-asm '(mov %ebp %esp))

        (set nlocal 0)
        (for stmt body (set newenv (compile-stmt stmt newenv)))
        (compile-stmt '(return) newenv)
        ))

    ; entry point
    (emit-asm '(entry _start))
    (compile-fundecl '(fun _start ()
        (asm (add %eax %esp))
        (asm (add %eax 8))
        (asm (push %eax))   ; argv
        (asm (push (mem %esp 8))) ; argc
        (main)
        (asm (add %esp 8))
        (syscall 1 0)   ; exit(0)
        ))

    ; compile declarations
    (def included ())
    (define compile-topdecls (decls)
        (for decl decls (switch (car decl)
            ('fun   (compile-fundecl decl))
            ('include (do
                (def path (strcat "pcc/lib/" (cadr decl)))
                (when (not (member? path included)) (do
                    (compile-topdecls (read-sexp-list path))
                    (set included (cons path included))
                    ))
                ))
            ('def   (do
                (def x (cadr decl))
                (def val (caddr decl))
                (set env (acons x `(const ,val) env))
                ))
            (not-implemented "compile")
            )))
    (compile-topdecls decls)

    (set asm-code (reverse asm-code))
    (set asm-pre  (reverse asm-pre))
    (println asm-code)

    (assemble (append asm-pre asm-code))
    ))
