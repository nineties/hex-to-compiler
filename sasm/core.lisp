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

    (define align (n) (& (+ n 3) 0xfffffffc))

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

    (define comp-expr? (e) (&& (cons? e)
        (member? (car e) '(< > <= >= u< u> u<= u>= == !=))))

    (define to-ifnot-jump (op) (switch op
        ('<     'jge)
        ('>     'jle)
        ('<=    'jg)
        ('>=    'jl)
        ('u<    'jae)
        ('u>    'jbe)
        ('u<=   'ja)
        ('u>=   'jb)
        ('==    'jne)
        ('!=    'je)
        (not-reachable "to-ifnot-jump")
        ))

    (define! compile-expr (expr env) (cond
        ((int? expr)    (push expr))
        ((str? expr) (do
            (def label (fresh-label))
            (set asm-pre (cons `(label ,label) asm-pre))
            (set asm-pre (cons `(asciz ,expr) asm-pre))
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
                ('local_addr (do
                    (def offs (cadr pos))
                    (emit-asm '(mov %eax %ebp))
                    (emit-asm `(sub %eax ,(* 4 (+ offs 1))))
                    (push '%eax)
                    ))
                ('param (do
                    (def offs (cadr pos))
                    (push `(mem %ebp ,(+ 8 (* 4 offs))))
                    ))
                ('const (push (cadr pos)))
                ('label (push (cadr pos)))
                ('global (do
                    (def label (cadr pos))
                    (emit-asm `(mov %eax ,label))
                    (push '(mem %eax))
                    ))
                (not-implemented "compile-expr:var")
                )
            ))
        ((cons? expr)   (switch (car expr)
            ('+     (if (= (length expr) 2)
                        (compile-expr (cadr expr) env)
                        (compile-binop 'add expr env)))
            ('-     (if (= (length expr) 2)
                        (compile-binop 'sub `(- 0 ,(cadr expr)) env)
                        (compile-binop 'sub expr env)))
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
            ('&     (compile-binop 'and expr env))
            ('|     (compile-binop 'or expr env))
            ('^     (compile-binop 'xor expr env))
            ('<     (not-implemented "<"))
            ('>     (not-implemented ">"))
            ('<=    (not-implemented "<="))
            ('>=    (not-implemented ">="))
            ('==    (not-implemented "=="))
            ('!=    (not-implemented "!="))
            ('char  (push (getb (cadr expr) 0)))
            ('get  (do
                (def idx (if (cddr expr) (caddr expr) 0))
                (compile-expr (cadr expr) env)
                (compile-expr `(* 4 ,idx) env)
                (pop '%ecx)
                (pop '%eax)
                (emit-asm '(add %eax %ecx))
                (emit-asm '(mov %eax (mem %eax)))
                (push '%eax)
                ))
            ('set (do
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
            ('getb (do ; (getb ptr offs) or (getb ptr)
                (def idx (if (cddr expr) (caddr expr) 0))
                (compile-expr (cadr expr) env)
                (compile-expr idx env)
                (pop '%ecx)
                (pop '%eax)
                (emit-asm '(add %eax %ecx))
                (emit-asm '(movzx %eax (mem %eax)))
                (push '%eax)
                ))
            ('setb (do ; (setb ptr offs val) or (setb ptr val)
                (def ptr (cadr expr))
                (def idx 0)
                (def val 0)
                (if (= (length expr) 3)
                    (set val (caddr expr))
                    (do
                        (set idx (caddr expr))
                        (set val (cadddr expr))
                        ))
                (compile-expr val env)
                (compile-expr ptr env)
                (compile-expr idx env)
                (pop '%ecx)
                (pop '%eax)
                (emit-asm '(add %eax %ecx))
                (pop '%ecx)
                (emit-asm '(mov (mem %eax) %cl))
                (push '%ecx)
                ))
            ('syscall   (do
                (def regs '(%eax %ebx %ecx %edx %esi %edi %ebp))
                (def args (cdr expr))


                (when (>= (length args) 7) (push '%ebp))
                (for arg (reverse args) (compile-expr arg env))
                (while args (do
                    (pop (car regs))
                    (set regs (cdr regs))
                    (set args (cdr args))
                    ))
                (emit-asm '(int 0x80))
                (when (>= (length (cdr expr)) 7) (pop '%ebp))

                (push '%eax)
                ))
            (compile-call expr env)
                ))
        (true
            (do (println expr) (not-implemented "compile-expr")))
        ))

    (define compile-stmt (stmt env) (switch (car stmt)
        ('do    (do
            (for s (cdr stmt) (set env (compile-stmt s env)))
            env
            ))
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
        ('var   (do
            (def x (cadr stmt))
            (def e (caddr stmt))
            (compile-expr e env)
            (set env (acons x `(local ,nlocal) env))
            (+= nlocal 1)
            env
            ))
        ('=     (do
            (def x (cadr stmt))
            (def e (caddr stmt))
            (def pos (assoc x env))
            (when (= pos 'error) (do
                (put "undefined variable: ")
                (println x)
                (exit 1)))
            (compile-expr e env)
            (switch (car pos)
                ('local (do
                    (def offs (cadr pos))
                    (pop '%eax)
                    (emit-asm `(mov (mem %ebp ,(negate (* 4 (+ offs 1)))) %eax))
                    ))
                ('param (do
                    (def offs (cadr pos))
                    (pop '%eax)
                    (emit-asm `(mov (mem %ebp ,(+ 8 (* 4 offs))) %eax))
                    ))
                ('label (do
                    (def label (cadr pos))
                    (pop '%eax)
                    (emit-asm `(mov %ecx ,label))
                    (emit-asm `(mov (mem %ecx) %eax))
                    ))
                ('global (do
                    (def label (cadr pos))
                    (pop '%eax)
                    (emit-asm `(mov %ecx ,label))
                    (emit-asm `(mov (mem %ecx) %eax))
                    ))
                (not-reachable "compile-stmt:=")
                )
            env
            ))
        ('+=    (compile-stmt `(= ,(cadr stmt) (+ ,(cadr stmt) ,(caddr stmt))) env))
        ('-=    (compile-stmt `(= ,(cadr stmt) (- ,(cadr stmt) ,(caddr stmt))) env))
        ('*=    (compile-stmt `(= ,(cadr stmt) (* ,(cadr stmt) ,(caddr stmt))) env))
        ('/=    (compile-stmt `(= ,(cadr stmt) (/ ,(cadr stmt) ,(caddr stmt))) env))
        ('%=    (compile-stmt `(= ,(cadr stmt) (% ,(cadr stmt) ,(caddr stmt))) env))
        ('if    (cond
            ((= (length stmt) 3)    (compile-stmt `(if ,(nth 1 stmt) ,(nth 2 stmt) (do)) env))
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
        ('while (do
            ; begin:
            ;       cond
            ;       goto exit if false
            ;       body
            ;       goto begin
            ; exit:
            (def begin (fresh-label))
            (def exit (fresh-label))

            (def e (cadr stmt))
            (def body (caddr stmt))

            (when (not (comp-expr? e)) (set e `(!= ,e 0)))
            (defvar (op lhs rhs) e)

            (emit-asm `(label ,begin))
            (compile-expr lhs env)
            (compile-expr rhs env)
            (pop '%ecx)
            (pop '%eax)
            (emit-asm '(cmp %eax %ecx))
            (emit-asm `(,(to-ifnot-jump op) ,exit))
            (compile-stmt body env)
            (emit-asm `(jmp ,begin))
            (emit-asm `(label ,exit))
            env
            ))
        ('asm   (do
            (emit-asm (cadr stmt))
            env
            ))
        ('char[]    (do
            ; allocate local buffer to stack
            (def size (align (cadr stmt)))
            (def x (caddr stmt))
            (emit-asm `(sub %esp ,size))
            (+= nlocal (/ size 4))
            (set env (acons x `(local_addr ,(- nlocal 1)) env))
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

    (define eval (e) (cond
        ((int? e)       e)
        ((sym? e)   (do
            (def pos (assoc e env))
            (println pos)
            (not-implemented "compile:eval")
            ))
        ((cons? e) (switch (car e)
            ('+     (+ (eval (cadr e)) (eval (caddr e))))
            ('-     (- (eval (cadr e)) (eval (caddr e))))
            ('*     (* (eval (cadr e)) (eval (caddr e))))
            ('/     (/ (eval (cadr e)) (eval (caddr e))))
            ('%     (% (eval (cadr e)) (eval (caddr e))))
            ('&     (& (eval (cadr e)) (eval (caddr e))))
            ('|     (| (eval (cadr e)) (eval (caddr e))))
            ('^     (^ (eval (cadr e)) (eval (caddr e))))
            (not-reachable "compile:eval")
            ))
        (true   (not-reachable "compile:eval"))
        ))


    ; compile declarations
    (def included ())
    (define compile-topdecls (decls)
        (for decl decls (switch (car decl)
            ('fun   (compile-fundecl decl))
            ('include (do
                (def path (strcat "sasm/lib/" (cadr decl)))
                (when (not (member? path included)) (do
                    (compile-topdecls (read-sexp-list path))
                    (set included (cons path included))
                    ))
                ))
            ('def   (do
                (def x (cadr decl))
                (def val (eval (caddr decl)))
                (set env (acons x `(const ,val) env))
                ))
            ('char[] (do
                (def len (cadr decl))
                (def x (caddr decl))
                (emit-asm `(label ,x))
                (emit-asm `(comm ,len))
                (set env (acons x `(label ,x) env))
                ))
            ('long (do
                (def x (cadr decl))
                (emit-asm `(label ,x))
                (emit-asm `(long 0))
                (set env (acons x `(global ,x) env))
                ))
            (not-implemented "compile")
            )))
    (compile-topdecls decls)

    (set asm-code (reverse asm-code))
    (set asm-pre  (reverse asm-pre))

    (assemble (append asm-pre asm-code))
    ))
