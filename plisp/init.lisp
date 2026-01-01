; init script for plancklisp
; Copyright (C) 2026 nineties

; # Primitive Functions
(def + prim:add)
(def - prim:sub)
(def * prim:mul)
(def / prim:div)
(def % prim:mod)
(def & prim:and)
(def | prim:or)
(def ^ prim:xor)
(def < prim:less)
(def u< prim:uless)
(def = prim:equal)
(def << prim:lshift)
(def >> prim:rshift)
(def asr prim:arshift)

(def cons prim:cons)
(def car prim:car)
(def cdr prim:cdr)
(def nil? prim:nil_p)

(def print prim:print)

(def caar (lambda (x) (car (car x))))
(def cadr (lambda (x) (car (cdr x))))
(def cdar (lambda (x) (cdr (car x))))
(def cddr (lambda (x) (cdr (cdr x))))
(def caaar (lambda (x) (car (caar x))))
(def caadr (lambda (x) (car (cadr x))))
(def cadar (lambda (x) (car (cdar x))))
(def caddr (lambda (x) (car (cddr x))))
(def cdaar (lambda (x) (cdr (caar x))))
(def cdadr (lambda (x) (cdr (cadr x))))
(def cddar (lambda (x) (cdr (cdar x))))
(def cdddr (lambda (x) (cdr (cddr x))))

; # Syntax Sugars

(def defmacro (macro args
    `(do
        (def ,(car args) ())
        (set ,(car args) (macro ,(cadr args) ,(caddr args)))
    )))

(defmacro define (name params body)
    `(do
        (def ,name ())
        (set ,name (lambda ,params ,body))
    ))

(defmacro when (cond body)
    `(if ,cond ,body ())
    )

; (cond
;   (condition0 body0)
;   (condition1 body1)
;   ...
;   )
(defmacro cond pairs
    (do
        (define f (pairs)
            (if (nil? pairs)
                ()
                `(if ,(caar pairs) ,(cadar pairs) ,(f (cdr pairs)))
            ))
        (f pairs)
    ))

; # Utility Functions
(define println (x) (do (print x) (print "\n")))

(define not (x) (= x ()))
(define > (a b) (< b a))
(define <= (a b) (not (> a b)))
(define >= (a b) (not (< a b)))
