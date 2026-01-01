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

(def car prim:car)
(def cdr prim:cdr)
(def print prim:print)

(def cadr (lambda (x) (car (cdr x))))
(def caddr (lambda (x) (car (cdr (cdr x)))))

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

; # Utility Functions
(define println (x) (do (print x) (print "\n")))

(define not (x) (= x ()))
(define > (a b) (< b a))
(define <= (a b) (not (> a b)))
(define >= (a b) (not (< a b)))
