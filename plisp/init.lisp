; init script for plancklisp
; Copyright (C) 2026 nineties

(def true 'true)

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
(define println (e) (do (print e) (type "\n")))
(define puts (s) (do (type s) (type "\n")))
(define not (x) (= x ()))
(define > (a b) (< b a))
(define <= (a b) (not (> a b)))
(define >= (a b) (not (< a b)))

(defmacro += (x v) `(set ,x (+ ,x ,v)))
(defmacro -= (x v) `(set ,x (- ,x ,v)))
(defmacro *= (x v) `(set ,x (* ,x ,v)))
(defmacro /= (x v) `(set ,x (/ ,x ,v)))
(defmacro %= (x v) `(set ,x (% ,x ,v)))

(define error (text) (do (puts text) (exit 1)))

; # List Functions
(define length (e) (cond
    ((nil? e)   0)
    ((cons? e)  (+ 1 (length (cdr e))))
    (true       (error "cons is expected"))
    ))
