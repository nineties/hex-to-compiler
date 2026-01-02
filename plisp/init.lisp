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

(defmacro with-scope (body) `(if true ,body ()))

(defmacro when (cond body)
    `(if ,cond ,body ())
    )

(defmacro for (x ls body)
    `(with-scope (do
        (def r ,ls)
        (while (not (nil? r)) (do
            (def ,x (car r))
            ,body
            (set r (cdr r))
        )))))

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

(define abort (msg) (do (puts msg) (exit 1)))

; # List Functions
(define list args args)
(define length (e) (if (nil? e) 0 (+ 1 (length (cdr e)))))
(define member? (v ls) (cond
    ((nil? ls)      ())
    ((= (car ls) v) true)
    (true           (member? v (cdr ls)))
    ))
(define map (f ls) (cond
    ((nil? ls)      ())
    (true           (cons (f (car ls)) (map f (cdr ls))))
    ))

; # S-expression parser
(define parse-sexp-list (str) (do
    (def r (parse str))
    (cond
      ((= 'error r)     'error)
      ((nil? r)         ())
      (true             (cons (car r) (parse-sexp-list (cadr r))))
    )))

; # Import
(def imported-paths ())
(defmacro import (path) (when (not (member? path imported-paths)) (do
        (set imported-paths (cons path imported-paths))
        (def r (read-file path))
        (when (< (car r) 0) (abort "file not found"))
        (cons 'do (map (lambda (e) `(eval ',e)) (parse-sexp-list (cadr r))))
        )))
