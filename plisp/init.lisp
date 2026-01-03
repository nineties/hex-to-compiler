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

(defmacro switch args (do
    (def e (car args))
    (def cases (cdr args))
    (def v (fresh-sym))
    (define f (cases) (cond
            ((nil? cases)   (abort "malformed switch statement"))
            ((nil? (cdr cases)) (car cases))
            (true `(if (= ,(caar cases) ,v) ,(cadar cases) ,(f (cdr cases))))
            ))
    `(do (def ,v ,e) ,(f cases))
    ))


(defmacro for (x ls body) (do
    (def v (fresh-sym))
    `(with-scope (do
        (def ,v ,ls)
        (while (not (nil? ,v)) (do
            (def ,x (car ,v))
            ,body
            (set ,v (cdr ,v))
        ))))))

(defmacro defvar (lhs rhs) (do
    (define iter (lhs v n) (cond
        ((nil? lhs)     ())
        (true           (cons
            `(def ,(car lhs) (nth ,n ,v))
            (iter (cdr lhs) v (+ n 1))
            ))))
    (cond
        ((sym? lhs)     `(def ,lhs ,rhs))
        ((cons? lhs)    (do
            (def v (fresh-sym))
            (cons 'do (cons `(def ,v ,rhs) (iter lhs v 0)))
            )))))

; # Utility Functions
(define println (e) (do (print e) (type "\n")))
(define puts (s) (do (type s) (type "\n")))
(define not (x) (= x ()))
(define != (a b) (not (= a b)))
(define > (a b) (< b a))
(define <= (a b) (not (> a b)))
(define >= (a b) (not (< a b)))

(defmacro += (x v) `(set ,x (+ ,x ,v)))
(defmacro -= (x v) `(set ,x (- ,x ,v)))
(defmacro *= (x v) `(set ,x (* ,x ,v)))
(defmacro /= (x v) `(set ,x (/ ,x ,v)))
(defmacro %= (x v) `(set ,x (% ,x ,v)))

(define abort (msg) (do (puts msg) (exit 1)))
(define not-implemented () (abort "not implemented"))
(define not-reachable () (abort "not reachable"))

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
(def first car)
(def second cadr)
(def third caddr)
(define nth (i ls) (cond
    ((nil? ls)  (abort "not enough length"))
    ((= i 0)    (car ls))
    (true       (nth (- i 1) (cdr ls)))
    ))

; # Assoc List
(define acons (k v ls) (cons (list k v) ls))
(define assoc (k ls) (cond
    ((nil? ls)          'error)
    ((= k (caar ls))    (cadar ls))
    (true               (assoc k (cdr ls)))
    ))
(defmacro acons! (k v ls) `(set ,ls (acons ,k ,v ,ls)))
(define assoc-set (k v ls) (cond
    ((nil? ls)          'error)
    ((= k (caar ls))    (setcar v (cdar ls)))
    (true               (assoc-set k v (cdr ls)))
    ))

; # S-expression parser
(define parse-sexp-list (str) (do
    (def r (parse str))
    (cond
      ((= 'error r) 'error)
      ((nil? r)     ())
      (true         (do
            (def s (parse-sexp-list (cadr r)))
            (if (= s 'error) 'error (cons (car r) s))
            ))
      )))

(define read-sexp-list (path) (do
    (defvar (size data) (read-file path))
    (when (< size 0) (abort "file not found"))
    (parse-sexp-list data)
    ))

; # Import
(def imported-paths ())
(defmacro import (path) (when (not (member? path imported-paths)) (do
        (set imported-paths (cons path imported-paths))
        (def es (read-sexp-list path))
        (cons 'do (map (lambda (e) `(eval ',e)) es))
        )))
