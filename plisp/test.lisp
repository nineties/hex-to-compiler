; init script for plancklisp
; Copyright (C) 2026 nineties

(def passed 0)
(def failed 0)

(defmacro expect (e v)
  (do
    (def v1 (eval e))
    (def v2 (eval v))
    (if (= v1 v2)
      (+= passed 1)
      (do
        (type "Test failed: expected ")
        (print v2)
        (type " but got ")
        (print v1)
        (type " for ")
        (println e)
        (+= failed 1)
      ))))
(defmacro expect-true (e) `(expect ,e true))
(defmacro expect-nil (e) `(expect ,e ()))
(defmacro expect-error (e) `(expect ,e 'error))

; === Tests ===

(expect-true (nil? ()))
(expect-nil (nil? '(1 2 3)))
(expect-true (cons? '(1 2 3)))
(expect-nil (cons? ()))
(expect (cons 0 ()) '(0))
(expect (car '(1 2)) 1)
(expect (cdr '(1 2)) '(2))
(expect (cadr '(1 2)) 2)
(expect (cdar '((1 2) 3)) '(2))

(expect (when true 1) 1)
(expect (when () 1) ())
(expect (cond
    ((= 1 2)    0)
    ((= 2 2)    1)
    (true       2)
    ) 1)
(expect (cond
    ((= 1 2)    0)
    ((= 2 3)    1)
    (true       2)
    ) 2)

(expect (do
    (def sum 0)
    (for x '(1 2 3 4 5) (+= sum x))
    sum)
    15)

(expect (do (defvar x 1) x) 1)
(expect (do (defvar (x y) '(1 2)) x) 1)
(expect (do (defvar (x y) '(1 2)) y) 2)
(expect (do (defvar (x y z) '(1 2 3)) z) 3)

(expect (length ()) 0)
(expect (length '(1 2 3)) 3)
(expect (list 1 2 3) '(1 2 3))
(expect-true (member? 1 '(1 2 3)))
(expect-nil  (member? 4 '(1 2 3)))
(expect (map (lambda (n) (+ n 1)) '(1 2 3)) '(2 3 4))
(expect (nth 1 '(1 2 3)) 2)

(expect (do
    (def ls ())
    (set ls (acons 0 "zero" ls))
    (set ls (acons 1 "one" ls))
    (set ls (acons 2 "two" ls))
    (assoc 1 ls)) "one")
(expect-error (do
    (def ls ())
    (set ls (acons 0 "zero" ls))
    (set ls (acons 1 "one" ls))
    (set ls (acons 2 "two" ls))
    (assoc 3 ls)))

(expect-true (= 123 123))
(expect-nil  (= 100 123))
(expect-nil  (= 123 "foo"))
(expect-true (= "foo" "foo"))
(expect-nil  (= "foo" "bar"))
(expect-true (= 'abc 'abc))
(expect-nil  (= 'abc 'def))
(expect-true (= '(1 2 3) '(1 2 3)))
(expect-nil  (= '(1 2 3) '(1 2 4)))

; parser for S-expression
; returns s-expr and remaining string
(expect (parse "(+ 1 2 3)abc") '((+ 1 2 3) "abc"))
; skip leading spaces and eliminates following spaces
(expect (parse "    (+ 1 2 3)   ") '((+ 1 2 3) ""))
; returns 'error for broken s-expr
(expect (parse "(+ 1 2") 'error)

(expect '(1 2 ,(+ 1 2)) '(1 2 ,(+ 1 2)))
(expect `(1 2 ,(+ 1 2)) '(1 2 3))
(expect ``(1 ,(+ 1 ,(+ 2 3))) '`(1 ,(+ 1 5)))


; === End of Tests ===

(type "Tests passed ") (print passed) (type "/") (print (+ passed failed)) (type "\n")
