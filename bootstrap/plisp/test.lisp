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
        (put "Test failed: expected ")
        (print v2)
        (put " but got ")
        (print v1)
        (put " for ")
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
(expect (do
    (def x '(1 2 3))
    (setcar 2 x)
    x
    ) '(2 2 3))
(expect (do
    (def x '(1 2 3))
    (setcdr () x)
    x
    ) '(1))

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

(expect (switch 0
    (0 "zero")
    (1 "one")
    (2 "two")
    "other") "zero")

(expect (switch 3
    (0 "zero")
    (1 "one")
    (2 "two")
    "other") "other")


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
(expect (find (lambda (n) (> n 0)) '(-1 0 1)) 1)
(expect-error (find (lambda (n) (> n 0)) '(-1 0 0)))
(expect (first '(1 2 3)) 1)
(expect (second '(1 2 3)) 2)
(expect (third '(1 2 3)) 3)
(expect (nth 1 '(1 2 3)) 2)
(expect (reverse '(1 2 3)) '(3 2 1))
(expect-true (all? (lambda (n) (> n 0)) '(1 2 3)))
(expect-nil  (all? (lambda (n) (> n 0)) '(0 1 2)))
(expect-true (any? (lambda (n) (> n 0)) '(0 1 2)))
(expect-nil  (any? (lambda (n) (> n 0)) '(0 -1 -2)))

(expect       (assoc 1 '((0 "zero") (1 "one") (2 "two"))) "one")
(expect-error (assoc 3 '((0 "zero") (1 "one") (2 "two"))))
(expect (assoc-find (lambda (k) (> k 0)) '((0 0) (1 1) (-1 -1))) 1)
(expect (do
    (def ls '((0 "zero") (1 "one") (2 "two")))
    (assoc-set 0 "z" ls)
    ls) '((0 "z") (1 "one") (2 "two")))
(expect-error (do
    (def ls '((0 "zero") (1 "one") (2 "two")))
    (assoc-set 3 "z" ls)
    ))


(expect-true (= 123 123))
(expect-nil  (= 100 123))
(expect-nil  (= 123 "foo"))
(expect-true (= "foo" "foo"))
(expect-nil  (= "foo" "bar"))
(expect-true (= 'abc 'abc))
(expect-nil  (= 'abc 'def))
(expect-true (= '(1 2 3) '(1 2 3)))
(expect-nil  (= '(1 2 3) '(1 2 4)))

(expect (strlen "hello") 5)
(expect (getb "abc" 1) (char "b"))
(expect (do
    (def s "abc")
    (setb s 1 (char "B"))
    s) "aBc")

; parser for S-expression
; returns s-expr and remaining string
(expect (parse "(+ 1 2 3)abc") '((+ 1 2 3) "abc"))
; skip leading spaces and eliminates following spaces
(expect (parse "    (+ 1 2 3)   ") '((+ 1 2 3) ""))

(expect '(1 2 ,(+ 1 2)) '(1 2 ,(+ 1 2)))
(expect `(1 2 ,(+ 1 2)) '(1 2 3))
(expect ``(1 ,(+ 1 ,(+ 2 3))) '`(1 ,(+ 1 5)))


; === End of Tests ===

(put "Tests passed ") (print passed) (put "/") (print (+ passed failed)) (put "\n")
