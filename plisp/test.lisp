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
        (print v1)
        (type " but got ")
        (print v2)
        (type " for ")
        (println e)
        (+= failed 1)
      ))))
(defmacro expect-true (e) `(expect ,e true))
(defmacro expect-nil (e) `(expect ,e ()))

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

(expect (length ()) 0)
(expect (length '(1 2 3)) 3)
(expect (list 1 2 3) '(1 2 3))
(expect-true (member? 1 '(1 2 3)))
(expect-nil  (member? 4 '(1 2 3)))

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
