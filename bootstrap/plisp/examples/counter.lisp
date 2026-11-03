(define make-counter () (do
    (def n 0)
    (lambda () (do
        (set n (+ n 1))
        n
        )
    )))

(def counter1 (make-counter))
(def counter2 (make-counter))

(println (counter1)) ; => 1
(println (counter1)) ; => 2
(println (counter2)) ; => 1
(println (counter1)) ; => 3
(println (counter2)) ; => 2
