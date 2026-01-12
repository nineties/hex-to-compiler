; fib.sv

(include "std.sv")

(fun fib (n)
    (if (< n 2)
        (return n)
        (return (+ (fib (- n 1)) (fib (- n 2))))
        )
    )

(fun main ()
    (putu (fib 20)) (puts "\n")
    )
