; indcall.sv

(include "std.sv")

(fun f()
    (puts "indirect call\n")
    )

(fun main ()
    (var g f)
    (g)
    )
