; hello.v
(entry start)

(= msg "Hello World!\n")

(function start ()
    (syscall 4 1 msg 13)
    (syscall 1 0)
    )
    
