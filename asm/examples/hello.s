; hello.s

(global _start)

(section data)
(label msg
    (.ascii "Hello World!\n")
    )

(def len (- here msg))

(section text)
(label _start
    (mov 1 %rax)                ; SYS_WRITE
    (mov 1 %rdi)                ; stdout
    (lea (mem msg %rip) %rsi)   ; addr
    (mov len %rdx)              ; length
    (syscall)

    (mov 60 %rax)   ; SYS_EXIT
    (xor %rdi %rdi) ; exit code 0
    (syscall)
    )
