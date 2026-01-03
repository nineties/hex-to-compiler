; hello.s in x86

(entry start)

(section data)
(label msg)
    (ascii "Hello World!\n")

(const len (- here msg))

(section text)
(label start)
    (mov %eax 4)   ; SYS_WRITE
    (mov %ebx 1)   ; stdout
    (mov %ecx msg) ; addr
    (mov %edx len) ; length
    (int 0x80)

    (mov %eax 1)    ; SYS_EXIT
    (xor %ebx %ebx) ; status = 0
    (int 0x80)
