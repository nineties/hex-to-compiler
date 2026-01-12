; std.sv

; === System Calls
(def SYS_EXIT 1)
(def SYS_READ 3)
(def SYS_WRITE 4)
(def SYS_OPEN 5)
(def SYS_CLOSE 6)
(def SYS_MUNMAP 91)
(def SYS_MMAP2 192)

(fun exit (n)
    (syscall SYS_EXIT n)
    )

(fun write (fd buf len)
    (syscall SYS_WRITE fd buf len)
    )

(def O_RDONLY   0)
(def O_WRONLY   1)
(def O_RDWR     2)
(def O_CREAT    0x40)
(def O_TRUNC    0x200)

(fun open (path flags)
    (return (syscall SYS_OPEN flags path 0x1a4))
    )

(fun close (fd)
    (syscall SYS_CLOSE fd)
    )

; === String
(fun strlen (ptr)
    (var len 0)
    (while (getb ptr) (do
        (+= len 1)
        (+= ptr 1)
        ))
    (return len)
    )

; === I/O

(def STDIN 0)
(def STDOUT 1)
(def STDERR 2)

(fun fputs (fd str)
    (write fd str (strlen str))
    (write fd "\n" 1)
    )

(fun puts (str)
    (fputs STDOUT str)
    )

(char[] 11 int2str_buf) ; max length of 32bit integer + 1 for '\0'
(fun int2char (n)
    (if (< n 10)
        (return (+ n 48))   ; 48 = '0'
        (return (+ n 87))   ; 87 = 'a' - 10
        )
    )
(fun fprint_int (fd n base)
    (var pos (+ int2str_buf 9))
    (setb pos (int2char (% n base)))
    (while (>= n base) (do
        (/= n base)
        (-= pos 1)
        (setb pos (int2char (% n base)))
        ))
    (write fd pos (- 10 (- pos int2str_buf)))
    )
