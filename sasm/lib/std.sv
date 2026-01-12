; std.sv

; === System Calls
(def SYS_EXIT 1)
(def SYS_READ 3)
(def SYS_WRITE 4)
(def SYS_OPEN 5)
(def SYS_CLOSE 6)
(def SYS_MUNMAP 91)
(def SYS_FSTAT 108)
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
    (return (syscall SYS_OPEN path flags 0x1a4))
    )

(fun close (fd)
    (syscall SYS_CLOSE fd)
    )

(fun fsize (fd)
    (char[] 64 buf) ; buf for struct stat
    (var r (syscall SYS_FSTAT fd buf))
    (if (< r 0) (return r))
    (return (get buf 5)) ; offset of st_size
    )

(def PROT_READ 0x1)
(def PROT_WRITE 0x2)
(def PROT_EXEC 0x4)
(def PROT_NONE 0x0)

(def MAP_SHARED 0x1)
(def MAP_PRIVATE 0x2)
(def MAP_FIXED 0x10)
(def MAP_ANONYMOUS 0x20)

(fun mmap2 (addr length prot flags fd pgoffset)
    (return (syscall SYS_MMAP2 0 length prot flags fd pgoffset))
    )

(fun munmap (addr length)
    (syscall SYS_MUNMAP addr length)
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

(fun fputs (fd str) (write fd str (strlen str)))
(fun puts (str) (fputs STDOUT str))
(fun eputs (str) (fputs STDERR str))

(fun int2char (n)
    (if (< n 10)
        (return (+ n 48))   ; 48 = '0'
        (return (+ n 87))   ; 87 = 'a' - 10
        )
    )

(fun fprint_int (fd n base)
    (char[] 11 int2str_buf) ; max length of 32bit integer + 1 for '\0'
    (var pos (+ int2str_buf 9))
    (setb pos (int2char (% n base)))
    (while (>= n base) (do
        (/= n base)
        (-= pos 1)
        (setb pos (int2char (% n base)))
        ))
    (write fd pos (- 10 (- pos int2str_buf)))
    )
