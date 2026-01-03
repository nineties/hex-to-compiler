; Assember written in plancklisp
; Copyright (C) 2026 nineties

(import "asm/core.lisp")

(def args (commandline-args))
(when (< (length args) 4) (do
    (puts "usage: asm.lisp input-file output-file")
    (exit 1)
    ))
(def input-file (nth 2 args))
(def output-file (nth 3 args))

(def asm-code (read-sexp-list input-file))
(when (= asm-code 'error) (abort "parse error"))

(defvar (data size) (assemble asm-code))
(def r (write-file output-file data size))
(when (< r size) (abort "write error"))
