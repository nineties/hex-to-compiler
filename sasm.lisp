; Simple compiler for C-like language written in plancklisp
; Copyright (C) 2026 nineties

(import "sasm/core.lisp")

(def args (commandline-args))
(when (< (length args) 4) (do
    (puts "usage: sasm.lisp input-file output-file")
    (exit 1)
    ))
(def input-file (nth 2 args))
(def output-file (nth 3 args))

(def sasm-code (read-sexp-list input-file))
(when (= sasm-code 'error) (abort "parse error"))

(defvar (data size) (compile sasm-code))
(def r (write-file output-file data size))
(when (< r size) (abort "write error"))
