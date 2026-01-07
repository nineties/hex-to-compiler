; Simple compiler for C-like language written in plancklisp
; Copyright (C) 2026 nineties

(import "pcc/core.lisp")

(def args (commandline-args))
(when (< (length args) 4) (do
    (puts "usage: pcc.lisp input-file output-file")
    (exit 1)
    ))
(def input-file (nth 2 args))
(def output-file (nth 3 args))

(def pcc-code (read-sexp-list input-file))
(when (= pcc-code 'error) (abort "parse error"))

(defvar (data size) (compile pcc-code))
(def r (write-file output-file data size))
(when (< r size) (abort "write error"))
