; Assember written in plancklisp
; Copyright (C) 2026 nineties

(import "asm/core.lisp")

(def args (commandline-args))
(when (< (length args) 3) (abort "no input file"))
(def input-file (nth 2 args))

(def asm-code (read-sexp-list input-file))
(when (= asm-code 'error) (abort "parse error"))

(def data (assemble asm-code))
