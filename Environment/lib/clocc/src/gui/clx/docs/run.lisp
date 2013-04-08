(proclaim '(optimize (safety 2) (space 1) (speed 1) (debug 2)))
(defconstant *base-char* 'character)

#+CLISP
(setf *default-file-encoding* (make-encoding :charset charset:iso-8859-1))

(mapcar #'(lambda (x)
	    (load (compile-file (truename x))))
	'("clex.lisp"
	  "lalr.lisp"
	  "dtd.lisp"
	  "sgml-lex.lisp"
	  "sgml-parse.lisp"
	  "sgml-unparse.lisp"
	  "clxman.lisp"
	  ))

(format T "~&;; Now call (RUN) to generate the manual.~%")

; (run)

