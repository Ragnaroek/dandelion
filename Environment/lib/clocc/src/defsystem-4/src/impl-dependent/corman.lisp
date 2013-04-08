;;; -*- Mode: CLtL -*-

(in-package "MK4")

(win:defwinapi run-dos-command ((name (:char *)))
  :return-type :int
  :library-name "msvcrt.dll"
  :entry-name "run-dos-command"
  :linkage-type :c)

(defun run-program (program &rest arguments)
  (win::run-dos-command (format nil "~A~@[~{ ~A~}~]" program arguments)))

;;; end of file -- corman.lisp --
