;;; Shell Access
;;;
;;; Copyright (C) 1999-2005 by Sam Steingold
;;; This is open-source software.
;;; GNU Lesser General Public License (LGPL) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code.
;;; See <URL:http://www.gnu.org/copyleft/lesser.html>
;;; for details and the precise copyright document.
;;;
;;; $Id: shell.lisp,v 1.18 2005/10/09 03:57:34 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/port/shell.lisp,v $

(eval-when (compile load eval)
  (require :port-ext (translate-logical-pathname "clocc:src;port;ext")))

(in-package :port)

(export '(run-prog pipe-output pipe-input close-pipe with-open-pipe))

;;;
;;; Shell interface
;;;

(defun run-prog (prog &rest opts &key args (wait t) &allow-other-keys)
  "Common interface to shell. Does not return anything useful."
  #+gcl (declare (ignore wait))
  (setq opts (remove-plist opts :args :wait))
  #+allegro (apply #'excl:run-shell-command (apply #'vector prog prog args)
                   :wait wait opts)
  #+(and clisp      lisp=cl)
  (apply #'ext:run-program prog :arguments args :wait wait opts)
  #+(and clisp (not lisp=cl))
  (if wait
      (apply #'lisp:run-program prog :arguments args opts)
      (lisp:shell (format nil "~a~{ '~a'~} &" prog args)))
  #+cmu (apply #'ext:run-program prog args :wait wait opts)
  #+gcl (apply #'si:run-process prog args)
  #+liquid (apply #'lcl:run-program prog args)
  #+lispworks (apply #'sys::call-system
                     (format nil "~a~{ '~a'~}~@[ &~]" prog args (not wait))
                     opts)
  #+lucid (apply #'lcl:run-program prog :wait wait :arguments args opts)
  #+sbcl (apply #'sb-ext:run-program prog args :wait wait opts)
  #-(or allegro clisp cmu gcl liquid lispworks lucid sbcl)
  (error 'not-implemented :proc (list 'run-prog prog opts)))

(defun pipe-output (prog &rest args)
  "Return an output stream which will go to the command."
  #+allegro (excl:run-shell-command (format nil "~a~{ ~a~}" prog args)
                                    :input :stream :wait nil)
  #+clisp (#+lisp=cl ext:make-pipe-output-stream
           #-lisp=cl lisp:make-pipe-output-stream
                     (format nil "~a~{ ~a~}" prog args))
  #+cmu (ext:process-input (ext:run-program prog args :input :stream
                                            :output t :wait nil))
  #+gcl (si::fp-input-stream (apply #'si:run-process prog args))
  #+lispworks (sys::open-pipe (format nil "~a~{ ~a~}" prog args)
                              :direction :output)
  #+lucid (lcl:run-program prog :arguments args :wait nil :output :stream)
  #+sbcl (sb-ext:process-input (sb-ext:run-program prog args :input :stream
                                                   :output t :wait nil))
  #-(or allegro clisp cmu gcl lispworks lucid sbcl)
  (error 'not-implemented :proc (list 'pipe-output prog args)))

(defun pipe-input (prog &rest args)
  "Return an input stream from which the command output will be read."
  #+allegro (excl:run-shell-command (format nil "~a~{ ~a~}" prog args)
                                    :output :stream :wait nil)
  #+clisp (#+lisp=cl ext:make-pipe-input-stream
           #-lisp=cl lisp:make-pipe-input-stream
                     (format nil "~a~{ ~a~}" prog args))
  #+cmu (ext:process-output (ext:run-program prog args :output :stream
                                             :error t :input t :wait nil))
  #+gcl (si::fp-output-stream (apply #'si:run-process prog args))
  #+lispworks (sys::open-pipe (format nil "~a~{ ~a~}" prog args)
                              :direction :input)
  #+lucid (lcl:run-program prog :arguments args :wait nil :input :stream)
  #+sbcl (sb-ext:process-output (sb-ext:run-program prog args :output :stream
                                                    :error t :input t :wait nil))
  #-(or allegro clisp cmu gcl lispworks lucid sbcl)
  (error 'not-implemented :proc (list 'pipe-input prog args)))

;;; Allegro CL: a simple `close' does NOT get rid of the process.
;;; The right way, of course, is to define a Gray stream `pipe-stream',
;;; define the `close' method and use `with-open-stream'.
;;; Unfortunately, not every implementation supports Gray streams, so we
;;; have to stick with this to further the portability.
;;; [2005] actually, all implementations support Gray streams (see gray.lisp)
;;; but Gray streams may be implemented inefficiently

(defun close-pipe (stream)
  "Close the pipe stream."
  (declare (stream stream))
  (close stream)
  ;; CLOSE does not close constituent streams
  ;; CLOSE-CONSTRUCTED-STREAM:ARGUMENT-STREAM-ONLY
  ;; http://www.lisp.org/HyperSpec/Issues/iss052.html
  (typecase stream
    (two-way-stream
     (close (two-way-stream-input-stream stream))
     (close (two-way-stream-output-stream stream))))
  #+allegro (sys:reap-os-subprocess))

(defmacro with-open-pipe ((pipe open) &body body)
  "Open the pipe, do something, then close it."
  `(let ((,pipe ,open))
    (declare (stream ,pipe))
    (unwind-protect (progn ,@body)
      (close-pipe ,pipe))))

(provide :port-shell)
;;; file shell.lisp ends here
