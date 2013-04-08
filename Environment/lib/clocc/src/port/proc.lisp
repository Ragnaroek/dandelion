;;; LispM-style portable multiprocessing
;;;
;;; Copyright (C) 1999-2003 by Sam Steingold
;;; This is open-source software.
;;; GNU Lesser General Public License (LGPL) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code.
;;; See <URL:http://www.gnu.org/copyleft/lesser.html>
;;; for details and the precise copyright document.
;;;
;;; $Id: proc.lisp,v 1.17 2005/05/23 15:18:15 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/port/proc.lisp,v $
;;;
;;; This is based on the code donated by Cycorp, Inc. to the public domain.
;;;
;;; More-or-less portable Lisp-machine style multithreading.  Under
;;; ports that do not support light-weight multiple threads we either
;;; signal errors or do something harmless.
;;;
;;; Meta stuff
;;; +THREADS-P+
;;; :THREADS
;;;
;;; Process creation
;;; MAKE-PROCESS (name function &rest args)
;;;
;;; Scheduling.
;;; PROCESS-WAIT (whostate predicate &rest args)
;;; PROCESS-WAIT-WITH-TIMEOUT (timeout whostate predicate &rest args)
;;; WITH-TIMEOUT ((timeout &body timeout-forms) &body body-forms)
;;; WITHOUT-SCHEDULING (&body body)
;;; PROCESS-YIELD
;;;
;;; Process manipulation
;;; KILL-PROCESS (process)
;;; INTERRUPT-PROCESS (process function &rest args)
;;; RESTART-PROCESS (process)
;;;
;;; Process information
;;; PROCESSP (object)
;;; PROCESS-NAME (process)
;;; PROCESS-ACTIVE-P (process)
;;; PROCESS-WHOSTATE (process)
;;; CURRENT-PROCESS ()
;;; ALL-PROCESSES ()
;;; SHOW-PROCESSES ()
;;;
;;; Locks
;;; MAKE-LOCK (&key name)
;;; GET-LOCK (lock)
;;; GIVEUP-LOCK (lock)
;;; WITH-LOCK ((lock) &rest body)

(eval-when (compile load eval)
  (require :port-ext (translate-logical-pathname "clocc:src;port;ext"))
  #+CormanLisp (require 'THREADS))

(in-package :port)

(export '(+threads-p+ make-process
          process-wait process-wait-with-timeout
          with-timeout y-or-n-p-timeout maybe-y-or-n-p
          without-scheduling process-yield
          kill-process interrupt-process restart-process
          processp process-name process-active-p process-whostate
          current-process all-processes show-processes
          make-lock get-lock giveup-lock with-lock))

;;;
;;; process creatioon
;;;

#+(or (and Allegro multiprocessing)
      (and CMU mp)
      CormanLisp
      Genera
      LispWorks
      (and Lucid multitasking)
      MCL
      scl)

(eval-when (compile load eval)
  (pushnew :threads *features*))

(defconstant +threads-p+ #+threads t #-threads nil
  "Whether the implementation has the ability to do
multiple processes in a single address space.")

(defun make-process (name function &rest args)
  "Create a new process and start it."
  #+CormanLisp (declare (ignore name))
  #+Allegro (apply #'mp:process-run-function name function args)
  #+CMU (mp:make-process (lambda () (apply function args)) :name name)
  #+CormanLisp (th:create-thread (lambda () (apply function args)))
  #+Genera
  (process:make-process name
                        :initial-function function
                        :initial-function-arguments (copy-list args)
                        ;; It might be nice to provide an option, but this is
                        ;; almost always what we want.
                        :warm-boot-action
                        #'process:process-warm-boot-delayed-restart)
  #+LispWorks (apply #'mp:process-run-function name '() function args)
  #+Lucid (lcl:make-process :function function :name name :args args)
  ;; CCL:RESTART-PROCESS appears to flush the process in
  ;; MCL 3.0, so we have to roll our own.
  #+MCL (flet ((process-wrapper ()
                 (loop
                  (catch 'restart-process
                    (return-from process-wrapper
                      (apply function args))))))
          (ccl:process-run-function name #'process-wrapper))
  #+scl (thread:thread-create (lambda () (apply function args)) :name name)
  #-threads
  (error 'not-implemented :proc (list 'make-process name function args)))

;;; N.B.: There is no guarantee that the wait function
;;; will run in the stack group of the waiting process.  So
;;; you can't depend on dynamic bindings and catches being in effect.
(defun process-wait (whostate predicate &rest args)
  "Sleep until PREDICATE becomes true."
  #+Allegro (apply #'mp:process-wait whostate predicate args)
  #+CMU (mp:process-wait whostate (lambda () (apply predicate args)))
  #+CormanLisp FIXME
  ;;; The scheduler in Genera sometimes calls a wait function twice
  ;;; before actually returning.  If the wait function returns true the
  ;;; first time but NIL the second, we keep on waiting.  This is bad
  ;;; because we like to write wait functions that modify their state
  ;;; such that they will return NIL the second time around.
  #+Genera
  (let ((done-waiting-p nil))
    (flet ((wait ()
             (or done-waiting-p
                 (when (apply predicate args)
                   (setf done-waiting-p t)
                   t))))
      (declare (dynamic-extent #'wait))
      (process:process-wait whostate #'wait)
      (setf done-waiting-p nil)))
  #+LispWorks (apply #'mp:process-wait whostate predicate args)
  #+Lucid     (apply #'lcl:process-wait whostate predicate args)
  #+MCL       (apply #'ccl:process-wait whostate
                     (if (symbolp predicate)
                         (symbol-function predicate)
                         predicate)
                     args)
  #+scl FIXME
  #-threads
  (error 'not-implemented :proc (list 'process-wait whostate predicate args)))

(defun process-wait-with-timeout (timeout whostate predicate &rest args)
  "Sleep until PREDICATE becomes true, or for TIMEOUT seconds,
whichever comes first."
  #+Allegro
  (apply #'mp:process-wait-with-timeout whostate timeout predicate args)
  #+CMU (mp:process-wait-with-timeout
         whostate timeout (lambda () (apply predicate args)))
  #+CormanLisp FIXME
  #+Genera
  (apply #'process:process-wait-with-timeout whostate timeout predicate args)
  #+LispWorks
  (apply #'mp:process-wait-with-timeout whostate timeout predicate args)
  #+Lucid
  (apply #'lcl:process-wait-with-timeout whostate timeout predicate args)
  #+MCL (apply #'ccl:process-wait-with-timeout whostate (* timeout 60)
               (if (symbolp predicate) (symbol-function predicate) predicate)
               args)
  #+scl FIXME
  #-threads
  (error 'not-implemented :proc (list 'process-wait-with-timeout timeout
                                      whostate predicate args)))

;;;
;;; with-timeout & y-or-n-p
;;;

(defun with-timeout-f (timeout bodyf timeoutf)
  (block timeout
    (let ((done nil) (process (current-process)))
      (make-process (format nil "Timeout monitor for ~A" process)
                    (lambda ()
                      (sleep timeout)
                      (unless done
                        (interrupt-process
                         process (lambda ()
                                   (return-from timeout
                                     (funcall timeoutf)))))))
      (unwind-protect (funcall bodyf)
        (setf done t)))))

(defmacro with-timeout ((seconds &body timeout-forms) &body body)
  "Execute BODY; if execution takes more than SECONDS seconds, terminate
and evaluate TIMEOUT-FORMS."
  #-threads (declare (ignore seconds timeout-forms))
  ;;#+(or (and allegro multiprocessing) (and cmu mp))
  ;;`(mp:with-timeout (,seconds ,@timeout-forms) ,@body)
  #+threads
  (with-gensyms ("WT-" bodyf timeoutf)
    `(flet ((,bodyf () ,@body)
            (,timeoutf () ,@timeout-forms))
      (with-timeout-f ,seconds #',bodyf #',timeoutf)))
  #-threads `(progn ,@body))

(defun y-or-n-p-timeout (seconds default &rest args)
  "`y-or-n-p' with timeout."
  (declare (ignorable seconds default))
  (with-timeout (seconds (format t "[Timed out] ~:[NO~;YES~]~%" default)
                         default)
    (apply #'y-or-n-p args)))

(defun maybe-y-or-n-p (val &rest args)
  "Maybe ask a question - if VAL is neither T nor NIL."
  (or (eq val t) (and val (apply #'y-or-n-p args))))

;;;
;;; utilities
;;;

(defmacro without-scheduling (&body body)
  "Run BODY with interrupts disabled."
  #+Allegro    `(mp:without-scheduling ,@body)
  #+CMU        `(mp:without-scheduling ,@body)
  #+CormanLisp FIXME
  #+Genera     `(process:with-no-other-processes ,@body)
  #+LispWorks  `(mp:without-interrupts ,@body)
  #+Lucid      `(lcl:with-scheduling-inhibited ,@body)
  #+MCL        `(ccl:without-interrupts ,@body)
  #+scl FIXME
  #-threads    `(progn ,@body))

(defun process-yield ()
  "Yields the current process' remaining time slice
and allows other processes to run."
  #+Allegro    (mp:process-allow-schedule)
  #+CMU        (mp:process-yield)
  #+CormanLisp FIXME
  #+Genera     (scl:process-allow-schedule)
  #+LispWorks  (mp:process-allow-scheduling)
  #+Lucid      (lcl:process-allow-schedule)
  #+(and MCL (not openmcl)) (ccl:process-yield)
  #+openmcl    (ccl:process-allow-schedule)
  #+scl FIXME
  #-threads    (error 'not-implemented :proc (list 'process-yield)))

(defun kill-process (process)
  "Kill PROCESS."
  #+Allegro    (mp:process-kill process)
  #+CMU        (mp:destroy-process process)
  #+CormanLisp (th:terminate-thread process)
  #+Genera     (process:process-kill process)
  #+LispWorks  (mp:process-kill process)
  #+Lucid      (lcl:kill-process process)
  #+MCL        (ccl:process-kill process)
  #+scl        (thread:destroy-thread process)
  #-threads    (error 'not-implemented :proc (list 'kill-process process)))

(defun interrupt-process (process function &rest args)
  "Run FUNCTION in PROCESS."
  #+Allegro   (apply #'mp:process-interrupt process function args)
  #+CMU       (mp:process-interrupt process (lambda () (apply function args)))
  #+CormanLisp FIXME
  #+Genera    (apply #'process:process-interrupt process function args)
  #+LispWorks (apply #'mp:process-interrupt process function args)
  #+Lucid     (apply #'lcl:interrupt-process process function args)
  #+MCL       (apply #'ccl:process-interrupt process function args)
  #+scl       (thread:thread-interrupt process
                                       (lambda () (apply function args)))
  #-threads   (error 'not-implemented :proc (list 'interrupt-process process
                                                  function args)))

(defun restart-process (process)
  "Throw out PROCESS' current computation
and reapply its initial function to its arguments."
  #+Allegro    (mp:process-reset process)
  #+CMU        (mp:restart-process process)
  #+CormanLisp FIXME
  #+Genera     (process:process-reset process)
  #+LispWorks  (mp:process-reset process)
  #+Lucid      (lcl:restart-process process)
  ;; CCL:RESTART-PROCESS appears to flush the process in
  ;; MCL 3.0, so we have to roll our own.
  #+MCL (interrupt-process process (lambda () (throw 'restart-process nil)))
  #+scl        FIXME
  #-threads (error 'not-implemented :proc (list 'restart-process process)))

(defun process-p (object)
  "T if OBJECT is a process."
  #+Allegro    (mp::process-p object)
  #+CMU        (mp:processp object)
  #+CormanLisp FIXME
  #+Genera     (process:process-p object)
  #+LispWorks  (mp:process-p object)
  #+Lucid      (lcl:processp object)
  #+MCL        (ccl::processp object)
  #+scl        (typep object 'thread:thread)
  #-threads    (error 'not-implemented :proc (list 'process-p object)))

(defun process-name (process)
  "PROCESS' name."
  #+Allegro    (mp:process-name process)
  #+CMU        (mp:process-name process)
  #+CormanLisp FIXME
  #+Genera     (process:process-name process)
  #+LispWorks  (mp:process-name process)
  #+Lucid      (lcl:process-name process)
  #+MCL        (ccl:process-name process)
  #+scl        (thread:thread-name process)
  #-threads    (error 'not-implemented :proc (list 'process-name process)))

(defun process-active-p (process)
  "T if PROCESS is doing soemthing."
  #+Allegro    (mp:process-active-p process)
  #+CMU        (mp:process-active-p process)
  #+CormanLisp FIXME
  #+Genera     (process:process-active-p process)
  #+LispWorks  (and (mp:process-run-reasons process)
                    (not (mp:process-arrest-reasons process)))
  #+Lucid      (lcl:process-active-p process)
  #+(and MCL (not OpenMCL))        (ccl:process-active-p process)
  #+OpenMCL    (string-equal (ccl:process-whostate process) "active")
  #+scl        (let (activep)
                 (thread::map-over-threads
                  (lambda (thread)
                    (when (eq thread process) (setq activep t))))
                 activep)
  #-threads    (error 'not-implemented :proc (list 'process-active-p process)))

(defun process-whostate (process)
  "Returns a string describing PROCESS' current status."
  #+Allegro    (mp:process-whostate process)
  #+CMU        (mp:process-whostate process)
  #+CormanLisp FIXME
  #+Genera     (process:process-whostate process)
  #+LispWorks  (mp:process-whostate process)
  #+Lucid      (lcl:process-whostate process)
  #+MCL        (ccl:process-whostate process)
  #+scl        FIXME
  #-threads    (error 'not-implemented :proc (list 'process-whostate process)))

(defun process-state (process)
  "Returns a symbol describing PROCESS' current state."
  #+Allegro    (car (mp:process-run-reasons process))
  #+CMU        (mp:process-state process)
  #+CormanLisp FIXME
  #+Genera     (process:process-state process)
  #+LispWorks  (mp:process-run-reasons process)
  #+Lucid      (lcl:process-state process)
  #+(and MCL (not openmcl)) (ccl:process-state process)
  #+openmcl    (intern (ccl:process-whostate process))
  #+scl        FIXME
  #-threads    (error 'not-implemented :proc (list 'process-state process)))

(defun current-process ()
  "The current process."
  #+Allegro    mp:*current-process*
  #+CMU        mp:*current-process*
  #+CormanLisp ccl:*current-thread-handle*
  #+Genera     scl:*current-process*
  #+LispWorks  mp:*current-process*
  #+Lucid      lcl:*current-process*
  #+MCL        ccl:*current-process*
  #+scl        thread::*thread*
  #-threads    (error 'not-implemented :proc (list 'current-process)))

(defun all-processes ()
  "A list of all processes."
  #+Allegro    mp:*all-processes*
  #+CMU        (mp:all-processes)
  #+CormanLisp FIXME
  #+Genera     process:*all-processes*
  #+LispWorks  (mp:list-all-processes)
  #+Lucid      lcl:*all-processes*
  #+(and MCL (not OpenMCL))        ccl:*all-processes*
  #+OpenMCL    (ccl:all-processes)
  #+scl        (let (threads)
                 (thread:map-over-threads
                  (lambda (thread)
                    (push thread threads)))
                 threads)
  #-threads    (error 'not-implemented :proc (list 'all-processes)))

(defun show-processes (&key (stream *standard-output*))
  "Print out info on all processes."
  #+Genera (progn
             (fresh-line stream)
             (si:com-show-processes :output-destination
                                    (list (si:follow-syn-stream stream))))
  #+scl (let ((*standard-output* stream)) (thread:show-threads))
  #-(or Genera scl)
  (let ((info '()))
    (dolist (process (all-processes))
      (flet ((get-value (accessor)
               (multiple-value-bind (value errorp)
                   (ignore-errors
                     (princ-to-string (funcall accessor process)))
                 (if errorp "<<error>>" value))))
        (push (list (get-value #'process-name)
                    (get-value #'process-whostate)
                    (get-value (lambda (p) (string-capitalize
                                            (princ-to-string
                                             (process-state p))))))
              info)))
    (flet ((max-length (column)
             (let ((max 0))
               (dolist (line info)
                 (let ((value (length (nth column line))))
                   (when (> value max)
                     (setf max value))))
               max)))
      (let* ((name-max     (+ (max-length 0) 2))
             (whostate-max (+ name-max (max-length 1) 2))
             ;; (state-max    (+ whostate-max (max-length 2) 2))
             )
        (format stream "~&Name~vTWhostate~vTState~%" name-max whostate-max)
        (dolist (line info)
          (format stream "~A~vT~A~vT~A~%" (first line) name-max (second line)
                  whostate-max  (third line)))))))

;;;
;;; Locks
;;;
;;; (MCL implementation actually uses MCL `queues', which are just
;;;  locks where the order in which processes block on a queue is the
;;;  order in which they unblock.)

(defun make-lock (&key name)
  "Creates a new lock."
  #+Allegro    (mp:make-process-lock :name name)
  #+CMU        (mp:make-lock name)
  #+CormanLisp FIXME
  #+Genera     (process:make-lock name)
  #+LispWorks  (mp:make-lock :name name)
  #+Lucid      FIXME
  #+(and MCL (not OpenMCL))        (ccl:make-process-queue name)
  #+OpenMCL    (ccl:make-lock name)
  #+scl        (thread:make-lock name)
  #-threads    (error 'not-implemented :proc (list 'make-lock name)))

(defun get-lock (lock)
  "Claims a lock, blocking until the current process can get it."
  #+Allegro    (mp:process-lock lock)
  #+CMU        (mp::lock-wait lock (mp:process-whostate mp:*current-process*))
  #+CormanLisp FIXME
  #+Genera     FIXME
  #+LispWorks  (mp:claim-lock lock)
  #+Lucid      (lcl:process-lock lock)
  #+(and MCL (not OpenMCL))        (ccl:process-enqueue lock)
  #+OpenMCL    (ccl:grab-lock lock)
  #+scl        FIXME
  #-threads    (error 'not-implemented :proc (list 'get-lock lock)))

(defun giveup-lock (lock)
  "Gives up possession of a lock."
  #+Allegro    (mp:process-unlock lock)
  #+CMU        (setf (mp::lock-process lock) nil)
  #+CormanLisp FIXME
  #+Genera     FIXME
  #+LispWorks  (mp:release-lock lock)
  #+Lucid      (lcl:process-unlock lock)
  #+(and MCL (not OpenMCL))        (ccl:process-dequeue lock)
  #+OpenMCL    (ccl:release-lock lock)
  #+scl        FIXME
  #-threads    (error 'not-implemented :proc (list 'giveup-lock lock)))

(defmacro with-lock ((lock) &rest body)
  "This macro executes the body with LOCK locked."
  #-threads (declare (ignore lock))
  #+Allegro    `(mp:with-process-lock (,lock) ,@body)
  #+CMU        `(mp:with-lock-held (,lock) ,@body)
  #+CormanLisp FIXME
  #+Genera     `(process:with-lock (,lock) ,@body)
  #+LispWorks  `(mp:with-lock (,lock) ,@body)
  #+Lucid      `(lcl:with-process-lock (,lock) ,@body)
  #+(and MCL (not OpenMCL))    `(ccl:with-process-enqueued (,lock) ,@body)
  #+OpenMCL    `(ccl:with-lock-grabbed (,lock) ,@body)
  #+scl        `(thread:with-lock-held (,lock) ,@body)
  #-threads    `(progn ,@body))

(provide :port-proc)
;;; proc.lisp end here
