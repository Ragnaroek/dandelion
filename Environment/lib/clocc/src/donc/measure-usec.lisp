;;; -*- Mode: Common-Lisp; Package: USER; -*-

;; I, Donald Cohen (donc@{isi.edu,compsvcs.com,ap5.com} hereby
;; (1) assert that I am the author of this file and
;; (2) place it in the public domain.  Feb. 1, 2000.

(in-package :user)

#|
Measure-usec returns the time it takes to do something in microseconds,
e.g., (measure-usec '(length *features*))
will do (length *features*) enough times (compiled) to estimate with
reasonable accuracy how long it takes.

The bindings argument allows you to time things that use local variables:
(measure-usec '(+ x y) :bindings '((x 1) (y 2)))

The precision argument describes the desired accuracy, e.g., .1 means
within 10%.  Of course, a much smaller value, e.g., .01, may never
converge.  (However the max-time argument will override that.)

Other keyword arguments are a maximum time (seconds) allowed to do the
measurement, a minimal number of times to run the code, and the
compiler optimization settings, e.g., ((speed 3) (debug 0))

It's worth mentioning that compilers can sometimes optimize out more
than you want for such a test.  For instance, in the example above
they may realize that x and y never change during the computation, so
the + really computes a constant.  Even worse, they might recognize
that the loop is really useless.

Obviously there's overhead in measurement.  For very fast operations
the actual results should not be taken too seriously.  However the
difference between the results for similar forms does seem pretty
reliable.  Note, however, that the time for accessing variables may
depend on where they're bound and how.  So it's really only fair to
compare forms that access the same variables in the same way the same
number of times.

The return values are:
- the number of microseconds per iteration
- the number of iterations used to get this average
- the time (get-internal-run-time) that many iterations took
|#

(provide :measure-usec)

(defun measure-usec (code &key (precision .05) bindings (max-time 10)
                     (min-n 0) optimize &allow-other-keys)
  (funcall
   (compile
    nil
    `(lambda (&aux (time1 0) time2 (n 1) ,.bindings)
       (declare (optimize ,.optimize))
       (labels ((rtime (&aux (tmp (get-internal-run-time)))
                  (run n)
                  (- (get-internal-run-time)
                     tmp))
                (run (n) (dotimes (i n) ,code)))
         (setf time2 (rtime))
         (loop until
               (or (> time2 ,(* (/ max-time 4)
                                internal-time-units-per-second))
                   ;; if that iteration took > .25 max, all earlier
                   ;; ones might have taken > .25 max, and the next
                   ;; will take > .5 max giving a total > max
                   (and (> n ,min-n) (> time2 ,(/ 1.0 precision))
                        (< (* time1 ,(* 2.0 (- 1.0 precision)))
                           time2
                           (* time1 ,(* 2.0 (+ 1.0 precision)))))
                   ;; e.g., consider it to have converged within 10%
                   ;; if new time within 10% of double previous time
                   ;; first few times should take care of paging etc.
                   )
             do (setf n (+ n n) time1 time2 time2 (rtime))))
       (values (* 1000000.0 (/ time2 n internal-time-units-per-second))
               n time2)))))

;; show disassembly of code
(defun show-asm (code &key bindings optimize)
  (disassemble
   (compile nil
            `(lambda () (declare (optimize ,.optimize))
                     (let ,bindings ,code)))))

