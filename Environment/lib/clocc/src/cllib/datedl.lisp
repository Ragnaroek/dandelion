;;; Dated Lists - extracted from date.lisp
;;;
;;; Copyright (C) 1997-2004 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: datedl.lisp,v 1.14 2005/01/27 23:02:49 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/datedl.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `index-t'
  (require :cllib-withtype (translate-logical-pathname "cllib:withtype"))
  ;; `date'
  (require :cllib-date (translate-logical-pathname "cllib:date"))
  ;; `with-printing'
  (require :cllib-laser (translate-logical-pathname "cllib:laser"))
  ;; `mean', `standard-deviation', `standard-deviation-relative', `volatility'
  ;; `lincom', `regress', `mean-weighted', `s/', `rel-diff'
  (require :cllib-math (translate-logical-pathname "cllib:math"))
  ;; `keyword-concat'
  (require :cllib-symb (translate-logical-pathname "cllib:symb")))

(in-package :cllib)

(export '(dated-list mk-dl dl-nth-date dated-list-name dl-nth-slot dl-shift
          copy-dated-list dl-endp dl-len dl-ll dl-date))

;;;
;;; Dated List
;;;

(defstruct (dated-list)
  "A dated list of records."
  (code nil :type symbol)       ; the code (usually, a 2 letter symbol)
  (name "??" :type simple-string) ; the name of the data
  (date 'date :type symbol)     ; the date accessor
  (val 'value :type symbol)     ; the value accessor
  (chg 'identity :type symbol)  ; the change accessor
  (misc nil :type (or symbol function)) ; the miscellaneous accessor
  (fl nil :type list)           ; the full list (list of lists)
  (cl nil :type list)           ; the current list (sublist of fl)
  (cp nil :type list))          ; the current position (sublist of (cdar cl))

(defmethod code ((dl dated-list)) (dated-list-code dl))

(defconst +bad-dl+ dated-list (make-dated-list)
  "*The convenient constant for init.")

(defsubst dl-ll (dl)
  "The current list."
  (declare (type dated-list dl)) (dated-list-cp dl))

(defsetf dl-ll (dl) (ls)
  "Set the dated list's current list."
  `(setf (dated-list-cp ,dl) ,ls))

(defsetf dl-fl (dl) (ls)
  "Init FL and CL."
  (with-gensyms ("DL-FL-" xx yy)
    `(let ((,xx ,dl) (,yy ,ls))
      (setf (dated-list-cl ,xx) ,yy (dated-list-fl ,xx) ,yy
       (dated-list-cp ,xx) (cdar ,yy)))))

(defun dl-reset (dl)
  "Reset CL to FL."
  (declare (type dated-list dl))
  (setf (dated-list-cl dl) (dated-list-fl dl)
        (dated-list-cp dl) (cdar (dated-list-fl dl)))
  dl)

(defun mk-dl (ll &rest others)
  "Make and init dated list."
  (let ((dl (apply #'make-dated-list :fl ll others)))
    (dl-reset dl)))

(declaim (ftype (function (dated-list) (values date-f-t)) dl-date))
(defsubst dl-date (dl)
  "Return the DATE function of the dated list DL."
  (declare (type dated-list dl))
  (fdefinition (dated-list-date dl)))

(declaim (ftype (function (dated-list) (values (function (t) double-float)))
                dl-val dl-chg))
(defsubst dl-val (dl)
  "Return the VAL function of the dated list DL."
  (declare (type dated-list dl))
  (fdefinition (dated-list-val dl)))

(defsubst dl-chg (dl)
  "Return the CHG function of the dated list DL."
  (declare (type dated-list dl))
  (fdefinition (dated-list-chg dl)))

(declaim (ftype (function (dated-list) (values function)) dl-misc))
(defsubst dl-misc (dl)
  "Return the MISC function of the dated list DL."
  (declare (type dated-list dl))
  (fdefinition (dated-list-misc dl)))

(declaim (ftype (function (dated-list symbol)
                          (values (function (t) double-float)))
                dl-slot))
(defsubst dl-slot (dl slot)
  "Return the SLOT function of the dated list DL."
  (declare (type dated-list dl) (type (member val chg) slot))
  (fdefinition (slot-value dl slot)))

(declaim (ftype (function (dated-list) (values fixnum)) dl-len dl-full-len))
(defsubst dl-len (dl)
  "Return the length of the dated list."
  (declare (type dated-list dl))
  (length (dl-ll dl)))

(defun dl-full-len (dl)
  "Return the full length of the dated list."
  (declare (type dated-list dl))
  (reduce #'+ (dated-list-fl dl) :key (compose length cdr)))

(defsubst dl-endp (dl)
  "Check for the end of the dated list."
  (declare (type dated-list dl))
  (and (endp (dl-ll dl)) (endp (cdr (dated-list-cl dl)))))

(defun dl-nth (dl &optional (nn 0))
  "Return the Nth record of the dated list.
Optional second arg defaults to 0. If it is negative, count from the end,
so that -1 corresponds to the last record."
  (declare (type dated-list dl) (fixnum nn))
  (if (minusp nn) (car (last (dl-ll dl) (- nn))) (nth nn (dl-ll dl))))

(declaim (ftype (function (dated-list &optional fixnum)
                          (values (or null date)))
                dl-nth-date))
(defun dl-nth-date (dl &optional (nn 0))
  "Return the Nth date of the dated list."
  (declare (type dated-list dl) (fixnum nn))
  (let ((bb (dl-nth dl nn))) (and bb (funcall (dl-date dl) bb))))

(declaim (ftype (function (dated-list &optional fixnum)
                          (values (or null double-float)))
                dl-nth-val dl-nth-chg))
(defun dl-nth-val (dl &optional (nn 0))
  "Return the Nth value of the dated list."
  (declare (type dated-list dl) (fixnum nn))
  (let ((bb (dl-nth dl nn))) (and bb (funcall (dl-val dl) bb))))

(defun dl-nth-chg (dl &optional (nn 0))
  "Return the Nth change of the dated list."
  (declare (type dated-list dl) (fixnum nn))
  (let ((bb (dl-nth dl nn))) (and bb (funcall (dl-chg dl) bb))))

(defun dl-nth-misc (dl &optional (nn 0))
  "Return the Nth MISC of the dated list."
  (declare (type dated-list dl) (fixnum nn))
  (let ((bb (dl-nth dl nn))) (and bb (funcall (dl-misc dl) bb))))

(defun dl-nth-slot (dl slot &optional (nn 0))
  "Return the Nth SLOT of the dated list."
  (declare (type dated-list dl) (fixnum nn))
  (let ((bb (dl-nth dl nn))) (and bb (funcall (slot-value dl slot) bb))))

(defun dl-last-date (dl)
  "Return the last date of the dated list."
  (declare (type dated-list dl))
  (funcall (dl-date dl) (car (last (car (last (dated-list-fl dl)))))))

(defmethod print-object ((dl dated-list) (out stream))
  (if *print-readably* (call-next-method)
      (let* ((fl (dated-list-fl dl)) (dd (dl-date dl))
             (lr (car (last (cdar (last fl))))) (fr (cadar fl))
             (cc (car (dated-list-cp dl))))
        (format out "~:d/~d ~a [~:@(~a~)] [~a -- ~a] [~a/~a]"
                (dl-full-len dl) (length fl) (dated-list-name dl)
                (dated-list-code dl) (and fr (funcall dd fr))
                (and lr (funcall dd lr)) (caar (dated-list-cl dl))
                (and cc (funcall dd cc))))))

(defmethod describe-object ((dl dated-list) (out stream))
  (let ((f (dated-list-fl dl)) (c (dated-list-cl dl)) (p (dated-list-cp dl)))
    (setf (dated-list-fl dl) nil (dated-list-cl dl) nil (dated-list-cp dl) nil)
    (call-next-method)
    (setf (dated-list-fl dl) f (dated-list-cl dl) c (dated-list-cp dl) p)
    (format out "~%fl (~d):~%" (length f))
    (dolist (f1 f)
      (format out " * ~a (~d)~%   ~a~%   ~a~%" (car f1) (length f1)
              (cadr f1) (car (last f1))))
    (format out "cl: ~a~%cp: ~a~%" (caar c) (car p))))

(defmacro with-saved-dl (dl &body body)
  "Save FL, CL, CP, do BODY, restore FL, CL, CP."
  (with-gensyms ("WSDL-" fl cl cp ll)
    `(let* ((,ll ,dl) (,fl (dated-list-fl ,ll))
            (,cl (dated-list-cl ,ll)) (,cp (dated-list-cp ,ll)))
      (unwind-protect (progn ,@body)
        (setf (dated-list-fl ,ll) ,fl (dated-list-cl ,ll) ,cl
              (dated-list-cp ,ll) ,cp)))))

(defmacro with-truncated-dl ((dl bd ed) &body body)
  "Evaluate BODY when DL is truncated by the dates BD and ED."
  (with-gensyms ("WTDL-" ll cp tp cl tl)
    `(let ((,ll ,dl))
      (with-saved-dl ,ll
        (when ,bd
          (dl-shift ,ll ,bd)
          (setf (dated-list-fl ,ll) (dated-list-cl ,ll)))
        (when ,ed (dl-shift ,ll ,ed))
        (let* ((,cp (dated-list-cp ,ll)) (,tp (cdr ,cp))
               (,cl (dated-list-cl ,ll)) (,tl (cdr ,cl)))
          (unwind-protect (progn (when ,ed
                                   (when ,cp (setf (cdr ,cp) nil))
                                   (when ,cl (setf (cdr ,cl) nil)))
                                 ,@body)
            (when ,ed
              (when ,cp (setf (cdr ,cp) ,tp))
              (when ,cl (setf (cdr ,cl) ,tl)))))))))

(defmacro with-saved-dls ((&rest dls) &body body)
  "Save several dated lists, like nested `with-saved-dl'."
  (let* ((nn (length dls))
         (ll (map-into (make-list nn) (lambda () (gensym "WSDLS-LL-"))))
         (fl (map-into (make-list nn) (lambda () (gensym "WSDLS-FL-"))))
         (cl (map-into (make-list nn) (lambda () (gensym "WSDLS-CL-"))))
         (cp (map-into (make-list nn) (lambda () (gensym "WSDLS-CP-")))))
    `(let* (,@(mapcar #'list ll dls)
            ,@(mapcar (lambda (sy dd) `(,sy (dated-list-fl ,dd))) fl ll)
            ,@(mapcar (lambda (sy dd) `(,sy (dated-list-cl ,dd))) cl ll)
            ,@(mapcar (lambda (sy dd) `(,sy (dated-list-cp ,dd))) cp ll))
      (unwind-protect (progn ,@body)
        (setf ,@(mapcan (lambda (sy dd) `((dated-list-fl ,dd) ,sy)) fl ll)
              ,@(mapcan (lambda (sy dd) `((dated-list-cl ,dd) ,sy)) cl ll)
              ,@(mapcan (lambda (sy dd) `((dated-list-cp ,dd) ,sy)) cp ll))))))

(defcustom *rollover-bad-date* symbol nil
  "*What to do in `rollover' when the first date is bigger than the last.
One of NIL (nothing), :ERROR and :WARN.")

(declaim (ftype (function (list &optional date-f-t) (values date)) rollover))
(defun rollover (list &optional (datef #'date))
  "Return the rollover date for the list."
  (declare (list list) (type date-f-t datef))
  (let ((ld (funcall datef (car (last (car list))))) (fd (caar list)))
    (if (date-p fd)
        (if (or (null (cdr list)) (date> ld fd)) fd
            (case *rollover-bad-date*
              (:error
               (error "rollover: first date [~a] after last [~a]~%" fd ld))
              (:warn
               (warn "rollover: first date [~a] after last [~a]~%" fd ld) fd)
              ((nil) fd)
              (t (error 'case-error :proc 'rollover
                        :arg (list '*rollover-bad-date* *rollover-bad-date*
                                   :error :warn nil)))))
        ld)))

(declaim (ftype (function ((or null date) dated-list &optional t)
                          (values list))
                date-in-dated-list))
(defun date-in-dated-list (dt dl &optional last)
  "Call `date-in-list' on the dated list.
If  LAST is non-nil, make sure that the next date is different.
No side effects.  Returns CP and CL for `dl-shift'."
  (declare (type (or null date) dt) (type dated-list dl))
  (if (null dt) (dl-ll dl)
      (do ((dd (dl-date dl)) (ls (dated-list-cl dl) (cdr ls)))
          ((or (null (cdar ls)) (date<= dt (rollover ls dd)))
           (values (and (cdar ls) (date-in-list dt (cdar ls) dd last)) ls))
        (declare (type date-f-t dd)))))

(defun dl-double-date-p (dt dl)
  "Return T if the date DT is present in DL twice."
  (declare (type dated-list dl))
  (let* ((dt (date dt)) (ta (second (date-in-dated-list dt dl))))
    (declare (type date dt))
    (and ta (date= dt (funcall (dl-date dl) ta)))))

(defcustom *dl-max-overlap* index-t 100
  "*The recommended maximum overlap in dated lists.")

(defun dl-overlap (dl &optional (out *standard-output*))
  "Calculate and print the overlap."
  (declare (type dated-list dl) (type (or null stream) out))
  (mesg :log out " *** Overlap for: ~a~%" dl)
  (loop :for ll = (dated-list-fl dl) :then (cdr ll)
        :and ii :of-type index-t :upfrom 0
        :for len :of-type index-t = (length (cdar ll))
        :with rd :of-type (or null date) = nil
        :and col :of-type index-t = len
        :and dd :of-type function = (dl-date dl) ; [CMUCL] date-f-t
        :while ll
        :when rd :do (setq col (position rd (cdar ll) :test #'date<= :key dd))
        :do (mesg :log out "[~2d ~a ~d]~@[ ~a~] ~d~%" ii (caar ll) len rd col)
        (setq rd (rollover ll dd))
        :minimize col :into mol
        :finally (mesg :head out " *** ~a --> ~d~%" dl mol) (return mol)))

(defun (setf dl-overlap) (ol dl &optional (out *standard-output*))
  "Modify the overlap.  The dated list is reset."
  (declare (type dated-list dl) (type index-t ol) (type (or null stream) out))
  (let ((len (length (car (dated-list-fl dl)))))
    (when (> ol len)
      (mesg :log out "setf dl-overlap: removed first list [~d records, ~a]~%"
            len (car (pop (dated-list-fl dl))))))
  (dl-reset dl)
  (loop :with dd :of-type function = (dl-date dl) :and tmp :and tmp1 ; date-f-t
        :with rd :of-type date = (rollover (dated-list-fl dl) dd)
        :and oo :of-type index-t = (1+ ol)
        :for ll = (cdr (dated-list-fl dl)) :then (cdr ll)
        :do (setf tmp (member rd (cdar ll) :key dd :test #'date<=)
                  tmp1 (cdr tmp)
                  (cdr tmp) nil
                  (cdar ll) (last (cdar ll) oo)
                  (cdr tmp) tmp1)
        :while (cdr ll) :do (setq rd (rollover ll dd))
        :finally (return ol)))

(defun dl-shift (dl &optional (dt 1))
  "Make DL start from DT. Return the DL.
If DT is a fixnum, skip that many records instead.
Defaults to 1.  The second values returned is
whether there was a rollover."
  (declare (type dated-list dl) (type (or fixnum date) dt))
  (etypecase dt
    (date (setf (values (dated-list-cp dl) (dated-list-cl dl))
                (date-in-dated-list dt dl))
          dl)
    (integer
     (loop :repeat dt :with dd :of-type function = (dl-date dl) :and roll
           :with rd :of-type date = (rollover (dated-list-cl dl) dd)
           :if (and (cdr (dated-list-cp dl))
                    (date<= (funcall dd (cadr (dated-list-cp dl))) rd))
           :do (pop (dated-list-cp dl))
           :else :do (pop (dated-list-cl dl)) (setq roll t) :and
             :if (dated-list-cl dl)
             :do (setf roll (date-in-list rd (cdar (dated-list-cl dl)) dd)
                       (dated-list-cp dl) (cdr roll)
                       rd (rollover (dated-list-cl dl) dd))
             :else :do (setf (dated-list-cp dl) nil)
                   :and :return (values dl roll) :end
           :end :finally (return (values dl roll))))))

(defun dl-next-chg (dl)
  "Shift dl to the next date, return the change in val.
Can be used with chain contracts, where there are double records for
roll-over dates."
  (declare (type dated-list dl))
  (let* ((v0 (dl-nth-val dl)) (d0 (dl-nth-date dl))
         (ro (nth-value 1 (dl-shift dl))))
    (when (dl-endp dl) (return-from dl-next-chg nil))
    (if (date= d0 (dl-nth-date dl)) ; never rolls over (1 list)
        (let ((v0 (dl-nth-val dl)))
          (when (dl-endp (dl-shift dl)) (return-from dl-next-chg nil))
          (- (dl-nth-val dl) v0))
        (- (dl-nth-val dl) (if ro (funcall (dl-val dl) (car ro)) v0)))))

(declaim (ftype (function (dated-list &optional (function (date) t)
                           (function (t t) t))
                          (values fixnum))
                dl-count-jumps))
(defun dl-count-jumps (dl &optional (key #'date-ye) (test #'eql))
  "Return the number of years in the dated list."
  (declare (type dated-list dl) (type (function (date) t) key))
  (with-saved-dl dl
    (loop :with kk :of-type function = (compose 'key (dl-date dl))
          :with kp = (funcall kk (dl-nth dl))
          :and kc = (funcall kk (dl-nth (dl-shift dl)))
          :do (dl-shift dl) :until (dl-endp dl)
          :do (setq kp kc kc (funcall kk (dl-nth dl)))
          :count (not (funcall test kp kc)))))

(defun dl-jumps (dl)
  "Return a cons of 2 lists - the up and down moves in the dated list."
  (declare (type dated-list dl))
  (do (up dn (dd (copy-dated-list dl)) ch)
      ((null (setq ch (dl-next-chg dd))) (cons (nreverse up) (nreverse dn)))
    (cond ((plusp ch) (push (cons ch (dl-nth-date dd)) up))
          ((minusp ch) (push (cons ch (dl-nth-date dd)) dn)))))

(defun dl-jumps-ui (dl &optional (out t))
  "Print information about the jumps of the dated list."
  (declare (type dated-list dl))
  (let ((jj (dl-jumps dl)))
    (multiple-value-bind (me nu) (mean (car jj) :key #'car)
      (format out "Up   [~:d]: mean: ~7,3f; standard deviation: ~7,3f~%"
              nu me (standard-deviation (car jj) :len nu :mean me :key #'car)))
    (top-bottom-ui (car jj) 5 5 t :out out :key #'car :label #'cdr)
    (multiple-value-bind (me nu) (mean (cdr jj) :key #'car)
      (format out "Down [~:d]: mean: ~7,3f; standard deviation: ~7,3f~%"
              nu me (standard-deviation (cdr jj) :len nu :mean me :key #'car)))
    (top-bottom-ui (cdr jj) 5 5 t :out out :key #'car :label #'cdr)))

(defun skip-dl-to-date (dl dt &optional stlog)
  "Skip (shift) the dated list DL to the date DT.
Signal error if DL ends before DT.
Print messages for: missing DT in DL and double date in DL.
In the latter case it is assumed that this is a contract switch, the
value difference for this date is ignored, and a message is printed to
STLOG if that is not nil.
Return: the change in misc."
  (declare (type dated-list dl) (type date dt))
  (do* ((dl-t (dl-ll dl) (cdr dl-t)) (dr 0)
        (dlr1 (first dl-t) dlr2) (dlr2 (second dl-t) (second dl-t))
        (dld (dl-nth-date dl) (if dlr1 (funcall (dl-date dl) dlr1)))
        (rr (dl-nth-misc dl)))
       ((not (date< dld dt))
        (when (date< dt dld)    ; next dx is later than fx
          (format t "Missing ~a data for: ~a~%" (dated-list-name dl) dt))
        (setf (dl-ll dl) dl-t)
        (+ dr (- (funcall (dl-misc dl) dlr1) rr)))
    (assert dl-t (dt) "~a ended before ~a~%" (dated-list-name dl) dt)
    (when (and dlr2 (date= dld (funcall (dl-date dl) dlr2)))
      (mesg t stlog " ---> new ~a contract~%" (dated-list-name dl))
      (format t "New ~a contract started ~a~%" (dated-list-name dl) dld)
      (incf dr (- (funcall (dl-misc dl) dlr1) (funcall (dl-misc dl) dlr2))))))

(defun skip-dl-to-extremum (dl)
  "Skip (shift) the dated list to the next extremum.
Return nil if at the end already, or the change in value."
  (declare (type dated-list dl))
  (unless (cdr (dl-ll dl)) (return-from skip-dl-to-extremum nil))
  (do ((ll (dl-ll dl) (dl-ll dl)) ch (mv 0d0))
      ((null (setq ch (dl-next-chg dl))) mv)
    (declare (double-float mv))
    (cond ((minusp (* ch mv)) (setf (dl-ll dl) ll)
           (return-from skip-dl-to-extremum mv))
          (t (incf mv ch)))))

(defun print-dated-lists (begd endd &rest dls)
  "Print the dated lists from BEGD to ENDD, inclusive."
  (let ((bd (date begd)) (ed (date endd)))
    (declare (type date bd ed))
    (assert dls (dls) "nothing to print for ~a -- ~a~%" bd ed)
    (with-printing (prn)
      (dolist (dl dls)
        (format prn "~a [~a -- ~a]~%" (dated-list-name dl) bd ed)
        (do ((td (dl-shift (copy-dated-list dl) bd) (dl-shift td)))
            ((date> (dl-nth-date td) ed) (format prn "~%"))
          (format prn "~a~%" (dl-nth td)))))))

(defun volatility-dl (dl &rest opts &key (split #'date-ye) (key (dl-val dl))
                      (dev-fn #'standard-deviation-relative df-p)
                      &allow-other-keys)
  "Apply `volatility' to the dated list.
Key defaults to VAL; split defaults to `date-ye'."
  (declare (type dated-list dl) (type (or fixnum function) split)
           (type (function (sequence) double-float) dev-fn))
  (setq opts (remove-plist opts :split :dev-fn))
  (loop :for ll :in (dated-list-fl dl) :and nn :of-type index-t :from 0
        :with sp :of-type (or function fixnum) =
        (if (functionp split) (compose 'split (dl-date dl)) split)
        :and ls :of-type list :and vv :of-type double-float
        :do (setf (values vv ls)
                  (if df-p
                      (apply #'volatility (cdr ll) sp :dev-fn dev-fn opts)
                      (volatility (cdr ll) sp :dev-fn dev-fn :key key)))
        :nconc ls :into lst :sum vv :into tv :of-type double-float
        :finally (return (values (/ tv nn)
                                 (call-on-split
                                  (sort lst #'< :key #'car)
                                  (lambda (ll) (mean ll :key #'cdr))
                                  :split-key #'car)))))

(defun print-volatilities (ls &key (out t) (dl #'identity) (head #'identity))
  "Print the annual and monthly volatilities of the objects in the list.
DL is the dated list accessor; HEAD is ihe header for the object,
it should return a short symbol or string."
  (declare (list ls) (function dl head))
  (format out "~&~70,,,'_:@< Volatilities ~>~%Name     Monthly Annual :")
  (let ((beg nil))
    (dolist (ob ls)
      (let ((dl (funcall dl ob)) (he (funcall head ob)) mv lv av)
        (setf (values av lv) (volatility-dl dl :split #'date-ye)
              mv (volatility-dl dl :split #'date-mo))
        (unless beg
          (setq beg (caar lv))
          (dolist (ye lv (terpri out)) (format out "   ~d" (car ye))))
        (format out "~a~10t~6,5f ~6,5f :" he mv av)
        (dolist (yv (member beg lv :key #'car) (terpri out))
          (format out " ~6,5f" (cdr yv)))))))

(defun exp-mov-avg (coeff seq &optional (key #'value) date)
  "Return the list of the exponential moving averages with the given
coefficient for the given sequence."
  (declare (double-float coeff) (sequence seq)
           (type (or null date-f-t) date)
           (type (function (t) double-float) key))
  (let* ((ema (funcall key (elt seq 0))) (c1 (- 1d0 coeff)))
    (declare (double-float ema c1))
    (map 'list
         (if date
             (lambda (el) (cons (funcall date el)
                                (setq ema (lincom coeff (funcall key el)
                                                  c1 ema))))
             (lambda (el) (setq ema (lincom coeff (funcall key el) c1 ema))))
         seq)))

(defun exp-mov-avg-append (coeff seq)
  "Put the exponential moving average of SEQ into it.
The SEQ is a sequence of conses with (cdr (last ELT)) being a double-float X.
It is is replaced with (X . EMA)."
  (declare (double-float coeff) (sequence seq))
  (let ((ema (cdr (last (elt seq 0)))) (c1 (- 1d0 coeff)))
    (declare (double-float ema c1))
    (map-in (lambda (el)
              (let* ((ee (last el)) (nn (cdr ee)))
                (declare (double-float nn))
                (setf (cdr ee) (cons nn (setq ema (lincom coeff nn c1 ema))))
                el))
            seq)))

(declaim (ftype (function (double-float dated-list &optional t symbol)
                          (values dated-list))
                exp-mov-avg-dl))
(defun exp-mov-avg-dl (coeff idl &optional double (slot 'val))
  "UI for `exp-mov-avg' when the argument is a dated list itself.
When DOUBLE is given, compute 2 averages, with COEFF and COEFF/2,
and make the latter accessible through MISC."
  (declare (double-float coeff) (type dated-list idl))
  (let* ((c2 (/ coeff 2d0)) (dd (dl-date idl)) (kk (dl-slot idl slot))
         (dl (make-dated-list
              :date 'car :val 'cdr :name
              (format nil "EMA [~3,2f~:[~*~;/~4,3f~]] `~a'" coeff
                      double c2 (dated-list-name idl))
              :code (keyword-concat (dated-list-code idl) :-ema)))
         (ll (mapcar (lambda (l1)
                       (cons (car l1) (exp-mov-avg coeff (cdr l1) kk dd)))
                     (dated-list-fl idl))))
    (declare (type dated-list dl))
    (setf (dl-fl dl) ll)
    (when double
      (setf (dated-list-val dl) 'cadr
            (dated-list-misc dl) 'cddr)
      (dolist (ll (dated-list-fl dl)) (exp-mov-avg-append c2 (cdr ll))))
    dl))

(defun regress-dl (dl &optional begd endd)
  "Regress the dated list in the given interval.
When a boundary is omitted, the end (or the beginning) is used.
Return the line object and the deviation therefrom.
Must not assume that the list is properly ordered!"
  (declare (type dated-list dl))
  (setq begd (if begd (date begd)) endd (if endd (date endd)))
  (with-sublist (ll (dl-ll dl) begd endd :key (dl-date dl)
                 :test #'date=)
    (regress ll :ykey (dl-val dl)
             :xkey (days-since-f (dl-date dl)
                                 (date-min (funcall (dl-date dl) (car ll))
                                           (funcall (dl-date dl)
                                                    (car (last ll))))))))

(defsubst mean-dl (dl &key (slot 'val))
  "Apply `mean' to the dated list."
  (declare (type dated-list dl))
  (mean (dl-ll dl) :key (dl-slot dl slot)))

(defun standard-deviation-dl (dl &rest opts &key (slot 'val)
                              &allow-other-keys)
  "Apply `standard-deviation' to the dated list."
  (declare (type dated-list dl))
  (apply #'standard-deviation (dl-ll dl) :key (dl-slot dl slot)
         (remove-plist opts :slot)))

(defsubst standard-deviation-relative-dl (dl &key (slot 'val))
  "Apply `standard-deviation' to the dated list."
  (declare (type dated-list dl))
  (standard-deviation-relative (dl-ll dl) :key (dl-slot dl slot)))

(defsubst mean-dl-weighted (dl wts &key (slot 'val))
  "Apply `mean-weighted' to the dated list."
  (declare (type dated-list dl))
  (mean-weighted (dl-ll dl) wts :value (dl-slot dl slot)))

;;;
;;; Change
;;;

(defstruct (change)
  "Change structure - for computing difference derivatives."
  (date +bad-date+ :type date)
  (val 0d0 :type double-float)  ; value
  (chf 0d0 :type double-float)  ; change forward
  (chb 0d0 :type double-float)) ; change backward

(defmethod date ((xx change)) (change-date xx))
(defmethod value ((xx change)) (change-val xx))

(defsubst change-max-p (chg)
  "Is this a local maximum?"
  (declare (type change chg))
  (not (or (plusp (change-chf chg)) (minusp (change-chb chg)))))

(defsubst change-min-p (chg)
  "Is this a local minimum?"
  (declare (type change chg))
  (not (or (minusp (change-chf chg)) (plusp (change-chb chg)))))

(defsubst change-type (chg)
  "Return :min or :max depending on whether chg is a min or an max."
  (declare (type change chg))
  (if (change-max-p chg) :max :min))

(defsubst change-type= (ch1 ch2)
  "Are these two of the same type (min/max)?"
  (declare (type change ch1 ch2))
  (eq (change-type ch1) (change-type ch2)))

(defmethod print-object ((chg change) (out stream))
  (if *print-readably* (call-next-method)
      (format out "~a [~7,3f <- ~8,3f -> ~7,3f]" (change-date chg)
              (change-chb chg) (change-val chg) (change-chf chg))))

(defsubst change-list-to-dated-list (chl &rest args)
  "Make a dated list containing this change list."
  (apply #'mk-dl (list (cons :all chl)) :date 'change-date
         :val 'change-val :chg 'change-chf :misc 'change-chb args))

(defun dl-extrema (dl)
  "Return a dated list of changes, each recording a local extremum.
DL may contain double records (for chain contracts), in which case
the difference between next values can differ from the corresponding
ch[bf], and dl-extrema will not be idempotent."
  (declare (type dated-list dl))
  (do ((dd (copy-dated-list dl)) res ch
       (chg (make-change :date (dl-nth-date dl) :val (dl-nth-val dl))))
      ((null (setq ch (skip-dl-to-extremum dd)))
       (change-list-to-dated-list
        (nreverse (push chg res)) :code
        (keyword-concat (dated-list-code dl) :-xtr)
        :name (format nil "Extrema of `~a'" (dated-list-name dl))))
    (setf (change-chf chg) ch) (push chg res)
    (setq chg
          (make-change :date (dl-nth-date dd) :val (dl-nth-val dd) :chb ch))))

;;;
;;; Diff
;;;

(defstruct (diff)
  "A dated diff."
  (date +bad-date+ :type date)
  (di 0d0 :type real)           ; difference
  (ra 1d0 :type real))          ; ratio

(defmethod date ((xx diff)) (diff-date xx))
(defmethod value ((xx diff)) (diff-di xx))

(defsubst diff-list-to-dated-list (dl &rest args)
  "Wrap a list of diff's into a dated-list.
Sets ll, date, val, and passes the rest directly to `make-dated-list'."
  (apply #'mk-dl (list (cons :all dl)) :date 'diff-date :val 'diff-di args))

(defmethod print-object ((df diff) (out stream))
  (if *print-readably* (call-next-method)
      (format out "~a ~15,6f ~15,6f" (diff-date df)
              (diff-di df) (diff-ra df))))

(defun diff-lists (ls0 ls1 &key (date0 #'date) (date1 #'date)
                   (val0 #'value) (val1 #'value))
  "Generate a list of diff's from the given 2 lists.
For each pair of records in 2 lists that have the same dates
a diff structure is created with the same date and the difference
and the ratio of the values.
The date is accessed by (funcall date* rec),
the value by (funcall val* rec)."
  (declare (list ls0 ls1) (type date-f-t date0 date1)
           (type (function (t) real) val0 val1))
  (do* ((bd (date-max (funcall date0 (car ls0))
                      (funcall date1 (car ls1)))) ll c0 c1 d0 d1 cd
        (pd nil cd)             ; prev date
        (l0 (date-in-list bd ls0 date0) (cdr l0))
        (l1 (date-in-list bd ls1 date1) (cdr l1)))
       ((or (null l0) (null l1)) (nreverse ll))
    (setq c0 (car l0) d0 (funcall date0 c0)
          c1 (car l1) d1 (funcall date1 c1)
          cd (date-max d0 d1))
    (cond ((date< d0 cd)
           (if (date= pd d0)
               (format t " -> Double  record in the 1st list on ~a~%" pd)
               (format t " -> Missing record in the 2nd list on ~a~%" d0))
           (setq l0 (cdr l0) c0 (car l0)))
          ((date< d1 cd)
           (if (date= pd d1)
               (format t " -> Double  record in the 2nd list on ~a~%" pd)
               (format t " -> Missing record in the 1st list on ~a~%" d1))
           (setq l1 (cdr l1) c1 (car l1))))
    (push (make-diff :date cd :di (- (funcall val0 c0) (funcall val1 c1))
                     :ra (s/ (funcall val0 c0) (funcall val1 c1))) ll)))

;;;
;;; Check Dates
;;;

(defun check-dates (lst order-p same-p jump gap &key (date #'date)
                    (val #'value) (out *standard-output*) fixp)
  "Check the dated list LST for order violations, redundancies, large
data changes and gaps. The accessors DATE and VAL default to CAR and
CDR respectively and may be omitted if LST is a dated list.
If you need to check more than one value for jumps, you can resort
to `check-list-values'.
If FIXP is non-nil, try to fix the problems.
Return NIL if no errors were detected, the number of errors otherwise."
  (declare (type date-f-t date) (stream out)
           (type (or null fixnum) gap) (type (or null number) jump)
           (type (function (t) double-float) val))
  (format out "~&Checking the list~:[~; `~:*~a'~] for:~%~5t~?.~%"
          (if (dated-list-p lst) (dated-list-name lst) nil)
          (list-format "~{~a~}")
          (nconc (if order-p (list (list "order")) nil)
                 (if same-p (list (list "redundancies")) nil)
                 (if jump (list (list "jumps of at least " jump)) nil)
                 (if gap (list (list "gaps of at least " gap)) nil)))
  (labels ((check (lst order-p same-p jump gap date val out fixp)
             (declare (type date-f-t date) (stream out)
                      (type (or null fixnum) gap) (type (or null number) jump)
                      (type (function (t) double-float) val))
             (do ((rr lst (cdr rr)) (len 1 (1+ len)) ; start from 1 (sic!)
                  (r0 (first lst) r1) r1 (err 0) (fixed 0) (fff nil nil)
                  (v0 (if val (funcall val (first lst))) v1) v1 jjj ggg
                  (d0 (funcall date (first lst)) d1) (d1 +bad-date+))
                 ((null (cdr rr))
                  (format out
                          "~:d record~:p, ~a through ~a. ~d error~:p found.~%"
                          len (funcall date (first lst)) d1 err)
                  (if (zerop err) nil
                      (cond ((zerop fixed) err)
                            (t (format
                                out "~d error~:p; ~d fixed; re-running...~%"
                                err fixed)
                               (check lst order-p same-p jump gap date
                                      val out nil)))))
               (declare (type index-t len err fixed) (type date d0 d1))
               (setq r1 (second rr) d1 (funcall date r1)
                     v1 (if val (funcall val r1)))
               (when (and (null fff) order-p (date< d1 d0))
                 (format out "~3d. Wrong Order:~% - ~s~% - ~s~2%"
                         (incf err) r0 r1)
                 (when fixp (format out " ### cannot fix~%")))
               (when (and (null fff) same-p (date= d0 d1))
                 (format out "~3d. Same Date:~% - ~s~% - ~s~2%"
                         (incf err) r0 r1)
                 (when fixp
                   (format out " *** removed ~a~%" r1)
                   (incf fixed) (setq fff t)
                   (setf (cdr rr) (cddr rr))))
               (when (and (null fff)
                          (or (setq ggg (and gap (> (days-between d0 d1) gap)))
                              (setq jjj (and jump (> (rel-diff v0 v1) jump)))))
                 (format out "~3d. Jump[~a] Gap[~a]:~% - ~s~% - ~s~2%"
                         (incf err) jjj ggg r0 r1)
                 (when fixp
                   (cond ((eq rr lst)
                          (format out " *** first record, removed ~a~%" r0)
                          (incf fixed) (setq fff t)
                          (setf (car rr) (cadr rr) (cdr rr) (cddr rr)))
                         ((null (cddr rr))
                          (format out " *** last record, removed ~a~%" r1)
                          (incf fixed) (setq fff t)
                          (setf (cdr rr) nil))
                         ((and ggg (every (lambda (zz) (= v1 (funcall val zz)))
                                          (cddr rr)))
                          (format out " *** truncated (end), kept ~a~%" r0)
                          (incf fixed) (setq fff t)
                          (setf (cdr rr) nil))
                         ((and ggg (every (lambda (zz) (= v0 (funcall val zz)))
                                          (ldiff lst rr)))
                          (format out " *** truncated (beg), kept ~a~%" r1)
                          (incf fixed) (setq fff t)
                          (setf (car lst) (cadr rr) (cdr lst) (cddr rr)))
                         (t (format out " ### cannot fix~%"))))))))
    (if (dated-list-p lst)
        (do ((date (dl-date lst)) (val (dl-val lst)) (ii 0 (1+ ii))
             (ll (dated-list-fl lst) (cdr ll)) fd ld)
            ((null ll))
          (declare (type index-t ii))
          (setq fd (caar ll))
          (format out " [~d: ~a ~d] " ii fd (length (cdar ll)))
          (check (cdar ll) order-p same-p jump gap date val out fixp)
          (when (and (cdr ll) (date-p fd)
                     (date>= fd
                             (setq ld (funcall date (car (last (car ll)))))))
            (format out "   fixed rollover date: ~a -> ~a [because of ~a]~%"
                    fd (setf (caar ll) (tomorrow ld -10)) ld)))
        (check lst order-p same-p jump gap date val out fixp))))

(provide :cllib-datedl)
;;; file datedl.lisp ends here
