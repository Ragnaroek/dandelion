;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: User; -*-
;;;; ------------------------------------------------------------------------------------------
;;;;     Title:	Unparse SGML parse trees
;;;;   Created:	1997-10-17
;;;;    Author: Gilbert Baumann <unk6@rz.uni-karslruhe.de>
;;;; ------------------------------------------------------------------------------------------
;;;;  (c) copyright 1997,2002 by Gilbert Baumann

(defvar *sgml-up-indent* 0)
(defvar *sgml-up-width* 78)

(defvar *sgml-up-current-line* nil
  "String with fill-pointer where the currently open line is accummunated.")

(defvar *sgml-up-last-break-point* nil
  "Index of the last good #\space in *sgml-up-current-line*, where we might break.")

(defvar *sgml-up-sink* nil
  "The character output stream, we write to.")

;;;;;

(defun up/char (c)
  (when (= (length *sgml-up-current-line*) 0)
    (dotimes (i *sgml-up-indent*)
      (declare (ignorable i))
      (vector-push-extend #\space *sgml-up-current-line*)))
  (vector-push-extend c *sgml-up-current-line*))

(defun up/breakable-space ()
  (when (and (> (length *sgml-up-current-line*) *sgml-up-width*)
             *sgml-up-last-break-point*)
    ;; XXX: The new line might be longer that the old, in that case we would barf.
    (write-line *sgml-up-current-line* *sgml-up-sink* :start 0 :end *sgml-up-last-break-point*)
    (replace *sgml-up-current-line* *sgml-up-current-line*
             :start1 *sgml-up-indent*
             :start2 (+ *sgml-up-last-break-point* 1))
    (setf (fill-pointer *sgml-up-current-line*)
          (+ *sgml-up-indent*
             (- (fill-pointer *sgml-up-current-line*)
                (+ *sgml-up-last-break-point* 1))))
    (fill *sgml-up-current-line* #\space :start 0 :end *sgml-up-indent*)
    (setf *sgml-up-last-break-point* nil))
  (up/char #\space)
  (setf *sgml-up-last-break-point* (1- (length *sgml-up-current-line*))))

(defun up/indent (delta)
  (incf *sgml-up-indent* delta))

(defun up/mandatory-newline ()
  (unless (= (length *sgml-up-current-line*) 0)
    ;; emit the current line
    (write-line *sgml-up-current-line* *sgml-up-sink*)
    (setf (fill-pointer *sgml-up-current-line*) 0)
    (setf *sgml-up-last-break-point* nil)))

;;;;

(defun up/cdata-char (c)
  (case c
    (#\< (map nil #'up/char "&lt;"))
    (#\> (map nil #'up/char "&gt;"))
    (#\& (map nil #'up/char "&amp;"))
    (#\" (map nil #'up/char "&quot;"))
    (#.(code-char 160) (map nil #'up/char "&nbsp;"))
    (#\space (up/breakable-space))
    (t
     (up/char c))))

(defun up/cdata (string &optional (start 0) (end (length string)))
  (declare (inline up/cdata-char))
  (loop for i fixnum from start below end
        do (up/cdata-char (char string i))))

(defun up/cdata-pre (string &optional (start 0) (end (length string)))
  ;; emit the current line, if there is something and do not emit a NL
  (when (> (length *sgml-up-current-line*) 0)
    (write-string *sgml-up-current-line* *sgml-up-sink*)
    (setf (fill-pointer *sgml-up-current-line*) 0)
    (setf *sgml-up-last-break-point* nil))
  (loop for i fixnum from start below end
        do (let ((c (char string i)))
             (case c
               (#\< (write-string "&lt;" *sgml-up-sink*))
               (#\> (write-string "&gt;" *sgml-up-sink*))
               (#\& (write-string "&amp;" *sgml-up-sink*))
               (#.(code-char 160) (write-string "&nbsp;" *sgml-up-sink*))
               (t
                (write-char c *sgml-up-sink*))))))

(defun up/open-tag (dtd gi attlist)
  (declare (ignore dtd))
  (up/char #\<)
  (map nil #'up/char (symbol-name gi))
  (do ((q attlist (cddr q)))
      ((null q))
    (let ((slot (car q))
	  (value (cadr q)))
      (unless (char= (char (symbol-name slot) 0) #\%)
        (up/char #\space)
        (map nil #'up/char (symbol-name slot))
        (up/char #\=)
        (up/char #\")
        (up/cdata (princ-to-string value))
        (up/char #\"))))
  (up/char #\>))

(defun up/close-tag (dtd gi)
  (when (or (null (find-element dtd gi nil nil))
            ;; netscape quirks:
            (member gi '(:TD :TH :TR))
            ;;
            (not (or (null (element-include (find-element dtd gi)))
                     (and               ;nil	;xxx
                      (element-oend? (find-element dtd gi))))))
    (up/char #\<)
    (up/char #\/)
    (map nil #'up/char (symbol-name gi))
    (up/char #\>)))

;;;;;

(defun up/aux (dtd pt &optional (pre? nil))
  (let ((style (sgml-up-element-style (pt-name pt))))
    (labels ((up-children ()
	       (do ((q (pt-children pt) (cdr q))
                    (prev nil (car q)))
		   ((null q))
                 ;; This fixes a netscape quirk:
                 (cond ((and prev (eq (gi prev) :P)
                             (member (gi (car q)) '(:DL :UL :OL :TABLE)))
                        (map nil #'up/char "</P>")))
                 ;;
		 (up/aux dtd (car q)
                         (or pre? (eq (pt-name pt) :pre))))))
      (declare (inline up-children))
      (cond ((eq (pt-name pt) :pcdata)
	     (cond (pre?
		    (up/cdata-pre (pt-attrs pt)))
		   (t
		    (up/cdata (pt-attrs pt)))));;(sanify-string/2 (pt-attrs pt) sbegin? send?)))))

	    ((eq style :inline)
	     (up/open-tag dtd (pt-name pt) (pt-attrs pt))
	     (up-children)
             (up/close-tag dtd (pt-name pt)))
	  
	    ((eq style :half-inline)
             (up/mandatory-newline)
	     (up/open-tag dtd (pt-name pt) (pt-attrs pt))
             (up/indent 2)
             (up-children)
             (up/indent -2)
	     (up/close-tag dtd (pt-name pt))
             (up/mandatory-newline))
	  
	    ((eq style :block)
             (up/mandatory-newline)
             (up/open-tag dtd (pt-name pt) (pt-attrs pt))
	     (up/mandatory-newline)
             (up/indent 2)
             (up-children)
             (up/indent -2)
             (up/mandatory-newline)
	     (up/close-tag dtd (pt-name pt))
	     (up/mandatory-newline))) )))

;;;;;;;

(defun sgml-up-element-style (element-name)
  (case element-name
    ((:Q :IMG :A :FONT :SPAN :SUP :SUB :ACRONYM :VAR :KBD :SAMP :CODE
      :DFN :STRONG :EM :SMALL :BIG :STRIKE :S :U :B :I :TT)
     :inline)
    ((:H1 :H2 :H3 :H4 :H5 :H6 :TITLE :CAPTION :TD) 
     :half-inline)
    (t
     :block)))

(defun sgml-unparse (dtd pt sink)
  (let ((*sgml-up-column* 0)
	(*sgml-up-width* 78)
        (*sgml-up-indent* 0)
        (*sgml-up-current-line* (make-array 100 :element-type 'character :fill-pointer 0 :adjustable t))
        (*sgml-up-sink* sink))
    (up/aux dtd pt nil)))