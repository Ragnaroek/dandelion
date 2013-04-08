;;; Bookmarks for WWW and other purposes
;;; Netscape bookmarks, Opera hotlist, IE favorites
;;;
;;; Copyright (C) 2001 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: bookmark.lisp,v 1.2 2001/11/02 22:31:15 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/bookmark.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  (require :cllib-url (translate-logical-pathname "clocc:src;cllib;url"))
  (require :cllib-card (translate-logical-pathname "clocc:src;cllib;card")))

(in-package :cllib)

(defclass rec ()
  ((name :type simple-string :initarg name :accessor name
         :documentation "The name of the record.")
   (desc :type simple-string :initarg desc :accessor description
         :documentation "The detailed description of the record.")
   (ctime :type integer :reader ctime :documentation "Creation time.")
   (mtime :type integer :reader mtime :documentation "Modification time.")
   (atime :type integer :reader atime :documentation "Access (read) time."))
  (:documentation "A record"))

(defclass link (rec)
  ((vtime :type integer :reader vtime :documentation "Visit time.")
   (url :type url :accessor link-url :documentation "The URL of this link."))
  (:documentation "WWW link"))

(defclass folder (rec)
  ((elts :type list :initarg elts :accessor folder-elts
         :documentation "The list of elements of the folder."))
  (:documentation "A container of other bookmarks."))

(defmethod initialize-instance :after ((rr rec) &rest junk)
  (declare (ignore junk))
  (let ((tm (get-universal-time)))
    (unless (slot-boundp rr 'ctime) (setf (slot-value rr 'ctime) tm))
    (unless (slot-boundp rr 'atime) (setf (slot-value rr 'atime) tm))
    (unless (slot-boundp rr 'mtime) (setf (slot-value rr 'mtime) tm))))

(provide :cllib-bookmark)
;;; file bookmark.lisp ends here
