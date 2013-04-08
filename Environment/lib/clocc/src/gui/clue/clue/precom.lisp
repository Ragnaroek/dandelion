(in-package :common-lisp-user)

#+pcl
(pcl::precompile-random-code-segments clue)

#+pcl
(progn
  (pushnew 'compile pcl::*defclass-times*)
  (pushnew 'compile pcl::*defgeneric-times*))

;; Ensure VALUES is a legal declaration
#-cmu17
(proclaim '(declaration values))

#+cmu17 ;; Don't warn about botched values decls
(setf c:*suppress-values-declaration* t)

#+pcl ;; This is now in current sources
(pushnew 'values pcl::*non-variable-declarations*)

;; Ensure *features* knows about CLOS and PCL
(when (find-package 'pcl)
  (pushnew :pcl  *features*)
  (pushnew :clos *features*)
  (unless (find-package :clos)
    (rename-package :pcl :pcl '(:clos))))

;; Ensure *features* knows about the Common Lisp Error Handler
(when (find-package 'conditions)
  (pushnew :cleh *features*))

