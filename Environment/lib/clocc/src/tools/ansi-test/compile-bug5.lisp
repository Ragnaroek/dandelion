(in-package :cl-user)

(defun tickle-bug ()
  (labels ((fun1 ()
             (fun2))
           (fun2 ()
             (when nil
               (tagbody
                tag
                  (fun2)
                  (go tag)))
             (when nil
               (tagbody
                tag
                  (fun1)
                  (go tag)))))

    (fun1)
    nil))

