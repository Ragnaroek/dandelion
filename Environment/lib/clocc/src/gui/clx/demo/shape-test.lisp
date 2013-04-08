;;; -*- Mode:Lisp; Syntax: Common-lisp; Package:XLIB; Base:10; Lowercase: Yes -*-
;;; Copyright BRIAN SPILSBURY <zhivago@iglou.com>
;;; Placed in the public domain, no warranty

(in-package :xlib)

(defun shape-test (&optional (host ""))
  (let* ((d (xlib:open-display host))
         (s (first (xlib:display-roots d)))
         (r (xlib:screen-root s))
         (w (xlib:create-window :x 0 :y 0 :parent r :width 100 :height 100))
         (p (xlib:create-pixmap :width 100 :height 100 :depth 1 :drawable w))
         (g (xlib:create-gcontext :drawable p :foreground 0)))
    
    (multiple-value-bind (b? bx by bw bh c? cx cy cw ch) (xlib:shape-query-extents w)
      (print (list b? bx by bw bh c? cx cy cw ch)))
    (setf (xlib:window-background w) 0)
    (xlib:draw-rectangle p g 0 0 100 100 t)
    (setf (xlib:gcontext-foreground g) 1)
    (xlib:draw-arc p g 0 0 100 100 0.0 (* 2 pi) t)
    (xlib:shape-combine-mask w xlib:shape-bounding 0 0 p xlib::shape-set)
    (xlib:map-window w)
    (xlib:display-finish-output d)

    (multiple-value-bind (b? bx by bw bh c? cx cy cw ch) (xlib:shape-query-extents w)
      (print (list b? bx by bw bh c? cx cy cw ch)))))
