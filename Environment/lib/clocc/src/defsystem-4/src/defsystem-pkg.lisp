;;; -*- Mode: CLtL -*-

;;; DEFSYSTEM 4.0

;;; defsystem-pkg.lisp --
;;; Package definition and other package related settings for
;;; MK:DEFSYSTEM.

;;;===========================================================================
;;; Prerequisites

(eval-when (:load-toplevel :compile-toplevel :execute)
  (unless (find-package "CL.ENV")
    (error "MK:DEFSYSTEM requires the package CL.ENVIRONMENT to run.~@
            You can find it in the CLOCC at~@
            <http://sourceforge.net/projects/clocc>."))

  (unless (find-package "CL.EXT.FILESYSTEM")
    (error "MK:DEFSYSTEM requires the package CL.EXT.FILESYSTEM to run.~@
            It should have been distributed with MK:DEFSYSTEM (check ~@
            file named `adjoin-dirs'.)"))
  )


;;;===========================================================================
;;; Package definition.

;;; Unfortunately, lots of lisps have their own defsystems, some more
;;; primitive than others, all uncompatible, and all in the DEFSYSTEM
;;; package. To avoid name conflicts, we've decided to name this the
;;; MAKE package. A nice side-effect is that the short nickname
;;; MK is my initials.

;;; NOTE:  providing "MAKE" should be done at the end of the loading
;;; operation.

(defpackage "MAKE-4" (:use "COMMON-LISP")
  (:nicknames "MK4" "mk4" "make-4")
  ;; (:nicknames "MK" "make")           ; Commented for the time being.
  ;; (:nicknames "MA")			; Ok. I could do this, but it
					; would make the system messier!
					; 19991211 Marco Antoniotti

  (:import-from #:cl.ext.filesystem #:adjoin-directories)
  
  (:export #:afs-binary-directory	; Do I still need these?
	   #:afs-source-directory
	   )

  (:export #:defsystem
	   #:find-system
	   #:compile-system
	   #:load-system
	   #:clean-system
	   #:undefsystem
	   #:defined-systems
	   #:describe-system
	   #:clean-system
	   #:edit-system
	   #:hardcopy-system
	   #:operate-on-system
	   #:oos
	   #:files-in-system
	   #:get-component
	   #:execute-action
  	   )

  (:export #:find-component)

  (:export #:operate-on-system
	   #:oos)

  (:export #:version
	   #:make-version
	   #:parse-version-string

	   #:version-major-number-tag
	   #:version-minor-number-tag
	   
	   #:version=
	   #:version<
	   #:version>
	   #:version<=
	   #:version>=)

  (:export #:*central-registry*
	   #:add-registry-location
	   #:delete-registry-location
	   #:delete-registry-entry
	   #:system-registry-paths
	   )

  (:export #:*bin-subdir*
	   #:machine-type-translation
	   #:software-type-translation
	   #:compiler-type-translation
	   ;; #:require
	   #:allegro-make-system-fasl
	   #:files-which-need-compilation
	   #:system-source-size #:make-system-tag-table
	   #:*defsystem-version*
	   #:*compile-during-load*
	   #:*minimal-load*
	   #:*dont-redefine-require*
	   #:*files-missing-is-an-error*
	   #:*reload-systems-from-disk*
	   #:*source-pathname-default*
	   #:*binary-pathname-default*
	   #:*multiple-lisp-support*
	   )

  (:export #:define-language
	   #:save-working-image)
  )

(in-package :mk4)

;;; end of file -- defsystem-pkg.lisp --
