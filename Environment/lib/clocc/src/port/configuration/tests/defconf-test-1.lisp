;;; -*- Mode: CLtL -*-

(use-package "CL.EXT.CONFIGURATION")

(cl.ext.configuration:defconfiguration "CLAW" ()
  (:library-location "/users/marcoxa/lang/cl/CLAW/"
		     :os-type :unix)
  (:library-location "d:\\users\\marcoxa\\lang\\cl\\claw\\"
		     :os-type :windows)
  ;; (:source-location)
  (:logical-pathname-host "CLAW")
  (:required-package "cl.ext.properties")
  (:required-system "zut")
  (:required-module zot)
  (:required-module gnao
		    :pathnames-components ("gnao:f1" "gnao:f2" "gnao:d;f3"))

  (:special-translations
   :host "elura"
   ("*.*.*" "/software/elura/se/le/cust/chi/" :os-type :unix))

     
  (:special-translations
   ("claw-pkg.*.*"
    (:prefix :source-location :translation "claw-pkg.*.*"))
   ("impl-dependent;cmucl-motif;*.*.*"
    (:prefix "/user/marcoxa/lang/cl/claw/special-stuff/"
	     :translation "impl-dependent/cmucl-motif/*.*.*")
    :configuration-key :cmucl-motif-location
    :os-type :unix)


   ("utilities;*.*.*"
    (:prefix :configuration :translation "utilities/*.*.*")))

  (:finally (configure-format *standard-output*
			      "done configuring claw.")))



;;; end of file -- defconf-test.lisp --
