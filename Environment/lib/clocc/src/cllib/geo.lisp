;;; geo - geographical data processing
;;;
;;; Copyright (C) 1998-2004 by Sam Steingold.
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: geo.lisp,v 2.17 2005/01/27 23:02:49 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/geo.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `index-t'
  (require :cllib-withtype (translate-logical-pathname "cllib:withtype"))
  ;; `kwd'
  (require :cllib-symb (translate-logical-pathname "cllib:symb"))
  ;; `save-restore', `skip-search', `skip-blanks', `skip-to-line',
  ;; `read-non-blanks'
  (require :cllib-fileio (translate-logical-pathname "cllib:fileio"))
  ;; `strip-html-markup'
  (require :cllib-html (translate-logical-pathname "cllib:html"))
  ;; `comma'
  (require :cllib-tilsla (translate-logical-pathname "cllib:tilsla"))
  ;; `make-url'
  (require :cllib-url (translate-logical-pathname "cllib:url")))

(in-package :cllib)

(export '(cite-info *weather-url* weather-report
          find-country *country-list* fetch-country-list))

;;;
;;; {{{ Georgaphic Coordinates
;;;

(defun parse-geo-coord (st)
  "Return the number parsed from latitude or longitude (dd:mm:ss[NSEW])
read from the stream."
  (declare (stream st))
  (let* ((sig 1) (cc (+ (read st) (/ (read st) 60d0)))
	 (lt (read st)) se nn)
    (if (numberp lt) (setq se lt nn 0 lt (string (read st)))
	(setf (values se nn)
	      (parse-integer (setq lt (string lt)) :junk-allowed t)))
    (unless se (setq se 0))
    (setq sig (cond ((or (char-equal (schar lt nn) #\n)
			 (char-equal (schar lt nn) #\e)) 1)
		    ((or (char-equal (schar lt nn) #\s)
			 (char-equal (schar lt nn) #\w)) -1)
		    (t (error "Wrong sign designation: `~a'. ~
Must be one of [N]orth, [S]outh, [E]ast or [W]est." (schar lt nn)))))
    (dfloat (* sig (+ cc (/ se 3600d0))))))

(defun geo-location (str &key (start 0))
  "Return the latitude and longitude as two numbers from a string of
type \"48:51:00N 2:20:00E\". Return 2 values - latitude and longitude."
  (declare (simple-string str) (fixnum start))
  (setq str (nsubstitute #\Space #\: (string str))
	str (nsubstitute #\Space #\, (string str)))
  (with-input-from-string (st str :start start)
    (complex (parse-geo-coord st) (parse-geo-coord st))))

(defun print-geo-coords (coord &optional (out *standard-output*))
  "Print the geographic coordinates to the stream."
  (declare (type (complex double-float) coord) (stream out))
  (let ((lat (realpart coord)) (lon (imagpart coord)))
    (declare (double-float lat lon))
    (format out "~9,5f ~a  ~9,5f ~a" lat (if (minusp lat) #\S #\N) lon
	    (if (minusp lon) #\W #\E))))

;;;
;;; }}}{{{ Geo-Data
;;;

(defstruct (geo-data (:conc-name geod-))
  (name "??" :type string)	; the name of the place
  (pop 0 :type (real 0))	; population
  (crd #C(0d0 0d0) :type (complex double-float)) ; coordinates
  (zip nil :type list))		; list of zip codes.

(defmethod print-object ((gd geo-data) (out stream))
  "Print the geo-data."
  (when *print-readably* (return-from print-object (call-next-method)))
  (format out "Place: ~a~%Population: ~12:d;~30tLocation: "
          (geod-name gd) (geod-pop gd))
  (print-geo-coords (geod-crd gd) out)
  (format out "~%Zip Code~p:~{ ~d~}~%" (length (geod-zip gd)) (geod-zip gd)))

(defcustom *census-gazetteer-url* url
  (make-url :port 80 :prot :http :host "www.census.gov"
            :path "/cgi-bin/gazetteer?")
  "*The URL to use to get the cite information.")

;;;###autoload
(defun cite-info (&key (url *census-gazetteer-url*) city state zip
                  (out *standard-output*))
  "Get the cite info from the U.S. Gazetteer.
Print the results to the stream OUT (defaults to T, discard if NIL)
and return a list of geo-data."
  (setq url (if (url-p url) (copy-url url) (url url)))
  (assert (or city state zip) (city state zip)
          "You must specify at least one of :city, :state or :zip~%")
  (flet ((prep (str) (if str (substitute #\+ #\Space (string str)) "")))
    (setf (url-path url)
	  (format nil "~acity=~a&state=~a&zip=~a" (url-path url) (prep city)
		  (prep state) (or zip ""))))
  (with-open-url (sock url :err *standard-output*)
    (skip-search sock "<ul>")
    (do ((str "") res gd (ii 1 (1+ ii)))
	((or (search "</ul>" str)
	     (search "</ul>" (setq str (read-line sock))))
	 (nreverse res))
      (declare (type index-t ii) (simple-string str))
      ;; name
      (setq gd (make-geo-data :name (strip-html-markup str))
	    str (read-line sock))
      ;; population
      (setf (geod-pop gd) (parse-integer str :junk-allowed t :start
					 (1+ (position #\: str))))
      ;; location
      (setq str (nsubstitute #\Space #\: (read-line sock))
	    str (nsubstitute #\Space #\, str)
	    str (nsubstitute #\Space #\< str))
      (with-input-from-string (st str)
	(read st)
	(setf (geod-crd gd)
              (complex (* (read st) (if (eq 'n (read st)) 1 -1))
                       (* (read st) (if (eq 'w (read st)) 1 -1)))))
      ;; zip
      (setq str (read-line sock))
      (setf (geod-zip gd)
	    (if (search "Zip Code" str)
		(with-input-from-string (st str :start (1+ (position #\: str)))
		  (do (rr re) ((null (setq rr (read st nil nil)))
			       (nreverse re))
		    (when (numberp rr) (push rr re))))
		(list zip)))
      (read-line sock) (setq str (read-line sock)) (push gd res)
      (when out (format out "~%~:d. ~a" ii gd)))))

(defcustom *weather-url* url
  (make-url :prot :telnet :port 3000 :host "mammatus.sprl.umich.edu")
  "*The url for the weather information.")

;;;###autoload
(defun weather-report (code &key (out t) (url *weather-url*))
  "Print the weather report for CODE to OUT."
  (setq url (if (url-p url) (copy-url url) (url url)))
  (setf (url-path url) (format nil "/~a//x" code))
  (with-open-url (sock url)
    (do (rr)
	((null (setq rr (read-line sock nil nil))))
      (format out "~a~%" rr))))

;;;
;;; Countries
;;;

(defstruct (country)
  "The country structure - all the data about a country you can think of."
  (name "" :type simple-string)	; name
  (fips nil :type symbol)	; FIPS PUB 10-4 code (US Dept of State)
  (iso2 nil :type symbol)	; ISO 3166: Codes for the Representation
  (iso3 nil :type symbol)	; of Names of Countries. 2- and 3- letters
  (isod 0 :type fixnum)		; ISO 3166: number
  (inet nil :type symbol)	; Internet Domain
  (incl nil :type (or null country)) ; Included in
  (captl nil :type (or null simple-string)) ; Capital
  (area 0d0 :type (double-float 0d0)) ; Area, in sq km
  (frnt 0d0 :type (double-float 0d0)) ; fontier length, in km
  (cstl 0d0 :type (double-float 0d0)) ; coastline, in km
  (crd #C(0d0 0d0) :type (complex double-float)) ; coordinates
  (pop 0 :type integer)		; population
  (birth 0d0 :type (double-float 0d0)) ; birth rate
  (death 0d0 :type (double-float 0d0)) ; death rate
  (mgrtn 0d0 :type double-float) ; net migration rate
  (fert 0d0 :type (double-float 0d0)) ; fertility rate per woman
  (life 0d0 :type (double-float 0d0)) ; life expectancy at birth
  (gdp 0d0 :type (double-float 0d0)) ; GDP, in $$
  (gdpgr nil :type (or null double-float)) ; GDP growth, in %%
  (gdppc 0d0 :type (double-float 0d0)) ; GDP per capita, in $$
  (note nil :type (or null simple-string)) ; ISO Note
  (lctn nil :type (or null simple-string)) ; Location Description
  (dspt nil :type (or null simple-string)) ; Territorial Disputes
  (clmt nil :type (or null simple-string)) ; Climate
  (rsrc nil :type (or null simple-string)) ; Natural Resources
  (ethn nil :type (or null simple-string)) ; ethnic divisions
  (lang nil :type (or null simple-string)) ; languages
  (rlgn nil :type (or null simple-string)) ; religions
  )

(defmethod print-object ((ntn country) (out stream))
  (when *print-readably* (return-from print-object (call-next-method)))
  (format out "~a~@[ (~a)~] [~a ~a] [ISO: ~a ~a ~d]~@[ part of ~a~]
Location: "
          (country-name ntn) (country-captl ntn) (country-fips ntn)
          (country-inet ntn) (country-iso2 ntn) (country-iso3 ntn)
          (country-isod ntn) (and (country-incl ntn)
                                  (country-name (country-incl ntn))))
  (print-geo-coords (country-crd ntn) out)
  (format out "~%Population: ~15:d  B: ~5f  D: ~5f  M: ~5f  Net: ~5f
Fertility: ~5f births/woman   Life expectancy at birth: ~5f years
Area: ~1/comma/ sq km  Frontiers: ~1/comma/ km  Coastline: ~1/comma/ km
GDP: ~2,15:/comma/ (~f $/cap~@[; %~4f growth~])
~@[ * Location: ~a~%~]~@[ * Disputes: ~a~%~]~
~@[ * Climate: ~a~%~]~@[ * Resources: ~a~%~]~@[ * Ethnic divisions: ~a~%~]~
~@[ * Languages: ~a~%~]~@[ * Religions: ~a~%~]~@[[~a]~%~]"
          (country-pop ntn) (country-birth ntn) (country-death ntn)
          (country-mgrtn ntn) (country-pop-chg ntn) (country-fert ntn)
          (country-life ntn) (country-area ntn) (country-frnt ntn)
          (country-cstl ntn) (country-gdp ntn) (country-gdppc ntn)
          (country-gdpgr ntn) (country-lctn ntn) (country-dspt ntn)
          (country-clmt ntn) (country-rsrc ntn) (country-ethn ntn)
          (country-lang ntn) (country-rlgn ntn) (country-note ntn)))

(defsubst country-pop-chg (ntn)
  "Return the net population change, per 1000."
  (declare (type country ntn))
  (- (+ (country-birth ntn) (country-mgrtn ntn)) (country-death ntn)))

(defcustom *geo-code-url* url
  (make-url :prot :http :host "www.odci.gov" :path
	    "/cia/publications/factbook/appf.html")
  "*The URL with the table of the codes.")
(defcustom *geo-info-template* simple-string
  "http://www.odci.gov/cia/publications/factbook/~a.html"
  "*The string for generating the URL for getting information on a country.")
(defcustom *country-list* list nil "*The list of known countries.")

;;;###autoload
(defsubst find-country (slot value &optional (ls *country-list*)
			(test (cond ((member slot '(name cap note)) #'search)
				    ((member slot '(isod pop)) #'=)
				    ((member slot '(area frnt lat lon gdp))
				     (lambda (va sl) (< (car va) sl (cdr va))))
				    (#'eq))))
  "Get the COUNTRY struct corresponding to the given SLOT VALUE.
Returns the list of all countries satisfying the condition.
Looks in list LS, which defaults to `*country-list*'.  If slot value
is a float, such as the GDP, VALUE is a cons with the range.
  (find-country SLOT VALUE &optional LS TEST)"
  (declare (symbol slot) (type (function (t t) t) test) (list ls))
  (remove-if-not (lambda (nn) (funcall test value (slot-value nn slot))) ls))

(defun save-restore-country-list (&optional (save t))
  "Save or restore `*country-list*'."
  (save-restore save :var '*country-list* :name "country.dat"
                :basedir *datadir*))

(defun str-core (rr)
  "Get the substring of the string from the first `>' to the last `<'."
  (declare (simple-string rr))
  (let ((cc (subseq rr (1+ (position #\> rr))
		    (position #\< rr :from-end t :start 2))))
    (cond ((string= "-" cc) "NIL") ((string= "<BR>" cc) nil)
	  ((string-trim +whitespace+ cc)))))

(defun read-some-lines (st)
  "Read some lines from tag to tag."
  (declare (stream st))
  (do* ((rr (read-line st) (read-line st))
	(cp (position #\< rr :from-end t :start 2) (position #\< rr))
	(cc (subseq rr (1+ (position #\> rr)) cp)
	    (concatenate 'string cc " " (subseq rr 0 cp))))
       (cp (if (char= #\& (schar cc 0)) nil (string-trim +whitespace+ cc)))
    (declare (simple-string rr cc))))

;;;###autoload
(defun fetch-country-list ()
  "Initialize `*country-list*' from `*geo-code-url*'."
  (format t "~&Reading `~a'" *geo-code-url*)
  (with-open-url (st *geo-code-url* :err *standard-output*)
    ;; (with-open-file (st "/home/sds/lisp/wfb-appf.htm" :direction :input)
    (skip-search st "Entity")
    (skip-search st "</TR>")
    (do (res)
	((search "</TABLE>" (skip-blanks st) :test #'char=)
	 (setq *country-list* (nreverse res)))
      (princ ".") (force-output)
      (push (make-country
	     :name (read-some-lines st)
	     :fips (kwd (str-core (read-line st)))
	     :iso2 (kwd (str-core (read-line st)))
	     :iso3 (kwd (str-core (read-line st)))
	     :isod (or (read-from-string (str-core (read-line st))) 0)
	     :inet (kwd (str-core (read-line st)))
	     :note (read-some-lines st)) res)))
  (format t "~d countries.~%" (length *country-list*))
  (dolist (nn *country-list*)	; set incl
    (when (country-note nn)
      (format t "~a: ~a~%" (country-name nn) (country-note nn))
      (do* ((iiw " includes with ") (iiwt " includes with the ") len
	    (pos (or (progn (setq len (length iiwt))
			    (search iiwt (country-note nn)))
		     (progn (setq len (length iiw))
			    (search iiw (country-note nn))))
		 (1+ (or (position #\Space new) (1- (length new)))))
	    (new (if pos (subseq (country-note nn) (+ len pos)))
		 (subseq new pos))
	    (ll (find-country 'name new) (find-country 'name new)))
	   ((or (null new) (zerop (length new)) (= 1 (length ll)))
	    (if (= 1 (length ll))
		(format t "~5tIncluding into --> ~a~%"
			(country-name (setf (country-incl nn) (car ll))))
		(if pos (format t "~10t *** Not found!~%"))))))))

(defun dump-country (ntn &rest opts)
  "Dump the URL for the country."
  (declare (type country ntn))
  (apply #'dump-url (url (format nil *geo-info-template* (country-fips ntn)))
	 opts))

(defun view-country (&rest find-args)
  (let ((ntn (if (country-p (car find-args)) (car find-args)
		 (car (apply #'find-country find-args)))))
    (browse-url (url (format nil *geo-info-template* (country-fips ntn))))))

(defmacro dump-find-country ((&rest find-args)
                             (&rest dump-args &key (out *standard-output*)
                                    &allow-other-keys))
  "Dump all the URLs for all the relevant countries."
  (setq dump-args (remove-plist dump-args :out))
  (dolist (cc (apply #'find-country find-args))
    (let ((st (if (or (and (symbolp out) (fboundp out)) (functionp out))
		  (funcall out cc) out)))
      (declare (stream st))
      (format st "~70~~%~a~70~~%" cc)
      (apply #'dump-country cc :out st dump-args))))

(defun update-country (cc)
  "Get the data from the WWW and update the structure."
  (declare (type country cc))
  ;; (setf (country-note cc) (current-time nil))
  (with-open-url (st (url (format nil *geo-info-template* (country-fips cc)))
		     :err *standard-output*)
    (ignore-errors
      (setf (country-lctn cc) (next-info st "<B>Location:</B>")
            (country-crd cc)
	    (geo-location (next-info st "<BR><B>Geographic coordinates:</B>")))
      (skip-to-line st "<b>Area:</b>")
      (setf (country-area cc) (next-info st "<BR><I>total:</I>" 'float))
      (let ((lb (next-info st "<BR><B>Land boundaries:</B>" 'number)))
	(typecase lb
	  (number (setf (country-frnt cc) (dfloat lb)))
	  (t (setf (country-frnt cc) (dfloat (parse-num (read-line st))))
	     (add-note cc "Borders with: "
		       (next-info st "<BR><I>border countr")))))
      (setf (country-cstl cc) (next-info st "<BR><B>Coastline:</B>" 'float)
	    (country-dspt cc)
	    (next-info st "<BR><B>International disputes:</B>")
	    (country-clmt cc) (next-info st "<BR><B>Climate:</B>")
	    (country-rsrc cc) (next-info st "<BR><B>Natural resources:</B>")
	    (country-pop cc) (next-info st "<B>Population:</B>" 'number)
	    (country-birth cc) (next-info st "<BR><B>Birth rate:</B>" 'float)
	    (country-death cc) (next-info st "<BR><B>Death rate:</B>" 'float)
	    (country-mgrtn cc)
	    (next-info st "<BR><B>Net migration rate:</B>" 'float)
	    (country-life cc)
	    (next-info st "<BR><B>Life expectancy at birth:</B>" 'float)
	    (country-fert cc)
	    (next-info st "<BR><B>Total fertility rate:</B>"'float)
	    (country-ethn cc) (next-info st "<BR><B>Ethnic groups:</B>")
	    (country-rlgn cc) (next-info st "<BR><B>Religions:</B>")
	    (country-lang cc) (next-info st "<BR><B>Languages:</B>")
	    (country-captl cc) (next-info  st "<BR><B>Capital:</B>")
	    (country-gdp cc) (next-info st "<BR><B>GDP:</B>" 'float)
	    (country-gdppc cc)
	    (next-info st "<BR><B>GDP per capita:</B>" 'float)
	    ))
    cc))

(defun next-info (str skip &optional type)
  "Get the next object from stream STR, skipping till SKIP, of type TYPE."
  (declare (stream str) (simple-string skip) (symbol type))
  (let ((ln (skip-to-line str skip)))
    (case type (float (dfloat (parse-num ln))) (number (parse-num ln))
	  (t (concatenate 'string ln (read-non-blanks str))))))

(defun add-note (cc &rest news)
  "Append a note."
  (declare (type country cc) (list news))
  (setf (country-note cc)
	(apply #'concatenate 'string (country-note cc)
	       #.(string #\Newline) news)))

;(defun true (&rest zz) (declare (ignore zz)) t)
;(defun false (&rest zz) (declare (ignore zz)) nil)

(defun parse-num (st)
  "Parse the number from the string, fixing commas."
  (declare (simple-string st))
  (fill st #\Space :end
	(let ((pp (position-if
		   (lambda (zz) (or (digit-char-p zz) (eql zz #\$))) st)))
	  (when pp (if (digit-char-p (char st pp)) pp (1+ pp)))))
  (nsubstitute #\Space #\% st)
  (do ((pos 0 (and next (1+ next))) next res)
      ((null pos)
       (setf st (apply #'concatenate 'string (nreverse res))
	     (values next pos) (read-from-string st nil nil))
       (and next
	    (* next (case (read-from-string st nil nil :start pos)
		      (trillion 1000000000000) (billion 1000000000)
		      (million 1000000) (t 1)))))
    (declare (type (or null index-t) pos next))
    (push (subseq st pos (setq next (position #\, st :start pos))) res)))

#+nil
(progn
  (load "geo")
  (fetch-country-list)
  (dolist (cc *country-list*)
    ;(update-country cc)
    (when (and (zerop (country-gdppc cc))
               (/= 0 (country-gdp cc)) (/= 0 (country-pop cc)))
      (setf (country-gdppc cc) (fround (country-gdp cc) (country-pop cc)))
      (format t " *** ~a~2%" cc)))
  (save-restore-country-list))

(provide :cllib-geo)
;;; geo.lisp ends here
