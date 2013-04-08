(in-package :cl-user)

(check-for-bug :unix-tests-legacy-3
  (progn (ensure-directories-exist "test-dir/") t)
  T)

(check-for-bug :unix-tests-legacy-7
  (#+cmu unix:unix-access
         #+sbcl sb-unix:unix-access "test-dir"
         #+cmu unix:r_ok
         #+sbcl sb-unix:r_ok)
  T)

(check-for-bug :unix-tests-legacy-14
  (#+cmu unix:unix-access #+sbcl sb-unix:unix-access "test-dir" #+cmu unix:w_ok #+sbcl sb-unix:w_ok)
  T)

(check-for-bug :unix-tests-legacy-18
  (#+cmu unix:unix-access #+sbcl sb-unix:unix-access "test-dir" #+cmu unix:x_ok #+sbcl sb-unix:x_ok)
  T)

(check-for-bug :unix-tests-legacy-22
  (#+cmu unix:unix-access #+sbcl sb-unix:unix-access "test-dir" #+cmu unix:f_ok #+sbcl sb-unix:f_ok)
  T)

(with-open-file (file "test-dir/a"
		      :direction :output
		      :if-exists :supersede)
  (princ "hello world" file))

(check-for-bug :unix-tests-legacy-31
  (#+cmu unix:unix-access #+sbcl sb-unix:unix-access "test-dir/a" #+cmu unix:r_ok #+sbcl sb-unix:r_ok)
  T)

(check-for-bug :unix-tests-legacy-35
  (#+cmu unix:unix-access #+sbcl sb-unix:unix-access "test-dir/a" #+cmu unix:w_ok #+sbcl sb-unix:w_ok)
  T)

(check-for-bug :unix-tests-legacy-39
  (#+cmu unix:unix-access #+sbcl sb-unix:unix-access "test-dir/a" #+cmu unix:x_ok #+sbcl sb-unix:x_ok)
  NIL)

(check-for-bug :unix-tests-legacy-43
  (#+cmu unix:unix-access #+sbcl sb-unix:unix-access "test-dir/a" #+cmu unix:f_ok #+sbcl sb-unix:f_ok)
  T)

(check-for-bug :unix-tests-legacy-47
  (progn
    (#+cmu unix:unix-gettimeofday #+sbcl sb-unix:unix-gettimeofday)
    t)
  t)

