;;
;;  DOUBLE-PRECISION MACHINE CONSTANTS
;;  D1MACH( 1) = B**(EMIN-1), THE SMALLEST POSITIVE MAGNITUDE.
;;  D1MACH( 2) = B**EMAX*(1 - B**(-T)), THE LARGEST MAGNITUDE.
;;  D1MACH( 3) = B**(-T), THE SMALLEST RELATIVE SPACING.
;;  D1MACH( 4) = B**(1-T), THE LARGEST RELATIVE SPACING.
;;  D1MACH( 5) = LOG10(B)
;;
(defun d1mach (i)
  (ecase i
    (1 least-positive-normalized-double-float)
    (2 most-positive-double-float)
    (3 double-float-epsilon)
    (4 (scale-float double-float-epsilon 1))
    (5 (log (float-radix 1d0) 10d0))))
