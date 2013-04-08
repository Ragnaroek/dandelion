;;; section 8 structures -*- mode: lisp -*-
(in-package :cl-user)

(proclaim '(special log))

;;;
;;; Example 1
;;; define town structure type
;;; area, watertowers, firetrucks, population, elevation are its components
;;;
(check-for-bug :section8-legacy-11
  (defstruct town
    area
    watertowers
    (firetrucks 1 :type fixnum)		;an initialized slot
    population
    (elevation 5128 :read-only t))	;a slot that can't be changed
  TOWN)

					;create a town instance
(check-for-bug :section8-legacy-21
  (progn
    (setq town1 (make-town :area 0 :watertowers 0))
    t)
  t )

					;town's predicate recognizes the new instance
(check-for-bug :section8-legacy-28
  (town-p town1)
  t)

					;new town's area is as specified by make-town
(check-for-bug :section8-legacy-33
  (town-area town1)
  0)

					;new town's elevation has initial value
(check-for-bug :section8-legacy-38
  (town-elevation town1)
  5128)

					;setf recognizes reader function
(check-for-bug :section8-legacy-43
  (setf (town-population town1) 99)
  99)

(check-for-bug :section8-legacy-47
  (town-population town1)
  99)

					;copier function makes a copy of town1
(check-for-bug :section8-legacy-52
  (progn
    (setq town2 (copy-town town1))
    t)
  t)

(check-for-bug :section8-legacy-58
  (= (town-population town1) (town-population town2))
  t)

					;since elevation is a read-only slot, its value can be set only
					;when the structure is created
(check-for-bug :section8-legacy-64
  (progn
    (setq town3 (make-town :area 0 :watertowers 3 :elevation 1200))
    t)
  t)

;;;
;;; Example 2
;;; define clown structure type
;;; this structure uses a nonstandard prefix
;;;
(check-for-bug :section8-legacy-75
  (defstruct (clown (:conc-name bozo-))
    (nose-color 'red)
    frizzy-hair-p polkadots)
  CLOWN)

(check-for-bug :section8-legacy-81
  (progn
    (setq funny-clown (make-clown))
    t)
  t)

					;use non-default reader name
(check-for-bug :section8-legacy-88
  (bozo-nose-color funny-clown)
  RED        )

(check-for-bug :section8-legacy-92
  (defstruct (klown (:constructor make-up-klown) ;similar def using other
                    (:copier clone-klown) ;customizing keywords
                    (:predicate is-a-bozo-p))
    nose-color frizzy-hair-p polkadots)
  klown)

					;custom constructor now exists
(check-for-bug :section8-legacy-100
  (fboundp 'make-up-klown)
  t)

;;;
;;; Example 3
;;; define a vehicle structure type
;;; then define a truck structure type that includes
;;; the vehicle structure
;;;
(check-for-bug :section8-legacy-110
  (defstruct vehicle name year (diesel t :read-only t))
  VEHICLE)

(check-for-bug :section8-legacy-114
  (defstruct (truck (:include vehicle (year 79)))
    load-limit
    (axles 6))
  TRUCK)

(check-for-bug :section8-legacy-120
  (progn
    (setq x (make-truck :name 'mac :diesel t :load-limit 17))
    t)
  t)

					;vehicle readers work on trucks
(check-for-bug :section8-legacy-127
  (vehicle-name x)
  MAC)

					;default taken from :include clause
(check-for-bug :section8-legacy-132
  (vehicle-year x)
  79 )

(check-for-bug :section8-legacy-136
  (defstruct (pickup (:include truck))	;pickup type includes truck
    camper long-bed four-wheel-drive)
  PICKUP)

(check-for-bug :section8-legacy-141
  (progn
    (setq x (make-pickup :name 'king :long-bed t))
    t)
  t)

					;:include default inherited
(check-for-bug :section8-legacy-148
  (pickup-year x)
  79)

;;;
;;; Example 4
;;; use of BOA constructors
;;;
(check-for-bug :section8-legacy-156
  (defstruct (dfs-boa			;BOA constructors
               (:constructor make-dfs-boa (a b c))
               (:constructor create-dfs-boa
                             (a &optional b (c 'cc) &rest d &aux e (f 'ff))))
    a b c d e f)
  DFS-BOA)

					;a, b, and c set by position, and the rest are uninitialized
(check-for-bug :section8-legacy-165
  (progn
    (setq x (make-dfs-boa 1 2 3))
    t)
  t)

(check-for-bug :section8-legacy-171
  (dfs-boa-a x)
  1)

					;a and b set, c and f defaulted
(check-for-bug :section8-legacy-176
  (progn
    (setq x (create-dfs-boa 1 2))
    t)
  t)

(check-for-bug :section8-legacy-182
  (dfs-boa-b x)
  2)

(check-for-bug :section8-legacy-186
  (eq (dfs-boa-c x) 'cc)
  t)

					;a, b, and c set, and the rest are collected into d
(check-for-bug :section8-legacy-191
  (progn
    (setq x (create-dfs-boa 1 2 3 4 5 6))
    t)
  t)

(check-for-bug :section8-legacy-197
  (dfs-boa-d x)
  (4 5 6))

