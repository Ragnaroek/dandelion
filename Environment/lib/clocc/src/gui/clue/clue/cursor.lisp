;;; -*- Mode:Lisp; Package:CLUEI; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-

;;;
;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 149149
;;;			       AUSTIN, TEXAS 78714-9149
;;;
;;; Copyright (C)1987,1988,1989,1990 Texas Instruments Incorporated.
;;;
;;; Permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;

(in-package "CLUEI")

(export '(
	  x-cursor
	  arrow-cursor
	  based-arrow-down-cursor
	  based-arrow-up-cursor
	  boat-cursor
	  bogosity-cursor
	  bottom-left-corner-cursor
	  bottom-right-corner-cursor
	  bottom-side-cursor
	  bottom-tee-cursor
	  box-spiral-cursor
	  center-ptr-cursor
	  circle-cursor
	  clock-cursor
	  coffee-mug-cursor
	  cross-cursor
	  cross-reverse-cursor
	  crosshair-cursor
	  diamond-cross-cursor
	  dot-cursor
	  dotbox-cursor
	  double-arrow-cursor
	  draft-large-cursor
	  draft-small-cursor
	  draped-box-cursor
	  exchange-cursor
	  fleur-cursor
	  gobbler-cursor
	  gumby-cursor
	  hand1-cursor
	  hand2-cursor
	  heart-cursor
	  icon-cursor
	  iron-cross-cursor
	  left-ptr-cursor
	  left-side-cursor
	  left-tee-cursor
	  leftbutton-cursor
	  ll-angle-cursor
	  lr-angle-cursor
	  man-cursor
	  middlebutton-cursor
	  mouse-cursor
	  pencil-cursor
	  pirate-cursor
	  plus-cursor
	  question-arrow-cursor
	  right-ptr-cursor
	  right-side-cursor
	  right-tee-cursor
	  rightbutton-cursor
	  rtl-logo-cursor
	  sailboat-cursor
	  sb-down-arrow-cursor
	  sb-h-double-arrow-cursor
	  sb-left-arrow-cursor
	  sb-right-arrow-cursor
	  sb-up-arrow-cursor
	  sb-v-double-arrow-cursor
	  shuttle-cursor
	  sizing-cursor
	  spider-cursor
	  spraycan-cursor
	  star-cursor
	  target-cursor
	  tcross-cursor
	  top-left-arrow-cursor
	  top-left-corner-cursor
	  top-right-corner-cursor
	  top-side-cursor
	  top-tee-cursor
	  trek-cursor
	  ul-angle-cursor
	  umbrella-cursor
	  ur-angle-cursor
	  watch-cursor
	  i-bar-cursor))

(defconstant x-cursor                    0)
(defconstant arrow-cursor                2)
(defconstant based-arrow-down-cursor	 4)
(defconstant based-arrow-up-cursor	 6)
(defconstant boat-cursor		 8)
(defconstant bogosity-cursor		 10)
(defconstant bottom-left-corner-cursor	 12)
(defconstant bottom-right-corner-cursor	 14)
(defconstant bottom-side-cursor		 16)
(defconstant bottom-tee-cursor		 18)
(defconstant box-spiral-cursor		 20)
(defconstant center-ptr-cursor		 22)
(defconstant circle-cursor		 24)
(defconstant clock-cursor		 26)
(defconstant coffee-mug-cursor		 28)
(defconstant cross-cursor		 30)
(defconstant cross-reverse-cursor        32)
(defconstant crosshair-cursor            34)
(defconstant diamond-cross-cursor        36)
(defconstant dot-cursor                  38)
(defconstant dotbox-cursor               40)
(defconstant double-arrow-cursor         42)
(defconstant draft-large-cursor          44)
(defconstant draft-small-cursor          46)
(defconstant draped-box-cursor           48)
(defconstant exchange-cursor             50)
(defconstant fleur-cursor                52)
(defconstant gobbler-cursor              54)
(defconstant gumby-cursor                56)
(defconstant hand1-cursor                58)
(defconstant hand2-cursor                60)
(defconstant heart-cursor                62)
(defconstant icon-cursor                 64)
(defconstant iron-cross-cursor           66)
(defconstant left-ptr-cursor             68)
(defconstant left-side-cursor            70)
(defconstant left-tee-cursor             72)
(defconstant leftbutton-cursor           74)
(defconstant ll-angle-cursor             76)
(defconstant lr-angle-cursor             78)
(defconstant man-cursor                  80)
(defconstant middlebutton-cursor         82)
(defconstant mouse-cursor                84)
(defconstant pencil-cursor               86)
(defconstant pirate-cursor               88)
(defconstant plus-cursor                 90)
(defconstant question-arrow-cursor       92)
(defconstant right-ptr-cursor            94)
(defconstant right-side-cursor           96)
(defconstant right-tee-cursor            98)
(defconstant rightbutton-cursor          100)
(defconstant rtl-logo-cursor             102)
(defconstant sailboat-cursor             104)
(defconstant sb-down-arrow-cursor        106)
(defconstant sb-h-double-arrow-cursor    108)
(defconstant sb-left-arrow-cursor        110)
(defconstant sb-right-arrow-cursor       112)
(defconstant sb-up-arrow-cursor          114)
(defconstant sb-v-double-arrow-cursor    116)
(defconstant shuttle-cursor              118)
(defconstant sizing-cursor               120)
(defconstant spider-cursor               122)
(defconstant spraycan-cursor             124)
(defconstant star-cursor                 126)
(defconstant target-cursor               128)
(defconstant tcross-cursor               130)
(defconstant top-left-arrow-cursor       132)
(defconstant top-left-corner-cursor      134)
(defconstant top-right-corner-cursor     136)
(defconstant top-side-cursor             138)
(defconstant top-tee-cursor              140)
(defconstant trek-cursor                 142)
(defconstant ul-angle-cursor             144)
(defconstant umbrella-cursor             146)
(defconstant ur-angle-cursor             148)
(defconstant watch-cursor                150)
(defconstant i-bar-cursor                152)
