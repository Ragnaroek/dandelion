;;;; cil.lsp: Chess In Lisp foundation programming toolkit

;;; Revised: 1997.06.08

;;; Send comments to: S. J. Edwards (sje@mv.mv.com)

;; This source file is the foundation of the Chess In Lisp programming
;; toolkit.  It contains the core processing functions needed to perform
;; research in the chess domain using Lisp.

;; Global optimization options

(declaim
 (optimize (speed 3) (safety 0) (space 0) (compilation-speed 0)))

;;; --- Constants -----------------------------------------------

;; Colors

(defconstant c-limit 4)
(defconstant rc-limit 2)
(defconstant c-nil -1)

(defconstant c-w 0) ; white
(defconstant c-b 1) ; black
(defconstant c-v 2) ; vacant
(defconstant c-x 3) ; extra

(defparameter c-strings
  (make-array c-limit
               :initial-contents '("w" "b" " " "?")))

(defparameter color-strings
  (make-array rc-limit
             :initial-contents '("white" "black")))

(defparameter player-strings
  (make-array rc-limit
              :initial-contents '("White" "Black")))

(defparameter invc-v
  (make-array rc-limit
              :element-type 'fixnum
              :initial-contents `(,c-b ,c-w)))

;; Pieces

(defconstant p-limit 8)
(defconstant rp-limit 6)
(defconstant p-nil -1)

(defconstant p-p 0) ; pawn
(defconstant p-n 1) ; knight
(defconstant p-b 2) ; bishop
(defconstant p-r 3) ; rook
(defconstant p-q 4) ; queen
(defconstant p-k 5) ; king
(defconstant p-v 6) ; vacant
(defconstant p-x 7) ; extra

(defparameter p-strings
  (make-array p-limit
              :initial-contents '("P" "N" "B" "R" "Q" "K" " " "?")))

(defparameter piece-strings
  (make-array rp-limit
               :initial-contents
               '("pawn" "knight" "bishop" "rook" "queen" "king")))

(defparameter lcp-strings
  (make-array p-limit
              :initial-contents '("p" "n" "b" "r" "q" "k" " " "?")))

;; Color-pieces

(defconstant cp-limit 16)
(defconstant rcp-limit 12)
(defconstant cp-nil -1)

(defconstant cp-wp   0) ; white 
(defconstant cp-wn   1) ; white 
(defconstant cp-wb   2) ; white 
(defconstant cp-wr   3) ; white 
(defconstant cp-wq   4) ; white 
(defconstant cp-wk   5) ; white 
(defconstant cp-bp   6) ; black
(defconstant cp-bn   7) ; black
(defconstant cp-bb   8) ; black
(defconstant cp-br   9) ; black
(defconstant cp-bq  10) ; black
(defconstant cp-bk  11) ; black
(defconstant cp-v0  12) ; vacant
(defconstant cp-x0  13) ; extra
(defconstant cp-x1  14) ; extra extra
(defconstant cp-x2  15) ; extra extra extra

(defparameter cp-strings
  (make-array cp-limit
              :initial-contents
              '("wP" "wN" "wB" "wR" "wQ" "wK"
                "bP" "bN" "bB" "bR" "bQ" "bK"
                "  " "??" "?1" "?2")))

(defparameter mapv-c
  (make-array cp-limit
              :element-type 'fixnum
              :initial-contents
              `(,c-w ,c-w ,c-w ,c-w ,c-w ,c-w
                     ,c-b ,c-b ,c-b ,c-b ,c-b ,c-b
                     ,c-v ,c-x ,c-x ,c-x)))

(defparameter mapv-p
  (make-array cp-limit
              :element-type 'fixnum
              :initial-contents
              `(,p-p ,p-n ,p-b ,p-r ,p-q ,p-k
                     ,p-p ,p-n ,p-b ,p-r ,p-q ,p-k
                     ,p-v ,p-x ,p-x ,p-x)))

(defparameter mapv-cp
  (make-array `(,rc-limit ,rp-limit)
              :element-type 'fixnum
              :initial-contents
              `((,cp-wp ,cp-wn ,cp-wb ,cp-wr ,cp-wq ,cp-wk)
                (,cp-bp ,cp-bn ,cp-bb ,cp-br ,cp-bq ,cp-bk))))

(defparameter sweeper-cp
  (make-array rcp-limit
              :element-type 'fixnum
              :initial-contents
              '(0 0 1 1 1 0 0 0 1 1 1 0)))

;; Edge limit

(defparameter edge-limit 8)

;; Ranks

(defparameter rank-limit edge-limit)
(defconstant rank-nil -1)

(defconstant rank-1 0) ; first rank
(defconstant rank-2 1) ; second rank
(defconstant rank-3 2) ; third rank
(defconstant rank-4 3) ; fourth rank
(defconstant rank-5 4) ; fifth rank
(defconstant rank-6 5) ; sixth rank
(defconstant rank-7 6) ; seventh rank
(defconstant rank-8 7) ; eighth rank

(defparameter rank-strings
  (make-array rank-limit
              :initial-contents '("1" "2" "3" "4" "5" "6" "7" "8")))

;; Files

(defparameter file-limit edge-limit)
(defconstant file-nil -1)

(defconstant file-a 0) ; queen rook file
(defconstant file-b 1) ; queen knight file
(defconstant file-c 2) ; queen bishop file
(defconstant file-d 3) ; queen file
(defconstant file-e 4) ; king file
(defconstant file-f 5) ; king bishop file
(defconstant file-g 6) ; king knight file
(defconstant file-h 7) ; king rook file

(defparameter file-strings
  (make-array file-limit
              :initial-contents '("a" "b" "c" "d" "e" "f" "g" "h")))

;; Squares

(defparameter sq-limit (* rank-limit file-limit))
(defconstant sq-nil -1)

(defparameter sq-a1 (+ file-a (* rank-1 file-limit)))
(defparameter sq-b1 (+ file-b (* rank-1 file-limit)))
(defparameter sq-c1 (+ file-c (* rank-1 file-limit)))
(defparameter sq-d1 (+ file-d (* rank-1 file-limit)))
(defparameter sq-e1 (+ file-e (* rank-1 file-limit)))
(defparameter sq-f1 (+ file-f (* rank-1 file-limit)))
(defparameter sq-g1 (+ file-g (* rank-1 file-limit)))
(defparameter sq-h1 (+ file-h (* rank-1 file-limit)))

(defparameter sq-a2 (+ file-a (* rank-2 file-limit)))
(defparameter sq-b2 (+ file-b (* rank-2 file-limit)))
(defparameter sq-c2 (+ file-c (* rank-2 file-limit)))
(defparameter sq-d2 (+ file-d (* rank-2 file-limit)))
(defparameter sq-e2 (+ file-e (* rank-2 file-limit)))
(defparameter sq-f2 (+ file-f (* rank-2 file-limit)))
(defparameter sq-g2 (+ file-g (* rank-2 file-limit)))
(defparameter sq-h2 (+ file-h (* rank-2 file-limit)))

(defparameter sq-a3 (+ file-a (* rank-3 file-limit)))
(defparameter sq-b3 (+ file-b (* rank-3 file-limit)))
(defparameter sq-c3 (+ file-c (* rank-3 file-limit)))
(defparameter sq-d3 (+ file-d (* rank-3 file-limit)))
(defparameter sq-e3 (+ file-e (* rank-3 file-limit)))
(defparameter sq-f3 (+ file-f (* rank-3 file-limit)))
(defparameter sq-g3 (+ file-g (* rank-3 file-limit)))
(defparameter sq-h3 (+ file-h (* rank-3 file-limit)))

(defparameter sq-a4 (+ file-a (* rank-4 file-limit)))
(defparameter sq-b4 (+ file-b (* rank-4 file-limit)))
(defparameter sq-c4 (+ file-c (* rank-4 file-limit)))
(defparameter sq-d4 (+ file-d (* rank-4 file-limit)))
(defparameter sq-e4 (+ file-e (* rank-4 file-limit)))
(defparameter sq-f4 (+ file-f (* rank-4 file-limit)))
(defparameter sq-g4 (+ file-g (* rank-4 file-limit)))
(defparameter sq-h4 (+ file-h (* rank-4 file-limit)))

(defparameter sq-a5 (+ file-a (* rank-5 file-limit)))
(defparameter sq-b5 (+ file-b (* rank-5 file-limit)))
(defparameter sq-c5 (+ file-c (* rank-5 file-limit)))
(defparameter sq-d5 (+ file-d (* rank-5 file-limit)))
(defparameter sq-e5 (+ file-e (* rank-5 file-limit)))
(defparameter sq-f5 (+ file-f (* rank-5 file-limit)))
(defparameter sq-g5 (+ file-g (* rank-5 file-limit)))
(defparameter sq-h5 (+ file-h (* rank-5 file-limit)))

(defparameter sq-a6 (+ file-a (* rank-6 file-limit)))
(defparameter sq-b6 (+ file-b (* rank-6 file-limit)))
(defparameter sq-c6 (+ file-c (* rank-6 file-limit)))
(defparameter sq-d6 (+ file-d (* rank-6 file-limit)))
(defparameter sq-e6 (+ file-e (* rank-6 file-limit)))
(defparameter sq-f6 (+ file-f (* rank-6 file-limit)))
(defparameter sq-g6 (+ file-g (* rank-6 file-limit)))
(defparameter sq-h6 (+ file-h (* rank-6 file-limit)))

(defparameter sq-a7 (+ file-a (* rank-7 file-limit)))
(defparameter sq-b7 (+ file-b (* rank-7 file-limit)))
(defparameter sq-c7 (+ file-c (* rank-7 file-limit)))
(defparameter sq-d7 (+ file-d (* rank-7 file-limit)))
(defparameter sq-e7 (+ file-e (* rank-7 file-limit)))
(defparameter sq-f7 (+ file-f (* rank-7 file-limit)))
(defparameter sq-g7 (+ file-g (* rank-7 file-limit)))
(defparameter sq-h7 (+ file-h (* rank-7 file-limit)))

(defparameter sq-a8 (+ file-a (* rank-8 file-limit)))
(defparameter sq-b8 (+ file-b (* rank-8 file-limit)))
(defparameter sq-c8 (+ file-c (* rank-8 file-limit)))
(defparameter sq-d8 (+ file-d (* rank-8 file-limit)))
(defparameter sq-e8 (+ file-e (* rank-8 file-limit)))
(defparameter sq-f8 (+ file-f (* rank-8 file-limit)))
(defparameter sq-g8 (+ file-g (* rank-8 file-limit)))
(defparameter sq-h8 (+ file-h (* rank-8 file-limit)))

(defparameter sq-strings
  (make-array sq-limit
              :initial-contents
              '(
                "a1" "b1" "c1" "d1" "e1" "f1" "g1" "h1"
                "a2" "b2" "c2" "d2" "e2" "f2" "g2" "h2"
                "a3" "b3" "c3" "d3" "e3" "f3" "g3" "h3"
                "a4" "b4" "c4" "d4" "e4" "f4" "g4" "h4"
                "a5" "b5" "c5" "d5" "e5" "f5" "g5" "h5"
                "a6" "b6" "c6" "d6" "e6" "f6" "g6" "h6"
                "a7" "b7" "c7" "d7" "e7" "f7" "g7" "h7"
                "a8" "b8" "c8" "d8" "e8" "f8" "g8" "h8")))

;; Directions: 4 orthogonal, 4 diagonal, 8 knight

(defconstant dx-limit 16)
(defconstant rdx-limit 8)
(defconstant dx-nil -1)

(defconstant dx-0  0) ; east
(defconstant dx-1  1) ; north
(defconstant dx-2  2) ; west
(defconstant dx-3  3) ; south
(defconstant dx-4  4) ; northeast
(defconstant dx-5  5) ; northwest
(defconstant dx-6  6) ; southwest
(defconstant dx-7  7) ; southeast
(defconstant dx-8  8) ; east by northeast
(defconstant dx-9  9) ; north by northeast
(defconstant dx-a 10) ; north by northwest
(defconstant dx-b 11) ; west by northwest
(defconstant dx-c 12) ; west by southwest
(defconstant dx-d 13) ; south by southwest
(defconstant dx-e 14) ; south by southeast
(defconstant dx-f 15) ; east by southeast

;; Directional rank deltas

(defconstant dr-0  0)
(defconstant dr-1  1)
(defconstant dr-2  0)
(defconstant dr-3 -1)
(defconstant dr-4  1)
(defconstant dr-5  1)
(defconstant dr-6 -1)
(defconstant dr-7 -1)
(defconstant dr-8  1)
(defconstant dr-9  2)
(defconstant dr-a  2)
(defconstant dr-b  1)
(defconstant dr-c -1)
(defconstant dr-d -2)
(defconstant dr-e -2)
(defconstant dr-f -1)

(defparameter mapv-dr
  (make-array dx-limit
              :element-type 'fixnum
              :initial-contents
              `(,dr-0 ,dr-1 ,dr-2 ,dr-3 ,dr-4 ,dr-5 ,dr-6 ,dr-7
                      ,dr-8 ,dr-9 ,dr-a ,dr-b ,dr-c ,dr-d ,dr-e ,dr-f)))

;; Directional file deltas

(defconstant df-0  1)
(defconstant df-1  0)
(defconstant df-2 -1)
(defconstant df-3  0)
(defconstant df-4  1)
(defconstant df-5 -1)
(defconstant df-6 -1)
(defconstant df-7  1)
(defconstant df-8  2)
(defconstant df-9  1)
(defconstant df-a -1)
(defconstant df-b -2)
(defconstant df-c -2)
(defconstant df-d -1)
(defconstant df-e  1)
(defconstant df-f  2)

(defparameter mapv-df
  (make-array dx-limit
              :element-type 'fixnum
              :initial-contents
              `(,df-0 ,df-1 ,df-2 ,df-3 ,df-4 ,df-5 ,df-6 ,df-7
                      ,df-8 ,df-9 ,df-a ,df-b ,df-c ,df-d ,df-e ,df-f)))

;; Directional offsets

(defparameter dv-0 (+ df-0 (* rank-limit dr-0)))
(defparameter dv-1 (+ df-1 (* rank-limit dr-1)))
(defparameter dv-2 (+ df-2 (* rank-limit dr-2)))
(defparameter dv-3 (+ df-3 (* rank-limit dr-3)))
(defparameter dv-4 (+ df-4 (* rank-limit dr-4)))
(defparameter dv-5 (+ df-5 (* rank-limit dr-5)))
(defparameter dv-6 (+ df-6 (* rank-limit dr-6)))
(defparameter dv-7 (+ df-7 (* rank-limit dr-7)))
(defparameter dv-8 (+ df-8 (* rank-limit dr-8)))
(defparameter dv-9 (+ df-9 (* rank-limit dr-9)))
(defparameter dv-a (+ df-a (* rank-limit dr-a)))
(defparameter dv-b (+ df-b (* rank-limit dr-b)))
(defparameter dv-c (+ df-c (* rank-limit dr-c)))
(defparameter dv-d (+ df-d (* rank-limit dr-d)))
(defparameter dv-e (+ df-e (* rank-limit dr-e)))
(defparameter dv-f (+ df-f (* rank-limit dr-f)))

(defparameter mapv-dv
  (make-array dx-limit
              :element-type 'fixnum
              :initial-contents
              `(,dv-0 ,dv-1 ,dv-2 ,dv-3 ,dv-4 ,dv-5 ,dv-6 ,dv-7
                      ,dv-8 ,dv-9 ,dv-a ,dv-b ,dv-c ,dv-d ,dv-e ,dv-f)))

;; Flanks

(defconstant flank-limit 2)
(defconstant flank-nil -1)

(defconstant flank-k 0) ; kingside
(defconstant flank-q 1) ; queenside

;; Flank castling strings

(defparameter fc-strings
  (make-array flank-limit
              :initial-contents '("O-O" "O-O-O")))

;; Castling status bit positions

(defconstant csbp-wk (+ flank-k (* c-w flank-limit))) ; white KS castling
(defconstant csbp-wq (+ flank-q (* c-w flank-limit))) ; white QS castling
(defconstant csbp-bk (+ flank-k (* c-b flank-limit))) ; black KS castling
(defconstant csbp-bq (+ flank-q (* c-b flank-limit))) ; black QS castling

;; Castling bitfields

(defconstant cflg-wk (ash 1 csbp-wk))
(defconstant cflg-wq (ash 1 csbp-wq))
(defconstant cflg-bk (ash 1 csbp-bk))
(defconstant cflg-bq (ash 1 csbp-bq))

;; Special case move indications

(defconstant scmv-limit 8)
(defconstant scmv-nil -1)

(defconstant scmv-reg 0) ; regular
(defconstant scmv-cks 1) ; castle kingside
(defconstant scmv-cqs 2) ; castle queenside
(defconstant scmv-epc 3) ; en passant capture
(defconstant scmv-ppn 4) ; pawn promotes to a knight
(defconstant scmv-ppb 5) ; pawn promotes to a bishop
(defconstant scmv-ppr 6) ; pawn promotes to a rook
(defconstant scmv-ppq 7) ; pawn promotes to a queen

;; Move flag type bit positions

(defconstant mfbp-anfd 0) ; algebraic notation needs file disambiguation
(defconstant mfbp-anrd 1) ; algebraic notation needs rank disambiguation
(defconstant mfbp-bust 2) ; illegal move
(defconstant mfbp-chec 3) ; checking move, including checkmating
(defconstant mfbp-chmt 4) ; checkmating move
(defconstant mfbp-exec 5) ; executed move
(defconstant mfbp-null 6) ; null move
(defconstant mfbp-srch 7) ; searched move
(defconstant mfbp-stmt 8) ; stalemating move

;; Move flag type bitfields

(defconstant mflg-anfd (ash 1 mfbp-anfd))
(defconstant mflg-anrd (ash 1 mfbp-anrd))
(defconstant mflg-bust (ash 1 mfbp-bust))
(defconstant mflg-chec (ash 1 mfbp-chec))
(defconstant mflg-chmt (ash 1 mfbp-chmt))
(defconstant mflg-exec (ash 1 mfbp-exec))
(defconstant mflg-null (ash 1 mfbp-null))
(defconstant mflg-srch (ash 1 mfbp-srch))
(defconstant mflg-stmt (ash 1 mfbp-stmt))

;; Move structure

(defstruct move
  (frsq sq-nil   :type fixnum) ; from square
  (tosq sq-nil   :type fixnum) ; to square
  (frcp cp-nil   :type fixnum) ; from color-piece
  (tocp cp-nil   :type fixnum) ; to color-piece
  (scmv scmv-nil :type fixnum) ; special case indication
  (mflg 0        :type fixnum) ; move flags
  )

;; The null move

(defvar null-move
  (make-move :mflg mflg-null))

;; The empty move

(defvar empty-move
  (make-move))

;; PGN Seven Tag Roster

(defconstant tag-name-limit 7) ; the Seven Tag Roster

(defconstant tag-name-event  0) ; PGN Event
(defconstant tag-name-site   1) ; PGN Site
(defconstant tag-name-date   2) ; PGN Date
(defconstant tag-name-round  3) ; PGN Round
(defconstant tag-name-white  4) ; PGN White
(defconstant tag-name-black  5) ; PGN Black
(defconstant tag-name-result 6) ; PGN Result

(defparameter tag-name-strings
  (make-array tag-name-limit
               :initial-contents
              `("Event" "Site" "Date" "Round" "White" "Black" "Result")))

;; game termination indicators

(defconstant gtim-limit 4)
(defconstant gtim-nil -1)

(defconstant gtim-white   0) ; White wins
(defconstant gtim-black   1) ; Black wins
(defconstant gtim-draw    2) ; drawn
(defconstant gtim-unknown 3) ; unknown or not specified

(defparameter gtim-strings
  (make-array gtim-limit
               :initial-contents '("1-0" "0-1" "1/2-1/2" "*")))

;; Fifty move draw rule limit

(defconstant fmrfmv-limit 50)
(defconstant fmrhmv-limit (* fmrfmv-limit rc-limit))

;; Ply search depth limit

(defconstant ply-limit 64)

;; Game full move history limit (maximum full moves per game)

(defconstant gfmh-limit 200)

;; Game half move history limit (maximum half moves per game)

(defconstant ghmh-limit (* gfmh-limit rc-limit))

;; The null bitboard

(defparameter null-bb
  (make-array sq-limit
              :element-type 'bit
              :initial-element 0))

;; Directional edge bitboard vector (dx0..dx7)

(defvar debbv
  (make-array rdx-limit
              :element-type `(simple-bit-vector ,sq-limit)
              :initial-element null-bb))

;; Directional scanning array of lists of squares

(defvar ds-offsets
  (make-array `(,dx-limit ,sq-limit)
              :element-type 'fixnum
              :initial-element 0))

;; Sum of ray/knight distances for all squares and all directions

(defconstant ds-square-limit 2816)

;; Squares along a ray/knight (indexed by ds-offsets)

(defvar ds-squares
  (make-array ds-square-limit
              :element-type 'fixnum
              :initial-element 0))

;; Directional locator; gives direction from sq0 to sq1

(defvar dloc
  (make-array `(,sq-limit ,sq-limit)
              :element-type 'fixnum
              :initial-element dx-nil))

;; On-board-next; check for continuing along a direction from a square

(defvar obnext
  (make-array `(,dx-limit ,sq-limit)
              :element-type t
              :initial-element nil))              

;; Interpath squares bitboard array

(defvar ipbbv
  (make-array `(,sq-limit ,sq-limit)
              :element-type `(simple-bit-vector ,sq-limit)
              :initial-element null-bb))

;; Knight moves bitboard vector (sq-a1..sq-h8)

(defvar nmbbv
  (make-array sq-limit
              :element-type `(simple-bit-vector ,sq-limit)
              :initial-element null-bb))

;; King moves bitboard vector (sq-a1..sq-h8)

(defvar kmbbv
  (make-array sq-limit
              :element-type `(simple-bit-vector ,sq-limit)
              :initial-element null-bb))

;; Centipawn material evaluations (can be tuned)

(defconstant cpe-p 100) ; pawn
(defconstant cpe-n 325) ; knight
(defconstant cpe-b 350) ; bishop
(defconstant cpe-r 500) ; rook
(defconstant cpe-q 900) ; queen
(defconstant cpe-k   0) ; king

(defparameter cpe-pv
  (make-array rp-limit
              :element-type 'fixnum
              :initial-contents
              `(,cpe-p ,cpe-n ,cpe-b ,cpe-r ,cpe-q ,cpe-k)))

(defparameter cpe-cpv
  (make-array rcp-limit
              :element-type 'fixnum
              :initial-contents
              `(,cpe-p ,cpe-n ,cpe-b ,cpe-r ,cpe-q ,cpe-k
                       ,cpe-p ,cpe-n ,cpe-b ,cpe-r ,cpe-q ,cpe-k)))

;;; --- Variables -----------------------------------------------

;;; IDV: Internal Database Variables (must keep mutually synchronized)

;; [IDV] The board

(declaim (type (simple-array fixnum 64) *board*))
(defvar *board*
  (make-array sq-limit
              :element-type 'fixnum
              :initial-element cp-v0))

;; [IDV] Current status items (included in Forsyth-Edwards Notation)

(declaim (type fixnum *actc*))
(declaim (type fixnum *pasc*))
(declaim (type fixnum *cast*))
(declaim (type fixnum *epsq*))
(declaim (type fixnum *hmvc*))
(declaim (type fixnum *fmvn*))

(defvar *actc* c-w)    ; active color
(defvar *pasc* c-b)    ; passive color (not used in FEN)
(defvar *cast* 0)      ; castling availability
(defvar *epsq* sq-nil) ; en passant target square
(defvar *hmvc* 0)      ; half move clock
(defvar *fmvn* 1)      ; full move number

;; [IDV] Color-piece occupancy bitboard vector (cp-wp..cp-bk)

(defvar *cpbbv*
  (make-array rcp-limit
              :element-type `(simple-bit-vector ,sq-limit)
              :initial-element null-bb))

;; [IDV] Color occupancy bitboard vector (unions of *cpbbv* by color)

(defvar *c0bbv*
  (make-array rc-limit
              :element-type `(simple-bit-vector ,sq-limit)
              :initial-element null-bb))

;; [IDV] All men merged bitboard (union of *c0bbv*)

(defvar *ammbb*
  (make-array sq-limit
              :element-type 'bit
              :initial-element 0))

;; [IDV] Attack to by color bitboard vector (c-w..c-b)

(defvar *acbbv*
  (make-array rc-limit
              :element-type `(simple-bit-vector ,sq-limit)
              :initial-element null-bb))

;; [IDV] Attack to by square bitboard vector (sq-a1..sq-h8)

(defvar *atbbv*
  (make-array sq-limit
              :element-type `(simple-bit-vector ,sq-limit)
              :initial-element null-bb))

;; [IDV] Attack from by square bitboard vector (sq-a1..sq-h8)

(defvar *afbbv*
  (make-array sq-limit
              :element-type `(simple-bit-vector ,sq-limit)
              :initial-element null-bb))

;;; PIV: Ply Indexed Variables

;; [PIV] The current ply, used as an index for several variables

(declaim (type fixnum *ply*))
(defvar *ply* 0)

;; [PIV] The move generation stack

(defconstant mgs-limit (* ply-limit 64)) ; average of 64 moves per ply
(defvar *mgs* (make-array mgs-limit :element-type 'move))
		
;; [PIV] The MGS base (saved values; start of moves for a ply)

(declaim (type fixnum *mgs-base-local*))
(defvar *mgs-base-local* 0) ; local for this ply (direct index to *mgs*)

(defvar *mgs-base*
  (make-array ply-limit
              :element-type 'fixnum
              :initial-element 0))

;; [PIV] The MGS current move index (saved values; current move in ply)

(declaim (type fixnum *mgs-current-local*))
(defvar *mgs-current-local* 0) ; local for this ply (direct index to *mgs*)

(defvar *mgs-current*
  (make-array ply-limit
              :element-type 'fixnum
              :initial-element 0))

;; [PIV] The MGS move count (saved values; number of moves per ply)

(declaim (type fixnum *mgs-count-local*))
(defvar *mgs-count-local* 0) ; local value for this ply

(defvar *mgs-count*
  (make-array ply-limit
              :element-type 'fixnum
              :initial-element 0))

;; [PIV] The MGS castling (indicates castling status at ply)

(defvar *mgs-cast*
  (make-array ply-limit
              :element-type 'fixnum
              :initial-element 0))

;; [PIV] The MGS ep targets (indicates ep target at ply)

(defvar *mgs-epsq*
  (make-array ply-limit
              :element-type 'fixnum
              :initial-element sq-nil))

;; [PIV] The MGS halfmove clocks (indicates hmvc at ply)

(defvar *mgs-hmvc*
  (make-array ply-limit
              :element-type 'fixnum
              :initial-element 0))

;; [PIV] The MGS fullmove numbers (indicates fmvn at ply)

(defvar *mgs-fmvn*
  (make-array ply-limit
              :element-type 'fixnum
              :initial-element 0))

;;; GHV: Game History Variables

;; [GHV] Move count in history; the master index for the GHV set (halfmoves)

(declaim (type fixnum *gmh-count*))
(defvar *gmh-count* 0)

;; [GHV] Moves in history

(defvar *gmh-move* (make-array ghmh-limit :element-type 'move))

;; [GHV] Boards in history

(defvar *gmh-board* (make-array ghmh-limit))

;; [GHV] Active colors in history

(defvar *gmh-actc* (make-array ghmh-limit :element-type 'fixnum))

;; [GHV] Castling availabilities in history

(defvar *gmh-cast* (make-array ghmh-limit :element-type 'fixnum))

;; [GHV] En passant target squares in history

(defvar *gmh-epsq* (make-array ghmh-limit :element-type 'fixnum))

;; [GHV] Halfmove clocks in history

(defvar *gmh-hmvc* (make-array ghmh-limit :element-type 'fixnum))

;; [GHV] Fullmove numbers in history

(defvar *gmh-fmvn* (make-array ghmh-limit :element-type 'fixnum))

;; Counters

(defvar *count-execute* 0)

;; Files

(defvar *pathway-file-stream* nil)

;;; --- Functions -----------------------------------------------

;;; *** Attack bitboard database management functions

(defun attack-add (sq)
  "Add attacks for a square; piece already on board"
  
  (declare (type fixnum sq))
  
  (let* ((cp (aref *board* sq))
         (c (aref mapv-c cp))
         (p (aref mapv-p cp))
         (bb (copy-seq null-bb)))
    
    (declare (type fixnum cp c p))
    
    (cond
     ((eql p p-p)
      (if (eql c c-w)
        (progn
          (if (aref obnext dx-4 sq)
            (setf (sbit bb (+ sq dv-4)) 1))
          (if (aref obnext dx-5 sq)
            (setf (sbit bb (+ sq dv-5)) 1)))
        (progn
          (if (aref obnext dx-6 sq)
            (setf (sbit bb (+ sq dv-6)) 1))
          (if (aref obnext dx-7 sq)
            (setf (sbit bb (+ sq dv-7)) 1)))))
     
     ((eql p p-n)
      (setf bb (copy-seq (aref nmbbv sq))))
     
     ((eql p p-b)
      (do* ((dx dx-4)) ((eql dx dx-8))
        (declare (type fixnum dx))
        (let* ((sqdex (aref ds-offsets dx sq)) (rsq (aref ds-squares sqdex)))
          (declare (type fixnum sqdex rsq))
          (do ()
              ((or
                (eql rsq sq-nil)
                (not (eql (aref *board* rsq) cp-v0))))
            (setf (sbit bb rsq) 1)
            (incf sqdex)
            (setf rsq (aref ds-squares sqdex)))
          (if (not (eql rsq sq-nil))
            (setf (sbit bb rsq) 1)))
        (incf dx)))
     
     ((eql p p-r)
      (do* ((dx dx-0)) ((eql dx dx-4))
        (declare (type fixnum dx))
        (let* ((sqdex (aref ds-offsets dx sq)) (rsq (aref ds-squares sqdex)))
          (declare (type fixnum sqdex rsq))
          (do ()
              ((or
                (eql rsq sq-nil)
                (not (eql (aref *board* rsq) cp-v0))))
            (setf (sbit bb rsq) 1)
            (incf sqdex)
            (setf rsq (aref ds-squares sqdex)))
          (if (not (eql rsq sq-nil))
            (setf (sbit bb rsq) 1)))
        (incf dx)))
     
     ((eql p p-q)
      (do* ((dx dx-0)) ((eql dx dx-8))
        (declare (type fixnum dx))
        (let* ((sqdex (aref ds-offsets dx sq)) (rsq (aref ds-squares sqdex)))
          (declare (type fixnum sqdex rsq))
          (do ()
              ((or
                (eql rsq sq-nil)
                (not (eql (aref *board* rsq) cp-v0))))
            (setf (sbit bb rsq) 1)
            (incf sqdex)
            (setf rsq (aref ds-squares sqdex)))
          (if (not (eql rsq sq-nil))
            (setf (sbit bb rsq) 1)))
        (incf dx)))
     
     ((eql p p-k)
      (setf bb (copy-seq (aref kmbbv sq)))))
    
    (setf (aref *afbbv* sq) (copy-seq bb))
    (bit-ior (aref *acbbv* c) bb t)
    
    (do* ((rsq)) ((equal bb null-bb))
      (declare (type fixnum rsq))
      (setf rsq (position 1 bb))
      (setf (sbit (aref *atbbv* rsq) sq) 1)
      (setf (sbit bb rsq) 0))))

(defun attack-del (sq)
  "Delete attacks for an occupied square"
  
  (declare (type fixnum sq))

  (let* ((cp (aref *board* sq))
         (c (aref mapv-c cp))
         (bb (copy-seq (aref *afbbv* sq))))
    
    (declare (type fixnum cp c))

    (setf (aref *afbbv* sq) (copy-seq null-bb))
    
    (do* ((rsq)) ((equal bb null-bb))
      (declare (type fixnum rsq))
      (setf rsq (position 1 bb))
      (setf (sbit bb rsq) 0)
      (setf (sbit (aref *atbbv* rsq) sq) 0)
      (if (equal
           (bit-and (aref *atbbv* rsq) (aref *c0bbv* c)) null-bb)
        (setf (sbit (aref *acbbv* c) rsq) 0)))))

(defun attack-pro (sq)
  "Propagate attacks through an empty square"
  
  (declare (type fixnum sq))

  (let* ((bb (copy-seq (aref *atbbv* sq))))
    (do* ((asq)) ((equal bb null-bb))
      (declare (type fixnum asq))
      (setf asq (position 1 bb))
      (setf (sbit bb asq) 0)
      (let* ((acp (aref *board* asq)))
        (declare (type fixnum acp))
        (when (eql (aref sweeper-cp acp) 1)
          (let*
            ((dx (aref dloc asq sq))
             (debb (copy-seq (aref debbv dx))))
            (declare (type fixnum dx))
            (if (eql (sbit debb sq) 0)
              (let*
                ((ac (aref mapv-c acp))
                 (axbb (copy-seq null-bb))
                 (bsbb (bit-ior debb *ammbb*))
                 (dv (aref mapv-dv dx))
                 (rsq sq))
                (declare (type fixnum ac dv rsq))
                (incf rsq dv)
                (setf (sbit (aref *atbbv* rsq) asq) 1)
                (setf (sbit axbb rsq) 1)
                (do () ((eql (sbit bsbb rsq) 1))
                  (incf rsq dv)
                  (setf (sbit (aref *atbbv* rsq) asq) 1)
                  (setf (sbit axbb rsq) 1))
                (bit-ior (aref *afbbv* asq) axbb t)
                (bit-ior (aref *acbbv* ac) axbb t)))))))))

(defun attack-cut (sq)
  "Cut attacks through an empty square"
  
  (declare (type fixnum sq))

  (let* ((bb (copy-seq (aref *atbbv* sq))))
    (do* ((asq)) ((equal bb null-bb))
      (declare (type fixnum asq))
      (setf asq (position 1 bb))
      (setf (sbit bb asq) 0)
      (let* ((acp (aref *board* asq)))
        (declare (type fixnum acp))
        (when (eql (aref sweeper-cp acp) 1)
          (let*
            ((dx (aref dloc asq sq))
             (debb (copy-seq (aref debbv dx))))
            (declare (type fixnum dx))
            (if (eql (sbit debb sq) 0)
              (let*
                ((ac (aref mapv-c acp))
                 (c0bb (copy-seq (aref *c0bbv* ac)))
                 (bsbb (bit-ior debb *ammbb*))
                 (dv (aref mapv-dv dx))
                 (rsq sq))
                (declare (type fixnum ac dv rsq))
                (incf rsq dv)
                (setf (sbit (aref *atbbv* rsq) asq) 0)
                (setf (sbit (aref *afbbv* asq) rsq) 0)
                (when (equal
                       (bit-and (aref *atbbv* rsq) c0bb)
                       null-bb)
                  (setf (sbit (aref *acbbv* ac) rsq) 0))
                (do () ((eql (sbit bsbb rsq) 1))
                  (incf rsq dv)
                  (setf (sbit (aref *atbbv* rsq) asq) 0)
                  (setf (sbit (aref *afbbv* asq) rsq) 0)
                  (when (equal
                         (bit-and (aref *atbbv* rsq) c0bb)
                         null-bb)
                    (setf (sbit (aref *acbbv* ac) rsq) 0)))))))))))

;;; *** Square set/clear interface routines to the attack bitboard managament functions

(defun square-clear (sq)
  "Clear the contents of an occupied square"
  
  (declare (type fixnum sq))

  (let* ((cp (aref *board* sq)))
    (declare (type fixnum cp))
    (setf (sbit *ammbb* sq) 0)
    (setf (sbit (aref *c0bbv* (aref mapv-c cp)) sq) 0)
    (setf (sbit (aref *cpbbv* cp) sq) 0))
  (attack-del sq)
  (setf (aref *board* sq) cp-v0)
  (attack-pro sq))

(defun square-set (sq cp)
  "Set the contents of a vacant square"
  
  (declare (type fixnum sq cp))

  (attack-cut sq)
  (setf (aref *board* sq) cp)
  (attack-add sq)
  (setf (sbit *ammbb* sq) 1)
  (setf (sbit (aref *c0bbv* (aref mapv-c cp)) sq) 1)
  (setf (sbit (aref *cpbbv* cp) sq) 1))

;;; *** Various reset routines for the internal database variables

(defun clear-bitboard-sets ()
  "Clear all the bitboard items for the current position"
  
  (dotimes (sq sq-limit)
    (declare (type fixnum sq))
    (setf (aref *afbbv* sq) (copy-seq null-bb))
    (setf (aref *atbbv* sq) (copy-seq null-bb)))

  (dotimes (cp rcp-limit)
    (declare (type fixnum cp))
    (setf (aref *cpbbv* cp) (copy-seq null-bb)))

  (dotimes (c rc-limit)
    (declare (type fixnum c))
    (setf (aref *c0bbv* c) (copy-seq null-bb))
    (setf (aref *acbbv* c) (copy-seq null-bb)))

  (setf *ammbb* (copy-seq null-bb)))

(defun clear-position-scalars ()
  "Clear the basic position scalars"
  
  (setf *actc* c-w)
  (setf *pasc* c-b)
  (setf *cast* 0)
  (setf *epsq* sq-nil)
  (setf *hmvc* 0)
  (setf *fmvn* 1))

(defun clear-board ()
  "Clear the board array"
  
  (dotimes (sq sq-limit)
    (declare (type fixnum sq))
    (setf (aref *board* sq) cp-v0)))

(defun clear-position ()
  "Clear the current position"
  
  (clear-position-scalars)
  (clear-board)
  (clear-bitboard-sets))

;;; *** Helper routines for board/square access

(declaim (inline on-board-next))
(defun on-board-next (dx sq)
  "Determine if the next square along a direction is really on the board"
 
  (declare (type fixnum dx sq))
 
  (let*
    ((new-file (the fixnum (+ (map-file sq) (aref mapv-df dx))))
     (new-rank (the fixnum (+ (map-rank sq) (aref mapv-dr dx)))))
    (declare (type fixnum new-rank new-file))
    (and
     (>= new-file file-a) (<= new-file file-h)
     (>= new-rank rank-1) (<= new-rank rank-8))))

;;; *** Various initialization routines; each called only once

(defun initialize-obnext ()
  "Initialize the on-board-next array"

  (dotimes (dx dx-limit)
    (declare (type fixnum dx))
    (dotimes (sq sq-limit)
      (declare (type fixnum sq))
      (setf (aref obnext dx sq) (on-board-next dx sq)))))

(defun initialize-knight-move-bitboards ()
  "Initialize the knight moves bitboard vector"
  
  (dotimes (sq sq-limit)
    (declare (type fixnum sq))
    (setf (aref nmbbv sq) (copy-seq null-bb))
    (do* ((dx rdx-limit)) ((eql dx dx-limit))
      (declare (type fixnum dx))
      (if (aref obnext dx sq)
        (setf (sbit (aref nmbbv sq) (+ sq (aref mapv-dv dx))) 1))
      (incf dx))))

(defun initialize-king-move-bitboards ()
  "Initialize the king moves bitboard vector"
  
  (dotimes (sq sq-limit)
    (declare (type fixnum sq))
    (setf (aref kmbbv sq) (copy-seq null-bb))
    (dotimes (dx rdx-limit)
      (declare (type fixnum dx))
      (if (aref obnext dx sq)
        (setf (sbit (aref kmbbv sq) (+ sq (aref mapv-dv dx))) 1)))))

(defun initialize-directional-edge-bitboards ()
  "Initialize the directional edge bitboards"
  
  (dotimes (dx dx-4)
    (declare (type fixnum dx))
    (setf (aref debbv dx) (copy-seq null-bb)))
  
  (dotimes (rank rank-limit)
    (declare (type fixnum rank))
    (setf (sbit (aref debbv dx-0) (map-sq rank file-h)) 1)
    (setf (sbit (aref debbv dx-2) (map-sq rank file-a)) 1))

  (dotimes (file file-limit)
    (declare (type fixnum file))
    (setf (sbit (aref debbv dx-1) (map-sq rank-8 file)) 1)
    (setf (sbit (aref debbv dx-3) (map-sq rank-1 file)) 1))
  
  (setf (aref debbv dx-4) (bit-ior (aref debbv dx-0) (aref debbv dx-1)))
  (setf (aref debbv dx-5) (bit-ior (aref debbv dx-1) (aref debbv dx-2)))
  (setf (aref debbv dx-6) (bit-ior (aref debbv dx-2) (aref debbv dx-3)))
  (setf (aref debbv dx-7) (bit-ior (aref debbv dx-3) (aref debbv dx-0))))

(defun initialize-directional-scanning-array ()
  "Initialize the direction scanning items: offsets and squares"
  
  (let* ((sqdex 0))
    (declare (type fixnum sqdex))
    (dotimes (dx rdx-limit)
      (declare (type fixnum dx))
      (let* ((delta (aref mapv-dv dx)) (edge (copy-seq (aref debbv dx))))
        (declare (type fixnum delta))
        (dotimes (sq sq-limit)
          (declare (type fixnum sq))
          (setf (aref ds-offsets dx sq) sqdex)
          (let* ((rsq sq))
            (declare (type fixnum rsq))
            (do* () ((eql (sbit edge rsq) 1))
              (incf rsq delta)
              (setf (aref ds-squares sqdex) rsq)
              (incf sqdex))
            (setf (aref ds-squares sqdex) sq-nil)
            (incf sqdex)))))
    
    (do* ((dx rdx-limit)) ((eql dx dx-limit))
      (declare (type fixnum dx))
      (dotimes (sq sq-limit)
        (declare (type fixnum sq))
        (setf (aref ds-offsets dx sq) sqdex)
        (when (aref obnext dx sq)
          (setf (aref ds-squares sqdex) (+ sq (aref mapv-dv dx)))
          (incf sqdex))		
        (setf (aref ds-squares sqdex) sq-nil)
        (incf sqdex))
      (incf dx))))		

(defun initialize-directional-locator-array ()
  "Intiailize the directional locator array"
  
  (dotimes (sq0 sq-limit)
    (declare (type fixnum sq0))
    (dotimes (sq1 sq-limit)
      (declare (type fixnum sq1))
      (setf (aref dloc sq0 sq1) dx-nil)))
  
  (dotimes (sq0 sq-limit)
    (declare (type fixnum sq0))
    (dotimes (dx dx-limit)
      (declare (type fixnum dx))
      (do* ((sqdex (aref ds-offsets dx sq0))) ((eql (aref ds-squares sqdex) sq-nil))
        (declare (type fixnum sqdex))
        (setf (aref dloc sq0 (aref ds-squares sqdex)) dx)
        (incf sqdex)))))

(defun initialize-intersquare-pathway-bitboards ()
  "Initialize the intersquare pathway bitboard vector"

  (dotimes (sq0 sq-limit)
    (declare (type fixnum sq0))
    (dotimes (sq1 sq-limit)
      (declare (type fixnum sq1))
      (setf (aref ipbbv sq0 sq1) (copy-seq null-bb))))
  
  (dotimes (sq0 sq-limit)
    (declare (type fixnum sq0))
    (dotimes (sq1 sq-limit)
      (declare (type fixnum sq1))
      (let* ((dx (aref dloc sq0 sq1)))
        (declare (type fixnum dx))
        (if (and (>= dx dx-0) (<= dx dx-7))
          (do* ((rsq (+ sq0 (aref mapv-dv dx)))) ((eql rsq sq1))
            (declare (type fixnum rsq))
            (setf (sbit (aref ipbbv sq0 sq1) rsq) 1)
            (incf rsq (aref mapv-dv dx))))))))

(defun initialize-constants ()
  "Perform initialization of constant values"
  
  (initialize-obnext)
  (initialize-directional-edge-bitboards)
  (initialize-directional-scanning-array)
  (initialize-directional-locator-array)
  (initialize-intersquare-pathway-bitboards)
  (initialize-knight-move-bitboards)
  (initialize-king-move-bitboards))

(defun initialize-variables ()
  "Perform initialization of variable values"
  
  (dotimes (sq sq-limit)
    (declare (type fixnum sq))
    (setf (aref *board* sq) cp-v0))

  (setf *actc* c-w)
  (setf *pasc* c-b)
  (setf *cast* (logior cflg-wk cflg-wq cflg-bk cflg-bq))
  (setf *epsq* sq-nil)
  (setf *hmvc* 0)
  (setf *fmvn* 1)

  (dotimes (cp rcp-limit)
    (declare (type fixnum cp))
    (setf (aref *cpbbv* cp) (copy-seq null-bb)))

  (dotimes (c rc-limit)
    (declare (type fixnum c))
    (setf (aref *c0bbv* c) (copy-seq null-bb)))

  (setf *ammbb* (copy-seq null-bb))

  (dotimes (sq sq-limit)
    (declare (type fixnum sq))
    (setf (aref *afbbv* sq) (copy-seq null-bb)))

  (dotimes (sq sq-limit)
    (declare (type fixnum sq))
    (setf (aref *atbbv* sq) (copy-seq null-bb)))

  (dotimes (c rc-limit)
    (declare (type fixnum c))
    (setf (aref *acbbv* c) (copy-seq null-bb)))

  (setf *ply* 0)

  (dotimes (index mgs-limit)
    (declare (type fixnum index))
    (setf (aref *mgs* index) (make-move)))

  (setf *mgs-base-local* 0)
  (setf *mgs-current-local* 0)
  (setf *mgs-count-local* 0)

  (dotimes (index ply-limit)
    (declare (type fixnum index))
    (setf (aref *mgs-base* index) 0)
    (setf (aref *mgs-current* index) 0)
    (setf (aref *mgs-count* index) 0))
 
  (dotimes (index ply-limit)
    (declare (type fixnum index))
    (setf (aref *mgs-cast* index) 0)
    (setf (aref *mgs-epsq* index) sq-nil)
    (setf (aref *mgs-hmvc* index) 0)
    (setf (aref *mgs-fmvn* index) 0))
 
  (setf *gmh-count* 0)

  (dotimes (index ghmh-limit)
    (declare (type fixnum index))
    (setf (aref *gmh-move* index) (make-move))
    (setf (aref *gmh-board* index) (copy-seq *board*))
    (setf (aref *gmh-cast* index) 0)
    (setf (aref *gmh-epsq* index) sq-nil)
    (setf (aref *gmh-hmvc* index) 0)
    (setf (aref *gmh-fmvn* index) 0))
 
  (setf *count-execute* 0)

  (new-game))

(defun initialize ()
  "Perform one time initialization"
  
  (initialize-constants)
  (initialize-variables)
  (format t "~%Ready~%")
  (values))

;;; *** Printing and string generation routines for bitboard items

(defun print-bitboard (bb)
  "Print a bitboard character string (eight lines long)"
  
  (dotimes (rank rank-limit)
    (declare (type fixnum rank))
    (dotimes (file file-limit)
      (declare (type fixnum file))
      (format t " ~d" (sbit bb (map-sq (- rank-8 rank) file))))
    (format t "~%"))
  (values))

(defun genstr-square-set (bb)
  "Generate a square set string from a bitboard"
  
  (let* ((s "[") (flag nil))
    (dotimes (sq sq-limit)
      (declare (type fixnum sq))
      (when (eql (sbit bb sq) 1)
        (if flag
          (setf s (strcat s " "))
          (setf flag t))
        (setf s (strcat s (aref sq-strings sq)))))		
    (setf s (strcat s "]"))
    s))

(defun print-square-set (bb)
  "Print a square set string from a bitboard"
  
  (format t "~a" (genstr-square-set bb))
  (values))

;;; *** Debugging output routines for bitboard items

(defun pbaf (sq)
  "Print bitboard: afbbv[sq]"

  (print-bitboard (aref *afbbv* sq)))

(defun pbat (sq)
  "Print bitboard: atbbv[sq]"
        
  (print-bitboard (aref *atbbv* sq)))

(defun pbac (c)
  "Print bitboard: acbbv[c]"

  (print-bitboard (aref *acbbv* c)))

(defun pbcp (cp)
  "Print bitboard: cpbbv[cp]"
  
  (print-bitboard (aref *cpbbv* cp)))

(defun pbc0 (c)
  "Print bitboard: c0bbv[c]"

  (print-bitboard (aref *c0bbv* c)))

;;; *** Debugging output routines for various tests

(defun pe (n)
  (pathway-enumerate n))

;;; *** Game history status routines

(defun regenerate-bitboards ()
  "Regenerate the bitboard environment from the board"
  
  (let* ((board (copy-seq *board*)))
    (clear-bitboard-sets)
    (clear-board)
    (dotimes (sq sq-limit)
      (declare (type fixnum sq))
      (if (not (eql (aref board sq) cp-v0))
        (square-set sq (aref board sq))))))

(defun history-clear ()
  "Clear the history for a new game or set-up position"

  (setf *gmh-count* 0))

(defun history-push ()
  "Push the current status items on to the history stack"

  (setf (aref *gmh-move* *gmh-count*)
        (copy-move (aref *mgs* *mgs-current-local*)))
  (setf (aref *gmh-board* *gmh-count*) (copy-seq *board*))
  (setf (aref *gmh-actc* *gmh-count*) *actc*)
  (setf (aref *gmh-cast* *gmh-count*) *cast*)
  (setf (aref *gmh-epsq* *gmh-count*) *epsq*)
  (setf (aref *gmh-hmvc* *gmh-count*) *hmvc*)
  (setf (aref *gmh-fmvn* *gmh-count*) *fmvn*)
  
  (incf *gmh-count*))

(defun history-pop ()
  "Pop the current status items off from the history stack"

  (decf *gmh-count*)
  
  (setf *board* (copy-seq (aref *gmh-board* *gmh-count*)))
  (setf *actc* (aref *gmh-actc* *gmh-count*))
  (setf *pasc* (aref invc-v *actc*))
  (setf *cast* (aref *gmh-cast* *gmh-count*))
  (setf *epsq* (aref *gmh-epsq* *gmh-count*))
  (setf *hmvc* (aref *gmh-hmvc* *gmh-count*))
  (setf *fmvn* (aref *gmh-fmvn* *gmh-count*))
  (clear-move-generation)
  (regenerate-bitboards)
  (generate)
  (setf *mgs-current-local* (ms-find-move (aref *gmh-move* *gmh-count*))))

(defun create ()
  "Create/recreate the environment at ply zero"

  (history-clear)
  (clear-move-generation)
  (if (valid-position)
    (generate)
    (format t "Warning: invalid position~%")))

;;; *** Move execution and retraction routines

(defun execute ()
  "Execute the current move in the internal environment"

  (incf *count-execute*)

  (let* ((move (aref *mgs* *mgs-current-local*)) (scmv (move-scmv move)))
    (cond
     ((eql scmv scmv-reg)
      (progn
        (if (not (eql (move-tocp move) cp-v0))
          (square-clear (move-tosq move)))
        (square-clear (move-frsq move))
        (square-set (move-tosq move) (move-frcp move))))

     ((eql scmv scmv-cks)
      (if (eql (move-frcp move) cp-wk)
        (progn
          (square-clear sq-e1)
          (square-set sq-g1 cp-wk)
          (square-clear sq-h1)
          (square-set sq-f1 cp-wr))
        (progn
          (square-clear sq-e8)
          (square-set sq-g8 cp-bk)
          (square-clear sq-h8)
          (square-set sq-f8 cp-br))))
     
     ((eql scmv scmv-cqs)
      (if (eql (move-frcp move) cp-wk)
        (progn
          (square-clear sq-e1)
          (square-set sq-c1 cp-wk)
          (square-clear sq-a1)
          (square-set sq-d1 cp-wr))
        (progn
          (square-clear sq-e8)
          (square-set sq-c8 cp-bk)
          (square-clear sq-a8)
          (square-set sq-d8 cp-br))))
     
     ((eql scmv scmv-epc)
      (if (eql (move-frcp move) cp-wp)
        (progn
          (square-clear (+ (move-tosq move) dv-3))
          (square-clear (move-frsq move))
          (square-set (move-tosq move) cp-wp))
        (progn
          (square-clear (+ (move-tosq move) dv-1))
          (square-clear (move-frsq move))
          (square-set (move-tosq move) cp-bp))))
     
     ((eql scmv scmv-ppn)
      (if (eql (move-frcp move) cp-wp)
        (progn
          (if (not (eql (move-tocp move) cp-v0))
            (square-clear (move-tosq move)))
          (square-clear (move-frsq move))
          (square-set (move-tosq move) cp-wn))
        (progn
          (if (not (eql (move-tocp move) cp-v0))
            (square-clear (move-tosq move)))
          (square-clear (move-frsq move))
          (square-set (move-tosq move) cp-bn))))
     
     ((eql scmv scmv-ppb)
      (if (eql (move-frcp move) cp-wp)
        (progn
          (if (not (eql (move-tocp move) cp-v0))
            (square-clear (move-tosq move)))
          (square-clear (move-frsq move))
          (square-set (move-tosq move) cp-wb))
        (progn
          (if (not (eql (move-tocp move) cp-v0))
            (square-clear (move-tosq move)))
          (square-clear (move-frsq move))
          (square-set (move-tosq move) cp-bb))))
     
     ((eql scmv scmv-ppr)
      (if (eql (move-frcp move) cp-wp)
        (progn
          (if (not (eql (move-tocp move) cp-v0))
            (square-clear (move-tosq move)))
          (square-clear (move-frsq move))
          (square-set (move-tosq move) cp-wr))
        (progn
          (if (not (eql (move-tocp move) cp-v0))
            (square-clear (move-tosq move)))
          (square-clear (move-frsq move))
          (square-set (move-tosq move) cp-br))))
     
     ((eql scmv scmv-ppq)
      (if (eql (move-frcp move) cp-wp)
        (progn
          (if (not (eql (move-tocp move) cp-v0))
            (square-clear (move-tosq move)))
          (square-clear (move-frsq move))
          (square-set (move-tosq move) cp-wq))
        (progn
          (if (not (eql (move-tocp move) cp-v0))
            (square-clear (move-tosq move)))
          (square-clear (move-frsq move))
          (square-set (move-tosq move) cp-bq)))))
    
    (setf *actc* (aref invc-v *actc*))
    (setf *pasc* (aref invc-v *pasc*))
    
    (setf (aref *mgs-cast* *ply*) *cast*)
    (setf (aref *mgs-epsq* *ply*) *epsq*)
    (setf (aref *mgs-hmvc* *ply*) *hmvc*)
    (setf (aref *mgs-fmvn* *ply*) *fmvn*)
    
    (mf-set mfbp-exec)
    
    (if (in-check)
      (mf-set mfbp-chec))
    
    (if (busted)
      (mf-set mfbp-bust))
    
    (when (not (eql *cast* 0))
      (if
        (and
         (logbitp csbp-wk *cast*)
         (or
          (eql (move-frsq move) sq-e1)
          (eql (move-frsq move) sq-h1)
          (eql (move-tosq move) sq-h1)))
        (setf *cast* (logxor *cast* cflg-wk)))
      
      (if 
        (and
         (logbitp csbp-wq *cast*)
         (or
          (eql (move-frsq move) sq-e1)
          (eql (move-frsq move) sq-a1)
          (eql (move-tosq move) sq-a1)))
        (setf *cast* (logxor *cast* cflg-wq)))
      
      (if 
        (and
         (logbitp csbp-bk *cast*)
         (or
          (eql (move-frsq move) sq-e8)
          (eql (move-frsq move) sq-h8)
          (eql (move-tosq move) sq-h8)))
        (setf *cast* (logxor *cast* cflg-bk)))
      
      (if 
        (and
         (logbitp csbp-bq *cast*)
         (or
          (eql (move-frsq move) sq-e8)
          (eql (move-frsq move) sq-a8)
          (eql (move-tosq move) sq-a8)))
        (setf *cast* (logxor *cast* cflg-bq))))
      
    (setf *epsq* sq-nil)
    
    (if
      (and
       (eql (move-frcp move) cp-wp)
       (eql (map-rank (move-frsq move)) rank-2)
       (eql (map-rank (move-tosq move)) rank-4))
      (setf *epsq* (+ (move-frsq move) dv-1)))
    
    (if
      (and
       (eql (move-frcp move) cp-bp)
       (eql (map-rank (move-frsq move)) rank-7)
       (eql (map-rank (move-tosq move)) rank-5))
      (setf *epsq* (+ (move-frsq move) dv-3)))
    
    (if (or (eql (aref mapv-p (move-frcp move)) p-p) (not (eql (move-tocp move) cp-v0)))
      (setf *hmvc* 0)
      (incf *hmvc*))
    
    (if (eql (aref mapv-c (move-frcp move)) c-b)
      (incf *fmvn*)))	
   
  (setf (aref *mgs-base* *ply*) *mgs-base-local*)
  (setf (aref *mgs-current* *ply*) *mgs-current-local*)
  (setf (aref *mgs-count* *ply*) *mgs-count-local*)
  
  (setf *mgs-base-local* (+ *mgs-base-local* *mgs-count-local*))
  (setf *mgs-current-local* *mgs-base-local*)
  (setf *mgs-count-local* 0)
  
  (incf *ply*))
 
(defun retract ()
  "Retract the previously executed move in the internal environment"
  
  (decf *ply*)
  
  (setf *mgs-base-local* (aref *mgs-base* *ply*))
  (setf *mgs-current-local* (aref *mgs-current* *ply*))
  (setf *mgs-count-local* (aref *mgs-count* *ply*))
  
  (setf *actc* (aref invc-v *actc*))
  (setf *pasc* (aref invc-v *pasc*))
  
  (setf *cast* (aref *mgs-cast* *ply*))
  (setf *epsq* (aref *mgs-epsq* *ply*))
  (setf *hmvc* (aref *mgs-hmvc* *ply*))
  (setf *fmvn* (aref *mgs-fmvn* *ply*))
  
  (let* ((move (aref *mgs* *mgs-current-local*)) (scmv (move-scmv move)))
    (cond
     ((eql scmv scmv-reg)
      (progn
        (square-clear (move-tosq move))
        (if (not (eql (move-tocp move) cp-v0))
          (square-set (move-tosq move) (move-tocp move)))
        (square-set (move-frsq move) (move-frcp move))))
     
     ((eql scmv scmv-cks)
      (if (eql (move-frcp move) cp-wk)
        (progn
          (square-clear sq-g1)
          (square-set sq-e1 cp-wk)
          (square-clear sq-f1)
          (square-set sq-h1 cp-wr))
        (progn
          (square-clear sq-g8)
          (square-set sq-e8 cp-bk)
          (square-clear sq-f8)
          (square-set sq-h8 cp-br))))
     
     ((eql scmv scmv-cqs)
      (if (eql (move-frcp move) cp-wk)
        (progn
          (square-clear sq-c1)
          (square-set sq-e1 cp-wk)
          (square-clear sq-d1)
          (square-set sq-a1 cp-wr))
        (progn
          (square-clear sq-c8)
          (square-set sq-e8 cp-bk)
          (square-clear sq-d8)
          (square-set sq-a8 cp-br))))
     
     ((eql scmv scmv-epc)
      (if (eql (move-frcp move) cp-wp)
        (progn
          (square-clear (move-tosq move))
          (square-set (move-frsq move) cp-wp)
          (square-set (+ (move-tosq move) dv-3) cp-bp))
        (progn
          (square-clear (move-tosq move))
          (square-set (move-frsq move) cp-bp)
          (square-set (+ (move-tosq move) dv-1) cp-wp))))
     
     ((eql scmv scmv-ppn)
      (if (eql (move-frcp move) cp-wp)
        (progn
          (square-clear (move-tosq move))
          (square-set (move-frsq move) cp-wp)
          (if (not (eql (move-tocp move) cp-v0))
            (square-set (move-tosq move) (move-tocp move))))
        (progn
          (square-clear (move-tosq move))
          (square-set (move-frsq move) cp-bp)
          (if (not (eql (move-tocp move) cp-v0))
            (square-set (move-tosq move) (move-tocp move))))))
     
     ((eql scmv scmv-ppb)
      (if (eql (move-frcp move) cp-wp)
        (progn
          (square-clear (move-tosq move))
          (square-set (move-frsq move) cp-wp)
          (if (not (eql (move-tocp move) cp-v0))
            (square-set (move-tosq move) (move-tocp move))))
        (progn
          (square-clear (move-tosq move))
          (square-set (move-frsq move) cp-bp)
          (if (not (eql (move-tocp move) cp-v0))
            (square-set (move-tosq move) (move-tocp move))))))
     
     ((eql scmv scmv-ppr)
      (if (eql (move-frcp move) cp-wp)
        (progn
          (square-clear (move-tosq move))
          (square-set (move-frsq move) cp-wp)
          (if (not (eql (move-tocp move) cp-v0))
            (square-set (move-tosq move) (move-tocp move))))
        (progn
          (square-clear (move-tosq move))
          (square-set (move-frsq move) cp-bp)
          (if (not (eql (move-tocp move) cp-v0))
            (square-set (move-tosq move) (move-tocp move))))))
     
     ((eql scmv scmv-ppq)
      (if (eql (move-frcp move) cp-wp)
        (progn
          (square-clear (move-tosq move))
          (square-set (move-frsq move) cp-wp)
          (if (not (eql (move-tocp move) cp-v0))
            (square-set (move-tosq move) (move-tocp move))))
        (progn
          (square-clear (move-tosq move))
          (square-set (move-frsq move) cp-bp)
          (if (not (eql (move-tocp move) cp-v0))
            (square-set (move-tosq move) (move-tocp move)))))))))

;;; *** PGN routines

(defun genstr-tag-pair (tag-name tag-value)
  "Generate a tag pair string"
  
  (format nil "[~a \"~a\"]" tag-name tag-value))

(defun print-tag-pair (tag-name tag-value)
  "Print a tag pair string on a line"
  
  (format t "~a~%" (genstr-tag-pair tag-name tag-value)))

(defun strcat (s0 s1)
  "Return the concatenation of two strings"
  
  (let* ((len0 (length s0)) (len1 (length s1))
         (s2 (make-string (+ len0 len1))))
    
    (dotimes (i len0)
      (setf (schar s2 i) (schar s0 i)))
    (dotimes (i len1)
      (setf (schar s2 (+ len0 i)) (schar s1 i)))
    s2))

;;; *** Move generation routines

(defun generate-psuedolegal-frsq-wp (sq)
  "Generate psuedolegal moves for a white pawn"
  
  (declare (type fixnum sq))

  (let* ((gmove (make-move)))
    (when (eql (aref *board* (+ sq dv-1)) cp-v0)
      (if (not (eql (map-rank sq) rank-7))
        (progn
          (setf (move-frsq gmove) sq)
          (setf (move-tosq gmove) (+ sq dv-1))
          (setf (move-frcp gmove) cp-wp)
          (setf (move-tocp gmove) cp-v0)
          (setf (move-scmv gmove) scmv-reg)
          (setf (move-mflg gmove) 0)
          (setf (aref *mgs* *mgs-current-local*) (copy-move gmove))
          (incf *mgs-count-local*)
          (incf *mgs-current-local*)
          
          (when
            (and
             (eql (map-rank sq) rank-2)
             (eql (aref *board* (+ sq (* dv-1 2))) cp-v0))
            
            (setf (move-frsq gmove) sq)
            (setf (move-tosq gmove) (+ sq (* dv-1 2)))
            (setf (move-frcp gmove) cp-wp)
            (setf (move-tocp gmove) cp-v0)
            (setf (move-scmv gmove) scmv-reg)
            (setf (move-mflg gmove) 0)
            (setf (aref *mgs* *mgs-current-local*) (copy-move gmove))
            (incf *mgs-count-local*)
            (incf *mgs-current-local*)))
        
        (do* ((scmv scmv-ppn)) ((eql scmv scmv-limit))
          (declare (type fixnum scmv))
          (setf (move-frsq gmove) sq)
          (setf (move-tosq gmove) (+ sq dv-1))
          (setf (move-frcp gmove) cp-wp)
          (setf (move-tocp gmove) cp-v0)
          (setf (move-scmv gmove) scmv)
          (setf (move-mflg gmove) 0)
          (setf (aref *mgs* *mgs-current-local*) (copy-move gmove))
          (incf *mgs-count-local*)
          (incf *mgs-current-local*)
          (incf scmv))))
    
    (dolist (dx `(,dx-4 ,dx-5))
      (declare (type fixnum dx))
      (if (aref obnext dx sq)
        (let*
          ((tosq (+ sq (aref mapv-dv dx)))
           (tocp (aref *board* tosq)))
          (declare (type fixnum tosq tocp))
          (when (eql (aref mapv-c tocp) c-b)
            (if (not (eql (map-rank sq) rank-7))
              (progn
                (setf (move-frsq gmove) sq)
                (setf (move-tosq gmove) tosq)
                (setf (move-frcp gmove) cp-wp)
                (setf (move-tocp gmove) tocp)
                (setf (move-scmv gmove) scmv-reg)
                (setf (move-mflg gmove) 0)
                (setf (aref *mgs* *mgs-current-local*) (copy-move gmove))
                (incf *mgs-count-local*)
                (incf *mgs-current-local*))
              (progn
                (do* ((scmv scmv-ppn)) ((eql scmv scmv-limit))
                  (setf (move-frsq gmove) sq)
                  (setf (move-tosq gmove) tosq)
                  (setf (move-frcp gmove) cp-wp)
                  (setf (move-tocp gmove) tocp)
                  (setf (move-scmv gmove) scmv)
                  (setf (move-mflg gmove) 0)
                  (setf (aref *mgs* *mgs-current-local*) (copy-move gmove))
                  (incf *mgs-count-local*)
                  (incf *mgs-current-local*)
                  (incf scmv))))))))
    
    (dolist (dx `(,dx-4 ,dx-5))
      (declare (type fixnum dx))
      (when
        (and
         (not (eql *epsq* sq-nil))
         (eql (map-rank sq) rank-5)
         (aref obnext dx sq)
         (eql *epsq* (+ sq (aref mapv-dv dx))))
        
        (setf (move-frsq gmove) sq)
        (setf (move-tosq gmove) *epsq*)
        (setf (move-frcp gmove) cp-wp)
        (setf (move-tocp gmove) cp-v0)
        (setf (move-scmv gmove) scmv-epc)
        (setf (move-mflg gmove) 0)
        (setf (aref *mgs* *mgs-current-local*) (copy-move gmove))
        (incf *mgs-count-local*)
        (incf *mgs-current-local*)))))

(defun generate-psuedolegal-frsq-bp (sq)
  "Generate psuedolegal moves for a black pawn"
  
  (declare (type fixnum sq))

  (let* ((gmove (make-move)))
    (when (eql (aref *board* (+ sq dv-3)) cp-v0)
      (if (not (eql (map-rank sq) rank-2))
        (progn
          (setf (move-frsq gmove) sq)
          (setf (move-tosq gmove) (+ sq dv-3))
          (setf (move-frcp gmove) cp-bp)
          (setf (move-tocp gmove) cp-v0)
          (setf (move-scmv gmove) scmv-reg)
          (setf (move-mflg gmove) 0)
          (setf (aref *mgs* *mgs-current-local*) (copy-move gmove))
          (incf *mgs-count-local*)
          (incf *mgs-current-local*)
          
          (when
            (and
             (eql (map-rank sq) rank-7)
             (eql (aref *board* (+ sq (* dv-3 2))) cp-v0))
            
            (setf (move-frsq gmove) sq)
            (setf (move-tosq gmove) (+ sq (* dv-3 2)))
            (setf (move-frcp gmove) cp-bp)
            (setf (move-tocp gmove) cp-v0)
            (setf (move-scmv gmove) scmv-reg)
            (setf (move-mflg gmove) 0)
            (setf (aref *mgs* *mgs-current-local*) (copy-move gmove))
            (incf *mgs-count-local*)
            (incf *mgs-current-local*)))
        
        (do* ((scmv scmv-ppn)) ((eql scmv scmv-limit))
          (declare (type fixnum scmv))
          (setf (move-frsq gmove) sq)
          (setf (move-tosq gmove) (+ sq dv-3))
          (setf (move-frcp gmove) cp-bp)
          (setf (move-tocp gmove) cp-v0)
          (setf (move-scmv gmove) scmv)
          (setf (move-mflg gmove) 0)
          (setf (aref *mgs* *mgs-current-local*) (copy-move gmove))
          (incf *mgs-count-local*)
          (incf *mgs-current-local*)
          (incf scmv))))
    
    (dolist (dx `(,dx-6 ,dx-7))
      (declare (type fixnum dx))
      (if (aref obnext dx sq)
        (let*
          ((tosq (+ sq (aref mapv-dv dx)))
           (tocp (aref *board* tosq)))
          (declare (type fixnum tosq tocp))
          (when (eql (aref mapv-c tocp) c-w)
            (if (not (eql (map-rank sq) rank-2))
              (progn
                (setf (move-frsq gmove) sq)
                (setf (move-tosq gmove) tosq)
                (setf (move-frcp gmove) cp-bp)
                (setf (move-tocp gmove) tocp)
                (setf (move-scmv gmove) scmv-reg)
                (setf (move-mflg gmove) 0)
                (setf (aref *mgs* *mgs-current-local*) (copy-move gmove))
                (incf *mgs-count-local*)
                (incf *mgs-current-local*))
              (progn
                (do* ((scmv scmv-ppn)) ((eql scmv scmv-limit))
                  (declare (type fixnum scmv))
                  (setf (move-frsq gmove) sq)
                  (setf (move-tosq gmove) tosq)
                  (setf (move-frcp gmove) cp-bp)
                  (setf (move-tocp gmove) tocp)
                  (setf (move-scmv gmove) scmv)
                  (setf (move-mflg gmove) 0)
                  (setf (aref *mgs* *mgs-current-local*) (copy-move gmove))
                  (incf *mgs-count-local*)
                  (incf *mgs-current-local*)
                  (incf scmv))))))))
    
    (dolist (dx `(,dx-6 ,dx-7))
      (declare (type fixnum dx))
      (when
        (and
         (not (eql *epsq* sq-nil))
         (eql (map-rank sq) rank-4)
         (aref obnext dx sq)
         (eql *epsq* (+ sq (aref mapv-dv dx))))
        
        (setf (move-frsq gmove) sq)
        (setf (move-tosq gmove) *epsq*)
        (setf (move-frcp gmove) cp-bp)
        (setf (move-tocp gmove) cp-v0)
        (setf (move-scmv gmove) scmv-epc)
        (setf (move-mflg gmove) 0)
        (setf (aref *mgs* *mgs-current-local*) (copy-move gmove))
        (incf *mgs-count-local*)
        (incf *mgs-current-local*)))))

(defun generate-psuedolegal-frsq-wk (sq)
  "Generate psuedolegal moves for a white king"
  
  (declare (type fixnum sq))

  (let*
    ((bb (bit-and (aref *afbbv* sq) (bit-not (aref *c0bbv* c-w))))
     (gmove (make-move)))
    
    (setf bb (bit-and bb (bit-not (aref *acbbv* c-b))))
    (do* ((tosq)) ((equal bb null-bb))
      (declare (type fixnum tosq))
      (setf tosq (position 1 bb))
      (setf (sbit bb tosq) 0)
      (setf (move-frsq gmove) sq)
      (setf (move-tosq gmove) tosq)
      (setf (move-frcp gmove) cp-wk)
      (setf (move-tocp gmove) (aref *board* tosq))
      (setf (move-scmv gmove) scmv-reg)
      (setf (move-mflg gmove) 0)
      (setf (aref *mgs* *mgs-current-local*) (copy-move gmove))
      (incf *mgs-count-local*)
      (incf *mgs-current-local*))
    
    (when
      (and
       (logbitp csbp-wk *cast*)
       (eql (aref *board* sq-f1) cp-v0)
       (eql (aref *board* sq-g1) cp-v0)
       (eql (sbit (aref *acbbv* c-b) sq-e1) 0)
       (eql (sbit (aref *acbbv* c-b) sq-f1) 0)
       (eql (sbit (aref *acbbv* c-b) sq-g1) 0))
      
      (setf (move-frsq gmove) sq-e1)
      (setf (move-tosq gmove) sq-g1)
      (setf (move-frcp gmove) cp-wk)
      (setf (move-tocp gmove) cp-v0)
      (setf (move-scmv gmove) scmv-cks)
      (setf (move-mflg gmove) 0)
      (setf (aref *mgs* *mgs-current-local*) (copy-move gmove))
      (incf *mgs-count-local*)
      (incf *mgs-current-local*))			
    
    (when
      (and
       (logbitp csbp-wq *cast*)
       (eql (aref *board* sq-d1) cp-v0)
       (eql (aref *board* sq-c1) cp-v0)
       (eql (aref *board* sq-b1) cp-v0)
       (eql (sbit (aref *acbbv* c-b) sq-e1) 0)
       (eql (sbit (aref *acbbv* c-b) sq-d1) 0)
       (eql (sbit (aref *acbbv* c-b) sq-c1) 0))
      
      (setf (move-frsq gmove) sq-e1)
      (setf (move-tosq gmove) sq-c1)
      (setf (move-frcp gmove) cp-wk)
      (setf (move-tocp gmove) cp-v0)
      (setf (move-scmv gmove) scmv-cqs)
      (setf (move-mflg gmove) 0)
      (setf (aref *mgs* *mgs-current-local*) (copy-move gmove))
      (incf *mgs-count-local*)
      (incf *mgs-current-local*))))			

(defun generate-psuedolegal-frsq-bk (sq)
  "Generate psuedolegal moves for a black king"
  
  (declare (type fixnum sq))
  
  (let*
    ((bb (bit-and (aref *afbbv* sq) (bit-not (aref *c0bbv* c-b))))
     (gmove (make-move)))
    
    (setf bb (bit-and bb (bit-not (aref *acbbv* c-w))))
    (do* ((tosq)) ((equal bb null-bb))
      (declare (type fixnum tosq))
      (setf tosq (position 1 bb))
      (setf (sbit bb tosq) 0)
      (setf (move-frsq gmove) sq)
      (setf (move-tosq gmove) tosq)
      (setf (move-frcp gmove) cp-bk)
      (setf (move-tocp gmove) (aref *board* tosq))
      (setf (move-scmv gmove) scmv-reg)
      (setf (move-mflg gmove) 0)
      (setf (aref *mgs* *mgs-current-local*) (copy-move gmove))
      (incf *mgs-count-local*)
      (incf *mgs-current-local*))
    
    (when
      (and
       (logbitp csbp-bk *cast*)
       (eql (aref *board* sq-f8) cp-v0)
       (eql (aref *board* sq-g8) cp-v0)
       (eql (sbit (aref *acbbv* c-w) sq-e8) 0)
       (eql (sbit (aref *acbbv* c-w) sq-f8) 0)
       (eql (sbit (aref *acbbv* c-w) sq-g8) 0))
      
      (setf (move-frsq gmove) sq-e8)
      (setf (move-tosq gmove) sq-g8)
      (setf (move-frcp gmove) cp-bk)
      (setf (move-tocp gmove) cp-v0)
      (setf (move-scmv gmove) scmv-cks)
      (setf (move-mflg gmove) 0)
      (setf (aref *mgs* *mgs-current-local*) (copy-move gmove))
      (incf *mgs-count-local*)
      (incf *mgs-current-local*))			
    
    (when
      (and
       (logbitp csbp-bq *cast*)
       (eql (aref *board* sq-d8) cp-v0)
       (eql (aref *board* sq-c8) cp-v0)
       (eql (aref *board* sq-b8) cp-v0)
       (eql (sbit (aref *acbbv* c-w) sq-e8) 0)
       (eql (sbit (aref *acbbv* c-w) sq-d8) 0)
       (eql (sbit (aref *acbbv* c-w) sq-c8) 0))
      
      (setf (move-frsq gmove) sq-e8)
      (setf (move-tosq gmove) sq-c8)
      (setf (move-frcp gmove) cp-bk)
      (setf (move-tocp gmove) cp-v0)
      (setf (move-scmv gmove) scmv-cqs)
      (setf (move-mflg gmove) 0)
      (setf (aref *mgs* *mgs-current-local*) (copy-move gmove))
      (incf *mgs-count-local*)
      (incf *mgs-current-local*))))			

(defun generate-psuedolegal-frsq-regular (sq)
  "Generate psuedolegal moves for a knight, bishop, rook, or queen"
  
  (declare (type fixnum sq))

  (let*
    ((cp (aref *board* sq))
     (c (aref mapv-c cp))
     (bb (bit-and (aref *afbbv* sq) (bit-not (aref *c0bbv* c))))
     (gmove (make-move)))
    
    (declare (type fixnum cp c))

    (do* ((tosq)) ((equal bb null-bb))
      (declare (type fixnum tosq))
      (setf tosq (position 1 bb))
      (setf (sbit bb tosq) 0)
      (setf (move-frsq gmove) sq)
      (setf (move-tosq gmove) tosq)
      (setf (move-frcp gmove) cp)
      (setf (move-tocp gmove) (aref *board* tosq))
      (setf (move-scmv gmove) scmv-reg)
      (setf (move-mflg gmove) 0)
      (setf (aref *mgs* *mgs-current-local*) (copy-move gmove))
      (incf *mgs-count-local*)
      (incf *mgs-current-local*))))

(defun generate-psuedolegal-frsq (sq)
  "Generate psuedolegal moves for from an occupied square"
  
  (declare (type fixnum sq))

  (let* ((cp (aref *board* sq)) (p (aref mapv-p cp)) (c (aref mapv-c cp)))
    (declare (type fixnum cp p c))
    (cond
     
     ((eql p p-p)
      (if (eql c c-w)
        (generate-psuedolegal-frsq-wp sq)
        (generate-psuedolegal-frsq-bp sq)))
     
     ((eql p p-k)
      (if (eql c c-w)
        (generate-psuedolegal-frsq-wk sq)
        (generate-psuedolegal-frsq-bk sq)))
     
     (t
      (generate-psuedolegal-frsq-regular sq)))))

(defun generate-psuedolegal ()
  "Generate psuedolegal moves for the current position at the current ply"
  
  (setf *mgs-current-local* *mgs-base-local*)
  (setf *mgs-count-local* 0)
  
  (let ((bb (copy-seq (aref *c0bbv* *actc*))))
    (do* ((frsq)) ((equal bb null-bb))
      (declare (type fixnum frsq))
      (setf frsq (position 1 bb))
      (setf (sbit bb frsq) 0)
      (generate-psuedolegal-frsq frsq))))

(defun generate-legal ()
  "Generate legal moves for the current position at the current ply"
  
  (generate-psuedolegal)
  (ms-execute)
  (ms-compact))

(defun generate ()
  "Generate legal moves with full notation"
  
  (generate-legal)
  (ms-matescan)
  (ms-disambiguate))

(defun clear-move-generation ()
  "Clear the move generation variables for ply zero"
  
  (setf *mgs-base-local* 0)
  (setf *mgs-current-local* 0)
  (setf *mgs-count-local* 0))

(defun fetch-move-strings (base count)
  "Return a list of move strings for the indicated bounds"
  
  (let* ((sl nil) (index base) (limit (+ base count)))
    
    (do () ((eql index limit))			
      (setf sl (cons (genstr-san (aref *mgs* index)) sl))
      (incf index))
    
    (reverse sl)))

(defun fetch-move-strings-at-ply (ply)
  "Return a list of move strings for the indicated ply"
  
  (if (eql ply *ply*)
    (fetch-move-strings *mgs-base-local* *mgs-count-local*)
    (fetch-move-strings (aref *mgs-base* *ply*) (aref *mgs-count* *ply*))))

(defun fetch-move-strings-at-current ()
  "Return a list of move strings for the current level"
  
  (fetch-move-strings-at-ply *ply*))

(defun fetch-move-strings-at-base ()
  "Return a list of move strings for the base level"
  
  (fetch-move-strings-at-ply 0))

;;; **** Moveset manipulation routines

(defun ms-execute-print ()
  "Execute and retract each move in the current set with diagnostic output"
  
  (let* ((save-index *mgs-current-local*) (limit *mgs-count-local*))
    
    (setf *mgs-current-local* *mgs-base-local*)
    (dotimes (index limit)
      (format t "Move: ~a~%" (genstr-san (aref *mgs* *mgs-current-local*)))
      (execute)
      (format t "FEN: ~a~%" (genstr-fen))
      (retract)
      (incf *mgs-current-local*))
    (setf *mgs-current-local* save-index)
    limit))

(defun ms-execute ()
  "Execute and retract each move in the current set (move flags: exec/bust)"
  
  (let* ((save-index *mgs-current-local*) (limit *mgs-count-local*))

    (declare (type fixnum save-index limit))

    (setf *mgs-current-local* *mgs-base-local*)
    (dotimes (index limit)
      (declare (type fixnum index))
      (execute)
      (retract)
      (incf *mgs-current-local*))
    (setf *mgs-current-local* save-index)
    limit))

(defun ms-compact ()
  "Compact current moveset by eliminating illegal moves"
  
  (let* ((limit *mgs-count-local*) (busted 0) (dst 0))

    (declare (type fixnum limit busted dst))

    (dotimes (src limit)
      (declare (type fixnum src))
      (setf *mgs-current-local* (+ src *mgs-base-local*))
      (if (mf-test mfbp-bust)
        (incf busted)
        (progn
          (if (not (eql src dst))
            (setf (aref *mgs* (+ *mgs-base-local* dst))
                  (aref *mgs* (+ *mgs-base-local* src))))
          (incf dst))))
    
    (decf *mgs-count-local* busted)
    (setf *mgs-current-local* *mgs-base-local*)
    *mgs-count-local*))

(defun ms-no-moves ()
  "Determine if no moves exist for the current position"
  
  (let* ((no-moves t))
    
    (generate-psuedolegal)
    (setf *mgs-current-local* *mgs-base-local*)
    (do* ((index 0)) ((or (not no-moves) (eql index *mgs-count-local*)))
      (declare (type fixnum index))
      (execute)
      (retract)
      (if (not (mf-test mfbp-bust))
        (setf no-moves nil))
      (incf index)
      (incf *mgs-current-local*))	
    no-moves))

(defun ms-matescan ()
  "Scan for mates and set checkmate and stalemate flags"
  
  (let* ((limit *mgs-count-local*) (save-current *mgs-current-local*))

    (declare (type fixnum limit save-current))

    (setf *mgs-current-local* *mgs-base-local*)
    (dotimes (index limit)
      (declare (type fixnum index))
      (let* ((no-moves-flag))
        (when (not (mf-test mfbp-bust))
          (execute)
          (setf no-moves-flag (ms-no-moves))
          (retract)
          (if no-moves-flag
            (if (mf-test mfbp-chec)
              (mf-set mfbp-chmt)
              (mf-set mfbp-stmt))))
        (incf *mgs-current-local*)))
    (setf *mgs-current-local* save-current)))

(defun ms-disambiguate ()
  "Assign rank and file disambiguation flags in the current moveset"
  
  (let* ((save-index *mgs-current-local*) (limit *mgs-count-local*))

    (declare (type fixnum save-index limit))

    (setf *mgs-current-local* *mgs-base-local*)
    (dotimes (i limit)
      (declare (type fixnum i))
      (let* (
         (move0 (aref *mgs* *mgs-current-local*))
         (frcp0 (move-frcp move0))
         (frp0  (aref mapv-p frcp0)))
        (declare (type fixnum frcp0 frp0))

        (when (and (not (eql frp0 p-p)) (not (eql frp0 p-k)))
          (let*
            ((frsq0 (move-frsq move0))
             (tosq0 (move-tosq move0))
             (frr0 (map-rank frsq0))
             (frf0 (map-file frsq0))
             (pun-frr 0)
             (pun-frf 0)
             (pun-tosq 0))
            (declare (type fixnum frsq0 tosq0 frr0 frf0 pun-frr pun-frf pun-tosq))
            (dotimes (j limit)
              (declare (type fixnum j))
              (let*
                ((move1 (aref *mgs* (+ j *mgs-base-local*)))
                 (frcp1 (move-frcp move1))
                 (frsq1 (move-frsq move1))
                 (tosq1 (move-tosq move1)))
                (declare (type fixnum frcp1 frsq1 tosq1))
                (when (and
                       (eql frcp0 frcp1)
                       (eql tosq0 tosq1)
                       (not (eql i j)))
                  (incf pun-tosq)
                  (if (eql frr0 (map-rank frsq1))
                    (incf pun-frr))
                  (if (eql frf0 (map-file frsq1))
                    (incf pun-frf)))))								
            
            (when (> pun-tosq 0)
              (if (or (> pun-frr 0) (and (eql pun-frr 0) (eql pun-frf 0)))
                (mf-set mfbp-anfd))
              (if (> pun-frf 0)
                (mf-set mfbp-anrd))))))
      
      (incf *mgs-current-local*))
    (setf *mgs-current-local* save-index)))

;;; *** Move location routines

(defun find-san-move (san)
  "Return the move stack index of the SAN move in the current set"
  
  (let*
    ((found nil)
     (save-index *mgs-current-local*)
     (limit *mgs-count-local*)
     (index 0)
     (result -1))

    (declare (type fixnum save-index limit index result))

    (setf *mgs-current-local* *mgs-base-local*)
    (do () ((or (eql index limit) found))
      (if (string= san (genstr-san (aref *mgs* *mgs-current-local*)))
        (setf found t)
        (progn
          (incf index)
          (incf *mgs-current-local*))))
    (setf *mgs-current-local* save-index)
    (if found
      (setf result (+ index *mgs-base-local*)))
    result))

(defun ms-find-move (move)
  "Return the move stack index of the move in the current set"
  
  (let* ((found nil) (limit *mgs-count-local*) (index 0) (result -1))

    (declare (type fixnum limit index result))

    (do ((tmove)) ((or (eql index limit) found))
      (setf tmove (aref *mgs* (+ index *mgs-base-local*)))
      (if
        (and
         (eql (move-tosq tmove) (move-tosq move))
         (eql (move-frcp tmove) (move-frcp move))
         (eql (move-frsq tmove) (move-frsq move))
         (eql (move-scmv tmove) (move-scmv move))
         (eql (move-tocp tmove) (move-tocp move)))
        (setf found t)
        (incf index)))
    (if found
      (setf result (+ index *mgs-base-local*)))
    result))

(defun ms-find-move2 (frsq tosq)
  "Return the move stack index of the first matching move in the current set"
  
  (let*
    ((found nil)
     (limit *mgs-count-local*)
     (index 0)
     (result -1))

    (declare (type fixnum limit index result))

    (do ((tmove)) ((or (eql index limit) found))
      (setf tmove (aref *mgs* (+ index *mgs-base-local*)))
      (if
        (and
         (eql (move-tosq tmove) tosq)
         (eql (move-frsq tmove) frsq))
        (setf found t)
        (incf index)))
    (if found
      (setf result (+ index *mgs-base-local*)))
    result))

;;; *** Move flags access routines

(declaim (inline mf-set))
(defun mf-set (bitpos)
  "Set the indicated move flag (bit position) for the current move"
  
  (declare (type fixnum bitpos))

  (let* ((flags (move-mflg (aref *mgs* *mgs-current-local*))))
    (declare (type fixnum flags))
    (setf flags (logior flags (ash 1 bitpos)))
    (setf (move-mflg (aref *mgs* *mgs-current-local*)) flags)))

(declaim (inline mf-clear))
(defun mf-clear (bitpos)
  "Clear the indicated move flag (bit position) for the current move"
  
  (declare (type fixnum bitpos))

  (let* ((flags (move-mflg (aref *mgs* *mgs-current-local*))))
    (declare (type fixnum flags))
    (setf flags (logandc2 flags (ash 1 bitpos)))
    (setf (move-mflg (aref *mgs* *mgs-current-local*)) flags)))

(declaim (inline mf-toggle))
(defun mf-toggle (bitpos)
  "Toggle the indicated move flag (bit position) for the current move"
  
  (declare (type fixnum bitpos))
  
  (let* ((flags (move-mflg (aref *mgs* *mgs-current-local*))))
    (declare (type fixnum flags))
    (setf flags (logxor flags (ash 1 bitpos)))
    (setf (move-mflg (aref *mgs* *mgs-current-local*)) flags)))

(declaim (inline mf-test))
(defun mf-test (bitpos)
  "Test the indicated move flag (bit position) for the current move"
  
  (declare (type fixnum bitpos))

  (logbitp bitpos (move-mflg (aref *mgs* *mgs-current-local*))))

;;; *** SAN (Standard Algebraic Notation) routines

(defun genstr-san (move)
  "Return the SAN (Standard Algebraic Notation) string for a move"
  
  (let*
    ((san "")
     (frsq (move-frsq move))
     (tosq (move-tosq move))
     (frcp (move-frcp move))
     (tocp (move-tocp move))
     (scmv (move-scmv move))
     (mflg (move-mflg move))
     (frfile (map-file frsq))
     (frrank (map-rank frsq))
     (torank (map-rank tosq)))
        
    (if (logbitp mfbp-bust mflg)
      (setf san (strcat san "*")))
    
    (cond
     ((eql scmv scmv-reg)
      (if (eql (aref mapv-p frcp) p-p)
        (progn
          (setf san (strcat san (aref file-strings frfile)))
          (if (eql tocp cp-v0)
            (setf san (strcat san (aref rank-strings torank)))
            (progn
              (setf san (strcat san "x"))
              (setf san (strcat san (aref sq-strings tosq))))))	
        
        (progn
          (setf san (strcat san (aref p-strings (aref mapv-p frcp))))
          (if (logbitp mfbp-anfd mflg)
            (setf san (strcat san (aref file-strings frfile))))          
          (if (logbitp mfbp-anrd mflg)
            (setf san (strcat san (aref rank-strings frrank))))        
          (if (not (eql tocp cp-v0))
            (setf san (strcat san "x")))        
          (setf san (strcat san (aref sq-strings tosq))))))
     
     ((eql scmv scmv-cks)
      (setf san (strcat san (aref fc-strings flank-k))))
     
     ((eql scmv scmv-cqs)
      (setf san (strcat san (aref fc-strings flank-q))))
     
     ((eql scmv scmv-epc)
      (progn
        (setf san (strcat san (aref file-strings frfile)))
        (setf san (strcat san "x")))
      (setf san (strcat san (aref sq-strings tosq))))
     
     ((eql scmv scmv-ppn)
      (progn
        (setf san (strcat san (aref file-strings frfile)))
        (if (eql tocp cp-v0)
          (setf san (strcat san (aref rank-strings torank)))
          (progn
            (setf san (strcat san "x"))
            (setf san (strcat san (aref sq-strings tosq)))))	
        (setf san (strcat san "="))
        (setf san (strcat san (aref p-strings p-n)))))
    
     ((eql scmv scmv-ppb)
      (progn
        (setf san (strcat san (aref file-strings frfile)))
        (if (eql tocp cp-v0)
          (setf san (strcat san (aref rank-strings torank)))
          (progn
            (setf san (strcat san "x"))
            (setf san (strcat san (aref sq-strings tosq)))))	
        (setf san (strcat san "="))
        (setf san (strcat san (aref p-strings p-b)))))
    
     ((eql scmv scmv-ppr)
      (progn
        (setf san (strcat san (aref file-strings frfile)))
        (if (eql tocp cp-v0)
          (setf san (strcat san (aref rank-strings torank)))
          (progn
            (setf san (strcat san "x"))
            (setf san (strcat san (aref sq-strings tosq)))))	
        (setf san (strcat san "="))
        (setf san (strcat san (aref p-strings p-r)))))
    
     ((eql scmv scmv-ppq)
      (progn
        (setf san (strcat san (aref file-strings frfile)))
        (if (eql tocp cp-v0)
          (setf san (strcat san (aref rank-strings torank)))
          (progn
            (setf san (strcat san "x"))
            (setf san (strcat san (aref sq-strings tosq)))))	
        (setf san (strcat san "="))
        (setf san (strcat san (aref p-strings p-q))))))
    
    (if (logbitp mfbp-chec mflg)
      (if (logbitp mfbp-chmt mflg)
        (setf san (strcat san "#"))
        (setf san (strcat san "+"))))
    
    san))

;;; *** Re-initialization routines (may be called more than once)

(defun new-game ()
  "Initialize for a new game"
  
  (clear-position)
  
  (setf *actc* c-w)
  (setf *pasc* c-b)
  (setf *cast* (logior cflg-wk cflg-wq cflg-bk cflg-bq))
  (setf *epsq* sq-nil)
  (setf *hmvc* 0)
  (setf *fmvn* 1)
  
  (square-set sq-a1 cp-wr)
  (square-set sq-b1 cp-wn)
  (square-set sq-c1 cp-wb)
  (square-set sq-d1 cp-wq)
  (square-set sq-e1 cp-wk)
  (square-set sq-f1 cp-wb)
  (square-set sq-g1 cp-wn)
  (square-set sq-h1 cp-wr)
  
  (square-set sq-a2 cp-wp)
  (square-set sq-b2 cp-wp)
  (square-set sq-c2 cp-wp)
  (square-set sq-d2 cp-wp)
  (square-set sq-e2 cp-wp)
  (square-set sq-f2 cp-wp)
  (square-set sq-g2 cp-wp)
  (square-set sq-h2 cp-wp)
  
  (square-set sq-a7 cp-bp)
  (square-set sq-b7 cp-bp)
  (square-set sq-c7 cp-bp)
  (square-set sq-d7 cp-bp)
  (square-set sq-e7 cp-bp)
  (square-set sq-f7 cp-bp)
  (square-set sq-g7 cp-bp)
  (square-set sq-h7 cp-bp)
  
  (square-set sq-a8 cp-br)
  (square-set sq-b8 cp-bn)
  (square-set sq-c8 cp-bb)
  (square-set sq-d8 cp-bq)
  (square-set sq-e8 cp-bk)
  (square-set sq-f8 cp-bb)
  (square-set sq-g8 cp-bn)
  (square-set sq-h8 cp-br)
  
  (create))

;;; *** FEN/EPD (Forsyth-Edwards Notation/Expanded Position Description) routines

(defun genstr-ppd ()
  "Generate a piece position description string"
  
  (let* ((ppd ""))
    
    (dotimes (aux-rank rank-limit)
      (declare (type fixnum aux-rank))
      (let* ((rank (- rank-8 aux-rank)) (s 0))
        (declare (type fixnum rank s))
        (dotimes (file file-limit)
          (let* ((sq (map-sq rank file)) (cp (aref *board* sq)))
            (declare (type fixnum sq cp))
            (if (eql cp cp-v0)
              (incf s)
              (progn
                (if (> s 0)
                  (progn
                    (setf ppd (strcat ppd (format nil "~d" s)))
                    (setf s 0)))
                (if (eql (aref mapv-c cp) c-w)
                  (setf ppd (strcat ppd (aref p-strings (aref mapv-p cp))))
                  (setf ppd (strcat ppd (aref lcp-strings (aref mapv-p cp)))))))))
        (if (> s 0)
          (setf ppd (strcat ppd (format nil "~d" s))))
        (if (> rank rank-1)
          (setf ppd (strcat ppd "/")))))
    ppd))

(defun genstr-actc ()
  "Generate a string with the active color"
  
  (aref c-strings *actc*))

(defun genstr-cast ()
  "Generate a string with the castling availability"
  
  (let* ((cast ""))
    (if (eql *cast* 0)
      (setf cast (strcat cast "-"))
      (progn
        (if (logbitp csbp-wk *cast*)
          (setf cast (strcat cast (aref p-strings p-k))))
        (if (logbitp csbp-wq *cast*)
          (setf cast (strcat cast (aref p-strings p-q))))
        (if (logbitp csbp-bk *cast*)
          (setf cast (strcat cast (aref lcp-strings p-k))))
        (if (logbitp csbp-bq *cast*)
          (setf cast (strcat cast (aref lcp-strings p-q))))))
    cast))

(defun genstr-epsq ()
  "Generate a string with the en passant target square"

  (let* ((epsq ""))
    (if (eql *epsq* sq-nil)
      (setf epsq "-")
      (setf epsq (aref sq-strings *epsq*)))
    epsq))

(defun genstr-hmvc ()
  "Generate a string with the halfmove count"
  
  (format nil "~d" *hmvc*))

(defun genstr-fmvn ()
  "Generate a string with the fullmove number"
  
  (format nil "~d" *fmvn*))

(defun genstr-fen ()
  "Generate a FEN string for the current position"
  
  (let* ((fen ""))
    
    (setf fen (strcat fen (genstr-ppd)))
    
    (setf fen (strcat fen " "))
    (setf fen (strcat fen (genstr-actc)))
    
    (setf fen (strcat fen " "))
    (setf fen (strcat fen (genstr-cast)))
    
    (setf fen (strcat fen " "))
    (setf fen (strcat fen (genstr-epsq)))
    
    (setf fen (strcat fen " "))
    (setf fen (strcat fen (genstr-hmvc)))
    
    (setf fen (strcat fen " "))
    (setf fen (strcat fen (genstr-fmvn)))
    
    fen))

;;; *** Position status routines

(defun in-check ()
  "Determine if the active color is in check"
  
  (if (eql *actc* c-w)
    (not (equal (bit-and (aref *cpbbv* cp-wk) (aref *acbbv* c-b)) null-bb))
    (not (equal (bit-and (aref *cpbbv* cp-bk) (aref *acbbv* c-w)) null-bb))))

(defun busted ()
  "Determine if the passive color is in check"
  
  (if (eql *pasc* c-w)
    (not (equal (bit-and (aref *cpbbv* cp-wk) (aref *acbbv* c-b)) null-bb))
    (not (equal (bit-and (aref *cpbbv* cp-bk) (aref *acbbv* c-w)) null-bb))))

(defun valid-position ()
  "Determine if the position is valid"
  
  (let*
    ((valid t)
     (count-cpv (make-array rcp-limit :initial-element 0))
     (count-cv (make-array rc-limit :initial-element 0))
     (count-scpv (make-array `(,rc-limit ,rp-limit) :initial-element 0))
     (extra-pawns (make-array rc-limit :initial-element 0)))
    
    (dotimes (cp rcp-limit)
      (declare (type fixnum cp))
      (setf (aref count-cpv cp) (count 1 (aref *cpbbv* cp))))

    (dotimes (c rc-limit)
      (declare (type fixnum c))
      (setf (aref count-cv c) (count 1 (aref *c0bbv* c))))
    
    (dotimes (c rc-limit)
      (declare (type fixnum c))
      (dotimes (p rp-limit)
        (declare (type fixnum p))
        (setf (aref count-scpv c p)
              (count 1 (aref *cpbbv* (aref mapv-cp c p))))))
    
    (dotimes (c rc-limit)
      (declare (type fixnum c))
      (setf (aref extra-pawns c) (- 8 (aref count-scpv c p-p))))
    
    (when valid
      (if
        (or
         (< (aref count-cv c-w) 1) (> (aref count-cv c-w) 16)
         (< (aref count-cv c-b) 1) (> (aref count-cv c-b) 16))
        (setf valid nil)))
    
    (when valid
      (if
        (or
         (not (eql (aref count-cpv cp-wk) 1))
         (not (eql (aref count-cpv cp-bk) 1))
         (> (aref count-cpv cp-wp) 8)
         (> (aref count-cpv cp-bp) 8))
        (setf valid nil)))
    
    (when valid
      (if (not (equal
                (bit-and
                 (bit-ior (aref *cpbbv* cp-wp) (aref *cpbbv* cp-bp))
                 (bit-ior (aref debbv dx-1) (aref debbv dx-3)))
                null-bb))
        (setf valid nil)))
    
    (when valid
      (dotimes (c rc-limit)
        (if (> (aref count-scpv c p-n) 2)
          (decf (aref extra-pawns c) (- (aref count-scpv c p-n) 2)))
        (if (> (aref count-scpv c p-b) 2)
          (decf (aref extra-pawns c) (- (aref count-scpv c p-b) 2)))
        (if (> (aref count-scpv c p-r) 2)
          (decf (aref extra-pawns c) (- (aref count-scpv c p-r) 2)))
        (if (> (aref count-scpv c p-q) 1)
          (decf (aref extra-pawns c) (- (aref count-scpv c p-q) 1))))
      (if (or (< (aref extra-pawns c-w) 0) (< (aref extra-pawns c-b) 0))
        (setf valid nil)))
    
    (when valid
      (if (logbitp csbp-wk *cast*)
        (if (or
             (not (eql (aref *board* sq-e1) cp-wk))
             (not (eql (aref *board* sq-h1) cp-wr)))
          (setf valid nil))))
    
    (when valid
      (if (logbitp csbp-wq *cast*)
        (if (or
             (not (eql (aref *board* sq-e1) cp-wk))
             (not (eql (aref *board* sq-a1) cp-wr)))
          (setf valid nil))))
    
    (when valid
      (if (logbitp csbp-bk *cast*)
        (if (or
             (not (eql (aref *board* sq-e8) cp-bk))
             (not (eql (aref *board* sq-h8) cp-br)))
          (setf valid nil))))
    
    (when valid
      (if (logbitp csbp-bq *cast*)
        (if (or
             (not (eql (aref *board* sq-e8) cp-bk))
             (not (eql (aref *board* sq-a8) cp-br)))
          (setf valid nil))))
    
    (when valid
      (if (and (not (eql *epsq* sq-nil)) (eql *actc* c-w))
        (if
          (or
           (not (eql (map-rank *epsq*) rank-6))
           (not (eql (aref *board* *epsq*) cp-v0))
           (not (eql (aref *board* (+ *epsq* dv-3)) cp-bp))
           (not (eql (aref *board* (+ *epsq* dv-1)) cp-v0)))
          (setf valid nil))))
    
    (when valid
      (if (and (not (eql *epsq* sq-nil)) (eql *actc* c-b))
        (if
          (or
           (not (eql (map-rank *epsq*) rank-3))
           (not (eql (aref *board* *epsq*) cp-v0))
           (not (eql (aref *board* (+ *epsq* dv-1)) cp-wp))
           (not (eql (aref *board* (+ *epsq* dv-3)) cp-v0)))
          (setf valid nil))))
    
    (when valid
      (if (< *hmvc* 0) (setf valid nil)))
    
    (when valid
      (if (< *fmvn* 1) (setf valid nil)))
    
    (when valid
      (if (busted) (setf valid nil)))
    
    valid))

(defun checkmated ()
  "Determine if the active side is checkmated"
  
  (and (in-check) (ms-no-moves)))

(defun stalemated ()
  "Determine if the active side is stalemated"
  
 (and (not (in-check)) (ms-no-moves)))

;;; *** Play/unplay routines (played move history access and internal state update)

(defun play-move (san)
  "Play the given move (a SAN string) in the game"
  
  (let* ((index (find-san-move san)))
    (declare (type fixnum index))
    (if (< index 0)
      (error "Move not found")
      (progn
        (history-push)
        (setf *mgs-current-local* index)
        (execute)
        (decf *ply*)
        (clear-move-generation)
        (generate)))))

(defun unplay-move ()
  "Unplay the a move in the game"
  
  (if (< *gmh-count* 1)
    (error "Can't unplay non-existent move")
    (history-pop)))

;;; *** Mapping functions for files, ranks, and squares 

(declaim (inline map-file))
(defun map-file (sq)
  "Map a square to its file"
  
  (declare (type fixnum sq))

  (the fixnum (logand sq 7)))

(declaim (inline map-rank))
(defun map-rank (sq)
  "Map a square to its rank"
  
  (declare (type fixnum sq))

  (the fixnum (ash sq -3)))

(declaim (inline map-sq))
(defun map-sq (rank file)
  "Map a rank and a file to a square"
  
  (declare (type fixnum rank file))

  (the fixnum (logior (the fixnum (ash rank 3)) file)))

;;; *** Routines for simple display output

(defun file-print-path ()
  "Print the move path to the current position onto the filepath stream"
  
  (dotimes (ply *ply*)
    (if (not (eql ply 0))
      (format *pathway-file-stream* " "))
    (format *pathway-file-stream* "~a"
            (genstr-san (aref *mgs* (aref *mgs-current* ply)))))
  (format *pathway-file-stream* "~%")
  (values))

(defun print-board ()
  "Print the board (eight lines long)"
  
  (dotimes (rank rank-limit)
    (declare (type fixnum rank))
    (dotimes (file file-limit)
      (declare (type fixnum file))
      (let* ((sq (map-sq (- rank-8 rank) file)) (cp (aref *board* sq)))
        (declare (type fixnum sq cp))
        (if (eql cp cp-v0)
          (if (eql (logand file 1) (logand rank 1))						
            (format t "  ")
            (format t "::"))
          (format t "~a" (aref cp-strings cp)))))
    (format t "~%"))
  (values))

;;; *** Fixed depth pathway enumeration; used for testing

(defun pathway-enumerate (depth)
  "Enumerate the pathways of the current position to the given ply depth"

  (declare (type fixnum depth))

  (let* ((sum 0) (limit 0))

    (declare (type fixnum sum limit))
    (if (eql depth 0)
      (progn
        (setf sum 1)
        (if *pathway-file-stream*
          (file-print-path)))
      (progn
        (generate-psuedolegal)
        (setf limit (+ *mgs-base-local* *mgs-count-local*))
        (do* ((index *mgs-base-local*)) ((eql index limit))
          (declare (type fixnum index))
          (setf *mgs-current-local* index)
          (execute)
          (if (not (busted))
            (incf sum (pathway-enumerate (- depth 1))))
          (retract)
          (incf index))))
    sum))

;;; *** Program function verification via pathway enumeration

(defun verify-enumeration (depth count)
  "Enumerate pathways to the given depth and check the count"

  (declare (type fixnum depth count))
  (let* ((sum))
    (declare (type fixnum sum))
    (format t "Enumerating to depth ~R; please wait~%" depth)
    (setf sum (pathway-enumerate depth))
    (format t "Calculated count: ~d   Expected count: ~d   " sum count)
    (if (eql sum count)
      (format t "Operation verified~%")
      (format t "Operation *failed*~%"))
    (eql sum count)))

;;; *** Simple user interface routines

(defun ui-init ()
  "Initialization; must be called before any other functions"

  (initialize)
  (values))

(defun ui-play (san)
  "Play a SAN (string) move with update of the game history"

  (play-move san)
  (values))

(defun ui-unpl ()
  "Unplay (reverse of ui-play) the previous played move"

  (unplay-move)
  (values))

(defun ui-cvsq (sq)
  "Clear value: square"
  
  (when (not (eql (aref *board* sq) cp-v0))
    (square-clear sq)
    (create))
  (values))

(defun ui-cvcb ()
  "Clear value: chessboard"
  
  (dotimes (sq sq-limit)
    (if (not (eql (aref *board* sq) cp-v0))
      (square-clear sq)))
  (setf *cast* 0)
  (setf *epsq* sq-nil)
  (create)
  (values))

(defun ui-svsq (sq cp)
  "Set value: square"
  
  (when (not (eql (aref *board* sq) cp))
    (if (not (eql (aref *board* sq) cp-v0))
      (square-clear sq))
    (square-set sq cp)
    (create))
  (values))

(defun ui-svac (c)
  "Set value: active color"
  
  (when (not (eql c *actc*))
    (setf *actc* c)
    (setf *pasc* (aref invc-v c))
    (create))
  (values))

(defun ui-svca (cast)
  "Set value: castling availability"
  
  (when (not (eql cast *cast*))
    (setf *cast* cast)
    (create))
  (values))

(defun ui-svep (epsq)
  "Set value: en passant target square"
  
  (when (not (eql epsq *epsq*))
    (setf *epsq* epsq)
    (create))
  (values))

(defun ui-svhc (hmvc)
  "Set value: halfmove clock"
  
  (when (not (eql hmvc *hmvc*))
    (setf *hmvc* hmvc)
    (create))
  (values))

(defun ui-svfn (fmvn)
  "Set value: fullmove number"
  
  (when (not (eql fmvn *fmvn*))
    (setf *fmvn* fmvn)
    (create))
  (values))

(defun ui-dvfe ()
  "Display value: Forsyth-Edwards Notation"

  (format t "~a~%" (genstr-fen))
  (values))

(defun ui-dvcb ()
  "Display value: chessboard"
  
  (print-board)
  (values))

(defun ui-dvms ()
  "Display value: moveset"
  
  (if (valid-position)
    (let* ((movelist (copy-list (fetch-move-strings-at-base))))
      (cond
       ((eql *mgs-count-local* 0)
        (format t "There are no moves.~%")
        (if (checkmated)
          (format t "~a is checkmated.~%"
                  (aref player-strings *actc*))
          (if (stalemated)
            (format t "~a is stalemated.~%"
                    (aref player-strings *actc*)))))
       
       ((eql *mgs-count-local* 1)
        (format t "There is one move: ~a~%" (car movelist)))
       
       (t
        (setf movelist (sort movelist #'string<))
        (format t "There are ~R moves:" *mgs-count-local*)
        (dolist (pmove movelist)
          (format t " ~a" pmove))
        (format t "~%"))))
 
   (format t "Invalid position; there are no moves.~%"))
  (values))

(defun ui-newg ()
  "Set up a new game"
  
  (new-game)
  (values))

(defun ui-enum (n)
  "Enumerate distinct pathways N plies deep"
  
  (let* ((count 0))
    (if (not (valid-position))
      (format t "Can't enumerate from invalid position.~%")
      (progn
        (setf count (pathway-enumerate n))
        (format t "Pathway count: ~R~%" count))))
  (values))

(defun ui-test ()
  "Perform simple program validity testing via pathway enumeration"

  (new-game)
  (verify-enumeration 0 1)
  (verify-enumeration 1 20)
  (verify-enumeration 2 400)
  (verify-enumeration 3 8902)
  (verify-enumeration 4 197281)
  (verify-enumeration 5 4865609)
  (values))

;;; *** Simple forced mate locator programming example: fms1

(defun fms1-search (n)
  "Attempt to locate a key move for a forced mate in -n- moves"
  
  (declare (type fixnum n))

  (let* ((result nil) (key-move empty-move) (count *count-execute*))
    
    (declare (type fixnum count))
    (setf result (fms1-search-attack n))
    (setf count (- *count-execute* count))
    (if result
      (progn
        (setf key-move (copy-move (aref *mgs* *mgs-current-local*)))
        (format t "Mate in ~R key move located: ~a~%" n (genstr-san key-move)))
      (format t "No forced mate in ~R located.~%" n))
    (format t "Move count: ~R~%" count)
     
    result))

(defun fms1-search-attack (n)
  "Attempt to force mate in -n- moves; return t if success, else nil"
  
  (declare (type fixnum n))

  (let* ((result nil))
   
    (if (not (eql *ply* 0))
      (generate-psuedolegal))

    (setf *mgs-current-local* *mgs-base-local*)
    (do* ((index 0) (limit *mgs-count-local*)) ((or result (eql index limit)))
      (declare (type fixnum index limit))
      (execute)
      (if (not (busted))
        (setf result (not (fms1-search-defend (- n 1)))))
      (retract)
      (when (not result)
        (incf *mgs-current-local*)
        (incf index)))

    result))

(defun fms1-search-defend (n)
  "Attempt to defend mate in -n- moves; return t if success, else nil"
  
  (declare (type fixnum n))

  (let* ((result nil))

    (if (eql n 0)
      (setf result (not (checkmated)))
      (progn
        (generate-psuedolegal)
        (setf *mgs-current-local* *mgs-base-local*)
        (do* ((index 0) (limit *mgs-count-local*)) ((or result (eql index limit)))
          (declare (type fixnum index limit))
          (execute)
          (if (not (busted))
            (setf result (not (fms1-search-attack n))))
          (retract)
          (when (not result)
            (incf *mgs-current-local*)
            (incf index)))))
 
    result))

;;; *** Simple forced mate locator programming example: fms2

(defvar *fms2-killers* (make-array ply-limit))

(defun fms2-search (n)
  "Attempt to locate a key move for a forced mate in -n- moves"
  
  (declare (type fixnum n))

  (dotimes (index ply-limit)
    (declare (type fixnum index))
    (setf (aref *fms2-killers* index) (copy-move empty-move)))

  (let* ((result nil) (key-move empty-move) (count *count-execute*))
    
    (declare (type fixnum count))
    (setf result (fms2-search-attack n))
    (setf count (- *count-execute* count))
    (if result
      (progn
        (setf key-move (copy-move (aref *mgs* *mgs-current-local*)))
        (format t "Mate in ~R key move located: ~a~%" n (genstr-san key-move)))
      (format t "No forced mate in ~R located.~%" n))
    (format t "Move count: ~R~%" count)
     
    result))

(defun fms2-search-attack (n)
  "Attempt to force mate in -n- moves; return t if success, else nil"
  
  (declare (type fixnum n))

  (let* ((result nil) (killer-index -1))

    (declare (type fixnum killer-index))
    
    (if (not (eql *ply* 0))
      (generate-psuedolegal))
    
    (setf killer-index (ms-find-move (aref *fms2-killers* *ply*)))
    (when (>= killer-index 0)
      (setf *mgs-current-local* killer-index)
      (execute)
      (if (not (busted))
        (setf result (not (fms2-search-defend (- n 1)))))
      (retract))
    
    (when (not result)
      (setf *mgs-current-local* *mgs-base-local*)
      (do* ((index 0) (limit *mgs-count-local*)) ((or result (eql index limit)))
        (declare (type fixnum index limit))
        (when (not (eql *mgs-current-local* killer-index))
          (execute)
          (if (not (busted))
            (setf result (not (fms2-search-defend (- n 1)))))
          (retract)
          (if result
            (setf (aref *fms2-killers* *ply*)
                  (copy-move (aref *mgs* *mgs-current-local*)))))
        (when (not result)
          (incf *mgs-current-local*)
          (incf index))))
    
    result))

(defun fms2-search-defend (n)
  "Attempt to defend mate in -n- moves; return t if success, else nil"
  
  (declare (type fixnum n))

  (let* ((result nil) (killer-index -1))
    
    (declare (type fixnum killer-index))
    
    (if (eql n 0)
      (setf result (not (checkmated)))
      (progn
        (generate-psuedolegal)

        (setf killer-index (ms-find-move (aref *fms2-killers* *ply*)))
        (when (>= killer-index 0)
          (setf *mgs-current-local* killer-index)
          (execute)
          (if (not (busted))
            (setf result (not (fms2-search-attack n))))
          (retract))
    
        (when (not result)
          (setf *mgs-current-local* *mgs-base-local*)
          (do* ((index 0) (limit *mgs-count-local*)) ((or result (eql index limit)))
            (declare (type fixnum index limit))
            (when (not (eql *mgs-current-local* killer-index))
              (execute)
              (if (not (busted))
                (setf result (not (fms2-search-attack n))))
              (retract)
              (if result
                (setf (aref *fms2-killers* *ply*)
                      (copy-move (aref *mgs* *mgs-current-local*)))))
            (when (not result)
              (incf *mgs-current-local*)
              (incf index))))))
    
    result))

;;; *** Simple forced mate locator programming example: fms3

(defvar *fms3-killers* (make-array `(,rc-limit ,sq-limit ,sq-limit)))

(defun fms3-search (n)
  "Attempt to locate a key move for a forced mate in -n- moves"
  
  (declare (type fixnum n))

  (dotimes (c rc-limit)
    (declare (type fixnum c))
    (dotimes (sq0 sq-limit)
      (declare (type fixnum sq0))
      (dotimes (sq1 sq-limit)
        (declare (type fixnum sq1))
        (setf (aref *fms3-killers* c sq0 sq1) (copy-move empty-move)))))
  
  (let* ((result nil) (key-move empty-move) (count *count-execute*))
    
    (declare (type fixnum count))
    (setf result (fms3-search-attack n))
    (setf count (- *count-execute* count))
    (if result
      (progn
        (setf key-move (copy-move (aref *mgs* *mgs-current-local*)))
        (format t "Mate in ~R key move located: ~a~%" n (genstr-san key-move)))
      (format t "No forced mate in ~R located.~%" n))
    (format t "Move count: ~R~%" count)
     
    result))

(defun fms3-prev-frsq ()
  "Return the frsq of the previous move"

  (move-frsq (aref *mgs* (aref *mgs-current* (- *ply* 1)))))

(defun fms3-prev-tosq ()
  "Return the tosq of the previous move"

  (move-tosq (aref *mgs* (aref *mgs-current* (- *ply* 1)))))

(defun fms3-search-attack (n)
  "Attempt to force mate in -n- moves; return t if success, else nil"
  
  (declare (type fixnum n))
  
  (let* ((result nil) (killer-index -1))
    
    (declare (type fixnum killer-index))
    
    (if (not (eql *ply* 0))
      (generate-psuedolegal))
    
    (when (> *ply* 0)
      (setf killer-index (ms-find-move
                          (aref *fms3-killers*
                                *actc* (fms3-prev-frsq) (fms3-prev-tosq))))
      (when (>= killer-index 0)
        (setf *mgs-current-local* killer-index)
        (execute)
        (when (not (busted))
          (setf result (not (fms3-search-defend (- n 1)))))
        (retract)))
    
    (when (not result)
      (setf *mgs-current-local* *mgs-base-local*)
      (do* ((index 0) (limit *mgs-count-local*)) ((or result (eql index limit)))
        (declare (type fixnum index limit))
        (when (not (eql *mgs-current-local* killer-index))
          (execute)
          (if (not (busted))
            (setf result (not (fms3-search-defend (- n 1)))))
          (retract)
          (if (and result (> *ply* 0))
            (setf (aref *fms3-killers* *actc* (fms3-prev-frsq) (fms3-prev-tosq))
                  (copy-move (aref *mgs* *mgs-current-local*)))))
        (when (not result)
          (incf *mgs-current-local*)
          (incf index))))
    
    result))

(defun fms3-search-defend (n)
  "Attempt to defend mate in -n- moves; return t if success, else nil"
  
  (declare (type fixnum n))
  
  (let* ((result nil) (killer-index -1))
    
    (declare (type fixnum killer-index))
    
    (if (eql n 0)
      (setf result (not (checkmated)))
      (progn
        (generate-psuedolegal)
        
        (when (> *ply* 0)
          (setf killer-index (ms-find-move
                              (aref *fms3-killers*
                                    *actc* (fms3-prev-frsq) (fms3-prev-tosq))))
          (when (>= killer-index 0)
            (setf *mgs-current-local* killer-index)
            (execute)
            (when (not (busted))
              (setf result (not (fms3-search-attack n))))
            (retract)))
        
        (when (not result)
          (setf *mgs-current-local* *mgs-base-local*)
          (do* ((index 0) (limit *mgs-count-local*)) ((or result (eql index limit)))
            (declare (type fixnum index limit))
            (when (not (eql *mgs-current-local* killer-index))
              (execute)
              (if (not (busted))
                (setf result (not (fms3-search-attack n))))
              (retract)
              (if (and result (> *ply* 0))
                (setf (aref *fms3-killers* *actc* (fms3-prev-frsq) (fms3-prev-tosq))
                      (copy-move (aref *mgs* *mgs-current-local*)))))
            (when (not result)
              (incf *mgs-current-local*)
              (incf index))))))
    
    result))

;;; cil.lsp: EOF

