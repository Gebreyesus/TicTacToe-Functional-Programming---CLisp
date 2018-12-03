;;Beteab Gebru
;;Compiler's I - Lisp Project
;;11/13/2018

;;this is a tic-tac-toe game using lisp
;;the game will be built using list of 9 items. 
(defparameter *gameboard-sample* (list 'x 'x 'x '- '0 '0 '- '- '0))

;;-------------------------------------------------------------------------
;;; set the maove merker for the first move
(setf *marker* :X)
(setf *player* "Player")

;;-------------------------------------------------------------------------
;; Create the playing board and setting intial values to mark each square
(defun create-board ()
	(setf *board* (make-array 9 :initial-contents '(1 2 3 4 5 6 7 8 9))))

;;-------------------------------------------------------------------------
;; prints the borad in proper format
(defun print-board (board)
	(do ((i 0(+ i 1)))
		((+ i 9) 'done)
		(format t " ~A" (nth i board))
	)
)

;;-------------------------------------------------------------------------
;; Determines if three items are all equal to each other
(defun threequal (a b c)
  (and (equal a b) (equal b c)))

;;-------------------------------------------------------------------------
;; Determines if a list representing a row or column or diagonal
;; of a tic-tac-toe board is a victory
;; FIXME: needs to return false for three '- items in the list.
(defun victory (alist) 
  (and (equal (first alist) (second alist))
       (equal (second alist) (third alist))))

;;-------------------------------------------------------------------------
;; Returns a list consisting of the nth row (zero-based) of
;; a tic-tac-toe board
(defun grab-row (board row)
  (let ((x (* 3 row)))
    (list (nth x board)
          (nth (+ x 1) board)
          (nth (+ x 2) board))
  )
)

;;-------------------------------------------------------------------------
;; Returns a list consisting of the nth column (zero-based) of
;; a tic-tac-toe board
(defun grab-col (board col)
  (list (nth col board)
        (nth (+ col 3) board)
        (nth (+ col 6) board)))

;;-------------------------------------------------------------------------

