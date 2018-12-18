;;Beteab Gebru
;;Compiler's I - Lisp Project
;;11/13/2018

;;this is a tic-tac-toe game using lisp
;;the game will be built using list of 9 items. 
(defparameter *gameboard-sample* (list 'x 'x 'x '- '0 '0 '- '- '0))

;;-------------------------------------------------------------------------
;; Create the Playing board as a array with 9 spots(array indexes).
;; We will label each spot to allow users to make Play by entering spot number
(defun InitialiseBoard () 
	(setf *board* (make-array 9 :initial-contents '(1 2 3 4 5 6 7 8 9))))

;;-------------------------------------------------------------------------
;;; Defines funtion to selct squares from entry
(defun LookUpBoard (cell) (aref *board* (1- cell)))

;;-------------------------------------------------------------------------
;; Setting values :- Player 1 will be represented by symbol ' X ' 
(setf *P1-marker* :X) 
(setf *Player* "First Player")

;;-------------------------------------------------------------------------
;; Next players turn --> change players
(defun NextPlayer ()
	(if (equal *P1-marker* :X) (setf *P1-marker* :O) (setf *P1-marker* :X))
	(if (equal *Player* "Player 1")	
		(setf *Player* "Player 2") (setf *Player* "Player 1")))

;;-------------------------------------------------------------------------
;; handling invlaid entry
(defun invalidEntry ()
	(format t " ~% Square is occupied - Enter Again! ~%")
	(draw-board) (force-output nil)	(checkUserSelection (readSelection)))

;;-------------------------------------------------------------------------
;; Marks selcted spot if selection is valid
(defun Mark (selection)
	(if (numberp (LookUpBoard Entry)) 
	(setf (LookUpBoard Entry) *P1-marker*) (invalidEntry) ) )

;;-------------------------------------------------------------------------
;;; Show user the board and read selection into 'Player 1'
(defun readSelection ()
	(draw-board)
	(format t "~% Enter a square ID(Number) to mark ~%" *Player*) 
	(force-output nil)
	(parse-integer (read-line *query-io*) :junk-allowed t))

;;-------------------------------------------------------------------------
;; Determines if three items are all equal to each other		
(defun LookUpBoard (cell)  (aref *board* (1- cell)))

;;-------------------------------------------------------------------------
;; Determines if three items are all equal to each other
;; row or column or diagonal values(a,b,c) are passed as to be compared
(defun threequal (a b c)
  (and (equal a b) (equal b c)))

;;-------------------------------------------------------------------------
;; re-run the game to Play again
(defun PlayAgain ()
	(create-board)	(format t "New Game - Start ~a.~%~%" *Player*)
	(Play))

;;-------------------------------------------------------------------------
;; Check for a win by brute-force checking all permutations on a line
(defun Winnner ()
	(or (threequal 1 2 3)
		(threequal 1 4 7)
		(threequal 1 5 9)
		(threequal 2 5 8)
		(threequal 3 6 9)
		(threequal 3 5 7)
		(threequal 4 5 6)
		(threequal 7 8 9)))

;;-------------------------------------------------------------------------
;; Check of it is a draw 
(defun Draw ()
	(when (and
			(not (numberp (aref *board* 0)))
			(not (numberp (aref *board* 1)))
			(not (numberp (aref *board* 2)))
			(not (numberp (aref *board* 3)))
			(not (numberp (aref *board* 4)))
			(not (numberp (aref *board* 5)))
			(not (numberp (aref *board* 6)))
			(not (numberp (aref *board* 7)))
			(not (numberp (aref *board* 8))) ) t))
	
;;-------------------------------------------------------------------------
;; defines user selection. only accepts selction 0-9, 
;; will ask user again for wrong entries
(defun checkUserSelection (selection)
	(if (and (numberp selection)  (> selection 0) (< selection 10))
		(Mark selection)
		(progn (format t "~% Entry is Invalid ~%")
		(check-selection (read-selection)))))

;;-------------------------------------------------------------------------
;;; Draw the board
;;; If numbersp is t, draw the numbers on the board
(defun draw-board ()
	(format t "=====================~%")
	(format t "||   ~a |   ~a |   ~a ~%||" 
			(LookUpBoard 1) (LookUpBoard 2) (LookUpBoard 3))
	(format t "||     |      |     ~%||")
	(format t "||___________________~%||")
	(format t "||   ~a |   ~a |   ~a ~%||" 
			(LookUpBoard 4) (LookUpBoard 5) (LookUpBoard 6))
	(format t "||     |      |     ~%||")
	(format t "||___________________~%||")
	(format t "||   ~a |   ~a |   ~a ~%||" 
			(LookUpBoard 7) (LookUpBoard 8) (LookUpBoard 9))
	(format t "=====================~%||")

;;-------------------------------------------------------------------------
;; If game is not paused(continue=true(t)) we keep asking for next move
;; If current move doesn't create stalemate or a win -> recursively play
;; If the current move creates draw or a win -> prmpts for new gaem
(defun Play (&optional ContinueGame)
  (when ContinueGame (NextPlayer))
  (checkUserSelection (readSelection))
  (when (and (not (Winnner)) (not (Draw))) (Play t))
  (when (Winnner) (progn (format t "~a is the Winner! " *Player*)
      (force-output nil) (if (y-or-n-p "Restart Game!? -> Enter Y or N") 
		(PlayAgain)
	  	(quit))) )
  (when (Draw) 
  	(if (y-or-n-p "~%~% Its a Draw! ~% Enter Y to Restart Game!")
	(PlayAgain)
	(quit))) )


;;-------------------------------------------------------------------------
;;; Start the game and setup the board and return function Play
(defun StartGame () 
	(format t "This is Tic Tac Toe") (InitialiseBoard) (Play nil))

;;-------------------------------------------------------------------------
;;Start Game called here
(StartGame)

