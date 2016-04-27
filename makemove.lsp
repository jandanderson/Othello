
;---------------------------------------------------------------------------------
; Function: make-move
; Author:  Jason Anderson, Mack Smith
; Parameters:
;		1) BoardState - the board currently being evaluated
;		2) player - the player making the move
; 		3) ply - the search depth
;
; Description:  This function is the required make-move function that returns a 
; move (row col) based on the current board state and our minimax/heuristic.
;---------------------------------------------------------------------------------
(defun make-move (BoardState player ply)
  (let ((move 0) (second-board 0) (val 0)
		(firstBoard (copy-list BoardState))
        (currentBoardState nil))
    (setf currentBoardState (make-node :board BoardState :alpha (- 1000000) :beta 1000000 :parent nil :turn player))
	
    (setf move (minimax currentBoardState ply))
    (setf second-board (node-board (car (car (cdr move)))))
	  (setf val (extractMove firstBoard second-board 0))
	
	  ; After the move is returned from extractMove, it needs to be converted to 
	  ; (row col) and then adds 1 to each because the list is not 0 based.
    (return-from make-move (list (floor(/ val 8)) (mod val 8)))
  )
)
;---------------------------------------------------------------------------------
; Function:  extractMove
; Author:  Mack Smith
; Parameters:
;		1) board - the current board state
;		2) next-board - the board state after minimax is called
;		3) position - the position to be returned 
;
; Description:  This function recursively compares the cars of the two boards
; looking for when the first board is a '-' and the second board is not.  It then
; returns the position of the difference in the board list.
;---------------------------------------------------------------------------------
(defun extractMove (board next-board position)
	(let ()
		(if (and (equal (car board) '-) (not (equal (car next-board) '-))) 
			(return-from extractMove position)
			(extractMove (cdr board) (cdr next-board) (1+ position))
		)
	)
)

(defun testExtractMove ()
	(let (start test)
		(setf start   '(- - - - - - - -
						- - - - - - - - 
						- - - - - - - - 
						- - - b w - - - 
						- - - w b - - - 
						- - - - - - - - 
						- - - - - - - - 
						- - - - - - - -))
		(setf test    '(- - - - - - - -
						- - - - - - - -
						- - - - - - - -
						- - - b w - - -
						- - - w b - - -
						- - - - - - - -
						- - - - - - - -
						- - - - - - - -))
		(setf *computer* 'b)
		(setf *player1* 'w)
		(make-move start 'b 2)
	)
)
