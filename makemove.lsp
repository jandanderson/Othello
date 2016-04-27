
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
    (if (null (setf cornerMove (cornersAvailable BoardState player)))
      (let ()
        (setf currentBoardState (make-node :board BoardState :alpha (- 1000000) :beta 1000000 :parent nil :turn player))

        (setf move (minimax currentBoardState ply))
        (setf second-board (node-board (car (car (cdr move)))))
        (setf val (extractMove firstBoard second-board 0))

        ; After the move is returned from extractMove, it needs to be converted to 
        ; (row col) and then adds 1 to each because the list is not 0 based.
        (return-from make-move (list (floor(/ val 8)) (mod val 8)))
      )
      (return-from make-move cornerMove)
    )
  )
)

;---------------------------------------------------------------------------------
; Function: CornersAvailable
; Author:  Jason Anderson
; Parameters:
;		1) BoardState - the board currently being evaluated
;		2) player - the player making the move
;
; Description:  Checks all of the corners to see if it is possible to take it.
;               Returns the Row and Col of the corner available to take or else nil.
;---------------------------------------------------------------------------------
(defun cornersAvailable (BoardState player)
  (let (p1 p2)
    (if (equal player 'b)
      (let ()
        (setf p1 'b)
        (setf p2 'w)
      )
      (let ()
        (setf p1 'w)
        (setf p2 'b)
      )
    )
    (cond
      ((isValid BoardState 0 0 p1 p2)
        (return-from cornersAvailable (list 0 0))
      )
      ((isValid BoardState 0 7 p1 p2)
        (return-from cornersAvailable (list 0 7))
      )
      ((isValid BoardState 7 0 p1 p2)
        (return-from cornersAvailable (list 7 0))
      )
      ((isValid BoardState 7 7 p1 p2)
        (return-from cornersAvailable (list 7 7))
      )
      (t
        (return-from cornersAvailable nil)
      )
    )
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
