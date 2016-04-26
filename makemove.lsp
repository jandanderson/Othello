;---------------------------------------------
;---------------------------------------------

(defun make-move (BoardState player ply)
  (let ((move 0) (second-board 0) (val 0)
		(firstBoard (copy-list BoardState))
        (currentBoardState nil))
    (setf currentBoardState (make-node :board BoardState :alpha (- 1000000) :beta 1000000 :parent nil :turn player))
	
    (setf move (minimax currentBoardState ply))
	(setf second-board (node-board (car (car (cdr move)))))
	(setf val (extractMove firstBoard second-board 0))
    (return-from make-move (list (floor(/ val 8)) (mod val 8)))
  )
)

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
