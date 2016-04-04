;---------------------------------------------------------
(load 'isValid)
;---------------------------------------------------------

(defun playermove (player1 player2 BoardState)
  (format t "What is your move [row col]? ")
  (let (userInput userRow userCol currentBoardState)
    (setf userRow (read))
    (setf userCol (read))
    (setf userRow (1- userRow))
    (setf userCol (1- userCol))
    (setf currentBoardState (copy-list BoardState))
(format t "userRow: ~S~%" userRow)
(format t "userCol: ~S~%" userCol)
    (setf (nth (+ (* userRow 8) userCol) currentBoardState) 'h)
    (cond
      ((isValid currentBoardState userRow userCol player1 player2)
        (doMove currentBoardState userRow userCol player1 player2)
        (display currentBoardState)
      )
      (t
        (format t "Invalid move.  You must place your piece so that you take at least 1 of your opponents.~%")
        (display BoardState)
        (playermove player1 player2 BoardState)
      )
    )
  )
)