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
    (setf (nth (+ (* userRow 8) userCol) currentBoardState) 'h)
    (cond
      ((isValid currentBoardState userRow userCol player1 player2)
        (setf node-currentBoardState (doMove currentBoardState userRow userCol player1 player2))
        (return-from playermove node-currentBoardState)
      )
      (t
        (format t "Invalid move.  You must place your piece so that you take at least 1 of your opponents.~%")
        (display BoardState)
        (playermove player1 player2 BoardState)
      )
    )
  )
)
