;-------------------------------------------------------------------------------
;Function: playermove(player1 player2 BoardState)
;
;Author: Jason Anderson
;
;arguments: 	player1 - the player who made the move.
;		player2 - the current opponent.
;		currentBoardState - the board state passed in be played on.
;
;Returns:	(updated) currentBoardState - the state of the game after
;				the latest move has been made.
;-------------------------------------------------------------------------------

(defun playermove (player1 player2 BoardState)
  (format t "It's ~S's turn!~%" player1)
  (format t "What is your move [row col]? ")
  (let (userInput userRow userCol currentBoardState)
    (setf userRow (read))
    (setf userCol (read))
    (when (or (not (> userRow 0)) (not (< userRow 9)))
      (format t "Invalid move.  Move not on board.~%")
      (playermove player1 player2 BoardState)
    )
    (when (or (not (> userCol 0)) (not (< userCol 9)))
      (format t "Invalid move.  Move not on board.~%")
      (playermove player1 player2 BoardState)
    )
    (setf userRow (1- userRow))
    (setf userCol (1- userCol))
    (setf currentBoardState (copy-list BoardState))
    (cond
      ((isValid currentBoardState userRow userCol player1 player2)
        (setf currentBoardState (doMove currentBoardState userRow userCol player1 player2))
        (return-from playermove currentBoardState)
      )
      (t
        (format t "Invalid move.  You must place your piece so that you take at least 1 of your opponents.~%")
        (display BoardState)
        (playermove player1 player2 BoardState)
      )
    )
  )
)
