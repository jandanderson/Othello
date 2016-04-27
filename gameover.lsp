;-----------------------------------------------------------------------
;Function: gameOver
;Author: Jason Anderson
;Parameters currentBoardState player1 player2
;	1) currentBoardState - the board state to be evaluated.
;	2) player1 - the current player
;	3) player2 - the current opponent
;
;Description: Checks if the game is over. Returns 0 is the game is over.
;             Returns player1 if player1 has at least 1 valid move.
;             Returns player2 if player2 has at least 1 valid move 
;             and while player1 does not have any valid moves.
;-----------------------------------------------------------------------

(defun gameOver (currentBoardState player1 player2)
  (let ((row 0)
        (col 0)
        (flag nil))
    (setf flag nil)
    (loop
      for row from 0 to 7 do
      (loop
        for col from 0 to 7 do
        (if (isValid currentBoardState row col player1 player2)
          (return-from gameOver player1)
        )
        (if (isValid currentBoardState row col player2 player1)
          (setf flag t)
        )
      )
    )
    (if (equal flag t)
      (return-from gameOver player2)
      (return-from gameOver 0)
    )
  )
)
