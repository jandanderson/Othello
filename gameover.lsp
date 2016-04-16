;----------------------------------------------
;Checks if the game is over.  
;----------------------------------------------

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
