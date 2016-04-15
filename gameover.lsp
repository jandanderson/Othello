;----------------------------------------------
;Checks if the game is over.  
;----------------------------------------------

(defun gameOver (node player1 player2)
(format t "LSKDF HS JGS~%")
  (loop
    for row from 0 to 8 do
    (loop
      for col from 0 to 8 do
(format t "check check~%")
      (if (isValid node row col player1 player2)
        (return-from gameOver player1)
      )
    )
  )
  (loop
    for row from 0 to 8 do
    (loop
      for col from 0 to 8 do
      (if (isValid node row col player2 player1)
        (return-from gameOver player2)
      )
    )
  )
  (return-from gameOver nil)
)
