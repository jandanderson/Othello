#|
 |
 |
 |
 |
 |
 |
 |
 |
 |
 |
 |#

;----------------------------------------
(load 'displayBoard)
(load 'playerMove)
(load 'gameOver)
(load 'makeMove)
(load 'userFunctions)
;-----------------------------------------

(setf *startState* '(- - - - - - - -
                     - - - - - - - -
                     - - - - - - - -
                     - - - w b - - -
                     - - - b w - - -
                     - - - - - - - -
                     - - - - - - - -
                     - - - - - - - -))
(setf *player1* nil)
(setf *player2* nil)
(setf *computer* nil)

(defun interactive ()
  (format t "Would you like to play against the computer or against someone else?~%")
  (format t "1) Computer ~%")
  (format t "2) Another Player~%")
  (format t "3) Exit~%~%")
  (format t ">>> ")
  (let (userInput difficulty)
    (setf input (read))
    (cond
      ((equal input 1)
        (againstComputer)
        (firstOrSecond 2) 
      )
      ((equal input 2)
        (format t "You have chosen to play against another player. Have fun! And may the best player win.~%~%")
        (gameLoop *startState* 'b 'w 0)
      )
      ((equal input 3)
        (format t "Good bye.~%")
        (exit)
      )
    )
  )
) 
