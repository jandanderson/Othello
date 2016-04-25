(defun userInput ()
  (let (input)
    (setf input (string-downcase (read-line)))
    (return-from userInput input)
  )
)

(defun againstComputer ()
  (format t "You have chosen to play against the computer.  Would you like to play~%")
  (format t "Black or White? ~%")
  (format t ">>> ")
  (setf input (userInput))
  (cond
    ((or (equal input "black") (equal input "b"))
      (setf *player1* 'b)
      (setf *computer* 'w)
      (return-from againstComputer)
    )
    ((or (equal input "white") (equal input "w"))
      (setf *player1* 'w)
      (setf *computer* 'b)
      (return-from againstComputer)
    )
    ((equal input "exit")
      (format t "Good bye.~%")
      (exit)
    )
    (t
      (format t "Error: Player needs to be either Black or White~%")
      (againstComputer)
    )
  )
)

(defun firstOrSecond (difficulty)
  (format t "Do you want to go first?: Y/N~%")
  (format t ">>> ")
  (setf input (userInput))
  (cond
    ((or (equal input "y") (equal input "yes"))
      (format t "OK! You will be playing ~S.  When asked for your move, please enter the row~%" *player1*)
      (format t "and column in which you would like to place a ~S stone.  Remember, you must~%" *player1*)
      (format t "outflank at least one ~S stone, or forfeit your move.~%~%" *computer*)
      (gameLoop *startState* *player1* *computer* difficulty)
    )
    ((or (equal input "n") (equal input "no"))
      (format t "OK! You will be playing ~S.  When asked for your move, please enter the row~%" *player1*)
      (format t "and column in which you would like to place a ~S stone.  Remember, you must~%" *player1*)
      (format t "outflank at least one ~S stone, or forfeit your move.~%~%" *computer*)
      (gameLoop *startState* *computer* *player1* difficulty)
    )
    ((equal input "exit")
      (format t "Good bye.~%")
      (exit)
    )
    (t
      (format t "You must type Y/N or Yes/No to choose whether or not you go first.~%")
      (firstOrSecond difficulty)
    )
  )
)

(defun winner (currentGameState)
  (let ((black 0)
        (white 0))
    (setf black 0)
    (setf white 0)
    (loop
      for row from 0 to 7 do
      (loop
        for col from 0 to 7 do
          (when (equal (nth (+ (* row 8) col) currentGameState) 'b)
            (incf black)
          )
          (when (equal (nth (+ (* row 8) col) currentGameState) 'w)
            (incf white)
          )
      )
    )
    (if (equal black white)
      (return-from winner (list 't black white))
    )
    (if (> black white)
      (return-from winner (list 'b black white))
      (return-from winner (list 'w black white))
    )
  )
)

(defun gameLoop (currentGameState player opponent difficulty)
  (display currentGameState)
  (when (equal (gameOver currentGameState player opponent) 0)
    (format t "Game Over!~%")
    (setf gameDone (winner currentGameState))
;(format t "gameDone: ~S~%" gameDone)
    (when (equal (car gameDone) t)
      (format t "Wow! You tied! Game well played!~%")
      (format t "You each had ~S tiles.~%" (cadr gameDone))
      (exit)
    )
    (format t "~S wins! Congrats!~%" (car gameDone))
    (format t "Black had ~S tiles.~%" (cadr gameDone))
    (format t "White had ~S tiles.~%" (caddr gameDone))
    (exit)
  )
  (when (equal (gameOver currentGameState player opponent) opponent)
    (format t "~S has no valid moves! ~S's turn!~%" player opponent)
    (when (equal 0 difficulty)
      (setf currentGameState (playerMove opponent player currentGameState))
      (gameLoop currentGameState player opponent difficulty)
    )
    (when (not (equal 0 difficulty))
      (if (equal opponent *player1*)
        (setf currentGameState (playerMove opponent player currentGameState))
        (setf currentGameState (make-move currentGameState opponent difficulty))
      )
      (gameLoop currentGameState player opponent difficulty)
    )
  )
  (when (equal 0 difficulty)
    (setf currentGameState (playerMove player opponent currentGameState))
    (gameLoop currentGameState opponent player difficulty)
  )
  (when (not (equal 0 difficulty))
    (if (equal player *player1*)
      (setf currentGameState (playerMove player opponent currentGameState))
      (setf currentGameState (make-move currentGameState player difficulty))
    )
    (gameLoop currentGameState opponent player difficulty)
  )
)
