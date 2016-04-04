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

(defun othello2p ()
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

(defun userInput ()
  (let (input)
    (setf input (string-downcase (read-line)))
    (return-from userInput input)
  )
)

(defun againstComputer ()
  (format t "You have chosen to play against the computer.  Would you like to play~%")
  (format t "Black or White? ~%")
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
      (format t "Error: Player needs to be either Black or White")
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
      (firstOrSecond)
    )
  )
)

(defun gameLoop (currentGameState player opponent difficulty)
  (display currentGameState)
  (gameOver)
  (when (equal 0 difficulty)
    (format t "It's ~S turn to play!~%" player)
    (setf currentGameState (playerMove player opponent currentGameState))
    (gameLoop currentGameState opponent player difficulty)
  )
  (when (not (equal 0 difficulty))
    (if (equal player *player1*)
      (playerMove player opponent currentGameState)
      (make-move currentGameState opponent difficulty)
    )
    (gameLoop currentGameState opponent player difficulty)
  )
)

;---------------Main function----------------
(cond
  ;test for argument
  ((= (length *ARGS*) 0)
    (othello2p)
  )
  (t
    (format t "~%---------------othello.lsp-----------------~%")
    (format t "Usage: (clisp othello2p.lsp)~%")
    (format t "Usage: (othello2p)~%")
  )
)
