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
(load 'othello2p)
;-----------------------------------------

(defun othello (player &optional firstOrSecond)
  (setf player (string-downcase player))
  (when (and (not (equal player "black")) (not (equal player "white")))
    (format t "Player needs to be either black or white.~%")
    (format t "Black or White?: ")
    (setf player (read-line))
    (setf player (string-downcase player))
    (when (and (not (equal player "black")) (not (equal player "white")))
      (error "Error: Player needs to be either Black or White!~%")
    )
  )
  (when (equal player "black")
    (setf computer 'w)
    (setf player 'b)
  )
  (when (equal player "white")
    (setf computer 'b)
    (setf player 'w)
  )
  (when (null firstOrSecond)
    ;ask user if they want to go first
    (format t "Do you want to go first?: Y/N~%")
    (format t ">>> ")
    (let ((firstOrSecond (read-line)) human)
      (setf firstOrSecond (string-downcase firstOrSecond))
      (cond 
        ((or (equal firstOrSecond "y") (equal firstOrSecond "yes"))
          (format t "OK! You will be playing ~S.  When asked for your move, please enter the row~%" player)
          (format t "and column in which you would like to place a ~S stone.  Remember, you must~%" player)
          (format t "outflank at least one ~S stone, or forfeit your move.~%~%" computer)
          (setf human 1))
        ((or (equal firstOrSecond "n") (equal firstOrSecond "no"))
          (format t "OK! You will be playing ~S.  When asked for your move, please enter the row~%" player)
          (format t "and column in which you would like to place a ~S stone.  Remember, you must~%" player)
          (format t "outflank at least one ~S stone, or forfeit your move.~%~%" computer)
          (setf human 2))
      )
    (setf startState '(- - - - - - - -
                       - - - - - - - -
                       - - - - - - - -
                       - - - w b - - -
                       - - - b w - - -
                       - - - - - - - -
                       - - - - - - - -
                       - - - - - - - -))
    (display startState)
    (playermove player computer startState)
    )
  )
)

;---------------Main function----------------
(cond
  ;test for argument
  ((= (length *ARGS*) 1)
    (othello (first *ARGS*))
  )
  ((= (length *ARGS*) 0)
    (othello2p)
  )
  (t
    (format t "~%---------------othello.lsp-----------------~%")
    (format t "Usage: (clisp othello.lsp player)~%")
    (format t "Usage: (othello [player])~%")
  )
)
