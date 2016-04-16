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
(load 'userFunctions)
;-----------------------------------------

(defun playerInput (player)
  (setf player (string-downcase player))
  (commandInput player)
  (firstOrSecond 2)
)

(defun commandInput (player)
  (cond
    ((or (equal player "black") (equal player "b"))
      (setf *player1* 'b)
      (setf *computer* 'w)
      (return-from commandInput)
    )
    ((or (equal player "white") (equal player "w"))
      (setf *player1* 'w)
      (setf *computer* 'b)
      (return-from commandInput)
    )
    (t
      (format t "Error: Player needs to be either Black or White~%")
      (againstComputer)
    )
  )
)

;---------------Main function----------------
(cond
  ;test for argument
  ((= (length *ARGS*) 1)
    (playerInput (first *ARGS*))
  )
  ((= (length *ARGS*) 0)
    (interactive)
  )
  (t
    (format t "~%---------------othello.lsp-----------------~%")
    (format t "Usage: (clisp othello.lsp player)~%")
    (format t "Usage: (othello [player])~%")
  )
)
