#|
 | This file contains the main function for the othello program.
 |
 | If the user puts a player in the command line then the program will assume
 | that the user wants to play against the computer.  If the user did not
 | input a player then it will prompt the user to see if they want to play
 | against the computer or another user. |
 |
 |#

;----------------------------------------
(load 'othello-init)
;-----------------------------------------

;---------------Main function----------------
(cond
  ;test for argument
  ((= (length *ARGS*) 1)
    (playerInput (first *ARGS*))
    (firstOrSecond 6)
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
