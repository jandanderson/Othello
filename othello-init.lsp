#|
 |This is where all of the globals/start of game variables get initialized.  
 |
 |*startState* is the starting board state for a game.
 |
 |*player1* is the main player.
 |*player2* is only used if 2 human players are playing.
 |*computer* is the color that the AI is playing.
 |
 |#

;----------------------------------------
(load 'displayboard)
(load 'domove)
(load 'gameover)
(load 'generatesuccs)
(load 'isvalid)
(load 'makemove)
(load 'minimax)
(load 'playermove)
(load 'static)
(load 'userfunctions)
;-----------------------------------------

;-----------------------------------------
; Function: othello-init
;
; Description: Required for the Othello tournament.  However we have no
; 		variables to initialize.
;-----------------------------------------
(defun othello-init ()
)

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

;-------------------------------------------------------------------------------
;Function: interactive
;
;arguments: 	none.
;
;description: 	Gets the user input on whether he or she wants to play against
;		the computer or another player.  And calls the correct functions,
;		to both initialize the game and to start it playing.
;-------------------------------------------------------------------------------

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

;-------------------------------------------------------------------------------
;Function: playerInput(player)
;
;arguments: 	player - the color the user has decided to be.
;
;description:	Calls command input after downcasing player.
;-------------------------------------------------------------------------------

(defun playerInput (player)
  (setf player (string-downcase player))
  (commandInput player)
)

;-------------------------------------------------------------------------------
;Function: commandInput(player)
;
;arguments: 	player - the color the user has decided to be.
;
;description:	Initialized the global player colors, whether that be player1
;		and player2 or player1 and computer.  Handles erroneous input.
;-------------------------------------------------------------------------------

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
