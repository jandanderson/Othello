(load 'isvalid)

(defun generate-successors (state)

	(let (new_moves current_tiles)

		; get current tile locations for the opposite player
		(setf current_tiles (get-current-tiles state-board state-turn))
		;find possible moves for player 
		(cond
			;black's turn
			;get current positions of player's tiles
			(equal state-turn 'b 
				(dolist (i current_tiles nil)
					;up and down
						;x, y + 1
						(if (equal (nth (+ (* (first i) 8) (+ (second i) 1) state-board) '-))
							(if (isValid state (first i)(+ (second i) 1) 'b 'w) 
									(setf new_moves (append new_moves (list (list (first i)(+ (second i) )))))
									(format t "~2d ~2d is not a valid move" (first i) (+ (second i)))
							)
						)
						;x, y - 1
						;(if (equal (nth (+ (* (first i) 8) (- (second i) 1) state-board) '-))
						;	(if (isValid state (first i)(- (second i) 1) 'b 'w) (setf new_moves (append new_moves (list (list (first i)(- (second i) ))))))
						;)
					;left and right
						;x + 1, y
						;x - 1, y
			
					;diagonals
						;x + 1, y + 1
						;x + 1, y - 1
						;x - 1, y + 1
						;x - 1, y - 1
				)
			)

			;white's turn
			(equal state-turn 'w ())
		)
	)
)

; Finds all current player tiles on the board, returns the coords for each
(defun get-current-tiles (board player)
	(let (tiles temp_player)
		(if (equal player 'w)(setf temp_player 'b)(setf temp_player 'w))
		(loop for row from 0 to 8 do
			(loop for col from 0 to 8 do
				(if (equal (nth (+ (* row 8) col) board) temp_player) (setf tiles (append  tiles (list (list row col)))))
			)
		)
		(print tiles)
	)
)
(defun main ()
	(setf startState '(- - - - - - - -
					   - - - - - - - -
		               - - - - - - - -
		               - - - w b - - -
		               - - - b w - - -
		               - - - - - - - -
		               - - - - - - - -
		               - - - - - - - -))
	(setf test-node (make-node :board startState :numB 0 :numW 0 :parent 0 :depth 0 :turn 'b) )
	(generate-successors test-node)
)
