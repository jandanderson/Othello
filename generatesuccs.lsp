(load 'isvalid)

(defun generate-successors (state)

	(let (new_moves current_tiles)

		; get current tile locations for the opposite player
		(setf current_tiles (get-current-tiles (node-board state) (node-turn state)))
		;find possible moves for player 
		(print current_tiles)
		(cond
			;black's turn
			;get current positions of player's tiles
			((equal (node-turn state) 'b)
				(dolist (i current_tiles nil)
					;up and down
						;x, y + 1		
						(if (equal (nth (+ (* (first i) 8) (- (second i) 1)) (node-board state)) '-)
							(if (isValid (node-board state) (first i)(- (second i) 1) 'b 'w) 
									(setf new_moves (append new_moves (list  (list (first i)(- (second i) 1)))))
									(format t "~2d ~2d is not a valid move ~%" (first i) (- (second i) 1))
							)
						)
						;x, y - 1
						(if (equal (nth (+ (* (first i) 8) (+ (second i) 1)) (node-board state)) '-)
							(if (isValid (node-board state) (first i)(+ (second i) 1) 'b 'w) 
									(setf new_moves (append new_moves (list (list (first i)(+ (second i) 1)))))
									(format t "~2d ~2d is not a valid move ~%" (first i) (+ (second i) 1))
							)
						)
					;left and right
						;x + 1, y
						(if (equal (nth (+ (* (- (first i) 1) 8) (second i)) (node-board state)) '-)
							(if (isValid (node-board state) (- (first i) 1)(second i) 'b 'w) 
									(setf new_moves (append new_moves (list (list (- (first i) 1)(second i)))))
									(format t "~2d ~2d is not a valid move ~%" (- (first i) 1) (second i))
							)
						)
						;x - 1, y
						(if (equal (nth (+ (* (+ (first i) 1) 8) (second i)) (node-board state)) '-)
							(if (isValid (node-board state) (+ (first i) 1)(second i) 'b 'w) 
									(setf new_moves (append new_moves (list (list (+ (first i) 1)(second i)))))
									(format t "~2d ~2d is not a valid move ~%" (+ (first i) 1) (second i))
							)
						)
			
					;diagonals
						;x + 1, y + 1
						(if (equal (nth (+ (* (- (first i) 1) 8) (- (second i) 1)) (node-board state)) '-)
							(if (isValid (node-board state) (- (first i) 1)(- (second i) 1) 'b 'w) 
									(setf new_moves (append new_moves (list (list (- (first i) 1)(- (second i) 1)))))
									(format t "~2d ~2d is not a valid move ~%" (- (first i) 1) (- (second i) 1))
							)
						)
						;x + 1, y - 1
						(if (equal (nth (+ (* (- (first i) 1) 8) (+ (second i) 1)) (node-board state)) '-)
							(if (isValid (node-board state) (- (first i) 1)(+ (second i) 1) 'b 'w) 
									(setf new_moves (append new_moves (list (list (- (first i) 1)(+ (second i) 1)))))
									(format t "~2d ~2d is not a valid move ~%" (- (first i) 1) (+ (second i) 1))
							)
						)
						;x - 1, y + 1
						(if (equal (nth (+ (* (+ (first i) 1) 8) (- (second i) 1)) (node-board state)) '-)
							(if (isValid (node-board state) (+ (first i) 1)(- (second i) 1) 'b 'w) 
									(setf new_moves (append new_moves (list (list (+ (first i) 1)(- (second i) 1)))))
									(format t "~2d ~2d is not a valid move ~%" (+ (first i) 1) (- (second i) 1))
							)
						)
						;x - 1, y - 1
						(if (equal (nth (+ (* (+ (first i) 1) 8) (+ (second i) 1)) (node-board state)) '-)
							(if (isValid (node-board state) (+ (first i) 1)(+ (second i) 1) 'b 'w) 
									(setf new_moves (append new_moves (list (list (+ (first i) 1)(+ (second i) 1)))))
									(format t "~2d ~2d is not a valid move ~%" (+ (first i) 1) (+ (second i) 1))
							)
						)
				)
			)

			;white's turn
			(equal state-turn 'w ())
			
		)
		(setf new_moves (remove-duplicates new_moves :test #'equal))
		(print new_moves)
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
	(setf start '	  (- - - - - - - -
					   - - - - - - - -
		               - - - b - - - -
		               - - - w b w - -
		               - - - b b w - -
		               - - - - b w - -
		               - - - - - - - -
		               - - - - - - - -))
	(setf test-node (make-node :board start :numB 0 :numW 0 :parent 0 :depth 0 :turn 'b) )
	(generate-successors test-node)
)
