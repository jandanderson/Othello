(load 'isvalid)
(load 'displayboard)
(load 'domove)

(defun generate-successors (state)

	(let (new_moves current_tiles cur_board new_boards)
		; set the current board to the parent board
		(setf cur_board (copy-list (node-board state)))
		; get current tile locations for the opposite player
		(setf current_tiles (get-current-tiles (node-board state) (node-turn state)))
		;find possible moves for player 
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
									;(format t "~2d ~2d is not a valid move ~%" (first i) (- (second i) 1))
							)
						)
						;x, y - 1
						(if (equal (nth (+ (* (first i) 8) (+ (second i) 1)) (node-board state)) '-)
							(if (isValid (node-board state) (first i)(+ (second i) 1) 'b 'w) 
									(setf new_moves (append new_moves (list (list (first i)(+ (second i) 1)))))
									;(format t "~2d ~2d is not a valid move ~%" (first i) (+ (second i) 1))
							)
						)
					;left and right
						;x + 1, y
						(if (equal (nth (+ (* (- (first i) 1) 8) (second i)) (node-board state)) '-)
							(if (isValid (node-board state) (- (first i) 1)(second i) 'b 'w) 
									(setf new_moves (append new_moves (list (list (- (first i) 1)(second i)))))
									;(format t "~2d ~2d is not a valid move ~%" (- (first i) 1) (second i))
							)
						)
						;x - 1, y
						(if (equal (nth (+ (* (+ (first i) 1) 8) (second i)) (node-board state)) '-)
							(if (isValid (node-board state) (+ (first i) 1)(second i) 'b 'w) 
									(setf new_moves (append new_moves (list (list (+ (first i) 1)(second i)))))
									;(format t "~2d ~2d is not a valid move ~%" (+ (first i) 1) (second i))
							)
						)
			
					;diagonals
						;x + 1, y + 1
						(if (equal (nth (+ (* (- (first i) 1) 8) (- (second i) 1)) (node-board state)) '-)
							(if (isValid (node-board state) (- (first i) 1)(- (second i) 1) 'b 'w) 
									(setf new_moves (append new_moves (list (list (- (first i) 1)(- (second i) 1)))))
									;(format t "~2d ~2d is not a valid move ~%" (- (first i) 1) (- (second i) 1))
							)
						)
						;x + 1, y - 1
						(if (equal (nth (+ (* (- (first i) 1) 8) (+ (second i) 1)) (node-board state)) '-)
							(if (isValid (node-board state) (- (first i) 1)(+ (second i) 1) 'b 'w) 
									(setf new_moves (append new_moves (list (list (- (first i) 1)(+ (second i) 1)))))
									;(format t "~2d ~2d is not a valid move ~%" (- (first i) 1) (+ (second i) 1))
							)
						)
						;x - 1, y + 1
						(if (equal (nth (+ (* (+ (first i) 1) 8) (- (second i) 1)) (node-board state)) '-)
							(if (isValid (node-board state) (+ (first i) 1)(- (second i) 1) 'b 'w) 
									(setf new_moves (append new_moves (list (list (+ (first i) 1)(- (second i) 1)))))
									;(format t "~2d ~2d is not a valid move ~%" (+ (first i) 1) (- (second i) 1))
							)
						)
						;x - 1, y - 1
						(if (equal (nth (+ (* (+ (first i) 1) 8) (+ (second i) 1)) (node-board state)) '-)
							(if (isValid (node-board state) (+ (first i) 1)(+ (second i) 1) 'b 'w) 
									(setf new_moves (append new_moves (list (list (+ (first i) 1)(+ (second i) 1)))))
									;(format t "~2d ~2d is not a valid move ~%" (+ (first i) 1) (+ (second i) 1))
							)
						)
				)
			)

			;white's turn
			( (equal (node-turn state) 'w)
				(dolist (i current_tiles nil)
				;up and down
						;x, y + 1		
						(if (equal (nth (+ (* (first i) 8) (- (second i) 1)) (node-board state)) '-)
							(if (isValid (node-board state) (first i)(- (second i) 1) 'w 'b) 
									(setf new_moves (append new_moves (list  (list (first i)(- (second i) 1)))))
									;(format t "~2d ~2d is not a valid move ~%" (first i) (- (second i) 1))
							)
						)
						;x, y - 1
						(if (equal (nth (+ (* (first i) 8) (+ (second i) 1)) (node-board state)) '-)
							(if (isValid (node-board state) (first i)(+ (second i) 1) 'w 'b) 
									(setf new_moves (append new_moves (list (list (first i)(+ (second i) 1)))))
									;(format t "~2d ~2d is not a valid move ~%" (first i) (+ (second i) 1))
							)
						)
					;left and right
						;x + 1, y
						(if (equal (nth (+ (* (- (first i) 1) 8) (second i)) (node-board state)) '-)
							(if (isValid (node-board state) (- (first i) 1)(second i) 'w 'b) 
									(setf new_moves (append new_moves (list (list (- (first i) 1)(second i)))))
									;(format t "~2d ~2d is not a valid move ~%" (- (first i) 1) (second i))
							)
						)
						;x - 1, y
						(if (equal (nth (+ (* (+ (first i) 1) 8) (second i)) (node-board state)) '-)
							(if (isValid (node-board state) (+ (first i) 1)(second i) 'w 'b) 
									(setf new_moves (append new_moves (list (list (+ (first i) 1)(second i)))))
									;(format t "~2d ~2d is not a valid move ~%" (+ (first i) 1) (second i))
							)
						)
			
					;diagonals
						;x + 1, y + 1
						(if (equal (nth (+ (* (- (first i) 1) 8) (- (second i) 1)) (node-board state)) '-)
							(if (isValid (node-board state) (- (first i) 1)(- (second i) 1) 'w 'b) 
									(setf new_moves (append new_moves (list (list (- (first i) 1)(- (second i) 1)))))
									;(format t "~2d ~2d is not a valid move ~%" (- (first i) 1) (- (second i) 1))
							)
						)
						;x + 1, y - 1
						(if (equal (nth (+ (* (- (first i) 1) 8) (+ (second i) 1)) (node-board state)) '-)
							(if (isValid (node-board state) (- (first i) 1)(+ (second i) 1) 'w 'b) 
									(setf new_moves (append new_moves (list (list (- (first i) 1)(+ (second i) 1)))))
									;(format t "~2d ~2d is not a valid move ~%" (- (first i) 1) (+ (second i) 1))
							)
						)
						;x - 1, y + 1
						(if (equal (nth (+ (* (+ (first i) 1) 8) (- (second i) 1)) (node-board state)) '-)
							(if (isValid (node-board state) (+ (first i) 1)(- (second i) 1) 'w 'b) 
									(setf new_moves (append new_moves (list (list (+ (first i) 1)(- (second i) 1)))))
									;(format t "~2d ~2d is not a valid move ~%" (+ (first i) 1) (- (second i) 1))
							)
						)
						;x - 1, y - 1
						(if (equal (nth (+ (* (+ (first i) 1) 8) (+ (second i) 1)) (node-board state)) '-)
							(if (isValid (node-board state) (+ (first i) 1)(+ (second i) 1) 'w 'b) 
									(setf new_moves (append new_moves (list (list (+ (first i) 1)(+ (second i) 1)))))
									;(format t "~2d ~2d is not a valid move ~%" (+ (first i) 1) (+ (second i) 1))
							)
						)
				)
			)
			
		)
		(setf new_moves (remove-duplicates new_moves :test #'equal))
		
		; generate new board states from possible moves
		(dolist (i new_moves nil ) 
			;if black's turn
			(when (equal (node-turn state) 'b)
				(setf cur_board (copy-list (node-board state)))
				(setf cur_board (domove cur_board (first i) (second i) 'b 'w))
				(setf new_boards (append new_boards (list cur_board)))
			)	
			;if white's turn
			(when (equal (node-turn state) 'w)
				(setf cur_board (copy-list (node-board state)))
				(setf cur_board (domove cur_board (first i) (second i) 'w 'b))
				(setf new_boards (append new_boards (list cur_board)))
			)	
		)
		(dolist (i new_boards nil) 
		(display i)
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
	(setf start 	  '(- - - - - - - -
					   - - - - - - - -
		               - - - b - - - -
		               - - - w b w - -
		               - - - b b w - -
		               - - - - b w - -
		               - - - - - - - -
		               - - - - - - - -))
	(setf test-node (make-node :board start :numB 0 :numW 0 :parent 0 :depth 0 :turn 'w) )
	(generate-successors test-node)
)
