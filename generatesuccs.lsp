(load 'isvalid)
(load 'displayboard)
(load 'domove)


(defun get-new-moves (state current_tiles)

	(let (new_moves)
	;black's turn
		(cond 
			((equal (node-turn state) 'b)
				(loop
          for row from 0 to 7 do
          (loop
            for col from 0 to 7 do
              (if (isValid (node-board state) row col 'b 'w) 
							  (setf new_moves (append new_moves (list  (list row col))))
						  )
          )
        )
			)

			;white's turn
			((equal (node-turn state) 'w)
      	(loop
          for row from 0 to 7 do
            (loop
              for col from 0 to 7 do
                (if (isValid (node-board state) row col 'w 'b) 
							    (setf new_moves (append new_moves (list  (list row col))))
						    )
            )
        )
      )
		)
		(return-from get-new-moves new_moves)
	)
)
(defun generate-successors (state)

	(let (new_moves current_tiles cur_board new_boards)
		; set the current board to the parent board
		(setf cur_board (copy-list (node-board state)))
		; get current tile locations for the opposite player
		(setf current_tiles (get-current-tiles (node-board state) (node-turn state)))
		;find possible moves for player 
		(setf new_moves (get-new-moves state current_tiles))
		(setf new_moves (remove-duplicates new_moves :test #'equal))
		
		; generate new board states from possible moves
		(dolist (i new_moves nil ) 
			(when (and (>= (+ (* (first i) 8) (- (second i) 1)) 0) (<= (+ (* (first i) 8) (- (second i) 1)) 63) )
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
		)
		(dolist (i new_boards nil) 
		(display i)
		)
	
	)
)
(defun valid_move (row col)
	(cond
		((< row 0) (return-from valid_move 0))
		((> row 7) (return-from valid_move 0))
		((< col 0) (return-from valid_move 0))
		((> col 7) (return-from valid_move 0))
		(t (return-from valid_move 1))
	)
)

; Finds all current player tiles on the board, returns the coords for each
(defun get-current-tiles (board player)
	(let (tiles temp_player)
		(cond 
			((equal player 'w)(setf temp_player 'b))
			((equal player 'b)(setf temp_player 'w))
		)
		(loop for row from 0 to 8 do
			(loop for col from 0 to 8 do
				(if (equal (nth (+ (* row 8) col) board) temp_player) (setf tiles (append  tiles (list (list row col)))))
			)
		)
		(return-from get-current-tiles tiles)
	)
)

(defun Unique (moves)
  (if moves (cons (car moves) (Unique (vl-remove (car moves) (cdr moves)))))
)

(defun main ()
	(setf start 	  '(- - - - - - - -
						- - - - - - - - 
						- - - - - - - - 
						- - - b w - - - 
						- - - w b - - - 
						- - - - - - - - 
						- - - - - - - - 
						- - - - - - - -))
	(setf test-node (make-node :board start :numB 0 :numW 0 :parent 0 :depth 0 :turn 'b) )
	(generate-successors test-node)
)
