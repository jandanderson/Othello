(load 'isvalid)
(load 'displayboard)
(load 'domove)
;---------------------------------------------------------------------------------
; Function: deepenough
; Author: Mack Smith
; Parameters:
;		1) depth - the depth of the current search
;
; Description:  Returns true if depth = 0, this function is called at the begining
; of the minimax function, and each iteration of minimax we subtract 1 from depth.
;---------------------------------------------------------------------------------
(defun deepenough (depth)
	(if (= depth 0) (return-from deepenough 't) (return-from deepenough nil))
)

;---------------------------------------------------------------------------------
; Function:  get-new-moves
; Author:  Mack Smith
; Parameters:
;		1) state - the current node being processed
;		2) current_tiles - the list of tiles the opponent currently has
;
; Description:  This function takes the current tiles of the opponent and checks
; every space around those tiles.  If any are empty, then I check to see if putting
; a piece there would be a valid move, if it is, I add the move (row col) to a list
; which is then returned.
;---------------------------------------------------------------------------------
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

;---------------------------------------------------------------------------------
; Function: generate-successors
; Author:  Mack Smith
; Parameters:	
;		1) state - the current node being processed
;
; Description:  This is the function that generates child board states.  There is 
; another function which translates those into nodes (i dont know why i didnt do 
; that here).  This function takes the current board state and finds all possible
; moves that the current player can make.  It then generates a new board for each 
; of those moves and returns the list of the boards.
;---------------------------------------------------------------------------------
(defun generate-successors (state)
	(let (new_moves current_tiles cur_board new_boards)
		; set the current board to the parent board
		(setf cur_board (copy-list (node-board state)))
		; get current tile locations for the opposite player
		(setf current_tiles (get-current-tiles (node-board state) (node-turn state)))
		;(print current_tiles)
		;find possible moves for player 
		(setf new_moves (get-new-moves state current_tiles))
		(setf new_moves (remove-duplicates new_moves :test #'equal))
		;(print new_moves)
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
		;(display (car new_boards))
		(return-from generate-successors new_boards)
	)
)

;---------------------------------------------------------------------------------
; Function:  valid_move
; Author:  Mack Smith
; Parameters:
;		1) row - the row entry of the move being checked
;		2) col - the column entry of the move being checked
;
; Description:  Since our board states are stored as a single list (no sublists),
; we need to make sure that any moves remain within the bounds of the board.  This
; function makes sure there are no row or column values below 0 or above 7.
;---------------------------------------------------------------------------------
(defun valid_move (row col)
	(cond
		((< row 0) (return-from valid_move 0))
		((> row 7) (return-from valid_move 0))
		((< col 0) (return-from valid_move 0))
		((> col 7) (return-from valid_move 0))
		(t (return-from valid_move 1))
	)
)

;---------------------------------------------------------------------------------
; Function: get-current-tiles
; Author:  Mack Smith
; Parameters: 
;		1) board - the current board being processed
;		2) player - black or white's turn
;
; Description:  This function looks at the current board state and finds all of the
; positions that the opponent currently has.  For instance the first time this is 
; called, if player = 'b then this will return the positions of the white tiles,
; ((3 3)(4 4)) since our lists are 0 based.
;---------------------------------------------------------------------------------
(defun get-current-tiles (board player)
	(let (tiles temp_player)
		(cond 
			((equal player 'w)(setf temp_player 'b))
			((equal player 'b)(setf temp_player 'w))
		)
		(loop for row from 0 to 8 do
			(loop for col from 0 to 8 do
				(when (equal (nth (+ (* row 8) col) board) temp_player) (setf tiles (append  tiles (list (list row col)))))
			)
		)
		(return-from get-current-tiles tiles)
	)
)

