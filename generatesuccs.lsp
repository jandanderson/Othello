(load 'isvalid)
(load 'displayboard)
(load 'domove)

(defun deepenough (depth)
	(if (= depth 0) (return-from deepenough 't) (return-from deepenough nil))
)
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
				(when (equal (nth (+ (* row 8) col) board) temp_player) (setf tiles (append  tiles (list (list row col)))))
			)
		)
		(return-from get-current-tiles tiles)
	)
)

(defun Unique (moves)
  (if moves (cons (car moves) (Unique (vl-remove (car moves) (cdr moves)))))
	)
)

;(defstruct node board alpha beta parent depth turn)
(defun make-nodes (parent boards turn )
	(let (nodes tempnode tempturn)
		(cond
			((equal turn 'b) (setf tempturn 'w))
			((equal turn 'w) (setf tempturn 'b))
		)
		(dolist (i boards nil)
			(setf tempnode (make-node :board i  :alpha (node-alpha parent) :beta (node-beta parent) :parent parent :turn tempturn))
			(setf nodes (append nodes (list tempnode)))
		)
		(return-from makeNodes nodes)
	)
)
#|  DONT THINK WE NEED THIS FUNCTION
(defun prune (state depth)
	;(when (= depth 0)	)
	;(format t "Turn ~S at depth ~2d ~%" (node-turn state) depth)
	(let (childNodes boards val)
		(when  (deepenough depth) 
		
			;calculate static eval fun of current node
			(setf val (static state))
			;(format t "sef: ~2d ~%" val)
			(cond
				;backing up alpha value
				((equal (node-turn state) 'b)
					(if (< (node-alpha state) val) 
						(setf (node-alpha state) val)
					)
				)
				;backing up beta value
				((equal (node-turn state) 'w)
					(if (< (node-beta state) val)
						(setf (node-beta state) val)
					)
				)
			)
			(format t "alpha: ~2d  beta: ~2d ~%" (node-alpha state) (node-beta state))
		(return-from prune))
		;generate successors, and sort them based on sef value
		(setf boards (generate-successors state))
		(setf childNodes (makeNodes state boards (node-turn state)))
		(format t "~2d number of child nodes at depth ~2d ~%" (length childNodes) depth)
		; for each child node
		(dolist (i childNodes nil)
			(cond
				((equal (node-turn state) 'b)
					(when (> (node-alpha i) (node-alpha state)) (setf (node-alpha state) (node-alpha i)))
				)
				((equal (node-turn state) 'w)
					(when (< (node-beta i) (node-beta state)) (setf (node-beta state) (node-beta i)))
				)
			)
			(prune i (- depth 1))
		)
		;call prune on best with depth - 1
	)
) |#

(defun main ()
	(setf start 	  '(- - - - - - - -
						- - - - - - - - 
						- - - - - - - - 
						- - - b w - - - 
						- - - w b - - - 
						- - - - - - - - 
						- - - - - - - - 
						- - - - - - - -))
	(setf test-node (make-node :board start :alpha (- 1000000) :beta 1000000 :parent nil :turn 'b) )
	(prune test-node 4)
)
