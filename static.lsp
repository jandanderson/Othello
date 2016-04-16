(load 'generatesuccs)

(defun static (state)
	(let (statval curtiles)
		(cond 
			((equal (node-turn state) 'b)
				(setf curtiles (get-current-tiles (node-board state) 'b))
				(setf statval (length(get-new-moves state curtiles)))
			)
			((equal (node-turn state) 'w)
				(setf curtiles (get-current-tiles (node-board state) 'b))
				(setf statval (- (length (get-new-moves state curtiles))))
			)
		)
		(return-from static statval)
	)
)

(defun teststatic ()
	(setf start 	  '(- - - - - - - -
						- - - - - - - - 
						- - - - - - - - 
						- - - b w - - - 
						- - - w b - - - 
						- - - - - - - - 
						- - - - - - - - 
						- - - - - - - -))
	(setf test-node (make-node :board start :numB 0 :numW 0 :parent 0 :depth 0 :turn 'b) )
	(static test-node)
)
