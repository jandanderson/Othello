(load 'generatesuccs)

(defun static (state)
	(let (statval curtiles)
		(cond 
			((equal (node-turn state) 'b)
				(setf curtiles (get-current-tiles (node-board state) 'b))
				(setf statval (length(get-new-moves state curtiles)))
			)
			((equal (node-turn state) 'w)
				(setf curtiles (get-current-tiles (node-board state) 'w))
				(setf statval (- (length (get-new-moves state curtiles))))
			)
		)
		(return-from static statval)
	)
)

(defun teststatic ()
	(setf start 	  '(- - - - - - - -
						- - - - w - - - 
						- - - b w b - - 
						- - - w w - - - 
						- - w w w b - - 
						- - - - b - - - 
						- - - - - b - - 
						- - - - - - - -))
	(setf test-node (make-node :board start :alpha 0 :beta 0 :parent 0 :turn 'w) )
	(static test-node)
)
