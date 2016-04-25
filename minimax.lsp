#|
                  ***** MINIMAX.LSP *****

Generalized recursive minimax routine.

Author: Dr. John M. Weiss
Class:	SDSM&T CSC447/547 Artificial Intelligence
Date: 	Spring 2016

Usage:    (minimax position depth)
          where position is the position to be evaluated,
          and depth is the search depth (number of plys).

Returns:  (value path)
          where value is the backed-up value from evaluation of leaf nodes,
          and path is the path to the desired leaf node.

Functions called:

          (deepenough depth) -
              predicate that returns T if the current position has reached
              the desired search depth, NIL otherwise.

          (move-generator position) -
              generates successors to the position.

          (static position) -
              applies the static evaluation function to the position.

          Note: these functions may need additional arguments.
|#
(load 'generatesuccs)
(load 'static)

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
		(return-from make-nodes nodes)
	)
)

(defun minimax (position depth)
    ; if we have searched deep enough, or there are no successors,
    ; return position evaluation and nil for the path
    (if (or (deepenough depth) (null (generate-successors position)))
        (list (static position) nil)

        ; otherwise, generate successors and run minimax recursively
        (let
            (
                ; generate list of sucessor positions
                (successors (generate-successors position))
                ; initialize current best path to nil
                (best-path nil)

                ; initialize current best score to negative infinity
                (best-score -1000000)
				
                ; other local variables
                succ-value
                succ-score
            )
			(setf successors (make-nodes position successors (node-turn position)))

            ; explore possible moves by looping through successor positions
            (dolist (successor successors)
;(format t "successor: ~S~%" successor)
                ; perform recursive DFS exploration of game tree
                (setq succ-value (minimax successor (1- depth)))

                ; change sign every ply to reflect alternating selection
                ; of MAX/MIN player (maximum/minimum value)
                (setq succ-score (- (car succ-value)))

				; if succ-score is negative -> MIN value
				(if (< succ-score 0) 
					; compare beta values
					(if (< succ-score (node-beta successor)) 
						; if better beta value, set it, otherwise return from this function
						(let () 
							(setf (node-beta successor) succ-score) 
							(setf best-path (cons successor (cdr succ-value)))
;(format t "successor: ~S~%" successor)
						)
						(return-from minimax)
					)
					; compare alpha values
					(if (> succ-score (node-alpha successor)) 
						; if better alpha value, set it, otherwise return from function
						(let () 
							(setf (node-alpha successor) succ-score) 
							(setf best-path (cons successor (cdr succ-value)))
						)						
						(return-from minimax)
					)
				)
            )

            ; return (value path) list when done
            (list succ-score best-path)
        )
    )
)

(defun testminimax ()
	(setf start 	  '(- - - - - - - -
						- - - - - - - - 
						- - - - - - - - 
						- - - b w - - - 
						- - - w b - - - 
						- - - - - - - - 
						- - - - - - - - 
						- - - - - - - -))

	(setf test    '(- - - - - - - -
			- - - - - - - -
			- - - b - - - -
			- - - b b b - -
			- - w w w b - -
			- - - - - b - -
			- - - - - - - -
			- - - - - - - -))

	(setf *computer* 'b)
	(setf *player1* 'w)
	(setf test-node (make-node :board test :alpha (- 1000000) :beta 1000000 :parent nil :turn 'w) )
	(setf path (cdr (minimax test-node 4)))
	(print "-----------------------------------------------------")
	(display (node-board (car (car  path))))
	(format t "alpha value: ~2d ~%beta value: ~2d" (node-alpha (car (car path)))(node-beta (car(car path))))
)
