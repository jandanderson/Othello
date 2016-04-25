(load 'generatesuccs)
(load 'isValid)

(defun static (state)
	(let (statval curtiles curOppTiles moves tileDiff points)
        	(setf points 0)
		(cond 
			((equal (node-turn state) *computer*)
				(setf curOppTiles (get-current-tiles (node-board state) *computer*))
                                (setf curTiles (get-current-tiles (node-board state) *player1*))
;(format t "curTiles: ~S~%" curTiles)
				;(setf moves (oppMoves state curTiles))
;(format t "curOppTiles: ~S~%" curOppTiles)
				(setf moves (oppMoves state curOppTiles))
                                (setf tileDiff (- (length curTiles) (length curOppTiles)))
                                (setf points (+ (checkCorners (node-board state)  0  1  8  9 *computer*) points))
                                (setf points (+ (checkCorners (node-board state)  7  6 14 15 *computer*) points))
                                (setf points (+ (checkCorners (node-board state) 56 48 49 57 *computer*) points))
                                (setf points (+ (checkCorners (node-board state) 63 54 55 62 *computer*) points))
;(format t "moves: ~S tileDiff: ~S points: ~S~%" moves tileDiff points)
                                (setf statval (+ moves tileDiff points))
			)
			(t
				(setf curOppTiles (get-current-tiles (node-board state) *player1*))
                                (setf curTiles (get-current-tiles (node-board state) *computer*))
				(setf moves (oppMoves state curTiles))
                                (setf tileDiff (- (length curtiles) (length curOppTiles)))
                                (setf points (+ (- (checkCorners (node-board state)  0  1  8  9 *player1*)) points))
                                (setf points (+ (- (checkCorners (node-board state)  7  6 14 15 *player1*)) points))
                                (setf points (+ (- (checkCorners (node-board state) 56 48 49 57 *player1*)) points))
                                (setf points (+ (- (checkCorners (node-board state) 63 54 55 62 *player1*)) points))
                                (setf statval (+ moves tileDiff points))
			)
		)
		(return-from static statval)
	)
)

(defun checkCorners (currentBoardState Corner tile1 tile2 tile3 player)
  (let ((points 0))
    (setf points 0)
    (when (equal (nth Corner currentBoardState) player)
      (setf points (+ points 100))
      (when (equal (nth tile1 currentBoardState) player)
        (setf points (+ points 25))
      )
      (when (equal (nth tile2 currentBoardState) player)
        (setf points (+ points 25))
      )
      (when (equal (nth tile3 currentBoardState) player)
        (setf points (+ points 25))
      )
    )
    (when (not (equal (nth Corner currentBoardState) player))
      (when (equal (nth tile1 currentBoardState) player)
        (setf points (- points 1000))
      )
      (when (equal (nth tile2 currentBoardState) player)
        (setf points (- points 1000))
      )
      (when (equal (nth tile3 currentBoardState) player)
        (setf points (- points 1000))
      )
    )
;(format t "corner points: ~S~%" points)
    (return-from checkCorners points)
  )
)

(defun oppMoves (state curTiles)
  (let ((points 0)
        (moves 0)
        (newState nil))
;(format t "state in oppMoves: ~S~%" state)
    (setf newState (make-node :board (node-board state) :alpha nil :beta nil :parent nil :turn 'b))
;(format t "newState: ~S~%" newState)
    (if (equal (node-turn newState) *player1*)
      (setf moves (length (get-new-moves newState curTiles)))
      (setf moves (- (length (get-new-moves newState curTiles))))
    )
;(format t "~S~%" (get-new-moves newState curTiles))
    (when (equal moves 0)
      (setf points 200)
    )
    (when (equal moves 1)
      (setf points 100)
    )
    (when (equal moves -1)
      (setf points -100)
    )
    (when (and (> moves 1) (< moves 5))
      (setf points 50)
    )
    (when (and (< moves -1) (> moves -5))
      (setf points -50)
    )
    (when (> moves 4)
      (setf points 0)
    )
    (when (< moves -4)
      (setf points 0)
    )
    (return-from oppMoves points)
  ) 
)

(defun teststatic ()
	(setf test 	      '(- - - - - - - -
			    	- - - - - - - - 
		            	- - - b - - - - 
			    	- - - b b b - - 
		            	- - w w w b - - 
			    	- - - - - b - - 
			    	- - - - - - - - 
			    	- - - - - - - -))

        (setf test1           '(- - - - - - - -
                            	- - - - - - - -
                            	- - w b - - - -
                            	- - - w b b - -
                            	- - w w w b - -
                            	- - - - - b - -
                            	- - - - - - - -
                          	- - - - - - - -))

	(setf test2	      '(- - - - - - - -
				- - - w - - - -
				- - - w - - - -
				- - - w b b - -
				- - w w w b - -
				- - - - - b - -
				- - - - - - - -
				- - - - - - - -))

	(setf test3	      '(- - - - - - - -
				- - - - - - - -
				- - - b w - - -
				- - - w w b - -
				- - w w w b - -
				- - - - - b - -
				- - - - - - - -
				- - - - - - - -))

	(setf test4	      '(- - - - - - - -
				- - - - - - - -
				- - - b - w - -
				- - - b w b - -
				- - w w w b - -
				- - - - - b - -
				- - - - - - - -
				- - - - - - - -))

	(setf test5	      '(- - - - - - - -
				- - - - - - - -
				- - - b - - w -
				- - - b b w - -
				- - w w w b - -
				- - - - - b - -
				- - - - - - - -
				- - - - - - - -))

	(setf test6	      '(- - - - - - - -
				- - - - - - - -
				- - - b - - - -
				- - - b b b - -
				- - w w w w w -
				- - - - - b - -
				- - - - - - - -
				- - - - - - - -))

	(setf test7	      '(- - - - - - - -
				- - - - - - - -
				- - - b - - - -
				- - - b b b - -
				- - w w w b - -
				- - - - - w - -
				- - - - - - w -
				- - - - - - - -))

(setf *computer* 'w)
(setf *player1* 'b)


	(setf board1 (make-node :board test1 :alpha 0 :beta 0 :parent 0 :turn 'w) )
	(setf board2 (make-node :board test2 :alpha 0 :beta 0 :parent 0 :turn 'w) )
	(setf board3 (make-node :board test3 :alpha 0 :beta 0 :parent 0 :turn 'w) )
	(setf board4 (make-node :board test4 :alpha 0 :beta 0 :parent 0 :turn 'w) )
	(setf board5 (make-node :board test5 :alpha 0 :beta 0 :parent 0 :turn 'w) )
	(setf board6 (make-node :board test6 :alpha 0 :beta 0 :parent 0 :turn 'w) )
	(setf board7 (make-node :board test7 :alpha 0 :beta 0 :parent 0 :turn 'w) )
	(setf points (static board1))
	(format t "Board1 points: ~S~%" points)
	(setf points (static board2))
	(format t "Board2 points: ~S~%" points)
	(setf points (static board3))
	(format t "Board3 points: ~S~%" points)
	(setf points (static board4))
	(format t "Board4 points: ~S~%" points)
	(setf points (static board5))
	(format t "Board5 points: ~S~%" points)
	(setf points (static board6))
	(format t "Board6 points: ~S~%" points)
	(setf points (static board7))
	(format t "Board7 points: ~S~%" points)
)
