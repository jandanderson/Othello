;---------------------------------------------------------------------------------
; Function:  static
; Author:  Jason Anderson, Mack Smith
; Parameters:
; 		1) state - the current node being evaluated
;
; Description:  The heuristic mainly used for our static eval function was the 
; mobility heuristic.  Essentially trying to limit the opponents possible next moves.
; however this does not include the importance of corner moves.  To account for that
; we included weights for any corner spots that are available to try to urge the 
; AI to take it. 
;---------------------------------------------------------------------------------
(defun static (state)
  (let (statval curtiles curOppTiles moves tileDiff points)
        	(setf points 0)
    (cond 
      ((equal (node-turn state) *computer*)
        ;sets the curTiles and curOppTiles.  Set curTiles to opponent because the
        ;node-turn is already changed to be the opponent's turn.
        (setf curTiles (get-current-tiles (node-board state) *player1*))
        (setf curOppTiles (get-current-tiles (node-board state) *computer*))

        ; Finds the difference between the number of tiles of the two players
        (setf tileDiff (- (length curTiles) (length curOppTiles)))

        ;set the point value for the number of opponent moves
        (setf moves (oppMoves state curOppTiles))

        ; check corners
        (setf points (+ (checkCorners (node-board state)  0  1  8  9 *computer*) points))
        (setf points (+ (checkCorners (node-board state)  7  6 14 15 *computer*) points))
        (setf points (+ (checkCorners (node-board state) 56 48 49 57 *computer*) points))
        (setf points (+ (checkCorners (node-board state) 63 54 55 62 *computer*) points))
        (setf statval (+ moves tileDiff points))
      )
      (t
        ;sets the curTiles and curOppTiles.  Set curTiles to opponent because the
        ;node-turn is already changed to be the opponent's turn.
        (setf curTiles (get-current-tiles (node-board state) *computer*))
        (setf curOppTiles (get-current-tiles (node-board state) *player1*))

        ; Finds the difference between the number of tiles of the two players
        (setf tileDiff (- (length curtiles) (length curOppTiles)))

        ;set the point value for the number of opponent moves
        (setf moves (oppMoves state curTiles))

        ;check corners
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
;---------------------------------------------------------------------------------
; Function:  checkCorners
; Author:  Jason Anderson
; Parameters:
;		1) currentBoardState - the board state being evaluated
;		2) Corner - the corner being looked at
;		3) tile1, tile2, tile3 - the three tiles directly around the corner
;		4) player - which player's turn it isValid
; 
; Description:  This function assigns weights according to the tiles in and around
; the corner being looked at.  It favorably weights corners when the opponent has
; a tile in one of the three surrounding tiles, however it heavily unfavors the
; three tiles around the corner to try to discourage the AI from placing a tile 
; there. 
;---------------------------------------------------------------------------------
(defun checkCorners (currentBoardState Corner tile1 tile2 tile3 player)
  (let ((points 0))
    (setf points 0)
	; Assigns weights to the corner tile 
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
	; Checking the 3 tiles around the corner
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
    (return-from checkCorners points)
  )
)

;---------------------------------------------------------------------------------
; Function:  oppMoves
; Author:  Jason Anderson
; Parameters:
;		1) state - the current state being evaluated
;		2) curTiles - the tiles of the current player
;
; Description:  This function returns point values based on the available moves
; of the opponent.  
;---------------------------------------------------------------------------------
(defun oppMoves (state curTiles)
  (let ((points 0)
        (moves 0)
        (newState nil))
    (setf newState (make-node :board (node-board state) :alpha nil :beta nil :parent nil :turn 'b))
    (if (equal (node-turn newState) *player1*)
      (setf moves (length (get-new-moves newState curTiles)))
      (setf moves (- (length (get-new-moves newState curTiles))))
    )
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

