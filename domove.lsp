(defun doMove (currentBoardState userRow userCol player1 player2)
  (let (moveMade posCol negCol posRow negRow)
    (setf moveMade 0)
    (setf posCol (1+ userCol))
    (setf negCol (1- userCol))
    (setf posRow (1+ userRow))
    (setf negRow (1- userRow))
    ;row and col
    (when
      ;check tile to the right of the players move
      (and (< posCol 7) (equal (nth (+ (* userRow 8) posCol) currentBoardState) player2))
        ;(format t "less than the max col AND player2 piece to the right~%")
        ;(format t "posCol ~S~%" posCol)
        (incf posCol)
        ;tile to the right is an opponents piece.  Check to see if you can capture it (or more).
        (loop
          while (< posCol 8) do
            (cond
              ((equal (nth (+ (* userRow 8) posCol) currentBoardState) player1)
                (loop
                  while (> posCol (1- userCol)) do
                    (setf (nth (+ (* userRow 8) posCol) currentBoardState) player1)
                    (decf posCol)
                )
                (setf moveMade t)
                (setf posCol 8)
              )
              (t
                (incf posCol)
              )
            )
        )
        ;reset posCol
        (setf posCol (1+ userCol))
    )
    ;check tile to the left of the players move
    (when 
      (and (> negCol 0) (equal (nth (+ (* userRow 8) negCol) currentBoardState) player2))
        ;(format t "greater than the min col AND player2 piece to the left~%")
        ;(format t "posCol ~S~%" negCol)
        (decf negCol)
        ;tile to the left is an opponents piece.  Check to see if you can capture it (or more).
        (loop
          while (> negCol 0) do
            (cond
              ((equal (nth (+ (* userRow 8) negCol) currentBoardState) player1)
                (loop
                  while (< negCol (1+ userCol)) do
                    (setf (nth (+ (* userRow 8) negCol) currentBoardState) player1)
                    (incf negCol)
                )
                (setf moveMade t)
                (setf negCol 0)
              )
              (t
                (decf negCol)
              )
            )
        )
        ;reset negCol
        (setf negCol (1- userCol))
    )
    ;check tile below the players move
    (when
      (and (< posRow 7) (equal (nth (+ (* posRow 8) userCol) currentBoardState) player2))
        ;(format t "less than the max row AND player2 piece below~%")
        ;(format t "posRow ~S~%" posRow)
        (incf posRow)
        ;tile below is an opponents piece.  Check to see if you can capture it (or more).
        (loop
          while (< posRow 8) do
            (cond
              ((equal (nth (+ (* posRow 8) userCol) currentBoardState) player1)
                (loop
                  while (> posRow (1- userRow)) do
                    (setf (nth (+ (* posRow 8) userCol) currentBoardState) player1)
                    (decf posRow)
                )
                (setf moveMade t)
                (setf posRow 8)
              )
              (t
                (incf posRow)
              )
            )
        )
        ;reset posRow
        (setf posRow (1+ userRow))
    )
     
    ;check tile above the players move
    (when
      (and (> negRow 0) (equal (nth (+ (* negRow 8) userCol) currentBoardState) player2))
        ;(format t " greater than the min row AND player2 piece above~%")
        ;(format t "negRow ~S~%" negRow)
        (decf negRow)
        ;tile abvoe is an opponents piece.  Check to see if you can capture it (or more).
        (loop
          while (> negRow 0) do
            (cond 
              ((equal (nth (+ (* negRow 8) userCol) currentBoardState) player1)
                (loop
                  while (< negRow (1+ userRow)) do
                    (setf (nth (+ (* negRow 8) userCol) currentBoardState) player1)
                    (incf negRow)
                )
                (setf moveMade t)
                (setf negRow 0)
    ;            (return)
              )
              (t
                (decf negRow)
              )
            )
        )
        ;reset negRow
        (setf negRow (1- userCol))
    )
    (when (equal moveMade t)
        (return-from doMove currentBoardState)
    )
    (when (not (equal moveMade))
    )
    ;dia

  )
)
