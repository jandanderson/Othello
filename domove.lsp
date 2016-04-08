(defun doMove (currentBoardState userRow userCol player1 player2)
  (let (moveMade posCol negCol posRow negRow)
    (setf moveMade 0)
    (setf posCol (1+ userCol))
    (setf negCol (1- userCol))
    (setf posRow (1+ userRow))
    (setf negRow (1- userRow))
    ;row and col
    
    ;check tile to the right of the players move
    (when
      (and (< posCol 8) (equal (nth (+ (* userRow 8) posCol) currentBoardState) player2))
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
      (and (< posRow 8) (equal (nth (+ (* posRow 8) userCol) currentBoardState) player2))
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
              )
              (t
                (decf negRow)
              )
            )
        )
        ;reset negRow
        (setf negRow (1- userRow))
    )

    ;Diagonal up right
    (when
      (and (> negRow 0) (< posCol 8) (equal (nth (+ (* negRow 8) posCol) currentBoardState) player2))
        ;tile to the right is an opponents piece.  Check to see if you can capture it (or more).
        (loop
          while (and (> negRow 0) (< posCol 8)) do
            (cond
              ((equal (nth (+ (* negRow 8) posCol) currentBoardState) player1)
                (loop
                  while (and (< negRow (1+ userRow)) (> posCol (1- userCol))) do
                    (setf (nth (+ (* negRow 8) posCol) currentBoardState) player1)
                    (incf negRow)
                    (decf posCol)
                )
                (setf moveMade t)
                (setf posCol 8)
              )
              (t
                (decf negRow)
                (incf posCol)
              )
            )
        )
        ;reset posCol
        (setf negRow (1- userRow))
        (setf posCol (1+ userCol))
    )

    ;Diagonal down right
    (when
      (and (< posRow 8) (< posCol 8) (equal (nth (+ (* posRow 8) posCol) currentBoardState) player2))
        ;tile to the right is an opponents piece.  Check to see if you can capture it (or more).
        (loop
          while (and (< posRow 8) (< posCol 8)) do
            (cond
              ((equal (nth (+ (* posRow 8) posCol) currentBoardState) player1)
                (loop
                  while (and (> posRow (1- userRow)) (> posCol (1- userCol))) do
                    (setf (nth (+ (* posRow 8) posCol) currentBoardState) player1)
                    (decf posRow)
                    (decf posCol)
                )
                (setf moveMade t)
                (setf posCol 8)
              )
              (t
                (incf posRow)
                (incf posCol)
              )
            )
        )
        ;reset posCol
        (setf posRow (1+ userRow))
        (setf posCol (1+ userCol))
    )

    ;Diagonal up left
    (when
      (and (> negRow 0) (> negCol 0) (equal (nth (+ (* negRow 8) negCol) currentBoardState) player2))
        (format t "up left~%")
        ;tile to the right is an opponents piece.  Check to see if you can capture it (or more).
        (loop
          while (and (> negRow 0) (> negCol 0)) do
            (cond
              ((equal (nth (+ (* negRow 8) negCol) currentBoardState) player1)
                (loop
                  while (and (< negRow (1+ userRow)) (< negCol (1+ userCol))) do
                    (setf (nth (+ (* negRow 8) negCol) currentBoardState) player1)
                    (incf negRow)
                    (incf negCol)
                )
                (setf moveMade t)
                (setf negCol 0)
              )
              (t
                (decf negRow)
                (decf negCol)
              )
            )
        )
        ;reset negRow and negCol
        (setf negRow (1- userRow))
        (setf negCol (1- userCol))
    )

    ;Diagonal down left
    (when
      (and (< posRow 8) (> negCol 0) (equal (nth (+ (* posRow 8) negCol) currentBoardState) player2))
        ;tile to the bottom left is an opponents piece.  Check to see if you can capture it (or more).
        (loop
          while (and (< posRow 8) (> negCol 0)) do
            (cond
              ((equal (nth (+ (* posRow 8) negCol) currentBoardState) player1)
                (loop
                  while (and (> posRow (1- userRow)) (< negCol (1+ userCol))) do
                    (setf (nth (+ (* posRow 8) negCol) currentBoardState) player1)
                    (decf posRow)
                    (incf negCol)
                )
                (setf moveMade t)
                (setf negCol 0)
              )
              (t
                (incf posRow)
                (decf negCol)
              )
            )
        )
        ;reset posRow and negCol
        (setf posRow (1+ userRow))
        (setf negCol (1- userCol))
    )

    (when (equal moveMade t)
      (return-from doMove currentBoardState)
    )
    (when (not (equal moveMade t))
      (format t "WHATS GOING ON?~%")
    )
  )
)

