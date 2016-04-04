;-----------------------------------------------------------------
(load 'doMove)
;-----------------------------------------------------------------

(defun isValid (currentBoardState userRow userCol player1 player2)
  (let (adjacent posCol negCol posRow negRow)
    (setf adjacent nil)
    (setf posCol (1+ userCol))
    (setf negCol (1- userCol))
    (setf posRow (1+ userRow))
    (setf negRow (1- userRow))
    ;row and col
    (cond
      ;check tile to the right of the players move
      ((and (< posCol 8) (equal (nth (+ (* userRow 8) posCol) currentBoardState) player2))
        ;(format t "less than the max col AND player2 piece to the right~%")
        (incf posCol)
        ;tile to the right is an opponents piece.  Check to see if you can capture it (or more).
        (loop
          while (< posCol 8) do
            (if (equal (nth (+ (* userRow 8) posCol) currentBoardState) player1)
              (return-from isValid t)
              (incf posCol)
            )
        )
        ;reset posCol
        (setf posCol (1+ userCol))
      )
      ;check tile to the left of the players move
      ((and (> negCol -1) (equal (nth (+ (* userRow 8) negCol) currentBoardState) player2))
        ;(format t "greater than the min col AND player2 piece to the left~%")
        (decf negCol)
        ;tile to the left is an opponents piece.  Check to see if you can capture it (or more).
        (loop
          while (> negCol -1) do
            (if (equal (nth (+ (* userRow 8) negCol) currentBoardState) player1)
              (return-from isValid t)
              (decf negCol)
            )
        )
        ;reset negCol
        (setf negCol (1- userCol))
      )
      ;check tile below the players move
      ((and (< posRow 8) (equal (nth (+ (* posRow 8) userCol) currentBoardState) player2))
        ;(format t "less than the max row AND player2 piece below~%")
        (incf posRow)
        ;tile below is an opponents piece.  Check to see if you can capture it (or more).
        (loop
          while (< posRow 8) do
            (if (equal (nth (+ (* posRow 8) userCol) currentBoardState) player1)
              (return-from isValid t)
              (decf negCol)
            )
        )
        ;reset posRow
        (setf posRow (1+ userRow))
      )
      ;check tile above the players move
      ((and (> negRow 0) (equal (nth (+ (* negRow 8) userCol) currentBoardState) player2))
        ;(format t " greater than the min row AND player2 piece above~%")
        (decf negRow)
        ;tile abvoe is an opponents piece.  Check to see if you can capture it (or more).
        (loop
          while (> negRow 0) do
            (if (equal (nth (+ (* negRow 8) userCol) currentBoardState) player1)
              (return-from isValid t)
              (decf negCol)
            )
        )
        ;reset negRow
        (setf negRow (1- userCol))
      )
;diagonalssssssssss
      ((and (< posCol 8) (equal (nth (+ (* userRow 8) posCol) currentBoardState) player2))
        ;(format t "less than the max col AND player2 piece to the right~%")
        (incf posCol)
        ;tile to the right is an opponents piece.  Check to see if you can capture it (or more).
        (loop
          while (< posCol 8) do
            (if (equal (nth (+ (* userRow 8) posCol) currentBoardState) player1)
              (return-from isValid t)
              (incf posCol)
            )
        )
        ;reset posCol
        (setf posCol (1+ userCol))
      )
;diagonalssssssssss
      ((and (< posCol 8) (equal (nth (+ (* userRow 8) posCol) currentBoardState) player2))
        ;(format t "less than the max col AND player2 piece to the right~%")
        (incf posCol)
        ;tile to the right is an opponents piece.  Check to see if you can capture it (or more).
        (loop
          while (< posCol 8) do
            (if (equal (nth (+ (* userRow 8) posCol) currentBoardState) player1)
              (return-from isValid t)
              (incf posCol)
            )
        )
        ;reset posCol
        (setf posCol (1+ userCol))
      )
;diagonalssssssssss
      ((and (< posCol 8) (equal (nth (+ (* userRow 8) posCol) currentBoardState) player2))
        ;(format t "less than the max col AND player2 piece to the right~%")
        (incf posCol)
        ;tile to the right is an opponents piece.  Check to see if you can capture it (or more).
        (loop
          while (< posCol 8) do
            (if (equal (nth (+ (* userRow 8) posCol) currentBoardState) player1)
              (return-from isValid t)
              (incf posCol)
            )
        )
        ;reset posCol
        (setf posCol (1+ userCol))
      )
;diagonalssssssssss
      ((and (< posCol 8) (equal (nth (+ (* userRow 8) posCol) currentBoardState) player2))
        ;(format t "less than the max col AND player2 piece to the right~%")
        (incf posCol)
        ;tile to the right is an opponents piece.  Check to see if you can capture it (or more).
        (loop
          while (< posCol 8) do
            (if (equal (nth (+ (* userRow 8) posCol) currentBoardState) player1)
              (return-from isValid t)
              (incf posCol)
            )
        )
        ;reset posCol
        (setf posCol (1+ userCol))
      )
      (t
        (return-from isValid nil)
      )
    )
    ;dia

  )
)
