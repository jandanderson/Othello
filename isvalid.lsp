(defstruct node board alpha beta parent turn)

;-----------------------------------------------------------------
(load 'doMove)
;----------------------------------------------------------------

(defun isValid (currentBoardState userRow userCol player1 player2)
  (let (moveMade posCol negCol posRow negRow)
    (setf moveMade 0)
    (setf posCol (1+ userCol))
    (setf negCol (1- userCol))
    (setf posRow (1+ userRow))
    (setf negRow (1- userRow))
    ;row and col

    (if (or (equal (nth (+ (* userRow 8) userCol) currentBoardState) player1) (equal (nth (+ (* userRow 8) userCol) currentBoardState) player2))
      (return-from isValid nil)
    )

    ;check tile to the right of the players move
    (when
      (and (< posCol 8) (equal (nth (+ (* userRow 8) posCol) currentBoardState) player2))
        (incf posCol)
        ;tile to the right is an opponents piece.  Check to see if you can capture it (or more).
        (loop
          while (< posCol 8) do
            (cond
              ((equal (nth (+ (* userRow 8) posCol) currentBoardState) player1)
                (return-from isValid 1)
              )
              ((not (equal (nth (+ (* userRow 8) posCol) currentBoardState) player2))
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
      (and (> negCol -1) (equal (nth (+ (* userRow 8) negCol) currentBoardState) player2))
        (decf negCol)
        ;tile to the left is an opponents piece.  Check to see if you can capture it (or more).
        (loop
          while (> negCol -1) do
            (cond
              ((equal (nth (+ (* userRow 8) negCol) currentBoardState) player1)
                (return-from isValid 2)
              )
              ((not (equal (nth (+ (* userRow 8) negCol) currentBoardState) player2))
                (setf negCol -1)
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
                (return-from isValid 3)
              )
              ((not (equal (nth (+ (* posRow 8) userCol) currentBoardState) player2))
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
      (and (> negRow -1) (equal (nth (+ (* negRow 8) userCol) currentBoardState) player2))
        (decf negRow)
        ;tile abvoe is an opponents piece.  Check to see if you can capture it (or more).
        (loop
          while (> negRow -1) do
            (cond 
              ((equal (nth (+ (* negRow 8) userCol) currentBoardState) player1)
                (return-from isValid 4)
              )
              ((not (equal (nth (+ (* negRow 8) userCol) currentBoardState) player2))
                (setf negRow -1)
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
      (and (> negRow -1) (< posCol 8) (equal (nth (+ (* negRow 8) posCol) currentBoardState) player2))
        ;tile to the right is an opponents piece.  Check to see if you can capture it (or more).
        (loop
          while (and (> negRow -1) (< posCol 8)) do
            (cond
              ((equal (nth (+ (* negRow 8) posCol) currentBoardState) player1)
                (return-from isValid 5)
              )
              ((not (equal (nth (+ (* negRow 8) posCol) currentBoardState) player2))
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
                (return-from isValid 6)
              )
              ((not (equal (nth (+ (* posRow 8) posCol) currentBoardState) player2))
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
      (and (> negRow -1) (> negCol -1) (equal (nth (+ (* negRow 8) negCol) currentBoardState) player2))
        ;tile to the right is an opponents piece.  Check to see if you can capture it (or more).
        (loop
          while (and (> negRow -1) (> negCol -1)) do
            (cond
              ((equal (nth (+ (* negRow 8) negCol) currentBoardState) player1)
                (return-from isValid 7)
              )
              ((not (equal (nth (+ (* negRow 8) negCol) currentBoardState) player2))
                (setf negCol -1)
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
      (and (< posRow 8) (> negCol -1) (equal (nth (+ (* posRow 8) negCol) currentBoardState) player2))
        ;tile to the bottom left is an opponents piece.  Check to see if you can capture it (or more).
        (loop
          while (and (< posRow 8) (> negCol -1)) do
            (cond
              ((equal (nth (+ (* posRow 8) negCol) currentBoardState) player1)
                (return-from isValid 8)
              )
              ((not (equal (nth (+ (* posRow 8) negCol) currentBoardState) player2))
                (setf negCol -1)
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
    (return-from isValid nil)
  )
)

