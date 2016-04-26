;---------------------------------------------------------------------------------
; Function:  display
; Author:  Jason Anderson
; Parameters:
;		1) boardState - the current board to be displayed
;
; Description:  This function takes a board and displays it in a nice format.
;---------------------------------------------------------------------------------
(defun display (boardState)
  (let (n j)
    (format t "~%  1 2 3 4 5 6 7 8~%")
    (setf j 0)
    (loop
      for n from 1 to 8 do
        (format t "~S " n)
        (format t "~S " (nth (* (1- n) 8) boardState))
        (format t "~S " (nth (+ (* (1- n) 8) 1) boardState))
        (format t "~S " (nth (+ (* (1- n) 8) 2) boardState))
        (format t "~S " (nth (+ (* (1- n) 8) 3) boardState))
        (format t "~S " (nth (+ (* (1- n) 8) 4) boardState))
        (format t "~S " (nth (+ (* (1- n) 8) 5) boardState))
        (format t "~S " (nth (+ (* (1- n) 8) 6) boardState))
        (format t "~S~%" (nth (+ (* (1- n) 8) 7) boardState))
    )
    (format t "~%")
  )
)
