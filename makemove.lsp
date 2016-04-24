;---------------------------------------------
(load 'minimax)
;---------------------------------------------

(defun make-move (BoardState player ply)
  (let ((move 0)
        (currentBoardState nil))
    (setf currentBoardState (make-node :board BoardState :alpha (- 1000000) :beta 1000000 :parent nil :turn player))
    (setf move (minimax currentBoardState ply))
    (format t "My move is:~%")
    (return-from make-move (node-board (car (car (cdr move)))))
  )
)
