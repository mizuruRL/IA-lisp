(defvar *optimal-play* (list nil most-negative-fixnum))
(defvar *cuts* 0)
(defvar *explored* 0)

(defun negamax (node depth player a b color &optional (max-player 2))
"Implementation of the negamax algorithm with alpha beta cuts. This is the fail-soft version. Root node is NODE, with max depth and player number for successor generation.
Color represents the signal switching of a terminal node's value. Max-player is the player number that the algorithm is trying to maximize, 2 by default."
    (if (or (terminal-p node) (= depth 0))
        (* color (eval-node node max-player))
        (let ((successors (qsort-nodes (generate-successors node (all-actions-list) player)))
        (value most-negative-fixnum))
            (dolist (child successors)
                (setf value (max value (- (negamax child (1- depth) (switch-player player) (- b) (- a) (- color) max-player))))
                (progn (when (and (null (get-parent-node node)) (< (second *optimal-play*) value)) (setf *optimal-play* (list (get-node-state child) value))))
                (setf a (max a value))
                (setf *explored* (1+ *explored*))
                (when (>= a b) (progn (setf *cuts* (1+ *cuts*))(return)))
            )
            value
        )
    )
)

(defun reset-vars (state)
"Resets the variables for the best play, cuts and explored nodes to be stored by the negamax algorithm."
    (setf *optimal-play* (list state most-negative-fixnum))
    (setf *cuts* 0)
    (setf *explored* 0)
)
