(defun insert-vertical-arc (board line column)
    (cond ((null board) nil)
        ((or (not (in-bounds-vertical board line column)) (= 1 (get-vertical-arc-at (cadr board) line column))) nil)
        (t (list (car board) (append (get-preceeding (cadr board) column) (cons (replace-n (get-n (cadr board) column) line 1) nil) (nthcdr column (cadr board)))))
    )
)

(defun insert-horizontal-arc (board line column)
    (cond ((null board) nil)
        ((or (not (in-bounds-horizontal board line column)) (= 1 (get-horizontal-arc-at (car board) line column))) nil)
        (t (list (append (get-preceeding (car board) line) (cons (replace-n (get-n (car board) line) column 1) nil) (nthcdr line (car board))) (cadr board)))
    )
)

(defun check-box (board line column)
    (cond ((null board) nil)
          (t (and (= 1 (get-vertical-arc-at (cadr board) line column)) (= 1 (get-vertical-arc-at (cadr board) line (1+ column))) (= 1 (get-horizontal-arc-at (car board) line column)) (= 1 (get-horizontal-arc-at (car board) (1+ line) column))))
        )      
)

(defun check-all-closed-boxes (board line column)
    (cond ((not (in-bounds-horizontal board (1+ line) 1)) 0)
          ((not (in-bounds-horizontal board 1 column)) (check-all-closed-boxes board (1+ line) 1))
          ((check-box board line column) (1+ (check-all-closed-boxes board line (1+ column))))
          (t (check-all-closed-boxes board line (1+ column)))
    )
)

(defun in-bounds-vertical (board line column)
    (and (>= (length (caadr board)) line) (>= (length (cadr board)) column) (< 0 line) (< 0 column))
)

(defun in-bounds-horizontal (board line column)
    (and (>= (length (caar board)) column) (>= (length (car board)) line) (< 0 line) (< 0 column))
)

(defun get-preceeding (l n)
    (subseq l 0 (1- n))
)

(defun replace-n (l i n)
    (cond ((null l) nil)
          ((= i 1) (cons n (cdr l)))
          (t (cons (car l) (replace-n (cdr l) (1- i) n)))
    )
)

(defun get-n (l n)
        (nth (1- n) l)
)

(defun get-vertical-arc-at (columns-list line column)
        (nth (1- line) (get-n columns-list column))
)

(defun get-horizontal-arc-at (lines-list line column)
        (nth (1- column) (get-n lines-list line))
)

(defun all-vertical-states (lambda-list)
    t
)

(defun test ()
    (setq board '(((0 0 0)(0 0 1)(0 1 1)(0 0 1)) ((0 0 0)(0 1 0)(0 0 1)(0 1 1))))
    (insert-horizontal-arc (insert-vertical-arc board 1 4) 3 1)
)