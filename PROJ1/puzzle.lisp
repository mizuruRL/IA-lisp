(defun insert-vertical-arc (board line column)
"Inserts a vertical arc on BOARD at LINE COLUMN"
    (cond ((null board) nil)
        ((or (not (in-bounds-vertical (cadr board) line column)) (= 1 (get-vertical-arc-at (cadr board) line column))) nil)
        (t (list (car board) (append (get-preceeding (cadr board) column) (cons (replace-n (get-n (cadr board) column) line 1) nil) (nthcdr column (cadr board)))))
    )
)

(defun insert-horizontal-arc (board line column)
"Inserts a horizontal arc on BOARD at LINE COLUMN"
    (cond ((null board) nil)
        ((or (not (in-bounds-horizontal (car board) line column)) (= 1 (get-horizontal-arc-at (car board) line column))) nil)
        (t (list (append (get-preceeding (car board) line) (cons (replace-n (get-n (car board) line) column 1) nil) (nthcdr line (car board))) (cadr board)))
    )
)

(defun check-box (board line column)
"Checks if there is a box at LINE COLUMN from BOARD."
    (cond ((null board) nil)
          (t (and (= 1 (get-vertical-arc-at (cadr board) line column)) (= 1 (get-vertical-arc-at (cadr board) line (1+ column))) (= 1 (get-horizontal-arc-at (car board) line column)) (= 1 (get-horizontal-arc-at (car board) (1+ line) column))))
        )      
)

(defun check-all-closed-boxes (board &optional (line 1) (column 1))
"Checks how many closed boxes a given BOARD has."
    (cond ((not (in-bounds-horizontal (car board) (1+ line) 1)) 0)
          ((not (in-bounds-horizontal (car board) 1 column)) (check-all-closed-boxes board (1+ line) 1))
          ((check-box board line column) (1+ (check-all-closed-boxes board line (1+ column))))
          (t (check-all-closed-boxes board line (1+ column)))
    )
)

(defun increment-pos-horizontal (horizontal-list line column)
"Increments LINE COLUMN to next coordinates available from HORIZONTAL-LIST."
    (cond ((or(null horizontal-list) (null line) (null column)) nil)
            ((in-bounds-horizontal horizontal-list line (1+ column)) (list line (1+ column)))
            ((in-bounds-horizontal horizontal-list (1+ line) 1) (list (1+ line) 1))
            (t nil)
    )
)

(defun increment-pos-vertical (vertical-list line column)
"Increments LINE COLUMN to next coordinates available from VERTICAL-LIST."
    (cond ((or(null vertical-list) (null line) (null column)) nil)
            ((in-bounds-vertical vertical-list (1+ line) column) (list (1+ line) column))
            ((in-bounds-vertical vertical-list 1 (1+ column)) (list 1 (1+ column)))
            (t nil)
    )
)

(defun in-bounds-vertical (vertical-list line column)
"Checks if LINE COLUMN coordinate is in bounds for VERTICAL-LIST."
    (if (or (null line) (null column))
        nil
        (and (>= (length (car vertical-list)) line) (>= (length vertical-list) column) (< 0 line) (< 0 column))
    )
)

(defun in-bounds-horizontal (horizontal-list line column)
"Checks if LINE COLUMN coordinate is in bounds for HORIZONTAL-LIST."
    (if (or (null line) (null column))
        nil
        (and (>= (length (car horizontal-list)) column) (>= (length horizontal-list) line) (< 0 line) (< 0 column))
    )
)

(defun get-preceeding (l n)
"Gets sublist of preceeding elements at N nth from L list."
    (subseq l 0 (1- n))
)

(defun replace-n (l i n)
"Replaces index I element with N element on L list."
    (cond ((null l) nil)
          ((= i 1) (cons n (cdr l)))
          (t (cons (car l) (replace-n (cdr l) (1- i) n)))
    )
)

(defun get-n (l n)
"Gets N nth element from L list."
        (nth (1- n) l)
)

(defun get-vertical-arc-at (columns-list line column)
"Gets vertical arc on COLUMNS-LIST, from given LINE COLUMN coordinate."
        (nth (1- line) (get-n columns-list column))
)

(defun get-horizontal-arc-at (lines-list line column)
"Gets horizontal arc on LINES-LIST, from given LINE COLUMN coordinate."
        (nth (1- column) (get-n lines-list line))
)

(defun get-adjacent-hor-arcs (board &optional (line 1) (column 1))
"Gets the sum of all types of adjacent arcs on given BOARD for each horizontal adjacent arc."
    (if (null board)
        nil
        (cond ((or (null column) (null line)) 0)
              (t (let ((new-coords (increment-pos-horizontal (car board) line column))
                        (hor-arc (get-horizontal-arc-at (car board) line column)))
                    (cond ((and (= 1 hor-arc) (has-adjacent-ver board line column) (has-adjacent-hor board line column)) (+ 2 (get-adjacent-hor-arcs board (car new-coords) (cadr new-coords))))
                          ((and (= 1 hor-arc) (has-adjacent-ver board line column)) (1+ (get-adjacent-hor-arcs board (car new-coords) (cadr new-coords))))
                          (t (get-adjacent-hor-arcs board (car new-coords) (cadr new-coords))))
                    )
                )
        )
    )
)

(defun has-adjacent-ver (board &optional (line 1) (column 1))
"Checks on a BOARD if any horizontal arc has another horizontal arc adjacent to it."
    (let ((ver-arcs (cadr board)))
        (or (and (in-bounds-vertical ver-arcs line column) (or (= 1 (get-vertical-arc-at ver-arcs line column)) (= 1 (get-vertical-arc-at ver-arcs line (1+ column))))) (and (in-bounds-vertical ver-arcs (1- line) column) (or (= 1 (get-vertical-arc-at ver-arcs (1- line) column)) (= 1 (get-vertical-arc-at ver-arcs (1- line) (1+ column))) ) ) )
    )
)

(defun has-adjacent-hor (board &optional (line 1) (column 1))
"Checks on a BOARD if any horizontal arc has another horizontal arc adjacent to it."
    (let ((hor-arcs (car board)))
        (or (and (in-bounds-horizontal hor-arcs line (1- column)) (= 1 (get-horizontal-arc-at hor-arcs line (1- column)))) (and (in-bounds-horizontal hor-arcs line (1+ column)) (= 1 (get-horizontal-arc-at hor-arcs line (1+ column)))))
    )
)