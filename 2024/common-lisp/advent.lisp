
(ql:quickload "alexandria")
(ql:quickload "arrows")
(ql:quickload "trivia")

(defpackage #:advent
  (:use #:cl)
  (:import-from #:arrows #:-<> #:-<>>)
  (:import-from #:trivia #:match)
  (:local-nicknames (#:a #:alexandria)))

(in-package #:advent)

;; helpers ---------------------------------------------------------------------

(defun average (&rest vals) (/ (apply #'+ vals) (length vals)))

(defun any? (lst) (some #'identity lst))

(defun sorted (lst pred) (sort (copy-list lst) pred))

(defun pair-consecutive (xs) (zip xs (cdr xs)))

(defmacro -> (&rest args) `(arrows:-<> ,@args))
(defmacro ->> (&rest args) `(arrows:-<>> ,@args))

(defun space? (c) (find c '(#\Space #\Tab #\Newline #\Return #\FormFeed)))

(defgeneric break-off (pred input))
(defmethod break-off (pred (input string))
  (do ((i 0 (1+ i)))
      ((or (>= i (length input)) (not (funcall pred (aref input i))))
       (values (subseq input 0 i) (subseq input i)))))

(defgeneric drop-while (pred input))
(defmethod drop-while (pred (input string))
  (multiple-value-bind (match rest) (break-off pred input) rest))

(defgeneric take-while (pred input))
(defmethod take-while (pred (input string))
  (multiple-value-bind (match rest) (break-off pred input) match))

(defun lstrip (s) (drop-while #'space? s))
(defun rstrip (s) (-> s reverse lstrip reverse))
(defun strip (s) (-> s lstrip rstrip))

(defun words (input)
  (labels ((recur (s)
             (if (string= s "") nil
                 (multiple-value-bind (match rest)
                     (break-off (complement #'space?) (lstrip s))
                   (cons match (recur rest))))))
    (recur (rstrip input))))

(defun zip (xs ys) ;; "you are reimplementing mapcar: mapcar #'list xs ys, etc"
  (match (list xs ys)
    ((list (cons x xs*) (cons y ys*))
     (cons (list x y) (zip xs* ys*)))
    ((list _ _) nil)))

(defun lines (input) (uiop:split-string (strip input) :separator uiop:+lf+))

(defun get-input (day-num)
  (let ((input-file-name (format nil "input/~a" day-num)))
    (if (probe-file input-file-name) (a:read-file-into-string input-file-name)

        ;; If the input isn't on disk, curl it
        (let* ((cur-dir (strip (uiop:run-program "pwd" :output 'string)))
               (url (format nil "https://adventofcode.com/2024/day/~a/input"
                            day-num))
               (cookie (strip (a:read-file-into-string ".cookie")))
               (curl-cmd (format nil "curl -b session=~a ~a -o ~a/input/~a"
                                 cookie url cur-dir day-num)))
          (uiop:run-program curl-cmd :output 'string)
          (a:read-file-into-string input-file-name)))))

(defun strcat (&rest args) (apply #'concatenate 'string args))

(defun split-on (delimiter input)
  (labels ((recur (d i acc)
             (if (or (string= d "")
                     (string= i "")
                     (> (length d) (length i)))
                 (list (strcat acc i))

                 (let ((taken (subseq i 0 (length d))))
                   (if (string= d taken)
                       (cons acc (recur d (subseq i (length d)) ""))
                       (recur d (subseq i 1) (strcat acc (subseq i 0 1))))))))
    (loop for v in (recur delimiter input "") if (string/= v "") collect v)))

;; day 1 -----------------------------------------------------------------------
(setf test "
  3   4
  4   3
  2   5
  1   3
  3   9
  3   3")

(defun lhs (input)
  (loop for i in input for index from 0 if (evenp index) collect i))

(defun rhs (input)
  (loop for i in input for index from 0 if (oddp index) collect i))

(defun parsed (input) (->> input words (mapcar #'parse-integer)))

(defun distance (x y) (- (max x y) (min x y)))

(defun part-one (input)
  (let* ((left (->> input lhs (sort <> #'<)))
         (right (->> input rhs (sort <> #'<))))
    (loop for (a b) in (zip left right)
          summing (distance a b))))

;; (-> (a:read-file-into-string "input/1") parsed part-one)

(defun part-two (input)
  (let ((left (lhs input))
        (right (rhs input)))
    (loop for ele in left summing (* ele (count ele right)))))

;; (-> (a:read-file-into-string "input/1") parsed part-two)

;; day 2 -----------------------------------------------------------------------
(setq test "
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
")

(defun parsed (input)
  (mapcar (lambda (x) (->> x words (mapcar #'parse-integer)))
          (lines input)))

(defun all-increasing? (report) (equalp report (sorted report #'<)))
(defun all-decreasing? (report) (equalp report (sorted report #'>)))

(defun differences-are-close? (report)
  (let ((step-size (mapcar (lambda (x) (apply #'distance x))
                            (pair-consecutive report))))
    (and (<= (apply #'max step-size) 3)
         (>= (apply #'min step-size) 1))))

(defun safe? (report)
  (and (or (all-increasing? report)
           (all-decreasing? report))
       (differences-are-close? report)))

(defun part-one (in)
  (->> (parsed in)
       (remove-if-not #'safe?)
       length))

;; (part-one (get-input 2))

(defun without (lst index)
  (append (subseq lst 0 index) (subseq lst (1+ index))))

(defun mostly-safe? (report)
  (->> (loop for _ in report for index from 0 collect (without report index))
       (mapcar #'safe?)
       any?))

(defun part-two (in)
  (->> (parsed in)
       (remove-if-not #'mostly-safe?)
       length))

;; (part-two (get-input 2))

;; day 3 -----------------------------------------------------------------------

(setq test
"xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(defun all-digits? (str) (every #'digit-char-p str))

(defun parse-comma-separated-int (str)
  (if (= (count #\, str) 1)
      (match (split-on "," str)
        ((list a b)
         (if (and (all-digits? a)
                  (all-digits? b))
             (list (parse-integer a) (parse-integer b))))
        (_ nil))))

(defun parse-mul (input)
  (->> (split-on "mul(" input)
       cdr
       (mapcar (lambda (x) (car (split-on ")" x))))
       (mapcar #'parse-comma-separated-int)
       (remove-if-not #'identity)))

(defun part-one (input) (loop for (a b) in (parse-mul input) summing (* a b)))

;; (part-one (get-input 3))

(setq test
"xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(defun parse-single-mul (input)
  (if (>= (length input) 4)
      (let* ((head (subseq input 0 4))
             (dropped (subseq input 4)))
        (multiple-value-bind (split-before split-after)
            (break-off (lambda (x) (char/= x #\))) dropped)
          (let ((parsed-val
                  (if (string= "mul(" head)
                      (parse-comma-separated-int split-before))))
            (if parsed-val
                (list (cons :mul parsed-val) (subseq split-after 1))))))))

(defun parse-token (token result input)
  (if (>= (length input) (length token))
      (if (string= (subseq input 0 (length token)) token)
          (list result (subseq input (length token))))))

(defun apply-parsers (input)
  (or (parse-single-mul input)
      (parse-token "do()" :do input)
      (parse-token "don't()" :dont input)))

(defun parse-input (input)
  (if (< (length input) 1) nil
      (match (apply-parsers input)
        ((list parsed to-do)
         (cons parsed (parse-input to-do)))
        (nil
         (parse-input (subseq input 1))))))

(defun part-two (input)
  (apply #'+
         (let ((mul-enabled t)
               (result nil))
           (loop for cmd in (parse-input input)
                 do (match cmd
                      ((list :mul a b)  (if mul-enabled (push (* a b) result)))
                      (:do              (setf mul-enabled t))
                      (:dont            (setf mul-enabled nil))))
           (reverse result))))

;; (part-two (get-input 3))

;; day 4 -----------------------------------------------------------------------

(setq test "
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

(defun add-indicies (input)
  (loop for line in (lines input)
        for y from 0
        append (loop for ele across line
                     for x from 0
                     collect (list (list x y) ele))))

(defun parse-to-grid (input)
  (let ((grid (make-hash-table :test 'equal)))
    (loop for (pos val) in (add-indicies input)
          do (setf (gethash pos grid) val))
    grid))

(defun adjacents (pos)
  (destructuring-bind (x y) pos
    (list (list (list (+ x 1) y) ; forwards
                (list (+ x 2) y)
                (list (+ x 3) y))
          (list (list (- x 1) y) ; backwards
                (list (- x 2) y)
                (list (- x 3) y))
          (list (list x (+ y 1)) ; down
                (list x (+ y 2))
                (list x (+ y 3)))
          (list (list x (- y 1)) ; up
                (list x (- y 2))
                (list x (- y 3)))
          (list (list (+ x 1) (+ y 1)) ; lower right
                (list (+ x 2) (+ y 2))
                (list (+ x 3) (+ y 3)))
          (list (list (+ x 1) (- y 1)) ; upper right
                (list (+ x 2) (- y 2))
                (list (+ x 3) (- y 3)))
          (list (list (- x 1) (+ y 1))  ; lower left
                (list (- x 2) (+ y 2))
                (list (- x 3) (+ y 3)))
          (list (list (- x 1) (- y 1)) ; upper left
                (list (- x 2) (- y 2))
                (list (- x 3) (- y 3))))))

(defun adjacent-strings (grid pos)
  (loop for direction in (adjacents pos)
        collect (coerce (cons (gethash pos grid #\.)
                              (loop for pos in direction
                                    collect (gethash pos grid #\.)))
                        'string)))

(defun part-one (input)
  (let ((grid (parse-to-grid input)))
    (/ (length
        (loop for key being the hash-keys of grid
              append (loop for search in (adjacent-strings grid key)
                           if (or (string= "XMAS" search)
                                  (string= "XMAS" (reverse search)))
                             collect search)))
       2)))

;; (part-one (get-input 4))

(defun backslash (pos)
  (destructuring-bind (x y) pos
    (list (list (+ x 1) (+ y 1))     ; lower right
          (list (- x 1) (- y 1))))) ; upper left

(defun forwardslash (pos)
  (destructuring-bind (x y) pos
    (list (list (+ x 1) (- y 1))        ; upper right
          (list (- x 1) (+ y 1)))))     ; lower left

(defun is-ms? (grid ps)
  (let ((vals (mapcar (lambda (pos) (gethash pos grid #\.)) ps)))
    (and (member #\M vals)
         (member #\S vals))))

(defun is-xmas? (grid pos)
  (and (is-ms? grid (backslash pos))
       (is-ms? grid (forwardslash pos))))

(defun part-two (input)
  (let ((grid (parse-to-grid input)))
    (loop for key being the hash-keys of grid
          when (char= #\A (gethash key grid #\.))
            count (is-xmas? grid key))))

;; (part-two (get-input 4))
