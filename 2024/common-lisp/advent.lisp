
(ql:quickload "alexandria")
(ql:quickload "arrows")
(ql:quickload "trivia")

(defpackage #:advent
  (:use #:cl #:trivia)
  (:import-from #:arrows #:-<> #:-<>>)
  ;; (:import-from #:trivia #:match)
  (:local-nicknames (#:a #:alexandria)))

(in-package #:advent)

;; helpers ---------------------------------------------------------------------

(defmethod print-object ((object hash-table) stream)
  (format stream "#HT{~{~{(~a: ~a)~}~^ ~}}"  (items object)))

(defun items (hash-table)
  (loop for key being the hash-keys of hash-table
          using (hash-value value)
        collect (list key value)))

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

;; day 5 -----------------------------------------------------------------------

(setq test "
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(defun parsed (input)
  (destructuring-bind (rules updates)
      (split-on (coerce '(#\Newline #\Newline) 'string) input)
    (list
     (->> rules
          lines
          (mapcar (lambda (x) (->> (split-on "|" x) (mapcar #'parse-integer))))
          (reduce (lambda (hash-map pair)
                    (setf (gethash (car pair) hash-map)
                          (append (gethash (car pair) hash-map nil)
                                  (cdr pair)))
                    hash-map)
                  <>
                  :initial-value (make-hash-table)))
     (->> updates
          lines
          (mapcar (lambda (x) (->> (split-on "," x) (mapcar #'parse-integer))))))))

(defun update-valid? (rules update)
  (if (eq update nil) 't
      (let* ((afters (gethash (car update) rules nil))
             (befores (cdr update)))
        (if (eq (intersection befores afters) nil)
            (update-valid? rules befores)
            nil))))

(defun valid-updates (rules updates)
  (loop for update in updates
        if (update-valid? rules (reverse update))
          collect update))

(defun middle-number (lst) (nth (/ (- (length lst) 1) 2) lst))

(defun part-one (input)
  (destructuring-bind (rules updates) (parsed input)
    (->> (valid-updates rules updates)
         (mapcar #'middle-number)
         (apply #'+))))

;; (part-one (get-input 5))

(defun comes-before? (rules x y) (member y (gethash x rules nil)))

(defun part-two (input)
  (destructuring-bind (rules updates) (parsed input)
    (loop for (orig sorted) in (mapcar (lambda (x)
                                         (list x (sorted x (a:curry #'comes-before? rules))))
                                       updates)
          when (not (equal orig sorted))
            sum  (middle-number sorted))))

;; (part-two (get-input 5))

;; day 6 -----------------------------------------------------------------------

(setq test "
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")

(defun find-key-by-val (hash-table to-find)
  (maphash (lambda (k v)
             (when (equal v to-find)
               (return-from find-key-by-val k)))
           hash-table))

(defun parsed (input)
  (let* ((grid (parse-to-grid input))
         (guard-pos (find-key-by-val grid #\^)))
    (setf (gethash guard-pos grid) #\.) ; The guard's spot is free of obstructions
    (list grid guard-pos :up)))

(defun turn-90 (dir)
  (match dir
    (:up    :right)
    (:right :down)
    (:down  :left)
    (:left  :up)))

(defun next-pos (cur-pos dir)
  (destructuring-bind (x y) cur-pos
    (match dir
      (:up    (list x (- y 1)))
      (:right (list (+ x 1) y ))
      (:down  (list x (+ y 1)))
      (:left  (list (- x 1) y)))))

(defun space-open? (c) (char= #\. c))

(defun step-game (grid guard-pos cur-dir)
  (let ((np (next-pos guard-pos cur-dir)))
    (multiple-value-bind (next in-grid) (gethash np grid)
      (if (not in-grid) nil
          (if (space-open? next)
              (list grid np cur-dir)
              (step-game grid guard-pos (turn-90 cur-dir)))))))

(defun run-game (state)
  (let ((s (apply #'step-game state)))
    (if s (cons s (run-game s)) nil)))

(defun part-one (input) ;; + 1 ... I assume for the starting pos?
  (->> (run-game (parsed input))
       (mapcar #'second)
       (remove-duplicates <> :test #'equal)
       length))

;; (part-one (get-input 6))

(defun run-potentially-endless-game (state current-run)
  (let ((s (apply #'step-game state)))
    (if (> current-run 10000) '(:loop)
        (if s (cons s (run-potentially-endless-game s (+ 1 current-run)))))))

(defun run-with-pos-blocked (pos state)
  (destructuring-bind (grid guard-pos dir) state
    (let ((orig-val (gethash pos grid)))
      (setf (gethash pos grid) #\#)
      (prog1 (run-potentially-endless-game (list grid guard-pos dir) 0)
        (setf (gethash pos grid) orig-val)))))

(defun part-two (input)
  (destructuring-bind (grid guard-pos dir) (parsed input)
    (loop for key being the hash-keys of grid
          and r = (last (run-with-pos-blocked key (list grid guard-pos dir)))
          if (equal (car r) :loop)
            count r)))

;; (part-two (get-input 6))

;; day 7 -----------------------------------------------------------------------
(setq test "
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")

(defun parsed (input)
  (->> (lines input)
       (mapcar (lambda (x)
                 (destructuring-bind (a b) (split-on ":" x)
                   (list (parse-integer a)
                         (mapcar #'parse-integer (words b))))))))

(defun mk-expression (xs)
  (match xs
    ((list a b) (list (list a ':add b)
                      (list a ':mul b)))
    ((cons y ys)
     (loop for v in (mk-expression ys)
           append (list (append (list y ':add) v)
                        (append (list y ':mul) v))))))

(defun do-expression (xs)
  (labels ((recur (ys acc)
             (match ys
               ((list* :add a as) (recur as (+ acc a)))
               ((list* :mul a as) (recur as (* acc a)))
               (_ acc))))
    (recur (cdr xs) (car xs))))

(defun part-one (input)
  (loop for (test-value vs) in (parsed input)
        if (any? (loop for exp in (mk-expression vs)
                       collect (eq test-value (do-expression exp))))
          sum test-value))

;; (part-one (get-input 7))

(defun mk-expression (xs)
  (match xs
    ((list a b) (list (list a ':add b)
                      (list a ':mul b)
                      (list a ':concat b)))
    ((cons y ys)
     (loop for v in (mk-expression ys)
           append (list (append (list y ':add) v)
                        (append (list y ':concat) v)
                        (append (list y ':mul) v))))))

(defun concat-op (x y)
  (parse-integer (concatenate 'string (write-to-string x) (write-to-string y))))

(defun do-expression (xs)
  (labels ((recur (ys acc)
             (match ys
               ((list* :add a as) (recur as (+ acc a)))
               ((list* :mul a as) (recur as (* acc a)))
               ((list* :concat a as) (recur as (concat-op acc a)))
               (_ acc))))
    (recur (cdr xs) (car xs))))

(defun part-two (input)
  (loop for (test-value vs) in (parsed input)
        if (any? (loop for exp in (mk-expression vs)
                       collect (eq test-value (do-expression exp))))
          sum test-value))

;; (part-two (get-input 7))

;; day 8 -----------------------------------------------------------------------
(setq test "
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............")

(defun parsed (input)
  (let ((g (make-hash-table :test 'equal)))
    (maphash (lambda (k v)
               (when (not (equal v #\.))
                 (setf (gethash v g) (append (gethash v g nil) (list k)))))
             (parse-to-grid input))
    g))

(defun pairs (xs)
  (let (r) (a:map-combinations (lambda (x) (push x r)) xs :length 2) (nreverse r)))

(defun anodes (positions)
  (destructuring-bind (pos1 pos2) positions
    (list
     (mapcar #'+ pos1 (mapcar #'- pos1 pos2))
     (mapcar #'+ pos2 (mapcar #'- pos2 pos1)))))

(defun anode-map (input)
  (let ((frequencies (parsed input))
        (indicies (parse-to-grid input)))
    (loop for (k v) in (items frequencies)
          collect (list k (->> (pairs v)
                               (mapcan #'anodes)
                               (remove-duplicates <> :test #'equal)
                               (remove-if-not (lambda (x) (gethash x indicies))))))))

(defun part-one (input)
  (-> (mapcan #'cadr (anode-map input))
      (remove-duplicates <> :test #'equal)
      length))

;; (part-one (get-input 8))

(defun anodes (positions)
  (destructuring-bind (pos1 pos2) positions
    (append positions
            (loop for mult from 1 to 100
                  append
                  (list
                   (mapcar #'+
                           pos1
                           (mapcar #'* `(,mult ,mult)
                                   (mapcar #'- pos1 pos2)))
                   (mapcar #'+
                           pos2
                           (mapcar #'* `(,mult ,mult)
                                   (mapcar #'- pos2 pos1))))))))

;; part two:
;; (part-one (get-input 8))
