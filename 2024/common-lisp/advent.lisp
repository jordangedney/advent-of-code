
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
