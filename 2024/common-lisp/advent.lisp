
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

; (-> (a:read-file-into-string "input/1") parsed part-two)



