(ql:quickload :alexandria)
(uiop:define-package advent
  (:use #:clump #:cl-syntax)
  (:local-nicknames (#:a #:alexandria)))

(in-package #:advent)
(cl-syntax:use-syntax :clump)

;; helpers ---------------------------------------------------------------------
(defvar whitespace-chars (list #\space #\newline #\tab #\return))
(def strip (s) (string-trim whitespace-chars s))

(def input-file-name (day-num) (format nil "input/~a" day-num))

(def get-input (day-num)
  (= ((input-file-name (format nil "input/~a" day-num)))
     (if (probe-file input-file-name) (alexandria:read-file-into-string input-file-name)

        ;; If the input isn't on disk, curl it
        (= ((cur-dir (strip (uiop:run-program "pwd" :output :string)))
               (url (format nil "https://adventofcode.com/2025/day/~a/input"
                            day-num))
               (cookie (strip (alexandria:read-file-into-string ".cookie")))
               (curl-cmd (format nil "curl -b session=~a ~a -o ~a/input/~a"
                                 cookie url cur-dir day-num)))
           (uiop:run-program curl-cmd :output :string)
           (alexandria:read-file-into-string input-file-name)))))

;; day 1 -----------------------------------------------------------------------
(set test 
"L68
L30
R48
L5
R60
L55
L1
L99
R14
L82")

(def rot-dir (x) (if (headmatch "R" x) #'+ #'-))
(def rotate (pos x) (mod (funcall (rot-dir x) pos (parse-integer (subseq x 1))) 100))

(def scanl (fn initial-arg xs)
  (if (null xs) nil
      (cons initial-arg (scanl fn (funcall fn initial-arg (car xs)) (cdr xs)))))

(def part-one (input)
  (count (fn (x) (equalp x 0)) (scanl #'rotate 50 (tokens input))))

;; (part-one (get-input 1))

(def rotate2 (oldpos todo)
  (= ((v (funcall (rot-dir todo) 0 (parse-integer (subseq todo 1))))
      (pos (car oldpos))
      (newpos (+ v pos)))
    (if (<= newpos 0) 
        (= ((npm (mod newpos 100))
            (zeroes (floor (+ 1 (/ (abs newpos) 100)))))
           (if (equalp (car oldpos) 0) (list npm (1- zeroes))
               (list npm zeroes)))
        
        (list (mod newpos 100) (floor (/ newpos 100))))))

(def part-two (input)
  (apply #'+ (mapcar #'cadr (scanl #'rotate2 '(50 0) (tokens input)))))

;; (part-two (get-input 1))

;; day 2 -----------------------------------------------------------------------