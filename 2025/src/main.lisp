(ql:quickload :alexandria)
(uiop:define-package advent
  (:use #:clump #:cl-syntax)
  (:local-nicknames (#:a #:alexandria)))

(in-package #:advent)
(cl-syntax:use-syntax :clump)

;; helpers ---------------------------------------------------------------------
(defvar whitespace-chars (list #\space #\newline #\tab #\return))

(def strip (s) (string-trim whitespace-chars s))

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

(def scanl (fn initial-arg xs)
  (if (null xs) (list initial-arg)
      (cons initial-arg (scanl fn (funcall fn initial-arg (car xs)) (cdr xs)))))

(def strcat (&rest args) (apply #'concatenate 'string args))

(def split-on (delimiter input)
  (labels ((recur (d i acc)
             (if (or (string= d "")
                     (string= i "")
                     (> (length d) (length i)))
                 (list (strcat acc i))

                 (= ((taken (subseq i 0 (length d))))
                   (if (string= d taken)
                       (cons acc (recur d (subseq i (length d)) ""))
                       (recur d (subseq i 1) (strcat acc (subseq i 0 1))))))))
    (loop for v in (recur delimiter input "") if (string/= v "") collect v)))

;; test-input ------------------------------------------------------------------
(set test-1
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

(set test-2
"11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")

(set test-3
"987654321111111
811111111111119
234234234234278
818181911112111")

;; day 1 -----------------------------------------------------------------------

(def rot-dir (x) (if (headmatch "R" x) #'+ #'-))
(def rotate (pos x) (mod (funcall (rot-dir x) pos (parse-integer (subseq x 1))) 100))

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

(def parsed (input)
  (->> (split-on "," input)
       (mapcar (fn (x)
                 (->> (split-on "-" x )
                      (mapcar (fn (y)
                                (parse-integer y))))))
       (mapcar (fn (xs) (apply #'range xs)))))

(def repeated-twice? (s)
  (= ((l (len s)))
     (if (even l)
         (string-equal (subseq s 0 (/ l 2)) (subseq s (/ l 2))))))

(def invalid-id? (num) (if (repeated-twice? (string num)) num))

(def without-nils (xs) (loop for x in xs when x collect x))

(def part-one (input)
  (->> (mapcar (fn (xs) (mapcar #'invalid-id? xs)) input)
       alexandria:flatten
       without-nils
       (apply #'+)))

;; (part-one (parsed (get-input 2)))

(def chunk (xs n)
  (if (<= (len xs) n) (list xs)
      (cons (subseq xs 0 n) (chunk (subseq xs n) n))))

;; (def chunks (xs)
  ;; (loop for i in (range 1 (1- (len xs)))
        ;; collecting (chunk xs i)))

(def even-chunks (xs)
  (if (<= (len xs) 1) xs
      (loop for i in (range 1 (1- (len xs)))
            when (integerp (/ (len xs) i))
            collect (chunk xs i))))

(def all-equal? (xs) (all (fn (x) (equalp x (car xs))) xs))

(def any? (xs) (if (null xs) nil (if (car xs) (car xs) (any? (cdr xs)))))

(def invalid-id2? (num)
  (if (< num 10) nil
      (if (any? (mapcar #'all-equal? (even-chunks (string num)))) num)))

(def part-two (input)
    (apply #'+ (-> (loop for x in (mapcar (fn (xs) (mapcar #'invalid-id2? xs)) input)
                    appending x)
                   without-nils)))

;; (part-two (parsed (get-input 2)))

;; day 3 -----------------------------------------------------------------------

(def digits (xs) (map #'digit xs))
(def digits->int (xs) (reduce (fn (l r) (+ (* 10 l) r)) xs))
(def drop-last (n xs) (subseq xs 0 (- (len xs) n)))

(def next-max (xs)
  (= ((m (apply #'max xs)))
     (values m (find-index (fn (x) (equalp m x)) xs))))

(def max-digits-from-line (int-len line)
  (if (equalp 0 int-len) nil
      (mvb (max index) (next-max (drop-last (1- int-len) line))
        (cons max (max-digits-from-line (1- int-len) (subseq line (1+ index)))))))

(def solve (int-len input)
  (->> (map #'digits (tokens input))
       (map (fn (xs) (max-digits-from-line int-len xs)))
       (map #'digits->int)
       (apply #'+)))

(def part-one (input) (solve 2 input))
(def part-two (input) (solve 12 input))

(part-one (get-input 3))
(part-two (get-input 3))
