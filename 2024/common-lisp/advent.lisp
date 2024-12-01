
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

;; day 1 -----------------------------------------------------------------------




