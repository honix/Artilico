(in-package :artilico)

;;
;; Artilici | utils
;;

;; math

(defun rand-from-to (from to)
  (+ (random (- to from)) from))

;; seqs

(defun split-by (seq elem)
  (let ((l (loop for i = 0 then (1+ j)
          as j = (position elem seq :start i)
          collect (subseq seq i j)
          while j)))
    (make-array (length l) :initial-contents l)))

(defun remove-nth (seq result-type nth)
  (concatenate result-type
           (subseq seq 0 nth)
           (subseq seq (1+ nth))))

(defun insert-after (seq result-type nth insertion)
  (concatenate result-type
           (subseq seq 0 nth)
           insertion
           (subseq seq nth)))
