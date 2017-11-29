(in-package :cffi)

;;
;; Artilico | cffi-fix
;;

;;; redefinition to not spam thise warnings

(defun parse-deprecated-struct-type (name struct-or-union)
  (check-type struct-or-union (member :struct :union))
  (let* ((struct-type-name `(,struct-or-union ,name))
         (struct-type (parse-type struct-type-name)))
    ;; (simple-style-warning
    ;;  "bare references to struct types are deprecated. ~
    ;;   Please use ~S or ~S instead."
    ;;  `(:pointer ,struct-type-name) struct-type-name)
    (make-instance (class-of struct-type)
                   :alignment (alignment struct-type)
                   :size (size struct-type)
                   :slots (slots struct-type)
                   :name (name struct-type)
                   :bare t)))
