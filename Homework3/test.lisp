(defun make-pairs (lst)
  (cond
    ((null lst) '())
    ((null (cdr lst)) (list (list (car lst))))
    (t (cons (list (min (car lst) (cadr lst)) (max (car lst) (cadr lst)))  
             (make-pairs (cddr lst))))))

(defun merge-lists (list1 list2)
  (cond
    ((null list1) list2)
    ((null list2) list1)
    ((<= (car list1) (car list2)) (cons (car list1) (merge-lists (cdr list1) list2)))
    (t (cons (car list2) (merge-lists list1 (cdr list2))))))

(defun merge-pass (lst)
  (format t "Merging pass: ~a~%" lst) ;; Prints the current state before merging
  (cond
    ((null lst) '())
    ((null (cdr lst)) lst)
    (t (let ((merged (merge-lists (car lst) (cadr lst))))
         (format t "Merging ~a and ~a -> ~a~%" (car lst) (cadr lst) merged)
         (cons merged (merge-pass (cddr lst)))))))

(defun bottom-up-mergesort (lst)
  (let ((sorted-lists (make-pairs lst)))
    (format t "First pass (sorted pairs): ~a~%" sorted-lists) ;; Capturing first pass
    (loop while (> (length sorted-lists) 1)
          do (setq sorted-lists (merge-pass sorted-lists)))
    (car sorted-lists)))

(print (bottom-up-mergesort '(1 7 2 1 8 6 5 3 7 9 4)))
