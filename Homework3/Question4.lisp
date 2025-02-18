

(defun insert-into-sorted (item sorted)
  (cond
    ((null sorted) (list item))
    ((<= item (car sorted)) (cons item sorted))
    (t (cons (car sorted) (insert-into-sorted item (cdr sorted))))))

(defun insertion-sort (unsorted sorted)
  (if (null unsorted)
      sorted
      (insertion-sort (cdr unsorted) (insert-into-sorted (car unsorted) sorted))))

(defun sort-list (lst)
  (insertion-sort lst '()))

(print (sort-list '(5 2 9 1 5 6)))
(print (sort-list '(3 1 4 1 5 9 2)))
(print (sort-list '(99 88 77 66 55)))
(print (sort-list '(1)))
(print (sort-list '()))
