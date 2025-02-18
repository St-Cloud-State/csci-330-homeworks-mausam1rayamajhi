(defun partition-list (lst first-half second-half)
  (cond
    ((null lst) (list (reverse first-half) (reverse second-half))) 
    ((null (cdr lst)) (list (reverse (cons (car lst) first-half)) (reverse second-half))) 
    (t (partition-list (cddr lst) (cons (car lst) first-half) (cons (cadr lst) second-half)))))  

(defun merge-lists (list1 list2)
  (cond
    ((null list1) list2) 
    ((null list2) list1) 
    ((<= (car list1) (car list2)) (cons (car list1) (merge-lists (cdr list1) list2)))
    (t (cons (car list2) (merge-lists list1 (cdr list2)))))) 

(defun mergesort (lst)
  (if (or (null lst) (null (cdr lst))) 
      lst
      (let* ((halves (partition-list lst '() '()))  
             (left (car halves))
             (right (cadr halves)))
        (merge-lists (mergesort left) (mergesort right)))))  

(print (mergesort '(10 7 8 9 1 5 3 4)))  
