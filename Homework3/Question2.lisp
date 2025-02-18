;;Mausam Rayamajhi
;;CSCI 330
;;Feb 18th 2025


;; Defining a function to split the list into two halves, 
;; alternating elements between `first-half` and `second-half`

(defun partition-list (lst first-half second-half)
  (cond

    ;; When reaching the end of the list, returning the two halves in reversed order
    ((null lst) (list (reverse first-half) (reverse second-half))) 

    ;; If there's only one element left, adding it to `first-half` and returning the result
    ((null (cdr lst)) (list (reverse (cons (car lst) first-half)) (reverse second-half))) 

    ;; Recursively processing the list by taking two elements at a time, 
    ;; adding one to `first-half` and the next to `second-half`
    (t (partition-list (cddr lst) (cons (car lst) first-half) (cons (cadr lst) second-half)))))  

;; Defining a function to merge two sorted lists into one sorted list
(defun merge-lists (list1 list2)

  (cond
    ;; If one list is empty, returning the other list
    ((null list1) list2) 
    ((null list2) list1) 

    ;; Comparing the first elements of both lists, taking the smaller one first,
    ;; and recursively merging the remaining elements
    ((<= (car list1) (car list2)) (cons (car list1) (merge-lists (cdr list1) list2)))

    ;; Otherwise, taking the first element of `list2` and merging the rest
    (t (cons (car list2) (merge-lists list1 (cdr list2)))))) 

;; Defining the Merge Sort function
(defun mergesort (lst)

  ;; If the list has zero or one element, returning it as is (already sorted)
  (if (or (null lst) (null (cdr lst))) 

      lst
      ;; Otherwise, splitting the list into two halves, sorting them recursively, 
      ;; and merging them back together
      (let* ((halves (partition-list lst '() '()))  
      
             (left (car halves))
             (right (cadr halves)))
        (merge-lists (mergesort left) (mergesort right)))))  

;; Testing Merge Sort with a sample list
(print (mergesort '(10 7 8 9 1 5 3 4)))  
