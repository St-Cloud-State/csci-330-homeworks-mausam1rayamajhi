;; Mausam Rayamajhi
;; CSCI 330
;; Feb 18th 2025


;; Defining a function to create pairs of elements from the list
;; Each pair is sorted so that the smaller element comes first
(defun make-pairs (lst)

  (cond
    ;; If the list is empty, returning an empty list
    ((null lst) '()) 

    ;; If only one element remains, returning it as a single-element list
    ((null (cdr lst)) (list (list (car lst)))) 

    ;; Otherwise, taking two elements at a time, sorting them, 
    ;; and recursively processing the rest of the list
    (t (cons (list (min (car lst) (cadr lst)) (max (car lst) (cadr lst)))  
             (make-pairs (cddr lst))))))

;; Defining a function to merge two sorted lists into a single sorted list
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

;; Defining a function to merge sorted sublists in a single pass
(defun merge-pass (lst)
  (format t "Merging pass: ~a~%" lst)  
  (cond

    ;; If the list is empty, returning an empty list
    ((null lst) '())  

    ;; If only one sublist remains, returning it
    ((null (cdr lst)) lst)  

    ;; Otherwise, merging pairs of sublists and recursively processing the rest
    (t (let ((merged (merge-lists (car lst) (cadr lst))))
         (format t "Merging ~a and ~a -> ~a~%" (car lst) (cadr lst) merged)
         (cons merged (merge-pass (cddr lst)))))))  

;; Implementing the bottom-up Merge Sort algorithm
(defun bottom-up-mergesort (lst)

  ;; Creating sorted pairs from the input list
  (let ((sorted-lists (make-pairs lst)))  
    (format t "First pass (sorted pairs): ~a~%" sorted-lists)
    ;; Displaying initial sorted pairs

    ;; Iteratively merging sublists until only one sorted list remains
    (loop while (> (length sorted-lists) 1)
          do (setq sorted-lists (merge-pass sorted-lists))) 
           
    ;; Returning the final sorted list
    (car sorted-lists)))  

;; Running the bottom-up Merge Sort on a sample list and printing the result
(print (bottom-up-mergesort '(1 7 2 1 8 6 5 3 7 9 4)))  

