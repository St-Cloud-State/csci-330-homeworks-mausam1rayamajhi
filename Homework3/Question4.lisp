;; Mausam Rayamajhi
;; CSCI 330
;; Feb 18th 2025

;; Defining a function to insert an element into its correct position 
;; in an already sorted list
(defun insert-into-sorted (item sorted)
  (cond
    ;; If the sorted list is empty, returning a new list with the item
    ((null sorted) (list item))  
    ;; If the item is smaller or equal to the first element, inserting it at the front
    ((<= item (car sorted)) (cons item sorted))  
    ;; Otherwise, keeping the first element and recursively inserting into the rest
    (t (cons (car sorted) (insert-into-sorted item (cdr sorted))))))  

;; Defining a recursive function to perform insertion sort
(defun insertion-sort (unsorted sorted)
  ;; If there are no more elements to sort, returning the sorted list
  (if (null unsorted)  
      sorted  
      ;; Taking the first element of `unsorted`, inserting it into `sorted`,
      ;; and recursively sorting the remaining elements
      (insertion-sort (cdr unsorted) (insert-into-sorted (car unsorted) sorted))))  


(defun sort-list (lst)
  ;; Calling insertion sort with an initially empty sorted list
  (insertion-sort lst '()))  

;; Testing the insertion sort function with different lists
(print (sort-list '(5 2 9 1 5 6)))     ;; Sorting a list with duplicate numbers
(print (sort-list '(3 1 4 1 5 9 2)))   ;; Sorting a list with a mix of small numbers
(print (sort-list '(99 88 77 66 55)))  ;; Sorting a list that is already in descending order
(print (sort-list '(1)))               ;; Sorting a single-element list
(print (sort-list '()))                ;; Sorting an empty list

