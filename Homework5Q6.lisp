(defparameter *input* "")
(defparameter *pos* 0)

(defun match (char)
  (if (and (< *pos* (length *input*))
           (char= (char *input* *pos*) char))
      (setq *pos* (+ *pos* 1))
      (error "Error: Unexpected symbol '~A' at position ~A" (char *input* *pos*) *pos*)))

(defun G ()
  (if (< *pos* (length *input*))
      (let ((char (char *input* *pos*)))
        (if (member char '(#\x #\y #\z #\w))
            (match char)
            (error "Error: Invalid character '~A' at position ~A. Expected one of 'x', 'y', 'z', 'w'." char *pos*)))
      (error "Error: Unexpected end of input while parsing G at position ~A" *pos*)))

(defun E-prime ()
  (when (and (< *pos* (length *input*)) (char= (char *input* *pos*) #\o))
    (match #\o)
    (G)
    (E-prime)))

(defun E ()
  (G)
  (E-prime))

(defun L-prime ()
  (when (and (< *pos* (length *input*)) (char= (char *input* *pos*) #\s))
    (match #\s)
    (L-prime)))

(defun L ()
  (when (< *pos* (length *input*))
    (match #\s)
    (L-prime)))

(defun S-prime ()
  (when (and (< *pos* (length *input*)) (char= (char *input* *pos*) #\s))
    (match #\s)
    (S-prime)))

(defun S ()
  (if (< *pos* (length *input*))
      (let ((char (char *input* *pos*)))
        (cond
          ((char= char #\s) 
           (match #\s) 
           (S-prime))
          ((char= char #\d) 
           (match #\d) 
           (L) 
           (if (< *pos* (length *input*)) 
               (match #\b) 
               (error "Error: Unexpected end of input while parsing S at position ~A" *pos*)))
          (t 
           (error "Error: Unexpected character '~A' at position ~A. Expected 's' or 'd'." char *pos*))))
      (error "Error: Unexpected end of input while parsing S at position ~A" *pos*)))

(defun I ()
  (match #\i)
  (E)
  (S))

(defun parse (input-string)
  (setq *input* input-string)
  (setq *pos* 0)
  (handler-case
      (progn
        (I)
        (if (= *pos* (length *input*))
            (format t "Accepted: ~A~%" input-string)
            (error "Error: Unexpected end of input at position ~A" *pos*)))
    (error (c) (format t "Error: ~A~%" c))))

;; Example usage:
;; (parse "ixoyowdssbes")  ; Should print "Accepted: ixoyowdssbes"
;; (parse "iwosdssb")     ; Should print an error message