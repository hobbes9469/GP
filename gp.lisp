; Matthew Kang
; matthesk@fullerton.edu
; CPSC 481
; Project 2: GP


(defvar x)    ; Global variables x, y, z
(defvar y)
(defvar z)
(defvar pool '())   ; Pool to hold expressions
(defvar nextpool '()) ; Pool to hold next generation
(defvar g_count 0)    ; Generation count
 ; TODO: create global for Test samples


; Function to return a random integer operand, from -9 to 9
(defun randint ()
  (let ((a))
    (setq a (random 19))   ; Pick a random integer, 0 to 18
    (setq a (- a 9))       ; Subtract 9 to get range -9 to 9
    a                      ; Return the random integer operand
    ))


; Function to return a random arithmetic operator
; Choices are one of (+ - *)
(defun randop ()
  (let ((a) (op '(+ - *)))  ; Set operator list
    (setq a (random 3))     ; Pick random number 0 to 2
    (nth a op)))            ; Select from list using random index


; Function to return a random variable in '(X Y Z)
(defun randvar ()
  (let ((a))
    (setq a (random 3))     ; Pick random number 0 to 2
    (nth a '(x y z))))      ; Select variable using random index


; Function to return a random expression
; Expressions are the form (op int var)
(defun randexpr ()
  (let ((a) (b) (c) (ret))
    (setq a (randint))
    (setq b (randop))
    (setq c (randvar))
    (setq ret (list b a c))
    ret
    ))



; Fill pool with 50 expressions

(loop
  (if (> 50 (list-length pool))
      (setq pool (append (list (randexpr)) pool))
      (when (>= (list-length pool) 50)
        (return pool))))
(loop for x from 1 to (list-length pool)
      do (print (nth x pool)))





; Scratch space







