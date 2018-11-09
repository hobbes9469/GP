; Matthew Kang
; matthesk@fullerton.edu
; CPSC 481
; Project 2: GP


(defvar X)    ; Global variables x, y, z
(defvar Y)
(defvar Z)
(defvar pool '())   ; Pool to hold expressions
(defvar nextpool '()) ; Pool to hold next generation
(defvar g_count 0)    ; Generation count

(defvar testsamples      ; Set of Test samples to use
  '((0 -2 1 -16)
    (-4 -5 -3 58)
    (9 8 -6 72)
    (9 -7 5 113)
    (-8 7 3 150)
    (5 4 -5 20)
    (6 -4 6 41)
    (-5 3 -7 -24)
    (-6 -5 9 18)
    (1 0 2 2)))


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



; Function to run expression with the variable list
(defun runexpr (rvars rexpr)
  (let ((X (car rvars))
        (Y (nth 1 rvars))
        (Z (nth 2 rvars)))
    (eval rexpr)))



; Function that takes an expression and test sample
; Returns a list of (rexpr evaluation fitness)
(defun fitness (rexpr rsample)
  (let* ((retfit)
         (reval)
         (rvars (list (nth 0 rsample)
                     (nth 1 rsample)
                     (nth 2 rsample))))
    (setq reval (runexpr rvars rexpr))
    (list rexpr reval)))
;THIS FUNCTION IS NOT DONE, CURRENTLY DEVELOPING


; Fill pool with 50 expressions

(loop
  (if (> 50 (list-length pool))
      (setq pool (append (list (randexpr)) pool))
      (when (>= (list-length pool) 50)
        (return pool))))
(loop for x from 0 to (- (list-length pool) 1)
      do (format t "~D: Expr: ~S , Eval: ~D ~%" x (nth x pool) (runexpr '(1 1 1) (nth x pool))))



; NEXT GOAL: Print Expr, Eval, and Fitness for each test sample, for each expr

; Scratch space
(loop for x from 0 to (- (list-length testsamples) 1) 
      do (print (nth x testsamples)))


(terpri)
(print "fitness test")
(print (fitness '(+ 1 X) '(1 2 3 20))) 






