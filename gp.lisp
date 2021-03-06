; Matthew Kang
; matthesk@fullerton.edu
; CPSC 481
; Project 2: GP


(defvar X)    ; Global variables x, y, z
(defvar Y)
(defvar Z)
(defvar pool '())   ; Pool to hold expressions
(defvar nextpool '()) ; Pool to hold next generation
(defvar mostfit '(() 1000))  ; Variable to hold most fit expr of the gen
(defvar g_count 0)    ; Generation count
(defvar best_fit '()) ; Holds best of each generation

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
  (let ((X (car rvars))     ; set X, Y, Z to the values in rvars
        (Y (nth 1 rvars))
        (Z (nth 2 rvars)))
    (eval rexpr)))          ; Run expr with set values



; Function that takes an expression and test sample
; Returns a list of (rexpr fitness)
(defun fitness (rexpr rsamples)
  (let* ((retfit 0)        ; Fitness value to return later
         (rvars)           ; List of X, Y, Z initialized with sample values below
         (reval)           ; The evaluation of the function
         (delta))          ; Difference between actual answer and evaluation
    (loop for sample in rsamples                       ; For each sample in the sample list
          do (setq rvars (list (nth 0 sample)          ; Set X, Y, Z
                               (nth 1 sample)
                               (nth 2 sample)))
             (setq reval (runexpr rvars rexpr))        ; Evaluate with set values
             (setq delta (- (nth 3 sample) reval))     ; Find delta
          (setq retfit (+ (abs delta) retfit)))        ; Add the absolute value of delta to existing retfit
    (list rexpr retfit)))                              ; Return expr and its fitness



; Function that will see if the current expression and fitness
; is lower than the current most fit expression
(defun checkfit (fitlist mostfit)
  (if (< (nth 1 fitlist) (nth 1 mostfit))  ; Compare the fitnesses of current expression and
      T                                    ; most recent fittest expression
      NIL))


	  
; Function to take 2 expressions and cross them over at a random point
(defun crossover (rexpr1 rexpr2)
  (let ((randindex (random 3))
        (cross1 '())
        (cross2 '()))
    (loop for x from 0 to randindex
          do (setq cross1 (append cross1 (list (nth x rexpr2))))
             (setq cross2 (append cross2 (list (nth x rexpr1)))))
    (loop for x from (+ randindex 1) to 2
          do (setq cross1 (append cross1 (list (nth x rexpr1))))
             (setq cross2 (append cross2 (list (nth x rexpr2)))))
    (list cross1 cross2)))
	

; Fill pool with 50 expressions

(loop for x from 1 to 50              ; For 50 generations
      do (loop
           (if (> 50 (list-length pool))     ; Fill the pool with 50 expressions
               (setq pool (append (list (randexpr)) pool))
               (when (>= (list-length pool) 50)
                 (return pool))))
         (setq g_count (+ g_count 1))        ; Bump up the generation count
         (format t "Generation: ~D~%" g_count)
         (loop for x from 0 to (- (list-length pool) 1)
               do (format t "~D: EXPR: ~S , FITNESS: ~D~%"       ; Output the fitness for the expressions
                          x
                          (nth 0 (fitness (nth x pool) testsamples))
                          (nth 1 (fitness (nth x pool) testsamples)))
                  (if (checkfit (fitness (nth x pool) testsamples) mostfit)     ; Update most fit expression if necessary
                      (setq mostfit (list (nth 0 (fitness (nth x pool) testsamples))
                                          (nth 1 (fitness (nth x pool) testsamples))))))
         (format t "MOST FIT:~D~%" mostfit)
         (setq best_fit (append (list (list g_count mostfit) ) best_fit))  ; Save a copy of the most fit expression
         (loop while (not (equal pool nil))
               do (let ((par_ind1 (random (list-length pool)))       ; Create crossed kids with 2 parents
                        (par_ind2 (random (list-length pool)))
                        (crossed '())
                        (par1 '())
                        (par2 '()))
                    (setq par1 (nth par_ind1 pool))                  
                    (setq par2 (nth par_ind2 pool))
                    (format t "ind1: ~D , ind2: ~D~%" par1 par2)
                    (setq crossed (crossover par1 par2))
                    (format t "crossed1: ~D , crossed2: ~D~%" (nth 0 crossed) (nth 1 crossed))
                    (setq nextpool (append nextpool (list (nth 0 crossed))))      ; Add kids to the next pool
                    (setq nextpool (append nextpool (list (nth 1 crossed))))
                    (setq pool (remove par1 pool))             ; Remove parents from current pool
                    (setq pool (remove par2 pool))))
         (setq pool nextpool)     ; Bump up the pools
         (setq nextpool nil))
(format t "BEST OF EACH GENERATION: ~S:~%" best_fit)




