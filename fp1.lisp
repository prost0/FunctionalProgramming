;Factorial
(defun fact (x)
  (labels ((iter (result counter)
       (if (= counter 0)
           result
           (iter (* result counter) (1- counter)))))
    (iter 1 x)))

;Iterative sum 
(defun sum(f a b)
  (labels ((iter (a result)
         (if (> a b)
         result
         (iter  (1+ a) (+ result (funcall f a))))))
    (iter a 0)))


;Usage
;(sum (function fact) 3 5)
;(sum (function idenity) 2 4)
