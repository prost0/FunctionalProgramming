(defun freqr (L Res)
  (cond ((null L) Res)
        ((member (car L) Res :key #'car) (freqr (cdr L) Res))
        ((freqr (cdr L) (cons (list (car L) (count (car L) L)) Res)))))
 
(defun freq (L)
  (freqr L nil))

;Usage: (freq '(a b c a))
