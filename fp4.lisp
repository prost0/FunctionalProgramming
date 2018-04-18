(defun task (text)
  (task_r () text)
  )

(defun task_r (ltext rtext)
  (cond ((eq rtext NIL) ltext)
        ((eq (exist-plus (car rtext)) NIL)
            (task_r (append ltext (list(car rtext))) (cdr rtext)))
        (T (append (list-digit-to-minus ltext) (cons (replace-digits (car rtext) (exist-plus (car rtext))) (cdr rtext))))))

(defun exist-plus (str)
  (loop for i from 0 to (- (length str) 1) do
    (if (eq (char str i) #\+)
      (return-from exist-plus i)))
  NIL)

(defun replace-digits (str i)
             (concatenate 'string
                                             (digit-to-minus(subseq str 0 i))
                                             (subseq str i (- (length str) 1))))

(defun list-digit-to-minus (lst)
   (cond ((eq lst NIL) NIL)
        (t (cons (digit-to-minus(car lst)) (list-digit-to-minus (cdr lst)))))) 

(defun digit-to-minus(str)  ;передеются копии
  (loop for i from 0 to (- (length str) 1) do
    (if (is-digit (char str i))
      (setf (char str i) #\-)))
  str)

(defun is-digit (ch)
  (cond ((eq ch #\0) T)
        ((eq ch #\1) T)
        ((eq ch #\2) T)
        ((eq ch #\3) T)
        ((eq ch #\4) T)
        ((eq ch #\5) T)
        ((eq ch #\6) T)
        ((eq ch #\7) T)
        ((eq ch #\8) T)
        ((eq ch #\9) T)
         (T NIL)))
                    
                  
(defun count-words-starting-with-char(ch str)
	(let
		(
			(res "")
			(cur-ch nil)
			(word? T)
		)
		
		(loop for i from 0 to (- (length str) 1) do
			(setq cur-ch (char str i))
			
			(if (not (eq cur-ch ))
				(progn
					(if (and word? (char= ch cur-ch))
						(setq res (+ res 1))
					)
					
					(setq word? nil)
				)
				
				(setq word? T)
			)
		)
		
		res
	)
)
