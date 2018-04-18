(defun col-extend-matrix (a k u)
  (let ((m (array-dimension a 0))
        (n (array-dimension a 1)))
    (let ((b (make-array (list m (1+ n)))))
      (dotimes (i m)
        (dotimes (j k)
          (setf (aref b i j) (aref a i j))))
      (loop
        :for i :below m
        :do (setf (aref b i k) (aref u i)))
      (dotimes (i m)
        (loop :for j :from k :to (- n 1) :do
          (setf (aref b i (1+ j)) (aref a i j))))
      b)))

(defun row-extend-matrix (a k v)
  (let ((m (array-dimension a 0))
        (n (array-dimension a 1)))
    (let ((b (make-array (list (1+ m) n))))
      (dotimes (i k)
        (dotimes (j n)
          (setf (aref b i j) (aref a i j))))
      (loop
        :for i :below n
        :do (setf (aref b k i) (aref v i)))
      (dotimes (i n)
        (loop :for j :from k :to (- m 1) :do
          (setf (aref b (1+ j) i) (aref a j i))))
      b)))
 
(defun print-matrix (a)
  (dotimes (i (array-dimension a 0))
    (dotimes (j (array-dimension a 1))
      (format t "~a " (aref a i j)))
    (terpri)))

(defvar mat1 (make-array '(3 3) :initial-contents '((0 1 2 ) (3 4 5) (6 7 8))))
(defvar u1 #(A B C))
(defvar v1 #(D E F G))
(defvar mat2 (make-array '(2 4) :initial-contents '((1.7 2.4 4.7 9.9) (6.9 7.4 3.8 2.3))))
(defvar u2 #(1.0 1.0 1.0 1.0))
(defvar v2 #(9.9 9.9 9.9))

(defun extend-matrix (a u v i j)
  (col-extend-matrix (row-extend-matrix a i u) j v)
)

;(print-matrix (extend-matrix mat1 u1 v1 0 0))
;(print-matrix (extend-matrix mat2 u2 v2 1 1))
