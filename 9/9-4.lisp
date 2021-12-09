(defun y-intercept (x y k)
  (if (numberp k) (- y (* k x)) 0))

(defun slope (x1 y1 x2 y2)
  (cond ((and (eql x1 x2) (eql y1 y2)) 'zero)
        ((eql x1 x2) 'infinity)
        (t (/ (- y2 y1) (- x2 x1)))))

(defun subtract-points (x1 y1 x2 y2)
  (list (- x2 x1) (- y2 y1)))

(defun add-pairs (p1 p2)
  (list (+ (car p1) (car p2)) (+ (cadr p1) (cadr p2))))

(defun scalar-prod (c v)
  (list (* c (car v)) (* c (cadr v))))

(defun cross-prod (v1 v2)
  (- (* (car v1) (cadr v2)) (* (car v2) (cadr v1))))

(defun parallel-segments (x1 y1 x2 y2 x3 y3 x4 y4)
  (let ((k1 (slope x1 y1 x2 y2)) (k2 (slope x3 y3 x4 y4)))
    (cond ((not (eql (y-intercept x1 y1 k1) (y-intercept x3 y3 k2))) nil)
          ((and (>= x4 x2 x3) (null k1) k2) (list x2 y2 x2 y2))
          ((and (> x1 x3) (< x2 x4)) (list x1 y1 x2 y2))
          ((and (> x3 x1) (< x4 x2)) (list x3 y3 x4 y4))
          ((>= x4 x2 x3) (list x3 y3 x2 y2))
          ((>= x4 x1 x3) (list x1 y1 x4 y4))
          (t nil))))

(defun order-points (x1 y1 x2 y2 x3 y3 x4 y4)
  (cond ((and (<= x1 x2) (<= x3 x4)) (values x1 y1 x2 y2 x3 y3 x4 y4))
        ((<= x1 x2) (values x1 y1 x2 y2 x4 y4 x3 y3))
        ((<= x2 x4) (values x2 y2 x1 y1 x3 y3 x4 y4))
        (t (values x2 y2 x1 y1 x4 y4 x3 y3))))

(defun parallel-p (x1 y1 x2 y2 x3 y3 x4 y4)
  (let ((res (multiple-value-call #'parallel-segments (order-points x1 y1 x2 y2 x3 y3 x4 y4))))
    (cond ((null res) nil)
          ((and (null (slope x1 y1 x2 y2)) (not (eql (slope x3 y3 x4 y4) 'infinity)))
           (values (car res) (cadr res) (car res) (cadr res)))
          (t (values-list res)))))

(defun intersection-p (x x1 x2 x3 x4)
  (and (>= x (min x1 x2)) (>= x (min x3 x4)) (<= x (max x1 x2)) (<= x (max x3 x4))))

(defun intersect-p (x1 y1 x2 x3 x4 p p1 p2)
  (let ((res (add-pairs (scalar-prod (/ (cross-prod p p2) (cross-prod p1 p2)) p1)
                       (list x1 y1))))
    (if (intersection-p (car res) x1 x2 x3 x4)
      (values-list res)
      nil)))

(defun intersect-segments (x1 y1 x2 y2 x3 y3 x4 y4)
  (let* ((p1 (subtract-points x1 y1 x2 y2))
         (p2 (subtract-points x3 y3 x4 y4)))
    (if (= (cross-prod p1 p2) 0)
      (parallel-p x1 y1 x2 y2 x3 y3 x4 y4)
      (intersect-p x1 y1 x2 x3 x4 (subtract-points x1 y1 x3 y3) p1 p2))))
