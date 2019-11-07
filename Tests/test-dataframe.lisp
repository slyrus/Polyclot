(in-package #:eu.turtleware.polyclot/tests)

(def-suite* polyclot.data-frame
  :in polyclot
  :description "Data frame protocol.")

(defconstant +max-cols+ 16)
(defconstant +max-rows+ 32)

(defun gen-valid-cols (length)
  (let ((cols (loop for i from 0 below length
                    collect (format nil "col-~a" i))))
    (lambda ()
      ;; we may use any sequence to specify cols
      (coerce cols (whichever 'vector 'list)))))

(defun gen-valid-row (length)
  (let ((gen-elt (gen-one-element 1 "a" '(1) #\d #(1) #*101 3.14))
        (gen-str (gen-string :length (constantly length))))
    (lambda ()
      (case (random 4)
        ;; rows specified with a list
        (0 (loop for i from 0 below length
                 collect (funcall gen-elt)))
        ;; rows specified with a vector
        (1 (coerce (loop for i from 0 below length
                         collect (funcall gen-elt))
                   'vector))
        ;; rows specified with a string
        (2 (funcall gen-str))
        ;; rows specified with a bit vector
        (3 (make-array length :element-type 'bit :initial-element 0))))))

(defun gen-valid-rows (how-many length)
  (let ((gen-row (gen-valid-row length)))
    (lambda ()
      (loop repeat how-many
            collect (funcall gen-row)))))

(defun gen-invalid-length (length)
  (cond ((zerop length)
         (gen-integer :min 1 :max +max-cols+))
        ((= length +max-cols+)
         (gen-integer :min 0 :max (1- length)))
        (t
         (lambda ()
           (let ((int (funcall (gen-integer :min 0 :max (1- length)))))
             (if (< int length)
                 int
                 (1+ int)))))))

(defun gen-invalid-cols (length)
  (let ((gen-invalid-length (gen-invalid-length length))
        (gen-invalid-value (gen-one-element 1 'column #(1 2) #(#\a #\b))))
    (lambda ()
      (case (cond ((zerop length) (random 1))
                  ((< length 2)   (random 2))
                  (t              (random 3)))
        ;; too many or to little columns.
        (0 (funcall (gen-valid-cols (funcall gen-invalid-length))))
        ;; non-string column name
        (1 (loop with invalid-index = (random length)
                 for i from 0 below length
                 if (= i invalid-index)
                   collect (funcall gen-invalid-value)
                 else
                   collect (format nil "col-~a" i)))
        ;; ;; duplicated column name
        (2 (loop with dup-src = (random length)
                 with dup-dst = (let ((dup (random length)))
                                  (if (/= dup dup-src)
                                      dup
                                      (if (zerop dup)
                                          (1+ dup)
                                          (1- dup))))
                 for i from 0 below length
                 if (= i dup-dst)
                   collect (format nil "col-~a" dup-src)
                 else
                   collect (format nil "col-~a" i)))))))

(defun gen-invalid-rows (how-many length)
  (let ((gen-invalid-length (gen-invalid-length length)))
    (gen-valid-rows how-many (funcall gen-invalid-length))))

(defun gen-data-frame (how-many length)
  (let ((gen-cols (gen-valid-cols length))
        (gen-rows (gen-valid-rows how-many length)))
    (lambda ()
      (apply #'make-data-frame
             (funcall gen-cols)
             (funcall gen-rows)))))

(defun df-equal (df1 df2)
  (and (equal (multiple-value-list (dims df1))
              (multiple-value-list (dims df2)))
       (equalp (cols df1) (cols df2))
       (prog1 t
         (map nil (lambda (row1 row2)
                    (unless (every #'equalp row1 row2)
                      (return-from df-equal nil)))
              (rows df1) (rows df2)))))

(defun gen-valid-row-index (data-frame)
  (lambda ()
    (random (dims data-frame))))

(defun gen-valid-col-index (data-frame)
  (lambda ()
    (let ((index (random (nth-value 1 (dims data-frame)))))
     (case (random 2)
       (0 index)
       (1 (elt (cols data-frame) index))))))

(defun gen-valid-row-slice (data-frame)
  (let ((gen-index (gen-valid-row-index data-frame)))
   (lambda ()
     (case (random 5)
       (0 t)
       (1 (cons t (funcall gen-index)))
       (2 (cons (funcall gen-index) t))
       (3 (let ((i1 (funcall gen-index))
                (i2 (funcall gen-index)))
            (cons (min i1 i2) (max i1 i2))))
       (4 (let* ((length (random (dims data-frame)))
                 (array (make-array length)))
            (loop for i from 0 below length
                  do (setf (elt array i)
                           (funcall gen-index))
                  finally (return array))))))))

(defun gen-valid-col-slice (data-frame)
  (let ((gen-index (gen-valid-col-index data-frame))
        (cols (cols data-frame)))
   (lambda ()
     (case (random 5)
       (0 t)
       (1 (cons t (funcall gen-index)))
       (2 (cons (funcall gen-index) t))
       (3 (let ((i1 (funcall gen-index))
                (i2 (funcall gen-index)))
            (cond ((and (integerp i1)
                        (integerp i2))
                   (cons (min i1 i2) (max i1 i2)))
                  ((integerp i1)
                   (if (< i1 (position i2 cols :test #'string=))
                       (cons i1 i2)
                       (cons i2 i1)))
                  ((integerp i2)
                   (if (> i2 (position i1 cols :test #'string=))
                       (cons i1 i2)
                       (cons i2 i1)))
                  (t
                   (if (< (position i1 cols :test #'string=)
                          (position i2 cols :test #'string=))
                       (cons i1 i2)
                       (cons i2 i1))))))
       (4 (let* ((length (random (dims data-frame)))
                 (array (make-array length)))
            (loop for i from 0 below length
                  do (setf (elt array i)
                           (funcall gen-index))
                  finally (return array))))))))


(def-test make-data-frame ()
  (for-all ((width  (gen-integer :min 0 :max +max-cols+))
            (height (gen-integer :min 0 :max +max-rows+)))
    (let ((col-spec (funcall (gen-valid-cols width)))
          (row-spec (funcall (gen-valid-rows height width))))
      (is (typep (apply #'make-data-frame col-spec row-spec) <data-frame>)))))

(def-test make-data-frame.invalid ()
  (for-all ((width  (gen-integer :min 0 :max +max-cols+))
            (height (gen-integer :min 1 :max +max-rows+)))
    (let ((invalid-col-spec (funcall (gen-invalid-cols width)))
          (invalid-row-spec (funcall (gen-invalid-rows height width)))
          (valid-col-spec   (funcall (gen-valid-cols width)))
          (valid-row-spec   (funcall (gen-valid-rows height width))))
      (signals error
        (apply #'make-data-frame invalid-col-spec valid-row-spec))
      (signals error
        (apply #'make-data-frame valid-col-spec invalid-row-spec)))))

(def-test copy-data-frame ()
  (for-all ((width (gen-integer :min 0 :max +max-cols+))
            (height (gen-integer :min 0 :max +max-rows+)))
   (let ((df (funcall (gen-data-frame height width))))
     (is (df-equal df (copy-data-frame df))))))

(def-test join-data-frame ()
  (fail "Not implemented yet."))
