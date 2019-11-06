(in-package #:eu.turtleware.polyclot.impl)

(define-class <data-frame> () ())

(define-class <raw-data-frame> (<data-frame>)
  ((rows :initarg :rows :reader rows)
   (cols :initarg :cols :reader cols)))

(deftype %col-index% () `(or string   (integer 0)))
(deftype %row-index% () `(integer 0))
(deftype %row-spec%  () `(or %row-index% sequence))

(deftype %col-slice% () `(or (eql t)
                             (cons %col-index% %col-index%)
                             (cons (eql t) %col-index%)
                             (cons %col-index% (eql t))
                             (vector %col-index%)))

(deftype %row-slice% () `(or (eql t)
                             (cons %row-index% %row-index%)
                             (cons (eql t) %row-index%)
                             (cons %row-index% (eql t))
                             (vector %row-index%)))

(define-condition <invalid-slice> (error) ())

(define-condition <invalid-index> (error) ())
(define-condition <row-invalid-index> (<invalid-index>) ())
(define-condition <col-invalid-index> (<invalid-index>) ())
(define-condition <row-does-not-exist> (<row-invalid-index>) ())
(define-condition <col-does-not-exist> (<col-invalid-index>) ())

(define-condition <insert-error> (error) ())
(define-condition <col-name-not-unique> (<insert-error>) ())
(define-condition <row-length-mismatch> (<insert-error>) ())

(defgeneric dims (<data-frame>)
  (:method ((df <raw-data-frame>))
    (values (length (rows df))
            (length (cols df)))))

;;; Methods ROWS and COLS for <RAW-DATA-FRAME> are defined as readers
;;; in the class definition.
(defgeneric cols (<data-frame>))
(defgeneric rows (<data-frame>))

(defgeneric ref (<data-frame> %row-spec% %col-index%)
  (:method ((df <raw-data-frame>) row col)
    (assert (typep row '%row-spec%)  nil '<row-invalid-index>)
    (assert (typep col '%col-index%) nil '<col-invalid-index>)
    (when (stringp col)
      (setf col (position col (cols df) :test #'string=)))
    (if (integerp row)
        (elt (elt (rows df) row) col)
        (elt row col))))

(defgeneric sel (<data-frame> %row-slice% %col-slice%)
  (:method ((df <raw-data-frame>) row-slice col-slice)
    (when (and (eql row-slice t)
               (eql col-slice t))
      (return-from sel (copy-data-frame df)))
    (let* ((rows (rows df))
           (cols (cols df))
           (row-num (etypecase row-slice
                      ((eql t)
                       (length rows))
                      ((cons (eql t) %row-index%)
                       (1+ (cdr row-slice)))
                      ((cons %row-index% (eql t))
                       (- (length rows) (car row-slice)))
                      ((cons %row-index% %row-index%)
                       (1+ (- (cdr row-slice) (car row-slice))))
                      ((vector %row-index%)
                       (length row-slice))))
           (col-num (flet ((norm-index (col)
                             (if (integerp col)
                                 col
                                 (position col cols :test #'string=))))
                      (etypecase col-slice
                        ((eql t)
                         (length cols))
                        ((cons (eql t) %col-index%)
                         (1+ (norm-index (cdr col-slice))))
                        ((cons %col-index% (eql t))
                         (- (length cols) (norm-index (car col-slice))))
                        ((cons %col-index% %col-index%)
                         (1+ (- (norm-index (cdr col-slice)) (norm-index (car col-slice)))))
                        ((vector %col-index%)
                         (length col-slice)))))
           (new-rows (make-array row-num :adjustable t :fill-pointer t))
           (new-cols (make-array col-num :adjustable t :fill-pointer t)))
      (let ((index 0))
        (map-data-frame-cols df nil col-slice
                             (lambda (col-index col-name value)
                               (declare (ignore col-index value))
                               (setf (elt new-cols index) col-name)
                               (incf index))))
      (let ((index 0))
        (flet ((copy-row (src dst &aux (index 0))
                 (map-data-frame-cols df src col-slice
                                      (lambda (col-index col-name value)
                                        (declare (ignore col-index col-name))
                                        (setf (elt dst index) value)
                                        (incf index)))))
          (map-data-frame-rows df row-slice
                               (lambda (row-index row)
                                 (declare (ignore row-index))
                                 (let ((new-row (make-array col-num :adjustable t
                                                                    :fill-pointer t)))
                                   (copy-row row new-row)
                                   (setf (elt new-rows index) new-row)
                                   (incf index))))))
      (<raw-data-frame> :cols new-cols :rows new-rows))))

(defgeneric map-data-frame (<data-frame> %row-slice% %col-slice% function)
  (:method ((df <raw-data-frame>) row-slice col-slice fun)
    (map-data-frame-rows
     df row-slice
     (lambda (row-index row)
       (map-data-frame-cols
        df row col-slice
        (lambda (col-index col-name value)
          (funcall fun row-index row col-index col-name value)))))))

(defgeneric map-data-frame-rows (<data-frame> %row-slice% function)
  (:method ((df <raw-data-frame>) row-slice fun)
    (assert (typep row-slice '%row-slice%) nil '<row-invalid-slice>)
    (let ((rows (rows df)))
      (etypecase row-slice
        ((eql t)
         (loop for index from 0
               for row across rows
               do (funcall fun index row)))
        ((cons (eql t) %row-index%)
         (loop for index from 0 upto (cdr row-slice)
               for row across rows
               do (funcall fun index row)))
        ((cons %row-index% (eql t))
         (loop for index from (car row-slice) below (length rows)
               for row = (elt rows index)
               do (funcall fun index row)))
        ((cons %row-index% %row-index%)
         (loop for index from (car row-slice) upto (cdr row-slice)
               for row = (elt rows index)
               do (funcall fun index row)))
        ((vector %row-index%)
         (loop for index across row-slice
               for row = (elt rows index)
               do (funcall fun index row)))))))

(defgeneric map-data-frame-cols (<data-frame> %row-index% %col-slice% function)
  (:method ((df <raw-data-frame>) row col-slice fun)
    (assert (typep row '%row-spec%)        nil '<row-invalid-index>)
    (assert (typep col-slice '%col-slice%) nil '<col-invalid-slice>)
    (let ((cols (cols df)))
      (typecase row
        (integer (setf row (elt (rows df) row)))
        ;; Undocumented hack to map over col names.
        (null (setf row cols)))
      (flet ((norm-index (col)
               (if (integerp col)
                   col
                   (position col cols :test #'string=))))
        (etypecase col-slice
          ((eql t)
           (loop for index from 0
                 for name across cols
                 for value across row
                 do (funcall fun index name value)))
          ((cons (eql t) %col-index%)
           (loop with i2 = (norm-index (cdr col-slice))
                 for index from 0 upto i2
                 for name across cols
                 for value across row
                 do (funcall fun index name value)))
          ((cons %col-index% (eql t))
           (loop with i1 = (norm-index (car col-slice))
                 for index from i1 below (length cols)
                 for name = (elt cols index)
                 for value = (elt row index)
                 do (funcall fun index name value)))
          ((cons %col-index% %col-index%)
           (loop with i1 = (norm-index (car col-slice))
                 with i2 = (norm-index (cdr col-slice))
                 for index from i1 upto i2
                 for name = (elt cols index)
                 for value = (elt row index)
                 do (funcall fun index name value)))
          ((vector %col-index%)
           (loop for col-index across col-slice
                 for index = (norm-index col-index)
                 for name = (elt cols index)
                 for value = (elt row index)
                 do (funcall fun index name value))))))))

(defgeneric add-rows! (<data-frame> &rest rows)
  (:method ((df <raw-data-frame>) &rest rows)
    (loop with len = (length (cols df))
          for row in rows
          for new-row = (copy-array (coerce row 'vector)
                                    :adjustable t
                                    :fill-pointer t)
          do (assert (sequence-of-length-p new-row len)
                     nil
                     '<row-length-mismatch>)
             (vector-push-extend new-row (rows df)))))

(defgeneric add-cols! (<data-frame> &rest name-fun-pairs)
  (:method ((df <raw-data-frame>) &rest name-fun-pairs)
    (loop for (name fun) on name-fun-pairs by #'cddr
          do (assert (not (find name (cols df) :test #'string=))
                     nil
                     '<col-name-not-unique>)
             (vector-push-extend name (cols df))
             (loop for ind from 0
                   for row across (rows df)
                   for val = (funcall fun ind row)
                   do (vector-push-extend val row)))))

(defun make-data-frame (cols &rest rows)
  (let* ((new-cols (copy-array (coerce cols 'vector)
                               :adjustable t
                               :fill-pointer t))
         (new-rows (make-array (length rows)
                               :adjustable t
                               :fill-pointer t))
         (col-width (length new-cols))
         (index 0))
    (assert (every #'stringp cols) nil '<col-invalid-index>)
    (assert (= col-width (length (remove-duplicates cols :test #'string=)))
            nil
            '<col-name-not-unique>)
    (map nil (lambda (row)
               (setf row (copy-array (coerce row 'vector)
                                     :adjustable t
                                     :fill-pointer t))
               (assert (sequence-of-length-p row col-width)
                       nil
                       '<row-length-mismatch>)
               (setf (elt new-rows index) row)
               (incf index))
         rows)
    (<raw-data-frame> :cols new-cols :rows new-rows)))

(defgeneric copy-data-frame (data-frame)
  (:method ((df <data-frame>))
    (let* ((src (rows df))
           (rows (make-array (length src)
                             :adjustable t
                             :fill-pointer t))
           (cols (copy-array (cols df)))
           (index 0))
      (map nil (lambda (row)
                 (setf (aref rows index) (copy-array row))
                 (incf index))
           src)
      (<raw-data-frame> :cols cols :rows rows))))

(defgeneric join-data-frame (df1 df2 &rest args)
  (:method ((df1 <data-frame>) (df2 <data-frame>) &rest args)
    (declare (ignore df1 df2 args))
    (error "Not specified yet.")))
