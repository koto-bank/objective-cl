(defpackage :objective-cl
  (:use :cl)
  (:export
   :enable
   :disable))

(in-package :objective-cl)

(defun make-slot-access (symbol)
  (unless (symbolp symbol)
    (return-from make-slot-access symbol))
  (let* ((str (string symbol))
         (parsing-obj t)
         (obj "")
         (start 0)
         (slots (list)))
    (loop
      :for c :across str
      :for n :from 0
      :do (when (char= c #\.)
            (if parsing-obj
              (setf parsing-obj nil
                    obj (subseq str 0 n)
                    start (1+ n))
              (progn
                (push (subseq str start n) slots)
                (setf start (1+ n))))))
    (if parsing-obj symbol
      (progn (push (subseq str start) slots)
             (reduce
              (lambda (acc elem)
                `(slot-value ,acc ',elem))
              (reverse (mapcar 'intern slots))
              :initial-value (intern obj))))))

(defun br-reader (stream char)
  (declare (ignore char))
  (let ((*readtable* (copy-readtable)))
    (set-macro-character #\]
                         (lambda (stream char)
                           (declare (ignore stream char))))
    (let ((form (loop
                   :for form := (read stream nil nil t)
                   :while form :collect form)))
      (cond ((null form) nil)
            ((= 1 (length form)) (make-slot-access (car form)))
            (t (mapcar #'make-slot-access
                       `(,(second form) ,(first form) ,@(cddr form))))))))

(defvar *original-readtable* (copy-readtable))

(defmacro enable ()
  `(eval-when (:compile-toplevel :execute)
     (setf *original-readtable* (copy-readtable))
     (set-macro-character #\[ #'br-reader)))

(defmacro disable ()
  `(eval-when (:compile-toplevel :execute)
     (setf *readtable* *original-readtable*)))
