(defpackage :objective-cl
  (:use :cl)
  (:export
   :enable
   :disable
   :ocl-readtable))

(in-package :objective-cl)

(defun parse-form (subform rest parsed-form)
  (when (null subform)
    (return-from parse-form (nreverse parsed-form)))
  (if (atom subform)
    (cond ((eq 'slot-access subform)
           (let ((object (car parsed-form))
                 (slot (car rest))
                 (new-rest (cdr rest)))
             (parse-form (car new-rest) (cdr new-rest)
                         (cons (list 'slot-value object `(quote ,slot))
                               (cdr parsed-form)))))
          (t (parse-form (car rest) (cdr rest) (cons subform parsed-form))))
    ;; else: subform is a list
    (parse-form (car rest) (cdr rest)
                (cons (parse-form (car subform)
                                  (cdr subform)
                                  list)
                      parsed-form))))

(defun br-reader (stream char)
  (declare (ignore char))
  (let ((*readtable* (copy-readtable)))
    (set-macro-character #\]
                         (lambda (stream char)
                           (declare (ignore stream char))))
    (set-macro-character #\.
                         (lambda (stream char)
                           (declare (ignore stream char))
                           'slot-access))
    (let* ((raw (loop
                  for form = (read stream nil nil t)
                  while form
                  collect form))
           (form (parse-form (car raw) (cdr raw)
                             (list))))
      (cond ((null form) nil)
            ((= 1 (length form)) (car form))
            (t `(,(second form) ,(first form) ,@(cddr form)))))))

(named-readtables:defreadtable ocl-readtable
  (:merge :standard)
  (:case :upcase)
  (:macro-char #\[ #'br-reader))

(defun enable ()
  (named-readtables:in-readtable ocl-readtable))

(defun disable ()
  (named-readtables:in-readtable :standard))
