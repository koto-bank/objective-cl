Objective-CL
----

CL syntax extension to make working with objects easier.
It introduces the `[]` syntax, which does two things:

1. In method calls, object and method are swapped, so instead of
   passing the object as the first argument, you instead pass method
   to the object. Remaining method arguments follow the method.

2. Inside `[]` object slots can be accessed by prefixing them with
   `.`.  E.g., `my-object.some-slot` expands to `(slot-value my-object
   'some-slot)`.

A demonstration:
```lisp
(defclass vec ()
  ((x :initarg :x)
   (y :initarg :y)
   (z :initarg :z)))

(defun make-vec (x y z)
  (make-instance 'vec :x x :y y :z z))
```


Then, in the REPL:
```lisp
CL-USER> (objective-cl:enable)
T
CL-USER> (defgeneric len (o))
#<STANDARD-GENERIC-FUNCTION COMMON-LISP-USER::LEN (0)>
CL-USER> (defmethod len ((v vec))
           (sqrt (+ [v.x expt 2]
                    [v.y expt 2]
                    [v.z expt 2])))
#<STANDARD-METHOD COMMON-LISP-USER::LEN (VEC) {1004820213}>
CL-USER> (len (make-vec 3 4 0))
5.0
CL-USER> [(make-vec 1 2 3) len] ;;; Same as above, with different syntax
3.7416575
CL-USER> ;;; You can also use [obj.slot] just to extract slot-value from an object:
(defmethod print-object ((v vec) stream)
  (format stream "~A ~A ~A" [v.x] [v.y] [v.z]))
#<STANDARD-METHOD COMMON-LISP:PRINT-OBJECT (VEC T) {1001A97F83}>
CL-USER> (print (make-vec 2 3 4))

2 3 4
2 3 4
```


Of course, this syntax can be used for anything:
```lisp
CL-USER> [2 + 3]
5 (3 bits, #x5, #o5, #b101)
CL-USER> ['(1 2 3 4) length]
4 (3 bits, #x4, #o4, #b100)
CL-USER> [add-nums defun (a b c) (+ a b c)]
ADD-NUMS
CL-USER> (add-nums 1 2 3)
6 (3 bits, #x6, #o6, #b110)
```

Packages may be specified as usual for both slots and objects:
```lisp
CL-USER> (defpackage :bar (:use :cl)
                     (:export :some-slot))
#<PACKAGE "BAR">
CL-USER> (in-package :bar)
#<PACKAGE "BAR">
BAR> (defclass some-class () (some-slot unexported-slot))
#<STANDARD-CLASS BAR:SOME-CLASS>
BAR> (defvar *an-instance* (make-instance 'some-class))
*AN-INSTANCE*
BAR> (in-package :cl-user)
#<PACKAGE "COMMON-LISP-USER">
CL-USER> [bar::*an-instance*.bar:some-slot setf 34]
34 (6 bits, #x22, #o42, #b100010)
CL-USER> (slot-value bar::*an-instance* 'bar:some-slot)
34 (6 bits, #x22, #o42, #b100010)
CL-USER> (read-from-string "[bar::*an-instance*.bar:some-slot]")
(SLOT-VALUE BAR::*AN-INSTANCE* 'BAR:SOME-SLOT)
CL-USER> [bar::*an-instance*.bar::unexported-slot setf 12]
12 (4 bits, #xC, #o14, #b1100)
CL-USER> (slot-value bar::*an-instance* 'bar::unexported-slot)
12 (4 bits, #xC, #o14, #b1100)
```

Limitations
---
Since there's no way to tell "." in the package or variable name from the "." separating object and slot,
variables and packages with "." in their names are not supported. For packages, this can be mitigated by
using `:local-nicknames`. For slot names the usual `slot-value` must be used.

Installation
---

Copy (or symlink) to `~/quicklisp/local-projects`, then type `(ql:register-local-projects)` in the REPL.
Or just copy the file `objective-cl.lisp` straight into your project.

Usage
---

To enable the extension for all project, use `:around-compile` attribute:
```lisp
(asdf:defsystem :your-awesome-system
  :license "Your favourite license"
  :depends-on (:<...>
               :objective-cl)
  :around-compile (lambda (next)
                    ;; uiop:symbol-call is needed because objective-cl system
                    ;; is not yet loaded by the time the defsystem form is read,
                    ;; so the package also doesn't exists, which causes an error
                    (uiop:symbol-call '#:objective-cl '#:enable)
                    (unwind-protect (funcall next)
                      (uiop:symbol-call '#:objective-cl '#:disable)))
  :components
  ((:file "packages")
    <...>
```

Please use responsibly.
