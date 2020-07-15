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

Installation
---

Copy (or symlink) to `~/quicklisp/local-projects`, then type `(ql:register-local-projects)` in the REPL.
Or just copy the file `objective-cl.lisp` straight into your project.

Please use responsibly.
