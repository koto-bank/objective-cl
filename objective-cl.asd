(in-package :cl-user)

(defpackage :objective-cl-system
  (:use :cl :asdf))

(in-package :objective-cl-system)

(defsystem :objective-cl
  :license "BSD-3-Clause"
  :components
  ((:file "objective-cl")))
