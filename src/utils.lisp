(in-package #:sxablono)

(defsection @utils (:title "Utility Functions")
  (do-the-job function))


(defun concat (first second)
  "This function is not exported and should not be showed in the API reference."
  (format nil "~A ~A" first second))


(defun do-the-job (first second)
  "
**Synopsis** -- Function to concatenate arguments

**Input Arguments**

FIRST -- an *object*

SECOND -- an *object*

**Return Values**

RESULT -- a *string*

**Description**

The documentation layout of this function is the template to be followed when documenting any function. It is akin to the *man* pages on Linux system.

**Examples**

The intent of the documentation is to ensure that it is as descriptive as possible so the use of examples is highly encouraged.

```cl-transcript
(do-the-job :abc 1)
=> \"ABC 1\"
```
"
  (concat first second))
