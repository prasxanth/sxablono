(in-package #:sxablono)

(defsection @aux (:title "Auxillary Functions")
  "This documentation is for the `aux.lisp` file.

   It contains only one function definition:"

  (foo function)

  "Note that this capability to interleave file or package documentation within an overarching narrative can be used to provide context to the internal definitions.

   An example of cross-referencing `->` For `sxablono`, here FOO
   references SXABLONO:DO-THE-JOB, which is desribed in SXABLONO:@UTILS.")


(defun foo (first &key (other 100500))
  "
**Synopsis** -- Function to concatentate arguments

**Input Arguments**

FIRST -- an *object*

OTHER -- an *object*. Defaults to 100500.

**Return Values**

RESULT -- a *string*

**Description**

The documentation layout of this function is the template to be followed when documenting any function.

Example of referencing another function -- [sxablono:do-the-job][function].

**Examples**

The intent of the documentation is to ensure that it is as descriptive as possible, so the use of examples is highly encouraged. Liberal use of doctests, AKA *transcripts* in MGL-PAX terminology, as examples are strongly recommended as they are often better at conveying information. Along with tests, transcripts provide an additional layer of validation to the code.

With default value of `:OTHER`,

```cl-transcript
(foo :abc)
=> \"ABC 100500\"
```

With custom value of `:OTHER`,

```cl-transcript
(foo :abc :other 10101)
=> \"ABC 10101\"
```
"
  (sxablono:do-the-job first other))
