(in-package #:sxablono.docs)

(defun build ()
  "
**Synopsis** -- Generate MGL-PAX documentation from docstrings and documentation files

**Input Arguments**

No input arguments.

**Return Values**

No return values.

**Description**

Uses `MGL-PAX:UPDATE-ASDF-SYSTEM*` to generate,

* `README` and `README.md` in the project root directory
* `index.html` in the `docs/` sub-directory.

**Examples**

```
(ql:quickload :sxablono/docs)
(sxdocs:build)
```
  "

  ;; Update READMEs
  (mgl-pax:update-asdf-system-readmes @index :sxablono)

  ;; Update html file only
  (mgl-pax:update-asdf-system-html-docs
   @index :sxablono
   :target-dir "."
   :update-css-p nil
   :pages `((:objects (,sxablono.docs:@index)))))
