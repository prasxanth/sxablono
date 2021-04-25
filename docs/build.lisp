(in-package #:sxablono.docs)

(defun build ()
  "
Uses `MGL-PAX:UPDATE-ASDF-SYSTEM*` to generate `README` and `README.md` in the project root directory and `index.html` in the `docs/` sub-directory.
  "

  ;; Update READMEs
  (mgl-pax:update-asdf-system-readmes @index :sxablono)

  ;; Update html file only
  (mgl-pax:update-asdf-system-html-docs
   @index :sxablono
   :target-dir "."
   :update-css-p nil
   :pages `((:objects (,sxablono.docs:@index)))))
