(in-package #:sxablono.docs)

(defun build ()

  ;; Update READMEs
  (mgl-pax:update-asdf-system-readmes @index :sxablono)

  ;; Update html file only
  (mgl-pax:update-asdf-system-html-docs
   @index :sxablono
   :target-dir "."
   :update-css-p nil
   :pages `((:objects (,sxablono.docs:@index)))))
