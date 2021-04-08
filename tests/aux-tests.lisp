(in-package :sxablono.tests)

(def-suite aux-suite :in principal-suite)

(in-suite aux-suite)

(test aux/foo
  "Just a placeholder."
  (is (string= (foo 5) "5 100500") "Test with default key")
  (is (string= (foo 4.1 :other -2) "4.1 -2") "Test with negative key"))
