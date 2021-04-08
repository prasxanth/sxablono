(in-package :sxablono.tests)

(def-suite utils-suite :in principal-suite)

(in-suite utils-suite)

(test utils/do-the-job
  "Just a placeholder."
  (is (string= (do-the-job 5 3) "5 3") "Positive args (base case).")
  (is (string= (do-the-job 4.1 -2) "4.1 -2") "Floating point and negative args."))
