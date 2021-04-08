(defpackage #:sxablono
  (:nicknames #:sxbl)
  (:use #:cl)
  (:import-from #:mgl-pax
                #:defsection)
  (:export #:do-the-job
	   #:foo)
  (:documentation "This is docstring for the package.

                   The package contains a function which does it's job by
                   applying transformation to the first and second arguments.

                   Note, despite the docstring indentation, it is displayed
                   correctly. And PAX is smart enough to distinguish the common
                   indentation and indentation of a code block below:

                       (apply #'foo 42)
                  "))
