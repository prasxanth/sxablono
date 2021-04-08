;;;; sxablono.asd

;; (ql:quickload "sxablono")
(asdf:defsystem "sxablono"
  :class :package-inferred-system
  :depends-on ("mgl-pax")
  :author "Your Name <your.name@example.com>"
  :license  "Unlicense"
  :version "0.1"
  :description "Skeleton for projects"
  :serial t
  :class :package-inferred-system
  :pathname "src/"
  :components ((:file "package")
	       (:file "utils" :depends-on ("package"))
	       (:file "aux" :depends-on ("package" "utils")))
  :in-order-to ((test-op (test-op "sxablono/tests"))))


(asdf:defsystem "sxablono/tests"

  :depends-on ("sxablono" "fiveam")
  :pathname "tests/"
  :serial t
  :components ((:file "package")
	       (:file "tests-suite" :depends-on ("package"))
	       (:file "aux-tests" :depends-on ("package" "tests-suite"))
	       (:file "utils-tests" :depends-on ("package" "tests-suite")))
  :perform (test-op (o c) (symbol-call :sxablono.tests
				       :run-principal-suite)))

;; (ql:quickload "sxablono/docs")
(asdf:defsystem "sxablono/docs"
  :class :package-inferred-system
  :depends-on ("sxablono" "iterate")
  :pathname "docs/"
  :serial t
  :components ((:file "package")
	       (:file "docs")
	       (:static-file "styles.css")
	       (:static-file "jquery.min.js")
	       (:static-file "toc.min.js")))
