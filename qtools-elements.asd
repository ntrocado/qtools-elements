;;;; qtools-elements.asd

(asdf:defsystem #:qtools-elements
  :description "Qtools widgets"
  :author "Nuno Trocado"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:qtools
	       #:qtcore
	       #:qtgui)
  :components ((:file "package")
               (:file "qtools-elements")
	       (:file "slider")))
