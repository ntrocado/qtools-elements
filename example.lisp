;;;; example.lisp

(define-widget main-window (QWidget)
  ())

(define-subwidget (main-window slider)
    (make-instance 'slider
		   :maximum 100.0
		   :minimum 1.0
		   :stepping 0.01
		   :default 50
		   :caption "Slider"
		   :curve :exp))

(define-subwidget (main-window layout) (q+:make-qhboxlayout main-window)
  (q+:add-widget layout slider))

(define-slot (slider value-changed) ((value double))
  (declare (connected slider (value-changed double)))
  (print value))

(defun main ()
  (with-main-window (window 'main-window)))
