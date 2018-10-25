;;;; slider.lisp

;;; This is based on the slider from Shinmera's library:
;;; http://shinmera.github.io/qtools-ui/
;;; with the following differences: no additional button, different sizes and
;;; styling, works as a linear or exponential slider, it's not integrated in
;;; the library's layout system.


(in-package #:qtools-elements)
(named-readtables:in-readtable :qtools)

(defun lin-lin (in in-min in-max out-min out-max)
  (+ (* (/ (- in in-min) (- in-max in-min))
	(- out-max out-min))
     out-min))

(defun lin-exp (in in-min in-max out-min out-max)
  (* (expt (/ out-max out-min)
	   (/ (- in in-min)
	      (- in-max in-min)))
     out-min))

(defun exp-lin (in in-min in-max out-min out-max)
  (+ (* (/ (log (/ in in-min))
	   (log (/ in-max in-min)))
	(- out-max out-min))
     out-min))

;; (defun lin-curve (in in-min in-max out-min out-max curve)
;;   (let* ((pos (/ (- in in-min)
;; 		 (- in-max in-min)))
;; 	 (denominator (- 1.0 (exp curve)))
;; 	 (numerator (- 1.0 (exp (* pos curve)))))
;;     (+ out-min
;;        (* (- out-max out-min) (/ numerator denominator)))))

(define-widget exp-slider (QSlider)
  ((maximum :initarg :maximum :accessor maximum)
   (minimum :initarg :minimum :accessor minimum)
   (stepping :initarg :stepping :accessor stepping)
   (curve :initarg :curve :accessor curve)
   (div))
  (:default-initargs
    :maximum 100.0 :minimum 1.0 :stepping 0.1 :curve :lin))

(define-initializer (exp-slider setup)
  (setf div (let ((str (string-trim "0" (format nil "~f" stepping))))
              (expt 10 (- (length str) (position #\. str) 1))))
  (setf (q+:maximum exp-slider) (round (* div maximum)))
  (setf (q+:minimum exp-slider) (round (* div minimum)))
  (setf (q+:orientation exp-slider) (q+:qt.horizontal))
  (setf (q+:style-sheet exp-slider)
	"QSlider { min-height: 74px;
                   min-width: 400px;
                   margin: -5px 0px;
                 }
         QSlider::groove:horizontal {
                   border: 1px solid;
                   height: 5px;
                   margin: 0px 12px;
                 }
         QSlider::handle:horizontal {
                   border: 1px solid;
                   background: lightblue;
                   width: 80px;
                   height: 50px;
                   margin: -24px -12px;
                 }"))

(define-signal (exp-slider value-changed) (double))

(define-slot (exp-slider update) ((value int))
  (declare (connected exp-slider (value-changed int)))
  (signal! exp-slider (value-changed double) 
	   (let* ((val (q+:value exp-slider))
		  (args `(,val ,(* minimum div) ,(* maximum div) ,minimum ,maximum))
		  (fun (case curve (:lin #'lin-lin) (:exp #'lin-exp))))
	     (format t "~%~a-> ~a" value (apply fun args))
	     (apply fun args))))

(define-slot (exp-slider released) ()
  (declare (connected exp-slider (slider-released))))

(defmethod value ((exp-slider exp-slider))
  (with-slots (div minimum maximum curve) exp-slider
    (let* ((val (q+:value exp-slider))
	   (args `(,val ,(* minimum div) ,(* maximum div) ,minimum ,maximum))
	   (fun (case curve (:lin #'lin-lin) (:exp #'lin-exp))))
      (apply fun args))))

(defmethod (setf value) (val (exp-slider exp-slider))
  (with-slots-bound (exp-slider exp-slider)
    (unless (<= minimum val maximum)
      (error "~a is not within [~a, ~a]." val minimum maximum))
    (let ((args `(,val ,minimum ,maximum ,(* minimum div) ,(* maximum div)))
	  (fun (case curve (:lin #'lin-lin) (:exp #'exp-lin))))
      (setf (q+:value exp-slider) (round (apply fun args))))))

(defmethod (setf maximum) :after (value (exp-slider exp-slider))
  (setf (q+:maximum exp-slider) value))

(defmethod (setf minimum) :after (value (exp-slider exp-slider))
  (setf (q+:minimum exp-slider) value))

(defmethod (setf stepping) :after (value (exp-slider exp-slider))
  (setf (q+:tick-interval exp-slider)
	(round (* (slot-value exp-slider 'div) value))))

(define-widget slider (Qwidget)
  ((text :initarg :text :accessor text)
   (minimum :initarg :minimum :accessor minimum)
   (maximum :initarg :maximum :accessor maximum)
   (stepping :initarg :stepping :accessor stepping)
   (default :initarg :default :accessor default)
   (curve :initarg :curve :accessor curve)
   (caption :initarg :caption :accessor caption))
  (:default-initargs
    :minimum 1.0
    :maximum 100.0
    :stepping 0.1
    :default 1.0
    :curve :lin
    :caption "Slider"))

(define-signal (slider value-changed) (double))

(define-initializer (slider setup)
  (setf (q+:minimum-height slider) 80)
  (setf (q+:minimum-width slider) 200)
  (setf (q+:maximum-height slider) 100))

(define-subwidget (slider label) (q+:make-qlabel caption))

(define-subwidget (slider exp-slider) (make-instance 'exp-slider :maximum maximum :minimum minimum :stepping stepping :curve curve)
  (setf (value exp-slider) (or default minimum)))

(define-subwidget (slider spin-box) (q+:make-qdoublespinbox)
  (setf (q+:single-step spin-box) stepping)
  (setf (q+:maximum spin-box) maximum)
  (setf (q+:minimum spin-box) minimum)
  (setf (q+:value spin-box) (or default minimum)))

(define-subwidget (slider layout) (q+:make-qhboxlayout slider)
  (q+:add-widget layout label)
  (q+:add-widget layout exp-slider)
  (q+:add-widget layout spin-box))

(define-slot (slider update) ((value double))
  (declare (connected exp-slider (value-changed double)))
  (declare (connected spin-box (value-changed double)))
  (when (or (/= (value exp-slider) value)
            (/= (value spin-box) value))
    (setf (value slider) value)
    (signal! slider (value-changed double) value)))

(defmethod value ((slider slider))
  (q+:value (slot-value slider 'spin-box)))

(defmethod (setf value) (value (slider slider))
  (with-slots-bound (slider slider)
    (unless (<= minimum value maximum)
      (error "~a is not within [~a, ~a]." value minimum maximum))
    (setf (value spin-box) value)
    (setf (value exp-slider) value)
    ))

(defmethod (setf maximum) :after (value (slider slider))
  (with-slots-bound (slider slider)
    (setf (maximum exp-slider) value)
    (setf (q+:maximum spin-box) value)
    (when default (setf (default slider) (min value default)))
    (setf (value slider) (min (value slider) value))))

(defmethod (setf minimum) :after (value (slider slider))
  (with-slots-bound (slider slider)
    (setf (minimum exp-slider) value)
    (setf (q+:minimum spin-box) value)
    (when default (setf (default slider) (max value default)))
    (setf (value slider) (max (value slider) value))))

(defmethod (setf stepping) :after (value (slider slider))
  (with-slots-bound (slider slider)
    (setf (stepping exp-slider) value)
    (setf (q+:single-step spin-box) stepping)))
