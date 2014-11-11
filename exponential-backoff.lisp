;;;; +----------------------------------------------------------------+
;;;; | Exponential backoff                                            |
;;;; +----------------------------------------------------------------+

(defpackage #:exponential-backoff
  (:use #:cl)
  (:export #:exponential-backoff
           #:inform
           #:reset
           #:time-until-release))

(in-package #:exponential-backoff)

(defun now ()
  (* (/ (get-internal-real-time) internal-time-units-per-second) 1000))

(defun exponential-backoff (&key (num-failures-to-ignore 0)
                                 (initial-delay-ms 0)
                                 (multiply-factor 2)
                                 (jitter-factor nil)
                                 (max-backoff-ms nil)
                                 (always-use-initial-delay nil))
  (let ((release-time 0)
        (num-failures 0))

    (labels ((inform (succeeded)
               (let ((delay (cond ((not succeeded)
                                   (incf num-failures)
                                   (calc-delay))
                                  (t
                                   (when (plusp num-failures)
                                     (decf num-failures))
                                   (if always-use-initial-delay
                                       initial-delay-ms
                                       0)))))
                 (setf release-time (max (+ (now) delay) release-time))
                 (values)))

             (reset ()
               (setf release-time 0)
               (setf num-failures 0))

             (calc-delay ()
               (let ((e-num-failures (max 0 (- num-failures num-failures-to-ignore))))
                 (when always-use-initial-delay
                   (incf e-num-failures))
                 (let ((delay (if (zerop e-num-failures)
                                  0
                                  (* initial-delay-ms
                                     (expt multiply-factor (1- e-num-failures))
                                     (- 1.0 (if jitter-factor (random jitter-factor) 0.0))))))
                   (min delay (or max-backoff-ms delay)))))

             (handle (op)
               (ecase op
                 (:inform #'inform)
                 (:reset #'reset)
                 (:release-time release-time))))

      #'handle)))

(defun inform (backoff succeeded)
  (funcall (funcall backoff :inform) succeeded))

(defun reset (backoff)
  (funcall (funcall backoff :reset)))

(defun release-time (backoff)
  (funcall backoff :release-time))

(defun time-until-release (backoff)
  (max 0 (- (release-time backoff) (now))))
