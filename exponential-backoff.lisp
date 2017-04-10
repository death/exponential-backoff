;;;; +----------------------------------------------------------------+
;;;; | Exponential backoff                                            |
;;;; +----------------------------------------------------------------+

(defpackage #:exponential-backoff
  (:use #:cl)
  (:import-from #:monotonic-clock #:monotonic-now/ms)
  (:export #:exponential-backoff
           #:inform
           #:reset
           #:time-until-release
           #:with-exponential-backoff))

(in-package #:exponential-backoff)

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
                 (setf release-time (max (+ (monotonic-now/ms) delay) release-time))
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
  (max 0 (- (release-time backoff) (monotonic-now/ms))))

(defun call-with-exponential-backoff (fn backoff &key (sleep-function #'sleep) max-retries reset)
  (let ((retries 0))
    (when reset
      (reset backoff))
    (loop
     (handler-case
         (return-from call-with-exponential-backoff
           (multiple-value-prog1 (funcall fn)
             (inform backoff t)))
       (error (e)
         (inform backoff nil)
         (incf retries)
         (when (and max-retries (> retries max-retries))
           (error e))
         (funcall sleep-function (/ (time-until-release backoff) 1000.0)))))))

(defmacro with-exponential-backoff ((backoff &rest options &key sleep-function max-retries reset) &body forms)
  (declare (ignore sleep-function max-retries reset))
  (when (and (consp backoff) (keywordp (car backoff)))
    (setf backoff `(exponential-backoff ,@backoff)))
  `(call-with-exponential-backoff (lambda () ,@forms)
                                  ,backoff
                                  ,@options))
