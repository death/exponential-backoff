# Exponential backoff

An implementation of the [exponential backoff](http://en.wikipedia.org/wiki/Exponential_backoff) algorithm in Common Lisp.

Inspired by the implementation found in [Chromium](https://github.com/adobe/chromium/blob/master/net/base/backoff_entry.cc).
Read the [header file](https://github.com/adobe/chromium/blob/master/net/base/backoff_entry.h) to learn about each of the parameters.

# Example

```lisp
(loop with b = (exponential-backoff:exponential-backoff
                :num-failures-to-ignore 1
                :initial-delay-ms (* 5 1000)
                :jitter-factor 0.1
                :max-backoff-ms (* 5 60 1000))
      for time-to-wait = (exponential-backoff:time-until-release b)
      for will-succeed in '(t nil nil nil t nil nil t t nil nil)
      do (format t "Waiting for ~F ms before ~:[failing~;succeeding~]...~%" time-to-wait will-succeed)
      do (sleep (/ time-to-wait 1000))
      do (exponential-backoff:inform b will-succeed))

;; Output:
;;
;; Waiting for 0.0 ms before succeeding...
;; Waiting for 0.0 ms before failing...
;; Waiting for 0.0 ms before failing...
;; Waiting for 4647.0 ms before failing...
;; Waiting for 9893.5 ms before succeeding...
;; Waiting for 0.0 ms before failing...
;; Waiting for 9357.5 ms before failing...
;; Waiting for 18710.0 ms before succeeding...
;; Waiting for 0.0 ms before succeeding...
;; Waiting for 0.0 ms before failing...
;; Waiting for 9969.0 ms before failing...
```

# License

MIT
