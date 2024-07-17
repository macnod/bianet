;; (in-package :cl-user)
(require :dc-dlist)
(require :dc-eclectic)
(require :bianet)

(defpackage :test-neuron
  (:use :cl :prove :dc-eclectic :bianet :dc-dlist :sb-thread :sb-concurrency))
(in-package :test-neuron)

(defun round-to-n-decimals (x n)
  (float (/ (truncate (* x (expt 10 n))) (expt 10 n))))

(defun round-3 (x)
  (round-to-n-decimals x 3))

(defun is-f (a b message &rest parameters)
  (is a b (apply #'format (cons nil (cons message parameters)))))

(defun is-approx (a b message &rest parameters)
  (apply #'is-f (cons (round-3 a) 
                      (cons (round-3 b) 
                            (cons message parameters)))))

(defun function-name (function)
  (let ((long-name (format nil "~a" function)))
    (string-downcase
     (subseq long-name 11 (1- (length long-name))))))

(defun check-outputs (function inputs outputs)
  (loop for input in inputs
        for output in outputs
        do (is-approx (funcall function input) 
                      output
                       "~a ~,3f -> ~,3f"
                       (function-name function) input output)))

(plan 7)

(subtest
 "Neuron and connection IDs"
 (is (next-neuron-id) 1 "next-neuron-id 1")
 (is (next-neuron-id) 2 "next-neuron-id 2")
 (is (next-cx-id) 1 "next-cx-id 1")
 (is (next-cx-id) 2 "next-cx-id 2")
 (is (next-neuron-id-peek) 3 "next-neuron-id-peek 3")
 (is (next-cx-id-peek) 3 "next-cx-id-peek 3")
 (is (next-neuron-id) 3 "next-neuron-id 3")
 (is (next-cx-id) 3 "next-cx-id 3")
 (reset-ids)
 (pass "reset ids")
 (is (next-neuron-id) 1 "next-neuron-id 1")
 (is (next-cx-id) 1 "next-cx-id 1"))

(subtest
 "Transfer functions"
 (let ((inputs '(-2 -1 -0.75 -0.5 -0.25 0 0.25 0.5 0.75 1.0 2.0)))
   (let ((outputs '(0.11920292 0.26894143 0.3208213 0.37754068
                    0.4378235 0.5 0.5621765 0.62245935 0.6791787
                    0.7310586 0.880797)))
     (check-outputs #'logistic inputs outputs))
   (let ((outputs '(-6.0 -2.0 -1.3125 -0.75 -0.3125 0.0 0.1875 0.25
                    0.1875 0.0 -2.0)))
     (check-outputs #'logistic-derivative inputs outputs))
   (let ((outputs '(0.0 0.0 0.0 0.0 0.0 0.0 0.25 0.5 0.75 1.0 2.0)))
     (check-outputs #'relu inputs outputs))
   (let ((outputs '(0.0 0.0 0.0 0.0 0.0 0.0 1.0 1.0 1.0 1.0 1.0)))
     (check-outputs #'relu-derivative inputs outputs))))

(subtest
 "Next weight"
 (let ((outputs (list -0.12500942 0.4230243 -0.21688426 -0.38199055
                      0.48883212 0.050487638 0.31733942 0.019901633
                      -0.11831629 -0.05465436)))
   (ok (loop for output in outputs always (= (next-weight) output))
       "Pseudo-random weights are as expected in the beginning")
   (ok (loop initially (reset-random-state)
             for output in outputs always (= (next-weight) output))
       "Pseudo-random weights are as expected after reset")))

(subtest
 "One neuron"
 (reset-state)
 (let* (ff-complete
        bp-complete
        (ff-callback (lambda (neuron)
                       (declare (ignore neuron))
                       (setf ff-complete t)))
        (bp-callback (lambda (neuron)
                       (declare (ignore neuron))
                       (setf bp-complete t)))
        (neuron (make-instance 't-neuron
                               :transfer-key :relu
                               :on-output-ready ff-callback
                               :on-input-ready bp-callback)))
   (is (id neuron) 1 "Neuron ID is 1")
   (ok (not (biased neuron)) "Neuron is not biased")
   (is (function-name (transfer-function neuron)) "relu"
       "Neuron transfer function is correct")
   (is (function-name (transfer-derivative neuron)) "relu-derivative"
       "Neuron transfer derivative is correct")
   (ok (mailbox-empty-p (i-mailbox neuron))
       "Neuron's i-mailbox is empty")
   (ok (mailbox-empty-p (e-mailbox neuron))
       "Neuron's e-mailbox is empty")
   (is (excite neuron 1.0) 1.0 "Neuron has been messaged to excite with 1.0")
   (ok (zerop (output neuron)) "Neuron output is zero")
   (ok (not (mailbox-empty-p (i-mailbox neuron)))
       "Neuron's i-mailbox is no longer empty")
   (ok (mailbox-empty-p (e-mailbox neuron))
       "Neuron's e-mailbox remains empty")
   (ok (zerop (process-count neuron)) "Process count is zero")
   (ok (zerop (ff-count neuron)) "ff-count is zero")
   (ok (not ff-complete) "ff-complete not set")
   (is (process neuron) 1 "Process neuron succeeds")
   (is (input-last neuron) 1.0 "Neuron input was 1.0")
   (ok (zerop (input neuron)) "Neuron input is now 0.0")
   (is (process-count neuron) 1 "Process count is 1")
   (is (output neuron) 1.0 "Nueron output is now 1.0")
   (is (ff-count neuron) 1 "ff-count is 1")
   (ok (mailbox-empty-p (i-mailbox neuron))
       "Neuron's i-mailbox is empty again")
   (ok (mailbox-empty-p (e-mailbox neuron))
       "Neuron's e-mailbox is empty")
   (ok ff-complete "ff-complete set")
   (ok (zerop (err-in neuron)) "error-in is zero")
   (ok (zerop (err neuron)) "err is zero")
   (ok (null (modulated neuron)) "modulated is zero")
   (ok (zerop (modulation-count neuron)) "modulation-count is zero")
   (ok (zerop (bp-count neuron)) "bp-count is zero")
   (ok (not bp-complete) "bp-complete not set")
   (is (modulate neuron 1.0) 1.0 "Neuron modulated")
   (ok (not bp-complete) "bp-complete still not set")
   (ok (mailbox-empty-p (i-mailbox neuron)) "i-mailbox is still empty")
   (ok (not (mailbox-empty-p (e-mailbox neuron))) "e-mailbox no longer empty")
   (ok (zerop (err-in neuron)) "error-in is still zero")
   (ok (zerop (err neuron)) "err is still zero")
   (ok (null (modulated neuron)) "modulated is still zero")
   (ok (zerop (modulation-count neuron)) "modulation-count is still zero")
   (ok (zerop (bp-count neuron)) "bp-count is still zero")
   (is (process-count neuron) 1 "Process count is still 1")
   (ok (zerop (err-in neuron)) "err-in is zero")
   (is (process neuron) 2 "Process neuron succeeds")
   (is (err-in-last neuron) 1.0 "last err-in was 1.0")
   (ok (zerop (err-in neuron)) "err-in reset to zero")
   (is (err neuron)
       (* (funcall (transfer-derivative neuron) (output neuron))
          (err-in-last neuron))
       "err computed correctly")
   (ok bp-complete "bp-complete set")
   (is (process-count neuron) 2 "Process count is now 2")
   (is (ff-count neuron) 1 "ff-count is still 1")
   (is (bp-count neuron) 1 "bp-count is now 1")))

(subtest
 "Two connected neurons"
 (reset-state)
 (let* (output-ready
        backprop-complete
        (output-ready-callback (lambda (neuron)
                                 (declare (ignore neuron))
                                 (setf output-ready t)))
        (backprop-complete-callback (lambda (neuron)
                                      (declare (ignore neuron))
                                      (setf backprop-complete t)))
        (neuron-1 (make-instance 't-neuron
                                 :transfer-key :relu
                                 :on-input-ready backprop-complete-callback))
        (neuron-2 (make-instance 't-neuron
                                 :transfer-key :relu
                                 :on-output-ready output-ready-callback))
        (neurons (list neuron-1 neuron-2))
        (cx (connect neuron-1 neuron-2 :weight 0.5)))
   (is (id neuron-1) 1 "neuron-1 id is 1")
   (is (id neuron-2) 2 "neuron-2 id is 2")
   (is-approx (weight cx) 0.5
              "cx weight is correct: ~f" (weight cx))
   (is (id (source cx)) 1 "cx source is neuron-1")
   (is (id (target cx)) 2 "cx target is neuron-2")
   (ok (zerop (output neuron-2)) "neuron-2 output is zero")
   (diag "feedforward")
   (is (excite neuron-1 1.0) 1.0 "excited neuron-1 with 1.0")
   (loop for neuron in neurons do
     (dlog "Processing neuron-~d" (id neuron))
     (process neuron))
   (pass "processed neurons")
   (ok output-ready "output of neuron-2 is ready")
   (ok (not backprop-complete) "backprop is not complete")
   (is-approx (output neuron-2) 0.5
              "output of neuron-2 is correct: ~f" 
              (output neuron-2))
   (diag "backprop")
   (is (modulate neuron-2 -0.5) -0.5 "modulated neuron-2 with -0.5")
   (ok (not backprop-complete) "backprop is still not complete")   
   (loop for neuron in (reverse neurons) do
     (dlog "Processing neuron-~d" (id neuron))
     (process neuron))
   (pass "processed neurons in reverse")
   (ok backprop-complete "backprop is complete")
   (is-approx (err neuron-1) -0.25 
              "error of neuron-1 is correct: ~,3f" (err neuron-1))
   (is-approx (weight cx) 0.45 
              "weight updated ~f -> ~f" (weight-last cx) (weight cx))
   (diag "feedforward")
   (is-approx (excite neuron-1 0.999) 0.999 "excited neuron-1 with 0.999")
   (loop for neuron in neurons do
     (dlog "Processing neuron-~d" (id neuron))
     (process neuron))
   (pass "processed neurons")
   (is-approx (output neuron-2) 0.44955
              "output of neuron-2 is correct: ~f" (output neuron-2))
   (diag "backprop")
   (is-approx (modulate neuron-2 -0.44955) -0.44955 
              "modulated neuron-2 with 0.44955")
   (loop for neuron in (reverse neurons) do
     (dlog "Processing neuron-~d" (id neuron))
     (process neuron))
   (is-approx (weight cx) 0.39008993
              "weight updated ~f -> ~f" (weight-last cx) (weight cx))))

(subtest
 "Two connected neurons, thread pool with 2 threads"
 (let* (ff-complete
        bp-complete
        (ff-complete-callback (lambda (neuron)
                                (declare (ignore neuron))
                                (setf ff-complete t)))
        (bp-complete-callback (lambda (neuron)
                                (declare (ignore neuron))
                                (setf bp-complete t)))
        (neuron-1 (make-instance 't-neuron
                                 :transfer-key :relu
                                 :on-input-ready bp-complete-callback))
        (neuron-2 (make-instance 't-neuron
                                 :transfer-key :relu
                                 :on-output-ready ff-complete-callback))
        (neurons (list neuron-1 neuron-2))
        (cx (connect neuron-1 neuron-2 :weight 0.5))
        (thread-pool (loop for neuron in neurons 
                           collect
                           (let ((n neuron))
                             (make-thread (lambda ()
                                            (loop while (running n) do
                                              (process n))))))))
   (is-approx (input neuron-1) 0.0 "Input of neuron-1 is zero")
   (is-approx (output neuron-2) 0.0 "Output of neuron-2 is zero")
   (is-approx (weight cx) 0.5 "Weight of cx is 0.5")
   (diag "feedforward")
   (setf ff-complete nil)
   (is (excite neuron-1 1.0) 1.0 "excited neuron-1 with 1.0")
   (loop until ff-complete)
   (is-approx (output neuron-2) 0.5 
              "output of neuron-2 is ~f" (output neuron-2))
   (setf (running neuron-1) nil)
   (setf (running neuron-2) nil)
   (loop for thread in thread-pool do (join-thread thread))))

(subtest
 "Connect, disconnect, isolate"
 (let ((neurons (loop with topology = '(2 2 2)
                      for count in topology
                      for layer = 0 then (1+ layer)
                      appending 
                      (loop for a from 1 to count
                            collect (make-instance 't-neuron 
                                                   :transfer-key :relu
                                                   :layer layer)))))
   (is (length neurons) 6 "Created 6 neurons in 3 equal layers")
   (loop for layer from 0 to 1
         for next-layer from 1 to 2
         do (loop for source in (remove-if-not 
                                 (lambda (n) (= (layer n) layer))
                                 neurons)
                  do (loop for target in (remove-if-not
                                          (lambda (n) (= (layer n) next-layer))
                                          neurons)
                           do (connect source target :weight (next-weight)))))
   (loop for neuron in neurons
         for incoming = (list-incoming neuron)
         for outgoing = (list-outgoing neuron)
         for incoming-count = (length incoming)
         for outgoing-count = (length outgoing)
         for expected-incoming-count = (if (zerop (layer neuron)) 0 2)
         for expected-outgoing-count = (if (< (layer neuron) 2) 2 0)
         do (is-f incoming-count expected-incoming-count
                  "neuron ~a has ~d incoming connections (~{~a~^, ~})"
                  (name neuron)
                  expected-incoming-count
                  (mapcar #'id incoming))
            (is-f outgoing-count expected-outgoing-count
                  "neuron ~a has ~d outgoing connections (~{~a~^, ~})"
                  (name neuron) 
                  expected-outgoing-count
                  (mapcar #'id outgoing)))
   (loop with neuron = (nth 2 neurons)
         for cx in (list-incoming neuron)
         do (is-f (layer (source cx)) (1- (layer neuron))
                  "neuron ~a has a connection from ~a (~a)"
                  (name neuron) (name (source cx)) (id cx)))
   (loop with neuron = (nth 2 neurons)
         for cx in (list-outgoing neuron)
         do (is-f (layer (target cx)) (1+ (layer neuron))
                  "neuron ~a has a connection to ~a (~a)"
                  (name neuron) (name (target cx)) (id cx)))
   (let* ((neuron (nth 2 neurons))
          (cxs (append (list-incoming neuron) (list-outgoing neuron)))
          (upstream-neuron (source (car cxs)))
          (downstream-neuron (target (car (last cxs))))
          (count (length cxs)))
     (is-f count 4 "neuron ~a has 4 connections (~{~a~^, ~})"
           (name neuron)
           (mapcar #'id cxs))
     (is-f (id (disconnect upstream-neuron neuron)) 3
           "disconnected neuron ~a from upstream neuron ~a (cx 3)"
           (name neuron)
           (name upstream-neuron))
     (setf cxs (append (list-incoming neuron) (list-outgoing neuron)))
     (is-f (length cxs) (1- count)
           "neuron ~a now has ~d connections"
           (name neuron) (1- count))
     (is-f (id (disconnect neuron downstream-neuron)) 8
           "disconnected neuron ~a from downstream neuron ~a (cx 8)"
           (name neuron)
           (name downstream-neuron))
     (setf cxs (append (list-incoming neuron) (list-outgoing neuron)))
     (is-f (length cxs) (- count 2)
           "neuron ~a now has ~d connections"
           (name neuron) (- count 2))
     (is-f (isolate neuron) (list :incoming 1 :outgoing 1)
           "isolated neuron ~a" (name neuron))
     (setf cxs (append (list-incoming neuron) (list-outgoing neuron)))
     (is-f (length (append (list-incoming neuron) (list-outgoing neuron))) 0
           "neuron ~a now has zero connections" (name neuron)))
   (let ((neuron (nth 4 neurons)))
     (is-f (isolate neuron) (list :incoming 1 :outgoing 0)
           "isolated neuron ~a, removing 1 remaining incoming connection"
           (name neuron))
     (is-f (length (append (list-incoming neuron) (list-outgoing neuron))) 0
           "neuron ~a now has zero connections" (name neuron)))))

(finalize)
