(in-package :cl-user)
(ql:quickload :yason)
(ql:quickload :cl-csv)
(require :dc-dlist)
(require :dc-eclectic)
(require :bianet)

(defpackage :test-network
  (:use :cl :prove :dc-eclectic :bianet :dc-dlist :sb-thread :sb-concurrency))
(in-package :test-network)

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

(plan 2)

(subtest
 "Create simple network (2 4 2 1)"
 (reset-state)
 (let* ((topology '(2 4 2 1))
        (topology-total (reduce '+ topology))
        (expected-biases (- (length topology) 2)) ;; Add biases to hidden layers
        (total-neurons (+ topology-total expected-biases))
        (net (make-instance 't-network :name "test-1" :topology topology)))
   (let ((count (length (neurons net))))
     (is-f count total-neurons "net contains ~d neurons" total-neurons))
   (let ((biases (length (remove-if-not #'biased (neurons net)))))
     (is-f biases expected-biases 
           "net contains ~d biased neurons" expected-biases))
   (let* ((layer-lengths (loop for layer in (layers net)
                               collect (length layer))))
     (is-f (nth 0 layer-lengths) 2 "Layer 0 has 2 neurons")
     (is-f (nth 1 layer-lengths) 5 "Layer 1 has 5 neurons")
     (is-f (nth 2 layer-lengths) 3 "Layer 2 has 3 neurons")
     (is-f (nth 3 layer-lengths) 1 "Layer 3 has 1 neuron"))
   (loop for neuron in (neurons net)
         for transfer-function-name = (function-name 
                                       (transfer-function neuron))
         for transfer-derivative-name = (function-name 
                                         (transfer-derivative neuron))
         do
            (cond ((and (< (layer neuron) 3) (not (biased neuron)))
                   (is-f transfer-function-name "relu"
                         "non-output-layer neuron ~a has transfer function ~a"
                         (name neuron) "relu")
                   (is-f transfer-derivative-name "relu-derivative"
                         "...and derivative function relu-derivative"))
                  ((= (layer neuron) 3) ;; output-layer neuron
                   (is-f transfer-function-name "logistic"
                         "output-layer neuron ~a has transfer function ~a"
                         (name neuron) "logistic")
                   (is-f transfer-derivative-name "logistic-derivative"
                         "...and derivative function logistic-derivative"))
                  (t ;; Biased neuron
                   (is-f (biased neuron) t "neuron ~a is biased" (name neuron))
                   (is-f transfer-function-name "biased-transfer"
                         "neuron ~a has transfer function ~a"
                         (name neuron) "biased-transfer")
                   (is-f transfer-derivative-name "biased-derivative"
                         "...and derivative function biased-derivative"))))))

(subtest
    "Train a simple, 5-layer network with XOR"
  (let* ((net (make-instance 't-network
                             :name "bravo-test"
                             :topology '(10 16 8 8 10)
                             :thread-count 4))
         (max-iterations 5000)
         (training-set '(((0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0))
                         ((0 0 0 0 0 0 0 0 0 1) (0 0 0 0 0 0 0 0 0 1))
                         ((1 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 1))
                         ((1 0 0 0 0 0 0 0 0 1) (0 0 0 0 0 0 0 0 0 0))))
         (target-error 0.05)
         (report-frequency 100)
         (reports-stack nil)
         (update-callback (lambda (e i time) 
                            (push (list e i time) reports-stack))))
    (train net target-error max-iterations 
           training-set report-frequency update-callback)
    (let ((training-result (wait-for-training net))
          (reports (reverse reports-stack)))
      (ok (> (length reports) 2) "received reports during training")
      (ok (loop for report in (butlast reports)
                for next-report in (cdr reports)
                always (and (< (nth 1 report) (nth 1 next-report))
                            (< (nth 2 report) (nth 2 next-report))))
          "reports exhibit expected progression")
      (is (nth 0 training-result) (car (car (last reports)))
          "final network error corresponds to network error in last report")
      (ok (<= (nth 0 training-result) target-error)
          (format nil "network error (~f) is at or below target error (~f)"
                  (nth 0 training-result) target-error))
      (ok (<= (nth 2 training-result) max-iterations)
          "number of training iterations is smaller than max-iterations")
      (loop for (inputs expected-outputs) in training-set
            for outputs = (excite net inputs)
            do (ok (loop for output in outputs
                         for expected-output in expected-outputs
                         always (< (abs (- expected-output output)) 0.1))
                   (format nil "inference for (~,3f ~,3f) is correct (~,3f)"
                           (car inputs) 
                           (car (last inputs)) 
                           (car (last outputs)))))
      (ok (<= (nth 1 training-result) 10)
          (format nil "training time (~,3f seconds) is less than 10 seconds"
                  (nth 1 training-result))))
    (stop-threads net)))
    
  
           
(finalize)
