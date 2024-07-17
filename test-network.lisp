(in-package :cl-user)
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
 "Train a simple network with XOR"
 (loop with net = (make-instance 't-network :name "test-1" :topology '(2 4 1))
       and running = t
       with thread = (make-thread
                      (lambda ()
                        (loop while running do
                          (loop for neuron in (neurons net) 
                                do (process neuron))
                          (loop for neuron in (neurons-reversed net) 
                                do (process neuron))
                          (sleep 0.1)))
                      :name "xor-network")
       and output-count = (length (output-layer net))
       and input-count = (length (input-layer net))
       and iterations = 10
       and training-set = '(((0 0) (0))
                            ((0 1) (1))
                            ((1 0) (1))
                            ((1 1) (0)))
       and target-error = 0.05
       for iteration from 1 to iterations
       do (loop for (inputs expected-outputs) in training-set
                for outputs = (excite net inputs)
                do (modulate net expected-outputs))
       finally (loop for (inputs expected-outputs) in training-set
                     for outputs = (excite net inputs)
                     for errors = (output-errors outputs expected-outputs)
                     do (is-f (< (car errors) 0.05) t
                              "(~{~a~^, ~}) -> ~{~a~^, ~}; expected=(~{~a~^, ~}); errors=(~{~a~^, ~})"
                              inputs outputs expected-outputs errors)
                     finally
                        (setf running nil)
                        (join-thread thread))))
(finalize)
