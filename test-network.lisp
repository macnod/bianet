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

(plan 1)

(subtest
 "Create simple network (2 4 2 1)"
 (reset-state)
 (let* ((topology '(2 4 2 1))
        (topology-total (reduce '+ topology))
        (expected-biases (- (length topology) 2)) ;; Add biases to hidden layers
        (total-neurons (+ topology-total expected-biases))
        (net (create-simple-network topology)))
   (let ((count (length net)))
     (is-f count total-neurons "net contains ~d neurons" total-neurons))
   (let ((biases (length (remove-if-not #'biased net))))
     (is-f biases expected-biases 
           "net contains ~d biased neurons" expected-biases))
   (let* ((layers (loop for a from 0 below (length topology)
                        collect (remove-if-not 
                                 (lambda (n) (= (layer n) a)) 
                                 net)))
          (layer-lengths (loop for layer in layers collect (length layer))))
     (is-f (nth 0 layer-lengths) 2 "Layer 0 has 2 neurons")
     (is-f (nth 1 layer-lengths) 5 "Layer 1 has 5 neurons")
     (is-f (nth 2 layer-lengths) 3 "Layer 2 has 3 neurons")
     (is-f (nth 3 layer-lengths) 1 "Layer 3 has 1 neuron"))
   (loop for neuron in net
         for transfer-function-name = (function-name (transfer-function neuron))
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

;; (subtest
;;  "Create a simple neural network to solve XOR"
;;  (reset-state)
;;  (let* ((topology '(2 4

(finalize)
