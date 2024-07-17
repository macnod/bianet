(in-package :bianet)

(defclass t-network ()
  ((name :reader name :initarg :name :type string 
         :initform (error "name is required"))
   (topology :accessor topology :initarg :topology :type list
                     :initform (error "initial-topology is required"))
   (neurons :accessor neurons :type list :initform nil)
   (neurons-reversed :accessor neurons-reversed :type list :initform nil)
   (layers :accessor layers :type list :initform nil)
   (input-layer :accessor input-layer :type list :initform nil)
   (input-count :accessor input-count :type integer :initform 0)
   (output-layer :accessor output-layer :type list :initform nil)
   (output-count :accessor output-count :type integer :initform 0)
   (inputs-ready-count :accessor inputs-ready-count :type cons 
                       :initform (cons 0 0))
   (outputs-ready-count :accessor outputs-ready-count :type cons 
                        :initform (cons 0 0))))

(defmethod initialize-instance :after ((network t-network) &key)
  (multiple-value-bind (neurons layers input-layer output-layer)
      (make-simple-network network)
    (setf (neurons network) neurons
          (neurons-reversed network) (reverse neurons)
          (layers network) layers
          (input-layer network) input-layer
          (output-layer network) output-layer
          (input-count network) (length input-layer)
          (output-count network) (length output-layer))))

(defun compute-cx-count (layers)
  (loop for layer in (butlast layers)
        for next-layer in (cdr layers)
        for neuron-count = (length layer)
        for neuron-count-next-layer = (length next-layer)
        for cx-count = (* neuron-count neuron-count-next-layer)
          then (+ cx-count (* neuron-count neuron-count-next-layer))
        finally (return cx-count)))

(defun compute-weights-sinusoidal (cx-count)
  (loop with step = (/ pi cx-count)
        for a from 1 to cx-count
        for b = 0.0 then (+ b step)
        collect (sin b)))

(defun compute-weights-random (count)
  (loop for a from 1 to count collect (next-weight)))

(defun connect-layers (layers)
  (loop with cx-count = (compute-cx-count layers)
        with weights = (compute-weights-random cx-count)
        for layer in (butlast layers)
        for next-layer in (cdr layers)
        do (loop for source in layer
                 do (loop for target in next-layer
                          do (connect source target
                                      :weight (pop weights)
                                      :learning-rate 0.1
                                      :momentum 0.3))))
  layers)

(defmethod simple-network-layers ((network t-network))
  (loop 
    with output-ready-callback = (lambda () 
                                   (sb-ext:atomic-incf 
                                       (car (outputs-ready-count network))))
    and input-ready-callback = (lambda ()
                                 (sb-ext:atomic-incf
                                     (car (inputs-ready-count network))))
    and layer-count = (length (topology network))
    for layer-neuron-count in (topology network)
    for layer-index = 0 then (1+ layer-index)
    for is-input-layer = (zerop layer-index)
    for is-output-layer = (= layer-index (1- layer-count))
    collect
    (loop 
      with layer-size = (if (or is-input-layer is-output-layer)
                            ;; This is not a hidden layer, so use the
                            ;; specified number of neurons.
                            layer-neuron-count
                            ;; Add a bias to hidden layers
                            (1+ layer-neuron-count))
      for a from 1 to layer-size
          for transfer-key = (if is-output-layer :logistic :relu)
          for biased = (and (not is-input-layer)
                            (not is-output-layer)
                            (= a layer-size))
          collect (cond
                    (is-output-layer
                     (make-instance 't-neuron
                                    :transfer-key transfer-key
                                    :biased biased
                                    :layer layer-index
                                    :on-output-ready output-ready-callback))
                    (is-input-layer
                     (make-instance 't-neuron
                                    :transfer-key transfer-key
                                    :biased biased
                                    :layer layer-index
                                    :on-input-ready input-ready-callback))
                    (t (make-instance
                        't-neuron
                        :transfer-key transfer-key
                        :biased biased
                        :layer layer-index))))))

(defmethod make-simple-network ((network t-network))
  (let ((layers (connect-layers (simple-network-layers network))))
    (values (reduce #'append layers)
            layers
            (car layers)
            (car (last layers)))))

(defmethod excite ((network t-network) (inputs list))
  (loop 
    initially (setf (car (outputs-ready-count network)) 0)
    for input in inputs
    for neuron in (input-layer network)
    do (excite neuron input)
    finally 
       (wait-for-outputs network)
       (return (loop for neuron in (output-layer network) 
                     collect (output neuron)))))

(defmethod wait-for-outputs ((network t-network))
  (loop while (< (outputs-ready-count network) (output-count network))))

(defmethod modulate ((network t-network) (expected-outputs list))
  (loop
    initially (setf (car (inputs-ready-count network)) 0)
    for neuron in (output-layer network)
    for expected-output in expected-outputs
    for output = (output neuron)
    for error = (- expected-output output)
    do (modulate neuron error)
    finally (wait-for-inputs network)))

(defmethod wait-for-inputs ((network t-network))
  (loop while (< (inputs-ready-count network) (input-count network))))

(defmethod output-errors ((network t-network) (expected-outputs list))
  (loop for neuron in (output-layer network)
        for expected-output in expected-outputs
        for output = (output neuron)
        collect (- expected-output output)))

(defmethod output-errors ((outputs list) (expected-outputs list))
  (loop for output in outputs
        for expected-output in expected-outputs
        collect (- expected-output output)))
