(in-package :bianet)

(defparameter *max-thread-count*
  (cl-cpus:get-number-of-processors))

(defparameter *default-thread-count*
    (if (< *max-thread-count* 3)
        *max-thread-count*
        (/ *max-thread-count* 2)))

(defclass t-network ()
  ((name :reader name :initarg :name :type string 
         :initform (error "name is required"))
   (topology :accessor topology :initarg :topology :type list
                     :initform (error "initial-topology is required"))
   (neurons :accessor neurons :type list :initform nil)
   (layers :accessor layers :type list :initform nil)
   (input-layer :accessor input-layer :type list :initform nil)
   (input-count :accessor input-count :type integer :initform 0)
   (cx-count :accessor cx-count :type integer :initform 0)
   (output-layer :accessor output-layer :type list :initform nil)
   (output-count :accessor output-count :type integer :initform 0)
   (inputs-ready-count :accessor inputs-ready-count :type cons 
                       :initform (cons 0 0))
   (outputs-ready-count :accessor outputs-ready-count :type cons 
                        :initform (cons 0 0))
   (running :accessor running :type boolean :initform t)
   (thread-count :accessor thread-count :initarg :thread-count
                 :type integer :initform *default-thread-count*)
   (thread-pool :accessor thread-pool :type list
                :initform nil)
   (job-set :accessor job-set :type hash-table :initform (make-hash-table))
   (job-set-mutex :accessor job-set-mutex :type mutex
                  :initform (make-mutex :name "job-set-mutex"))
   (job-queue :accessor job-queue :type mailbox :initform (make-mailbox))))

(defmethod initialize-instance :after ((network t-network) &key)
  (multiple-value-bind (neurons layers input-layer output-layer)
      (make-simple-network network)
    (setf (neurons network) neurons
          (layers network) layers
          (topology network) (mapcar #'length layers)
          (input-layer network) input-layer
          (output-layer network) output-layer
          (input-count network) (length input-layer)
          (output-count network) (length output-layer))
    (start-threads network)))

(defmethod dequeue-job ((network t-network))
  (let ((neuron (receive-message (job-queue network) :timeout 0.1)))
    (when neuron
      (with-mutex ((job-set-mutex network))
        (remhash (id neuron) (job-set network)))
      neuron)))

(defmethod enqueue-job ((network t-network))
  (let ((queue (job-queue network))
        (hash (job-set network))
        (mutex (job-set-mutex network)))
    (lambda (neuron)
      (unless (with-mutex (mutex) (gethash (id neuron) hash))
        (with-mutex (mutex) (setf (gethash (id neuron) hash) (id neuron)))
        (send-message queue neuron)))))

(defmethod thread-work ((network t-network))
  (lambda ()
    (loop while (running network)
          for neuron = (dequeue-job network)
          when neuron 
            do (process neuron))))

(defmethod start-threads ((network t-network))
  (loop for index from 1 to (thread-count network)
        for thread-name = (format nil "~a-thread-~3,'0d" (name network) index)
        do (push (make-thread (thread-work network) :name thread-name)
                 (thread-pool network))))

(defmethod stop-threads ((network t-network))
  (setf (running network) nil)
  (loop for thread-index from 1 to (* (thread-count network) 2)
        do (send-message (job-queue network) (car (neurons network)))
        finally (loop for thread in (thread-pool network) 
                      do (join-thread thread))))

(defun make-groups (group-count original-list)
  (loop with groups = (loop for a from 1 to group-count collect nil)
        for element in original-list
        for index = 0 then (1+ index)
        for group-index = (mod index group-count)
        do (push element (nth group-index groups))
        finally (return (mapcar #'reverse groups))))

(defun compute-cx-count (layers)
  (loop for layer in (butlast layers)
        for next-layer in (cdr layers)
        for neuron-count = (length layer)
        for neuron-count-next-layer = (length next-layer)
        for cx-count = (* neuron-count neuron-count-next-layer)
          then (+ cx-count (* neuron-count neuron-count-next-layer))
        finally (return cx-count)))

(defun compute-weights-sinusoidal (cx-count)
  (loop with step = (/ (* pi 2) cx-count)
        for a from 1 to cx-count
        for b = 0.0 then (+ b step)
        collect (* (cos b) 0.5)))

(defun compute-weights-random (cx-count)
  (loop for a from 1 to cx-count collect (next-weight)))

(defmethod connect-layers ((network t-network) (layers list))
  (loop for layer in (butlast layers)
        for next-layer in (cdr layers)
        for cx-count = (* (length layer) (length next-layer))
        for weights = ;; (compute-weights-sinusoidal cx-count)
        (compute-weights-random cx-count)
        do (loop for source in layer
                 do (loop for target in next-layer
                          do (connect source target
                                      :weight (pop weights)
                                      :learning-rate 0.1
                                      :momentum 0.3)
                             (incf (cx-count network)))))
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
                                    :job-queue (enqueue-job network)
                                    :on-output-ready output-ready-callback))
                    (is-input-layer
                     (make-instance 't-neuron
                                    :transfer-key transfer-key
                                    :biased biased
                                    :layer layer-index
                                    :job-queue (enqueue-job network)
                                    :on-input-ready input-ready-callback))
                    (t 
                     (make-instance
                        't-neuron
                        :transfer-key transfer-key
                        :biased biased
                        :layer layer-index
                        :job-queue (enqueue-job network)))))))

(defmethod make-simple-network ((network t-network))
  (let ((layers (connect-layers network (simple-network-layers network))))
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
  (loop while (< (car (outputs-ready-count network)) (output-count network))))

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
  (loop while (< (car (inputs-ready-count network)) (input-count network))))

(defmethod output-errors ((network t-network) (expected-outputs list))
  (loop for neuron in (output-layer network)
        for expected-output in expected-outputs
        for output = (output neuron)
        collect (- expected-output output)))

(defmethod output-errors ((outputs list) (expected-outputs list))
  (loop for output in outputs
        for expected-output in expected-outputs
        collect (- expected-output output)))

(defmethod max-error ((network t-network) (training-set list))
  (loop for (inputs expected-outputs) in training-set
        for outputs = (excite network inputs)
        for errors = (output-errors outputs expected-outputs)
        for err = (sqrt (reduce '+ (mapcar (lambda (x) (* x x)) errors)))
        for max-error = err then (if (> err max-error) err max-error)
        finally (return max-error)))

(defmethod eucledian-error ((network t-network) (expected-outputs list))
  (sqrt 
   (reduce #'+ (mapcar (lambda (x) (* x x))
                       (output-errors network expected-outputs)))))

(defmethod eucledian-error ((outputs list) (expected-outputs list))
  (sqrt
   (reduce #'+ (mapcar (lambda (x) (* x x))
                       (output-errors outputs expected-outputs)))))
