(in-package :bianet)

(declaim (fixnum **outputs-ready-count**))
(sb-ext:defglobal **outputs-ready-count** 0)

(declaim (fixnum **inputs-ready-count**))
(sb-ext:defglobal **inputs-ready-count** 0)

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

(defun simple-network-layers (layer-counts)
  (loop with ff-callback = (lambda () 
                             (sb-ext:atomic-incf **outputs-ready-count**))
        with bp-callback = (lambda () 
                             (sb-ext:atomic-incf **inputs-ready-count**))
        for layer-count in layer-counts
        for layer-index = 0 then (1+ layer-index)
        for is-input-layer = (zerop layer-index)
        for is-output-layer = (= (1+ layer-index) (length layer-counts))
        collect
        (loop 
          with layer-size = (if (or is-input-layer is-output-layer)
                                layer-count
                                (1+ layer-count)) ;; hidden layers have biases
          for a from 1 to layer-size
          for transfer-key = (if is-output-layer :logistic :relu)
          for biased = (and (not (or is-input-layer is-output-layer))
                            (= a (1- layer-size)))
          collect (cond
                    (is-output-layer
                     (make-instance 't-neuron
                                    :transfer-key transfer-key
                                    :biased biased
                                    :layer layer-index
                                    :on-output ff-callback))
                    (is-input-layer
                     (make-instance 't-neuron
                                    :transfer-key transfer-key
                                    :biased biased
                                    :layer layer-index
                                    :on-backprop bp-callback))
                    (t (make-instance
                        't-neuron
                        :transfer-key transfer-key
                        :biased biased
                        :layer layer-index))))))

(defun create-simple-network (topography)
  (reduce #'append
          (connect-layers 
           (simple-network-layers topography))))
