(defpackage :bianet
  (:use :cl :sb-concurrency :sb-thread :cl-ppcre)
  (:local-nicknames (:dl :dc-dlist)
                    (:u :dc-eclectic))
  (:export
   ;; neuron methods
   adjust-weight
   adjust-weights
   biased-derivative
   biased-transfer
   compute-new-weight
   connect
   disconnect
   evaluate-error-messages
   evaluate-input-messages
   excite ;; this is a network method as well
   excite-internal
   fire-error
   fire-output
   initialize-instance
   isolate
   list-incoming
   list-incoming-weights
   list-outgoing
   logistic
   logistic-derivative
   modulate
   modulate-internal
   name
   next-cx-id
   next-cx-id-peek
   next-neuron-id
   next-neuron-id-peek
   next-weight
   process
   relu
   relu-derivative
   reset-ids
   reset-random-state
   reset-state
   t-cx
   t-neuron
   transfer
   transfer-error
   wait-for-backprop-p
   wait-for-output-p

   ;; neuron attributes
   biased
   bp-count
   delta
   e-mailbox
   err
   err-in
   err-in-last
   err-last
   excitation-count
   excited
   ff-count
   fire-count
   i-mailbox
   id
   incoming
   input
   input-last
   layer
   learning-rate
   modulated
   modulation-count
   modulator-count
   momentum
   on-input-ready
   on-output-ready
   outgoing
   output
   output-last
   process-count
   running
   source
   target
   transfer-count
   transfer-derivative
   transfer-function
   transfer-key
   update-count
   weight
   weight-last

   ;; network attributes
   max-error
   cx-count
   input-layer
   inputs-ready-count
   job-queue ;; also an attribute of neuron
   layers
   name
   neurons
   neurons-reversed
   output-layer
   outputs-ready-count
   t-network
   topology

   ;; network methods
   compute-cx-count
   compute-weights-random
   compute-weights-sinusoidal
   eucledian-error
   output-errors
   stop-threads
   wait-for-inputs
   wait-for-outputs
   ))
