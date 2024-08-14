(in-package :bianet)

(h:define-easy-handler (api-net :uri "/api/net")
    ()
  (case (h:request-method*)
    (:get (api-net-get))
    (:post (api-net-post))
    (:delete (api-net-delete))
    (:options (api-options))
    (otherwise (unsupported-method))))

(defun api-net-get ()
  (set-headers)
  (encoded-network-info))

(defun api-net-post ()
  (set-headers)
  (process-json
    (multiple-value-bind (name topology thread-count errors)
        (validate-create-net json)
      (if errors
          (apply #'failed-request errors)
          (progn
            (when *net* (stop-threads *net*))
            (setf *net* (make-instance 't-network
                                       :name name
                                       :topology topology
                                       :thread-count thread-count))
            (encoded-network-info))))))

(defun api-net-delete ()
  (set-headers)
  (if *net*
      (progn
        (stop-threads *net*)
        (setf *net* nil)
        (encode (list :status "ok" :result (list "Neural network deleted"))))
      (failed-request "No network defined")))

(defun encoded-network-info ()
  (let* ((training-log-entry (car (last (dl:to-list (training-log *net*)))))
         (network-error (nth 0 training-log-entry))
         (training-time (nth 1 training-log-entry))
         (iterations (nth 2 training-log-entry)))
    (encode 
     (list
      :status "ok"
      :result (list
               :name (name *net*)
               :topology (map 'vector 'identity (topology *net*))
               :neuron_count (reduce '+ (topology *net*))
               :input_count (car (topology *net*))
               :output_count (car (last (topology *net*)))
               :hidden_count (reduce '+ (butlast (cdr (topology *net*))))
               :connection_count (length 
                                  (list-outgoing (neurons *net*)))
               :thread_count (thread-count *net*)
               :running (json-bool (running *net*))
               :training (json-bool (is-training *net*))
               :network_error network-error
               :training_time training-time
               :iterations iterations
               :max_weight (max-weight *net*)
               :min_weight (min-weight *net*))))))

(defun validate-create-net (json)
  (let* ((name (ds:ds-get json "name"))
         (topology (ds:ds-get json "topology"))
         (thread-count (ds:ds-get json "thread_count"))
         (errors (append
                  (validate-stringp
                   "name" name :required t :min-size 1 :max-size 20)
                  (validate-listp
                   "topology" topology
                   :required t
                   :min-size 2
                   :max-size 100
                   :element-check (lambda (n)
                                    (and (integerp n)
                                         (>= n 1)
                                         (<= n 10000)))
                   :element-check-doc "an integer between 1 and 10000")
                  (validate-integerp
                   "thread_count" thread-count
                   :required t :min 1 :max *max-thread-count*))))
    (values name topology thread-count errors)))

