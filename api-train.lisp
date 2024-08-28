(in-package :bianet)

(h:define-easy-handler (api-train :uri "/api/train"
                                  :default-request-type :both)
    ((page :parameter-type 'integer :init-form 1)
     (page-size :parameter-type 'integer :init-form *default-page-size*))
  (case (h:request-method*)
    (:get (api-train-get page page-size))
    (:post (api-train-post page page-size))
    (:delete (api-train-delete))
    (:options (api-options))
    (otherwise (unsupported-method))))

(defun api-train-get (page page-size)
  (set-headers)
  (if *net*
      (encoded-training-status *net* page page-size)
      (failed-request "No network defined")))

(defun api-train-post (page page-size)
  (set-headers)
  (if *net*
      (process-json
        (multiple-value-bind (target-error max-iterations update-frequency errors)
            (validate-train-net *net* json)
          (if errors
              (apply #'failed-request errors)
              (progn
                (clear-training-log *net*)
                (train *net*
                       target-error
                       max-iterations
                       (training-set *net*)
                       update-frequency
                       (lambda (e i time)
                         (declare (ignore e i time))))
                (encoded-training-status *net* page page-size)))))
      (failed-request "No network defined")))

(defun api-train-delete ()
  (if *net*
      (progn
        (set-headers)
        (clear-weights *net*)
        (encoded-training-status *net* 1 1))
      (failed-request "No network defined")))
        
(defun encoded-training-status (net page page-size)
  (let* ((log (training-log-tail net))
         (network-error (nth 0 log))
         (elapsed-time (nth 1 log))
         (iteration (nth 2 log)))
    (encode 
     (paged-list
      :key :training_log
      :list-function (lambda () (training-log-list net))
      :rows-function #'network-log-plist
      :page page
      :page-size page-size
      :other-result-keys (list
                          :input_count (input-count net)
                          :output_count (output-count net)
                          :connection_count (cx-count net)
                          :min_weight (min-weight net)
                          :max_weight (max-weight net)
                          :network_error network-error
                          :training_time elapsed-time
                          :iteration iteration
                          :running (json-bool (running net))
                          :training (json-bool (is-training net)))))))

(defun validate-train-net (net json)
  (let* ((target-error (ds:pick json "target_error"))
         (max-iterations (ds:pick json "max_iterations"))
         (update-frequency (ds:pick json "update_frequency"))
         (errors (append
                  (validate-floatp
                   "target_error" target-error
                   :required t
                   :min 0.01 :max 0.5)
                  (validate-integerp
                   "max_iterations" max-iterations
                   :required t
                   :min 1 :max 10000000)
                  (validate-floatp
                   "update_frequency" update-frequency
                   :required t
                   :min 0.05 :max 60.0)
                  (unless net
                    (list "No network defined")))))
    (when net
      (unless (training-set net)
        (push "No training set defined" errors))
      (when (is-training net)
        (push "Network is already training" errors)))
    (values target-error max-iterations update-frequency errors)))

(defun network-log-plist (list)
  (loop for row in list
        for (network-error elapsed-time iteration iteration-time) = row
        collect (list :network_error network-error
                      :elapsed_time elapsed-time
                      :iteration iteration
                      :iteration_time iteration-time)
          into log
        finally (return (map 'vector 'identity log))))
