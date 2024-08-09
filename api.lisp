(in-package :bianet)

(defparameter *server* nil)
(defparameter *default-port* 3001)
(defparameter *net* nil)
(defparameter *default-page-size* 20)
(defparameter *last-json* nil)
(defparameter *min-integer* (truncate -1e12))
(defparameter *max-integer* (truncate 1e12))
(defparameter *min-float* -1e12)
(defparameter *max-float* 1e12)

(defun rest-service-start (&key (port *default-port*) log-destination)
  (rest-service-stop)
  (setf *server* (make-instance 'h:easy-acceptor
                                :port port
                                :address "0.0.0.0"
                                :access-log-destination log-destination
                                :message-log-destination log-destination))
  (h:start *server*))

(defun rest-service-stop ()
  (when *net*
    (stop-threads *net*)
    (setf *net* nil))
  (when *server*
    (h:stop *server*)
    (setf *server* nil)))

(defun encode (plist)
  (let ((y:*list-encoder* #'y:encode-plist)
        (y:*symbol-encoder* #'y:encode-symbol-as-lowercase)
        (y:*symbol-key-encoder* #'y:encode-symbol-as-lowercase))
    (with-output-to-string (json)
      (y:encode plist json))))

(defun parse-json (json)
  (handler-case (y:parse json)
    (error (e) (list :status "fail"
                     :error (format nil "Malformed JSON. ~a" e)))))

(defun connection-plists (connections)
  (map 'vector
       (lambda (c)
         (list :id (id c)
               :source (name (source c))
               :target (name (target c))
               :weight (weight c)
               :learning_rate (learning-rate c)
               :momentum (momentum c)
               :delta (delta c)
               :fire_count (fire-count c)
               :update_count (update-count c)))
       connections))

(defun neuron-plists (neurons)
  (map 'vector
       (lambda (n)
         (list :name (name n)
               :id (id n)
               :layer (layer n)
               :biased (if (biased n) t y:false)
               :input (input n)
               :output (output n)
               :err (err n)
               :incoming (dl:len (incoming n))
               :outgoing (dl:len (outgoing n))))
       neurons))

(defun network-plist (network)
  (when network
    (list :topology (map 'vector 'identity (topology network))
          :neuron_count (length (neurons network))
          :input_neuron_count (car (topology network))
          :output_neuron_count (car (last (topology network)))
          :hidden_neuron_count (reduce '+ (mapcar (lambda (l) (length l))
                                                  (cdr (butlast (layers network)))))
          :hidden_layer_count (- (length (topology network)) 2)
          :connection_count (cx-count network))))

(defun error-plists (errors)
  (map 'vector
       (lambda (e)
         (list :error (nth 0 e)
               :time (nth 1 e)
               :iteration (nth 2 e)
               :iteration-time (nth 3 e)))
       errors))

(defun select-page (key list page page-size)
  (loop with begin = (* (1- page) page-size)
        with end = (1- (+ begin page-size))
        for item in list
        for index = 0 then (1+ index)
        count item into total-size
        when (and (>= index begin) (<= index end))
          collect item into items
          and count item into selection-size
        finally (return (list :total-size total-size
                              :selection-size selection-size
                              key items))))

(defun paged-list (key list-function filter plists-function page page-size)
  (if *net*
      (let* ((list (remove-if-not filter (funcall list-function)))
             (data (select-page key list page page-size)))
        (list :status "ok"
              :result (list :total_size (getf data :total-size)
                            :selection_size (getf data :selection-size)
                            key (funcall plists-function
                                         (getf data key)))))
      (list :status "ok"
            :result (list :total_size 0
                          :selection_size 0
                          key (vector)))))

(defun validate-stringp (name value
                         &key
                           (required t)
                           (min-size 1)
                           (max-size 100))
  (let (errors)
    (cond
      ((and required (null value))
       (push (format nil "~s is required" name) errors))
      ((and (not (null value)) (not (stringp value)))
       (push (format nil "~s must be a string" name) errors))
      ((and (not (null value))
            (or (< (length value) min-size)
                (> (length value) max-size)))
       (push (format nil "~s must have a length between ~d and ~d, inclusive"
                     name min-size max-size)
             errors)))
    errors))

(defun validate-integerp (name value
                          &key
                            (required t)
                            (min *min-integer*)
                            (max *max-integer*))
  (let (errors)
    (cond
      ((and required (null value))
       (push (format nil "~s is required" name) errors))
      ((and (not (null value)) (not (integerp value)))
       (push (format nil "~s must be an integer" name) errors))
      ((and (not (null value))
            (or (< value min)
                (> value max)))
       (push (format nil "~s must be between ~d and ~d, inclusive"
                     name min max)
             errors)))
    errors))

(defun validate-floatp (name value
                             &key
                             (required t)
                             (min *min-float*)
                             (max *max-float*))
  (let (errors)
    (cond
      ((and required (null value))
       (push (format nil "~s is required" name) errors))
      ((and (not (null value)) (not (floatp value)))
       (push (format nil "~s must be a floating-point value" name) errors))
      ((and (not (null value))
            (or (< value min)
                (> value max)))
       (push (format nil "~s must be between ~f and ~f, inclusive"
                     name min max)
             errors)))
    errors))

(defun validate-listp (name value
                       &key
                         (required t)
                         (min-size 1)
                         (max-size 10000)
                         (element-check
                          (lambda (n)
                            (and (integerp n)
                                 (>= n *min-integer*)
                                 (<= n *max-integer*))))
                         (element-check-doc
                          (format nil "an integer between ~d and ~d"
                                  *min-integer*
                                  *max-integer*)))
  (let (errors)
    (cond ((and required (null value))
           (push (format nil "~s is required" name) errors))
          ((and (not (null value)) (not (listp value)))
           (push (format nil "~s must be an array" name) errors))
          ((not (null value))
           (when (or (< (length value) min-size)
                     (> (length value) max-size))
             (push (format nil "~s array must have a length between ~d and ~d, inclusive"
                           name min-size max-size)
                   errors))
           (when (not (loop for n in value always (funcall element-check n)))
             (push (format nil "Each ~s array element must be ~a"
                           name element-check-doc)
                   errors))))
    errors))

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

(defun validate-frames (net json)
  (let* ((frames (ds:ds-get json "frames"))
         (tset (loop for frame in frames
                     for inputs = (subseq frame 0 (input-count net))
                     for outputs = (subseq frame (input-count net))
                     collect (list inputs outputs)))
         (errors (unless (validate-training-set net tset)
                   (list "Invalid training set"))))
    (values tset errors)))
        

(defun validate-train-net (json)
  (let* ((target-error (ds:ds-get json "target_error"))
         (max-iterations (ds:ds-get json "max_iterations"))
         (update-frequency (ds:ds-get json "update_frequency"))
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
                  :min 0.05 :max 60.0))))
    (values target-error max-iterations update-frequency errors)))

(defmacro process-json (&body body)
  `(let* ((data (h:raw-post-data :force-text t))
          (json (parse-json data)))
     (setf *last-json* json)
     (if (and (listp json)
              (equal (getf json :status) "fail")
              (getf json :error))
         (encode json)
         (progn ,@body))))

(defun set-post-headers ()
  (setf (h:content-type*) "application/json")
  (setf (h:header-out "Access-Control-Allow-Origin") "*")
  (setf (h:header-out "Access-Control-Allow-Methods") "POST,GET,OPTIONS")
  (setf (h:header-out "Access-Control-Allow-Headers")
        (format nil "~{~a~^, ~}"
                (list "Content-Type")))
  (setf (h:header-out "Content-Type") "application/json"))


(h:define-easy-handler (api-create-net :uri "/api/create-net"
                                       :default-request-type :post)
    ()
  (process-json
    (setf (h:content-type*) "application/json")
    (setf (h:header-out "Access-Control-Allow-Origin") "*")
    (multiple-value-bind (name topology thread-count errors)
        (validate-create-net json)
      (if errors
          (encode (list :status "fail"
                        :errors (map 'vector 'identity errors)))
          (progn
            (when *net*
              (stop-threads *net*))
            (setf *net*
                  (make-instance 't-network
                                 :name name
                                 :topology topology
                                 :thread-count thread-count))
            (encode (list :status "ok"
                          :result (network-plist *net*))))))))

(h:define-easy-handler (api-net-info :uri "/api/net")
    ()
  (setf (h:content-type*) "application/json")
  (setf (h:header-out "Access-Control-Allow-Origin") "*")
  (if *net*
      (let* ((training-log-entry (car (last (dl:to-list (training-log *net*)))))
             (network-error (nth 0 training-log-entry))
             (training-time (nth 1 training-log-entry))
             (iterations (nth 2 training-log-entry)))
        (encode (list
                 :status "ok"
                 :result (list
                          :name (name *net*)
                          :topology (map 'vector 'identity (topology *net*))
                          :neuron_count (reduce '+ (topology *net*))
                          :connection_count (length 
                                             (list-outgoing (neurons *net*)))
                          :thread_count (thread-count *net*)
                          :running (if (running *net*) t y:false)
                          :training (with-mutex ((training-mutex *net*))
                                      (if (training *net*) t y:false))
                          :network_error network-error
                          :training_time training-time
                          :iterations iterations
                          :max_weight (max-weight *net*)
                          :min_weight (min-weight *net*)))))
      (encode (list
               :status "ok"
               :result (list
                        :name ""
                        :topology (vector)
                        :neuron_count 0
                        :connection_count 0
                        :thread_count 0
                        :running nil
                        :training nil
                        :network_error nil
                        :training_time nil
                        :iterations nil
                        :max-weight nil
                        :min-weight nil)))))

(h:define-easy-handler (api-delete-net :uri "/api/delete-net"
                                       :default-request-type :post)
    ()
  (if *net*
      (let ((name (name *net*)))
        (stop-threads *net*)
        (setf *net* nil)
        (encode (list :status "ok"
                      :result (format nil "Deleted network ~a" name))))
      (encode (list :status "ok"
                    :result "No network to delete"))))

(h:define-easy-handler (api-get-neurons :uri "/api/neurons")
    ((page :parameter-type 'integer :init-form 1)
     (page-size :parameter-type 'integer :init-form *default-page-size*)
     (layer :parameter-type 'integer :init-form nil)
     (id :parameter-type 'integer :init-form nil)
     (name :parameter-type 'string :init-form nil))
  (setf (h:content-type*) "application/json")
  (setf (h:header-out "Access-Control-Allow-Origin") "*")
  (let ((filter (lambda (neuron)
                  (and (or (not layer) (= (layer neuron) layer))
                       (or (not id) (= (id neuron) id))
                       (or (not name) (equal (name neuron) name))))))
    (encode (paged-list :neurons
                        (lambda () (neurons *net*))
                        filter
                        #'neuron-plists
                        page
                        page-size))))

(h:define-easy-handler (api-get-connections :uri "/api/connections")
    ((page :parameter-type 'integer :init-form 1)
     (page-size :parameter-type 'integer :init-form *default-page-size*)
     (id :parameter-type 'integer :init-form nil))
  (setf (h:content-type*) "application/json")
  (setf (h:header-out "Access-Control-Allow-Origin") "*")
  (let ((filter (lambda (cx) (or (not id) (= (id cx) id)))))
    (encode (paged-list :connections
                        (lambda () (list-outgoing (neurons *net*)))
                        filter
                        #'connection-plists
                        page
                        page-size))))

(h:define-easy-handler (api-get-error :uri "/api/error")
    ()
  (setf (h:content-type*) "application/json")
  (setf (h:header-out "Access-Control-Allow-Origin") "*")
  (let ((response (paged-list :errors
                              (lambda () (with-mutex ((training-log-mutex *net*))
                                           (dl:to-list (training-log *net*))))
                              'identity
                              #'error-plists
                              1
                              1000))
        (training (with-mutex ((training-mutex *net*))
                    (if (training *net*) t y:false))))
    (setf (getf (getf response :result) :training) training)
    (encode response)))


(h:define-easy-handler (api-clear-weights :uri "/api/clear-weights"
                                          :default-request-type :post)
    ()
  (set-post-headers)
  ;; (setf (h:content-type*) "application/json")
  ;; (setf (h:header-out "Access-Control-Allow-Origin") "*")
  (clear-weights *net*)
  (encode (list :status "ok")))

(h:define-easy-handler (api-train-net :uri "/api/train"
                                      :default-request-type :post)
    ()
  (set-post-headers)
  (if (equal (h:request-method*) "OPTIONS")
      "{\"status\": \"fail\", \"errors\":[\"options request\"]}"
      (process-json
        (multiple-value-bind
              (target-error max-iterations update-frequency errors)
            (validate-train-net json)
          (format t "target-error=~a; max-iterations=~a; update-frequency=~a; errors=~a~%"
                  target-error max-iterations update-frequency errors)
          (if (is-training *net*)
              (push "Network is already training" errors)
              (loop for thread in (list-all-threads)
                    when (equal (format nil "bianet-~a-training" (name *net*))
                                (thread-name thread))
                      do (when (eql (join-thread thread :default :fail :timeout 1.0)
                                    :fail)
                           (terminate-thread thread))))
          (if errors
              (progn
                (format t "Errors:~%~{~a~^~%~}~%" errors)
                (encode (list :status "fail"
                              :errors (map 'vector 'identity errors))))
              (progn
                (format t "Starting training:~%~a~%" data)
                (make-thread
                 (lambda ()
                   (train *net*
                          target-error
                          max-iterations
                          (training-set *net*)
                          update-frequency
                          (lambda (e i time)
                            (format t "error=~,5f; iteration=~d; time=~,5f seconds~%"
                                    e i time))))
                 :name (format nil "bianet-~a-training" (name *net*)))
                (encode (list :status "ok"))))))))

(h:define-easy-handler (api-training-set :uri "/api/training-set"
                                         :default-request-type :get)
    ((page :parameter-type 'integer :init-form 1)
     (page-size :parameter-type 'integer :init-form *default-page-size*))
  (set-post-headers)
  (case (h:request-method*)
    (:post
     (process-json
       (multiple-value-bind (frames errors)
           (validate-frames *net* json)
         (if errors
             (encode (list :status "fail"
                           :errors (map 'vector #'identity errors)))
             (progn
               (update-training-set *net* frames)
               (encode (list :status "ok")))))))
    (:get
     (if *net*
         (let* ((result (paged-list :frames
                                    (lambda () (training-set *net*))
                                    #'identity
                                    (lambda (data) 
                                      (map 'vector
                                           (lambda (frame)
                                             (map 'vector 'identity
                                                  (append (first frame)
                                                          (second frame))))
                                           data))
                                    page
                                    page-size)))
           (setf (getf (getf result :result) :input_count) 
                 (input-count *net*)
                 (getf (getf result :result) :output_count) 
                 (output-count *net*))
           (encode result))
           
         (encode
          (list :status "ok"
                :frames (vector)))))
    (t
     (encode (list :status "ok"
                   :frames (vector))))))
