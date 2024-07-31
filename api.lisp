(in-package :bianet)

(defparameter *server* nil)
(defparameter *default-port* 3001)
(defparameter *net* nil)
(defparameter *default-page-size* 20)
(defparameter *last-json* nil)
(defparameter *min-integer* (truncate -1e12))
(defparameter *max-integer* (truncate 1e12))

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

(defun encoded-network-item-list 
    (key list-function plists-function page page-size)
  (if *net*
      (let* ((list (funcall list-function))
             (data (select-page key list page page-size)))
        (encode (list :status "ok"
                      :result (list :total_size (getf data :total-size)
                                    :selection_size (getf data :selection-size)
                                    key (funcall plists-function 
                                                 (getf data key))))))
      (encode (list :status "ok"
                    :result (list :total_size 0
                                  :selection_size 0
                                  key (vector))))))

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

(defmacro process-json (&body body)
  `(let* ((data (h:raw-post-data :force-text t))
          (json (parse-json data)))
     (setf *last-json* json)
     (if (and (listp json)
              (equal (getf json :status) "fail")
              (getf json :error))
         (encode json)
         (progn ,@body))))
         
(h:define-easy-handler (api-create-net :uri "/api/create-net" 
                                       :default-request-type :post)
    ()
  (process-json
    (setf (h:content-type*) "application/json")
    (setf (h:header-out "Access-Control-Allow-Origin") "*")
    (multiple-value-bind (name topology thread-count errors)
        (validate-create-net json)
      (if errors
          (progn
            (encode (list :status "fail" 
                          :errors (map 'vector 'identity errors))))
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
     (page-size :parameter-type 'integer :init-form *default-page-size*))
  (setf (h:content-type*) "application/json")
  (setf (h:header-out "Access-Control-Allow-Origin") "*")
  (encoded-network-item-list :neurons 
                             (lambda () (neurons *net*))
                             #'neuron-plists
                             page
                             page-size))

(h:define-easy-handler (api-get-connections :uri "/api/connections")
    ((page :parameter-type 'integer :init-form 1)
     (page-size :parameter-type 'integer :init-form *default-page-size*))
  (setf (h:content-type*) "application/json")
  (setf (h:header-out "Access-Control-Allow-Origin") "*")
  (encoded-network-item-list :connections
                             (lambda () (list-outgoing (neurons *net*)))
                             #'connection-plists
                             page
                             page-size))
