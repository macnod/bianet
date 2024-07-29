(in-package :bianet)

(defparameter *server* nil)
(defparameter *net* nil)
(defparameter *default-page-size* 20)

(push (cons "application" "json") drakma:*text-content-types*)

(defun rest-service-start (&optional (port 3001))
  (setf *server* (make-instance 'h:easy-acceptor
                                :port port
                                :address "0.0.0.0"))
  (h:start *server*))

(defun rest-service-stop ()
  (h:stop *server*))

(defun encode (plist)
  (let ((y:*list-encoder* #'y:encode-plist)
        (y:*symbol-key-encoder* #'y:encode-symbol-as-lowercase))
    (with-output-to-string (json)
      (y:encode plist json))))

(defun connection-plists (connections)
  (map 'vector
       (lambda (c)
         (list :id (id c)
               :source (name (source c))
               :target (name (target c))
               :weight (weight c)
               :learning-rate (learning-rate c)
               :momentum (momentum c)
               :delta (delta c)
               :fire-count (fire-count c)
               :update-count (update-count c)))
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
          :neuron-count (length (neurons network))
          :input-neuron-count (car (topology network))
          :output-neuron-count (car (last (topology network)))
          :hidden-neuron-count (reduce '+ (mapcar (lambda (l) (length l))
                                                  (cdr (butlast (layers network)))))
          :hidden-layer-count (- (length (topology network)) 2)
          :connection-count (cx-count network))))

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
        (encode (list :total-size (getf data :total-size)
                      :selection-size (getf data :selection-size)
                      key (funcall plists-function (getf data key)))))
      (encode (list :total-size 0
                    :selection-size 0
                    key (vector)))))

(h:define-easy-handler (api-create-net :uri "/api/create-net" 
                                       :default-request-type :post)
    ()
  (let* ((data (h:raw-post-data :force-text t))
         (json (y:parse data)))
    (setf (h:content-type*) "application/json")
    (setf *net* (make-instance 't-network
                               :name (gethash "name" json)
                               :topology (gethash "topology" json)
                               :thread-count (gethash "thread-count" json)))
    (encode (network-plist *net*))))

(h:define-easy-handler (api-delete-net :uri "/api/delete-net"
                                       :default-request-type :post)
    ()
  (when *net*
    (stop-threads *net*)
    (setf *net* nil)
    "OK"))

(h:define-easy-handler (api-get-neurons :uri "/api/neurons")
    ((page :parameter-type 'integer :init-form 1)
     (page-size :parameter-type 'integer :init-form *default-page-size*))
  
  (encoded-network-item-list :neurons 
                             (lambda () (neurons *net*))
                             #'neuron-plists
                             page
                             page-size))

(h:define-easy-handler (api-get-connections :uri "/api/connections")
    ((page :parameter-type 'integer :init-form 1)
     (page-size :parameter-type 'integer :init-form *default-page-size*))
  (encoded-network-item-list :connections
                             (lambda () (list-outgoing (neurons *net*)))
                             #'connection-plists
                             page
                             page-size))
