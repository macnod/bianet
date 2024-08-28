(in-package :bianet)

(defparameter *server* nil)
(defparameter *default-port* 3001)
(defparameter *net* nil)
(defparameter *default-page-size* 20)
(defparameter *last-json* nil)
(defparameter *last-data* nil)
(defparameter *last-request* nil)
(defparameter *min-integer* (truncate -1e12))
(defparameter *max-integer* (truncate 1e12))
(defparameter *min-float* -1e12)
(defparameter *max-float* 1e12)
(defparameter *cors-headers* (format nil "~{~a~^,~}"
                                     (list "Content-Type"
                                           "Accept")))
(defparameter *cors-methods* (format nil "~{~a~^,~}"
                                 (list :post :put :get :delete :options)))

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

(defun json-bool (value)
  (if value t y:false))

(defun encode (plist)
  (let ((y:*list-encoder* #'y:encode-plist)
        (y:*symbol-encoder* #'y:encode-symbol-as-lowercase)
        (y:*symbol-key-encoder* #'y:encode-symbol-as-lowercase))
    (with-output-to-string (json)
      (y:encode plist json))))

(defun parse-json (json)
  (handler-case (y:parse json)
    (error (e) 
      (let ((se (ppcre:regex-replace-all
                 " *\\n *"
                 (ppcre:regex-replace-all 
                  "#.([^ ])" (format nil "~a" e) "'\\1'")
                 " ")))
        (ds:ds `(:map :status "fail" 
                      :errors (:array "Malformed JSON" ,se)))))))

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

(defun paged-list (&key 
                     (key (error ":key required"))
                     (list-function (error ":list-function required"))
                     (filter-function (lambda (x) (declare (ignore x)) t))
                     (rows-function (error ":row-function required"))
                     (page 1)
                     (page-size *default-page-size*)
                     other-result-keys)
  (let* ((list (remove-if-not filter-function (funcall list-function)))
         (data (select-page key list page page-size))
         (result (list :total_size (getf data :total-size)
                       :selection_size (getf data :selection-size)
                       key (funcall rows-function (getf data key)))))
    (loop for key in other-result-keys by #'cddr
          for value in (cdr other-result-keys) by #'cddr
          do (setf (getf result key) value))
    (list :status "ok" :result result)))

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

(defmacro process-json (&body body)
  `(let* ((data (h:raw-post-data :force-text t))
          (json (parse-json data)))
     (setf *last-json* json
           *last-data* data)
     (if (equal (ds:pick json :status) "fail")
         (encode json)
         (progn ,@body))))

(defun set-headers ()
  (setf (h:header-out "Access-Control-Allow-Origin") "*"
        (h:header-out "Access-Control-Allow-Methods") *cors-methods*
        (h:header-out "Access-Control-Allow-Headers") *cors-headers*
        (h:header-out "Content-Type") "application/json"))

(defun api-options ()
  (set-headers)
  (encode (list :status "ok")))

(defun failed-request (&rest errors)
  (set-headers)
  (loop for e in errors
        for format-string = (if (listp e) (car e) e)
        for parameters = (when (listp e) (second e))
        for format-parameters = (cons nil
                                      (if parameters
                                          (list format-string parameters)
                                          (list format-string)))
        collect (apply #'format format-parameters) into error-strings
        finally (return 
                  (encode
                   (list :status "fail"
                         :errors (map 'vector 'identity error-strings))))))

(defun unsupported-method ()
  (set-headers)
  (failed-request (list "Unsupported HTTP method ~a" (h:request-method*))))
