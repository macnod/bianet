(in-package :bianet)

(h:define-easy-handler (api-training-set :uri "/api/training-set"
                                         :default-request-type :both)
    ((page :parameter-type 'integer :init-form 1)
     (page-size :parameter-type 'integer :init-form *default-page-size*))
  (case (h:request-method*)
    (:get (api-training-set-get page page-size))
    (:post (api-training-set-post page page-size))
    (:delete (api-training-set-delete))
    (:options (api-options))
    (otherwise (unsupported-method))))

(defun api-training-set-get (page page-size)
  (set-headers)
  (if *net*
      (encoded-training-set *net* page page-size)
      (failed-request "No network defined")))

(defun api-training-set-post (page page-size)
  (set-headers)
  (if *net*
      (process-json
        (multiple-value-bind (frames errors)
            (validate-frames *net* json)
          (if errors
              (apply #'failed-request errors)
              (progn
                (replace-training-set *net* frames)
                (encoded-training-set *net* page page-size)))))
      (failed-request "No network defined")))
  
(defun api-training-set-delete ()
  (set-headers)
  (if *net*
      (progn 
        (replace-training-set *net* nil)
        (encoded-training-set *net* 1 1))
      (failed-request "No network defined")))

(defun encoded-training-set (net page page-size)
  (encode
   (paged-list
    :key :frames
    :list-function (lambda () (training-set net))
    :rows-function #'training-set-rows
    :page page
    :page-size page-size
    :other-result-keys (list :input_count (input-count net)
                             :output_count (output-count net)))))

(defun training-set-rows (data)
  ;; data looks like this: '(id inputs outputs)
  (loop for row in data
        collect (list :id (car row)
                      :in (map 'vector 'identity (second row))
                      :out (map 'vector 'identity (third row)))
        into rows
        finally (return (map 'vector 'identity rows))))

(defun validate-frames (net json)
  (let* ((frames (ds:ds-get json "frames"))
         (tset (loop for frame in frames
                     for id = (ds:ds-get frame "id")
                     for inputs = (ds:ds-get frame "in")
                     for outputs = (ds:ds-get frame "out")
                     collect (list id inputs outputs) into set
                     finally
                        (format t "~%Training set: ~a~%" set)
                        (return set)))
         (errors (unless (validate-training-set net tset)
                   (list "Invalid training set"))))
    (values tset errors)))
