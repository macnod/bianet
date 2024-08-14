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
                (update-training-set *net* frames)
                (encoded-training-set *net* page page-size)))))
      (failed-request "No network defined")))
  
(defun api-training-set-delete ()
  (failed-request "Not implemented"))

(defun encoded-training-set (net page page-size)
  (encode
   (paged-list
    :key :frames
    :list-function (lambda () (training-set net))
    :rows-function (lambda (data)
                     (map 'vector
                          (lambda (frame)
                            (map 'vector 'identity
                                 (append (first frame) (second frame))))
                          data))
    :page page
    :page-size page-size
    :other-result-keys (list :input_count (input-count net)
                             :output_count (output-count net)))))

(defun validate-frames (net json)
  (let* ((frames (ds:ds-get json "frames"))
         (tset (loop for frame in frames
                     for inputs = (subseq frame 0 (input-count net))
                     for outputs = (subseq frame (input-count net))
                     collect (list inputs outputs)))
         (errors (unless (validate-training-set net tset)
                   (list "Invalid training set"))))
    (values tset errors)))
