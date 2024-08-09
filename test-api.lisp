(in-package :cl-user)
(ql:quickload :drakma)
(ql:quickload :yason)
(ql:quickload :prove)
(ql:quickload :cl-ppcre)
(require :bianet)
(require :drakma)
(require :yason)
(require :prove)
(require :dc-ds)
(require :cl-ppcre)
(require :dc-eclectic)

(defpackage :test-api 
  (:use :cl :prove :bianet :cl-ppcre :dc-eclectic)
  (:local-nicknames (:dr :drakma)
                    (:y :yason)
                    (:ds :dc-ds)
                    (:u :dc-eclectic)))

(in-package :test-api)

(defparameter *port* 3002)
(defparameter *url-format-string* "http://localhost:~a~a")
(push (cons "application" "json") dr:*text-content-types*)
(setf y:*symbol-key-encoder* #'y:encode-symbol-as-lowercase
      y:*symbol-encoder* #'y:encode-symbol-as-lowercase)

(defun http-get (path)
  (multiple-value-bind (content status)
      (dr:http-request (format nil *url-format-string* *port* path))
    (if (= status 200)
        (y:parse content)
        status)))

(defun http-post (path plist)
  (multiple-value-bind (content status)
      (dr:http-request (format nil *url-format-string* *port* path)
                                  :method :post
                                  :content-type "application/json"
                                  :content (with-output-to-string (json)
                                             (y:encode-plist plist json)))
    (if (= status 200)
        (y:parse content)
        (progn
          (format t content)
          status))))

(rest-service-start :port *port*)

(plan 7)

(subtest
    "Check /api/create-net endpoint"
  (let ((data (http-post "/api/create-net"
                         (list :name "test-c"
                               :topology (vector 2 8 4 1)
                               :thread_count 1))))
    (is (ds:ds-get data "status") "ok" 
        "/api/create-net call succeeds")
    (is (ds:ds-get data "result" "topology") (list 2 9 5 1)
        "Topology is correct")
    (is (ds:ds-get data "result" "neuron_count") 17
        "Neuron count is correct")
    (is (ds:ds-get data "result" "input_neuron_count") 2
        "Count of input-layer neurons is correct")
    (is (ds:ds-get data "result" "output_neuron_count") 1
        "Count of output-layer neurons is correct")
    (is (ds:ds-get data "result" "hidden_neuron_count") 14
        "Count of hidden-layer neurons is correct")
    (is (ds:ds-get data "result" "hidden_layer_count") 2
        "Hidden-layer count is correct")
    (is (ds:ds-get data "result" "connection_count") 68
        "Connection count is correct")))

(subtest
    "Check /api/net endpoint"
  (http-post "/api/create-net"
             (list :name "test-c"
                   :topology (vector 2 8 4 1)
                   :thread_count 1))
  (let* ((data (http-get "/api/net"))
         (result (ds:ds-get data "result")))
    (is (ds:ds-get data "status") "ok"
        "/api/net call succeeds")
    (is (ds:ds-get result "name") "test-c"
        "network name is correct")
    (is (ds:ds-get result "topology") (list 2 9 5 1)
        "network topology is correct")
    (is (ds:ds-get result "neuron_count") 17
        "network neuron count is correct")
    (is (ds:ds-get result "connection_count") 68
        "network connection count is correct")
    (is (ds:ds-get result "thread_count") 1
        "network thread count is correct")
    (ok (ds:ds-get result "running")
        "network threads are running")
    (is (ds:ds-get result "training") nil
        "network training status is correct")
    (ok (not (ds:ds-get result "network_error"))
        "network error is correct")
    (ok (not (ds:ds-get result "training_time"))
        "network training time is correct")
    (ok (not (ds:ds-get result "interations"))
        "network training iterations is correct")
    (ok (> (ds:ds-get result "max_weight") 0.0)
        "network max-weight > 0")
    (ok (< (ds:ds-get result "min_weight") 0.0)
        "network min-weight < 0")))

(subtest
    "Bad JSON posted to /api/create-net"
  (let ((data (http-post "/api/create-net"
                         (list :name "test-d"
                               :name1 "test-e"
                               :topology (vector 2 8 4 1)
                               :thread_count 1))))
    (is (ds:ds-get data "status") "ok"
        "extra parameters are ignored"))
  (let ((data (http-post "/api/create-net"
                         (list :name1 "test-f"
                               :topology (vector 2 8 4 1)
                               :thread_count 1))))
    (is (ds:ds-get data "status") "fail"
        "call fails because required parameter 'name' is missing")
    (is (length (ds:ds-get data "errors")) 1
        "single error reported")
    (is (ds:ds-get data "errors" 0) "\"name\" is required"
        "error: \"name\" is required"))
  (let ((data (http-post "/api/create-net"
                         (list :name "test-g"
                               :thread_count 1))))
    (is (ds:ds-get data "status") "fail"
        "call fails because required parameter 'topology' is missing"))
  (let ((data (http-post "/api/create-net"
                         (list :name "test-h"
                               :topology (vector 2 8 4 1)))))
    (is (ds:ds-get data "status") "fail"
        "call fails because required parameter 'thread_count' is missing")
    (is (car (ds:ds-get data "errors")) "\"thread_count\" is required"
        "error: \"thread_count\" is required"))
  (let ((data (http-post "/api/create-net" (list :bogus ""))))
    (is (ds:ds-get data "status") "fail"
        "call fails because all required keys are missing")
    (is (length (ds:ds-get data "errors")) 3
        "3 errors messages relating to the missing keys")))        

(subtest
    "Check /api/neurons endpoint"
  (let ((data (http-get "/api/neurons")))
    (is (ds:ds-get data "status") "ok"
        "/api/neurons call succeeds")
    (is (ds:ds-get data "result" "total_size") 17
        "Total size is correct")
    (is (ds:ds-get data "result" "selection_size") 17
        "Selection size is correct")
    (let ((neurons (ds:ds-get data "result" "neurons")))
      (is (length neurons) 17
          "Correct number of neurons listed")
      (ok (zerop (gethash "layer" (nth 0 neurons)))
          "First neuron is in layer 0")
      (ok (zerop (gethash "layer" (nth 1 neurons)))
          "Second neuron is in layer 0")
      (is (gethash "layer" (nth 2 neurons)) 1
          "Third neuron is in layer 1"))))

(subtest
    "Check /api/connections endpoint"
  (let ((data (http-get "/api/connections")))
    (is (ds:ds-get data "status") "ok"
        "/api/connections call succeeds")
    (is (ds:ds-get data "result" "total_size") 68
        "Total size is correct")
    (is (ds:ds-get data "result" "selection_size") 20
        "Selection size is correct")
    (let ((connections (ds:ds-get data "result" "connections")))
      (is (length connections) 20
          "Correct number of connections listed")
      (ok (and (equal (subseq (gethash "source" (nth 0 connections)) 0 1)
                      "0")
               (equal (subseq (gethash "target" (nth 0 connections)) 0 1)
                      "1"))
          "First cx connects layer 0 with layer 1"))))

(subtest
    "Check /api/error endpoint"
  (let ((data (http-get "/api/error")))
    (is (ds:ds-get data "status") "ok"
        "/api/error call succeeds")
    (is (ds:ds-get data "result" "total_size") 0)))

(subtest
    "Check POST /api/training-set"
  (let ((data (http-post 
               "/api/training-set"
               (list :frames 
                     (vector 
                      (vector 0 0 0 1)
                      (vector 0 1 1 1)
                      (vector 1 0 1 1)
                      (vector 1 1 0 1))))))
    (is (ds:ds-get data "status") "fail")
    (is (nth 0 (ds:ds-get data "errors"))
        "Invalid training set"))
  (let ((data (http-post 
               "/api/training-set"
               (list :frames 
                     (vector 
                      (vector 0 0 0)
                      (vector 0 1 1)
                      (vector 1 0 1)
                      (vector 1 1 0))))))
    (is (ds:ds-get data "status") "ok" "POST /api/training-set succceeds"))
  (let ((data (http-get "/api/training-set?page=1&page-size=3")))
    (is (ds:ds-get data "status") "ok" "GET /api/training-set succeeds")
    (is (length (ds:ds-get data "result" "frames")) 3
        "training set page size is correct")
    (is (ds:ds-get data "result" "total_size") 4
        "total_size key has correct value")
    (is (ds:ds-get data "result" "selection_size") 3
        "selection_size key has correct value")))

(rest-service-stop)

(finalize)
