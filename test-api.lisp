(in-package :cl-user)
(ql:quickload :drakma)
(ql:quickload :yason)
(ql:quickload :prove)
(require :bianet)
(require :drakma)
(require :yason)
(require :prove)

(defpackage :test-api 
  (:use :cl :prove :bianet)
  (:local-nicknames (:dr :drakma)
                    (:y :yason)))

(in-package :test-api)

(defparameter *port* 3001)
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
        status)))

(rest-service-start 3002)

(plan 2)

(subtest
    "Check /api/create-net endpoint"
  (let ((result (http-post "/api/create-net"
                           (list :name "test-c"
                                 :topology (vector 2 8 4 1)
                                 :thread-count 1))))
    (is (gethash "topology" result) (list 2 9 5 1)
        "Topology is correct")
    (is (gethash "neuron-count" result) 17
        "Neuron count is correct")
    (is (gethash "input-neuron-count" result) 2
        "Count of input-layer neurons is correct")
    (is (gethash "output-neuron-count" result) 1
        "Count of output-layer neurons is correct")
    (is (gethash "hidden-neuron-count" result) 14
        "Count of hidden-layer neurons is correct")
    (is (gethash "hidden-layer-count" result) 2
        "Hidden-layer count is correct")
    (is (gethash "connection-count" result) 68
        "Connection count is correct")))

(subtest
    "Check /api/neurons endpoint"
  (let ((result (http-get "/api/neurons")))
    (is (gethash "total-size" result) 17
        "Total size is correct")
    (is (gethash "selection-size" result) 17
        "Selection size is correct")
    (let ((neurons (gethash "neurons" result)))
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
  (let ((result (http-get "/api/connections")))
    (is (gethash "total-size" result) 68
        "Total size is correct")
    (is (gethash "selection-size" result) 20
        "Selection size is correct")
    (let ((connections (gethash "connections" result)))
      (is (length connections) 20
          "Correct number of connections listed")
      (ok (and (equal (subseq (gethash "source" (nth 0 connections)) 0 1)
                      "0")
               (equal (subseq (gethash "target" (nth 0 connections)) 0 1)
                      "1"))
          "First cx connects layer 0 with layer 1"))))
    

(rest-service-stop)

(finalize)
