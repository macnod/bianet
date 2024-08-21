(in-package :bianet)

(h:define-easy-handler (api-get-neurons :uri "/api/neurons"
                                        :default-request-type :get)
    ((page :parameter-type 'integer :init-form 1)
     (page-size :parameter-type 'integer :init-form *default-page-size*)
     (layer :parameter-type 'integer :init-form nil)
     (id :parameter-type 'integer :init-form nil)
     (name :parameter-type 'string :init-form nil))
  (case (h:request-method*)
    (:get (api-neurons-get page page-size layer id name))
    (otherwise (unsupported-method))))
      
(defun api-neurons-get (page page-size layer id name)
  (set-headers)
  (if *net*
      (let ((filter (lambda (neuron)
                      (and (or (not layer) (= (layer neuron) layer))
                           (or (not id) (= (id neuron) id))
                           (or (not name) (equal (name neuron) name))))))
        (encode 
         (paged-list 
          :key :neurons
          :list-function (lambda () (neurons *net*))
          :filter-function filter
          :rows-function #'neuron-plists
          :page page
          :page-size page-size)))
  (failed-request "No network defined")))

(defun neuron-plists (neurons)
  (map 'vector
       (lambda (n)
         (list :name (name n)
               :id (id n)
               :layer (layer n)
               :biased (json-bool (biased n))
               :input (input n)
               :output (output n)
               :err (err n)
               :incoming (dl:len (incoming n))
               :outgoing (dl:len (outgoing n))))
       neurons))
