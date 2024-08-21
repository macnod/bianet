(in-package :bianet)

(h:define-easy-handler (api-get-connections :uri "/api/connections"
                                            :default-request-type :get)
    ((page :parameter-type 'integer :init-form 1)
     (page-size :parameter-type 'integer :init-form *default-page-size*)
     (id :parameter-type 'integer :init-form nil))
  (case (h:request-method*)
    (:get (api-connections-get page page-size id))
    (otherwise (unsupported-method))))

(defun api-connections-get (page page-size id)
  (set-headers)
  (if *net*
      (let ((filter (lambda (cx) (or (not id) (= (id cx) id)))))
        (encode (paged-list :key :connections
                            :list-function (lambda () 
                                             (list-outgoing (neurons *net*)))
                            :filter-function filter
                            :rows-function #'connection-plists
                            :page page
                            :page-size page-size)))
      (failed-request "No network defined")))

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
