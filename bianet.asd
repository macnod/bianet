(asdf:defsystem :bianet
  :description "A neural network framework."
  :author "Donnie Cameron <macnod@gmail.com>"
  :license "MIT License"
  :depends-on (:sb-concurrency 
               :cl-cpus 
               :cl-ppcre 
               :dc-eclectic
               :dc-dlist
               :dc-ds
               :hunchentoot
               :yason
               :cl-ppcre)
  :serial t
  :components ((:file "bianet-package")
               (:file "neuron")
               (:file "network")
               (:file "api")))
