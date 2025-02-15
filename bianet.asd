(ql:quickload :cl-cpus)
(asdf:defsystem :bianet
  :description "A neural network framework."
  :author "Donnie Cameron <macnod@gmail.com>"
  :license "MIT License"
  :depends-on (:sb-concurrency :cl-cpus)
  :serial t
  :components ((:file "bianet-package")
               (:file "neuron")
               (:file "network")))
