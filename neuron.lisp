(in-package :bianet)

(defparameter *next-neuron-id* 0)
(defparameter *neuron-id-mutex* (make-mutex :name "neuron-id"))

(defparameter *next-cx-id* 0)
(defparameter *cx-id-mutex* (make-mutex :name "cx-id"))

(defparameter *weight-mutex* (make-mutex :name "weight"))

(defparameter *default-learning-rate* 0.1)
(defparameter *default-momentum* 0.3)

(defparameter *bianet-random-state* (make-random-state (u:reference-random-state)))

(defparameter *debug* t)

(defun reset-ids ()
  (with-mutex (*neuron-id-mutex*)
    (setf *next-neuron-id* 0))
  (with-mutex (*cx-id-mutex*)
    (setf *next-cx-id* 0)))

(defun next-neuron-id ()
  (with-mutex (*neuron-id-mutex*)
    (incf *next-neuron-id*)))

(defun next-neuron-id-peek ()
  (with-mutex (*neuron-id-mutex*)
    (1+ *next-neuron-id*)))

(defun next-cx-id ()
  (with-mutex (*cx-id-mutex*)
    (incf *next-cx-id*)))

(defun next-cx-id-peek ()
  (with-mutex (*cx-id-mutex*)
    (1+ *next-cx-id*)))

(defun next-weight ()
  (with-mutex (*weight-mutex*)
    (let ((min -0.5)
          (max 0.5))
      (+ min (random (- max min) *bianet-random-state*)))))

(defun reset-random-state ()
  (setf *bianet-random-state* (make-random-state (u:reference-random-state))))

(defun reset-state ()
  (reset-ids)
  (reset-random-state))

(defun logistic (x)
  (/ 1.0 (1+ (exp (- x)))))

(defun logistic-derivative (x)
  (* x (- 1.0 x)))

(defun relu (x)
  (max 0.0 x))

(defun relu-derivative (x)
  (if (> x 0) 1.0 0.0))

(defun biased-transfer (x)
  (declare (ignore x))
  1.0)

(defun biased-derivative (x)
  (declare (ignore x))
  0.0)

(defparameter *transfer-functions*
  (list :logistic (list :function #'logistic
                        :derivative #'logistic-derivative)
        :relu (list :function #'relu
                    :derivative #'relu-derivative)
        :biased (list :function #'biased-transfer
                      :derivative #'biased-derivative)))

(defclass t-cx ()
  ((id :reader id :type integer :initform (next-cx-id))
   (source :reader source :initarg :source :type t-neuron
           :initform (error ":source required"))
   (target :reader target :initarg :target :type t-neuron
           :initform (error ":target required"))
   (weight :accessor weight :initarg :weight :initform (next-weight) 
           :type float)
   (weight-last :accessor weight-last :initform 0.0 :type float)
   (learning-rate :accessor learning-rate :initarg :learning-rate
                  :type float :initform 0.02)
   (momentum :accessor momentum :initarg :momentum :type float
             :initform 0.1)
   (delta :accessor delta :initarg :delta :type float :initform 0.0)
   (fire-count :accessor fire-count :type integer :initform 0)
   (update-count :accessor update-count :type integer :initform 0)))

(defclass t-neuron ()
  ((id :reader id :type integer :initform (next-neuron-id))
   (input :accessor input :type float :initform 0.0)
   (input-last :accessor input-last :type float :initform 0.0)
   (biased :accessor biased :initarg :biased :type boolean :initform nil)
   (layer :accessor layer :initarg :layer :type integer :initform 0)
   (transfer-key :accessor transfer-key :initarg :transfer-key
                 :initform :logistic)
   (transfer-function :accessor transfer-function :type function)
   (transfer-derivative :accessor transfer-derivative :type function)
   (transfer-count :accessor transfer-count :type integer :initform 0)
   (output :accessor output :type float :initform 0.0)
   (output-last :accessor output-last :type float :initform 0.0)
   (err :accessor err :type float :initform 0.0)
   (err-last :accessor err-last :type float :initform 0.0)
   (err-in :accessor err-in :type float :initform 0.0)
   (err-in-last :accessor err-in-last :type float :initform 0.0)
   (excitation-count :accessor excitation-count :type integer :initform 0)
   (modulation-count :accessor modulation-count :type integer :initform 0)
   (modulator-count :accessor modulator-count :type integer :initform 0)
   (excited :accessor excited :type boolean :initform nil)
   (modulated :accessor modulated :type boolean :initform nil)
   (incoming :accessor incoming :type dl:dlist 
             :initform (make-instance 'dl:dlist))
   (outgoing :accessor outgoing :type dl:dlist 
             :initform (make-instance 'dl:dlist))
   (ff-count :accessor ff-count :type integer :initform 0)
   (bp-count :accessor bp-count :type integer :initform 0)
   (on-input-ready :accessor on-input-ready
                   :type (or function null)
                   :initarg :on-input-ready
                   :initform nil)
   (on-output-ready :accessor on-output-ready
                    :type (or function null)
                    :initarg :on-output-ready
                    :initform nil)
   (process-count :accessor process-count :type integer :initform 0)
   (process-mutex :accessor process-mutex :type mutex
                  :initform (make-mutex :name "process-mutex"))
   (i-mailbox :accessor i-mailbox :type mailbox :initform (make-mailbox))
   (e-mailbox :accessor e-mailbox :type mailbox :initform (make-mailbox))
   (job-queue :accessor job-queue :initarg :job-queue :type (or null function)
              :initform nil)
   (running :accessor running :type boolean :initform t)))

(defmethod initialize-instance :after ((neuron t-neuron) &key)
  (let ((transfer (getf *transfer-functions*
                        (if (biased neuron) :biased (transfer-key neuron)))))
    (setf (transfer-function neuron) (getf transfer :function)
          (transfer-derivative neuron) (getf transfer :derivative))))

(defmethod name ((neuron t-neuron))
  "Creates a string that identifies this neuron. The string consists
 of the neuron's layer and ID."
  (format nil "~a-~a" (layer neuron) (id neuron)))

(defmethod transfer ((neuron t-neuron))
  (let ((new-output (funcall (transfer-function neuron) (input neuron))))
    (setf (output-last neuron) (output neuron)
          (output neuron) new-output
          (input-last neuron) (input neuron))
    (unless (biased neuron)
      (setf (input neuron) 0.0))
    (incf (transfer-count neuron))
    (output neuron)))

(defmethod fire-output ((neuron t-neuron))
  (loop for cx-node = (dl:head (outgoing neuron)) then (dl:next cx-node)
        while cx-node
        for cx = (dl:value cx-node)
        for target = (target cx)
        do (excite target (* (output neuron) (weight cx)))
           (incf (fire-count cx))))

(defmethod transfer-error ((neuron t-neuron))
  (setf (err-last neuron) (err neuron)
        (err neuron) (* (funcall (transfer-derivative neuron) (output neuron))
                        (err-in neuron))
        (err-in-last neuron) (err-in neuron)
        (err-in neuron) 0.0)
  (err neuron))

(defmethod fire-error ((neuron t-neuron))
  (unless (biased neuron)
    (loop for cx-node = (dl:head (incoming neuron)) then (dl:next cx-node)
          while cx-node
          for cx = (dl:value cx-node)
          for upstream-neuron = (source cx)
          for weight = (weight cx)
          for err = (err neuron)
          do (modulate upstream-neuron (* err weight)))))

(defmethod adjust-weights ((neuron t-neuron))
  (loop for cx-node = (dl:head (outgoing neuron)) then (dl:next cx-node)
        while cx-node
        for cx = (dl:value cx-node)
        do (adjust-weight cx)))

(defmethod adjust-weight ((cx t-cx))
  (multiple-value-bind (new-weight new-delta)
      (compute-new-weight (weight cx)
                          (delta cx)
                          (err (target cx))
                          (output (source cx))
                          (learning-rate cx)
                          (momentum cx))
    (setf (delta cx) new-delta
          (weight-last cx) (weight cx)
          (weight cx) new-weight)
    (incf (update-count cx))))

(defun compute-new-weight (old-weight
                           old-delta
                           target-error
                           source-output
                           learning-rate
                           momentum)
  (let* ((new-delta (+ (* learning-rate target-error source-output)
                       (* momentum old-delta))))
    (values (+ old-weight new-delta) new-delta)))

(defmethod process ((neuron t-neuron))
  (with-mutex ((process-mutex neuron))
    (evaluate-input-messages neuron)
    (evaluate-error-messages neuron)
    (when (excited neuron)
      (transfer neuron)
      (fire-output neuron)
      (incf (ff-count neuron))
      (setf (excited neuron) nil)
      (setf (excitation-count neuron) 0)
      (when (on-output-ready neuron) 
        (funcall (on-output-ready neuron))))
    (when (modulated neuron)
      (transfer-error neuron)
      (fire-error neuron)
      (adjust-weights neuron)
      (incf (bp-count neuron))
      (setf (modulated neuron) nil)
      (setf (modulation-count neuron) 0)
      (when (on-input-ready neuron)
        (funcall (on-input-ready neuron))))
    (incf (process-count neuron))))

(defmethod evaluate-input-messages ((neuron t-neuron))
  (loop until (mailbox-empty-p (i-mailbox neuron))
        for value = (receive-message (i-mailbox neuron))
        do (excite-internal neuron value)))

(defmethod evaluate-error-messages ((neuron t-neuron))
  (loop until (mailbox-empty-p (e-mailbox neuron))
        for err = (receive-message (e-mailbox neuron))
        do (modulate-internal neuron err)))

(defmethod excite-internal ((neuron t-neuron) value)
  (unless (biased neuron)
    (incf (input neuron) value))
  (incf (excitation-count neuron))
  (when (or (zerop (dl:len (incoming neuron)))
            (= (excitation-count neuron) (dl:len (incoming neuron))))
    (setf (excited neuron) t)))

(defmethod excite ((neuron t-neuron) value)
  (send-message (i-mailbox neuron) value)
  (when (job-queue neuron)
    (funcall (job-queue neuron) neuron))
  value)

(defmethod modulate-internal ((neuron t-neuron) err)
  (incf (err-in neuron) err)
  (incf (modulation-count neuron))
  (when (or (zerop (dl:len (outgoing neuron)))
            (= (modulation-count neuron) (modulator-count neuron)))
    (setf (modulated neuron) t)))

(defmethod modulate ((neuron t-neuron) err)
  (send-message (e-mailbox neuron) err)
  (when (job-queue neuron)
    (funcall (job-queue neuron) neuron))
  err)

(defmethod connect ((source t-neuron)
                    (target t-neuron)
                    &key
                      (weight (error "weight is required"))
                      (learning-rate *default-learning-rate*)
                      (momentum *default-momentum*))
  (when (and
         (loop
           for cx-node = (dl:head (outgoing source)) then (dl:next cx-node)
           while cx-node
           for cx = (dl:value cx-node)
           for cx-target = (target cx)
           never (= (id target) (id cx-target)))
         (loop
           for cx-node = (dl:head (incoming target)) then (dl:next cx-node)
           while cx-node
           for cx = (dl:value cx-node)
           for cx-source = (source cx)
           never (= (id source) (id cx-source))))
    (let ((cx (make-instance 't-cx
                             :momentum momentum
                             :learning-rate learning-rate
                             :weight weight
                             :source source
                             :target target)))
      (dl:push-tail (outgoing source) cx)
      (dl:push-tail (incoming target) cx)
      (unless (biased target)
        (incf (modulator-count source)))
      cx)))

(defmethod disconnect ((source t-neuron) (target t-neuron))
  (loop for cx-node = (dl:head (outgoing source)) then (dl:next cx-node)
        while cx-node
        for cx = (dl:value cx-node)
        for cx-target = (target cx)
        when (= (id target) (id cx-target))
          do (dl:delete-node (outgoing source) cx-node))
  (loop for cx-node = (dl:head (incoming target)) then (dl:next cx-node)
        while cx-node
        for cx = (dl:value cx-node)
        for cx-source = (source cx)
        when (= (id source) (id cx-source))
          do (dl:delete-node (incoming target) cx-node)
             (return cx)))

(defmethod isolate ((neuron t-neuron))
  (let ((source (loop
                  for cx-node = (dl:head (incoming neuron)) then (dl:next cx-node)
                  while cx-node
                  for cx = (dl:value cx-node)
                  for source = (source cx)
                  do (disconnect source neuron)
                  counting cx))
        (target (loop
                  for cx-node = (dl:head (outgoing neuron)) then (dl:next cx-node)
                  while cx-node
                  for cx = (dl:value cx-node)
                  for target = (target cx)
                  do (disconnect neuron target)
                  counting cx)))
    (list :incoming source :outgoing target)))

(defmethod list-incoming ((neuron t-neuron))
  (dl:to-list (incoming neuron)))

(defmethod list-incoming ((neurons list))
  (reduce #'append (mapcar #'list-incoming neurons)))

(defmethod list-incoming-weights ((neuron t-neuron))
  (mapcar (lambda (cx)
            (list :source (name (source cx))
                  :target (name (target cx))
                  :weight (weight cx)
                  :fire-count (fire-count cx)))
          (list-incoming neuron)))

(defmethod list-outgoing ((neuron t-neuron))
  (dl:to-list (outgoing neuron)))

(defmethod list-outgoing ((neurons list))
  (reduce #'append (mapcar #'list-outgoing neurons)))
