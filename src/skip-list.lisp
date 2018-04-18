(in-package :skip-list)

(declaim (optimize (speed 3) (debug 0) (safety 0)))

;; sbcl --eval "(progn (ql:quickload 'skip-list) (skip-list::test))"

;; Each node contains lanes which containers pointers to other nodes on that
;; respective level
(defstruct (node (:conc-name ne-))
  data      ; arbitrary data
  spans     ; dist to next node for each lane/level
  forwards) ; next nodes

;; store top height?
(defstruct (skip-list (:conc-name sl-))
  height     ; max height
  length
  node-head)

(setf *random-state* (make-random-state t))

;; (defun random-from-range (start end)
;;   (+ start (random (+ 1 (- end start)))))

(declaim (inline generate-random-level))
(defun generate-random-level (height)
  (loop
     :with level := 0
     :while (and (< (random 1.0) 0.36787968862663156) ; p = 0.5, e=2.71828, 1/e = <-
		 (< level (1- height)))
     :do (incf level)
     :finally (return-from generate-random-level level)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline make-node))
(defun init-node (height i data)
  (make-node :index i
	     :data data
	     :spans (make-array height :fill-pointer nil :adjustable nil :initial-element 0)
	     :forwards (make-array height :fill-pointer nil :adjustable nil :initial-element nil)))

;; Rename header
(defun init-node-head (height)
  (make-node :index -1
	     :data nil
	     :spans (make-array height :fill-pointer nil :adjustable nil :initial-element 0)
	     :forwards (make-array height :fill-pointer nil :adjustable nil :initial-element nil)))

(defun init-skip-list (size &optional (preallocate nil))
  (let* ((height (truncate (+ 1 (log size 2.71828))))
	 (skip-list (make-skip-list :height height
				    :length 0
				    :node-head (init-node-head height))))
    ;; Preallocate nodes
    (when preallocate
      (loop
	 :for i :from 0 :below size
	 :do (progn
	       (insert skip-list 0 nil))))
    skip-list))

;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nth (sl-list i)
  (let ((node (sl-node-head sl-list)))
    (loop
       :for level :from (1- (sl-height sl-list)) :downto 0
       :with x = -1
       :do (loop
	      :while (and (aref (ne-forwards node) level)
			     (< (+ x (aref (ne-spans node) level)) i))
	      :for spans = (ne-spans node)
	      :for forwards = (ne-forwards node)
	      :do (progn
		    (incf x (aref spans level))
		    (setf node (aref forwards level)))))
    ;; Did not find it
    (if (eq node (sl-node-head sl-list))
	nil
	node)))

;; Raise error?
(defun get-nth-data (sl-list i)
  (let ((n (nth sl-list i)))
    (when n
      (ne-data n))))

(defun set-nth-data (sl-list i data)
  (let ((n (nth sl-list i)))
    (when n
      (setf (ne-data n) data))))

(defun insert (sl-list i data)
  
  (let* ((height (sl-height sl-list))
	 (height-new (generate-random-level height))
         (node-new (init-node (1+ height-new) i data)))

    ;; (format t "i: ~a, Height-max: ~a, Height-new: ~a~%" i height height-new)
    ;; (incf (gethash height-new *tracker*))
    
    ;; Create the new node then splice into existing nodes
    
    (loop
       :for level :from (1- height) :downto 0
       :with node = (sl-node-head sl-list) :and x = -1
       :do (progn

	     ;; (format t "Traversing level ~a...~%" level)
	     
	     ;; Find the backwards node (or node@i)
	     ;; Search until node and span@level is less than index
	     (loop
		:while (and (aref (ne-forwards node) level)
			       (< (+ x (aref (ne-spans node) level)) i))
		:for spans = (ne-spans node)
		:for forwards = (ne-forwards node)
		:do (progn
		      (incf x (aref spans level))
		      (setf node (aref forwards level))))

	     ;; Increase span for backwards node to account for new node
	     (incf (aref (ne-spans node) level))

	     ;; Update nodes that are <= height of new node
	     ;; These are nodes that link to the new node
	     (when (<= level height-new)
	       
	       ;; Link new-node to previous-node's forward
	       (setf (aref (ne-forwards node-new) level) (aref (ne-forwards node) level))
	       ;; Link previous-node's forward to node-new
	       (setf (aref (ne-forwards node) level) node-new)
	       ;; Update node-new's span = prev - (new - prev)
	       (setf (aref (ne-spans node-new) level) (- (aref (ne-spans node) level)
							 (- i x)))
	       ;; Update previous node's span
	       (setf (aref (ne-spans node) level) (- i x)))))
    
    (incf (sl-length sl-list))
    node-new))

(defun delete (sl-list i)

  ;; Implement delete range:
  ;; - Need to decrease each span by length of range
  
  (let ((node (sl-node-head sl-list))
	(data nil))
    
    (loop
       :for level :from (1- (sl-height sl-list)) :downto 0
       :with x = -1
       :do (progn

	     ;; Find node@i
	     (loop
		:while (and (aref (ne-forwards node) level)
			       (< (+ x (aref (ne-spans node) level)) i))
		:for spans = (ne-spans node)
		:for forwards = (ne-forwards node)
		:do (progn
		      (incf x (aref (ne-spans node) level))
		      (setf node (aref (ne-forwards node) level))))

	     ;; Dec span by 1 @ level
	     (decf (aref (ne-spans node) level))

	     ;; Update previous node
	     (when (and (= (+ x (aref (ne-spans node) level) 1) i)
			(aref (ne-forwards node) level))
	       ;; Incf span by span of rem node
	       (incf (aref (ne-spans node) level)
		     (aref (ne-spans (aref (ne-forwards node) level)) level))
	       ;; Set forward node to forward node of rem node
	       (setf (aref (ne-forwards node) level)
		     (aref (ne-forwards (aref (ne-forwards node) level)) level))
	       ;; If prev node is sentinel then decrease max height
	       (when (and (eq node (sl-node-head sl-list))
			  (aref (ne-forwards node) level))
		 (decf (sl-height sl-list))))))
    
    (decf (sl-length sl-list))
    data))

(defun enqueue (sl data)
  (insert sl 0 i))

(defun dequeue (sl)
  (delete sl 0))

(defun test ()
  (declaim (optimize (speed 3) (debug 0) (safety 0)))
  
  (setf *random-state* (make-random-state t))

  ;; 16 = ~64k
  ;; 20 = 1048576
  (let* ((n (expt 2 20))
	 (sl (init-skip-list n)))

    (defparameter *tracker* (make-hash-table :size (sl-height sl)))
    (dotimes (i (sl-height sl))
      (setf (gethash i *tracker*) 0))

    ;; Preallocate nodes
    (loop
       :for i :from 0 :below n
       :do (progn
	     (insert sl 0 "before")))

    (format t "~a~%" (sl-length sl))
    
    ;; Insert at end
    ;; (loop :for i :from 0 :below n :do (progn
    ;; 					(insert-sl sl i i)))
    ;; (loop :for i :from 0 :below n :do (progn
    ;; 					(delete-sl sl i)))
    
    ;; Insert at beginning
    (when nil
      (format t "Insert ~a elements...~%~%" n)
      (loop :for i :from 0 :below n
	 :do (progn
	       (insert sl 0 i))))

    (format t "Length: ~a~%" (sl-length sl))

    ;; Make nth zero based
    (format t "Before: ~a~%" (get-nth-data sl 1))
    (set-nth-data sl 1 "hello")
    (format t "After: ~a~%" (set-nth-data sl 1))
    
    (loop :for key :being :the :hash-keys :of *tracker*
       :using (hash-value value)
       :do (format t "Level ~S : ~S nodes, ~a~a ~%"
		   key
		   value
		   (coerce (* (/ value n) 100) 'single-float)
		   #\%))

    (when t
      (format t "Delete ~a elements...~%~%" n)
      (loop :for i :from 0 :below n
	 :do (progn
	       (delete sl 0))))
    
    t))
