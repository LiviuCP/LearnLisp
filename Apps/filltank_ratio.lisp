(load "../Libs/inputoutput.lisp")

(defmacro fill-tank(all-containers used-containers partially-used-container partially-used-capacity partially-added-value total-filled-value free-tank-capacity container-attributes)
  `(loop for container in ,all-containers
	do
	(let* ((current-capacity-value-pair (gethash (car container) ,container-attributes))
	       (current-capacity (car current-capacity-value-pair))
	       (current-value (cadr current-capacity-value-pair)))
	  (when (> current-capacity ,free-tank-capacity)
	    (setq ,partially-used-container (car container))
	    (setq ,partially-used-capacity ,free-tank-capacity)
	    (setq ,partially-added-value (* (/ ,partially-used-capacity current-capacity) current-value))
	    (setq current-capacity ,partially-used-capacity)
	    (setq current-value ,partially-added-value))
	  (setq ,used-containers (cons (car container) ,used-containers))
	  (setq ,total-filled-value (+ ,total-filled-value current-value))
	  (setq ,free-tank-capacity (- ,free-tank-capacity current-capacity))
	  (when (= ,free-tank-capacity 0)
	    (return)))))

(defmacro display-results(total-tank-capacity free-tank-capacity total-filled-value used-containers partially-used-container partially-used-capacity partially-added-value)
  `(progn
     (format t "Total tank capacity: ~d liter(s)~%" ,total-tank-capacity)
     (format t "Filled tank capacity: ~d liter(s)~%" (- ,total-tank-capacity ,free-tank-capacity))
     (format t "Filled tank value: ~,2f EUR~%" ,total-filled-value)
     (format t "~d containers used: ~a~%" (length ,used-containers) ,used-containers)
     (unless (null ,partially-used-container)
       (format t "Container ~s could only partially fit into tank!~%" ,partially-used-container)
       (format t "Capacity used for filling in the residual space: ~d liter(s)~%" ,partially-used-capacity)
       (format t "Value added when filling in the residual space: ~,2f EUR~%" ,partially-added-value))
     (when (= ,free-tank-capacity 0)
       (write-line "The whole tank has been filled! Congrats!"))))

(defun request-container-params()
  (let ((result))
    (princ "Enter the container name (press ENTER to stop input): ")
    (let ((container-name) (container-capacity) (container-value))
      (setq container-name (read-line))
      (when (> (length container-name) 0)
	(setq container-capacity (request-integer-input-with-condition "Enter the container capacity (press ENTER to stop input): " #'(lambda(val)(let ((is-greater)) (setq is-greater (> val 0)))) "The container capacity should be strictly positive. Please try again"))
	(unless (null container-capacity)
	  (setq container-value (request-integer-input-with-condition "Enter the container value (press ENTER to stop input): " #'(lambda(val)(let ((is-greater)) (setq is-greater (> val 0)))) "The container value should be strictly positive. Please try again")))
	(unless (null container-value)
	    (setq result (list container-name container-capacity container-value)))))
    (return-from request-container-params result)))

(defun main()
  (write-line "In this exercise you need to fill in a water tank of a specific capacity by using multiple containers of different capacities and values")
  (write-line "All containers are filled with water and the capacities are expressed in liters and the values in EUR")
  (write-line "The tank is initially empty")
  (write-line "You need to fill it by using the containers so the total value in EUR is maximized")
  (write-line "Partial container usage is allowed")
  (terpri)
  ;; step 1: enter tank capacity
  (let ((total-tank-capacity (request-integer-input-with-condition "Enter the requested tank capacity (press ENTER to quit): " #'(lambda(val)(let ((is-greater)) (setq is-greater (> val 0)))) "The number should be strictly positive. Please try again")))
    (if (not (null total-tank-capacity))
	(let ((container-attributes (make-hash-table :test #'equal))) ; this table is required for avoiding duplicate container names. If the user enters a container with same name the value from previous container is overwritten
	  ;; step 2: enter data for each container
	  (loop
	   (terpri)
	   (let ((params (request-container-params)))
	     (if (not (null params))
		 (setf (gethash (car params) container-attributes) (cdr params))
	       (return))))
	  (terpri)
	  ;; step 3: process tank/container data and display resulting tank fill info
	  (if (> (hash-table-count container-attributes) 0)
	      (let ((all-containers (list)) ; all available containers
		    (used-containers (list)) ; containers effectively used for filling in the tank
		    (total-filled-value 0) ; total value of the added fluid
		    (free-tank-capacity total-tank-capacity) ; how much is left to fill the tank
		    (partially-used-container) ; name of the container partially used (if any - this is the last container to be used whose capacity is higher than the remaining space within tank)
		    (partially-used-capacity) ; capacity filled with part of the last container
		    (partially-added-value)) ; value added from part of last container
		(loop for capacity-value-pair being the hash-values of container-attributes using (hash-key container-name)
		      do (setq all-containers (cons (list container-name (/ (cadr capacity-value-pair) (car capacity-value-pair))) all-containers)))
		(sort all-containers #'> :key #'cadr) ; use greedy algorithm for filling in the tank (start with container that has highest value per capacity unit)
		(fill-tank all-containers used-containers partially-used-container partially-used-capacity partially-added-value total-filled-value free-tank-capacity container-attributes)
		(sort used-containers #'string-lessp) ; used containers to be displayed in alphabetical order
		(display-results total-tank-capacity free-tank-capacity total-filled-value used-containers partially-used-container partially-used-capacity partially-added-value))
	    (write-line "No containers added to input!")))
      (write-line "You quit!"))))

(main)
