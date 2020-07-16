(load "../Libs/inputoutput.lisp")
(load "../Libs/parse.lisp")

(defun requestContainerParams()
  (setq result nil)
  (setq containerName "")
  (setq containerCapacity nil)
  (princ "Enter the container name (press ENTER to stop input): ")
  (setq containerName (read-line))
  (when (> (length containerName) 0)
    (setq containerCapacity (requestIntInputWithCondition "Enter the container capacity (press ENTER to stop input): " #'(lambda(val)(setq isGreater (> val 0))) "The number should be strictly positive. Please try again"))
    (if (not (null containerCapacity))
	(setq result (list containerName containerCapacity))))
  (return-from requestContainerParams result)
)

(defun main()
  (write-line "In this exercise you need to fill in a water tank of a specific capacity by using multiple containers of different capacities")
  (write-line "All containers are filled with water and the capacities are expressed in liters")
  (write-line "The tank is initially empty")
  (terpri)
  (setq containers (list)) ; all available containers
  (setq usedContainers (list)) ; containers effectively used for filling in the tank
  (setq tankCapacity (requestIntInputWithCondition "Enter the requested tank capacity (press ENTER to quit): " #'(lambda(val)(setq isGreater (> val 0))) "The number should be strictly positive. Please try again"))
  (terpri)
  (if (null tankCapacity)
      (write-line "You quit!")
    (progn
      (loop
       (setq params (requestContainerParams))
       (if (null params)
	   (return)
	 (setq containers (cons params containers)))) ; it is possible to override values from hash, e.g. if the user has entered the wrong capacity for one of the containers
      (terpri)
      (if (= (length containers) 0)
	  (write-line "No containers added to input!")
	(progn
	  (sort containers #'> :key #'cadr) ; use greedy algorithm for filling in the tank (start with highest capacity container)
	  (setq remainingTankCapacity tankCapacity)
	  (loop for container in containers
	       do (when (<= (cadr container) remainingTankCapacity)
		 (setq usedContainers (cons container usedContainers))
		 (setq remainingTankCapacity (- remainingTankCapacity (cadr container)))
		 (if (= remainingTankCapacity 0)
		     (return))))
	  (if (= (length usedContainers) 0)
	      (write-line "None of the added containers could be used for filling in the tank as each one has a higher capacity")
	    (progn
	      (format t "Total tank capacity: ~d liters~%" tankCapacity)
	      (format t "Filled tank capacity: ~d liters~%" (- tankCapacity remainingTankCapacity))
	      (format t "~d containers used: ~a~%" (length usedContainers) (map 'list #'car usedContainers))
	      (if (= remainingTankCapacity 0)
		  (write-line "The whole tank has been filled! Congrats!"))))))))
)

(main)


