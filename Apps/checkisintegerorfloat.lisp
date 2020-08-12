(load "../Libs/parse.lisp")

(defun main()
  (screen:clear-window (screen:make-window))
  (loop
   (princ "Enter an integer number from keyboard (press ENTER to exit): ")
   (let ((input (read-line)))
     (setq input (string-left-trim " " input))                                ; spaces from the left side don't count
     (setq input (string-right-trim " " input))                               ; neither the ones from right
     (cond ((= (length input) 0) (write-line "You exited") (return))
	   ((convertStringToInt input) (write-line "The entered string is an integer value"))
	   ((convertStringToFloat input) (write-line "The entered string is a float value"))
	   (t (write-line "The entered string is neither an integer nor a float value"))))
   (terpri)))

(main)
