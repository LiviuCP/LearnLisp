(load "../Libs/parse.lisp")

(defun main()
  (screen:clear-window (screen:make-window))
  (loop
   (princ "Enter an integer number from keyboard (press ENTER to exit): ")
   (setq input (read-line))
   (setq input (string-left-trim " " input))                                ; spaces from the left side don't count
   (setq input (string-right-trim " " input))                               ; neither the ones from right
   (cond ((= (length input) 0) (write-line "You exited") (return))
	 ((isStringInteger input) (write-line "The entered string is an integer value"))
	 (t (write-line "The entered string is NOT an integer value")))
   (terpri)))

(main)
