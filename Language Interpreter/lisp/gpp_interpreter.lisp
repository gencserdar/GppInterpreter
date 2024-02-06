(load "lexer.lisp")

(defvar final_dfa ()) 
(defvar result ())
(defvar funcNames())
(defvar functions())

(defun gppinterpreter ()
  "Read given input file line by line."
	(loop
	  	(setq final_dfa '())
	  	(setq result '())
		(let ((line (read-line *terminal-io* nil)))
			(if (or (null line) (string= "exit" line))
		      	(return)
		      	(progn
		        	(get_tokens line)
		        	;values is received from lexer.lisp to store values of every valuef to execute operations
	  			   	(setq values (reverse values))
	  			   	;variables is received from lexer.lisp to store name of every identifier to understand function names
	  			   	(setq variables (reverse variables))
	  			   	;call parser function by sending tokens to it
		          	(my_parser (reverse tokens))
		          	;if the last dfa is $EXP its an expression and print result
		           	(if (equal final_dfa '("$EXP"))
	          			(format t "Result: ~a~%~%" (car result))
	          			;if the last dfa is $FUNCTION its an function def and there is no result so print #function
		              	(if (equal final_dfa '("$FUNCTION"))
							(format t "#function~%~%")
							;print $_ and exit if exit kw is entered
			          		(if(equal (reverse tokens) '("OP_OP" "KW_EXIT" "OP_CP"))
					      		(progn
					      			(format t "$_~%~%")
					      			(exit))
					      		;if nothing matches its a syntax error
					   			(if tokens (format t "Syntax error!~%~%" ))))))))))
				       			
		           			
               		   
(defun my_parser (my_list)
  	(setq i 0) ;iterator
  	(if (equal (second my_list) "KW_DEF") ;if function def
  	 	(progn
	  	  	(push (car variables) funcNames) ;save function name into funcnames
	  	  	(loop for i below (length my_list)
	  	  	    do (progn
	  	  	    	 (loop for j from i downto 0
	  	 				   do (let ((subsequence (subseq my_list j (1+ i)))) ;get every subseq of tokens
	  	 				   		(setq index_decrease_amount (is_expression_func subsequence)) ;send the subseq into is_expression function specific for function defs
	  	 				   		(if (> index_decrease_amount 0) ;if the subseq is an expression
	  	 				   			(progn
				                      (setq my_list (append (subseq my_list 0 j) '("$EXP") (subseq my_list (1+ i)))) ;write $EXP instead of the subseq 
				                      ; (format t "RESULT: ~a~%~%" my_list) ;uncomment this to see every step
				                      (if (and (> index_decrease_amount 1) (not (= 1 (length my_list)))) ;if finaldfa is not set and index needs to be updated 
				                          (setq i (- i index_decrease_amount))) ;update the index if subseq has more than 1 element 
				                      (when (equal my_list '("$EXP")) ;if $EXP is the all dfa left it means its an expression so push it to final_dfa
				                      	(push (car my_list) final_dfa)
				                        (return-from my_parser))))
				                	(if(is_function my_list) ;if function then return $function into final_dfa
										(progn
											(push '"$FUNCTION" final_dfa)
											(push '"OP_CP" functions)
											;(format t "~a~%"(reverse functions))
											(return-from my_parser))))))))
	
		(progn ;if not a function def
			(if (member (car variables) funcNames :test #'equal) ;check if its a function call and function name is included in function list
				(progn
					(setq vals '())
					(setq vars '())

					;get variable definitions for function in order
					(let ((start-iteration nil))
						(dolist (token (reverse functions))
							(if (equal token (car variables))
								(setq start-iteration t))

							(when start-iteration
							  (push token vars))

							(if (equal token '"OP_OP")
								(setq start-iteration nil))))
					(pop vars)
					(setq vars (butlast vars))

					;push values into vals in the same order as variables for function
					(let ((start-iteration nil))
						(dolist (token my_list)
							(if (equal token '"IDENTIFIER")
								(setq start-iteration t))

							(when start-iteration
							  (push (pop values) vals))

							(if (equal token '"OP_CP")
								(setq start-iteration nil))))
					(pop vals)
					(pop vals)
					(setq values '())
					
					;iterate the function and push the vals for every variable into values, replace the variable with VALUEF token 
					(let ((ready-iteration nil)
						  (start-iteration nil)
						  (result-tokens '()))
						  
					  (dolist (token (reverse functions))
						(if (equal token (car variables))
							(setq ready-iteration t))
						(if (and ready-iteration (equal token '"OP_OP"))
							(setq start-iteration t))
						(when start-iteration
							(if (is_val token) 
								(progn
									(setq result-tokens (append result-tokens '("VALUEF")))
									(push token values))
								(if (member token vars :test #'equal)
								  (progn
									(setq result-tokens (append result-tokens '("VALUEF")))
									(push (nth (position token vars :test #'equal) vals) values))
								  (setq result-tokens (append result-tokens (list token))))))
						;if token is kw_def it means the function called is finished and next function definition is starting so end iteration
						(if (equal token '"KW_DEF")
							(progn
							  (setq result-tokens (butlast result-tokens))
							  (setq result-tokens (butlast result-tokens))
							  (setq ready-iteration nil)
							  (setq start-iteration nil))))
							  
						(setq result-tokens (butlast result-tokens))
					    (setq my_list result-tokens) ;new tokens that defines expression 
					    (setq values (reverse values)))))
					  ;(format t "tokenlist: ~a~%~%" my_list)
					  ;(format t "values: ~a~%~%" values)
					   		
				;iterate through tokens
				(loop for i below (length my_list)
					do (progn
						 (loop for j from i downto 0
							   do (let ((subsequence (subseq my_list j (1+ i)))) ;call is_expression
									(setq index_decrease_amount (is_expression subsequence))
									(if (> index_decrease_amount 0)
									    (progn
									      (setq my_list (append (subseq my_list 0 j) '("$EXP") (subseq my_list (1+ i))))
									      ; (format t "RESULT: ~a~%~%" my_list) ;uncomment this to see every step
									      (if (and (> index_decrease_amount 1) (not (= 1 (length my_list))))
									          (setq i (- i index_decrease_amount)))
									      (when (equal my_list '("$EXP"))
									      	(push (car my_list) final_dfa)
									        (return-from my_parser)))))))))))


(defun is_function (my_list)       
	(cond 
		((equal my_list '("OP_OP" "KW_DEF" "$EXP" "$EXP" "OP_CP"))  1)
		((equal my_list '("OP_OP" "KW_DEF" "$EXP" "$EXP" "$EXP" "OP_CP"))  1)
		((equal my_list '("OP_OP" "KW_DEF" "$EXP" "$EXP" "$EXP" "$EXP" "OP_CP")) 1)
		(t nil)))
		 		
;the numbers returned by this function is returning index decrease amount
;for example if it is an adding operation it has "OP_OP" "OP_PLUS" "$EXP" "$EXP" "OP_CP" which is 5 elements
;but i replace it with 1 element and size is decreased by 4
;so i need to decrease the index according to this
(defun is_expression (my_list)
  (cond
    ((equal my_list '("VALUEF")) (push (pop values) result) 1) ;push the value to result list in which calculations will be done and at the end holds the result
    ((equal my_list '("IDENTIFIER")) 1)
    ((equal my_list '("OP_OP" "OP_PLUS" "$EXP" "$EXP" "OP_CP")) (add) 4) ;add function
    ((equal my_list '("OP_OP" "OP_MULT" "$EXP" "$EXP" "OP_CP")) (mul) 4) 
    ((equal my_list '("OP_OP" "OP_DIV" "$EXP" "$EXP" "OP_CP")) (div) 4)
    ((equal my_list '("OP_OP" "OP_MINUS" "$EXP" "$EXP" "OP_CP")) (sub) 4)
    ((equal my_list '("OP_OP" "$EXP" "$EXP" "OP_CP")) 3)
    ((equal my_list '("OP_OP" "$EXP" "$EXP" "$EXP" "OP_CP")) 4)
    ((equal my_list '("OP_OP" "$EXP" "$EXP" "$EXP" "$EXP" "OP_CP")) 5)
    (t 0)))

;in this func i check if its an expression and also push the function definitions into a functions list
;to make it possible to call defined functions
(defun is_expression_func (my_list)
  (cond
    ((equal my_list '("VALUEF")) (push (pop values) functions) 1)
    ((equal my_list '("IDENTIFIER")) (push (pop variables) functions) 1) ;push the variables
    ((equal my_list '("OP_OP" "OP_PLUS" "$EXP" "$EXP" "OP_CP")) 4)
    ((equal my_list '("OP_OP" "OP_MULT" "$EXP" "$EXP" "OP_CP")) 4)
    ((equal my_list '("OP_OP" "OP_DIV" "$EXP" "$EXP" "OP_CP")) 4)
    ((equal my_list '("OP_OP" "OP_MINUS" "$EXP" "$EXP" "OP_CP")) 4)
    ((equal my_list '("OP_OP" "$EXP" "$EXP" "OP_CP")) 3)
    ((equal my_list '("OP_OP" "$EXP" "$EXP" "$EXP" "OP_CP")) 4)
    ((equal my_list '("OP_OP" "$EXP" "$EXP" "$EXP" "$EXP" "OP_CP")) 5)
    ;push operators and kw def
    ((equal my_list '("OP_OP")) (push (car my_list) functions) 0)
    ((equal my_list '("OP_CP")) (push (car my_list) functions) 0)
    ((equal my_list '("OP_PLUS")) (push (car my_list) functions) 0)
    ((equal my_list '("OP_MINUS")) (push (car my_list) functions) 0)
    ((equal my_list '("OP_MULT")) (push (car my_list) functions) 0)
    ((equal my_list '("OP_DIV")) (push (car my_list) functions) 0)
    ((equal my_list '("KW_DEF")) (push (car my_list) functions) 0)
    (t 0)))

;returns true if string is a value
(defun is_val (string)
  (let ((len (length string))
        (b-counter 0)
        (digit-counter 0))
    (if (not (digit-char-p (char string (1- len))))
        nil
        (loop for i from 0 below len
              do (let ((char (char string i)))
                   (cond
                     ((char= char #\b) (incf b-counter))
                     ((digit-char-p char) (incf digit-counter))
                     (t (return nil))))
              finally
              (let ((is-value (and (= b-counter 1) (= digit-counter (- len 1)))))
                (if is-value
					(return-from is_val t)))))))
                
(defun add ()
  (let ((decimal1 (pop result))
        (decimal2 (pop result)))
    (let* ((decimal_point2 (position #\b decimal2))
           (decimal_point1 (position #\b decimal1)))
      (let* ((numerator2 (parse-integer (subseq decimal2 0 decimal_point2)))
             (denominator2 (parse-integer (subseq decimal2 (1+ decimal_point2))))
             (numerator1 (parse-integer (subseq decimal1 0 decimal_point1)))
             (denominator1 (parse-integer (subseq decimal1 (1+ decimal_point1)))))
        (let* ((result_numerator (+ (* numerator1 denominator2) (* numerator2 denominator1)))
               (result_denominator (* denominator1 denominator2)))
          (push (format nil "~ab~a" result_numerator result_denominator) result))))))
          
(defun mul ()
  (let ((decimal1 (pop result))
        (decimal2 (pop result)))
    (let* ((decimal_point2 (position #\b decimal2))
           (decimal_point1 (position #\b decimal1)))
      (let* ((numerator2 (parse-integer (subseq decimal2 0 decimal_point2)))
             (denominator2 (parse-integer (subseq decimal2 (1+ decimal_point2))))
             (numerator1 (parse-integer (subseq decimal1 0 decimal_point1)))
             (denominator1 (parse-integer (subseq decimal1 (1+ decimal_point1)))))
        (let* ((result_numerator (* numerator1 numerator2))
               (result_denominator (* denominator1 denominator2)))
          (push (format nil "~ab~a" result_numerator result_denominator) result))))))
          
(defun div ()
  (let ((decimal2 (pop result))
        (decimal1 (pop result)))
    (let* ((decimal_point2 (position #\b decimal2))
           (decimal_point1 (position #\b decimal1)))
      (let* ((numerator2 (parse-integer (subseq decimal2 0 decimal_point2)))
             (denominator2 (parse-integer (subseq decimal2 (1+ decimal_point2))))
             (numerator1 (parse-integer (subseq decimal1 0 decimal_point1)))
             (denominator1 (parse-integer (subseq decimal1 (1+ decimal_point1)))))
        (let* ((result_numerator (* numerator1 denominator2))
               (result_denominator (* denominator1 numerator2)))
          (push (format nil "~ab~a" result_numerator result_denominator) result))))))
          
(defun sub ()
  (let ((decimal2 (pop result))
        (decimal1 (pop result)))
    (let* ((decimal_point2 (position #\b decimal2))
           (decimal_point1 (position #\b decimal1)))
      (let* ((numerator2 (parse-integer (subseq decimal2 0 decimal_point2)))
             (denominator2 (parse-integer (subseq decimal2 (1+ decimal_point2))))
             (numerator1 (parse-integer (subseq decimal1 0 decimal_point1)))
             (denominator1 (parse-integer (subseq decimal1 (1+ decimal_point1)))))
        (let* ((result_numerator (- (* numerator1 denominator2) (* numerator2 denominator1)))
               (result_denominator (* denominator1 denominator2)))
          (push (format nil "~ab~a" result_numerator result_denominator) result))))))

	
(gppinterpreter)
