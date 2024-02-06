(setq keyword-list
      (list "and" "or" "not" "equal" "less" "nil" "list" "append" "concat" "set" "def" "for" "if" "exit" "load" "display" "true" "false"))

(defvar tokens ()) 
(defvar values ()) 
(defvar variables ()) 

(defun get_tokens (line)
	(setq tokens '())
	(setq values '())
	(setq variables '())
        (split-line line))

(defun split-line (string)
  ;Split string into words
  (let ((words '()) (start nil))
    (loop for i from 0 to (1- (length string))
      do (if (char= (char string i) #\Space)
             (if start
                 (progn
                   (push (subseq string start i) words)
                   (setf start nil)))
             (unless start (setf start i)))
      finally (if start (push (subseq string start) words)))

    (dolist (word (reverse words))
      (if (equal (check-tokens word) 1)
          (return)))
    'done))

(defun check-tokens (word)
  ;check given tokens
  (if (string= word "")
      nil   ; if the word is empty
      (let ((char (char word 0)))
        (cond
         ((char= char #\;)
          1) ; return 1 if its a comment
         ((char= char #\()
          (push "OP_OP" tokens)
          (check-tokens (subseq word 1)))
         ((char= char #\))
          (push "OP_CP" tokens)
          (check-tokens (subseq word 1)))
         ((char= char #\+)
          (push "OP_PLUS" tokens)
          (check-tokens (subseq word 1)))
         ((char= char #\-)
          (push "OP_MINUS" tokens)
          (check-tokens (subseq word 1)))
         ((char= char #\/)
          (push "OP_DIV" tokens)
          (check-tokens (subseq word 1)))
         ((char= char #\*)
          (push "OP_MULT" tokens)
          (check-tokens (subseq word 1)))
         ((char= char #\,)
          (push "OP_COMMA" tokens)
          (check-tokens (subseq word 1)))
          
         (t ;if is not comment or operator check other possibilities
          (let ((cp-position (position #\) word)) (comment-position (position #\; word)))
            (progn
              (if comment-position 
		          (progn 
		              (check-tokens (subseq word 0 comment-position))
		        	  1)
		          (if cp-position
		            (progn
		              (if (not (is-keyword (subseq word 0 cp-position)))
		                  (if (digit-char-p (char (subseq word 0 cp-position) 0))
		                    (is-value (subseq word 0 cp-position))
							(is-identifier (subseq word 0 cp-position))))
		              (check-tokens (subseq word cp-position)))
		            (progn
		              (if (not (is-keyword word) )
		                  (if (digit-char-p (char word 0))
		                    (is-value word)
		                    (is-identifier word)))))))))))))

  
(defun is-keyword (string)
  ;check if keyword
  (let ((is-keyword (member string keyword-list :test #'string=)))
	(when is-keyword (push (concatenate 'string "KW_" (string-upcase string)) tokens))
    	is-keyword))

(defun is-value (string)
  ;check if value
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
                   (progn
		               	(push string values)
		               	(push "VALUEF" tokens))
		           (if (digit-char-p (char string 0))
		           		(progn
				       		(format t "Syntax Error: ~a~%"string)
				       		(exit))))
                is-value)))))

(defun is-identifier (str)
  ;check if identifier
  (let ((char-count 0)
        (digit-count 0))
    (dotimes (i (length str))
      (let ((char (char str i)))
        (cond
          ((alpha-char-p char) (incf char-count))
          ((digit-char-p char) (incf digit-count)))))
    (let ((is-identifier (= (+ char-count digit-count) (length str))))
      (if is-identifier 
      	(progn
      		(push str variables) 
      		(push "IDENTIFIER" tokens))
		(if (some (lambda (c) (member c '(#\! #\@ #\# #\$ #\% #\^ #\& #\[ #\] #\\ #\: #\' #\" #\` #\~ #\. #\< #\> #\? #\| #\_ #\=))) str)
			(progn
				(format t "Syntax Error: ~a~%"str)
				(exit))))
      is-identifier)))
