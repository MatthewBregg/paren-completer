;;;;IDEA!
;; A) If someone does complete all delimiters, then just rescan the whole buffer!
;; B) something like (disable-limiters ...) remvoes the limiters given to it from the delimiter list.
;; C) (add-limiters ...) takes in pairs of delimiters, and adds them to the list. 
;; Can't just use the current hooks, as if someone moves the cursor, it won't work!!!
;;Instead, just go through the whole buffer, it's not as slow as I thought.  Tested on a 128,000 line file, did it within a
;;second or two.... Since a 128000 file is going to be very rare, that should be fine....

(defgroup paren-completer nil
  "A package to automatically, language agnostically, fill in delimiters"
  :group 'convenience
  :link '(url-link "https://github.com/MatthewBregg/paren-completer")
  :version '1
  )


(make-variable-buffer-local 'paren-completer--delimiter-stack)
(make-variable-buffer-local 'paren-completer--last-processed-point)
(setq paren-completer--delimiter-stack (list) )
(setq paren-completer--last-processed-point -1)
(setq paren-completer--open-delimiter-list (list ?\( ?\[ ?\< ?\{ ))
(setq paren-completer--close-delimiter-list (list ?\) ?\] ?\> ?\} ))
(setq paren-completer--neutral-delimiter-list (list ?\' ?\"))

(defun paren-completer--is-opening-charp? (char)
  (member char paren-completer--open-delimiter-list)
  )

(defun paren-completer--is-closing-charp? (char)
  (member char paren-completer--close-delimiter-list)
  )

(defun paren-completer--get-matching-helper (open-delimiter open-list closed-list)
  (if (and open-list closed-list)
      (if (eq (car open-list) open-delimiter)
          (car closed-list) (paren-completer--get-matching-helper open-delimiter (cdr open-list) (cdr closed-list)))
    \??)
  )

(defun paren-completer--get-matching (open-delimiter)
  (paren-completer--get-matching-helper
   open-delimiter paren-completer--open-delimiter-list paren-completer--close-delimiter-list)
  )

(defun paren-completer--process-string-added (string)
  ;(message (format "String is %s" string))
  (dotimes (i (length string))
    (if (paren-completer--is-opening-charp? (aref string i))
        (setq paren-completer--delimiter-stack (cons (aref string i) paren-completer--delimiter-stack)))
    (if (paren-completer--is-closing-charp? (aref string i))
        (setq paren-completer--delimiter-stack (cdr paren-completer--delimiter-stack)))
    )
  )

(defun paren-completer--get-string-upto-point ()
  (buffer-substring-no-properties 1 (point))
  )
(defun paren-completer--process-and-add-delimiter (delimiter-adder)
  (cond ((eq paren-completer--last-processed-point (point)) (add-delimiter-in))
        (t
         (setq paren-completer--delimiter-stack (list)) ;;Clear the list
         (paren-completer--process-string-added (paren-completer--get-string-upto-point));;GetTheCurrentBufferUpToPoint
         (funcall delimiter-adder);;Add the delimiter in, and end
         (setq paren-completer--last-processed-point (point)) ;;Set the point
         ))) 

(defun paren-completer--add-delimiter-in ()
  (if (eq paren-completer--delimiter-stack nil) (message "No delimiters to add?!")
  (insert-char (paren-completer--get-matching (car paren-completer--delimiter-stack))))
  (setq paren-completer--delimiter-stack (cdr paren-completer--delimiter-stack))
  )
(defun add-all-delimiters-in ()
  (paren-completer--add-delimiter-in)
  (if (eq paren-completer--delimiter-stack nil) (message "Done")
    (add-all-delimiters-in))
  )
    
     
(defun paren-completer--process-and-add-single-delimiter ()
  (interactive)
  (paren-completer--process-and-add-delimiter 'paren-completer--add-delimiter-in)
  )


(defun process-and-add-all-delimiters ()
  (interactive)
  (paren-completer--process-and-add-delimiter 'add-all-delimiters-in)
  )

