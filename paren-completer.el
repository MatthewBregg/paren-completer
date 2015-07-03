;;; paren-completer.el --- Complete Delimiters
;;; Version: 1.0
;;; package  --- Summary : A package to automatically, language agnostically, fill in delimiters.
;;; Commentary: 
;; See readme.org



;;;;IDEA!
;; A) If someone does complete all delimiters, then just rescan the whole buffer!
;; B) something like (disable-limiters ...) remvoes the limiters given to it from the delimiter list.
;; C) (add-limiters ...) takes in pairs of delimiters, and adds them to the list. 
;; Can't just use the current hooks, as if someone moves the cursor, it won't work!!!
;;Instead, just go through the whole buffer, it's not as slow as I thought.  Tested on a 128,000 line file, did it within a
;;second or two.... Since a 128000 file is going to be very rare, that should be fine....

;;; Code:


(defgroup paren-completer nil
  "A package to automatically, language agnostically, fill in delimiters"
  :group 'convenience
  :link '(url-link "https://github.com/MatthewBregg/paren-completer")
  :version '1
  )


(make-variable-buffer-local 'paren-completer--delimiter-stack)
(make-variable-buffer-local 'paren-completer--last-processed-point)
(defvar paren-completer--delimiter-stack (list) "List of un-matched delimiters.")
(defcustom paren-completer--open-delimiter-list (list ?\( ?\[ ?\< ?\{ )
  "List of opening delimiters to look for.  Must be in same order as close-delimiter-list.")
(defcustom paren-completer--close-delimiter-list (list ?\) ?\] ?\> ?\} )
  "List of closing delimiters to look for.  Must be in same order as open-delimiter-list.")
(defvar paren-completer--neutral-delimiter-list (list ?\' ?\") "List of nuetral delimiters.  Not used atm.")

(defun paren-completer--is-opening-charp? (char)
  "Checks if CHAR is an opening delimiter."
  (member char paren-completer--open-delimiter-list)
  )

(defun paren-completer--is-closing-charp? (char)
  "Checks if CHAR is a closing delimiter."
  (member char paren-completer--close-delimiter-list)
  )

(defun paren-completer--get-matching-helper (open-delimiter open-list closed-list)
  "Helper for get-matching.
OPEN-DELIMITER : Delimiter to look for.
OPEN-LIST : List of delimiters.
CLOSED-LIST : Matching closed list of delimiters.  Must be in same order as open list."
  (if (and open-list closed-list)
      (if (eq (car open-list) open-delimiter)
          (car closed-list) (paren-completer--get-matching-helper open-delimiter (cdr open-list) (cdr closed-list)))
    (message (concat "Error check integrity of delimiter-lists, no matching delimiter to " (format "%s" open-delimiter))))
  )

(defun paren-completer--get-matching (open-delimiter)
  "Return the matching delimiter to the OPEN-DELIMITER given."
  (paren-completer--get-matching-helper
   open-delimiter paren-completer--open-delimiter-list paren-completer--close-delimiter-list)
  )

(defun paren-completer--process-string-added (string)
  "Process given STRING to build delimiter list."
  ;(message (format "String is %s" string))
  (dotimes (i (length string))
    (if (paren-completer--is-opening-charp? (aref string i))
        (setq paren-completer--delimiter-stack (cons (aref string i) paren-completer--delimiter-stack)))
    (if (paren-completer--is-closing-charp? (aref string i))
        (setq paren-completer--delimiter-stack (cdr paren-completer--delimiter-stack)))
    )
  )

(defun paren-completer--get-string-upto-point ()
  "Get buffer-substring-with-no-properties up to point."
  (buffer-substring-no-properties 1 (point))
  )
(defun paren-completer--process-and-add-delimiter (delimiter-adder)
  "Process buffer up to point, then run given DELIMITER-ADDER function."
  (setq paren-completer--delimiter-stack (list)) ;;Clear the list
  (paren-completer--process-string-added (paren-completer--get-string-upto-point));;GetTheCurrentBufferUpToPoint
  (funcall delimiter-adder);;Add the delimiter in, and end
  )

(defun paren-completer--add-delimiter-in ()
  "Add a single delimiter."
  (if (eq paren-completer--delimiter-stack nil) (message "No delimiters to add?!")
  (insert-char (paren-completer--get-matching (car paren-completer--delimiter-stack))))
  (setq paren-completer--delimiter-stack (cdr paren-completer--delimiter-stack))
  )
(defun paren-completer--add-all-delimiters-in ()
  "Add all delimiters."
  (paren-completer--add-delimiter-in)
  (if (eq paren-completer--delimiter-stack nil) (message "Done")
    (paren-completer--add-all-delimiters-in))
  )
    
     
(defun paren-completer--process-and-add-single-delimiter ()
  "Process buffer, then add a delimiters."
  (interactive)
  (paren-completer--process-and-add-delimiter 'paren-completer--add-delimiter-in)
  )


(defun paren-completer--process-and-add-all-delimiters ()
  "Process buffer, then add all delimiters."
  (interactive)
  (paren-completer--process-and-add-delimiter 'paren-completer--add-all-delimiters-in)
  )

(provide 'paren-completer)
;;; paren-completer.el ends here
