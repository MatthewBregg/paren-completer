;;; paren-completer.el --- Automatically, language agnostically, fill in delimiters.  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Matthew Bregg

;; Author: Matthew Bregg
;; Mantainer: Matthew Bregg
;; Keywords: convenience
;; URL: https://github.com/MatthewBregg/paren-completer
;; Version: 1.3.2
;; Package-Requires: ((emacs "24.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides 4 functions to generically auto-complete delimiters.
;; Avoids use of syntax table, instead relies on user defined lists.(except for strings and comments)
;; See readme.org

;;; Code:

(defgroup paren-completer nil
  "A package to automatically, language agnostically, fill in delimiters"
  :group 'convenience
  :link '(url-link "https://github.com/MatthewBregg/paren-completer")
  :version '1
  )



(defcustom paren-completer--open-delimiter-list (list ?\( ?\[ ?\{ )
  "List of opening delimiters to look for.  Must be in same order as close-delimiter-list.")
(defcustom paren-completer--close-delimiter-list (list ?\) ?\] ?\} )
  "List of closing delimiters to look for.  Must be in same order as open-delimiter-list.")
;(defvar paren-completer--neutral-delimiter-list (list ?\' ?\") "List of nuetral delimiters.  Not used atm.")
(defcustom paren-completer--complete-stringsp? t "If true, will attempt to close strings as well.")
(defcustom paren-completer--ignore-commentsp? t "If true, don't check comments for delimiters.")
(defcustom paren-completer--ignore-stringsp? t "If true, don't check strings for delimiters.")

(defun paren-completer--is-opening-charp? (char)
  "Checks if CHAR is an opening delimiter."
  (member char paren-completer--open-delimiter-list)
  )

(defun paren-completer--is-closing-charp? (char)
  "Checks if CHAR is a closing delimiter."
  (member char paren-completer--close-delimiter-list)
  )
(defun paren-completer--is-in-stringp? (position)
  "Checks if the given POSITION is inside a string. Moves cursor to that position. Nil if not in string, else t."
       (nth 3 (syntax-ppss position))
       )

(defun paren-completer--is-in-commentp? (position)
  "Return the POSITION of the start of the current comment, else nil."
       (nth 8 (syntax-ppss position))
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
  (let ((old-point (point))
        (delimiter-stack (list)))
  (dotimes (i (length string))

    (if (paren-completer--is-opening-charp? (aref string i)) ;;Copypaste, will clean up later
        (cond ((and paren-completer--ignore-commentsp? (paren-completer--is-in-commentp? (+ 1 i))) nil)
              ((and paren-completer--ignore-stringsp? (paren-completer--is-in-stringp? (+ 1 i))) nil)
              (t
               (setq delimiter-stack (cons (aref string i) delimiter-stack)))))
      (if (paren-completer--is-closing-charp? (aref string i))
          (cond ((and paren-completer--ignore-commentsp? (paren-completer--is-in-commentp? (+ 1 i))) nil)
                ((and paren-completer--ignore-stringsp? (paren-completer--is-in-stringp? (+ 1 i))) nil)
                (t
                 (setq delimiter-stack (cdr delimiter-stack)))))
    )
  (syntax-ppss old-point)
  delimiter-stack))

(defun paren-completer--get-string-upto-point ()
  "Get buffer-substring-with-no-properties up to point."
  (buffer-substring-no-properties 1 (point))
  )
(defun paren-completer--process-and-add-delimiter (delimiter-adder)
  "Process buffer up to point, then run given DELIMITER-ADDER function."
  (let ((stack (paren-completer--process-string-added (paren-completer--get-string-upto-point))))
  ;;GetTheCurrentBufferUpToPoint
  (funcall delimiter-adder stack);;Add the delimiter in, and end
  ))

(defun paren-completer--add-delimiter (delimiter-stack)
  "Add a single delimiter.
DELIMITER-STACK : The delimiters found so far.
Messy, but the cond statement handles the
special cases of in a comment/string, and then in the end it returns
a list of open/closing delimiters."
(let ((string? (paren-completer--is-in-stringp? (point))))
  (cond ((and paren-completer--complete-stringsp? string?)
                                               (insert-char string?))
        (t
         (if (eq delimiter-stack nil) (message "No delimiters to add?!")
            (insert-char (paren-completer--get-matching (car delimiter-stack)))
           (setq delimiter-stack (cdr delimiter-stack))))))
  delimiter-stack)

(defun paren-completer--add-delimiter-with-newline (delimiter-stack)
  "Add a single delimiter with newline.
DELIMITER-STACK : The delimiters found so far"
  (let ((delimiter-stack (paren-completer--add-delimiter delimiter-stack)))
  (insert-char 10)
  delimiter-stack))

(defun paren-completer--add-all-delimiters-with-newline (delimiter-stack)
  "Add all delimiters with newline.
DELIMITER-STACK : The delimiters found so far"
  (let ((delimiter-stack (paren-completer--add-delimiter-with-newline delimiter-stack)))
  (if (eq delimiter-stack nil) (message "Done")
    (paren-completer--add-all-delimiters-with-newline delimiter-stack))
  delimiter-stack))
(defun paren-completer--add-all-delimiters (delimiter-stack)
  "Add all delimiters.
DELIMITER-STACK : The delimiters found so far"
  (let ((delimiter-stack (paren-completer--add-delimiter delimiter-stack)))
  (if (eq delimiter-stack nil) (message "Done")
    (paren-completer--add-all-delimiters delimiter-stack))
  delimiter-stack))
    
;;;###autoload
(defun paren-completer-add-single-delimiter ()
  "Process buffer, then add a delimiters."
  (interactive)
  (paren-completer--process-and-add-delimiter 'paren-completer--add-delimiter)
  )

;;;###autoload
(defun paren-completer-add-all-delimiters ()
  "Process buffer, then add all delimiters."
  (interactive)
  (paren-completer--process-and-add-delimiter 'paren-completer--add-all-delimiters)
  )

;;;###autoload
(defun paren-completer-add-single-delimiter-with-newline ()
  "Process buffer, then add a delimiters."
  (interactive)
  (paren-completer--process-and-add-delimiter 'paren-completer--add-delimiter-with-newline)
  )

;;;###autoload
(defun paren-completer-add-all-delimiters-with-newline ()
  "Process buffer, then add all delimiters."
  (interactive)
  (paren-completer--process-and-add-delimiter 'paren-completer--add-all-delimiters-with-newline)
  )


(provide 'paren-completer)
;;; paren-completer.el ends here
