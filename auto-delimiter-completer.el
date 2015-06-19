
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(make-variable-buffer-local 'removed-shit)
(make-variable-buffer-local 'delimiter-stack)
(setq removed-shit "")
(setq delimiter-stack (list) )
(setq open-delimiter-list (list ?\( ?\[ ?\< ?\{ ))
(setq close-delimiter-list (list ?\) ?\] ?\> ?\} ))
(setq neutral-delimiter-list (list ?\' ?\"))

(defun is-opening-charp? (char)
  (member char open-delimiter-list)
  )

(defun is-closing-charp? (char)
  (member char close-delimiter-list)
  )

(defun get-matching-helper (open-delimiter open-list closed-list)
  (if (and open-list closed-list)
      (if (eq (car open-list) open-delimiter) (car closed-list) (get-matching-helper open-delimiter (cdr open-list) (cdr closed-list)))
    \??)
  )

(defun get-matching (open-delimiter)
  (get-matching-helper open-delimiter open-delimiter-list close-delimiter-list)
  )
  
(defun process-string-added (string)
  ;(message (format "String is %s" string))
  (dotimes (i (length string))
    (if (is-opening-charp? (aref string i)) (setq delimiter-stack (cons (aref string i) delimiter-stack)))
    (if (is-closing-charp? (aref string i)) (setq delimiter-stack (cdr delimiter-stack)))
    )
  )
(defun add-delimiter-in ()
  (interactive)
  (insert-char (get-matching (car delimiter-stack)))
  )
(setq only-once nil)
(defun paren-adding-parser-once (begin end size-removed)
  (cond ((not only-once)
	 (setq only-once t)
	 (paren-adding-parser begin end size-removed)
	 )
	(t nil)
	)
  (setq only-once nil)
  )
(defun paren-adding-parser (begin end size-removed)
  (if (and (not (= begin end) )(not (= size-removed 0)))
      (process-string-added (""))   ;Removed stuff goes here 
    (process-string-added (buffer-substring begin end))
    )
  )
(add-hook 'after-change-functions 'paren-adding-parser-once)

(defun paren-removing-parser (begin end)
  (setq removed-shit (buffer-substring begin end))
  )



(add-hook 'before-change-functions 'paren-removing-parser)


;; (defun is-opening-charp? (char) 
;;   (cond ((eq (char-syntax char) 40) t)
;; 	((eq (char-syntax char) 95) t)
;; 	(t nil)
;; 	)
;;   )

;; (defun is-closing-charp? (char) 
;;   (cond ((eq (char-syntax char) 40) t)
;; 	((eq (char-syntax char) 95) t)
;; 	(t nil)
;; 	)
;;   )
				


(eq (char-syntax (following-char)) something)
  modify-syntax-entry is an interactive built-in function in `C source
code'.

(modify-syntax-entry CHAR NEWENTRY &optional SYNTAX-TABLE)
;;(char-syntax ?\}) This maybe???
;;(char-syntax (aref "aas" 0)) THIS!


Set syntax for character CHAR according to string NEWENTRY.
The syntax is changed only for table SYNTAX-TABLE, which defaults to
 the current buffer's syntax table.
CHAR may be a cons (MIN . MAX), in which case, syntaxes of all characters
in the range MIN to MAX are changed.
The first character of NEWENTRY should be one of the following:
  Space or -  whitespace syntax.    w   word constituent.
  _           symbol constituent.   .   punctuation.
  (           open-parenthesis.     )   close-parenthesis.
  "           string quote.         \   escape.
  $           paired delimiter.     '   expression quote or prefix operator.
  <           comment starter.      >   comment ender.
  /           character-quote.      @   inherit from parent table.
  |           generic string fence. !   generic comment fence.

Only single-character comment start and end sequences are represented thus.
Two-character sequences are represented as described below.
The second character of NEWENTRY is the matching parenthesis,
 used only if the first character is `(' or `)'.
Any additional characters are flags.
Defined flags are the characters 1, 2, 3, 4, b, p, and n.
 1 means CHAR is the start of a two-char comment start sequence.
 2 means CHAR is the second character of such a sequence.
 3 means CHAR is the start of a two-char comment end sequence.
 4 means CHAR is the second character of such a sequence.

There can be several orthogonal comment sequences.  This is to support
language modes such as C++.  By default, all comment sequences are of style
a, but you can set the comment sequence style to b (on the second character
of a comment-start, and the first character of a comment-end sequence) and/or
c (on any of its chars) using this flag:
 b means CHAR is part of comment sequence b.
 c means CHAR is part of comment sequence c.
 n means CHAR is part of a nestable comment sequence.

 p means CHAR is a prefix character for `backward-prefix-chars';
   such characters are treated as whitespace when they occur
   between expressions.

[back]
