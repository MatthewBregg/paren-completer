a       aaaa



(

 )

{ }

(

 )

((  ))
o
( )
()
(
 )
((((((
      )))))

 ((((((( ))))))))



(((

)))
   )
(defun paren-complete--refactor()
  (interactive)
  (setq symbol (thing-at-point 'symbol))
  (query-replace (format "%s" symbol) (concat "paren-complete--" (format "%s" symbol)))
  )

		 
