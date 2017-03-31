(defun pairhash (keys vals)
  (let ((table (make-hash-table)))
    (mapcar #'(lambda (key val) (setf (gethash key table) val)) keys vals)
    table))

(defun group (source n)
  (if (endp source) nil
      (let ((rest (nthcdr n source)))
	(cons (if (consp rest) (subseq source 0 n) source)
	      (group rest n)))))

(defmacro mac-1 (expr)
  `(pprint (macroexpand-1 ',expr)))

(defmacro mac (expr)
  `(pprint (macroexpand ',expr)))


(defun write-file (name content)
    (with-open-file (stream name
        :direction :output
        :if-exists :supersede
        :if-does-not-exist :create)
    (format stream content)))


(defun split-str-1 (string &optional (separator "-") (r nil))
  (let ((n (position separator string
		     :from-end t
		     :test #'(lambda (x y)
			       (find y x :test #'string=)))))
    (if n
	(split-str-1 (subseq string 0 n) separator (cons (subseq string (1+ n)) r))
      (cons string r))))

(defun split-str (string &optional (separator "-"))
  (split-str-1 string separator))


;; (defun lower (sym)
;;   (let ((words (mapcar #'string-capitalize (split-str (symbol-name sym)))))
;;     (format nil "~(~a~)~{~a~}" (car words) (cdr words))))
(defun interleave (lst sep)
  (if (equal 1 (length lst))
      lst
      (cons (car lst) (cons sep (interleave (cdr lst) sep)))))

(defun lower-camel (sym &optional (separator ""))
  (let ((words (interleave (mapcar #'string-capitalize (split-str (symbol-name sym))) separator)))
    (format nil "~(~a~)~{~a~}" (car words) (cdr words))))

(defun upper-camel (sym &optional (separator ""))
  (let ((words (interleave (mapcar #'string-capitalize (split-str (symbol-name sym))) separator)))
    (format nil "~{~a~}" words)))

(defun flatten (ls &key (test #'atom))
  (labels ((mklist (x) (if (listp x) x (list x))))
    (mapcan #'(lambda (x) (if (funcall test x) (mklist x) (flatten x :test test))) ls)))

(defun flat (ls &optional (test #'atom))
  (if (or (null ls)
	  (funcall test ls))
      ls
      (if (funcall test (car ls))
	  (cons (car ls) (flat (cdr ls) test))
	  (concatenate 'list (flat (car ls) test) (flat (cdr ls) test)))))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))
(defun keyw (&rest args)
  (values (intern (apply #'mkstr args) "KEYWORD")))
(defun ppair-p (pair)
  (and (consp pair)
       (typep (car pair) 'symbol)))
(defun plist-p (lst)
  (every #'ppair-p (group lst 2)))

(defun pairprop (key val)
  (apply #'append (mapcar #'list (mapcar #'symb key) val)))

(defmacro bindall (bindings form)
  (if (null bindings)
      form
      (let ((binding (car bindings)))
	`(multiple-value-bind ,(butlast binding) ,@(last binding)
	   (bindall ,(cdr bindings) ,form)))))

(defun csplice (cond &rest exps)
  (if cond
      `(,@exps)))

(defmacro evnames (&rest actions)
  `(append ,@(mapcar #'(lambda (action) 
                        `(if ,action (list ,(keyw "(" action ")") (concatenate 'string (lower-camel ',action) (upper-camel name) "()"))))
                    actions)))
;; (pprint (bindall ((x y (values 1 2))
;; 		  (z 4)) (+ x y z)))
;; (let ((l (pairprop '(a b) '(1 2)))) (pprint (getf l 'a)))

;; (defmacro define)


;; (setf *print-readably* t)

;; (setf *print-pretty* t)
;; (set-pprint-dispatch 'hash-table
;; 		     (lambda (str ht)
;; 		       (format str "{~{~{~S => ~S~}~^, ~}}"
;; 			       (loop for key being the hash-keys of ht
;; 				  for value being the hash-values of ht
;; 				  collect (list key value)))))

;; (defmacro unless (condition &rest body)
;;   `(if (not ,condition) (progn ,@body)))
