(defstruct tuple 
  (fst nil) 
  (snd nil))
(defun tuple (fst snd)
  (make-tuple :fst fst :snd snd)) 
(defun result (x)
  #'(lambda (s) 
      (list (tuple x s))))

(defun apply-parser (parser input)
  (funcall parser input))
(defun parse (parser input)
  (let ((res (car (apply-parser parser input)))) 
    (if (null res) 
	nil
	(tuple-fst res))))

(defun bind (p q)
  #'(lambda (s)
      (apply #'concatenate 
	     'list
	     (mapcar #'(lambda (tuple)
			 (let ((x (tuple-fst tuple))
			       (s1 (tuple-snd tuple)))
			   (apply-parser (funcall q x) s1)))
		     (apply-parser p s)))))

(defun fail ()
  #'(lambda (*) ()))
  
(defun item ()
  #'(lambda (s) (cond ((null s) ())
		      (t (list (tuple (car s) (cdr s)))))))


(defun do-with-fn (binds form)
  (if (null binds)
      form
      (case (length (car binds))
	(1 `(bind ,(caar binds) #'(lambda (*) ,(do-with-fn (cdr binds) form))))
	(2 `(bind ,(cadar binds) #'(lambda (,(caar binds)) ,(do-with-fn (cdr binds) form)))))))
(defmacro do-with (binds form)
  (do-with-fn binds form))

(defun sat (p)
  (do-with ((s (item)))
    (if (funcall p s)
	(result s)
	(fail))))

(defun sym (m)
  (do-with ((a (sat #'(lambda (x) (eq m x)))))
    (result a)))

(defun choose (p q)
  #'(lambda (s)
      (let ((ps (apply-parser p s)))
	(if (null ps)
	    (apply-parser q s)
	    ps))))

(defun zero ()
  #'(lambda (*) ()))
(defun plus (p q)
  #'(lambda (s)
      (append (apply-parser p s) (apply-parser q s))))
(defun choice (p q)
  #'(lambda (s)
      (let ((res (apply-parser (plus p q) s)))
	(if (null res)
	    nil
	    (car res)))))

(defun many (p)
  (choose (many1 p)
	  (result nil)))
(defun many1 (p)
  (do-with ((a p)
	    (as (many p)))
    (result (cons a as))))
(defun sepby (p sep)
  (choose (sepby1 p sep)
	  (result nil)))
(defun sepby1 (p sep)
  (do-with ((a p)
	    (as (many (do-with ((sep))
			p))))
    (result (cons a as))))

(defun pair ()
  (sat #'(lambda (x) (and (consp x) 
			  (eq (length x) 2)))))
;; (defun key-pair ()
;;   (sat #'(lambda (x) (and (consp x) 
;; 			  (eq (length x) 2)
;; 			  (consp (cadr x))
;; 			  (eq (length (cadr x)) 2)))))
(defun optional (p)
  (choose (do-with ((a p))
	    (result a))
	  (result nil)))

(defun destruc ()
  (do-with ((req (many (pair)))
	    (opt (optional (do-with (((sym '&optional))
				     (opts (many (pair))))
			     (result opts))))
	    (rest (optional (do-with (((sym '&rest))
				     (r (pair)))
			      (result (list r)))))
	    (key (optional (do-with (((sym '&key))
				     (keys (many (pair))))
			     (result keys)))))
    (result (pairhash '(req opt rest key) (list req opt rest key)))))

