(defun arg-list (sym args &key pref (func #'car))
  (let ((l (mapcar func (gethash sym args))))
    (if (null l)
	nil
	(if (null pref)
	    l
	    (cons pref l)))))

(defun key-par-name (par)
  (let ((par (car par)))
    (if par
        (cond ((symbolp par)
               par)
              ((consp par)
               (cond ((symbolp (car par))
                      (car par))
                     ((consp (car par))
                      (cadar par))))))))

(defun arg-names (lambda-list)
  (let* ((args (parse (destruc) lambda-list))
	 (req (arg-list 'req args))
	 (opt (arg-list 'opt args))
	 (rest (arg-list 'rest args))
	 (key (arg-list 'key args :func #'key-par-name)))
    (append req opt rest key)))

(pprint (arg-names '((a t1) (b t2) &optional (c t3) ((d 0 d-supplied-p) t4) &rest (e t5) &key (((:f-key f)) t6))))

(defmacro defprim (name args &rest attrs)
  `(defun ,name ,args
     (pandoriclet ,(mapcar #`(,a1 ,a1) (arguments args))
       (dlambda ,@attrs
                (t (&rest args) (apply this args))))))
(defun synth (att box &rest args)
  (apply box att args))
(defun synth-all (att boxlist &rest args)
  (mapcar (lambda (box) (apply #'synth att box args) boxlist)
          boxlist))



;; (defprim text (template &rest args)
;;   (:pretty () `(text (:template ,template :args ,args))))
;; (defprim vcat (&rest docs)
;;   (:pretty () `(vcat (:docs ,(synth-all :pretty docs))))
;;   (:output (indent) (let ((fdocs (flatten docs)))
;; 		     (unless (null fdocs) 
;; 		       (progn (synth :output (car fdocs) indent)
;; 			      (unless (null (cdr fdocs)) 
;; 				(progn (format t "~%"))
;; 				(synth :output (apply #'vcat (cdr fdocs)) indent))))))
;;   (:string (indent) (with-output-to-string (*standard-output*)
;; 			(synth :output this indent)))
;;   (:doc () this)  
;;   (:extent () (let ((fdocs (flatten docs)))
;; 		     (synth :extent (car (last fdocs))))))

;; (defun dtext (template &rest args)
;;   (pandoriclet ((template template)
;;                 (args args))
;;     (dlambda 
;;      (:pretty () `(text (:template ,template :args ,args)))
;;      (:self () this)
;;      (t (&rest args)
;;         (apply this args)))))

;; (defun ptext (template &rest args)
;;   (let ((template template)
;;         (args args))
;;     (plambda () (template args)
;;      (dlambda
;;       (:pretty () `(text (:template ,template :args ,args)))
;;       (:self () this)
;;       (t (&rest args)
;;          (apply this args))))))

;; (setf (symbol-function 'pptext) (ptext "ptext3: ~a" 30))
;; (with-pandoric (template args) #'pptext
;;   (format t "template: ~a, args: ~{~a~}~%" template args))
;; (pptext :pandoric-set 'args (list 31))
;; (funcall #'pptext :pandoric-set 'template "aa")
;; (with-pandoric (template args) #'pptext
;;   (format t "template: ~a, args: ~{~a~}~%" template args))


;; (let ((dtextmod (funcall (dtext "ptext2: ~a" 30) :pandoric-set 'template "aa")))
;;   (with-pandoric (template args) dtextmod
;;     (format t "template: ~a, args: ~{~a~}~%" template args)))

;; (let* ((dtest (dtext "ptext2: ~a" 30))
;;        (dtestmod (funcall dtest :pandoric-set 'template "aa")))
;;   (with-pandoric (template args) dtestmod
;;     (format t "template: ~a, args: ~{~a~}~%" template args)))

;; (let* ((ptest (ptext "ptext: ~a" 1))
;;        (dtest (dtext "dtext: ~a" 2)))
;;   (with-pandoric (template args) ptest
;;     (format t "template: ~a, args: ~{~a~}~%" template args))
;;   (with-pandoric (template args) (funcall ptest :pandoric-set 'args 31)
;;     (format t "template: ~a, args: ~{~a~}~%" template args))
;;   (with-pandoric (template args) dtest
;;     (format t "template: ~a, args: ~{~a~}~%" template args)))

;; (setf (symbol-function 'test) 
;;       (text "~a" 1))
;; (pprint (test :pretty))

;; (let* ((test (text "~a" 1))
;;        (test2 (funcall test :self)))
;;   (pprint (funcall test :pandoric-get 'template)))

;; (pprint (funcall (text "~a" 2) :pretty))
