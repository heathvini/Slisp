(defun not (s) 
	(if s nil t))

(defun consp (s) 
	(not (atom s)))

(defun len (s) 
	(if (cdr s)
		(+ (len (cdr s)) 1)
		1))

(defun last (s)
	(if (cdr s)
		(last (cdr s))
		(car s)))

(defun nth (n s)
	(if (eq n 0)
		(car s)
		(nth (- n 1) (cdr s))))
		
(defun append (x y) 
	(if (not x)
		y
		(cons (car x) (append (cdr x) y))))
		
(defun member (e s)
	(if (eq s nil)
		nil
		(if (eq e (car s))
			t
			(member e (cdr s)))))

(defun remove (e s)
	(if (not s)
		nil
		(if (eq e (car s))
			(remove e (cdr s))
			(cons (car s) (remove e (cdr s))))))
		
(defun reverse (s)
	(if (cdr s)
		(append (reverse (cdr s)) (list (car s)))
		s))
		
(defun apply (f s)
	(if (not s) 
		nil
		(eval (eval (cons f s)))))
		
(defun mapcar (f s)
	(if (not s) 
		nil
		(cons (eval (list f (list '(car s)))) 
			(mapcar f (cdr s)))))
			
(defun factorial (n)
	(if (or (eq n 0) (eq n 1))
		1
		(* n (factorial (- n 1)))))
		
(defun fibonacci (n)
	(if (or (eq n 1) (eq n 2))
		1
		(+ (fibonacci (- n 1)) (fibonacci (- n 2)))))		