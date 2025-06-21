(defun list_ (x y)
  (cons x (cons y '())))

(defun assoc_ (x y)
  (cond ((eq (caar y) x) (cadar y))
        (t (assoc_ x (cdr y)))))

(defun pair_ (x y)
  (cond ((and (null x) (null y)) '())
        ((and (not (atom x)) (not (atom y)))
         (cons (list_ (car x) (car y)) (pair_ (cdr x) (cdr y))))))

(defun evlis_ (m a)
  (cond ((null m) '())
        (t (cons (eval_ (car m) a) (evlis_ (cdr m) a)))))

(defun evcon_ (c a)
  (cond ((eval_ (caar c) a) (eval_ (cadar c) a))
        (t (evcon_ (cdr c) a))))

(defun eval_ (e a)
  (cond
    ((atom e)
     (if (or (numberp e) (stringp e) (characterp e) (eq e t) (eq e nil))
         e
         (assoc_ e a)))
    ((atom (car e))
     (cond
       ((eq (car e) 'quote) (cadr e))
       ((eq (car e) 'atom)  (atom (eval_ (cadr e) a)))
       ((eq (car e) 'eq)    (eq (eval_ (cadr e) a)
                                (eval_ (caddr e) a)))
       ((eq (car e) 'car)   (car (eval_ (cadr e) a)))
       ((eq (car e) 'cdr)   (cdr (eval_ (cadr e) a)))
       ((eq (car e) 'cons)  (cons (eval_ (cadr e) a)
                                  (eval_ (caddr e) a)))
       ((eq (car e) 'cond)  (evcon_ (cdr e) a))
       ((eq (car e) 'print) (print (eval_ (cadr e) a)))
       ((eq (car e) 'format)
        (apply #'format (mapcar (lambda (x) (eval_ x a)) (cdr e))))
       (t (eval_ (cons (assoc_ (car e) a) (cdr e)) a))))
    ((eq (caar e) 'lambda)
     (eval_ (caddar e)
            (append (pair_ (cadar e) (evlis_ (cdr e) a)) a)))
    ((eq (caar e) 'label)
     (eval_ (cons (caddar e) (cdr e))
            (cons (list_ (cadar e) (car e)) a)))))

(defun run-my-program (filename)
  (unless filename
    (error "ファイル名を指定してください。"))

  (with-open-file (in filename)
    (loop for expr = (read in nil :eof)
          while (not (eq expr :eof))
          do
            (eval_ expr '()))))
