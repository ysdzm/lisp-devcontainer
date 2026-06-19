;; --- 補助関数の定義 ---
(defun assoc_ (x a)
  (cond ((eq (caar a) x) (cadar a))
        (t (assoc_ x (cdr a)))))

(defun pair_ (x y)
  (cond ((and (null x) (null y)) '())
        (t (cons (cons (car x) (car y))
                 (pair_ (cdr x) (cdr y))))))

(defun evlis_ (m a)
  (cond ((null m) '())
        (t (cons (eval_ (car m) a)
                 (evlis_ (cdr m) a)))))

(defun evcon_ (c a)
  (cond ((eval_ (caar c) a) (eval_ (cadar c) a))
        (t (evcon_ (cdr c) a))))

;; --- 評価器の定義をS式で記述 ---
(defparameter eval-def
  '(label eval_
     (lambda (e a)
       (cond
         ((atom e) (assoc_ e a))
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
            ((eq (car e) 'eval_) (eval_ (eval_ (cadr e) a) a))
            (t (eval_ (cons (assoc_ (car e) a)
                            (cdr e)) a))))
         ((eq (caar e) 'lambda)
          (eval_ (caddr e)
                 (pair_ (cadr (car e))
                        (evlis_ (cdr e) a))))
         (t (quote error))))))

;; --- 初期環境の構築 ---
(defparameter base-env
  (list
   (cons 'assoc_ #'assoc_)
   (cons 'pair_  #'pair_)
   (cons 'evlis_ #'evlis_)
   (cons 'evcon_ #'evcon_)))

;; --- 評価器を評価して、環境にeval_を導入 ---
(defparameter env (eval_ eval-def base-env))
