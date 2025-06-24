(let ((env nil)) ; ← 初期環境は空
  (labels
      ((eval_ (e)
         (cond
           ((or (numberp e) (stringp e) (characterp e)) e)

           ((symbolp e)
            (let ((val (cdr (assoc e env))))
              (eval_ val)))

           ((and (consp e) (eq (car e) 'quote))
            (cadr e))

           ((and (consp e) (eq (car e) 'print))
            (apply #'print (mapcar #'eval_ (cdr e))))

           ((and (consp e) (eq (car e) '+))
            (apply #'+ (mapcar #'eval_ (cdr e))))

           ((and (consp e) (eq (car e) '*))
            (apply #'* (mapcar #'eval_ (cdr e))))

           ((and (consp e) (eq (car e) 'cons))
            (apply #'cons (mapcar #'eval_ (cdr e))))

           ((and (consp e) (eq (car e) 'eval_))
            (apply #'eval_ (mapcar #'eval_ (cdr e))))

           (t (error "Unknown expression: ~S" e)))))
    
    ;; 環境に hello と eval_ を登録
    (setf env
          (list
           (cons 'hello '(print (+ 1 (* 3 3)))) ; → 10
           (cons 'eval_ #'eval_)))
    
    ;; 1回評価（直接）
    (eval_ 'hello) ; → 10 を出力
    
    (eval_ '(eval_ '(eval_ '(eval_ 'hello))))

    ;; 2回評価（入れ子）
    (eval_ '(eval_ 'hello)))) ; → 同じく 10 を出力
