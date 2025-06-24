(let ((env nil))  ; 初期環境は空
  (labels
      ((eval_ (e)
         (cond
           ;; リテラル値はそのまま返す
           ((or (numberp e) (stringp e) (characterp e)) e)

           ;; シンボルは環境から探索して再帰評価
           ((symbolp e)
            (let ((val (cdr (assoc e env))))
              (eval_ val)))

           ;; quote式
           ((and (consp e) (eq (car e) 'quote))
            (cadr e))

           ;; print式
           ((and (consp e) (eq (car e) 'print))
            (apply #'print (mapcar #'eval_ (cdr e))))

           ;; 加算
           ((and (consp e) (eq (car e) '+))
            (apply #'+ (mapcar #'eval_ (cdr e))))

           ;; 乗算
           ((and (consp e) (eq (car e) '*))
            (apply #'* (mapcar #'eval_ (cdr e))))

           ;; cons構築
           ((and (consp e) (eq (car e) 'cons))
            (apply #'cons (mapcar #'eval_ (cdr e))))

           ;; eval_ の入れ子呼び出し
           ((and (consp e) (eq (car e) 'eval_))
            (apply #'eval_ (mapcar #'eval_ (cdr e))))

           (t (error "Unknown expression: ~S" e)))))

    ;; 環境に hello と eval_ を登録
    (setf env
          (list
           (cons 'hello '(print (+ 1 (* 3 3))))  ; hello → (print 10)
           (cons 'eval_ #'eval_)))               ; eval_ の参照登録

    ;; 評価実験
    (eval_ 'hello)                                        ; → 10
    (eval_ '(eval_ 'hello))                               ; → 10
    (eval_ '(eval_ '(eval_ '(eval_ 'hello))))))           ; → 10
