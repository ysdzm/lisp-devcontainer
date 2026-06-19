;; 評価器を let 内で定義（自己参照可能にする）
(let ((my-eval nil))
  (setq my-eval
        (lambda (e)
          (cond
            ;; リテラル
            ((or (stringp e) (numberp e) (characterp e)) e)

            ;; シンボル
            ((symbolp e)
             (cond ((eq e 'my-eval) my-eval)
                   (t (symbol-value e))))

            ;; print式
            ((and (consp e) (eq (car e) 'print))
             (apply #'print (mapcar my-eval (cdr e))))

            ;; lambda式
            ((and (consp e) (eq (car e) 'lambda)) e)

            ;; 関数適用
            ((consp e)
             (let* ((f (funcall my-eval (car e)))
                    (args (mapcar my-eval (cdr e))))
               (apply (eval f) args)))

            ;; その他
            (t (error "Unknown expression: ~S" e)))))


  ;; 評価器のテスト
  (funcall my-eval '(print "Hello from my-eval"))
  (funcall my-eval my-eval)) ; ← 自己評価もOK
