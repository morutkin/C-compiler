;; A basic top-down interpreter for the four-function calculator

(define eval_expr
  (lambda (exp env)
    (cond ((number? exp) exp)
    ((symbol? exp)  ((cadr(assoc exp env))))
    ((sum? exp)
     (+ (eval_expr (subexp1 exp) env)
        (eval_expr (subexp2 exp) env)))
    
    ((fcall? exp )
      
        ( let ((fname (cadr exp)) (aparams (caddr exp)))
          (let ((closure (get_closure fname env)))
            (let ((fparams (car closure))
              (fbody (cadr closure))
              (denv (caddr closure)))
            (let ((values (map (lambda(x)(eval_expr x env)) aparams)))
              (let ((bindings (map (lambda(x y)(list x y))
                fparams values)))
              (let ((working_env
                (list (append bindings (car denv))
                  (cadr denv))))
              (eval_expr fbody working_env))))))
        )) ;;eval_expr (body (eval_defs(vdef))) d
    ;;newly added cond 
    ((cond? exp) (if (eval_bool (cadr exp) env) (eval_expr (caddr exp) env)(eval_expr (cadddr exp) env)))
    ((difference? exp )
     (- (eval_expr (subexp1 exp) env)
        (eval_expr (subexp2 exp) env)))
    ((product? exp )
     (* (eval_expr (subexp1 exp) env)
        (eval_expr (subexp2 exp) env)))
    ((quotient? exp )
     (/ (eval_expr (subexp1 exp) env)
        (eval_expr (subexp2 exp) env)))

    (else (error 'eval_expr "invalid expression ~s" exp)))))

;; The following functions test the operation of arithmetic expressions
    ;;newly added call to fcalld




(define get_closure 
  (lambda (fname env)(
     (cadr (assoc (cadr (assoc fname (car env))) (cadr env))))))

(define fcall?
  (lambda (exp) (eq? (car exp) 'fcall)))

(define sum?
  (lambda (exp) (eq? (cadr exp) '+)))

(define difference?
  (lambda (exp) (eq? (cadr exp) '-)))

(define product?
  (lambda (exp) (eq? (cadr exp) '*)))

(define quotient?
  (lambda (exp) (eq? (cadr exp) '/)))
;;newly added call to cond
(define cond?
  (lambda (exp) (eq? (car exp) 'if)))

;; The following are functions to retrieve the appropriate subexpressions
;; from an arithmetic expression

(define subexp1
  (lambda (exp) (caddr exp)))

(define subexp2
  (lambda (exp) (cadddr exp)))





;;eval_bool expression added 
;;changed some of the comparison operators to match the grammar 
(define eval_bool
  (lambda (exp env)
    (cond ((comple exp)
     (< (eval_expr (subexp1 exp) env)
        (eval_expr (subexp2 exp) env)))
    ((compleq exp)
     (>= (eval_expr (subexp1 exp) env)
        (eval_expr (subexp2 exp) env)))
    ((compequ exp)
     (= (eval_expr (subexp1 exp) env)
        (eval_expr (subexp2 exp) env)))
    ((compgre exp)
     (> (eval_expr (subexp1 exp) env)
        (eval_expr (subexp2 exp) env)))
    ((compgreq exp)
      (>= (eval_expr (subexp1 exp) env)
        (eval_expr (subexp2 exp) env)))
    ((compneq exp)
      (!= (eval_expr (subexp1 exp) env)
        (eval_expr (subexp2 exp) env)))
    (else (error 'eval_expr "invalid expression ~s" exp)))))

(define comple
  (lambda (exp) (eq? (cadr exp) 'le)))

(define compleq
  (lambda (exp) (eq? (cadr exp) 'leq)))

(define compequ
  (lambda (exp) (eq? (cadr exp) 'eq)))

(define compgre
  (lambda (exp) (eq? (cadr exp) 'ge)))

(define compgreq
  (lambda (exp) (eq? (cadr exp) 'geq)))
(define compneq
  (lambda (exp) (eq? (cadr exp) 'neq)))

;;eval def 
;; eval_def creates the binding for a single definition 

(define vdef? (lambda (def) (eq? (car def) 'vdef)))
(define fdef? (lambda (def) (eq? (car def) 'fdef)))

(define eval_def 
  (lambda (def env) 
    (cond ((vdef? def)   
           (list (list (cadr def) (eval_expr (caddr def) env)) '()))

 ;; variable definition 
          ((fdef? def) 
            (let ((ref (gensym)))
              (let ((closure-env (cons (list (cadr def) ref) (car env))))
                (list (list (cadr def) ref)(list ref (list (caddr def)(cadddr def) closure-env))))))
 ;; function definition 
            (else (error "unknown definition type" def))) 
   )) 

;; eval_defs loops through multiple definitions, calling eval_def 
;; for each, and collects all the bindings into a list. 
(define eval_defs 
  (lambda (defs env) 
     (if (null? defs) env 
         (let ((new_binding (eval_def (car defs) env)))
               (eval_defs (cdr defs) (mymerge new_binding env)))))) ;;I dont understand why you cant put parentesis around merge new_binding env 

(define mymerge
  (lambda(new_binding env)
    (list (cons  (car new_binding) (car env)) ;;maybe add (list 
      (cons  (cadr new_binding) (cadr env)) )))





;; where "merge" would take the bindings returned from eval_def
;; and cons the two association lists to the current environment


;;; Bring in the parse tree and then evaluate it
;;; The parse tree has two parts:
;;; def_part -- this is the list of definitions
;;; exp_part -- this is the expression after all the definitions

(define compute
  (lambda ()
    (load "scheme_ast")  ;; << This needs to be consistent
                         ;; << with the file generated by ast.exe
    (let ((environment (eval_defs (def_part ptree) '(()()))))
      (eval_expr (exp_part ptree) environment))))

(define def_part
  (lambda (ptree) (cadr ptree)))

(define exp_part
  (lambda (ptree) (caddr ptree)))