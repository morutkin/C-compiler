(define eval_expr
  (lambda (exp combo_env)
    (cond ((number? exp) exp)
    ((symbol? exp)  (get_val exp combo_env)
    ((sum? exp)
     (+ (eval_expr (subexp1 exp) combo_env)
        (eval_expr (subexp2 exp) combo_env)))
    
    ((fcall? exp )
      
        ( let ((fname (cadr exp)) (aparams (caddr exp)))
          (let ((closure (get_closure fname combo_env))) ;;(display (get_closure fname combo_env))
            (let ((fparams (car closure))
              (fbody (cadr closure))
              (dcombo_env (caddr closure))) 
            (let ((values (map (lambda(x)(eval_expr x combo_env)) aparams)))
              (let ((bindings (re (map (lambda(x y)(list x y))
                fparams values) '())))
              (let ((working_combo_env
              	(let ((reference_lst (ref_lst bindings '())) (value_lst (val_lst bindings '()))))
                (list (append  reference_lst (car combo_env)) ;;;dcombo_env
                  (append  value_lst (cadr combo_env)) (caddr combo_env))))
              (eval_expr fbody working_combo_env))))))
        )) ;;eval_expr (body (eval_defs(vdef))) d
    ;;newly added cond 


    ;;;;;;;;;;;;;;;this is to make the lists to add on the expr ;;;;;;;;;;;;;;;;;;;;;

    ;;this function takes the bindings and appends a ref to each one ((x 2 )(y 1)) --> ((ref1 x 2)(ref2 y 1))
    (define re 
		(lambda (bindings nl)
		(if (null? bindings) nl
		(let ((new (list (append2 (list (gensym)) (car bindings)))))
		  (let ((newlist ( append2 new nl)))
		    ;;(display (list (cdr bindings) 'yeeet))
		    (re (cdr bindings) newlist))))))

;;appends 2 lists together (ref) (x 1) --> (ref x 1)
(define (append2 lis1 lis2)
  (cond ((null? lis1)
         lis2)
        (else
         (cons (car lis1)
               (append2 (cdr lis1) lis2)))))
;; takes bindings returns a list of variables to references ((ref1 x 1)(ref2 y 2)) --> ((x ref1)(y ref2)) 
(define ref_list 
	(lambda (lst nl)
	(if (null? lst) nl
	(let ((new (list (list (cadar lst)(caar lst)))))
	(let ((newlist (append2 new nl)))
	(ref_list (cdr lst) newlist))) )))

;;takes bindings and returns a list of references to values ((ref1 x 1)(ref2 y 2)) --> ((ref1 1)(ref2 2))
(define val_list 
	(lambda (lst nl)
	(if (null? lst) nl
	(let ((new (list (list (caar lst)(caddar lst)))))
	(let ((newlist (append2 new nl)))
	(val_list (cdr lst) newlist))) )))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





    ((cond? exp) (if (eval_bool (cadr exp) combo_env) (eval_expr (caddr exp) combo_env)(eval_expr (cadddr exp) combo_env)))
    ((print? exp)
    (display (eval_expr (cadr exp))))
    ((difference? exp )
     (- (eval_expr (subexp1 exp) combo_env)
        (eval_expr (subexp2 exp) combo_env)))
    ((product? exp )
     (* (eval_expr (subexp1 exp) combo_env)
        (eval_expr (subexp2 exp) combo_env)))
    ((quotient? exp )
     (/ (eval_expr (subexp1 exp) combo_env)
        (eval_expr (subexp2 exp) combo_env)))

    (else (error 'eval_expr "invalid expression ~s" exp)))))


;;gets the value of a varable from the environment: used for symbol 
(define get_val 
	(lambda (vname combo_env)
		(let (( ref (cadr (assoc vname (car combo_env))))) 
			(cadr (assoc ref (cadr combo_env)) ))))

(define get_closure 
  (lambda (fname combo_env)
      (cadr(assoc (cadr (assoc fname (car combo_env))) (caddr combo_env)))))

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
(define print?
	(lambda (exp) (eq? (car exp) 'print)))

;; The following are functions to retrieve the appropriate subexpressions
;; from an arithmetic expression

(define subexp1
  (lambda (exp) (caddr exp)))

(define subexp2
  (lambda (exp) (cadddr exp)))





;;eval_bool expression added 
;;changed some of the comparison operators to match the grammar g
(define eval_bool
  (lambda (exp combo_env)
    (cond ((comple exp)
     (< (eval_expr (subexp1 exp) combo_env)
        (eval_expr (subexp2 exp) combo_env)))
    ((compleq exp)
     (>= (eval_expr (subexp1 exp) combo_env)
        (eval_expr (subexp2 exp) combo_env)))
    ((compequ exp)
     (= (eval_expr (subexp1 exp) combo_env)
        (eval_expr (subexp2 exp) combo_env)))
    ((compgre exp)
     (> (eval_expr (subexp1 exp) combo_env)
        (eval_expr (subexp2 exp) combo_env)))
    ((compgreq exp)
      (>= (eval_expr (subexp1 exp) combo_env)
        (eval_expr (subexp2 exp) combo_env)))
    ((compneq exp)
      (!= (eval_expr (subexp1 exp) combo_env)
        (eval_expr (subexp2 exp) combo_env)))
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
  (lambda (def combo_env) 
    (cond ((vdef? def) 
    	(let ((ref (gensym)))
    		(list (list (cadr def) ref)(list ref (eval_expr (caddr def) combo_env)) '()))
    		)))  
           

 ;; variable definition 
          ((fdef? def) 
            (let ((ref (gensym)))
              (let ((closure-combo_env (cons (list (cadr def) ref) (car combo_env))))
                (list (list (cadr def) ref) '() (list ref (list (caddr def)(cadddr def) closure-combo_env)))))
 ;; function definition 
            (else (error "unknown definition type" def))))
   			

;; eval_defs loops through multiple definitions, calling eval_def 
;; for each, and collects all the bindings into a list. 
(define eval_defs 
  (lambda (defs combo_env) 
     (if (null? defs) combo_env 
         (let ((new_binding (eval_def (car defs) combo_env)))
               (eval_defs (cdr defs) (mymerge new_binding combo_env)))))) ;;I dont understand why you cant put parentesis around merge new_binding combo_env 

(define mymerge
  (lambda(new_binding combo_env)
    (list (cons  (car new_binding) (car combo_env)) ;;maybe add (list 
      (cons  (cadr new_binding) (cadr combo_env))
      (cons (caddr new_binding) (caddr combo_env)) )))

















;;(define eval_progs
;;	(lambda(prog_part)
;;		(if (null? prog_part) )))
;;you have to just do the eval def for one procedure first





(define compute
  (lambda ()
    (load "scheme_ast")
    (display 'howdeee)  ;; << This needs to be consistent
                         ;; << with the file generated by ast.exe
    (let ((combo_environment (eval_defs (def_part ptree) '(()()())) )) ;;passes in en_with (combo_env store def)

      (eval_expr (exp_part ptree) combo_environment)))) ;;this should pass in
;; ((if2 (comp x ge y) (print x) (print y)))  (with the environment from the definitions )



;;(define exp_part
;;  (lambda (ptree) (caddr ptree)))
;;the list of procedures 


 ;;(((if2 (comp x ge y) (print x) (print y)))) 



