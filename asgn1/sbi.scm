#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; Paired Programming: Ryan Teves (rteves) Ky Nguyen (kymnguye)
;; $Id: sbi.scm,v 1.3 2016-09-23 18:23:20-07 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

(define *stderr* (current-error-port))

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

; === symbol tables creation and initilization ===
(define *function-table* (make-hash)) ; functions to be interpreted
(define *label-table* (make-hash)) ; label hash table
(define *variable-table* (make-hash)) ; variable hash table
(define *line-table* (make-hash)) ; variable hash table
(define (variable-put! key value)
	(hash-set! *variable-table* key value))
(define (line-put! key value)
	(hash-set! *line-table* key value))
; print sbir file and commands
(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (map (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n"))

(define (h_eval expr) ; Evaluates expressions.
  ;(printf "DEBUG: h_Evaluating...~n")
  ;(printf "       ~s~n" expr)
	(cond
		((string? expr)
		;(printf "       is a string~n") 
		expr
		)
		((number? expr)
		;(printf "       is a number~n") 
		expr
		)
		((hash-has-key? *function-table* expr)
		;(printf "       is a hash key in function table~n")
		(hash-ref *function-table* expr))
		((hash-has-key? *variable-table* expr)
		;(printf "       is a hash key variable table~n")
		(hash-ref *variable-table* expr))
		((list? expr)
		;(printf "       is a list~n")
		(if (hash-has-key? *function-table* (car expr))
			(let((head (hash-ref *function-table*  (car expr))))
			(cond 
				[(procedure? head)
				(apply head (map (lambda (x) (h_eval x)) (cdr expr)))]
				[(vector? head)
				;(printf "It's a vector.")
				(vector-ref head (cadr expr))]
				[(number? head)
				;(printf "It's a number.~n") 
				head
				]
				[else
				(die "Fatal: Broken function table.")
				]))
			(die (list "Fatal error: " 
			(car expr) " not in symbol table!\n"))
		)
		)
))

; print statement
(define (basic_print expr)
	(map (lambda (x) (display (h_eval x))) expr)
	(newline))

; dim creates an arr[] given by the var name & insert it into the Symbol table
; the dimension of the arr[] is given by the expression
(define (basic_dim expr)
	(variable-put! (caar expr) (make-vector (value (cadar expr))) )
	(hash-set! *function-table* (caar expr) 
      	(lambda(x) (vector-ref (hash-ref *variable-table* (caar expr)) (sub1 x))))
	;(map (lambda (el) (printf "~s~n" el))(hash->list *variable-table*))
)

(define (value lenl)
  (if (pair? lenl)
    (apply (hash-ref *function-table* (car lenl)) (map value (cdr lenl)))     
      (cond ((number? lenl) lenl)               
          (else (hash-ref *variable-table* lenl)))))

; let makes an assignment to a variable
; value of Variable is stored into the Symbol table
; store message of an Array is sent to the vector representing the array
(define (basic_let expr)
	  (if (pair? (car expr))
    (begin
     (vector-set! (hash-ref *variable-table* (caar expr)) 
	(- (value (cadar expr))1) (value (cadr expr)))
    )
    (begin
    (let ((result (value (cadr expr))))
       (variable-put! (car expr) result)
     )))
)

(define (basic_input expr)
	(variable-put! 'inputcount 0)
	(define (get-input expr)
		(when (not (null? (car expr)))
        	(variable-put! (car expr) (void))
        	(let ((object (read)))
		(cond ((eof-object? object)(variable-put! 'inputcount -1))
       			((number? object)(variable-put! (car expr) object)
        		(variable-put! 'inputcount (+ (hash-ref *variable-table* 'inputcount) 1)))
          		(else (begin (printf "invalid number: ~a~n" object)
                                )) )) 
         	(when (not (null? (cdr expr)))
     		(get-input (cdr expr)))
   		))
  	(get-input expr)
)

(define (exe-line instr program line-num)
	(when (not (hash-has-key? *function-table* (car instr)))
		(display (car instr)) (display " is not a valid instruction.")
		(newline)
		(usage-exit))
	(cond
		((eq? (car instr) 'goto)
		(read-cmd program (hash-ref *label-table* (cadr instr))))
		((eq? (car instr) 'if)
		(if (h_eval (car (cdr instr)))
			(read-cmd program (hash-ref *label-table* (cadr (cdr instr))))
			(read-cmd program (add1 line-num))))
		((eq? (car instr) 'print)
		(if (null? (cdr instr))
			(newline)
			(basic_print (cdr instr))) ; Bad ident
			(read-cmd program (add1 line-num)))
		(else
			((hash-ref *function-table* (car instr)) (cdr instr))
			(read-cmd program (add1 line-num)))))

(define (read-cmd program line-num) ; line number
	(when (> (length program) line-num)
	;(printf "DEBUG: Executing line ~a of ~a.~n"
	;	line-num (length program))
	;(printf "	~s~n" (list-ref program line-num))
	(let((line (list-ref program line-num)))
	(cond
		((= (length line) 3)
		;(set! line (cddr line))
		(line-put! (car line) (cddr line))
		(let ((head(hash-ref *line-table* (car line))))
		;(printf "test 1~n")
		(exe-line (car head) program line-num)))
		((and (= (length line) 2) (list? (cadr line)))
		;(set! line (cdr line))
		(line-put! (car line) (cdr line))
		(let ((head(hash-ref *line-table* (car line))))
		;(printf "test 2~n")
		(exe-line (car head) program line-num)))
		(else
			(read-cmd program (add1 line-num)))
		))))


(define (label-hash program)
	;(printf "Hashing labels:~n")
	;(printf "==================================================~n")
	(map (lambda (line)
		(when (not (null? line))
			(when (or (= 3 (length line))
				(and (= 2 (length line))
					(not (list? (cadr line)))))
			(printf "~a: ~s~n" (sub1 (car line)) (cadr line))
			(printf "    ~s~n" (list-ref program (sub1 (car line))))
			(hash-set! *label-table* (cadr line) (sub1 (car line)))
			))) program)
	;(printf "==================================================~n")
 	;(printf "Dumping label table...~n")
	;(map (lambda (el) (printf "~s~n" el))(hash->list *label-table*))
)

; start of program
(define (main arglist)
	; no file passed in or file passed in
    (if (or (null? arglist) (not (null? (cdr arglist))))
	; no file => print usage
        (usage-exit)
	; file => sbprogfile = sbir file
        (let* ((sbprogfile (car arglist))
		; program = commands in sbir file
		(program (readlist-from-inputfile sbprogfile)))
		; print filename and commands in sbir file
;		(write-program-by-line sbprogfile program)
		; get labels of program 
		(label-hash program)
		; read commands
		(read-cmd program 0)
)))

(for-each
	(lambda (pair)
		(variable-put! (car pair) (cadr pair)))
	`(
		(e       2.718281828459045235360287471352662497757247093)
        	(pi      3.141592653589793238462643383279502884197169399)
))

; Code based on ../examples/symbols.scm provided by Prof. Mackey
(for-each
	(lambda (pair)
		(hash-set! *function-table* (car pair) (cadr pair)))
	`(
	(log10_2 0.301029995663981195213738894724493026768189881)
    	(sqrt_2  1.414213562373095048801688724209698078569671875)
       	(div     ,(lambda (x y) (floor (/ x y))))
    	(log10   ,(lambda (x) (/ (log x) (log 10.0))))
    	(mod     ,(lambda (x y) (- x (* (div x y) y))))
    	(quot    ,(lambda (x y) (truncate (/ x y))))
    	(rem     ,(lambda (x y) (- x (* (quot x y) y))))
	(% 	 ,(lambda (x y) (- x (* (div x y) y))))
    	(+       ,+)
    	(-       ,-)
    	(*       ,*)
    	(/       ,(lambda (x y) (/ x (if (equal? y 0) 0.0 y))))
    	(>=      ,(lambda (x y) (>= x y)))
    	(<=      ,(lambda (x y) (<= x y)))
    	(>       ,(lambda (x y) (> x y)))
    	(<       ,(lambda (x y) (< x y)))
    	(=       ,(lambda (x y) (eqv? x y)))
    	(<>      ,(lambda (x y) (not (equal? x y))))
    	(^       ,expt)
    	(ceil    ,ceiling)
    	(exp     ,exp)
    	(floor   ,floor)
	(log     ,(lambda(x)(log (if (equal? x 0) 0.0 x))))
    	(sqrt    ,sqrt)
	(sin	 ,sin)
	(cos	 ,cos)
	(tan	 ,tan)
    	(asin 	 ,asin) 
	(acos 	 ,acos) 
	(abs 	 ,abs) 
	(round 	 ,round)
    	(atan    ,(lambda(x)(atan (if (equal? x 0) 0.0 x))))
	(print 	 ,basic_print)
	(dim 	 ,basic_dim)
	(let	 ,basic_let)
	(input	 ,basic_input)
	(if	 (void))
	(goto	 (void))
	))

(main (vector->list (current-command-line-arguments)))
