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

; print sbir file and commands
(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (map (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n"))

; symbol tables creation and initilization
(define *function-table* (make-hash)) ; functions to be interpreted
(define *label-table* (make-hash)) ; label hash table
(define *variable-table* (make-hash)) ; variable hash table
(define *line-table* (make-hash)) ; line hash table
(define (variable-put! key value) ; put key and value into var table
    (hash-set! *variable-table* key value))
(define (line-put! key value) ; put key and value into line table
    (hash-set! *line-table* key value))

; Evaluates expressions.
(define (ident-expr expr)
    (cond
        [(string? expr) expr] ; print string
        [(number? expr) expr] ; print number
        [(hash-has-key? *function-table* expr) ; if in function table 
          (hash-ref *function-table* expr)]    ; return value 
        [(hash-has-key? *variable-table* expr) ; if in variable table
          (hash-ref *variable-table* expr)]    ; return value
        [(list? expr) ; if expr is a list
        ; check if in function table
        (if (hash-has-key? *function-table* (car expr))
            ; if so set to head 
            (let((head (hash-ref *function-table*  (car expr)))) 
            (cond 
                [(procedure? head) ; if a procedure
                (apply head (map (lambda (x) (ident-expr x)) 
                (cdr expr)))]
                [(vector? head) ; if a vector
                (vector-ref head (cadr expr))]
                [(number? head) head] ; if a number
                [else ; not in function table
                  (die (list "error: " (car expr) 
                    " is not in the function table."))]
             )) ; expr can't be found
        (die (list "error: " 
        (car expr) " not in variable or function table!\n"))
        )]
))

; === PRINT === 
; identify expression then print
(define (basic_print printable)
    (map (lambda (return-value) 
    (display (ident-expr return-value))) printable)
    (newline))

; === DIM ===
; dim creates an arr[] given by the var name and 
; insert it into either variable or function table
; the dimension of the arr[] is given by the expression
(define (basic_dim array)
    (variable-put! (caar array)(make-vector(type-check (cadar array))))
    (hash-set! *function-table* (caar array) 
          (lambda(x) (vector-ref (hash-ref *variable-table* 
          (caar array)) (sub1 x))))
)

; checks the type of expr
; depending which type expr is, perform appropriate action
(define (type-check expr)
  (cond 
    [(pair? expr) ; if pair, check function table
    (apply (hash-ref *function-table* (car expr)) 
    (map type-check (cdr expr)))] 
    [(number? expr) expr] ; if number               
    [(hash-ref *variable-table* expr)])) ; check variable table

; === LET ===
; let makes an assignment to a variable
; value of Variable is stored into the variable table
; store message of an Array is sent to the vector representing the array
(define (basic_let mem-expr)
    (if (pair? (car mem-expr))
        (begin
        (vector-set! (hash-ref *variable-table* (caar mem-expr)) 
        (sub1 (type-check(cadar mem-expr)))(type-check (cadr mem-expr)))
        )
        (begin
            (let ((result (type-check (cadr mem-expr))))
               (variable-put! (car mem-expr) result))
    ))
)

; === INPUT ===
; variable inputcount is inserted into variable table
; listens for user input, checks if input not null
; when input not null, store into variable-table key as 0
; if input value is a number, store in variable table
; if end of file, inputcount is -1, store to variable table
(define (basic_input num-value)
    (variable-put! 'inputcount 0) ; initailize inputcount to 0
    (define (listen-for-input num-value)
        (when (not (null? (car num-value)))
            (variable-put! (car num-value) (void))
            (let ((input-value (read)))
        (cond 
            ((number? input-value) ; if number
            (variable-put! (car num-value) input-value) ; store
                (variable-put! 'inputcount ; store & update input count
            (add1 (hash-ref *variable-table* 'inputcount))))
            ((eof-object? input-value) ; if end of file
            (variable-put! 'inputcount -1)) ; set input count to -1
                  (else 
                  (printf "~nerror: ~a is not a number~n" 
                  input-value) ; error
                  ))) 
             (when (not (null? (cdr num-value))) 
             ; keep listening for input
             (listen-for-input (cdr num-value)))
           ))
      (listen-for-input num-value) ; keep listening for input
)
; check instruction of program at line number
; handles if and goto commands 
; else run functions from function table
(define (check-exe-cmd instr program line-number)
    (when (not (hash-has-key? *function-table* (car instr)))
        (display (car instr)) (display " is not a valid instruction.")
        (newline)
        (usage-exit))
    (cond
        ((eq? (car instr) 'if) ;if if command is found
        (if (ident-expr (car (cdr instr)))
            (index-list program (hash-ref *label-table* 
            (cadr (cdr instr))))
            (index-list program (add1 line-number))))
        ((eq? (car instr) 'goto) ; if goto command is found
        (index-list program (hash-ref *label-table* (cadr instr))))
        (else
             ; return value
            ((hash-ref *function-table* (car instr)) (cdr instr))
            ; increment line number
            (index-list program (add1 line-number))))) 

; index each list in a line in a program at a given line number
(define (index-list program line-number) ; intial line number is 0
    (when (> (length program) line-number) ; when line number not null
    (let((line (list-ref program line-number))) 
    (cond
        ((= (length line) 3) ; if there is a label
        (line-put! (car line) (cddr line)) ; store line
        (let ((head(hash-ref *line-table* (car line))))
        (check-exe-cmd (car head) program line-number))) ; execute line
         ; if there is no label
        ((and (= (length line) 2) (list? (cadr line)))
        (line-put! (car line) (cdr line)) ; store line
        (let ((head(hash-ref *line-table* (car line))))
        (check-exe-cmd (car head) program line-number))) ; execute line
        (else
            (index-list program (add1 line-number))) ; line-number++ 
        ))))

; Label hash table holding the addresses of each line
; Initilized by scanning the list retured by (read) at start of prog
(define (hashing-labels program)
    (map (lambda (line)
        (when (not (null? line))
            (when (or (= 3 (length line))
                (and (= 2 (length line))
                    (not (list? (cadr line)))))
            (hash-set! *label-table* (cadr line) (sub1 (car line)))
            ))) program)
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
        ;(write-program-by-line sbprogfile program)
        ; get labels of program 
        (hashing-labels program)
        ; read commands
        (index-list program 0)
)))

; Variable hash table containing the initialized values e and pi
(for-each
    (lambda (pair)
        (variable-put! (car pair) (cadr pair)))
    `(
     (e       2.718281828459045235360287471352662497757247093)
     (pi      3.141592653589793238462643383279502884197169399)
))

; Function hash table holding all functions and operators
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
        (%       ,(lambda (x y) (- x (* (div x y) y))))
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
        (sin     ,sin)
        (cos     ,cos)
        (tan     ,tan)
        (asin    ,asin) 
        (acos    ,acos) 
        (abs     ,abs) 
        (round   ,round)
        (atan    ,(lambda(x)(atan (if (equal? x 0) 0.0 x))))
        (print   ,basic_print)
        (dim     ,basic_dim)
        (let     ,basic_let)
        (input   ,basic_input)
        (if      (void))
        (goto    (void))
))

(main (vector->list (current-command-line-arguments)))
