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
(define *symbol-table* (make-hash))
(define (symbol-put! key value)
	(hash-set! *symbol-table* key value))
; print sbir file and commands
(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (map (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n"))

(define l-hash (make-hash)) ;command hash table

(define (hash-labels program)
	(printf "Hashing labels:~n")
 	(printf "==================================================~n")
	(map (lambda (line)
		(when (not (null? line))
			(when (or (= 3 (length line))
				(and (= 2 (length line))
					(not (list? (cadr line)))))
			(printf "~a: ~s~n" (- (car line) 1) (cadr line))
			(printf "    ~s~n" (list-ref program (- (car line) 1)))
			(hash-set! l-hash (cadr line) (- (car line) 1 ))
			))) program)
	(printf "==================================================~n")
 	(printf "Dumping label table...~n")
	(map (lambda (el) (printf "~s~n" el))(hash->list l-hash))
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
              (write-program-by-line sbprogfile program)
		; get program commands
		(hash-labels program)

)))

(main (vector->list (current-command-line-arguments)))
