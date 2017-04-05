#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: blowstack.scm,v 1.2 2014-10-31 17:35:08-07 - - $

;;
;; Blow the stack by infinite recursion.  This function is not
;; a tail call.
;;

(define modulus 1000000)

{define (recur count)
        (when (= (remainder count modulus) 0)
              [printf "count = ~a~n" count])
        (+ 1 (recur (+ 1 count)))}

(recur 0)


