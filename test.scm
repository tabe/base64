#!r6rs

(import (rnrs)
        (prefix (base64) base64:))

(define *pass* #t)

(define-syntax report
  (syntax-rules ()
    ((_ port expr actual expected)
     (let ((p port))
       (write expected p)
       (put-string p " expected, but ")
       (report p expr actual)))
    ((_ port expr actual)
     (let ((p port))
       (write 'expr p)
       (put-string p " => ")
       (write actual p)
       (newline p)))))

(define-syntax test-error-case
  (syntax-rules ()
    ((_ (predicate description) str ...)
     (begin
       (guard (con
               ((predicate con)
                'ok))
         (let ((result (base64:decode-string str)))
           (let ((e (current-error-port)))
             (put-string e description)
             (put-string e " expected, but ")
             (report e (base64:decode-string str) result)))
         (set! *pass* #f))
       ...))))

(define-syntax test-unknown-case
  (syntax-rules ()
    ((_ str ...)
     (test-error-case (base64:unknown-alphabet? "unknown alphabet") str ...))))

(define-syntax test-invalid-case
  (syntax-rules ()
    ((_ str ...)
     (test-error-case (base64:invalid-encoding? "invalid encoding") str ...))))

(define-syntax test-
  (syntax-rules ()
    ((_ (predicate equivalence) expected expr)
     (let ((actual expr))
       (unless (and (predicate actual)
                    (equivalence expected actual))
         (set! *pass* #f)
         (report (current-error-port) expr actual expected))))))

(define-syntax test-string=?
  (syntax-rules ()
    ((_ expected expr)
     (test- (string? string=?) expected expr))))

(define-syntax test-bytevector=?
  (syntax-rules ()
    ((_ expected expr)
     (test- (bytevector? bytevector=?) expected expr))))

(define-syntax test-case
  (syntax-rules ()
    ((_) #t)
    ((_ (str bv) e0 ...)
     (begin
       (test-string=? str (base64:encode-bytevector bv))
       (test-bytevector=? bv (base64:decode-string str))
       (test-case e0 ...)))))

(test-invalid-case
 "A"
 "="
 "AB"
 "A="
 "=="
 "ABC"
 "AB="
 "A=="
 "===")

(test-unknown-case
 "!!!!"
 "AAA!"
 "!AAA"
 "====")

(test-case
 ("" #vu8())
 ("AA==" #vu8(0))
 ("AAE=" #vu8(0 1))
 ("AAEC" #vu8(0 1 2))
 ("YWJj" #vu8(97 98 99)) ; abc
 ("jhfl6fo=" #vu8(#x8e #x17 #xe5 #xe9 #xfa))
 )

(cond (*pass*
       (display "passed.\n")
       (exit))
      (else
       (display "failed.\n")
       (exit 1)))
