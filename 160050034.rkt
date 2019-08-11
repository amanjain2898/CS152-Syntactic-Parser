#lang racket

(provide all-defined-out)

(define-struct gnode (sym list) #:transparent)
(define-struct ident (str) #:transparent)
(define-struct num (val) #:transparent)

(define (combine-cc char1 char2)
  (list->string (list char1 char2)))

(define (combine-sc str char)
  (list->string (append (string->list str)
                        (list char))))

(define (combine-cs char str)
  (list->string (cons char (string->list str))))

(define (combine-ss str1 str2)
  (list->string (append (string->list str1)
                        (string->list str2))))

(define (pred-p p)
  (lambda(str)(let* ((lis (string->list str)))
                (if (p (car lis)) (cons (car lis) (list->string (cdr lis)))
                    'fail))))


(define single-digit-p
  (lambda(str) ((pred-p (lambda(c) (and (>= (char->integer c) 48)
                                        (<= (char->integer c) 57)))) str)))

(define single-alphabet-p
  (lambda(str) ((pred-p (lambda(c) (let* ((num (char->integer c)))
                                     (or (and (>= num 65)
                                              (<= num 90))
                                         (and (>= num 97)
                                              (<= num 122)))))) str)))

(define (seq p1 p2 f)
  (lambda(str) (let* ((res1 (p1 str))
                      (res2 (p2 str)))
                 (cond [(equal? res1 'fail) 'fail]
                       [(equal? (p2 (cdr res1)) 'fail) 'fail]
                       [else (cons (f (car res1) (car (p2 (cdr res1)))) (cdr (p2 (cdr res1))))]))))
(define (alt p1 p2)
  (lambda(str) (cond [(and (eq? (p1 str) 'fail) (eq? (p2 str) 'fail)) 'fail]
                     [(eq? (p1 str) 'fail) (p2 str)]
                     [(eq? (p2 str) 'fail) (p1 str)])))

(define epsilon-p
  (lambda(str) (cons "" str)))

(define (zero-or-more p f)
  (define (helper s str)
    (cond [(string=? str "") (cons (list->string (reverse (string->list s))) "")]
          [(eq? (p str) 'fail) (cons (list->string (reverse (string->list s)))  str)]
          [#t (helper (f (car (p str)) s) (cdr (p str)))]))
  (lambda(str) (helper "" str)))

(define (one-or-more p f)
  (λ(str) (cond [(eq? (p str) 'fail) 'fail]
                [#t ((zero-or-more p f) str)])))
  

(define empty-p
  (lambda(str) ((pred-p (lambda(c) (char=? c #\space))) str)))

(define without-brackets
  (λ(str) (let* ((len (string-length str)))
            (substring str 1 (- len 1)))))
                 


    
(define whitespace-p
  (let* ((a (lambda(str) ((zero-or-more empty-p combine-cs) str))))
    (lambda(str) (cons "" (cdr (a str))))))

(define number-p
  (λ(str) (let* ((new-str (cdr (whitespace-p str)))
                 (a ((one-or-more single-digit-p combine-cs) new-str)))
            (if (eq? a 'fail) 'fail
                (cons (make-num (string->number (car a))) (cdr (whitespace-p (cdr a))))))))
  

(define identifier-p
  (λ(str) (let* ((new-str (cdr (whitespace-p str)))
                 (a (single-alphabet-p new-str))
                 (b (lambda(x) ((zero-or-more (alt single-alphabet-p single-digit-p) combine-cs) (cdr x))))
                 (c (lambda(x) (combine-cs (car x) (car (b x))))))
            (if (eq? a 'fail) 'fail
                (cons (ident (c a)) (cdr (whitespace-p (cdr (b a)))))))))

(define (last-bracket str c1 c2 n)
  (define (last-bracket-helper str x y z)
    (cond [(char=? (string-ref str z) c1)  (begin (set! x (+ x 1)))]
          [(char=? (string-ref str z) c2)  (begin (set! y (+ y 1)))]
          [else (last-bracket-helper str x y (+ z 1))])
    (if(= x y) z
       (last-bracket-helper str x y (+ z 1))))
  (last-bracket-helper str 0 0 0))
      
  


(define variable-p
  (λ(str) (let* ((a (identifier-p str))
                 (c (λ(x) (cdr (whitespace-p (cdr x))))))
            (if (or (equal? a 'fail) (string=? (cdr a) "") (not (equal? (string-ref (cdr (whitespace-p (c a))) 0) #\[))) a
                (let* ((n (+ 1 (last-bracket (c a) #\[ #\] 0)))
                       (d (cdr (whitespace-p (c a)))))
                  (if (equal? (expression-p (substring d 1 (- n 1))) 'fail) (expression-p (substring d 1 (- n 1)))
                      (cons (gnode 'ARRAY (list (car a) (car (expression-p (substring d 1 (- n 1))))))
                            (cdr (whitespace-p (substring d n))))))))))

(define term-p
  (λ(str1) (let* ((str (cdr (whitespace-p str1))))
             (cond [(equal? str "") (cons "" "")]
                   [(char=? (string-ref str 0) #\() (let* ((n (+ 1 (last-bracket str #\( #\) 0))))
                                                      (cons (car (expression-p (substring str 1 (- n 1))))
                                                            (cdr (whitespace-p (substring str n)))))]
                   [(not (eq? (single-digit-p str) 'fail)) (number-p str)]
                   [else (variable-p str)]))))



(define remove-+
  (λ(str) (substring str 1)))


(define expression-p
  (λ(str) (let* ((str1 (term-p str))
                 (str4 (λ(x) (cdr (whitespace-p (cdr x))))))
            (cond [(and (not (equal? str1 'fail)) (not (string=? (str4 str1) "")) (char=? (string-ref (str4 str1) 0) #\+))
                   (if (equal? (term-p (cdr (whitespace-p (remove-+ (str4 str1))))) 'fail) (cons (car str1) (cdr (whitespace-p (cdr str1))))
                       (cons (gnode 'PLUS (list (car str1)
                                                (car (expression-p (remove-+ (str4 str1))))))
                             (cdr (expression-p (remove-+ (str4 str1))))))]
                  [else str1])))) 

(define assignment-p
  (λ(str) (let* ((str1 (expression-p str))
                 (s1 (car str1))
                 (s2 (cdr (whitespace-p (cdr str1))))
                 (s3 (substring s2 1))
                 (s4 (car (expression-p s3)))
                 (s5 (cdr (expression-p s3))))
            (if (and (not (string=? s2 "")) (char=? (string-ref s2 0) #\=)) (cons (gnode 'ASSIGN (list s1 s4))
                                                                                  (cdr (whitespace-p s5)))
                'fail))))
                
                

         

           
  
  


            
    


                      










    


  
  
  
  


