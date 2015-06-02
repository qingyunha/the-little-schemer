;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; atom- a string of characters or digits
;; lsit- a collection of S-expression encolsed by parentheses
;; S-expreesion- atom or list
;;
;; (car l)- the first S-expression of a non-empty list 'l
;; (cdr l)- a list without the first S-expression of a non-empty list 'l
;; (cons s l)- a list construct by  add S-expression 's to the front of the list 'l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; (null? l)- only for list
;; lat- a list of atom

;; (eq? a1 a2)- only for non-numeric atom
;; (= a1 a2)- for number atom
;; (eqan? a1 a2)- for all atom
;; (eqlist? l1 l2)- determine if two lists are equal
;; (equal? s1 s2)- determine if two S-expressions are equal



(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (caar l) (firsts (cdr l)))))))


(define seconds
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (cadr l) (firsts (cdr l)))))))

(define pick
  (lambda (n lat)
    (cond 
      ((zero? (- n 1)) (car lat))
      (else (pick (- n 1) (cdr lat))))))



(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (equal? (car lat) a) (member? a (cdr lat)))))))


(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? a (car l)) (rember* a (cdr l)))
         (else (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l)) (rember* a (cdr l)))))))


(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond 
         ((eq? old (car l)) (cons old (cons new (insertR* new old (cdr l)))))
         (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond 
         ((eq? old (car l)) (cons new (cons old (insertL* new old (cdr l)))))
         (else (cons (car l) (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))


(define occur*
  (lambda (a l)
    (cond 
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? a (car l)) (+ 1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else (+ (occur* a (car l)) (occur* a (cdr l)))))))



(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond 
         ((eq? old (car l)) (cons new (subst* new old (cdr l))))
         (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))


(define member*
  (lambda (a l)
    (cond 
      ((null? l) #f)
      ((atom? (car l))
       (or (eq? a (car l)) (member* a (cdr l))))
      (else
       (or (member* a (car l)) (member* a (cdr l)))))))


(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))


(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((null? l1) #f)
      ((and (atom? (car l1)) (null? l2)) #f)
      ((and (atom? (car l1)) (atom? (car l2))) (and (eq? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
      ((atom? (car l1)) #f)
      ((null? l2) #f)
      ((atom? (car l2)) #f)
      (else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))


(define oequal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eq? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))


(define multirember
  (lambda (a l)
    (cond
      ((null? l) '())
      (else (cond
              ((equal? a (car l)) (multirember a (cdr l)))
              (else (cons (car l) (multirember a (cdr l)))))))))


;; 6.shadows


;; arithmetic expression (3 + 4) ((3 + 4) * 5) 
(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
       (and (numbered?  (car aexp)) (numbered? (caddr aexp)))))))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? '+ (cadr nexp)) (+ (value (car nexp)) (value (caddr nexp))))
      ((eq? '* (cadr nexp)) (* (value (car nexp)) (value (caddr nexp))))
      (else -1))))


;; use help function to hide representation
;;(+ 1 2) (+ (* 1 3) 4)
(define 1st-sub-exp
  (lambda (nexp)
    (cadr nexp)))

(define 2nd-sub-exp
  (lambda (nexp)
    (caddr nexp)))

(define operator
  (lambda (nexp)
    (car nexp)))

(define valueq
  (lambda (nexp)
    (cond
      ((number? nexp) nexp)
      ((eq? '+ (operator nexp)) (+ (valueq (1st-sub-exp nexp)) (valueq (2nd-sub-exp nexp))))
      ((eq? '* (operator nexp)) (* (valueq (1st-sub-exp nexp)) (valueq (2nd-sub-exp nexp)))))))


;;7. friends and relations
(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))


(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((member (car lat) (cdr lat)) (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))))))

(define makeset2
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else(cons (car lat)
                 (makeset2
                  (multirember (car lat)
                               (cdr lat))))))))

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and (member? (car set1) set2)
                 (subset? (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

(define intersect? 
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member? (car set1) set2) (intersect? (cdr set1) set2))))))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2)
       (cons (car set1) (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

(define union 
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2)
       (union (cdr set1) set2))
      (else (cons (car set1) (union (cdr set1) set2))))))


(define difference 
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2) (difference (cdr set1) set2))
      (else (cons (car set1) (difference  (cdr set1) set2))))))

(define intersect-all
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set)
                       (intersect-all (cdr l-set)))))))


;; pair
(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cddr x)) #t)
      (else #f))))

(define first
  (lambda (pair)
    (car pair)))

(define second
  (lambda (pair)
    (cadr pair)))

(define build
  (lambda (first second)
    (cons first (cons second '()))))

(define third
  (lambda (l)
    (caddr l)))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))


(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (build (second (car rel)) (first (car rel)))
                  (revrel (cdr rel)))))))


(define funall?
  (lambda (fun)
    (set? (seconds fun))))

(define ono-to-one?
  (lambda (fun)
    (fun? (revrel fun))))



;; Lambda the Ultimate
(define rember-f
  (lambda (test? a l)
    (cond
      ((null? l) '())
      ((test? a (car l)) (cdr l))
      (else (cons (car l) 
                  (rember-f test? a (cdr l)))))))


(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define rember-ff
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? a (car l)) (cdr l))
        (else (cons (car l)
                    ((rember-ff test?) a (cdr l))))))))


(define insert-g
  (lambda (test? re)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? (car l) old)
         (cond
           (re (cons old (cons new (cdr l))))
           (else (cons new l))))
        (else (cons (car l)
                    ((insert-g test? re) new old (cdr l))))))))

(define value2
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else
       (((lambda (x)
           (cond ((eq? x '+) +)
                 ((eq? x '*) *))) (operator nexp))
        (value2 (1st-sub-exp nexp))
        (value2 (2nd-sub-exp nexp)))))))





;;collecter or **continuation**
(define multirember-co
  (lambda (a lat col)
    (cond
      ((null? lat)
       (col (quote ()) (quote ())))
      ((eq? (car lat) a)
       (multirember-co a
                       (cdr lat)
                       (lambda (newlat seen)
                         (col newlat
                              (cons (car lat) seen)))))
      (else
       (multirember-co a
                       (cdr lat)
                       (lambda (newlat seen)
                         (col (cons (car lat) newlat)
                              seen)))))))

(define a-friend
  (lambda (x y)
    (null? y)))




(define the-last-friend
  (lambda (newl product sum)
    (cons sum (cons product newl))))

(define evens-only** 
  (lambda (l col)
    (cond
      ((null? l) (col '() 1 0))
      ((atom? (car l))
       (cond
         ((even? (car l)) 
          (evens-only** (cdr l)
                        (lambda (newl p s)
                          (col (cons (car l) newl)
                               (* (car l) p) s))))
         (else (evens-only** (cdr l)
                             (lambda (newl p s)
                               (col newl p
                                    (+ s (car l))))))))
      (else (evens-only** (car l)
                          (lambda (al ap as)
                            (evens-only** (cdr l)
                                          (lambda (dl dp ds)
                                            (col (cons al dl)
                                                 (* ap dp)
                                                 (+ as ds))))))))))



;; 9...and Again, and Again, and Again,...

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))


(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn)
       (keep-looking  a (pick sorn lat) lat))
      (else (eq? a sorn)))))


(define enternity
  (lambda (x)
    (enternity x)))


(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (align (shift pora)))
      (else (build (first pora)
                   (align (second pora)))))))

(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (+ (length* (first pora))
               (length* (second pora)))))))


(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (+ (* (weight* (first pora)) 2)
               (weight* (second pora)))))))


(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (shuffle (revpair pora)))
      (else (build (first pora)
                   (shuffle (second pora)))))))



(define C
  (lambda (n)
    (cond 
      ((= n 1) 1)
      ((even? n) (C (/ n 2)))
      (else (C (+ (* 3 n) 1))))))


(define A
  (lambda (n m)
    (cond
      ((zero? n) (+ m 1))
      ((zero? m) (A (- n 1) 1))
      (else (A (- n 1)
               (A n (- m 1)))))))

;; What Is the Value of All of This
(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name (first entry) (second entry) entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((equal? (car names) name) (car values))
      (else (lookup-in-entry-help name (cdr names) (cdr values) entry-f)))))

(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else (lookup-in-entry name 
                             (car table)
                             (lambda (name)
                               (lookup-in-table name 
                                                (cdr table table-f))))))))


(define expression-to-action
  (lambda (e)
    (cond 
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e 'cons) *const)
      ((eq? e 'car) *const)
      ((eq? e 'cdr) *const)
      ((eq? e 'null?) *const)
      ((eq? e 'atom?) *const)
      ((eq? e 'zero?) *const)
      ((eq? e 'add1) *const)
      ((eq? e 'sub1) *const)
      ((eq? e 'number?) *const)
      (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond
         ((eq? (car e) 'quote) *quote)
         ((eq? (car e) 'lambda) *lambda)
         ((eq? (car e) 'cond) *cond)
         (else *application)))
      (else *application))))

(define value 
  (lambda (e)
    (meaning e '())))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

;;;action
;;const
(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build 'primitive e)))))

;;quote
(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)


;;indentifier
(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    '()))

;;lambda
(define *lambda
  (lambda (e table)
    (build 'non-primitive
           (cons table (cdr e)))))


(define table-of first)
(define formals-of second)
(define body-of third)


;;cond 
(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines)) table))
      ((meaning (question-of (car lines)) table)
       (meaning (answer-of (car lines)) table))
      (else (evcon (cdr lines) table)))))

(define else?
  (lambda (e)
    (cond
      ((atom? e) (eq? e 'else))
      (else #f))))

(define question-of first)
(define answer-of second)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

;;application
(define evlis
  (lambda (args table)
    (cond
      ((null? args) '())
      (else
       (cons (meaning (car args) table)
             (evlis (cdr args) table))))))

(define *application
  (lambda (e table)
    (*apply 
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define function-of car)
(define arguments-of cdr)

(define primitive?
  (lambda (l)
    (eq? (first l) 'primitive)))

(define non-primitive?
  (lambda (l)
    (eq? (first l) 'non-primitive)))

(define *apply 
  (lambda (fun vals)
    (cond
      ((primitive? fun)
       (apply-primitive 
        (second fun) vals))
      ((non-primitive? fun)
       (apply-closure
        (second fun) vals)))))

(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name 'cons) (cons (first vals) (second vals)))
      ((eq? name 'car) (car (first vals)))
      ((eq? name 'cdr) (cdr (first vals)))
      ((eq? name 'null?) (null? (first vals)))
      ((eq? name 'eq?)) (eq? (first vals) (scond vals))
      ((eq? name 'atom?) (*atom? (first vals)))
      ((eq? name 'zero?) (zero? (fisrt vals)))
      ((eq? name 'add1) (+ (first vals) 1))
      ((eq? name 'sub1) (- (first vals) 1))
      ((eq? name 'number?) (number? (first vals))))))

(define *atom?
  (lambda (x)
    (cond
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x) 'primitive) #t)
      ((eq? (car x) 'non-primitive #t))
      (else #f))))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure) 
             (extend-table (build 
                            (formals-of closure)
                            vals)
                           (table-of closure)))))

                 
    