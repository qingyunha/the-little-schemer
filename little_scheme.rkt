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

