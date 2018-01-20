(define (make-tree value left right)
  (list value left right))
(define (value h) (car h))
(define (left h) (cadr h))
(define (right h) (caddr h))
"final practice"
"problem 1"
"b"
(define (vector-map v f)
  (let ((result (make-vector (vector-length v))))
    (do ((i 0 (+ i 1)))
      ((>= i (vector-length v)) result)
      (vector-set! result i (f (vector-ref v i))))))
(define (f x) (* x x))
(vector-map (vector 1 2 3) f)


"c"
(define (duplicates? L)
  (define (in-the-list a l)
    (cond ((null? l) #f)
          ((eq? a (car l)) #t)
          (else (in-the-list a (cdr l)))))
  (cond ((null? L) #f)
        ((in-the-list (car L) (cdr L)) #t)
        (else (duplicates? (cdr L)))))

(duplicates? '(1 2 3 4 5 6))
(duplicates? '(1 2 3 4 5 6 3))


"d"
(define (fiblist n)
  (if (eq? n 1)
      '(1 0)
      (let ((p-list (fiblist (- n 1))))
        (cons (+ (car p-list)
                 (cadr p-list))
              p-list))))

(fiblist 1)
(fiblist 2)
(fiblist 3)
(fiblist 20)

(define (evens n)
  (cons (* 2 n)
        (delay (evens (force n)))))


(define (stream-ex start)
  (cons start
        (delay (stream-ex (+ start 1)))))
"problem 2"
"a"
(define (union f g)
  (lambda (x) (or (f x) (g x))))
"b"

"problem 3"
(define test-heap (make-tree 3
                     (make-tree 5
                                (make-tree 8 '() '())
                                (make-tree 10 '() '()))
                     (make-tree 6
                                (make-tree 11 '() '())
                                (make-tree 15 '() '()))))
test-heap
"a"
(define (heap-min h) (value h))
"b"
(define (heap-insert x h)
  (if (null? h) (make-tree x '() '())
      (let ((child-value (max x (value h)))
            (root-value (min x (value h))))
        (make-tree root-value
                   (heap-insert child-value (right h))
                   (left h)))))
(heap-insert 7 test-heap)
"c"
(define (remove-min H)
  (define (combine h1 h2)
    (cond ((null? h1) h2)
          ((null? h2) h1)
          ((< (value h1) (value h2))
           (make-tree (value h1)
                      h2
                      (combine (left h1) (right h1))))
          (else (make-tree (value h2)
                           h1
                           (combine (left h2) (right h2))))))
  (combine (left H) (right H)))

(remove-min test-heap)
"d"
(define (extract-h H)
  (if (null? H)
      '()
      (cons (value H)
            (extract-h (remove-min H)))))
(extract-h test-heap)

"problem 4"
"a"
(define (make-set)
  (let ((s '())
        (size 0))
    (define (empty?) (null? s))
    (define (insert x) (set! s (cons x s)) (set! size (+ size 1)))
    (define (member? x)
      (define (helper l)
        (cond ((null? l) #f)
              ((eq? (car l)) #t)
              (else (help (cdr l)))))
      (helper s))
    (define (dispatcher method)
      (cond ((eq? method 'empty?) empty?)
            ((eq? method 'insert) insert)
            ((eq? method 'member?) member?)))
    dispatcher))

(define new-set (make-set))
((new-set 'empty?))
((new-set 'insert) 6)


"dequeue 的用法，插入或者取走一个元素在前面或者后面"

"make queue"
(define (make-queue)
  (let ((head '())
        (tail '()))
    (define (value n) (car n))
    (define (next n) cdr n)
    (define (empty?) (null? head))
    (define (front) (value head))
    (define (enqueue x)
      (let ((new-node (cons x '())))
        (begin
          (if (empty?)
              (set! head new-node)
              (set-cdr! tail new-node))
          (set! tail new-node))))

    (define (dispatcher method)
      (cond ((eq? method 'empty?) empty?)
            ((eq? method 'enqueue) enqueue)))
    dispatcher))
(define v (make-queue))
((v 'enqueue) 3)

(define (integers a b)
  (if (< b a)
      '()
      (cons a (integers (+ a 1) b))))
(integers 3 9)
(integers 3 3)
(integers 9 3)


(define (filter f L)
  (cond ((null? L) L)
        ((f (car L)) (cons (car L) (filter f (cdr L))))
        (else (filter f (cdr L)))))
(filter even? (integers 1 20))
"vector sum functionally"
(define (vector-sum v)
  (define (sum-accumulate i)
    (if (>= i (vector-length v))
        0
        (+ (vector-ref v i)
           (sum-accumulate (+ i 1)))))
  (sum-accumulate 0))

(vector-sum (vector 1 2 3 4 5))

"vector sum destructively"
(define (vector-sum-d v)
  (let ((result 0))
    (do ((index 0 (+ index 1)))
      ((>= index (vector-length v)) result)
      (set! result
            (+ result (vector-ref v index))))))
(vector-sum-d (vector 1 2 3 4 5))
"inner product"
(define (vector-product v1 v2)
  (let ((sp 0))
    (do ((index 0 (+ index 1)))
      ((>= index (vector-length v1)) sp)
      (set! sp (+ sp (* (vector-ref v1 index)
                        (vector-ref v2 index)))))))
(vector-product (vector 1 2 3) (vector 4 5 6))

"sum of square"
(define (sum-square v)
  (vector-product v v))
(sum-square (vector 1 2 3))

"vector-average"
(define (vector-average v)
  (/ (vector-sum v)
     (vector-length v)))
(vector-average (vector 1 2 3 4 5))
"make-stack"
(define (make-stack)
  (let ((S '()))
    (define (empty?) (null? S))
    (define (top) (car S))
    (define (pop)
      (let ((top (car S)))
        (begin (set! S (cdr S))
               S)))
    (define (push x) (set! S (cons x S)))
    (define (dispatcher method)
      (cond ((eq? method 'top) top)
            ((eq? method 'pop) pop)
            ((eq? method 'push) push)
            ((eq? method 'empty) empty?)))
    dispatcher))

(define A (make-stack))
((A 'empty))
((A 'push) 1)
((A 'push) 2)
((A 'push) 3)
((A 'top))
((A 'pop))
((A 'pop))
((A 'pop))

"object make tree"
(define (make-set)
  (let ((s '()))
    (define (value n) (car n))
    (define (pointer n) (cdr n))
    (define (left n) (car (pointer n)))
    (define (right n) (cdr (pointer n)))
    (define (make-tree value left right)
      (cons value (cons left right)))
    (define (empty?) (null? s))
    (define (element? x)
      (define (search L)
        (cond ((null? L) #f)
              ((eq? x (value L)) #t)
              ((< x (value L)) (search (left L)))
              ((> x (value L)) (search (right L)))))
    (search s))
    (define (insert x)
      (let ((new-node (make-tree x '() '())))
        (define (insert-internal t)
          (cond ((>= x (value t))
                 (if (null? (right t))
                     (set-cdr! (pointer t) new-node)
                     (insert-interfnal (right t))))
                ((< x (value t))
                 (if (null? (left t))
                     (set-car! (pointer t) new-node)
                     (insert-internal (left t))))))
        (if (null? s)
            (ser! s new-node)
            (insert-internal s))))
    (define (list-elements)
      (define (sorted-extract t)
        (if (null? t)
            '()
            (append (sorted-extract (left t))
                    (list (value t))
                    (sorted-extract (right t)))))
      (sorted-extract s))
    (define (dispatcher method)
      (cond ((eq? method 'empty?) empty?)
            ((eq? method 'element?) element?)
            ((eq? method 'insert) insert)
            ((eq? 'list-elemnets) list-elements)))
    dispatcher))

"the do command. a built-in iterator with syntax "
(define (vector-add v1 v2)
  (let ((result (make-vector (vector-length v1))))
    (do ((i 0 (+ i 1)))
      ((>= i (vector-length v1)) result)
      (vector-set! result i (+ (vector-ref v1 i)
                               (vector-ref v2 i))))))
(define (vector-sum v)
  (define (sum-acc i)
    (if (>= i (vector-length v))
        0
        (+ (vector-ref v i)
           (sum-acc (+ i 1)))))
  (sum-acc 0))
(vector-sum (vector 1 2 3 4 5))

(vector-add (vector 1 2 3) (vector 1 2 3))

"vector to list"
(define (v-l v)
  (let ((result '()))
    (do ((i (- (vector-length v) 1) (- i 1)))
      ((< i 0) result)
      (set! result (cons (vector-ref v i) result)))))
(v-l (vector 1 2 3 4 5))










