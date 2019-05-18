;; Representing a dequeue with a doubly linked list
(define (front-ptr dequeue) (car dequeue))
(define (rear-ptr dequeue) (cdr dequeue))
(define (set-front-ptr! dequeue item)
  (set-car! dequeue item))
(define (set-rear-ptr! dequeue item)
  (set-cdr! dequeue item))

(define (set-prev node item)
  (set-car! (cdr node) item))
(define (set-next node item)
  (set-car! (cdr (cdr node)) item))
(define (get-prev node)
  (cadr node))
(define (get-next node)
  (caddr node))
(define (get-item node)
  (car node))

(define (empty-dequeue? dequeue)
  (null? (front-ptr dequeue)))

(define (make-dequeue) (cons '() '()))

(define (front-dequeue dequeue)
  (if (empty-dequeue? dequeue)
      (error "Front called with an empty dequeue" dequeue)
      (car (front-ptr dequeue))))

(define (rear-dequeue queue)
  (if (empty-dequeue? dequeue)
      (error "Front called with an empty dequeue" dequeue)
      (car (rear-ptr dequeue))))


(define (insert-dequeue! dequeue item end)
  (let ((new-list (list item '() '())))
    (cond ((empty-dequeue? dequeue)
	   (set-front-ptr! dequeue new-list)
	   (set-rear-ptr! dequeue new-list))
	  ((eq? end 'front)
	   (set-prev (front-ptr dequeue) new-list)
	   (set-next new-list (front-ptr dequeue))
	   (set-front-ptr! dequeue new-list))
	  ((eq? end 'rear)
	   (set-next (rear-ptr dequeue) new-list)
	   (set-prev new-list (rear-ptr dequeue))
	   (set-rear-ptr! dequeue new-list)))))


(define (front-insert-dequeue! dequeue item)
  (insert-dequeue! dequeue item 'front))
(define (rear-insert-dequeue! dequeue item)
  (insert-dequeue! dequeue item 'rear))


(define (front-delete-dequeue! dequeue)
  (cond ((empty-dequeue? dequeue)
	 (error "DELETE! called with an empty dequeue" dequeue))
	((null? (get-next (front-ptr dequeue))) (set-front-ptr! dequeue '()))
	(else (set-prev (get-next (front-ptr dequeue)) '())
	      (set-front-ptr! dequeue (get-next (front-ptr dequeue))))))
	      

(define (rear-delete-dequeue! dequeue)
  (cond ((empty-dequeue? dequeue)
	 (error "DELETE! called with an empty dequeue" dequeue))
	((null? (get-prev (rear-ptr dequeue))) (set-front-ptr! dequeue '()))
	(else (set-next (get-prev (rear-ptr dequeue)) '())
	      (set-rear-ptr! dequeue (get-prev (rear-ptr dequeue))))))
	
	
(define (print-dequeue dequeue)
  (if (empty-dequeue? dequeue)
      '()
      (let ((element (front-ptr dequeue)))
	(define (iterator node)
	  (if (null? node)
	      '()
	      (cons (get-item node) (iterator (get-next node)))))
	(iterator element))))
		    
		    
	      
	  
