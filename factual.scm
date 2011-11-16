(module
 factual

 (factual-datum-name
  factual-datum-address
  factual-datum-locality
  factual-datum-region
  factual-datum-country
  factual-datum-postcode
  factual-datum-tel
  factual-datum-category
  factual-datum-latitude
  factual-datum-longitude
  factual-datum-status
  factual-read
  factual-key
  factual-read
  factual-random-data
  with-factual-key)

 (import scheme
         chicken
         extras
         data-structures
         srfi-1)

 (use intarweb
      uri-common
      http-client
      medea
      matchable
      vector-lib
      defstruct)

 (define (alist-delete/key key-deletandum alist)
   (delete (match-lambda ((key . value) (equal? key key-deletandum))) alist))

 (define (alist-partition/key key alist)
   (values (alist-ref key alist)
           (alist-delete key alist)))

 (define factual-key (make-parameter #f))

 (define (boolean->string boolean)
   (if boolean "true" "false"))

 (defstruct factual-datum
   name
   address
   locality
   region
   country
   postcode
   tel
   category
   latitude
   longitude
   status)

 (define-record-printer (factual-datum datum out)
   (format out "#,(factual-datum ~a)" (factual-datum->alist datum)))

 (define (factual-read #!key
                       (table "places")
                       (limit 10)
                       (include-count #f)
                       (offset 0))
   (let-values
       (((data-metadata uri response)
         (with-input-from-request
          (parameterize ((form-urlencoded-separator "&"))
                        (update-uri (uri-reference "http://api.v3.factual.com")
                                    path: `("" "t" ,table "read")
                                    query: `((limit . ,limit)
                                             (include_count
                                              . ,(boolean->string include-count))
                                             (KEY . ,(factual-key))
                                             (offset . ,offset))))
          #f
          (lambda ()
            (let ((data (read-json)))
              (let*-values (((response headers)
                             (alist-partition/key 'response data))
                            ((data metadata)
                             (alist-partition/key 'data response)))
                (cons (map (lambda (datum) (alist->factual-datum datum))
                           (vector->list data))
                      (append headers metadata))))))))
     (values (car data-metadata) (cdr data-metadata))))

 (define (factual-total-row-count)
   (let-values (((data metadata) (factual-read limit: 1
                                               include-count: #t)))
     (alist-ref 'total_row_count metadata)))

 (define (factual-random-data #!key
                              (limit 10)
                              (max-offset 500))
   (let* ((total-row-count (factual-total-row-count))
          (offset (random (- (min total-row-count max-offset) limit))))
     (factual-read offset: offset limit: limit)))

 (define (with-factual-key key thunk)
   (parameterize ((factual-key key)) (thunk))))
