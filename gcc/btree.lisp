    (define (cgt c1 c2)

        (let (x1 (car c1)
              y1 (cdr c1)
              x2 (car c2)
              y2 (cdr c2))
            (if (gt y1 y2)
                (TRUE)
                ((if (= y1 y2)
                    ((if (gt x1 x2)
                        (TRUE)
                        (FALSE)))
                    (FALSE))))))

    (define (ceq c1 c2)

        (let (x1 (car c1)
              y1 (cdr c1)
              x2 (car c2)
              y2 (cdr c2))
            (if (= y1 y2)
                ((if (= x1 x2)
                    (TRUE)
                    (FALSE)))
                (FALSE))))

    (define (clt c1 c2)

        (if (cgt c1 c2)
            (FALSE)
            ((if (ceq c1 c2)
                (FALSE)
                (TRUE)))))

    (define (btree-init)
        nil)

    (define (btree-element key left right)

        (cons key (cons left right)))

    (define (btree-key elem)

        (car elem))

    (define (btree-left elem)

        (car (cdr elem)))

    (define (btree-right elem)

        (cdr (cdr elem)))

    (define (btree-append tree elem)

        (if (atom tree)
            ((cons TRUE (btree-element elem nil nil)))
            ((let (key (btree-key tree))
                (if (ceq key elem)
                    ((cons FALSE tree))
                    ((if (clt elem key)
                        ((let (result (btree-append (btree-left tree) elem))
                            (cons (car result)
                                  (btree-element key (cdr result) (btree-right tree)))))
                        ((let (result (btree-append (btree-right tree) elem))
                            (cons (car result)
                                  (btree-element key (btree-left tree) (cdr result))))))))))))

    (define (btree-check tree elem)
        (if (atom tree)
          (FALSE)
          ((let (key (btree-key tree))
             (if (ceq key elem)
               (TRUE)
               ((if (clt elem key)
                  ((btree-check (btree-left  tree) elem))
                  ((btree-check (btree-right tree) elem))
               ))
              )
          ))
        )
    )


(define (has-coords list coords)
       (if (empty-list list)
           (0)
           ((let (x1   (car coords)
                  y1   (cdr coords)
                  head (car list))
               (let (x2 (car head)
                     y2 (cdr head))
                    (if (= x1 x2)
                       ((if (= y1 y2)
                           (1)
                           ((has-coords (cdr list) coords))))
                       ((has-coords (cdr list) coords))))))))

