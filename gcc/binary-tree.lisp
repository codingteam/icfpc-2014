(define (main)
    (define (cgt c1 c2)
        (let (x1 (car c1)
              y1 (cdr c1)
              x2 (car c2)
              y2 (cdr c2))
            (if (gt y1 y2)
                (1)
                ((if (= y1 y2)
                    ((if (gt x1 x2)
                        (1)
                        (0)))
                    (0))))))

    (define (ceq c1 c2)
        (let (x1 (car c1)
              y1 (cdr c1)
              x2 (car c2)
              y2 (cdr c2))
            (if (= y1 y2)
                ((if (= x1 x2)
                    (1)
                    (0)))
                (0))))

    (define (clt c1 c2)
        (if (cgt c1 c2)
            (0)
            ((if (ceq c1 c2)
                (0)
                (1)))))

    (define (btree-init)
        0)

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
            ((cons 1 (btree-element elem 0 0)))
            ((let (key (btree-key tree))
                (if (ceq key elem)
                    ((cons 0 tree))
                    ((if (clt elem key)
                        ((let (result (btree-append (btree-left tree) elem))
                            (cons (car result)
                                  (btree-element key (cdr result) (btree-right tree)))))
                        ((let (result (btree-append (btree-right tree) elem))
                            (cons (car result)
                                  (btree-element key (btree-left tree) (cdr result))))))))))))

    (define (btree-test1)
        (let (tree (btree-init))
            (dbug tree)
            (let (r2 (btree-append tree (cons 1 1)))
                (dbug r2)
                (let (r3 (btree-append (cdr r2) (cons 1 1)))
                    (dbug r3)))))

    (define (btree-test2)
        (let (tree (btree-init))
            (dbug tree)
            (let (r2 (btree-append tree (cons 1 1)))
                (dbug r2)
                (let (r3 (btree-append (cdr r2) (cons 1 1)))
                    (dbug r3)
                    (let (r4 (btree-append (cdr r3) (cons 2 2)))
                        (dbug r4)
                        (let (r5 (btree-append (cdr r4) (cons 2 2)))
                            (dbug r5)))))))

    (btree-test2))