(define (main)
    (define (empty-list list)

        (atom list))

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
                (dbug (cons 100 key))
                (if (ceq key elem)
                    ((cons 0 tree))
                    ((if (clt elem key)
                        ((let (result (btree-append (btree-left tree) elem))
                            (cons (car result)
                                  (btree-element key (cdr result) (btree-right tree)))))
                        ((let (result (btree-append (btree-right tree) elem))
                            (cons (car result)
                                  (btree-element key (btree-left tree) (cdr result))))))))))))

    (define (find-walls map)
      (let (row-index 0
            col-index 0
            walls (btree-init))
        (let (current-row map
              other-rows map)
          (do
            (dbug (cons 10 row-index))
            (set current-row (car other-rows))
            (set other-rows (cdr other-rows))
            (let (cell current-row
                  other-cells current-row)
              (set col-index 0)
              (do
                (dbug (cons 20 col-index))
                (set cell (car other-cells))
                (set other-cells (cdr other-cells))
                (if (= cell 0)
                  ((dbug (cons 0 (cons row-index col-index)))
                   (dbug (cons 101 walls))
                   (set walls (cdr (btree-append walls (cons row-index col-index)))))
                  ((dbug (cons cell (cons row-index col-index))))
                )
                (set col-index (+ col-index 1))
                (= 0 (atom other-cells))
              )
            )
            (set row-index (+ row-index 1))
            (= 0 (atom other-rows))
          )
        )
        walls
      )
    )

  (let (map (list (list 1 0 2)
                   (list 0 4 5)))
    (dbug (find-walls map))
  )
)
