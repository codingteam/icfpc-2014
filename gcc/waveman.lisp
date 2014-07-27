(define (main state reserved)
    (dbug 0)
    (define (elt2 lst coords)
      (dbug 1)
      (elt (elt lst (cdr coords)) (car coords))
    )
    (define (is-wall grid coords)
      (dbug 2)
      (= 0 (elt2 grid coords))
    )
    (define (vitality man)
        (dbug 3)
        (car man))
    (define (location man)
        (dbug 4)
        (elt man 1))
    (define (direction man)
        (dbug 5)
        (elt man 2))

    (define (at-left coords)
        (dbug 6)
        (cons (- (car coords) 1)
              (cdr coords)))

    (define (at-right coords)
        (dbug 7)
        (cons (+ 1 (car coords))
              (cdr coords)))

    (define (at-up coords)
        (dbug 8)
        (cons (car coords)
              (- (cdr coords) 1)))

    (define (at-down coords)
        (dbug 9)
        (cons (car coords)
              (+ (cdr coords) 1)))

    (define (in-direction dir coords)
      (dbug 10)
      (if (= dir 0)
        ((at-up coords))
        ((if (= dir 1)
           ((at-right coords))
           ((if (= dir 2)
              ((at-down coords))
              ((at-left coords))
            )
          )
         )
       )
      )
    )
    (define (rotate-clockwise dir)
      (dbug 11)
      (if (= dir 3)
        (0)
        ((+ dir 1))
      )
    )

    (define (get-cell state coords)
        (dbug 12)
        (let (map (car state))
            (elt2 map coords)))

    (define (not x)
        (dbug 13)
        (= x 0))

    (define (is-passable cell)
        (dbug 14)
        (not (= cell 0)))

    (define (is-ghost-loop ghosts x y)
        (dbug 15)
        (if (empty-list ghosts)
            (0)
            ((let (ghost (car ghosts))
                (let (ghost-pos (car (cdr ghost)))
                    (let (ghost-x (car ghost-pos)
                          ghost-y (cdr ghost-pos))
                        (if (= x ghost-x)
                            ((if (= y ghost-y)
                                (1)
                                ((is-ghost-loop (cdr ghosts) x y))))
                            ((is-ghost-loop (cdr ghosts) x y)))))))))

    (define (is-ghost state coords)
        (dbug 16)
        (let (ghosts (car (cdr (cdr state)))
              x (car coords)
              y (cdr coords))
            (is-ghost-loop ghosts x y)))

    (define (is-fruit cell state coords)
        (dbug 17)
        (if (= cell 4)
            ((let (fruit-state (cdr (cdr (cdr state))))
                (gt fruit-state 0)))
            (0)))

    (define (is-eatable cell state coords)
        (dbug 18)
        (if (= cell 2)
            (1)
            ((if (= cell 3)
                (1)
                ((is-fruit cell state coords))))))

    (define (get-code state coords)
        (dbug 19)
        (let (cell (get-cell state coords))
            (if (is-passable cell)
                ((if (is-ghost state coords)
                    (20)
                    ((if (is-eatable cell state coords)
                        (1)
                        (0)))))
                (10))))

    (define (empty-list list)
        (dbug 20)
        (atom list))

    (define (cgt c1 c2)
        (dbug 21)
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
        (dbug 22)
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
        (dbug 23)
        (if (cgt c1 c2)
            (0)
            ((if (ceq c1 c2)
                (0)
                (1)))))

    (define (btree-init)
        (dbug 24)
        0)

    (define (btree-element key left right)
        (dbug 25)
        (cons key (cons left right)))

    (define (btree-key elem)
        (dbug 26)
        (car elem))

    (define (btree-left elem)
        (dbug 27)
        (car (cdr elem)))

    (define (btree-right elem)
        (dbug 28)
        (cdr (cdr elem)))

    (define (btree-append tree elem)
        (dbug 29)
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

    (define (has-coords list coords)
        (dbug 30)
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

    (define (eatable-distance-loop state visited-tree coords)
        (dbug 31)
        (let (code (get-code state coords))
            (if (geq code 10)
                (1000000)
                ((let (append-result (btree-append visited-tree coords))
                    (if (car append-result)
                        ((if (= code 0)
                            ((let (new-visited (cdr append-result))
                                (+ 1 (min-4 (eatable-distance-loop state new-visited (at-up coords))
                                            (eatable-distance-loop state new-visited (at-right coords))
                                            (eatable-distance-loop state new-visited (at-down coords))
                                            (eatable-distance-loop state new-visited (at-left coords))))))
                            ((if (= code 1)
                                (0)
                                (1000000)))))
                        (1000000)))))))

    (define (eatable-distance state coords)
        (dbug 32)
        (eatable-distance-loop state (btree-init) coords))

    (define (min-4 a b c d)
        (dbug 33)
        (if (geq a b)
            ((if (geq b c)
                ((if (geq c d)
                    (d)
                    (c)))
                ((if (geq b d)
                    (d)
                    (b)))))
            ((if (geq a c)
                ((if (geq c d)
                    (d)
                    (c)))
                ((if (geq a d)
                    (d)
                    (a)))))))

    (define (min-idx-4 a b c d)
        (dbug 34)
        (if (geq a b)
            ((if (geq b c)
                ((if (geq c d)
                    (3)
                    (2)))
                ((if (geq b d)
                    (3)
                    (1)))))
            ((if (geq a c)
                ((if (geq c d)
                    (3)
                    (2)))
                ((if (geq a d)
                    (3)
                    (0)))))))

    (define (wave-search state coords)
        (dbug 35)
        (min-idx-4 (eatable-distance state (at-up coords))
                   (eatable-distance state (at-right coords))
                   (eatable-distance state (at-down coords))
                   (eatable-distance state (at-left coords))))

    (define (step ai-state world-state)
        (dbug 36)
        (let (grid (car world-state)
              man  (elt world-state 1))
            (let (loc (location man))
                (cons ai-state (wave-search world-state loc)))))

    (cons 0 step))
