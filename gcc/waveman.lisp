(define (main state reserved)

    (define (elt2 lst coords)

      (elt (elt lst (cdr coords)) (car coords))
    )
    (define (is-wall grid coords)

      (= 0 (elt2 grid coords))
    )
    (define (vitality man)

        (car man))
    (define (location man)

        (elt man 1))
    (define (direction man)

        (elt man 2))

    (define (at-left coords)

        (cons (- (car coords) 1)
              (cdr coords)))

    (define (at-right coords)

        (cons (+ 1 (car coords))
              (cdr coords)))

    (define (at-up coords)

        (cons (car coords)
              (- (cdr coords) 1)))

    (define (at-down coords)

        (cons (car coords)
              (+ (cdr coords) 1)))

    (define (in-direction dir coords)

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

      (if (= dir 3)
        (0)
        ((+ dir 1))
      )
    )

    (define (get-cell state coords)

        (let (map (car state))
            (elt2 map coords)))

    (define (not x)

        (= x 0))

    (define (is-passable cell)

        (not (= cell 0)))

    (define (is-ghost-loop ghosts x y)

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

        (let (ghosts (car (cdr (cdr state)))
              x (car coords)
              y (cdr coords))
            (is-ghost-loop ghosts x y)))

    (define (is-fruit cell state coords)

        (if (= cell 4)
            ((let (fruit-state (cdr (cdr (cdr state))))
                (gt fruit-state 0)))
            (0)))

    (define (is-eatable cell state coords)

        (if (= cell 2)
            (1)
            ((if (= cell 3)
                (1)
                ((is-fruit cell state coords))))))

    (define (get-code state coords)

        (let (cell (get-cell state coords))
            (if (is-passable cell)
                ((if (is-ghost state coords)
                    (20)
                    ((if (is-eatable cell state coords)
                        (1)
                        (0)))))
                (10))))

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

    (define (min-4 a b c d)

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

    (define (min-3 a b c)

        (min-4 a b c 1000000))

    (define (min-2 a b)

        (min-4 a b 1000000 1000000))

    (define (eatable-distance-loop state visited-tree coords len max-len)

        (if (geq len max-len)
            (1000000)
            ((let (code (get-code state coords)
                   current-len (+ len 1))
                (if (geq code 10)
                    (1000000)
                    ((let (append-result (btree-append visited-tree coords))
                        (if (car append-result)
                            ((if (= code 0)
                                ((let (new-visited (cdr append-result))
                                    (let (result-1 (eatable-distance-loop state new-visited (at-up coords) current-len max-len))
                                        (let (result-2 (eatable-distance-loop state new-visited (at-right coords) current-len result-1))
                                            (let (result-3 (eatable-distance-loop state new-visited (at-down coords) current-len (min-2 result-1 result-2)))
                                                (let (result-4 (eatable-distance-loop state new-visited (at-left coords) current-len (min-3 result-1 result-2 result-3)))
                                                    (+ 1 (min-4 result-1 result-2 result-3 result-4))))))))
                                ((if (= code 1)
                                    (0)
                                    (1000000)))))
                            (1000000)))))))))

    (define (eatable-distance state coords)

        (eatable-distance-loop state (btree-init) coords 0 1000000))

    (define (min-idx-4 a b c d)

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

    (define (wave-search state coords direction)

        (min-idx-4 (if (= direction 2) (1000000) ((eatable-distance state (at-up coords))))
                   (if (= direction 3) (1000000) ((eatable-distance state (at-right coords))))
                   (if (= direction 0) (1000000) ((eatable-distance state (at-down coords))))
                   (if (= direction 1) (1000000) ((eatable-distance state (at-left coords))))))

    (define (step ai-state world-state)

        (let (grid (car world-state)
              man  (elt world-state 1))
            (let (loc (location man)
                  dir (direction man))
                (cons ai-state (wave-search world-state loc dir)))))

    (cons 0 step))
