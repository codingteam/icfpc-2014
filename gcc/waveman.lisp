(define (main state reserved)
    (define (elt2 lst coords)
      (elt (elt lst (car coords)) (cdr coords))
    )
    (define (is-wall grid coords)
      (= 0 (elt2 grid coords))
    )
    (define (vitality man) (car man))
    (define (location man) (elt man 1))
    (define (direction man) (elt man 2))
    (define (at-left coords)
      (pair (cdr coords)
            (- (car coords) 1)
      )
    )
    (define (at-right coords)
      (pair (cdr coords)
            (+ 1 (car coords))
      )
    )
    (define (at-up coords)
      (pair (- (cdr coords) 1)
            (car coords)
      )
    )
    (define (at-down coords)
      (pair (+ (cdr coords) 1)
            (car coords)
      )
    )
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
    (define (get-code state coords)
        (let (cell (get-cell state coords))
            (if (is-passable cell)
                ((if (is-ghost cell)
                    (20)
                    ((if (is-eatable cell)
                        (1)
                        (0)))))
                (10))))

    (define (has-coords list coords)
        (if (atom list)
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

    (define (eatable-distance-loop state visited coords)
        (if (has-coords visited coords)
            (1000000)
            ((let (code (get-code state coords))
                (if (= code 0)
                    ((let (new-visited (append visited coords))
                        (+ 1 (min-4 (eatable-distance-loop state new-visited (to-up coords))
                                    (eatable-distance-loop state new-visited (to-right coords))
                                    (eatable-distance-loop state new-visited (to-down coords))
                                    (eatable-distance-loop state new-visited (to-left coords))))))
                    ((if (= code 1)
                        (0)
                        (1000000))))))))

    (define (eatable-distance state coords)
        (eatable-distance-loop state (list) coords))

    (define (wave-search state coords)
        (min-idx-4 (eatable-distance state (to-up coords))
                   (eatable-distance state (to-right coords))
                   (eatable-distance state (to-down coords))
                   (eatable-distance state (to-left coords))))

    (define (step ai-state world-state)
      (let (grid (car world-state)
            man  (elt world-state 1))
          (let (loc (location man)
                dir (direction man))
            (dbug (is-wall grid (in-direction dir loc)))
            (if (is-wall grid (in-direction dir loc))
              ((pair ai-state (rotate-clockwise dir)))
              ((pair ai-state dir))
            )
        )
      )
    )
    (pair 0 step))
