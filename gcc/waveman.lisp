#include "constants.h"

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

      (if (= dir UP)
        ((at-up coords))
        ((if (= dir RIGHT)
           ((at-right coords))
           ((if (= dir DOWN)
              ((at-down coords))
              ((at-left coords))
            )
          )
         )
       )
      )
    )
    (define (rotate-clockwise dir)

      (if (= dir LEFT)
        (UP)
        ((+ dir 1))
      )
    )

    (define (get-cell state coords)

        (let (map (car state))
            (elt2 map coords)))

    (define (is-passable cell)
      cell
    )

    (define (is-ghost-loop ghosts x y)
        (if (empty-list ghosts)
            (FALSE)
            ((let (ghost (car ghosts))
                (let (ghost-pos (car (cdr ghost)))
                    (let (ghost-x (car ghost-pos)
                          ghost-y (cdr ghost-pos))
                        (if (= x ghost-x)
                            ((if (= y ghost-y)
                                (TRUE)
                                ((is-ghost-loop (cdr ghosts) x y))))
                            ((is-ghost-loop (cdr ghosts) x y)))))))))

    (define (is-ghost state coords)
        (let (ghosts (car (cdr (cdr state)))
              x (car coords)
              y (cdr coords))
            (is-ghost-loop ghosts x y)))

    (define (fruit-score cell state coords)
        (if (= cell FRUIT)
            ((let (fruit-state (cdr (cdr (cdr state))))
                (* fruit-state 500)
            ))
            (0)
        )
    )

    (define (eatable-score cell state coords)
        (if (= cell PILL)
            (PILLSCORE)
            ((if (= cell POWERPILL)
                (POWERPILLSCORE)
                ((fruit-score cell state coords))))))


   (define (get-code state coords)
       (let (cell (get-cell state coords))
           (if (is-passable cell)
               ((if (is-ghost state coords)
                   (GHOST)
                   ((eatable-score cell state coords))
               ))
               (WALLCODE))))

    (define (empty-list list)
        (atom list))

    (define (not x)
      (= x 0))

#include "btree.lisp"

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

        (min-4 a b c INFINITY))

    (define (min-2 a b)

        (min-4 a b INFINITY INFINITY))

    (define (eatable-distance-loop penalty old state visited-tree coords dir len max-len)
        (if (geq len max-len)
            (max-len)
            (
             (if (btree-check old coords)
              (penalty)
              ((let (code (get-code state coords)
                     current-len (+ len 1)
                     penalty (+ penalty VISITED_PENALTY)
                     man (car (cdr state)))
                (let (lives (elt man 3)
                      vitality (car man))
                   (let (to-stop (if (+ (= lives 3) vitality)
                                   ((= code WALLCODE))
                                   ((geq code WALLCODE))
                                 ) )
                    (if to-stop
                        ( max-len)
                        ((let (append-result (btree-append visited-tree coords))
                            (if (car append-result)
                                ((if (= code PASSABLE)
                                    ((let (new-visited (cdr append-result)
                                           result max-len
                                           src-dir dir
                                          )
                                        (set result (eatable-distance-loop penalty old state new-visited (in-direction dir coords) src-dir current-len result))
                                        (set dir (rotate-clockwise dir))
                                        (set result (eatable-distance-loop penalty old state new-visited (in-direction dir coords) src-dir current-len result))
                                        (set dir (rotate-clockwise dir))
                                        (set result (eatable-distance-loop penalty old state new-visited (in-direction dir coords) src-dir current-len result))
                                        (set dir (rotate-clockwise dir))
                                        (set result (eatable-distance-loop penalty old state new-visited (in-direction dir coords) src-dir current-len result))
                                        (+ 1 result)
                                    ))
                                    ((- 0 code))
                                ))
                                (max-len)
                            )
                       ))
                    )
                  )
                )
              ))
             )
           )
        )
    )

    (define (eatable-distance penalty visited state dir coords)

        (eatable-distance-loop penalty visited state (btree-init) coords dir 0 INFINITY))

    (define (min-idx-4 dir a b c d)
      (if (= (+ (= a INFINITY) (+ (= b INFINITY) (+ (= c INFINITY) (= d INFINITY)))) 4)
        ((rotate-clockwise dir))
        (
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
                      (0))))))
        )
      )
    )

    (define (wave-search state penalty visited coords direction)

        (min-idx-4 direction
                   (if (= direction DOWN)  (INFINITY) ((eatable-distance penalty visited state DOWN  (at-up coords))))
                   (if (= direction LEFT)  (INFINITY) ((eatable-distance penalty visited state LEFT  (at-right coords))))
                   (if (= direction UP)    (INFINITY) ((eatable-distance penalty visited state UP    (at-down coords))))
                   (if (= direction RIGHT) (INFINITY) ((eatable-distance penalty visited state RIGHT (at-left coords))))))

    (define (step ai-state world-state)
        (let (man (elt world-state 1))
            (let (loc (location man)
                  dir (direction man)
                  penalty (car ai-state)
                  visited (cdr ai-state))
              (let (append-result (btree-append visited loc))
                (cons
                  (cons (+ penalty VISITED_PENALTY) (cdr append-result))
                  (wave-search world-state penalty visited loc dir)
                )
              )
            )
        )
    )
    
    (define (find-powerpills map)
      (let (row-index 0
            col-index 0
            powerpills nil)
        (let (current-row map
              other-rows map)
          (do
            (set current-row (car other-rows))
            (set other-rows (cdr other-rows))
            (let (cell current-row
                  other-cells current-row)
              (set col-index 0)
              (do
                (set cell (car other-cells))
                (set other-cells (cdr other-cells))
                (if (= cell POWERPILL)
                  (
                   (set powerpills (cons (cons col-index row-index) powerpills))
                  )
                  ()
                )
                (set col-index (+ col-index 1))
                (= 0 (atom other-cells))
              )
            )
            (set row-index (+ row-index 1))
            (= 0 (atom other-rows))
          )
        )
        powerpills
      )
    )

    (cons (cons VISITED_PENALTY nil) step)
)

