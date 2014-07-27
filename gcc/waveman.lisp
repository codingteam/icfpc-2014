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

    (define (is-passable walls coords)
        (not (has-coords walls coords))
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

    (define (is-fruit cell state coords)

        (if (= cell FRUIT)
            ((let (fruit-state (cdr (cdr (cdr state))))
                (gt fruit-state 0)))
            (FALSE)))

    (define (is-eatable cell state coords)

        (if (= cell PILL)
            (TRUE)
            ((if (= cell POWERPILL)
                (TRUE)
                ((is-fruit cell state coords))))))

    (define (get-code walls state coords)

        (if (has-coords walls coords)
          (WALLCODE)
          (
           (if (is-ghost state coords)
              (GHOST)
              (
               (let (cell (get-cell state coords))
                        (if (is-eatable cell state coords)
                            (EATABLE)
                            (0)))
              )
          )
        )
      )
    )

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

        (min-4 a b c 1000000))

    (define (min-2 a b)

        (min-4 a b 1000000 1000000))

    (define (eatable-distance-loop walls state visited-tree coords len max-len)

        (if (geq len max-len)
            (1000000)
            ((let (code (get-code walls state coords)
                   current-len (+ len 1))
                (if (geq code WALLCODE)
                    (1000000)
                    ((let (append-result (btree-append visited-tree coords))
                        (if (car append-result)
                            ((if (= code 0)
                                ((let (new-visited (cdr append-result))
                                    (let (result-1 (eatable-distance-loop walls state new-visited (at-up coords) current-len max-len))
                                        (let (result-2 (eatable-distance-loop walls state new-visited (at-right coords) current-len result-1))
                                            (let (result-3 (eatable-distance-loop walls state new-visited (at-down coords) current-len (min-2 result-1 result-2)))
                                                (let (result-4 (eatable-distance-loop walls state new-visited (at-left coords) current-len (min-3 result-1 result-2 result-3)))
                                                    (+ 1 (min-4 result-1 result-2 result-3 result-4))))))))
                                ((if (= code EATABLE)
                                    (0)
                                    (1000000)))))
                            (1000000)))))))))

    (define (eatable-distance walls state coords)

        (eatable-distance-loop walls state (btree-init) coords 0 1000000))

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

    (define (wave-search state walls coords direction)

        (min-idx-4 (if (= direction DOWN)  (1000000) ((eatable-distance walls state (at-up coords))))
                   (if (= direction LEFT)  (1000000) ((eatable-distance walls state (at-right coords))))
                   (if (= direction UP)    (1000000) ((eatable-distance walls state (at-down coords))))
                   (if (= direction RIGHT) (1000000) ((eatable-distance walls state (at-left coords))))))

    (define (step walls world-state)
        (let (man (elt world-state 1))
            (let (loc (location man)
                  dir (direction man))
                (cons walls (wave-search world-state walls loc dir)))))
    
    (define (find-walls map)
      (let (row-index 0
            col-index 0
            walls nil)
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
                (if (= cell WALL)
                  (
                   (set walls (cons (cons col-index row-index) walls))
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
        walls
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

    (cons (find-walls (car state)) step)
)

