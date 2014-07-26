(define (main)
  (let (grid (list (list 0 1 2)
                   (list 3 4 5)))
        (let (x (elt (elt grid 1) 0))
          (dbug x)
          x
        )
      )
    )
