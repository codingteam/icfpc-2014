(define (main state reserved)
    (define (elt2 lst row col)
      (elt col (elt row lst))
    )
    (define (step ai-state world-state)
      (let (grid (car world-state))
        (let (x (elt2 grid 3 1))
          (dbug x)
          (pair ai-state x)
        )
      )
    )
    (pair 0 step))
