(define (main state reserved)
    (define (step ai-state world-state)
      (let (grid (car ai-state))
        (let (first-row (car grid))
          (let (first-item (car first-row))
            (pair ai-state first-item)
          )
        )
      )
    )
    (pair 0 step))
