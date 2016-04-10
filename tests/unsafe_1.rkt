(project
  (vector-ref
    (project
      (inject (vector (inject 1 Integer) (inject 42 Integer) (inject 3 Integer))
              (Vector Any Any Any))
      (Vectorof Any))
    1)
  Integer)
