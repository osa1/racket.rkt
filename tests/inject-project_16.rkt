(project
  (vector-ref-dynamic
    (project
      (inject
        (vector (inject 42 Integer) (inject 123 Integer))
        (Vector Any Any))
      (Vectorof Any))
    (project (inject 0 Integer) Integer))
  Integer)
