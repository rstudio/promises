# hybrid_then() validates tee parameter

    Code
      hybrid_then(42, tee = "invalid")
    Condition
      Error in `hybrid_then()`:
      ! `tee` must be `TRUE` or `FALSE`

---

    Code
      hybrid_then(42, tee = 1)
    Condition
      Error in `hybrid_then()`:
      ! `tee` must be `TRUE` or `FALSE`

