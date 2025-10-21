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

# hybrid_then() validates `on_success` and `on_failure` parameters

    Code
      hybrid_then(42, on_success = "invalid")
    Condition
      Error in `check_hybrid_callback()`:
      ! `on_success=` must be a function or `NULL`

---

    Code
      hybrid_then(42, on_failure = "invalid")
    Condition
      Error in `check_hybrid_callback()`:
      ! `on_failure=` must be a function or `NULL`

