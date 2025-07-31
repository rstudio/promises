# tee types are handled correctly

    Code
      then(promise_resolve(1), tee = 4)
    Condition
      Error in `then()`:
      ! `tee` must be `TRUE` or `FALSE`, not the number 4.

---

    Code
      catch(promise_resolve(1), function(err) { }, tee = "4")
    Condition
      Error in `catch()`:
      ! `tee` must be `TRUE` or `FALSE`, not the string "4".

