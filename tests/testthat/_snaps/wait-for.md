# wait_for() works with mirai promise that rejects

    Code
      wait_for(p)
    Condition
      Error in `callback()`:
      ! Error: there was an error

# wait_for() throws when mirai then() callback errors

    Code
      wait_for(p)
    Condition
      Error in `onFulfilled()`:
      ! chain error

