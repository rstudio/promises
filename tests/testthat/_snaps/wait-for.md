# wait_for() validates timeout

    Code
      wait_for(p, timeout = -1)
    Condition
      Error in `wait_for()`:
      ! `timeout` must be a single non-negative number
    Code
      wait_for(p, timeout = NA)
    Condition
      Error in `wait_for()`:
      ! `timeout` must be a single non-negative number
    Code
      wait_for(p, timeout = c(1, 2))
    Condition
      Error in `wait_for()`:
      ! `timeout` must be a single non-negative number
    Code
      wait_for(p, timeout = "1")
    Condition
      Error in `wait_for()`:
      ! `timeout` must be a single non-negative number

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

