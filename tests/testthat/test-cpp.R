describe("C++ interface", {
  env <- new.env()
  Rcpp::sourceCpp(system.file("promise_task.cpp", package = "promises"), env = env)

  it("basically works", {
    promise(function(resolve, reject) {
      env$asyncFib(resolve, reject, 3)
    }) %...>%
      {
        expect_identical(., 2)
      } %>%
      wait_for_it()
  })

  it("works with domains", {
    cd <- create_counting_domain()

    expect_true(is.null(current_promise_domain()))
    with_promise_domain(cd, {
      promise(function(resolve, reject) {
        env$asyncFib(resolve, reject, 3)
      }) %...>%
        {
          expect_identical(., 2)
          expect_identical(cd$counts$onFulfilledCalled, 1L)
          promise_resolve(TRUE) %...>%
            {
              expect_true(!is.null(current_promise_domain()))
              expect_identical(cd$counts$onFulfilledCalled, 2L)
            }
        } %>%
        wait_for_it()
    })
  })
})
