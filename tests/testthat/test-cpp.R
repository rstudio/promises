print(library())

describe("C++ interface", {
  # `cpp11::cpp_source()` errors if these packages are not installed:
  # brio, callr, cli, decor, desc, glue, tibble, vctrs
  skip_on_cran()

  env <- new.env()
  cpp11::cpp_source(
    system.file("promise_task.cpp", package = "promises"),
    env = env
  )

  it("basically works", {
    promise(function(resolve, reject) {
      env$asyncFib(resolve, reject, 3)
    }) |>
      then(\(x) {
        expect_identical(x, 2)
      }) |>
      wait_for_it()
  })

  it("works with domains", {
    cd <- create_counting_domain()

    expect_true(is.null(current_promise_domain()))
    with_promise_domain(cd, {
      promise(function(resolve, reject) {
        env$asyncFib(resolve, reject, 3)
      }) |>
        then(\(x) {
          expect_identical(x, 2)
          expect_identical(cd$counts$onFulfilledCalled, 1L)
          promise_resolve(TRUE) |>
            then(\(y) {
              expect_true(!is.null(current_promise_domain()))
              expect_identical(cd$counts$onFulfilledCalled, 2L)
            })
        }) |>
        wait_for_it()
    })
  })
})
