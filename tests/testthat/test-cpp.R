describe("C++ interface", {
  # `cpp11::cpp_source()` errors if these packages are not installed:
  skip_if_not_installed("brio")
  skip_if_not_installed("callr")
  skip_if_not_installed("cli")
  skip_if_not_installed("decor")
  skip_if_not_installed("desc")
  skip_if_not_installed("glue")
  skip_if_not_installed("tibble")
  skip_if_not_installed("vctrs")

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
