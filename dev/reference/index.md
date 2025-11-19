# Package index

## Handling promises

- [`then()`](https://rstudio.github.io/promises/dev/reference/then.md)
  [`catch()`](https://rstudio.github.io/promises/dev/reference/then.md)
  [`finally()`](https://rstudio.github.io/promises/dev/reference/then.md)
  : Access the results of a promise

- [`hybrid_then()`](https://rstudio.github.io/promises/dev/reference/hybrid_then.md)
  :

  Asynchronous or synchronous
  [`then()`](https://rstudio.github.io/promises/dev/reference/then.md)

- [`` `%...>%` ``](https://rstudio.github.io/promises/dev/reference/pipes.md)
  [`` `%...T>%` ``](https://rstudio.github.io/promises/dev/reference/pipes.md)
  [`` `%...!%` ``](https://rstudio.github.io/promises/dev/reference/pipes.md)
  [`` `%...T!%` ``](https://rstudio.github.io/promises/dev/reference/pipes.md)
  **\[superseded\]** : Promise pipe operators

## Combining promises

- [`promise_all()`](https://rstudio.github.io/promises/dev/reference/promise_all.md)
  [`promise_race()`](https://rstudio.github.io/promises/dev/reference/promise_all.md)
  : Combine multiple promise objects

## Functional promises

- [`promise_map()`](https://rstudio.github.io/promises/dev/reference/promise_map.md)
  : Promise-aware lapply/map
- [`promise_reduce()`](https://rstudio.github.io/promises/dev/reference/promise_reduce.md)
  : Promise-aware version of Reduce

## Creating promises

- [`promise()`](https://rstudio.github.io/promises/dev/reference/promise.md)
  : Create a new promise object

- [`future_promise_queue()`](https://rstudio.github.io/promises/dev/reference/future_promise.md)
  [`future_promise()`](https://rstudio.github.io/promises/dev/reference/future_promise.md)
  :

  future promise

- [`promise_resolve()`](https://rstudio.github.io/promises/dev/reference/promise_resolve.md)
  [`promise_reject()`](https://rstudio.github.io/promises/dev/reference/promise_resolve.md)
  : Create a resolved or rejected promise

- [`is.promise()`](https://rstudio.github.io/promises/dev/reference/is.promise.md)
  [`is.promising()`](https://rstudio.github.io/promises/dev/reference/is.promise.md)
  [`as.promise()`](https://rstudio.github.io/promises/dev/reference/is.promise.md)
  : Coerce to a promise

## Domains

- [`with_promise_domain()`](https://rstudio.github.io/promises/dev/reference/with_promise_domain.md)
  [`new_promise_domain()`](https://rstudio.github.io/promises/dev/reference/with_promise_domain.md)
  : Promise domains

## OpenTelemetry

- [`with_otel_span()`](https://rstudio.github.io/promises/dev/reference/otel.md)
  [`with_otel_promise_domain()`](https://rstudio.github.io/promises/dev/reference/otel.md)
  [`local_otel_promise_domain()`](https://rstudio.github.io/promises/dev/reference/otel.md)
  : OpenTelemetry integration
