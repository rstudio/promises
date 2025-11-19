# Promise pipe operators

With R 4.1, the promise pipe operators are **\[superseded\]** by
[`then`](https://rstudio.github.io/promises/dev/reference/then.md),
[`catch`](https://rstudio.github.io/promises/dev/reference/then.md), and
[`finally`](https://rstudio.github.io/promises/dev/reference/then.md)
methods when used in tandem with the function shorthand (`\(x) rhs(x)`)
and `|>`.

## Usage

``` r
lhs %...>% rhs

lhs %...T>% rhs

lhs %...!% rhs

lhs %...T!% rhs
```

## Arguments

- lhs:

  A promise object.

- rhs:

  A function call using the magrittr semantics. It can return either a
  promise or non-promise value, or throw an error.

## Value

A new promise.

## Details

Promise-aware pipe operators, in the style of
[magrittr](https://CRAN.R-project.org/package=magrittr/vignettes/magrittr.html).
Like magrittr pipes, these operators can be used to chain together
pipelines of promise-transforming operations. Unlike magrittr pipes,
these pipes wait for promise resolution and pass the unwrapped value (or
error) to the `rhs` function call.

The `>` variants are for handling successful resolution, the `!`
variants are for handling errors. The `T` variants of each return the
lhs instead of the rhs, which is useful for pipeline steps that are used
for side effects (printing, plotting, saving).

1.  `promise %...>% func()` is equivalent to `promise %>% then(func)`.

2.  `promise %...!% func()` is equivalent to `promise %>% catch(func)`.

3.  `promise %...T>% func()` is equivalent to `promise %T>% then(func)`.

4.  `promise %...T!% func()` is equivalent to `promise %T>% catch(func)`
    or `promise %>% catch(func, tee = TRUE)`.

One situation where 3. and 4. above break down is when `func()` throws
an error, or returns a promise that ultimately fails. In that case, the
failure will be propagated by our pipe operators but not by the
magrittr-plus-function "equivalents".

For simplicity of implementation, we do not support the magrittr feature
of using a `.` at the head of a pipeline to turn the entire pipeline
into a function instead of an expression.

## See also

https://rstudio.github.io/promises/articles/promises_03_overview.html#using-pipes

## Examples

``` r
if (FALSE) { # \dontrun{
library(mirai)

mirai(cars) %...>%
  head(5) %...T>%
  print()

# If the read.csv fails, resolve to NULL instead
mirai(read.csv("http://example.com/data.csv")) %...!%
  { NULL }
} # }
```
