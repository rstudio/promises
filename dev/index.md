# Promises

The promises package brings asynchronous programming capabilities to R.
Asynchronous programming is a technique used by many programming
languages to increase scalability and responsiveness. Traditionally,
this style of programming has not been useful to R users. But the advent
of R web applications like Shiny has made async programming relevant.

This website provides a multi-step guide that will help familiarize you
with several related concepts that are required for effective async
programming. It is highly recommended that you go through the topics in
order.

## Installation

``` r
install.packages("promises")
```

## Contents

### Overview

#### [1. Why use `promises`?](https://rstudio.github.io/promises/dev/articles/promises_01_motivation.md)

Why do we need async programming? What is it good for, and not good for?

#### [2. An informal intro to async programming](https://rstudio.github.io/promises/dev/articles/promises_02_intro.md)

Async programming can require a serious mental shift, even for veteran
programmers. This document attempts to introduce the “average” R user to
the topic, as gently as possible.

#### [3. Working with `promises`](https://rstudio.github.io/promises/dev/articles/promises_03_overview.md)

A more thorough exploration of the concepts behind `promises`, and the
API provided by the `promises` package.

### Integration

#### [4. Launching tasks](https://rstudio.github.io/promises/dev/articles/promises_04_mirai.md)

A guide to the `mirai` package, the place where we expect most async
programming in R to begin.

#### [5a. Launching tasks with `future`](https://rstudio.github.io/promises/dev/articles/promises_05a_futures.md)

A guide to the `future` package, an alternative async programming R
package that pre-dates `mirai`.

#### [5b. Advance `future` and `promises` usage](https://rstudio.github.io/promises/dev/articles/promises_05b_future_promise.md)

Leverage `promises` to make sure that `future` execution does not block
the main R process.

### Usage

#### [6. Using promises with Shiny](https://rstudio.github.io/promises/dev/articles/promises_06_shiny.md)

Learn how to integrate `promises` into your Shiny applications.

#### [7. Combining `promises`](https://rstudio.github.io/promises/dev/articles/promises_07_combining.md)

Functions and techniques for working with multiple `promises`
simultaneously.

#### [8. Case study: converting a Shiny app to async](https://rstudio.github.io/promises/dev/articles/promises_08_casestudy.md)

Walk through the conversion of a realistic Shiny example app to async.
