# Promises

The promises package brings asynchronous programming capabilities to R. Asynchronous programming is a technique used by many programming languages to increase scalability and responsiveness. Traditionally, this style of programming has not been useful to R users. But the advent of R web applications like Shiny has made async programming relevant.

This website provides a multi-step guide that will help familiarize you with several related concepts that are required for effective async programming. It is highly recommended that you go through the topics in order.

---

## Installation

The promises package is not yet on CRAN, nor is an async-compatible version of Shiny. You can install both promises and an async-compatible version of Shiny with this command:

```r
devtools::install_github("rstudio/shiny")
```

There are also some supporting packages that have experimental async-compatible branches:

```r
devtools::install_github("ramnathv/htmlwidgets@async")
devtools::install_github("jcheng5/plotly@joe/feature/async")
devtools::install_github("rstudio/DT@async")
```

---

<style>
.contents a.anchor { display: none; }
</style>

## Contents

#### [1. Why use promises?](articles/motivation.html)

Why do we need async programming? What is it good for, and not good for?

#### [2. An informal intro to async programming](articles/intro.html)

Async programming can require a serious mental shift, even for veteran programmers. This document attempts to introduce the "average" R user to the topic, as gently as possible.

#### [3. Working with promises](articles/overview.html)

A more thorough exploration of the concepts behind promises, and the API provided by the promises package.

#### [4. Launching tasks](articles/futures.html)

A guide to the future package, the place where we expect most async programming in R to begin.

#### [5. Using promises with Shiny](articles/shiny.html)

Learn how to integrate promises into your Shiny applications.

#### [6. Combining promises](articles/combining.html)

Functions and techniques for working with multiple promises simultaneously.
