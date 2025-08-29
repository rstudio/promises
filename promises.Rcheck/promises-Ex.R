pkgname <- "promises"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('promises')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("future_promise")
### * future_promise

flush(stderr()); flush(stdout())

### Name: future_promise_queue
### Title: 'future' promise
### Aliases: future_promise_queue future_promise

### ** Examples

## Not run: 
##D # Relative start time
##D start <- Sys.time()
##D # Helper to force two `future` workers
##D with_two_workers <- function(expr) {
##D   if (!require("future")) {
##D     message("`future` not installed")
##D     return()
##D   }
##D   old_plan <- future::plan(future::multisession, workers = 2)
##D   on.exit({future::plan(old_plan)}, add = TRUE)
##D   start <<- Sys.time()
##D   force(expr)
##D   while(!later::loop_empty()) {Sys.sleep(0.1); later::run_now()}
##D   invisible()
##D }
##D # Print a status message. Ex: `"PID: XXX; 2.5s promise done"`
##D print_msg <- function(pid, msg) {
##D   message(
##D     "PID: ", pid, "; ",
##D     round(difftime(Sys.time(), start, units = "secs"), digits = 1), "s " ,
##D     msg
##D   )
##D }
##D 
##D # `"promise done"` will appear after four workers are done and the main R session is not blocked
##D # The important thing to note is the first four times will be roughly the same
##D with_two_workers({
##D   promise_resolve(Sys.getpid()) |>
##D     then(\(x) {print_msg("promise done")})
##D   for (i in 1:6) {
##D     future::future({Sys.sleep(1); Sys.getpid()}) |>
##D       then(\(x) {print_msg("future done")})
##D   }
##D })
##D {
##D #> PID: XXX; 2.5s promise done
##D #> PID: YYY; 2.6s future done
##D #> PID: ZZZ; 2.6s future done
##D #> PID: YYY; 2.6s future done
##D #> PID: ZZZ; 2.6s future done
##D #> PID: YYY; 3.4s future done
##D #> PID: ZZZ; 3.6s future done
##D }
##D 
##D # `"promise done"` will almost immediately, before any workers have completed
##D # The first two `"future done"` comments appear earlier the example above
##D with_two_workers({
##D   promise_resolve(Sys.getpid()) |>
##D     then(\(x) {print_msg("promise")})
##D   for (i in 1:6) {
##D     future_promise({Sys.sleep(1); Sys.getpid()}) |>
##D       then(\(x) {print_msg("future done")})
##D   }
##D })
##D {
##D #> PID: XXX; 0.2s promise done
##D #> PID: YYY; 1.3s future done
##D #> PID: ZZZ; 1.4s future done
##D #> PID: YYY; 2.5s future done
##D #> PID: ZZZ; 2.6s future done
##D #> PID: YYY; 3.4s future done
##D #> PID: ZZZ; 3.6s future done
##D }
## End(Not run)



cleanEx()
nameEx("pipes")
### * pipes

flush(stderr()); flush(stdout())

### Name: pipes
### Title: Promise pipe operators
### Aliases: pipes %...>% %...T>% %...!% %...T!%

### ** Examples

## Not run: 
##D library(mirai)
##D 
##D mirai(cars) %...>%
##D   head(5) %...T>%
##D   print()
##D 
##D # If the read.csv fails, resolve to NULL instead
##D mirai(read.csv("http://example.com/data.csv")) %...!%
##D   { NULL }
## End(Not run)




cleanEx()
nameEx("promise")
### * promise

flush(stderr()); flush(stdout())

### Name: promise
### Title: Create a new promise object
### Aliases: promise

### ** Examples

# Create a promise that resolves to a random value after 2 secs
p1 <- promise(\(resolve, reject) {
  later::later(\() resolve(runif(1)), delay = 2)
})

p1 |> then(print)

# Create a promise that errors immediately
p2 <- promise(\(resolve, reject) {
  reject("An error has occurred")
})
then(p2,
  onFulfilled = \(value) message("Success"),
  onRejected = \(err) message("Failure")
)




cleanEx()
nameEx("promise_all")
### * promise_all

flush(stderr()); flush(stdout())

### Name: promise_all
### Title: Combine multiple promise objects
### Aliases: promise_all promise_race

### ** Examples

p1 <- promise(\(resolve, reject) later::later(\() resolve(1), delay = 1))
p2 <- promise(\(resolve, reject) later::later(\() resolve(2), delay = 2))

# Resolves after 1 second, to the value: 1
promise_race(p1, p2) |>
  then(\(x) {
    cat("promise_race:\n")
    str(x)
  })

# Resolves after 2 seconds, to the value: list(1, 2)
promise_all(p1, p2) |>
  then(\(x) {
    cat("promise_all:\n")
    str(x)
  })




cleanEx()
nameEx("promise_map")
### * promise_map

flush(stderr()); flush(stdout())

### Name: promise_map
### Title: Promise-aware lapply/map
### Aliases: promise_map

### ** Examples

# Waits x seconds, then returns x*10
wait_this_long <- function(x) {
  promise(\(resolve, reject) {
    later::later(\() resolve(x*10), delay = x)
  })
}

promise_map(
  list(A=1, B=2, C=3),
  wait_this_long
) |>
  then(print)




cleanEx()
nameEx("promise_reduce")
### * promise_reduce

flush(stderr()); flush(stdout())

### Name: promise_reduce
### Title: Promise-aware version of Reduce
### Aliases: promise_reduce

### ** Examples

# Returns a promise for the sum of e1 + e2, with a 0.5 sec delay
slowly_add <- function(e1, e2) {
  promise(\(resolve, reject) {
    later::later(\() resolve(e1 + e2), delay = 0.5)
  })
}

# Prints 55 after a little over 5 seconds
promise_reduce(1:10, slowly_add, .init = 0) |>
  then(print)




cleanEx()
nameEx("promise_resolve")
### * promise_resolve

flush(stderr()); flush(stdout())

### Name: promise_resolve
### Title: Create a resolved or rejected promise
### Aliases: promise_resolve promise_reject

### ** Examples

promise_resolve(mtcars) |>
  then(head) |>
  then(print)

promise_reject("Something went wrong") |>
  catch(tee = TRUE, \(e) message(conditionMessage(e)))




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
