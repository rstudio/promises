# chromote

<details>

* Version: 0.5.1
* GitHub: https://github.com/rstudio/chromote
* Source code: https://github.com/cran/chromote
* Date/Publication: 2025-04-24 03:40:02 UTC
* Number of recursive dependencies: 51

Run `revdepcheck::revdep_details(, "chromote")` for more info

</details>

## In both

*   checking running R code from vignettes ...
    ```
      ‘chromote.Rmd’ using ‘UTF-8’... failed
      ‘commands-and-events.Rmd’ using ‘UTF-8’... OK
      ‘example-attach-existing.Rmd’ using ‘UTF-8’... OK
      ‘example-authentication.Rmd’ using ‘UTF-8’... OK
      ‘example-cran-tests.Rmd’ using ‘UTF-8’... OK
      ‘example-custom-headers.Rmd’ using ‘UTF-8’... OK
      ‘example-custom-user-agent.Rmd’ using ‘UTF-8’... OK
      ‘example-extract-text.Rmd’ using ‘UTF-8’... OK
      ‘example-loading-page.Rmd’ using ‘UTF-8’... OK
      ‘example-remote-hosts.Rmd’ using ‘UTF-8’... OK
    ...
      9.             └─knitr::knit(..., tangle = opts_knit$get("tangle"), envir = envir)
     10.               └─xfun::read_utf8(input)
     11.                 └─base::readLines(con, encoding = "UTF-8", warn = FALSE)
     12.                   └─base::file(con, "r")
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
      When tangling ‘chromote.Rmd’:
    Error: cannot open the connection
    Execution halted
    ```

# dipsaus

<details>

* Version: 0.3.1
* GitHub: https://github.com/dipterix/dipsaus
* Source code: https://github.com/cran/dipsaus
* Date/Publication: 2025-04-03 16:50:02 UTC
* Number of recursive dependencies: 62

Run `revdepcheck::revdep_details(, "dipsaus")` for more info

</details>

## In both

*   checking compiled code ... NOTE
    ```
    File ‘dipsaus/libs/dipsaus.so’:
      Found non-API calls to R: ‘CLOENV’, ‘ENCLOS’
    
    Compiled code should not call non-API entry points in R.
    
    See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual,
    and section ‘Moving into C API compliance’ for issues with the use of
    non-API entry points.
    ```

# ellmer

<details>

* Version: 0.2.0
* GitHub: https://github.com/tidyverse/ellmer
* Source code: https://github.com/cran/ellmer
* Date/Publication: 2025-05-17 22:20:02 UTC
* Number of recursive dependencies: 65

Run `revdepcheck::revdep_details(, "ellmer")` for more info

</details>

## In both

*   checking running R code from vignettes ...
    ```
      ‘ellmer.Rmd’ using ‘UTF-8’... failed
      ‘programming.Rmd’ using ‘UTF-8’... failed
      ‘prompt-design.Rmd’ using ‘UTF-8’... failed
      ‘streaming-async.Rmd’ using ‘UTF-8’... OK
      ‘structured-data.Rmd’ using ‘UTF-8’... failed
      ‘tool-calling.Rmd’ using ‘UTF-8’... failed
     ERROR
    Errors in running code in vignettes:
    when running code in ‘ellmer.Rmd’
      ...
    ...
    
    > get_current_time <- function(tz = "UTC") {
    +     format(Sys.time(), tz = tz, usetz = TRUE)
    + }
    
    > chat <- chat_openai(model = "gpt-4o")
    
      When sourcing ‘tool-calling.R’:
    Error: Can't find env var `OPENAI_API_KEY`.
    Execution halted
    ```

# plumbertableau

<details>

* Version: 0.1.1
* GitHub: https://github.com/rstudio/plumbertableau
* Source code: https://github.com/cran/plumbertableau
* Date/Publication: 2023-12-19 02:20:03 UTC
* Number of recursive dependencies: 64

Run `revdepcheck::revdep_details(, "plumbertableau")` for more info

</details>

## In both

*   checking running R code from vignettes ...
    ```
      ‘introduction.Rmd’ using ‘UTF-8’... failed
      ‘publishing-extensions.Rmd’ using ‘UTF-8’... OK
      ‘r-developer-guide.Rmd’ using ‘UTF-8’... failed
      ‘tableau-developer-guide.Rmd’ using ‘UTF-8’... OK
     ERROR
    Errors in running code in vignettes:
    when running code in ‘introduction.Rmd’
      ...
    > knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
    
    ...
    > set.seed(35487)
    
    > knitr::read_chunk(path = "../inst/plumber/loess/plumber.R", 
    +     labels = "loess")
    Warning in file(con, "r") :
      cannot open file '../inst/plumber/loess/plumber.R': No such file or directory
    
      When sourcing ‘r-developer-guide.R’:
    Error: cannot open the connection
    Execution halted
    ```

# Prostar

<details>

* Version: 1.40.0
* GitHub: https://github.com/edyp-lab/Prostar
* Source code: https://github.com/cran/Prostar
* Date/Publication: 2025-04-15
* Number of recursive dependencies: 189

Run `revdepcheck::revdep_details(, "Prostar")` for more info

</details>

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘DAPARdata’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# tapLock

<details>

* Version: 0.2.0
* GitHub: https://github.com/ixpantia/tapLock
* Source code: https://github.com/cran/tapLock
* Date/Publication: 2024-10-28 11:50:08 UTC
* Number of recursive dependencies: 64

Run `revdepcheck::revdep_details(, "tapLock")` for more info

</details>

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
      Missing dependency on R >= 4.1.0 because package code uses the pipe
      |> or function shorthand \(...) syntax added in R 4.1.0.
      File(s) using such syntax:
        ‘auth0.R’ ‘config.R’ ‘entra_id.R’ ‘google.R’ ‘utils.R’
    ```

# telegram.bot

<details>

* Version: 3.0.0
* GitHub: https://github.com/ebeneditos/telegram.bot
* Source code: https://github.com/cran/telegram.bot
* Date/Publication: 2022-09-07 15:40:02 UTC
* Number of recursive dependencies: 98

Run `revdepcheck::revdep_details(, "telegram.bot")` for more info

</details>

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) sendChatAction.Rd:16: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) sendChatAction.Rd:17: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) sendChatAction.Rd:18: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) sendChatAction.Rd:19: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) sendChatAction.Rd:20: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) sendChatAction.Rd:21: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) sendChatAction.Rd:22: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) sendChatAction.Rd:23: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) sendChatAction.Rd:24: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) sendChatAction.Rd:25: Lost braces in \itemize; meant \describe ?
    ```

# TKCat

<details>

* Version: 1.1.12
* GitHub: https://github.com/patzaw/TKCat
* Source code: https://github.com/cran/TKCat
* Date/Publication: 2025-03-17 14:50:02 UTC
* Number of recursive dependencies: 122

Run `revdepcheck::revdep_details(, "TKCat")` for more info

</details>

## In both

*   checking running R code from vignettes ...
    ```
      ‘TKCat-KMR-POK.Rmd’ using ‘UTF-8’... OK
      ‘TKCat.Rmd’ using ‘UTF-8’... failed
     ERROR
    Errors in running code in vignettes:
    when running code in ‘TKCat.Rmd’
      ...
    
    > knitr::opts_chunk$set(eval = Sys.getenv("USER") %in% 
    +     c("pgodard"))
    
    > k <- chTKCat(host = "localhost", port = 9111, drv = ClickHouseHTTP::ClickHouseHTTP(), 
    +     user = "default", password = "")
    
      When sourcing ‘TKCat.R’:
    Error: Could not connect to server [localhost]: Failed to connect to localhost port 9111 after 0 ms: Could not connect to server
    Execution halted
    ```

