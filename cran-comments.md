## Comments

#### 2021-02-11

Had a bad regex in .Rbuildignore. My fault. Fixed the regex.

Also reduced a couple image file sizes.

Thank you,
Joe

#### 2021-02-11

Thanks, we see:

   Size of tarball: 6063083 bytes

Not more than 5 MB for a CRAN package, please.

Best,
Uwe Ligges

#### 2021-02-10

Bug fixes and a new function.

Thank you,
Joe


## Test environments and R CMD check results

* local macOS install 10.15.7
  * R 4.0
* GitHub Actions - https://github.com/rstudio/promises/pull/61/checks
  * macOS - R devel
  * macOS, windows, ubuntu 16 - R 4.0
  * ubuntu 16 - R 3.6
  * ubuntu 16 - R 3.5
  * ubuntu 16 - R 3.4
  * ubuntu 16 - R 3.3

* win-builder
  * devel
  * release
  * oldrelease

#### R CMD check results

0 errors | 0 warnings | 0 notes

## revdepcheck results

We checked 17 reverse dependencies (16 from CRAN + 1 from BioConductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
