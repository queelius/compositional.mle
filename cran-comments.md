## R CMD check results

0 errors | 0 warnings | 1 note

The NOTE is about possibly misspelled words in DESCRIPTION:
- "Nocedal" - Jorge Nocedal, author of cited textbook
- "Raphson" - Newton-Raphson method
- "composable" - valid English word

## Test environments

* local Ubuntu 24.04.3 LTS, R 4.3.3
* win-builder R-devel (R Under development, 2026-02-04 r89376 ucrt)
* win-builder R-release

## This is a new submission

This is the first CRAN submission of `compositional.mle`.

## Dependencies

This package depends on `algebraic.mle` which is available on CRAN.

## Package purpose

Provides composable optimization strategies for maximum likelihood estimation.
Solvers are first-class functions that combine via sequential chaining (`%>>%`),
parallel racing (`%|%`), and random restarts (`with_restarts`).
