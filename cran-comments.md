## Changes in 2.0.1

Failed solver attempts now preserve their list positions in `race()` and
`with_restarts()`. This prevents a failed final restart from discarding an
earlier successful result and preserves the correspondence between solver
indices and diagnostic alternatives. Regression tests cover failed first and
last attempts, and runs where every attempt fails.

## Test environment

* Ubuntu 24.04.3 LTS, R 4.3.3, x86_64-pc-linux-gnu
* Local source build and R CMD check --as-cran, 2026-10-28

## R CMD check results

0 errors | 0 warnings | 2 notes

* Checking for future file timestamps: unable to verify current time.
  This is an environment/network time-verification limitation.
* HTML manual validation was skipped because `tidy` is not installed and
  math rendering checks were skipped because the suggested check tool `V8`
  is unavailable in this environment.

Package installation, examples (including --run-donttest), tests, vignette
rebuilding, and the PDF manual all passed. The installed-package tests include
the parallel solver race, which is skipped by the development-load test runner.
