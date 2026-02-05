# Repository Guidelines

## Project Structure & Module Organization
- Package source lives in `R/` (solvers, composition operators, tracing, configuration). Keep new APIs exported via roxygen comments (`@export`) and rely on `devtools::document()` to refresh `NAMESPACE` and `man/`.
- Tests are under `tests/testthat/`; prefer `test-<topic>.R` files that mirror the module names (e.g., `test-solvers.R`, `test-transformers.R`).
- Documentation sources: `README.Rmd` (rendered into `README.md`), vignettes in `vignettes/`, and pkgdown site output in `docs/` driven by `_pkgdown.yml`.
- Additional references: `DESIGN.md` for architecture context and `TESTING_SUMMARY.md` for historical coverage notes.

## Build, Test, and Development Commands
- Load package for interactive dev: `R -q -e "devtools::load_all()"`.
- Run unit + integration tests: `R -q -e "devtools::test()"` (uses `tests/testthat`).
- Full check before PRs:  
  ```bash
  R CMD build .
  R CMD check --as-cran compositional.mle_*.tar.gz
  ```
- Regenerate docs after API changes: `R -q -e "devtools::document()"`. Rebuild site when docs change: `R -q -e "pkgdown::build_site()"`.

## Coding Style & Naming Conventions
- Use base R style with 2-space indents, no tabs. Keep line length reasonable (~100 chars) and prefer explicit argument names.
- Functions and variables use `snake_case`; operators stay descriptive (`%>>%`, `%|%`, `with_restarts()`).
- Validate inputs early with `stopifnot()`/`stop()`, and keep solver outputs consistent (`theta.hat`, `loglike`, `trace`).
- Write roxygen comments for all exported functions, including examples that set seeds for reproducibility.

## Testing Guidelines
- Framework: testthat (edition 3). Name tests with the behavior under test, grouping by module. Use deterministic seeds and small synthetic data to avoid flaky convergence.
- Quick targeted run: `R -q -e "testthat::test_file('tests/testthat/test-solvers.R')"`.
- When adding solvers, include unit coverage for constraints, tracing, and failure modes; add integration paths in `test-integration.R` where appropriate.

## Commit & Pull Request Guidelines
- Commit messages follow short, imperative summaries (e.g., `Add Remotes field for algebraic.mle dependency`). Squash fixup noise locally.
- PRs should describe the change, note any new commands or tuning flags, and list tests executed. Link related issues and include output snippets when convergence tuning changes behavior.
- Avoid committing generated artifacts unless intentional (`docs/` pkgdown output is versioned); never edit `README.md` directly—update `README.Rmd`.
