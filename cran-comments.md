## Submission

First submission of FoSci.

## Test environments

* GitHub Actions: Ubuntu (R devel, release, oldrel-1), macOS (release), Windows (release).
* Local: Windows 11 x64, R 4.6.2 ucrt.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Notes for CRAN

* Network access is used only by `fs_collect_openalex()`; its example is wrapped in `\donttest{}` and checks for an internet connection. Tests use synthetic data and never access the network.
* `fs_simulate()` sets a seed only for its own computation and restores the user's random number state on exit.
* All example data in `inst/extdata` are synthetic; names, journals and DOIs are fictitious.
* Examples and tests write only to `tempdir()`.
