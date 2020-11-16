## Test environments
* local Windows 10 install, R 4.0.3
* ubuntu 14.04 (on travis-ci), R 4.0.3
* win-builder (devel and release)

## R CMD check results

Duration: 33.8s

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded

## Responding to concerns raised in previous submission attempt

* Changed examples so that they run. Any files created are saved using `tempdir`.
* Used `on.exit` to restore `par` settings after exiting functions that alter `par`.
