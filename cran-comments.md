## Test environments
* local Windows 10 install, R 4.0.3
* ubuntu 14.04 (on travis-ci), R 4.0.3
* win-builder (devel and release)

## R CMD check results

Duration: 38.7s

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded

There was 1 NOTE:

* New submission
* Possibly mis-spelled words in DESCRIPTION:
    - Spirographs (2:8)
    - epitrochoids (11:32)
    - hypotrochoids (11:14)
    - spirograph (13:69)
    - svg (14:28)

All these words are correctly spelled.

## Responding to concerns raised in previous submission attempt

* Revised title to explain what the package does.
* Used `on.exit` to restore `par` settings after exiting functions that alter `par`.
