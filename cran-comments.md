## ggwordcloud 0.6.0

## Test environments
* local Windows 11, R 4.2.1, mingw x86_64
* rhub: "Debian Linux, R-devel, GCC ASAN/UBSAN", "Windows Server 2022, R-oldrel, 32/64 bit", "Windows Server 2022, R-devel, 64 bit"
* devtools: check, check_win_oldrelease, check_mac_release

## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release fixing in particular the example issues in CRAN: non latin annotations and too long runtime.
* Note: Some UTF8 encoded strings are present but only in some datasets used only in the vignette.
* I have contacted the maintainers of the packages using mine, even if there should be no breaking change as hinted by a successful revdepcheck.
