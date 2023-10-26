## ggwordcloud 0.6.1

## Test environments
* local Windows 11, R 4.2.1, mingw x86_64
* rhub: "Debian Linux, R-devel, GCC ASAN/UBSAN", "Windows Server 2022, R-oldrel, 32/64 bit", "Windows Server 2022, R-devel, 64 bit"
* devtools: check, check_win_oldrelease, check_mac_release

## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a fix only release to pass an issue spotted by Pr. Ripley in the test: using Unicode chars not available to pdf(). I have removed the non-latin characters from the test (as I had removed them from the examples in 0.6.0)
