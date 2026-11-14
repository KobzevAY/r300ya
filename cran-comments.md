## Test environments

* local Windows 10, R 4.5.1
* GitHub Actions: macOS (release), Windows (release),
  Ubuntu (devel, release, oldrel-1)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Notes for reviewers

* The examples are wrapped in `\dontrun{}` and the vignette code is not
  evaluated because every call to the service requires a personal OAuth
  token for <https://300.ya.ru>, which cannot be provided on CRAN machines.
* Tests do not access the network: HTTP responses are mocked with
  `httr2::local_mocked_responses()`. The only live test is skipped on CRAN
  and when no token is set.
* The package automates requests that a user can make on 300.ya.ru
  themselves, with their own token. The service's terms of use
  <https://yandex.ru/legal/300ya_termsofuse/> do not prohibit programmatic
  access; they restrict use to personal, non-commercial purposes and forbid
  using retellings to train AI models. These restrictions are stated in the
  README, the vignette and `?retell_text`.
