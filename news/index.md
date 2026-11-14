# Changelog

## r300ya 0.10.0

- Fixed parsing of the retelling page: 300.ya.ru changed its markup, and
  the old selectors no longer matched anything. Selectors now ignore the
  generated `svelte-*` suffixes and fall back to `<meta>` tags.
- Fixed the authorization header: it is now
  `Authorization: OAuth <token>` instead of `Bearer OAuth <token>`.
- `token` defaults to the `YA300_TOKEN` environment variable.
- Errors are now raised with
  [`stop()`](https://rdrr.io/r/base/stop.html) instead of being printed
  with [`message()`](https://rdrr.io/r/base/message.html), and have
  classes (`r300ya_error_token`, `r300ya_error_api`,
  `r300ya_error_parse`, `r300ya_error_input`) so they can be handled
  with [`tryCatch()`](https://rdrr.io/r/base/conditions.html). Error
  messages include the reason reported by the service.
- The source page is no longer downloaded just to check that it exists,
  and the token is no longer checked with a separate request: one API
  call does both.
- The result gains a `theses` data frame with the detailed retelling
  grouped by chapters, and a
  [`print()`](https://rdrr.io/r/base/print.html) method.
- Requests use a package user agent and retry on transient errors.
- Dropped the `stringr` dependency; requires R \>= 4.1.0.
- Tests no longer need network access or a token.
- New vignette
  [`vignette("r300ya")`](https://kobzevay.github.io/r300ya/articles/r300ya.md);
  the README is now in English, with a Russian version in
  `README.ru.md`.
- Package website: <https://kobzevay.github.io/r300ya/>.
- Documented the main restrictions of the 300.ya.ru terms of use.
