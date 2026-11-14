# Retell a web page with YandexGPT

Sends the URL of a web page to the [300.ya.ru](https://300.ya.ru)
service, which generates a short retelling (a list of theses) with
YandexGPT, and returns the result as an R object.

## Usage

``` r
retell_text(url, token = Sys.getenv("YA300_TOKEN"))
```

## Arguments

- url:

  A single string: the URL of the page you want to retell. The page can
  be in any language.

- token:

  A single string: your 300.ya.ru OAuth token. By default it is read
  from the `YA300_TOKEN` environment variable, so you can keep it in
  your `.Renviron` file instead of your scripts (see
  `usethis::edit_r_environ()`).

## Value

An object of class `r300ya_retelling`: a list with elements

- `title`: the title of the article;

- `short_text`: a character vector with the theses of the short
  retelling;

- `theses`: a data frame with columns `chapter` and `thesis`, the
  detailed retelling grouped by chapters (may have zero rows if the
  service did not produce one);

- `short_url`: the link to the retelling on 300.ya.ru;

- `original_url`: the URL you passed.

Errors are signalled with [`stop()`](https://rdrr.io/r/base/stop.html)
and have class `r300ya_error`, plus one of `r300ya_error_token` (missing
or rejected token), `r300ya_error_api` (the service could not retell the
page) or `r300ya_error_parse` (the retelling page has an unexpected
structure), so they can be handled with
[`tryCatch()`](https://rdrr.io/r/base/conditions.html).

## Terms of use

r300ya is an unofficial package, not affiliated with Yandex. By using it
you accept the [300.ya.ru terms of
use](https://yandex.ru/legal/300ya_termsofuse/): among other things, the
service and the retellings may be used for personal, non-commercial
purposes only and must not be used to train or test AI models.
Retellings of web pages are published on 300.ya.ru and are available to
anyone with the link.

## Examples

``` r
if (FALSE) { # \dontrun{
# Sys.setenv(YA300_TOKEN = "your_token") or put it in .Renviron
res <- retell_text("https://habr.com/ru/news/729422/")
res$short_text

# Several pages, skipping the ones that fail
urls <- c("https://habr.com/ru/news/729422/", "https://example.com")
results <- lapply(urls, function(u) tryCatch(retell_text(u), r300ya_error = function(e) NULL))
} # }
```
