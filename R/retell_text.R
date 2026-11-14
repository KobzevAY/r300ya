#' Retell a web page with YandexGPT
#'
#' Sends the URL of a web page to the [300.ya.ru](https://300.ya.ru) service,
#' which generates a short retelling (a list of theses) with YandexGPT, and
#' returns the result as an R object.
#'
#' @param url A single string: the URL of the page you want to retell. The
#'   page can be in any language.
#' @param token A single string: your 300.ya.ru OAuth token. By default it is
#'   read from the `YA300_TOKEN` environment variable, so you can keep it in
#'   your `.Renviron` file instead of your scripts (see
#'   `usethis::edit_r_environ()`).
#'
#' @return An object of class `r300ya_retelling`: a list with elements
#' * `title`: the title of the article;
#' * `short_text`: a character vector with the theses of the short retelling;
#' * `theses`: a data frame with columns `chapter` and `thesis`, the detailed
#'   retelling grouped by chapters (may have zero rows if the service did not
#'   produce one);
#' * `short_url`: the link to the retelling on 300.ya.ru;
#' * `original_url`: the URL you passed.
#'
#' Errors are signalled with [stop()] and have class `r300ya_error`, plus one of
#' `r300ya_error_token` (missing or rejected token), `r300ya_error_api` (the
#' service could not retell the page) or `r300ya_error_parse` (the retelling
#' page has an unexpected structure), so they can be handled with
#' [tryCatch()].
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Sys.setenv(YA300_TOKEN = "your_token") or put it in .Renviron
#' res <- retell_text("https://habr.com/ru/news/729422/")
#' res$short_text
#'
#' # Several pages, skipping the ones that fail
#' urls <- c("https://habr.com/ru/news/729422/", "https://example.com")
#' results <- lapply(urls, function(u) tryCatch(retell_text(u), r300ya_error = function(e) NULL))
#' }
retell_text <- function(url, token = Sys.getenv("YA300_TOKEN")) {
  check_url(url)
  check_token(token)

  sharing_url <- request_sharing_url(url, token)
  html <- fetch_page(sharing_url)
  parsed <- parse_retelling(html)

  structure(
    list(
      title = parsed$title,
      short_text = parsed$short_text,
      theses = parsed$theses,
      short_url = sharing_url,
      original_url = url
    ),
    class = c("r300ya_retelling", "list")
  )
}

#' @export
print.r300ya_retelling <- function(x, ...) {
  cat(x$title, "\n", sep = "")
  cat(paste0("* ", x$short_text), sep = "\n")
  cat("\nRetelling: ", x$short_url, "\nOriginal:  ", x$original_url, "\n", sep = "")
  invisible(x)
}

# ---- internals ---------------------------------------------------------------

api_url <- "https://300.ya.ru/api/sharing-url"

user_agent <- "r300ya (https://github.com/KobzevAY/r300ya)"

abort <- function(message, class) {
  stop(structure(
    class = c(paste0("r300ya_error_", class), "r300ya_error", "error", "condition"),
    list(message = message, call = NULL)
  ))
}

check_url <- function(url) {
  if (missing(url) || !is.character(url) || length(url) != 1 || is.na(url) ||
      !grepl("^https?://", url)) {
    abort("`url` must be a single string starting with http:// or https://.", "input")
  }
}

check_token <- function(token) {
  if (!is.character(token) || length(token) != 1 || is.na(token) || !nzchar(token)) {
    abort(
      paste(
        "No token. Pass `token` or set the YA300_TOKEN environment variable.",
        "See https://github.com/KobzevAY/r300ya#readme"
      ),
      "token"
    )
  }
}

request_sharing_url <- function(url, token) {
  resp <- httr2::request(api_url) |>
    httr2::req_headers(Authorization = paste("OAuth", token)) |>
    httr2::req_body_json(list(article_url = url)) |>
    httr2::req_user_agent(user_agent) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  status <- httr2::resp_status(resp)
  body <- tryCatch(httr2::resp_body_json(resp), error = function(e) list())

  if (status %in% c(401, 403)) {
    abort("The service rejected the token. Check that it is correct and not expired.", "token")
  }

  if (status >= 400 || is.null(body$sharing_url)) {
    details <- if (!is.null(body$message)) body$message else paste("HTTP", status)
    abort(
      paste0(
        "300.ya.ru could not retell ", url, " (", details, "). ",
        "The page may be too long, too short, not an article (e.g. a PDF) ",
        "or unreachable for the service."
      ),
      "api"
    )
  }

  body$sharing_url
}

fetch_page <- function(url) {
  httr2::request(url) |>
    httr2::req_user_agent(user_agent) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform() |>
    httr2::resp_body_string()
}

# Parses a 300.ya.ru retelling page. Class names on the page carry generated
# suffixes (e.g. "title svelte-1jjcgx5") that change with every site release,
# so we match on the stable part of the class only and fall back to <meta> tags.
parse_retelling <- function(html) {
  doc <- rvest::read_html(html)

  title <- rvest::html_element(doc, xpath = paste0("//h1", has_class("title"))) |>
    rvest::html_text2()
  if (is.na(title) || !nzchar(title)) {
    title <- rvest::html_element(doc, xpath = "//meta[@property='og:title']") |>
      rvest::html_attr("content")
    title <- sub("\\s+-\\s+[^-]*YandexGPT\\s*$", "", title)
  }

  theses <- parse_chapters(doc)

  description <- rvest::html_element(doc, xpath = "//meta[@name='description']") |>
    rvest::html_attr("content")
  short_text <- if (is.na(description)) character() else split_theses(description)
  if (length(short_text) == 0) short_text <- theses$thesis

  if (length(short_text) == 0) {
    abort(
      "Could not find the retelling on the 300.ya.ru page. The page layout may have changed.",
      "parse"
    )
  }

  list(title = trimws(title), short_text = short_text, theses = theses)
}

parse_chapters <- function(doc) {
  chapters <- rvest::html_elements(doc, xpath = paste0("//li", has_class("chapter")))
  rows <- lapply(chapters, function(ch) {
    heading <- rvest::html_element(ch, xpath = paste0(".//h2", has_class("chapter-subheading"))) |>
      rvest::html_text2()
    theses <- rvest::html_elements(ch, xpath = paste0(".//*", has_class("thesis-text"))) |>
      rvest::html_text2() |>
      clean_thesis()
    data.frame(
      chapter = rep(trimws(heading), length(theses)),
      thesis = theses,
      stringsAsFactors = FALSE
    )
  })
  if (length(rows) == 0) {
    return(data.frame(chapter = character(), thesis = character(), stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}

# XPath predicate matching one whole class name inside the class attribute
has_class <- function(class) {
  sprintf("[contains(concat(' ', normalize-space(@class), ' '), ' %s ')]", class)
}

split_theses <- function(text) {
  x <- clean_thesis(strsplit(text, "\n", fixed = TRUE)[[1]])
  x[nzchar(x)]
}

clean_thesis <- function(x) {
  # "[\\h\\v]" also strips non-breaking spaces, which the page uses
  trimws(sub("^[\\h\\v]*\u2022", "", x, perl = TRUE), whitespace = "[\\h\\v]")
}
