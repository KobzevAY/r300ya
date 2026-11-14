fixture <- function(name) {
  path <- test_path("fixtures", name)
  readChar(path, file.size(path), useBytes = TRUE)
}

json_response <- function(status, body) {
  httr2::response(
    status_code = status,
    headers = list("Content-Type" = "application/json"),
    body = charToRaw(body)
  )
}

html_response <- function(html) {
  httr2::response(
    status_code = 200,
    headers = list("Content-Type" = "text/html; charset=utf-8"),
    body = charToRaw(html)
  )
}

# Mocks both requests: the API call and the download of the retelling page
mock_service <- function(api_status = 200,
                         api_body = '{"status":"success","sharing_url":"https://300.ya.ru/abc"}',
                         page = fixture("retelling.html"),
                         env = parent.frame()) {
  seen <- new.env()
  httr2::local_mocked_responses(function(req) {
    if (grepl("/api/sharing-url$", req$url)) {
      seen$api_req <- req
      json_response(api_status, api_body)
    } else {
      seen$page_url <- req$url
      html_response(page)
    }
  }, env = env)
  seen
}

# ---- input checks -------------------------------------------------------------

test_that("invalid url is an error", {
  expect_error(retell_text(token = "t"), class = "r300ya_error_input")
  expect_error(retell_text(NA_character_, token = "t"), class = "r300ya_error_input")
  expect_error(retell_text(c("https://a.ru", "https://b.ru"), token = "t"), class = "r300ya_error_input")
  expect_error(retell_text("habr.com", token = "t"), class = "r300ya_error_input")
})

test_that("missing token is an error, token is read from YA300_TOKEN", {
  withr::local_envvar(YA300_TOKEN = "")
  expect_error(retell_text("https://example.com"), class = "r300ya_error_token")

  withr::local_envvar(YA300_TOKEN = "env-token")
  seen <- mock_service()
  retell_text("https://example.com")
  expect_equal(httr2::req_get_headers(seen$api_req, "reveal")$Authorization, "OAuth env-token")
})

# ---- API responses ------------------------------------------------------------

test_that("request is sent with OAuth header and article_url body", {
  seen <- mock_service()
  retell_text("https://example.com/article", token = "secret")

  expect_equal(httr2::req_get_method(seen$api_req), "POST")
  expect_equal(httr2::req_get_headers(seen$api_req, "reveal")$Authorization, "OAuth secret")
  expect_equal(seen$api_req$body$data, list(article_url = "https://example.com/article"))
  expect_equal(seen$page_url, "https://300.ya.ru/abc")
})

test_that("403 from the API is a token error", {
  mock_service(api_status = 403, api_body = '{"status":"error","message":"not authorized"}')
  expect_error(retell_text("https://example.com", token = "bad"), class = "r300ya_error_token")
})

test_that("API failure is an api error that includes the service message", {
  mock_service(api_status = 400, api_body = '{"status":"error","message":"article too long"}')
  expect_error(
    retell_text("https://example.com", token = "t"),
    regexp = "article too long",
    class = "r300ya_error_api"
  )

  mock_service(api_status = 200, api_body = '{"status":"error"}')
  expect_error(retell_text("https://example.com", token = "t"), class = "r300ya_error_api")
})

# ---- result -------------------------------------------------------------------

test_that("retelling page is parsed into the result", {
  mock_service()
  res <- retell_text("https://example.com", token = "t")

  expect_s3_class(res, "r300ya_retelling")
  expect_equal(res$title, "Test article")
  expect_equal(res$short_text, c("First short thesis", "Second short thesis", "\u0422\u0440\u0435\u0442\u0438\u0439 \u0442\u0435\u0437\u0438\u0441"))
  expect_equal(res$theses$chapter, c("Chapter one", "Chapter one", "Chapter two"))
  expect_equal(res$theses$thesis, c("Detailed thesis 1.", "Detailed thesis 2.", "Detailed thesis 3."))
  expect_equal(res$short_url, "https://300.ya.ru/abc")
  expect_equal(res$original_url, "https://example.com")
  expect_output(print(res), "First short thesis")
})

test_that("og:title and detailed theses are used as fallbacks", {
  page <- fixture("retelling.html")
  page <- sub('<h1 class="title[^"]*">[^<]*</h1>', "", page)
  page <- sub('<meta name="description"[^>]*>', "", page)
  mock_service(page = page)

  res <- retell_text("https://example.com", token = "t")
  expect_equal(res$title, "Test article")
  expect_equal(res$short_text, res$theses$thesis)
})

test_that("unexpected page layout is a parse error", {
  mock_service(page = "<html><body><p>nothing here</p></body></html>")
  expect_error(retell_text("https://example.com", token = "t"), class = "r300ya_error_parse")
})

# ---- live service -------------------------------------------------------------

test_that("live request works", {
  skip_on_cran()
  skip_if_offline("300.ya.ru")
  skip_if(Sys.getenv("YA300_TOKEN") == "", "YA300_TOKEN is not set")

  res <- retell_text("https://habr.com/ru/news/729422/")
  expect_true(nzchar(res$title))
  expect_gt(length(res$short_text), 0)
})
