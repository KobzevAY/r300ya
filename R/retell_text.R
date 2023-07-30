#' Title
#'
#' @param url URL you want to retell
#' @param token Your Token
#'
#' @return List
#' @export
#'
#' @examples
#' retell_text(url = 'your_url', token = 'your_token')
retell_text <- function(
    url = NULL,
    token = NULL) {


  # check URL
  if(is.null(url)) {
    message('You forgot to enter the URL!')
  } else {
    status <- tryCatch(
      httr2::request(url) |>
        httr2::req_perform(),
      error = function(e) e
    )

    if(inherits(status,  "error")) {
      message(paste("URL does not seem to exist:", url))
    } else {
      # check token
      if (is.null(token)) {
        message('You forgot to fill token!')
      } else {
        req <- httr2::request("https://300.ya.ru/api/sharing-url") |>
          httr2::req_auth_bearer_token(paste('OAuth', token, sep = " ")) |>
          httr2::req_error(is_error = function(resp) FALSE) |>
          httr2::req_perform()


        if(req$status_code == 403) {
          message("Check your token")
        } else {
          req <-  tryCatch(
            httr2::request("https://300.ya.ru/api/sharing-url") |>
              httr2::req_auth_bearer_token(paste('OAuth', token = token, sep = " ")) |>
              httr2::req_body_json(list("article_url" = url)) |>
              httr2::req_perform() |>
              httr2::resp_body_json(),
            error = function(e) e
          )

          if(inherits(req,  "error")) {
            message(paste("The neural network was unable to extract the text of the article. Perhaps the article is too long, and neural networks are not yet able to retell such articles. Try another.:", url))
          } else {

            res <- list()

            res['title'] <- rvest::read_html(req$sharing_url) |>
              rvest::html_nodes(xpath = '//*[@class="stretched svelte-umz3h1"]') |> #
              rvest::html_text2()

            res['short_text'] <- rvest::read_html(req$sharing_url) |>
              rvest::html_nodes(xpath = '//*[@class="content-theses svelte-h3ittf"]') |>
              rvest::html_text2() |>
              stringr::str_split('\n' ) # |> print()

            res['short_url'] <- req$sharing_url

            res['original_url'] <- url

            return(res)
          }
        }
      }
    }
  }
}
