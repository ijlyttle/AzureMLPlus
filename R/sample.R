#' Get sample request/response
#'
#' For endpoints that use request/response, use this function to get
#' json-formatted text.
#'
#' @inheritParams validate_endpoint
#'
#' @return character, json format
#' @seealso json_viewer
#' @export
#'
get_response_sample <- function(endpoint){

  endpoint %>%
    validate_endpoint() %>%
    `[[`("HelpLocation") %>%
    paste("score", sep = "/") %>%
    scrape_text("#responseSummary pre")
}

#' @rdname get_response_sample
#' @export
#'
get_request_sample <- function(endpoint){

  endpoint %>%
    validate_endpoint() %>%
    `[[`("HelpLocation") %>%
    paste("score", sep = "/") %>%
    scrape_text("#requestSummary pre")
}

#' Scrapes text from node
#'
#' @keywords internal
#'
#' @param url    character, URL to scrape
#' @param css    character, css selector to use to identify node
#'
#' @return character
#'
#' @export
#'
scrape_text <- function(url, css){

  result <-
    xml2::read_html(url) %>%
    rvest::html_node(css) %>%
    rvest::html_text()

  result
}

