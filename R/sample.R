#' Get sample request/response payloads
#'
#' For endpoints with request/response, these functions return sample
#' json files.
#'
#' @inheritParams validate_endpoint
#'
#' @return character, json format
#' @export
#'
sample_response <- function(endpoint){

  endpoint$HelpLocation %>%
    paste("score", sep = "/") %>%
    scrape_help("#responseSummary pre")
}

#' @rdname sample_response
#' @export
#'
sample_request <- function(endpoint){

  endpoint$HelpLocation %>%
    paste("score", sep = "/") %>%
    scrape_help("#requestSummary pre")
}

scrape_help <- function(url, css){

  result <-
    xml2::read_html(url) %>%
    rvest::html_node(css) %>%
    rvest::html_text()

  result
}

