#' Parses a response-body into a dataframe
#'
#' @param response_body    character (JSON)
#' @param response_sample  character (JSON)
#' @param element          character or numeric, component of \code{response_body} result
#'   result to return
#'
#' @return dataframe
#' @export
#'
parse_response <- function(response_body, response_sample, element = 1){

  response_body <- jsonlite::fromJSON(response_body)
  response_sample <- jsonlite::fromJSON(response_sample)

  # validate element
  results_names <-
    response_sample %>%
    `[[`("Results") %>%
    names()

  # determine find-function according to type of element
  if (is.numeric(element)) {
    fn_find <- seq_along
  } else {
    fn_find <- identity
  }

  # stop if element is not a part of Results
  if (!(element %in% fn_find(results_names))){
    stop(
      paste(
        "element",
        paste0('"', element, '"'),
        "not found in response_sample"
      )
    )
  }

  # convert from JSON
  body_data <-
    response_body %>%
    `[[`("Results") %>%
    `[[`(element)

  # apply the parsing instructions from response_sample
  sample_names <-
    response_sample %>%
    `[[`("Results") %>%
    `[[`(element) %>%
    `[[`("value") %>%
    `[[`("ColumnNames")

  sample_types <-
    response_sample %>%
    `[[`("Results") %>%
    `[[`(element) %>%
    `[[`("value") %>%
    `[[`("ColumnTypes")

  # make a readr column-specification
  #
  # Ask Microsoft for all the available types
  #
  list_spec <- list(
    Object = readr::col_guess(),
    Numeric = readr::col_double(),
    Logical = readr::col_logical() # need to confirm
  )

  col_spec <- stats::setNames(list_spec[sample_types], sample_names)

  result <-
    body_data %>%
    readr::type_convert(col_types = do.call(readr::cols, col_spec)) %>%
    tibble::as_tibble()

  result
}
