###############
### request ###
###############

#' @keywords internal
#' @export
#'
request_body <- function(x, ...){
  UseMethod("request_body")
}

#' @keywords internal
#' @export
#'
request_body.default <- function(x, ...){
  "Unknown class"
}

#' Gets request/response body
#'
#' @param x \code{\link{azureml_request_response}} object
#'
#' @return character (JSON), body of request/response
#'
#' @export
#'
request_body.azureml_request_response <- function(x, ...){
  x$request_body
}

################
### response ###
################

#' @keywords internal
#' @export
#'
response_body <- function(x, ...){
  UseMethod("response_body")
}

#' @keywords internal
#' @export
#'
response_body.default <- function(x, ...){
  "Unknown class"
}


#' @rdname request_body.azureml_request_response
#' @export
#'
response_body.azureml_request_response <- function(x, ...){
  x$response_body
}
