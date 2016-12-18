#' Validate an endpoint
#'
#' Will throw an error or return the \code{endpoint}. If a service is sent, its default
#' endpoint is returned.
#'
#' The source code for this code
#' was taken from the source code AzureML package. Perhaps this function can be a part
#' of the AzureML package so that the source code can be "returned".
#'
#' @param endpoint Either an AzureML web service endpoint returned by
#'   \code{AzureML::\link[AzureML]{publishWebService}},
#'   \code{AzureML::\link[AzureML]{endpoints}},
#'   or simply an AzureML web service from
#'   \code{AzureML::\link[AzureML]{services}};
#'
#' @return \code{endpoint}
#' @export
#'
validate_endpoint <- function(endpoint){

  # taken from https://github.com/RevolutionAnalytics/AzureML/blob/master/R/consume.R
  #
  # I would rather ask the AzureML maintainers if they would consider a
  # function like this one, then I could use theirs.
  #
  if (AzureML::is.Service(endpoint)) {
    if(nrow(endpoint) > 1) endpoint = endpoint[1, ]
    default <- endpoint$DefaultEndpointName
    endpoint <- AzureML::endpoints(attr(endpoint, "workspace"), endpoint)
    endpoint <- subset(endpoint, Name = default)
  }

  if (!AzureML::is.Endpoint(endpoint)) {
    stop("Invalid endpoint. Use publishWebservice() or endpoints() to create or obtain a service endpoint.")
  }

  endpoint
}

#' Obscure an authorization token
#'
#' This function takes a JSON string and replaces the `"authorization_token"`
#' field with some replacement text. It is useful for demonstrating credentials
#' without compromising security.
#'
#'
#' @param x           character (JSON), AzureML credentials
#' @param replacement character, replacement for key
#'
#' @return character (JSON), \code{x} with text replaced
#' @seealso \code{\link{obscure_endpoint_keys}}
#'
#' @export
#'
obscure_authorization_token <- function(x, replacement = "<token_goes_here>"){


  # ("authorization_token"\\s*:\\s*")   key for authorization token,
  #                                     non-strict on whitespace
  # ([^"]*)                             anything that is not a quote
  # (")                                 quote
  #
  pattern <- '("authorization_token"\\s*:\\s*")([^"]*)(")'

  sub(pattern, x, replacement = paste0("\\1", replacement, "\\3"))
}

#' Obscure the keys in an endpoint
#'
#' This can be useful step to take before displaying information
#' about an endpoint.
#'
#' @inheritParams validate_endpoint
#' @param replacement character, replacement for key
#'
#' @return \code{endpoint}, with \code{PrimaryKey} and \code{SecondaryKey}
#'   obscured
#'
#' @seealso \code{\link{obscure_authorization_token}}
#'
#' @export
#'
obscure_endpoint_keys <- function(endpoint, replacement = "<key_goes_here>"){

  endpoint <- validate_endpoint(endpoint)

  endpoint[["PrimaryKey"]] <- "<key_goes_here>"
  endpoint[["SecondaryKey"]] <- "<key_goes_here>"

  endpoint
}

