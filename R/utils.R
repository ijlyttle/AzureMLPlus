#' Validate an endpoint
#'
#' Will throw an error or return the \code{endpoint}. If a service is sent, its default
#' endpoint is returned.
#'
#' The source code for this code
#' was taken from the source code AzureML package. Perhaps this function can be a part
#' of the AzureML package so that the source code can be "returned".
#'
#' @param endpoint    \code{AzureML::\link[AzureML]{endpoints}} member or
#'    \code{AzureML::\link[AzureML]{services}} member
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
  if(AzureML::is.Service(endpoint))
  {
    if(nrow(endpoint) > 1) endpoint = endpoint[1, ]
    default <- endpoint$DefaultEndpointName
    endpoint <- endpoints(attr(endpoint, "workspace"), endpoint)
    endpoint <- subset(endpoint, Name = default)
  }

  if(!AzureML::is.Endpoint(endpoint)) {
    stop("Invalid endpoint. Use publishWebservice() or endpoints() to create or obtain a service endpoint.")
  }

  endpoint
}
