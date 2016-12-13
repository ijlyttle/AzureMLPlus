#' validate that this is a valid endpoint
#'
#' @param endpoint    \code{AzureML::\link[AzureML]{endpoints}} (single member)
#'
#' @return \code{endpoint}
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
