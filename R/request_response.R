#' Remix of AzureML consume() function
#'
#' This is a re-imagining of the \code{AzureML::\link[AzureML]{consume}} function.
#' The API and documentation is adapted from this function.
#'
#' @inheritParams validate_endpoint
#' @param inputs       list (named), inputs to be sent as the body of the request.
#'   This differs from (and is stricter than) the API for
#'   \code{AzureML::\link[AzureML]{consume}}
#' @param globalParameters  list (named), global parameters
#' @param retryDelay   numeric, time in seconds to delay before retrying
#'    in case of a server error
#' @param .retry       numeric, number of tries before failing
#'
#' @return \code{\link{azureml_request_response}} object
#'
#' @seealso \code{\link{validate_inputs}}, \code{\link{validate_globalParameters}}
#'
consume_endpoint <- function(endpoint, inputs, globalParameters = NULL, retryDelay = 10,
                        .retry = 5){

  # validate endpoint
  endpoint <- validate_endpoint(endpoint)

  # get url and api_key
  url <- endpoint$ApiLocation
  api_key <- endpoint$PrimaryKey

  # set globalParameters
  if (is.null(globalParameters)){
    globalParameters <- setNames(list(), character(0))
  }

  # make api call (consider rewriting callAPI using httr)
  request_timestamp <- Sys.time()

  # call API here
  success <- TRUE
  body_req_resp <-
    call_api(api_key, url, inputs,  globalParameters, retryDelay, .retry = .retry)

  response_timestamp <- Sys.time()

  if (inherits(body_req_resp, "error")) {
    success <- FALSE
    response_body <- NULL
  }

  request_body <- body_req_resp$request
  response_body <- body_req_resp$response

  azureml_request_response(
    url = url,
    success = success,
    request_timestamp = request_timestamp,
    request_body = request_body,
    response_timestamp = response_timestamp,
    response_body = response_body
  )
}


#' Creates an azureml_request_response object
#'
#' Generally, you will create this object using the \code{\link{consume_endpoint}} function.
#'
#' @param url                character, URL of API endpoint
#' @param success            logical, indicates if call to API succeeded
#' @param request_timestamp  POSIXct, datetime when client sent request
#' @param request_body       character, request body (json format)
#' @param response_timestamp POSIXct, datetime when client received response
#'   or gave up
#' @param response_body      character, response body (json format)
#'
#' @return S3 object with class \code{azureml_request_response}, with members:
#' \describe{
#'   \item{url}{character, URL of API endpoint}
#'   \item{success}{logical, indicates if call to API succeeded}
#'   \item{request_timestamp}{POSIXct, datetime when client sent request
#'     or gave up}
#'   \item{request_body}{character, request body (json format)}
#'   \item{response_timestamp}{datetime when client received response}
#'   \item{response_body}{response body (json format)}
#' }
#'
#' @seealso \code{\link{azureml_request_response.print}}
#'
#' @export
#'
azureml_request_response <- function(url, success,
                                     request_timestamp, request_body,
                                     response_timestamp, response_body){

  structure(
    list(
      url = url,
      success = success,
      request_timestamp = request_timestamp,
      request_body = request_body,
      response_timestamp = response_timestamp,
      response_body = response_body
    ),
    class = "azureml_request_response"
  )
}


#' Prints an azureml_request_response object
#'
#' This method returns the object passed to it, so that it can
#' be used in a pipe.
#'
#' @param x \code{\link{azureml_request_response}} object
#'
#' @return \code{x}
#' @export
#'
print.azureml_request_response <- function(x, ...){

  format_timestamp <- lubridate::stamp("2013-01-01T06:00:00Z", quiet = TRUE)

  delta_time <-
    (as.numeric(x$response_timestamp) - as.numeric(x$request_timestamp)) %>%
    round(digits = 1) %>%
    lubridate::as.duration()

  format_size <- function(x){
    x %>%
    utils::object.size() %>%
    format()
  }

  paste(
    paste0("URL:\t\t\t", x$url),
    paste0("Request timestamp:\t\t", format_timestamp(x$request_timestamp)),
    paste0("Response timestamp:\t\t", format_timestamp(x$response_timestamp)),
    paste0("Request-response duration:\t", format(delta_time)),
    paste0("Request-body size:\t\t", format_size(x$request_body)),
    paste0("Response-body size:\t\t", format_size(x$response_body)),
    paste0("Success:\t\t\t", x$success),
    sep = "\n"
  ) %>%
  cat()

  invisible(azureml_request_response)
}

# Framework for making an Azure ML web service API call.
#
# Note - this has only a small (but necessary modification to the Microsoft function)
#
# Helper function that constructs and send the API call to a Microsoft Azure
# Machine Learning web service, then receives and returns the response in JSON format.
#
# @param apiKey primary API key
# @param requestUrl API URL
# @param inputs the data to be passed to the web service
# @param globalParam the global parameters for the web service
# @param retryDelay number of seconds to wait after failing (max 3 tries) to try again
# @param .retry the number of retry attempts
# @return result the response
#
# @importFrom jsonlite toJSON
# @importFrom curl handle_setheaders new_handle handle_setopt curl_fetch_memory
# @keywords internal
call_api <- function(apiKey, requestUrl, inputs, globalParameters,
                    retryDelay=10, .retry = 5) {
  # Set number of tries and HTTP status to 0
  result <- NULL
  # Construct request body
  request_body <-
    list(
      Inputs = inputs,
      GlobalParameters = globalParameters
    ) %>%
    jsonlite::toJSON(auto_unbox = TRUE, digits = 16)

  request_body_raw <-
    request_body %>%
    paste(collapse = "\n") %>%
    charToRaw()

  h <- curl::new_handle()
  headers <- list(`User-Agent` = "R",
                  `Content-Type` = "application/json",
                  `Authorization` = paste0("Bearer ", apiKey))
  curl::handle_setheaders(h, .list = headers)
  curl::handle_setopt(h,
                .list = list(
                  post = TRUE,
                  postfieldsize = length(request_body_raw),
                  postfields = request_body_raw
                )
  )
  # ALERT! using :::
  r <- AzureML:::try_fetch(requestUrl, h, no_retry_on = 400, delay = retryDelay, .retry = .retry)
  response_body <- rawToChar(r$content)

  success <- TRUE
  if(r$status_code >= 400)  {
    warning(paste(capture.output(fromJSON(response_body)), collapse="\n"))
    success <- FALSE
  }

  list(
    success = success,
    request_body = request_body,
    response_body = response_body
  )
}








