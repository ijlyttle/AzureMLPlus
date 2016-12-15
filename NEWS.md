# AzureMLPlus 0.0.0.9000

## To do

- reworks `consume_endpoint()` to use

- adds `encode_global_paramaters()`

- adds `validate_inputs()` and `validate_globalParamaters()`

- adds `parse_response()` function to get a dataframe from a response body, using a response sample as a template

- adds `request_body()`, `response_body()` accessors for `azureml_request_response`

## Done

- adds print method for `azureml_request_response` object

- adds `consume_endpoint()`, a re-working of the `AzureML::consume()` function

    - stricter input and more-verbose output than `AzureML::consume()`
    
    - returns an `azureml_request_response` object

- adds `json_viewer()` (adapted from **listviewer**), an htmlwidget to explore JSON text

- adds `get_request_sample()`, `get_response_sample()` to get JSON specifications for request/response

- adds `validate_endpoint()`

- imports **magrittr** pipe

- adds a `NEWS.md` file to track changes to the package



