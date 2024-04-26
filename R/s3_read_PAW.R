#' @export
#' @title s3_read_PAW(bucket, path)
#'
#' @param bucket your aws bucket
#' @param path your list of paths to read 
#' @description Standard function for reading from S3, returns a dataframe based on the parameters provided.
#'

s3_read_PAW<- function(bucket, path) {
  
  stopifnot("Sorry, an s3 bucket must be provided." = !is.null(bucket))
  stopifnot("Sorry, at least one path must be provided." = !is.null(path))
  
  allMyData <- lapply(
    path,
    function(f) {
      aws.s3::s3read_using(
        FUN = readr::read_delim, "|", escape_double = FALSE,
        trim_ws = TRUE, col_types = readr::cols(.default = readr::col_character()
        ),
        object = f,
        bucket = bucket,
      ) 
    }
  ) %>% 
    bind_rows()
}
