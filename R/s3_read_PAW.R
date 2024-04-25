# library(dplyr)

s3_read_PAW<- function(bucket, path) {
  
  fileList <- path 
  
  allMyData <- lapply(
    fileList,
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
