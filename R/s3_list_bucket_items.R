#' @export
#' @title s3_list_bucket_items(bucket)
#'
#' @param bucket your aws bucket
#' @param prefix a specific directory inside your bucket
#' @param filter_parquet default is FALSE and filters for txt, xlsx ,csv and xlsx. If TRUE returns parquet
#' @description Standard function for connecting to S3, returns a dataframe of files.
#'
#'

s3_list_bucket_items_p <- function(bucket, prefix = NULL, filter_parquet = FALSE) {
  
  # params and structuring ----
  my_bucket <- bucket
  my_prefix <- prefix
  
  # Lists all of bucket contents
  choices <-
    tryCatch({
      aws.s3::get_bucket(bucket = my_bucket, prefix = my_prefix)
    },
    error = function(e) {
      print("Could not retreive data from S3, have you ran s3_connect?")
    })
  
  # get just path names
  choices <- lapply(choices, "[[", 1)
  
  # get just file names
  cleaned_choices <- lapply(choices, function(x) gsub(".*\\/", "", x))
  
  # make dataframe of file names and path names
  choices <- do.call(rbind, Map(data.frame, file_names = cleaned_choices,
                                path_names = choices, stringsAsFactors = FALSE))
  
  if(!filter_parquet) {
    
    # filter just files that end in txt or xlsx or csv
    choices <- choices[grepl("txt$|xlsx$|csv$|xlsx$", choices$file_names), ]
    #print(choices)
    
  } else {
    choices <- choices[grepl("parquet", choices$file_names), ]
  }
  
  # reset row names
  rownames(choices) <- NULL
  
  choices
  
}
