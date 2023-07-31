#' @export
#' @title s3_list_bucket_items(bucket)
#'
#' @param bucket your aws bucket
#' @param prefix a specific directory inside your bucket
#' @param filter_results default is TRUE and filters for txt, xlsx ,csv and xlsx. If FALSE returns everything.
#' @description Standard function for connecting to S3, returns a dataframe of files.
#'
#'

s3_list_bucket_items <- function(bucket, prefix = NULL, filter_results = TRUE) {
  
  # params and structuring ----
  my_bucket <- bucket
  my_prefix <- prefix
  
  # Lists all of bucket contents, fill in your bucket
  choices <- aws.s3::get_bucket(bucket = my_bucket, prefix = my_prefix)
  
  # get just path names
  choices <- lapply(choices, "[[", 1)
  
  # get just file names
  cleaned_choices <- lapply(choices, function(x) gsub(".*\\/", "", x))
  
  # make dataframe of file names and path names
  choices <- do.call(rbind, Map(data.frame, file_names = cleaned_choices,
                                path_names = choices, stringsAsFactors = FALSE))
  
  if(filter_results) {
    
    # filter just files that end in txt or xlsx or csv
    choices <- choices[grepl("txt$|xlsx$|csv$|xlsx$", choices$file_names), ]
    #print(choices)
    
  }
  
  # reset row names
  rownames(choices) <- NULL
  
  choices
  
}
