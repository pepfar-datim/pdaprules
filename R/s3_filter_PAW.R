s3_filter_PAW <- function(category = "MER", subcategory = NULL, metadata = FALSE, country = NULL) {
  
  #category options ^MER|^Financial|^Narratives
  #subcategory Site_Recent, Site_Historic, PSNU_Recent, PSNU_Historic, OU_Recent, OU_Historic
  #metadata include true or false
  #Country specific country to sort for 
  
  #List bucket choices
  choices <- s3_list_bucket_items(bucket = Sys.getenv("TEST_BUCKET"))
  
  #Sort bucket based upon desired category
  choices <- choices[grepl(category, choices$path_names), ]
  #choices[grepl("^MER), ]
  
  if(!is.null(subcategory)) {
    choices <- choices[grepl(subcategory, choices$path_names), ]
  }
  
  #IF metadata is set to true DONT filter out 
  if(metadata == FALSE) {
    choices <- choices[!grepl("metadata", choices$path_names), ]
  }
  
  #IF metadata is set to true DONT filter out 
  if(!is.null(country)) {
    choices <- choices[grepl(country, choices$path_names), ]
  }
  
  # reset row names
  rownames(choices) <- NULL
  
  choices
  
}