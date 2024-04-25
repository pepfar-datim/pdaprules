s3_filter_PAW <- function(bucketlist = NULL, category = "MER", subcategory = NULL,
                          metadata = FALSE, country = NULL) {
  
  
  stopifnot("Sorry, an s3 bucket must be provided." = !is.null(bucketlist))
  stopifnot("Sorry, the metadata argument must be logical." = is.logical(metadata))
  
  allowedcategory <- c("MER", "Financial", "Narratives")
  allowedsubcategory <- c("Site_Recent", "Site_Historic", "PSNU_Recent",
                          "PSNU_Historic", "OU_Recent", "OU_Historic")
  
  # Check that provided category are supported ####
  if (!category %in% allowedcategory) {
    stop(paste0("Sorry, for the category please use MER, Financial, or Narratives"))
  }
  
  # Check that provided subcategory are supported ####
  if (!subcategory %in% allowedsubcategory) {
    stop(paste0("Sorry, for the subcategory please use Site_Recent, Site_Historic,
                PSNU_Recent, PSNU_Historic, OU_Recent, or OU_Historic"))
  }
  
  #List bucket choices
  choices <- bucketlist
  
  #Sort bucket based upon desired category
  choices <- choices[grepl(category, choices$path_names), ]
  
  if(!is.null(subcategory)) {
    choices <- choices[grepl(subcategory, choices$path_names), ]
  }
  
  #IF metadata is set to true DONT filter out 
  if(metadata == FALSE) {
    choices <- choices[!grepl("metadata", choices$path_names), ]
  }
  
  #IF metadata is set to true DONT filter out 
  if(!is.null(country)) {
    choices <- choices[grepl(paste(country, collapse = "|"), choices$path_names), ]
  }
  
  #Reset row names
  rownames(choices) <- NULL
  
  #Turn into List
  choices <- as.list(choices$path_names)
  
  choices
}
