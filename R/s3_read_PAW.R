# library(dplyr)

s3_read_PAW<- function(path) {
  
  fileList <- path 
  
  allMyData <- lapply(
    fileList,
    function(f) {
      aws.s3::s3read_using(
        FUN = readr::read_delim, "|", escape_double = FALSE,
        trim_ws = TRUE, col_types = readr::cols(.default = readr::col_character()
        ),
        object = f,
        bucket = Sys.getenv("TEST_BUCKET"),
      ) 
    }
  ) %>% 
    bind_rows()
}




# ######################## ALT OPTION ########################################################
# #Works
# s3_object <- s3$get_object(Bucket = Sys.getenv("TEST_BUCKET"), Key = test)
# 
# #Works
# s3_object_body <- s3_object$Body
# 
# #Works
# file_name2 <- paste0(getwd(), "/", test)
# 
# #Doesn't work because no file exists already. 
# con <- file(file_name2, "wb")
# writeBin(s3_object_body, con = con)
# close(con) 
# 
# #Works
# file_name2 <- s3_object_body %>%
#   rawToChar %>%
#   read.csv(text = ., sep = "|")
# 
# 
# 
# 
# s3_read_PAW<- function(path, locally = TRUE) {
#   #########################################################################
#   #Make original connection
#   s3 <- paws::s3()
#   
#   s3_object <-
#     tryCatch({
#       s3$get_object(Bucket = Sys.getenv("TEST_BUCKET"),
#                     Key = path)
#     },
#     error = function(e) {
#       interactive_warning("Could not retreive support file data from S3")
#       return(NULL)
#     })
#   
#   s3_object_body <- s3_object$Body
#   #########################################################################
#   if (locally==T) {
#     file_name2 <- paste0(getwd(), "/", path)
#     
#     if (file.exists(file_name2)) {
#       unlink(file_name2)
#     }
#     
#     tryCatch({con <- file(file_name2, "wb")
#     writeBin(s3_object_body, con = con)
#     close(con)},
#     error = function(e) {
#       interactive_warning("Could not save support file locally")
#       return(NULL)
#       
#     })
#     
#     futile.logger::flog.info(paste0("Retreived support file to ", file_name2))
#     if (!file.exists(file_name2)) {
#       stop("Could not retreive support file.")
#     }
#     #########################################################################    
#   } else{
#     file_name2 <- s3_object_body %>%
#       rawToChar %>%
#       read.csv(text = ., sep = "|")
#     
#     futile.logger::flog.info("Retreived support file in memory")
#   }
#   
#   return(file_name2)
# }
