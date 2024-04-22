# #read the xlsx from the s3 bucket
ndf <- aws.s3::s3read_using(FUN = read_excel,
                            bucket = paste(server,".pepfar.data.extracts-971c9206e248",sep=""),
                            object = narFiles$path_names[grepl("Narratives.xlsx", narFiles$path_names)])
# #narFiles$path_names[3]) # Wed Nov 30 14:25:30 2022 In case above doesn't work this does

#WORKS but need to delim by |
ndf <- aws.s3::s3read_using(FUN = read.csv,
                            bucket = Sys.getenv("TEST_BUCKET"),
                            object = test)
#Works
s3_object <- s3$get_object(Bucket = Sys.getenv("TEST_BUCKET"), Key = test)

#Works
s3_object_body <- s3_object$Body

#Works
file_name2 <- paste0(getwd(), "/", test)

#Doesn't work because no file exists already. 
con <- file(file_name2, "wb")
writeBin(s3_object_body, con = con)
close(con) 

#Works
file_name2 <- s3_object_body %>%
  rawToChar %>%
  read.csv(text = ., sep = "|")


s3_read_PAW<- function(path, locally = TRUE) {
  #########################################################################
  #Make original connection
  s3 <- paws::s3()
  
  s3_object <-
    tryCatch({
      s3$get_object(Bucket = Sys.getenv("TEST_BUCKET"),
                    Key = path)
    },
    error = function(e) {
      interactive_warning("Could not retreive support file data from S3")
      return(NULL)
    })
  
  s3_object_body <- s3_object$Body
  #########################################################################
  if (locally==T) {
    file_name2 <- paste0(getwd(), "/", path)
    
    if (file.exists(file_name2)) {
      unlink(file_name2)
    }
    
    tryCatch({con <- file(file_name2, "wb")
    writeBin(s3_object_body, con = con)
    close(con)    },
    error = function(e) {
      interactive_warning("Could not save support file locally")
      return(NULL)
      
    })
    
    futile.logger::flog.info(paste0("Retreived support file to ", file_name2))
    if (!file.exists(file_name2)) {
      stop("Could not retreive support file.")
    }
    #########################################################################    
  } else{
    file_name2 <- s3_object_body %>%
      rawToChar %>%
      read.csv(text = ., sep = "|")
    
    futile.logger::flog.info("Retreived support file in memory")
  }
  
  return(file_name2)
}