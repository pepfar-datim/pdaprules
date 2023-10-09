#' @export
#' @title send_event_to_s3(d, event_type, user_input)
#'
#' @param d a list object that represents app information, time stamps etc
#' @param event_type a type of event such as LOGIN, LOGOUT OR READ_S3
#' @param user_input information about a user for analysis
#' @description custom function for sending event information to S3.
#'
#'
send_event_to_s3 <- function(d=NULL, event_type=NULL, user_input=NULL) {
  
  if (is.null(d)) {
    stop("you have not provided logging information")
  }
  
  if(is.null(event_type)) {
    stop("you have not provided event type information")
  }
  
  if(is.null(user_input)) {
    
  }
  
  s3 <- paws::s3()
  tm <- as.POSIXlt(Sys.time(), "UTC")
  ts_file <- strftime(tm, "%Y_%m_%d_%H_%M_%s")
  
  object_name <-
    paste0("R/",
           Sys.getenv("SECRET_ID"),"/",
           d$year,
           "/",
           ts_file, ".csv")
  
  event_info <- list(
    event_type = event_type,
    app = d$app,
    year = d$year,
    uuid = user_input$uuid,
    user = digest(user_input$d2_session$username, "md5", serialize = FALSE),
    ts = strftime(tm, "%Y-%m-%dT%H:%M:%S%z")
  )
  
  tmp <- tempfile()
  write.table(
    as.data.frame(event_info),
    file = tmp,
    quote = FALSE,
    sep = "|",
    row.names = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )
  
  # Load the file as a raw binary
  read_file <- file(tmp, "rb")
  raw_file <- readBin(read_file, "raw", n = file.size(tmp))
  close(read_file)
  print(raw_file)
  
  r <- tryCatch({
    foo <- s3$put_object(Bucket = Sys.getenv("LOG_BUCKET"),
                         Body = raw_file,
                         Key = object_name,
                         ContentType = "text/csv")
    
  },
  error = function(err) {
    print(err)
    futile.logger::flog.error("Event could not be saved to S3", name = "datapack")
    FALSE
  })
  
  unlink(tmp)
  
  return(r)
}
