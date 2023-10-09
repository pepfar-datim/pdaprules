#' @export
#' @title send_event_to_s3(app_name, event_type, user_input, log_bucket)
#'
#' @param app_name the name of the application you are logging
#' @param event_type a type of event, choose from one of the following LOGIN, LOGOUT, S3_READ or S3_WRITE
#' @param user_input information about a user for analysis
#' @param log_bucket the s3 bucket for logging event data
#' @description custom function for sending event information to S3.
#'
#'
send_event_to_s3 <- function(app_name=app_name, event_type=event_type, user_input=user_input, log_bucket=log_bucket) {
  
  # is log bucket available?
  if(is.null(log_bucket)) {
    stop("you have not provided a log bucket for this data!")
  }
  
  # does app_name exist?
  if(is.null(app_name)) {
    stop("you have not provided an app name!")
  }
  
  # if user input is missing
  if (is.null(user_input) || is.null(event_type)) {
    stop("you have not provided sufficient logging information; make sure to provide both a type of event and user input!")
  }
  
  # check event type
  if(!event_type %in% c("LOGIN", "S3_READ", "S3_WRITE", "LOGOUT")) {
    stop("the event type you are trying to record is not a valid choice!")
  }
  
  # establish params
  app_name <- app_name
  app_year <- substr(Sys.Date(),1,4)
  uuid  = user_input$uuid
  source_user = user_input$d2_session$username
  s3 <- paws::s3()
  tm <- as.POSIXlt(Sys.time(), "UTC")
  ts_file <- strftime(tm, "%Y_%m_%d_%H_%M_%s")
  
  # name of object in s3
  object_name <-
    paste0("R/",
           app_name,"/",
           app_year,
           "/",
           ts_file, ".csv")
  
  # add payload info
  event_info <- list(
    event_type = event_type,
    app = app_name,
    year = app_year,
    uuid = uuid,
    user = digest(source_user, "md5", serialize = FALSE),
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
  
  r <- tryCatch({
    foo <- s3$put_object(Bucket = log_bucket,
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
