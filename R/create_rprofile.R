#' @export
#' @title create_rprofile()
#'
#' @description creates an example rprofile, it is available
#' on your clipboard for you to paste.
#'
#'

create_rprofile <- function() {

  text <- '
  source("renv/activate.R")

  # user credentials ----
  Sys.setenv(BASE_URL = "****")
  Sys.setenv(SECRET_NAME = "****")

  # buckets ----
  Sys.setenv(READ_S3 = "*****")
  Sys.setenv(WRITE_S3 = "***")
  Sys.setenv(LOG_BUCKET = "***")
  Sys.setenv(REGION = "***")

  # oauth ----
  Sys.setenv(APP_URL = "****")
  Sys.setenv(OAUTH_APPNAME = "****")
  Sys.setenv(OAUTH_KEYNAME = "****")
  Sys.setenv(OAUTH_SECRET = "****")


  '
  cat(text)
  clipr::write_clip(text)

}
