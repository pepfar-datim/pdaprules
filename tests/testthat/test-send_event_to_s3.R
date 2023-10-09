
test_that("arguments are valid", {
  
  user_input <- list(
    uuid = "1234",
    d2_session = list(
      username = "my_username"
    )
  )
  
  # log bucket is empty
  testthat::expect_error(
    send_event_to_s3(app_name="my_app", event_type = "S3_READ", user_input = user_input, log_bucket = NULL),
    "you have not provided a log bucket for this data!"
  )
  
  # user input is empty
  testthat::expect_error(
    send_event_to_s3(app_name="my_app", event_type = "S3_READ", user_input = NULL, log_bucket = "my log bucket"),
    "you have not provided sufficient logging information; make sure to provide both a type of event and user input!"
  )
  
  # event type is empty
  testthat::expect_error(
    send_event_to_s3(app_name="my_app", event_type = NULL, user_input = user_input, log_bucket = "my log bucket"),
    "you have not provided sufficient logging information; make sure to provide both a type of event and user input!"
  )
  
  # app name is empty
  testthat::expect_error(
    send_event_to_s3(app_name=NULL, event_type = "S3_READ", user_input = user_input, log_bucket = "my log bucket"),
    "you have not provided an app name!"
  )
  
  # check event type
  testthat::expect_error(
    send_event_to_s3(app_name="my app", event_type = "READ", user_input = user_input, log_bucket = "my log bucket"),
    "the event type you are trying to record is not a valid choice!"
  )
  

  })