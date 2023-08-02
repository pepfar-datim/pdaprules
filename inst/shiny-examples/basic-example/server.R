# import libraries -----
library(shiny)
library(futile.logger)
library(shinyWidgets)

#source functions ----
#source("s3_connect.R")
#source("s3_read.R")
#source("s3_write.R")

# server ----
server <- function(input, output, session) {

  # user information
  user_input  <-  reactiveValues(authenticated = FALSE,
                                 status = "",
                                 d2_session = NULL,
                                 memo_authorized = FALSE)

  # is the user authenticated?
  output$ui <- renderUI({
    if(user_input$authenticated == FALSE) {
      uiOutput("uiLogin")
    } else {
      uiOutput("authenticated")
    }
  })

  # login page with username and password
  output$uiLogin  <-  renderUI({

    fluidPage(
      wellPanel(
        fluidRow(
          h4("APP FOR TESTING CONNECT/READ/WRITE TO S3"),
          br()
        ),
        fluidRow(
          textInput("user_name", "Username: ", width = "500px"),
          passwordInput("password", "Password:", width = "500px"),
          actionButton("login_button", "Log in!")
        )
      )
    )

  })

  # once you login this page shows up
  output$authenticated <- renderUI({
    fluidPage(
      fluidRow(
        h4("Click the following buttons to test S3 compatibility:")
      ),
      fluidRow(
        column(
          br(),
          br(),
          actionButton("read_s3", "Read from S3"),
          br(),
          br(),
          #actionButton("write_s3_test", "Write to S3 TEST"),
          br(),
          br(),
          #actionButton("write_s3_prod", "Write to S3 PROD"),
          #br(),
          br(),
          width = 6
        ),
        column(
          actionButton("logout_button", "Log out of Session", style="color: #fff; background-color: #FF0000; border-color: #2e6da4"),
          width = 6
        )
      ),
      br(),
      fluidRow(
        column(12,
               wellPanel(
                 verbatimTextOutput("message")
                 ,style = "overflow-y:scroll; max-height: 400px")
        )
      ),
      br(),
      fluidRow(
        column(12,
               dataTableOutput('table')
        )
      )

    )
  })


  # Login process ----
  observeEvent(input$login_button, {
    tryCatch({
      datimutils::loginToDATIM(base_url = Sys.getenv("BASE_URL"),
                               username = input$user_name,
                               password = input$password,
                               d2_session_envir = parent.env(environment())
      )

      # connect to S3
      tryCatch({
        pdaprules::s3_connect()
      },
      error = function(e) {
        print(e)
      })
    },
    # This function throws an error if the login is not successful
    error = function(e) {
      flog.info(paste0("User ", input$username, " login failed."), name = "datapack")
    }
    )

    if (exists("d2_default_session")) {
      if (any(class(d2_default_session) == "d2Session")) {
        user_input$authenticated  <-  TRUE
        user_input$d2_session  <-  d2_default_session$clone()
        d2_default_session <- NULL


        # Need to check the user is a member of the PRIME Data Systems Group, COP Memo group, or a super user
        user_input$memo_authorized  <-
          grepl("VDEqY8YeCEk|ezh8nmc4JbX", user_input$d2_session$me$userGroups) |
          grepl(
            "jtzbVV4ZmdP",
            user_input$d2_session$me$userCredentials$userRoles
          )
        flog.info(
          paste0(
            "User ",
            user_input$d2_session$me$userCredentials$username,
            " logged in."
          ),
          name = "datapack"
        )
      }
    } else {
      sendSweetAlert(
        session,
        title = "Login failed",
        text = "Please check your username/password!",
        type = "error"
      )
    }
  })


  # read data ----
  observeEvent(input$read_s3, {

    # read data (this is an example file)
    tryCatch({
      my_data <- "MER_Structured_Datasets/Current_Frozen/PSNU_Recent/txt/MER_Structured_Datasets_PSNU_IM_Recent_Ukraine.txt"
      my_df <- aws.s3::s3read_using(FUN = readr::read_delim, "|", escape_double = FALSE,
                                    trim_ws = TRUE, col_types = readr::cols(.default = readr::col_character()
                                    ),
                                    bucket = Sys.getenv("READ_S3"),
                                    object = my_data)
    },
    error = function(e) {
      print(e)
    })

    # show data
    output$table <- renderDataTable(my_df,
                                    options = list(
                                      pageLength = 10
                                    )
    )

  })

  # write data test ----
  observeEvent(input$write_s3_test, {

    response <- s3_write(Sys.getenv("TEST_BUCKET_WRITE"))

    # render message
    output$message <- renderPrint({ response })

  })

  # write data prod ----
  observeEvent(input$write_s3_prod, {


    response <- s3_write(Sys.getenv("PROD_SERVER"))

    # render message
    output$message <- renderPrint({ response })

  })


  # logout process ----
  observeEvent(input$logout_button, {
    flog.info(paste0("User ", user_input$d2_session$me$userCredentials$username, " logged out."))
    user_input$authenticated  <-  FALSE
    user_input$user_name <- ""
    user_input$authorized  <-  FALSE
    user_input$d2_session  <-  NULL
    d2_default_session <- NULL
    gc()
    session$reload()
  })
}
