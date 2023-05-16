# import libraries -----
library(shiny)
library(futile.logger)
library(shinyWidgets)

source("./read_data.R")

# which users should have access to data? IN THIS CASE ONLY PARTNERS - USG FOLKS SEE EVERYTHING, PARTNERS LIMITED TO MECH ACCESS
USG_USERS = c("Agency", "Interagency", "Global Agency", "Global")
PARTNER_USERS = c("Global Partner", "Partner")

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
          h4("The following up was created as an example of how developers can access the datim security information of their shiny app users utilizing the package datimutils. Please login with your DATIM credentials:"),
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
        h4("Click the following button to see information about this user, session and data access:")
      ),
      fluidRow(
        column(
          br(),
          br(),
          actionButton("groupid_button", "Streams"),
          br(),
          br(),
          actionButton("me_button", "User Type"),
          br(),
          br(),
          actionButton("mech_cocuid_button", "Mechanisms by Category Option Combos Id"),
          br(),     
          br(),
          actionButton("mech_id_button", "Mechanisms by Mech Code"),
          br(),
          br(),
          actionButton("mech_name_button", "Mechanisms by Name"),
          br(),
          br(),
          actionButton("test_data", "Test Data"),
          br(),
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
  
  # User and mechanisms reactive value pulled only once ----
  user <- reactiveValues(type = NULL)
  mechanisms <- reactiveValues(my_cat_ops = NULL)
  userGroups <- reactiveValues(streams = NULL)
  
  # Login process ----
  observeEvent(input$login_button, {
    tryCatch({
      datimutils::loginToDATIM(base_url = Sys.getenv("BASE_URL"),
                               username = input$user_name,
                               password = input$password,
                               d2_session_envir = parent.env(environment())
      )
      
      # DISALLOW USER ACCESS TO THE APP-----
      
      # store data so call is made only once
      userGroups$streams <-  datimutils::getMyStreams()
      user$type <- datimutils::getMyUserType()
      mechanisms$my_cat_ops <- datimutils::listMechs()
      
      # if a user is not to be allowed deny them entry
      if (!user$type %in% USG_USERS) {
        
        # alert the user they cannot access the app
        sendSweetAlert(
          session,
          title = "YOU CANNOT LOG IN",
          text = "You are not authorized to use this application",
          type = "error"
        )
        
        # log them out
        Sys.sleep(3)
        flog.info(paste0("User ", user_input$d2_session$me$userCredentials$username, " logged out."))
        user_input$authenticated  <-  FALSE
        user_input$user_name <- ""
        user_input$authorized  <-  FALSE
        user_input$d2_session  <-  NULL
        d2_default_session <- NULL
        gc()
        session$reload()
        
      }
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
  
  
  # show user information ----
  observeEvent(input$me_button, {
    output$message <- renderPrint({ user$type })
  })
  
  # show streams ids data ----
  observeEvent(input$groupid_button, {
    groups_id_df <- userGroups$streams
    groups_id_df_f <- groups_id_df[!grepl("^Global|OU", groups_id_df)]
    
    # display streams
    output$message <- renderPrint({ groups_id_df_f })
    
  })
  
  
  # show mechs by cocuid ----
  observeEvent(input$mech_cocuid_button, {
    
    # display mechanisms  
    output$table <- renderDataTable(mechanisms$my_cat_ops[,c("combo_id", "name")],
                                    options = list(
                                      pageLength = 10
                                    )
    )
    
  })
  
  # show mechs by mechs code----
  observeEvent(input$mech_id_button, {
    
    # display mechanisms  
    output$table <- renderDataTable(mechanisms$my_cat_ops[,c("mech_code", "name")],
                                    options = list(
                                      pageLength = 10
                                    )
    )
    
  })
  
  # show mechs by name ----
  observeEvent(input$mech_name_button, {
    
    # display mechanisms 
    output$table <- renderDataTable(mechanisms$my_cat_ops[,c("name"), drop=FALSE],
                                    options = list(
                                      pageLength = 10
                                    )
    )
    
    
  })
  
  # test data button ----
  observeEvent(input$test_data, {
    

      # USG users can see all the data
      output$table <- renderDataTable(sample_data,
                                      options = list(
                                        pageLength = 10
                                      )
      )
    
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