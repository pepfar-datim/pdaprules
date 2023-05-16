# import libraries -----
library(shiny)
library(futile.logger)
library(shinyWidgets)

source("./read_data.R")
source("./oAuthLogin.R")


### OAuth Config
if (interactive()) {
  # testing url
  options(shiny.port = 8100)
  APP_URL <- "http://127.0.0.1:8100/"# This will be your local host path
} else {
  # deployed URL
  APP_URL <- "https://rstudio-connect.testing.ap.datim.org/content/XX" #This will be your shiny server path
}

################ OAuth Client information ##################################### 
{
  app <- oauth_app("Shiny App Datimutils", # dhis2 = Name
                   key = "Shiny App Datimutils",         # dhis2 = Client ID
                   secret = "e74a0ced9-946d-8e9b-2b31-2a6b330f36e", #dhis2 = Client Secret
                   redirect_uri = APP_URL
  )
  
  api <- oauth_endpoint(base_url = "https://cop-test.datim.org/uaa/oauth", #paste(Sys.getenv("BASE_URL"),"uaa/oauth",sep = ""),
                        request=NULL,# Documentation says to leave this NULL for OAuth2 
                        authorize = "authorize",
                        access="token"
  ) 
  
  scope <- "ALL"
}

has_auth_code <- function(params) {
  
  return(!is.null(params$code))
}

mykey = paste(sample(LETTERS,20,replace = T),collapse="") # Just an added layer of security, feel free to remove here and the output$ui_hasauth block






### App global variables
# which users should have access to data? IN THIS CASE ONLY PARTNERS - USG FOLKS SEE EVERYTHING, PARTNERS LIMITED TO MECH ACCESS
USG_USERS = c("Agency", "Interagency", "Global Agency", "Global")
PARTNER_USERS = c("Global Partner", "Partner")










# server ----
server <- function(input, output, session) {
  
  #Username and PW login Option
  output$ui_hasauth = renderUI({
    req(input$login_button)
    req(input$user_name)
    req(input$password)
    
    hashcode = safer::encrypt_string(paste0(input$user_name,input$password,'@time:',Sys.time()),key = mykey)
    
    if(user_input$authenticated  ==  TRUE){
      
      hashcode = safer::encrypt_string(paste0(input$user_name,input$password,'@time:',Sys.time()),key = mykey)
      
      redirect <- sprintf("location.replace(\"%s\");", paste0(APP_URL,"?code="))
      tags$script(HTML(redirect))
      
    } 
  })
  
  # Login process ----
  observeEvent(input$login_button, {
    tryCatch({
      datimutils::loginToDATIM(base_url = "https://cop-test.datim.org/", #Sys.getenv("BASE_URL"),
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
      if (!user$type %in% c(USG_USERS, PARTNER_USERS)) {
        
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
  
  # logout process ----
  observeEvent(input$logout_button, {
    req(input$logout_button)
    # Gets you back to the login without the authorization code at top
    updateQueryString("?",mode="replace",session=session) 
    flog.info(paste0("User ", user_input$d2_session$me$userCredentials$username, " logged out."))
    user_input$authenticated  <-  FALSE
    user_input$user_name <- ""
    user_input$authorized  <-  FALSE
    user_input$d2_session  <-  NULL
    d2_default_session <- NULL
    gc()
    session$reload()
  })
  
  # OAuth Redirect process ----
  output$ui_redirect = renderUI({
    #print(input$login_button_oauth) useful for debugging 
    if(!is.null(input$login_button_oauth)){
      if(input$login_button_oauth>0){
        url <- oauth2.0_authorize_url(api, app, scope = scope)
        redirect <- sprintf("location.replace(\"%s\");", url)
        tags$script(HTML(redirect))
      } else NULL
    } else NULL
  })
  
  ### Login Button oauth Checks 
  observeEvent(input$login_button_oauth > 0,{
    
    #Grabs the code from the url
    params <- parseQueryString(session$clientData$url_search)
    req(has_auth_code(params)) 
    
    tryCatch({
      
      #Manually create a token
      token <- oauth2.0_token(
        app = app,
        endpoint =api,
        scope = scope,
        use_basic_auth = TRUE,
        oob_value=APP_URL,
        cache = FALSE,
        credentials = oauth2.0_access_token(endpoint = api,
                                            app = app,
                                            code = params$code,
                                            use_basic_auth = TRUE)
      )
      
      loginToDATIMOAuth(base_url = "https://cop-test.datim.org/", #Sys.getenv("BASE_URL"),
                        token = token,
                        app=app,
                        api = api,
                        redirect_uri= APP_URL,
                        scope = scope,
                        d2_session_envir = parent.env(environment())
      )
      
      
      
      
      
      # DISALLOW USER ACCESS TO THE APP-----
      
      # store data so call is made only once
      userGroups$streams <-  datimutils::getMyStreams()
      user$type <- datimutils::getMyUserType()
      mechanisms$my_cat_ops <- datimutils::listMechs()
      
      # if a user is not to be allowed deny them entry
      if (!user$type %in% c(USG_USERS, PARTNER_USERS)) {
        
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
    
    
    
    
    
    error = function(e) { # Not needed for oauth
      #flog.info(paste0("User ", input$user_name, " login failed."),
      #name = "datapack")
    }
    )
    
    if (exists("d2_default_session")) {
      if (any(class(d2_default_session) == "d2Session")) {
        user_input$authenticated  <-  TRUE
        user_input$d2_session  <-  d2_default_session$clone()
        d2_default_session <- NULL
        
        # Need to check the user is a member of the
        # PRIME Data Systems Group, COP Memo group, or a super user
        user_input$memo_authorized  <-
          grepl("VDEqY8YeCEk|ezh8nmc4JbX",
                user_input$d2_session$me$userGroups) |
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
      
    }
    
  })
  
  ##################### ACTUAL APP ###############################  
  # show user information ----
  observeEvent(input$me_button, {
    output$message <- renderPrint({ user$type })
  })
  
  # show streams ids data ----
  observeEvent(input$groupid_button, {
    groups_id_df <- userGroups$streams
    
    # display streams
    output$message <- renderPrint({ groups_id_df })
    
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
    
    # show data test data based on user type
    if (user$type %in% USG_USERS) {
      
      # USG users can see all the data
      output$table <- renderDataTable(sample_data,
                                      options = list(
                                        pageLength = 10
                                      )
      )
      
      # PARTNERS see filtered data
    } else {
      
      sample_data_f <- merge(mechanisms$my_cat_ops, sample_data, by= "mech_code")
      output$table <- renderDataTable(sample_data_f,
                                      options = list(
                                        pageLength = 10
                                      )
      )
    }
    
  })
  ##########################################################
  
  #Probs not needed
  #Check if the user is authenticated via console
  observe(print(isolate(user_input$authenticated)))
} 
  
  
  
  
  
  
  
  
  
  
  
  ############### UI ###########################################################
  
  ### Developers will place their AFTER AUTHENTICATED code in this ui block 
  ui <- function(req_txt) {
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
  }
  
  
  # login page with username and password
  ui_auth <- function(req_txt){
    
    fluidPage(
      wellPanel(
        fluidRow(
          h4("The following up was created as an example of how developers can access the datim security information of their shiny app users utilizing the package datimutils. Please login with your DATIM credentials:"),
          br()
        ),
        fluidRow(
          textInput("user_name", "Username: ", width = "500px"),
          passwordInput("password", "Password:", width = "500px"),
          actionButton("login_button", "Log in!"),
          actionButton("login_button_oauth","Log in with DATIM"),
          uiOutput("ui_hasauth"),
          uiOutput("ui_redirect")
        )
      )
    )
    
  }
  
  
  #Assists with standard UserName and PW method
  user_input <- reactiveValues(authenticated = FALSE,
                               status = "",
                               d2_session = NULL,
                               memo_authorized = FALSE)
  
  # User and mechanisms reactive value pulled only once ----
  user <- reactiveValues(type = NULL)
  mechanisms <- reactiveValues(my_cat_ops = NULL)
  userGroups <- reactiveValues(streams = NULL)
  
  
  uiFunc <- function(req) {
    if (!has_auth_code(parseQueryString(req$QUERY_STRING))){
      ui_auth(req_txt)
    } else {
      ui(req_txt)
    }
  }
  
 # NOTE: uiFunc, as opposed to ui
   shinyApp(uiFunc, server)