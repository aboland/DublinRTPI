#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(stringr)
library(XML)
library(rvest)



all_train_station_data <- xmlParse("http://api.irishrail.ie/realtime/realtime.asmx/getAllStationsXML")
all_train_station_data <- xmlToDataFrame(all_train_station_data, stringsAsFactors = F)
tidy_staion_data <- all_train_station_data %>% select(station = StationDesc, code = StationCode) %>%
  arrange(station) %>%
  mutate(code = tolower(code)) %>% distinct(station, .keep_all = TRUE)

tidy_station_list <- as.list(tidy_staion_data$code)
names(tidy_station_list) <- tidy_staion_data$station


source("helpers.R")


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  theme="bootstrap.css",
  includeCSS("www/styles.css"),
  
  
  
  navbarPage("Dublin - Real Time Passenger Info", id="main_navbar",
             tabPanel("Buses", 
                      fluidRow(column(3,uiOutput("currentTime", container = span)), 
                               column(3,uiOutput("db_last_update", container = span))
                      ),
                      sidebarLayout(
                        sidebarPanel(width=3, 
                                                br(),
                                                actionButton("bus_refresh", "Refresh"),
                                                br(),br(),
                                                # checkboxGroupInput("db_selected_stops", label = "Choose Stops", 
                                                                   # # choices = c("D'Olier St. - Outside Office (7b, 7d, 46a, 140, 145)" = 334, 
                                                                   # #             "D'Olier St. - D'Olier House (13, 39a, 40, 123)" = 335, 
                                                                   # #             "D'Olier St. - Ashfield House (9, 14, 16, 83, 83a)" = 336,
                                                                   # #             "Hawkins St. (Chaplins)" = 4495),
                                                                   # choiceNames = available_bus_stops$longname,
                                                                   # choiceValues = available_bus_stops$number,
                                                                   # selected = c(334, 336, 4495)),
                                                selectInput("db_selected_stops", label ="Choose Stops",
                                                               choices=1:5000,
                                                               selected = c(334, 336, 4495), multiple = T),
                                                br(),
                                                uiOutput("selected_buses_UI"),
                                                br(),br(),
                                                h3("Auto Refresh"),
                                     fluidRow(
                                       column(3, checkboxInput("auto_refresh_on", "On")),
                                       column(9,
                                              selectInput("interval", 
                                                          NULL,
                                                          # "Refresh interval",
                                                          choices = c(
                                                            "30 seconds" = 30,
                                                            "1 minute" = 60,
                                                            "2 minutes" = 120,
                                                            "5 minutes" = 300,
                                                            "10 minutes" = 600
                                                          ),
                                                          selected = 60)
                                              )
                                     ),
                                                
                                                br(),br(),
                                                h3("Custom URL"),
                                                p("A custom URL can be used to pre select choices when loading the app.",br(),
                                                  "Use the button below to create a URL for the choices currently selected."),br(),
                                                actionButton("bus_custom_url", "Create custom URL")
                        ),
                        mainPanel(
                          
                          # textOutput("testText"),
                          DT::dataTableOutput("bus_table")
                        )
                      )
                      
                      
                      
             ),
             tabPanel("Dart",
                      fluidRow(column(3,uiOutput("dart_currentTime", container = span)), 
                               column(3,uiOutput("dart_last_update", container = span))
                      ),
                      sidebarLayout(
                        sidebarPanel(width=3,
                                     actionButton("dart_refresh", "Refresh"),
                                     br(),br(),
                                     selectInput("dart_selected_stop", label = "Choose Stop", 
                                                 choices = tidy_station_list,
                                                 # choices = c("Tara Street" = "tara",
                                                 #             "Connolly" = "cnlly",
                                                 #             "Heustion" = "hston"),
                                                 selected = "tara "),
                                     uiOutput("dart_direction"),
                                     uiOutput("dart_destination"),
                                     br(),br(),
                                     h3("Custom URL"),
                                     p("A custom URL can be used to pre select choices when loading the app.",br(),
                                       "Use the button below to create a URL for the choices currently selected."),br(),
                                     actionButton("dart_custom_url", "Create custom URL")
                        ),
                        mainPanel(
                          DT::dataTableOutput("dart_table")
                        )
                      )
                      
             )
  )
)



# Define server logic for app
server <- function(input, output, session) {
  
  output$currentTime <- renderUI({
    # invalidateLater causes this output to automatically
    # become invalidated every minute
    invalidateLater(60*1000, session)
    input$bus_refresh  # also update when refresh clicked
    
    h2(paste0("Current Time: ", format(Sys.time(), "%H:%M")))
  })
  
  db_last_update_time <- reactiveValues(time = NULL)  # reactive value to store last time API was successfully called
  output$db_last_update <- renderUI({
    if(!is.null(db_last_update_time$time)){
      return(h2(paste0("Last Update: ",format(db_last_update_time$time, "%H:%M"))))
    }else{
      return(h2("No Update (click Refresh)"))
    }
  })
  
  # This observe takes inputs from the URL and updates the checkboxes for
  # the selected routes and buses.
  # This allows a user to start with their custom routes and buses selected.
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if("stops" %in% names(query)){
      selected_stops <- unlist(strsplit(query$stops, ","))
      updateSelectInput(session, "db_selected_stops", selected = selected_stops)
    }
    
    if("routes" %in% names(query)){
      selected_routes <- unlist(strsplit(query$routes, ","))
      updateCheckboxGroupInput(session, "db_selected_buses", selected = selected_routes)
    }
  })
  
  
  observeEvent(input$bus_custom_url, {
    custom_url <- paste0("http://", session$clientData[["url_hostname"]],":", session$clientData[["url_port"]],session$clientData[["url_pathname"]],
                         "?stops=",paste(input$db_selected_stops, collapse = ","), 
                         "&routes=", paste(input$db_selected_buses, collapse = ","))
    
    showModal(modalDialog(
      title = "Your custom URL",
      a(custom_url, href = custom_url),
      easyClose = TRUE
    ))
  })
  
  
  
  
  # Collect the bus times and tidy up times for the output table
  bus_times <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    
    # Only run after refresh has been clicked once or if paramters supplied
    if(input$bus_refresh > 0 || sum(c("stops", "routes") %in% names(query))>0){  
      
      if(input$auto_refresh_on)
        invalidateLater(as.numeric(input$interval) * 1000)
      
      
      bus_info <- tryCatch({
        api_info <- db_scrape_multi_stop_info(isolate(input$db_selected_stops))$results
        list(results = api_info, error = FALSE)
      }, error = function(e){return(list(results = "Could not retrieve results", error= TRUE))}
      )
      
      # bus_info <- list(results= sample_bus_info, error= FALSE)
      
      if(bus_info$error == FALSE){
        bus_info_tidy <- bus_info$results %>%
          mutate(
            # lastbuscontact = as.POSIXct(sourcetimestamp, format="%d/%m/%Y %H:%M:%S") %>% format("%H:%M"),
            datatime = as.POSIXct(datatime, format="%Y/%m/%d %H:%M:%S") %>% format("%H:%M"),
            route = as.factor(route)) %>% 
          select(Route = route, Destination = destination, EstimatedArrival = duetime)  # , ArrivalLastUpdate = datatime, LastBusContact = lastbuscontact
        
        db_last_update_time$time <<- Sys.time()
      }else{
        bus_info_tidy <- data_frame(error = bus_info$results)
      }
      
      return(bus_info_tidy)
      
    }
  })
  
  
  
  # The idea of this is to update the list of possible bus routes depending on 
  # which routes are selected
  output$selected_buses_UI <- renderUI({
    
    if(!is.null(bus_times())){
      bus_routes <- bus_times() %>% 
      distinct(Route) %>% 
      mutate(numeric_val = gsub("[^0-9]", "", Route) %>% as.numeric()) %>% 
      arrange(numeric_val) %>% pull(Route)
    }
    
    if("Route" %in% names(bus_times())){
      
      if(is.null(input$db_selected_buses)){
        query <- parseQueryString(session$clientData$url_search)
        if("routes" %in% names(query)){
          selected_routes <- unlist(strsplit(query$routes, ","))
        }else{
          selected_routes <- bus_routes
        }
      }else{
        
        selected_routes <- input$db_selected_buses
      }
      
      checkboxGroupInput("db_selected_buses", label = "Choose Routes", 
                         choices = bus_routes, selected = selected_routes, inline = TRUE)
    }
  })
  
  
  
  # This is the table of bus times which is displayed in the app
  output$bus_table <- DT::renderDataTable({
    if(!is.null(input$db_selected_buses)){
      return(bus_times() %>% filter(Route %in% input$db_selected_buses))
    }else{
      return(bus_times())
    }
    
  }, options = list(dom = "ti", 
                    pageLength=100, lengthMenu=c(10, 50 ,100),
                    # columnDefs = list(list(className = 'dt-center', targets = 3)),
                    initComplete = DT::JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                      "}"),
                    # scrollY = "88vh", scrollX = TRUE,
                    scrollCollapse=TRUE), 
  rownames=F)
  
  
  
  
  
  
  
  
  
  # --------------------------------------------------------------------------------------------
  #
  #      DART
  #
  # -------------------------------------------------------------------------------------------
  
  
  output$dart_currentTime <- renderUI({
    # invalidateLater causes this output to automatically
    # become invalidated every minute
    invalidateLater(30*1000, session)
    input$dart_refresh
    
    h2(paste0("Current Time: ", format(Sys.time(), "%H:%M")))
  })
  
  dart_last_update_time <- reactiveValues(time = NULL)  # reactive value to store last time API was successfully called
  output$dart_last_update <- renderUI({
    if(!is.null(dart_last_update_time$time)){
      return(h2(paste0("Last Update: ",format(dart_last_update_time$time, "%H:%M"))))
    }else{
      return(h2("No Update"))
    }
  })
  
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if(sum(c("direction", "destination", "station") %in% names(query)) > 0){
      updateNavbarPage(session, "main_navbar", selected = "Dart")
      # browser()
      updateSelectInput(session, "dart_selected_stop", selected = query$station)
      # selected_direction <- unlist(strsplit(query$direction, ","))
      # updateCheckboxGroupInput(session, "selected_dart_direction", selected = selected_direction)
    }
  })
  
  
  
  # observe({
  #   session$sendCustomMessage(type = 'testmessage',
  #                             message = list(a = 1, b = 'text',
  #                                            controller = input$dart_custom_url))
  # })
  
  observeEvent(input$dart_custom_url, {
    # custom_url <- paste0("http://127.0.0.1:4727/?station=",input$dart_selected_stop, "&direction=", paste(input$selected_dart_direction, collapse = ","))
    
    custom_url <- paste0("http://", session$clientData[["url_hostname"]],":", session$clientData[["url_port"]],session$clientData[["url_pathname"]],
                         "?station=",input$dart_selected_stop, 
                         "&direction=", paste(input$selected_dart_direction, collapse = ","), 
                         "&destination=", paste(input$selected_dart_destination, collapse = ","))
    
    showModal(modalDialog(
      title = "Your custom URL",
      a(custom_url, href = custom_url),
      easyClose = TRUE
    ))
  })
  
  
  dart_times <- reactive({
    input$dart_refresh
    invalidateLater(30*1000, session)
    
    temp_info <- dart_stop_info(input$dart_selected_stop)
    
    dart_last_update_time$time <<- Sys.time()
    
    return(temp_info)
    })
  
  
  output$dart_direction <- renderUI({
    
    possible_directions <- unique(dart_times()$Direction)
    
    if(is.null(input$selected_dart_direction)){
      query <- parseQueryString(session$clientData$url_search)
      if("direction" %in% names(query)){
        selected_direction <- unlist(strsplit(query$direction, ","))
      }else{
        selected_direction <- possible_directions
      }
    }else{
      selected_direction <- input$selected_dart_direction
    }
    
    checkboxGroupInput("selected_dart_direction", "Direction",
                       choices = possible_directions,
                       selected = selected_direction,
                       inline = T)
  })
  
  output$dart_destination <- renderUI({
    
    possible_destinations <- unique(dart_times()$Destination)
    
    if(is.null(input$selected_dart_destination)){
      query <- parseQueryString(session$clientData$url_search)
      if("destination" %in% names(query)){
        selected_destinations <- unlist(strsplit(query$destination, ","))
      }else{
        selected_destinations <- possible_destinations
      }
    }else{
      selected_destinations <-input$selected_dart_destination
    }
    
    checkboxGroupInput("selected_dart_destination", "Destination",
                       choices = possible_destinations,
                       selected = selected_destinations)
  })
  
  
  dart_table <- reactive({
    dart_table <- dart_times()
    
    if(!is.null(input$selected_dart_direction))
      dart_table <- dart_table %>% filter(Direction %in% input$selected_dart_direction)
    
    if(!is.null(input$selected_dart_destination))
      dart_table <- dart_table %>% filter(Destination %in% input$selected_dart_destination)
    
    dart_table <- dart_table %>% select(Due = Duein, Destination, Status, LastLocation = Lastlocation, ExpectedDeparture = Expdepart, Direction, Type = Traintype)
  })
  
  
  
  output$dart_table <- DT::renderDataTable({
    
    dart_table()
    
  }, options = list(dom = "ti", 
                    pageLength=100,
                    # columnDefs = list(list(className = 'dt-center', targets = 3)),
                    initComplete = DT::JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                      "}"),
                    # scrollY = "88vh", scrollX = TRUE,
                    scrollCollapse=TRUE), 
  rownames=F)
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

