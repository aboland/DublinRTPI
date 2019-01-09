#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Data frame used to provide possible bus stop selection
available_bus_stops <- data_frame(longname = c("D'Olier St. - Outside Office (7b, 7d, 46a, 140, 145)", 
                                               "D'Olier St. - D'Olier House (13, 39a, 40, 123)", 
                                               "D'Olier St. - Ashfield House (9, 14, 16, 83, 83a)",
                                               "Hawkins St. - Chaplins (15, 27, 68, 69, 120,...)"),
                                  shortname = c("D'Olier - Outside Office", 
                                                "D'Olier - D'Olier House", 
                                                "D'Olier - Ashfield House",
                                                "Hawkins St."),
                                  number = c(334, 
                                             335, 
                                             336,
                                             4495))


# Function to access RTPI API
db_get_multi_stop_info <- function(stop_numbers){
  
  # Check that the input stop numbers are numeric
  stop_numbers = as.numeric(stop_numbers)
  if(sum(is.na(stop_numbers))>0)
    stop("Non numeric stop number!")
  
  stop_info <- list()
  combined_info <- NULL
  
  # Progress function will only work in Shiny
  withProgress(message = 'Updating data', value = 0.1, {
    # Loop over all bus stops
    for(i in 1:length(stop_numbers)){
      # Update progress bar message
      incProgress(0, detail = paste0("Getting stop ", stop_numbers[i], " info"))
      
      # Call API
      temp_info <- jsonlite::fromJSON(paste0("https://data.smartdublin.ie/cgi-bin/rtpi/realtimebusinformation?stopid=", stop_numbers[i],"&format=json"))
      
      if(temp_info$errorcode == 0){
        # If no error then tidy up data
        temp_info <- temp_info$results %>% 
          select(arrivaldatetime, duetime, departureduetime, destination, route, monitored, sourcetimestamp, monitored) %>%
          mutate(datatime = Sys.time(), stopnumber = stop_numbers[i])
        stop_info[[i]] <- temp_info
        combined_info <- bind_rows(combined_info, temp_info)
      }else{
        # If error return the error message
        stop_info[[i]] <- temp_info$errormessage
      }
      # increase progress bar indicator
      incProgress(1/length(stop_numbers))
    }
  })
  names(stop_info) <- paste0("number", stop_numbers)
  combined_info <- combined_info %>% arrange(arrivaldatetime)
  return(list(results = combined_info, stop = stop_info))
}




# Define UI for application that draws a histogram
ui <- fluidPage(
  
  theme="bootstrap.css",
  includeCSS("www/styles.css"),
  
  
  
  navbarPage("O'Connell Bridge House - Real Time Transport Info", id="main_navbar",
             tabPanel("Buses", 
                      fluidRow(column(3,uiOutput("currentTime", container = span)), 
                               column(3,uiOutput("last_update", container = span))
                      ),
                      sidebarLayout(
                        sidebarPanel(width=3, 
                                     tabsetPanel(
                                       tabPanel("Bus Stop & Routes",
                                                br(),
                                                actionButton("bus_refresh", "Refresh"),
                                                br(),br(),
                                                checkboxGroupInput("db_selected_stops", label = "Choose Stops", 
                                                                   # choices = c("D'Olier St. - Outside Office (7b, 7d, 46a, 140, 145)" = 334, 
                                                                   #             "D'Olier St. - D'Olier House (13, 39a, 40, 123)" = 335, 
                                                                   #             "D'Olier St. - Ashfield House (9, 14, 16, 83, 83a)" = 336,
                                                                   #             "Hawkins St. (Chaplins)" = 4495),
                                                                   choiceNames = available_bus_stops$longname,
                                                                   choiceValues = available_bus_stops$number,
                                                                   selected = c(334, 336, 4495)),
                                                br(),
                                                uiOutput("selected_buses_UI")
                                       ),
                                       tabPanel("Auto Refresh",
                                                checkboxInput("auto_refresh_on", "On"),
                                                selectInput("interval", "Refresh interval",
                                                            choices = c(
                                                              "30 seconds" = 30,
                                                              "1 minute" = 60,
                                                              "2 minutes" = 120,
                                                              "5 minutes" = 300,
                                                              "10 minutes" = 600
                                                            ),
                                                            selected = 60)
                                       )
                                     )
                        ),
                        mainPanel(
                          textOutput("testText"),
                          DT::dataTableOutput("bus_table")
                        )
                      )
                      
                      
                      
             ),
             tabPanel("Dart",
                      h1("Coming soon..."))
  )
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$currentTime <- renderUI({
    # invalidateLater causes this output to automatically
    # become invalidated every minute
    invalidateLater(60*1000, session)
    input$bus_refresh  # also update when refresh clicked
    
    h2(paste0("Current Time: ", format(Sys.time(), "%H:%M")))
  })
  
  last_update_time <- reactiveValues(time = NULL)  # reactive value to store last time API was successfully called
  output$last_update <- renderUI({
    if(!is.null(last_update_time$time)){
      return(h2(paste0("Last Update: ",format(last_update_time$time, "%H:%M"))))
    }else{
      return(h2("No Update"))
    }
  })
  
  # This observe takes inputs from the URL and updates the checkboxes for
  # the selected routes and buses.
  # This allows a user to start with their custom routes and buses selected.
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if("stops" %in% names(query)){
      selected_stops <- unlist(strsplit(query$stops, ","))
      updateCheckboxGroupInput(session, "db_selected_stops", selected = selected_stops)
    }
    
    if("routes" %in% names(query)){
      selected_routes <- unlist(strsplit(query$routes, ","))
      updateCheckboxGroupInput(session, "db_selected_buses", selected = selected_routes)
    }
  })
  
  
  # Collect the bus times and tidy up times for the output table
  bus_times <- reactive({
    input$bus_refresh
    if(input$auto_refresh_on)
      invalidateLater(as.numeric(input$interval) * 1000)
    
    bus_info <- tryCatch({
      api_info <- db_get_multi_stop_info(isolate(input$db_selected_stops))$results
      list(results = api_info, error = FALSE)
    }, error = function(e){return(list(results = "Could not retrieve results", error= TRUE))}
    )
    
    # bus_info <- list(results= sample_bus_info, error= FALSE)
    
    if(bus_info$error == FALSE){
      bus_info_tidy <- bus_info$results %>%
        mutate(
          # lastbuscontact = difftime(lubridate::now(),  lubridate::dmy_hms(sourcetimestamp), units="mins") %>% round(), 
          # datatime = difftime(lubridate::now(),  lubridate::ymd_hms(datatime), units="mins") %>% round(),
          lastbuscontact = lubridate::dmy_hms(sourcetimestamp) %>% format("%H:%M"),
          datatime = lubridate::ymd_hms(datatime) %>% format("%H:%M"),
          route = as.factor(route)) %>% 
        select(Route = route, Destination = destination, EstimatedArrival = duetime, LastBusContact = lastbuscontact)  # , ArrivalLastUpdate = datatime
      
      last_update_time$time <<- Sys.time()
    }else{
      bus_info_tidy <- data_frame(error = bus_info$results)
    }
    
    return(bus_info_tidy)
  })
  
  
  
  # The idea of this is to update the list of possible bus routes depending on 
  # which routes are selected
  output$selected_buses_UI <- renderUI({
    if("Route" %in% names(bus_times())){
      bus_routes <- bus_times() %>% 
        # filter(Route == input$db_selected_stops) %>%
        distinct(Route) %>% 
        mutate(numeric_val = gsub("[^0-9]", "", Route) %>% as.numeric()) %>% 
        arrange(numeric_val) %>% pull(Route)
      
      
      query <- parseQueryString(session$clientData$url_search)
      if("routes" %in% names(query)){
        selected_routes <- unlist(strsplit(query$routes, ","))
      }else{
        selected_routes <- bus_routes
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
                    columnDefs = list(list(className = 'dt-center', targets = 3)),
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

