##### SECOND APP #####

##### Setting up our global environment #####

library(shiny)
library(httr)
library(leaflet)
library(XML)
library(shinythemes)
library(shinydashboard)

ngiTheme <- bslib::bs_theme(
  bg = "#464646", 
  fg = "#AE2025", 
  primary = "#d9534f", 
  base_font = "Grandstander"
)

##### Setting up our global environment #####
ui <- fluidPage(
  #theme = shinytheme("superhero"),
  theme = ngiTheme,
  
  # Application title
  titlePanel("Weather Forecast"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      numericInput("lat",
                   "Latitude:",
                   "61.74495"),
      
      numericInput("long",
                   "Longitude:",
                   "8.43067")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot",width = "100%"),
      leaflet::leafletOutput("map"),
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  dataset = reactive({
    
    baseUrl = "https://api.met.no/weatherapi/locationforecast/2.0/classic"
    url = paste0(baseUrl,"?lat=",input$lat,"&lon=",input$long)
    response = try(httr::GET(url))
    
    #test if response is empty
    if(is.null(response)) return()
    if(is.null(response$status_code != 200)) return()
    
    #extraxt data from rawdata
    content = content(response, encoding ="UTF-8")
    res <- xmlParse(content)
    resList = xmlToList(res)
    prod = resList$product
    
    #extraxt values from data
    temp = NULL
    time = as.POSIXct(NULL)
    
    for(i in 1:length(prod)){
      
      #cleaning temperature
      tempTemp = as.numeric(prod[i]$time$location$temperature["value"])
      if(length(tempTemp) == 0) next()
      temp = c(temp,tempTemp)
      
      #cleaning time string
      timeRaw = gsub("T"," ",prod[i]$time$.attrs["to"])
      timeRaw = gsub("Z","",timeRaw)
      tempTime = as.POSIXct(timeRaw)
      if(length(tempTime) == 0) next()
      time = c(time,tempTime)
      
    }
    
    return(list("time" = time, "temp" = temp))
  })
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    
    dataPlot = dataset()
    if(is.null(dataPlot)) return()
    
    x    <- dataPlot$time
    y    <- dataPlot$temp
    
    plot(x,y, type ="l",xlab="Date",ylab="Temperature")
  })
  
  output$map = renderLeaflet({
    
    leaflet() %>%
      addTiles() %>%
      addAwesomeMarkers(lng = input$long, lat = input$lat)
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
