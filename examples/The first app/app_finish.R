##### FIRST APP #####


library(shiny)

library(reactlog)
options(shiny.reactlog = TRUE)

ui <- fluidPage(

    titlePanel("Data Explorer"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            selectInput("selDataset", "select Dataset", choices = c("Old Faithful","Cars"))

        ),

        mainPanel(
           plotOutput("distPlot",width = "50%")
        )
    )
)

server <- function(input, output) {

    dataset = reactive({
        if(input$selDataset == "Old Faithful") dataset = faithful[,2]
        else if (input$selDataset == "Cars") dataset = as.numeric(mtcars[,1])

        return(dataset)
    })

    output$distPlot <- renderPlot({

        x    <- dataset()
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

shinyApp(ui = ui, server = server)
