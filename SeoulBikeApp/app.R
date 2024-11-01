#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(readr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Seoul Bike Sharing Data Explorer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          # Dropdowns for categorical variables
          # Dropdowns for categorical variables
          selectInput("season", "Select Season:", choices = unique(df$Seasons), multiple = TRUE, selected = unique(df$Seasons)),
          selectInput("holiday", "Select Holiday:", choices = unique(df$Holiday), multiple = TRUE, selected = unique(df$Holiday)),
          
          # Dynamic sliders for numeric filtering
          uiOutput("temp_range"),
          uiOutput("humidity_range"),
          
          # Action button to apply filter
          actionButton("filter_data", "Apply Filter")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
