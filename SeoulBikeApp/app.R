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

# Read in the data
df <- read.csv("SeoulBikeData.csv", header = FALSE)
df <- df[-1, ]  # Remove the first row if it's redundant
colnames(df) <- c("Date", "Rented_Bike_Count", "Hour", "Temperature", "Humidity", 
                  "Wind_speed", "Visibility", "Dew_point_temperature", "Solar_Radiation",
                  "Rainfall", "Snowfall", "Seasons", "Holiday", "Functioning_Day")

# Convert relevant columns to numeric
df <- df %>%
  mutate(across(c(Rented_Bike_Count, Hour, Temperature, Humidity, Wind_speed,
                  Visibility, Dew_point_temperature, Solar_Radiation, 
                  Rainfall, Snowfall), as.numeric))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Seoul Bike Sharing Data Explorer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
          # Widget to select first categorical variable for subsetting
          selectInput("cat_var1", "Select Categorical Variable 1:",
                      choices = c("Seasons", "Holiday", "Functioning_Day"),
                      selected = "Seasons"),
          uiOutput("cat_var1_levels"),
          
          # Widget to select second categorical variable for subsetting
          selectInput("cat_var2", "Select Categorical Variable 2:",
                      choices = c("Seasons", "Holiday", "Functioning_Day"),
                      selected = "Holiday"),
          uiOutput("cat_var2_levels"),
          
          # Widget to select first numeric variable for subsetting
          selectInput("num_var1", "Select Numeric Variable 1:",
                      choices = c("Temperature", "Humidity", "Wind_speed",
                                  "Visibility", "Solar_Radiation", "Rainfall", "Snowfall"),
                      selected = "Temperature"),
          uiOutput("num_var1_slider"),
          
          # Widget to select second numeric variable for subsetting
          selectInput("num_var2", "Select Numeric Variable 2:",
                      choices = c("Temperature", "Humidity", "Wind_speed",
                                  "Visibility", "Solar_Radiation", "Rainfall", "Snowfall"),
                      selected = "Humidity"),
          uiOutput("num_var2_slider"),
          
          # Action button to subset data
          actionButton("update_button", "Update Data"),
          
          # Instructions
          helpText("Select variables and click 'Update Data' to apply subsetting.")
          
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("About", 
                     h3("About This App"),
                     p("This app allows users to explore the Seoul Bike Sharing Demand dataset."),
                     p("Use the sidebar to subset the data and explore various summaries and plots."),
                     p("Data Source: ",
                       a("Kaggle Dataset", href = "https://www.kaggle.com/datasets/saurabhshahane/seoul-bike-sharing-demand-prediction", target = "_blank")),
                     img(src = "https://storage.googleapis.com/kaggle-datasets-images/1182486/1978532/347f28c40107cd8c896fb8e1230fcc87/dataset-cover.jpg?t=2021-02-26-16-11-12", height = "200px", width="400px"),
                     p("The app contains the following tabs:"),
                     tags$ul(
                       tags$li(strong("About:"), " Information about the app and data."),
                       tags$li(strong("Data Download:"), " View and download the dataset."),
                       tags$li(strong("Data Exploration:"), " Generate summaries and plots.")
                     )
            ),
            tabPanel("Data Download",
                     DT::dataTableOutput("data_table"),
                     downloadButton("download_data", "Download Data")
            ),
            tabPanel("Data Exploration",
                     # Sub-tabs or content for data exploration
                     uiOutput("exploration_ui")
            )
          )
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
