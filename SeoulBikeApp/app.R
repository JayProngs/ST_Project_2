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
df <- df %>%
  mutate(across(c(Seasons, Holiday, Functioning_Day), as.factor))
# Convert relevant columns to numeric
df <- df %>%
  mutate(across(c(Rented_Bike_Count, Hour, Temperature, Humidity, Wind_speed,
                  Visibility, Dew_point_temperature, Solar_Radiation, 
                  Rainfall, Snowfall), as.numeric))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Seoul Bike Sharing Data Explorer"),

    # Sidebar
    sidebarLayout(
      sidebarPanel(
        # Widget to select first categorical variable for subsetting
        selectInput("cat_var1", "Select Categorical Variable 1:",
                    choices = c("Seasons", "Holiday", "Functioning_Day"),
                    selected = "Seasons"),
        
        selectInput("cat_var1_level", "Select Levels of Seasons",
                    choices = levels(df$Seasons),
                    selected = levels(df$Seasons),
                    multiple = FALSE),
        
        # Widget to select second categorical variable for subsetting
        selectInput("cat_var2", "Select Categorical Variable 2:",
                    choices = c("Seasons", "Holiday", "Functioning_Day"),
                    selected = "Holiday"),
        
        selectInput("cat_var2_level", "Select Levels of Holiday",
                    choices = levels(df$Holiday),
                    selected = levels(df$Holiday),
                    multiple = FALSE),
        
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

# Define server logic
server <- function(input, output, session) {
  
  # Reactive values to store the subsetted data
  values <- reactiveValues(data = df)
  
  # First categorical variable
  output$cat_var1_levels <- renderUI({
    req(input$cat_var1)
    selectInput("cat_var1_level", paste("Select Levels of", input$cat_var1),
                choices = levels(as.factor(df[[input$cat_var1]])),
                selected = levels(as.factor(df[[input$cat_var1]])),
                multiple = TRUE)
  })
  
  # Second categorical variable
  output$cat_var2_levels <- renderUI({
    req(input$cat_var2)
    selectInput("cat_var2_level", paste("Select Levels of", input$cat_var2),
                choices = levels(as.factor(df[[input$cat_var2]])),
                selected = levels(as.factor(df[[input$cat_var2]])),
                multiple = TRUE)
  })
  
  # First numeric variable
  output$num_var1_slider <- renderUI({
    req(input$num_var1)
    numeric_var <- df[[input$num_var1]]
    sliderInput("num_var1_range", paste("Select Range of", input$num_var1),
                min = min(numeric_var, na.rm = TRUE),
                max = max(numeric_var, na.rm = TRUE),
                value = c(min(numeric_var, na.rm = TRUE), max(numeric_var, na.rm = TRUE)))
  })
  
  # Second numeric variable
  output$num_var2_slider <- renderUI({
    req(input$num_var2)
    numeric_var <- df[[input$num_var2]]
    sliderInput("num_var2_range", paste("Select Range of", input$num_var2),
                min = min(numeric_var, na.rm = TRUE),
                max = max(numeric_var, na.rm = TRUE),
                value = c(min(numeric_var, na.rm = TRUE), max(numeric_var, na.rm = TRUE)))
  })
  
  # Subset the data
  observeEvent(input$update_button, {
    print(paste("Selected Levels for", input$cat_var1, ":", paste(input$cat_var1_level, collapse = ", ")))
    print(paste("Selected Levels for", input$cat_var2, ":", paste(input$cat_var2_level, collapse = ", ")))
    subset_data <- df %>%
      filter(
        .data[[input$cat_var1]] %in% input$cat_var1_level,
        .data[[input$cat_var2]] %in% input$cat_var2_level,
        .data[[input$num_var1]] >= input$num_var1_range[1],
        .data[[input$num_var1]] <= input$num_var1_range[2],
        .data[[input$num_var2]] >= input$num_var2_range[1],
        .data[[input$num_var2]] <= input$num_var2_range[2]
      )
    values$data <- subset_data
  })
  
  # show the data in the Data Download tab
  output$data_table <- DT::renderDataTable({
    DT::datatable(values$data)
  })
  
  # downloading the data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("seoul_bike_data_subset", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$data, file, row.names = FALSE)
    }
  )
  
}



# Run the application 
shinyApp(ui = ui, server = server)
