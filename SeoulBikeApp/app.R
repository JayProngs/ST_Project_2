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
  
  # Update levels for the first categorical variable
  output$cat_var1_levels <- renderUI({
    req(input$cat_var1)
    selectInput("cat_var1_level", paste("Select Levels of", input$cat_var1),
                choices = c("All of the above", levels(as.factor(df[[input$cat_var1]]))),
                selected = "All of the above", multiple = FALSE)
  })
  
  # Update levels for the second categorical variable
  output$cat_var2_levels <- renderUI({
    req(input$cat_var2)
    selectInput("cat_var2_level", paste("Select Levels of", input$cat_var2),
                choices = c("All of the above", levels(as.factor(df[[input$cat_var2]]))),
                selected = "All of the above", multiple = FALSE)
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
    # Print the selected options for debugging
    print(paste("Selected Levels for", input$cat_var1, ":", paste(input$cat_var1_level, collapse = ", ")))
    print(paste("Selected Levels for", input$cat_var2, ":", paste(input$cat_var2_level, collapse = ", ")))
    
    # Filter data based on user inputs
    subset_data <- df %>%
      filter(
        (input$cat_var1_level == "All of the above" | .data[[input$cat_var1]] %in% input$cat_var1_level),
        (input$cat_var2_level == "All of the above" | .data[[input$cat_var2]] %in% input$cat_var2_level),
        .data[[input$num_var1]] >= input$num_var1_range[1],
        .data[[input$num_var1]] <= input$num_var1_range[2],
        .data[[input$num_var2]] >= input$num_var2_range[1],
        .data[[input$num_var2]] <= input$num_var2_range[2]
      )
    
    # Update the reactive values with the filtered data
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
  
  output$exploration_ui <- renderUI({
    tagList(
      # Set default selected value
      selectInput("summary_type", "Choose Summary Type:",
                  choices = c("Categorical Summaries", "Numeric Summaries", "Plots"),
                  selected = "Categorical Summaries"),
      uiOutput("summary_ui")
    )
  })
  
  output$summary_ui <- renderUI({
    req(input$summary_type)
    if (input$summary_type == "Categorical Summaries") {
      tagList(
        selectInput("cat_var_summary", "Select Categorical Variable:",
                    choices = names(df)[sapply(df, is.factor)]),
        verbatimTextOutput("cat_summary")
      )
    } else if (input$summary_type == "Numeric Summaries") {
      tagList(
        selectInput("num_var_summary", "Select Numeric Variable:",
                    choices = names(df)[sapply(df, is.numeric)]),
        selectInput("group_var", "Group By (Categorical Variable):",
                    choices = c("None", names(df)[sapply(df, is.factor)])),
        verbatimTextOutput("num_summary")
      )
    } else if (input$summary_type == "Plots") {
      tagList(
        selectInput("plot_type", "Select Plot Type:",
                    choices = c("Scatter Plot", "Box Plot", "Heatmap", "Custom Plot")),
        uiOutput("plot_ui"),
        plotOutput("plot_output")
      )
    }
  })
  
  observe({
    req(input$summary_type)
    if (input$summary_type == "Categorical Summaries") {
      output$cat_summary <- renderPrint({
        req(input$cat_var_summary)
        cat_table <- table(values$data[[input$cat_var_summary]])
        print(cat_table)
      })
    } else if (input$summary_type == "Numeric Summaries") {
      output$num_summary <- renderPrint({
        req(input$num_var_summary)
        if (input$group_var == "None") {
          summary(values$data[[input$num_var_summary]])
        } else {
          aggregate(values$data[[input$num_var_summary]], 
                    by = list(values$data[[input$group_var]]), summary)
        }
      })
    } else if (input$summary_type == "Plots") {
      output$plot_ui <- renderUI({
        tagList(
          selectInput("x_var", "X-axis Variable:", choices = names(values$data)),
          selectInput("y_var", "Y-axis Variable:", choices = names(values$data)),
          selectInput("color_var", "Color By:", choices = c("None", names(values$data)))
        )
      })
      
      output$plot_output <- renderPlot({
        req(input$plot_type, input$x_var, input$y_var)
        p <- ggplot(values$data, aes_string(x = input$x_var, y = input$y_var))
        if (input$color_var != "None") {
          p <- p + aes_string(color = input$color_var)
        }
        if (input$plot_type == "Scatter Plot") {
          p <- p + geom_point()
        } else if (input$plot_type == "Box Plot") {
          p <- p + geom_boxplot()
        } else if (input$plot_type == "Heatmap") {
          p <- ggplot(values$data, aes_string(x = input$x_var, y = input$y_var, fill = input$color_var)) +
            geom_tile()
        }
        p
      })
    }
  })
  
  output$summaryText <- renderText({
    paste("Data last updated on:", Sys.Date())
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)
