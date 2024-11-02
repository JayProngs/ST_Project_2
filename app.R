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
df <- df[-1, ]  # Remove the first row
colnames(df) <- c("Date", "Rented_Bike_Count", "Hour", "Temperature", "Humidity", 
                  "Wind_speed", "Visibility", "Dew_point_temperature", "Solar_Radiation",
                  "Rainfall", "Snowfall", "Seasons", "Holiday", "Functioning_Day")
df <- df |>
  mutate(across(c(Seasons, Holiday, Functioning_Day), as.factor))

# Convert relevant columns to numeric
df <- df |>
  mutate(across(c(Rented_Bike_Count, Hour, Temperature, Humidity, Wind_speed,
                  Visibility, Dew_point_temperature, Solar_Radiation, 
                  Rainfall, Snowfall), as.numeric))
df <- df |>
  mutate(Date = as.Date(Date, format = "%d/%m/%Y"))


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
                    choices = c("Date", "Temperature", "Humidity", "Wind_speed",
                                "Visibility", "Solar_Radiation", "Rainfall", "Snowfall"),
                    selected = "Date"),
        uiOutput("num_var1_slider"),
        
        # Widget to select second numeric variable for subsetting
        selectInput("num_var2", "Select Numeric Variable 2:",
                    choices = c("Date","Temperature", "Humidity", "Wind_speed",
                                "Visibility", "Solar_Radiation", "Rainfall", "Snowfall"),
                    selected = "Humidity"),
        uiOutput("num_var2_slider"),
        
        # Action button to subset data
        actionButton("update_button", "Update Data"),
        
        # Instructions
        helpText("Select variables and click 'Update Data' to apply subsetting. Use 'All of the above' to include all levels of a categorical variable.")
      ),


        mainPanel(
          tabsetPanel(
            # First tab for about
            tabPanel("About", 
                     h3("About This App"),
                     p("This app allows users to explore the Seoul Bike Sharing Demand dataset. This dataset is referred from Kaggle."),
                     p("Use the sidebar to subset the data and explore various summaries and plots. Sidebar allows user to select upto Two categorical variables and 2 Numeric Variables."),
                     p("Data Source: ",
                       a("Kaggle Dataset", href = "https://www.kaggle.com/datasets/saurabhshahane/seoul-bike-sharing-demand-prediction", target = "_blank")),
                     img(src = "https://storage.googleapis.com/kaggle-datasets-images/1182486/1978532/347f28c40107cd8c896fb8e1230fcc87/dataset-cover.jpg?t=2021-02-26-16-11-12", height = "300px", width="500px"),
                     p("The app contains the following tabs:"),
                     tags$ul(
                       tags$li(strong("About:"), " Information about how to use the app, data and where data is taken from."),
                       tags$li(strong("Data Download:"), " This tab shows user the data which is filtered using Sidebar and allows user to download that data in CSV format."),
                       tags$li(strong("Data Exploration:"), " This tab allows user to generate Categorical summary (count), Numeric Variable summary (Mean, Median, 1st and 3rd Quartile, Minimum and Maximum) with or without grouping over Categorical variable. It also allows user to generate various plots using different variables.")
                     )
            ),
            
            # second tab for data download
            tabPanel("Data Download",
                     DT::dataTableOutput("data_table"),
                     downloadButton("download_data", "Download Data")
            ),
            
            # Third tab for plot and summaries
            tabPanel("Data Exploration",
                     uiOutput("exploration_ui")
            )
          )
        )
    )
)

# Define server logic
server <- function(input, output, session) {
  
  # store the subsetted data
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
    # changed for date variable
    if (input$num_var1 == "Date") {
      # Render date slider
      sliderInput("num_var1_range", "Select Date Range:",
                  min = min(df$Date, na.rm = TRUE),
                  max = max(df$Date, na.rm = TRUE),
                  value = c(min(df$Date, na.rm = TRUE), max(df$Date, na.rm = TRUE)),
                  timeFormat = "%Y-%m-%d")
    } else {
      # Render numeric slider
      numeric_var <- df[[input$num_var1]]
      sliderInput("num_var1_range", paste("Select Range of", input$num_var1),
                  min = min(numeric_var, na.rm = TRUE),
                  max = max(numeric_var, na.rm = TRUE),
                  value = c(min(numeric_var, na.rm = TRUE), max(numeric_var, na.rm = TRUE)))
    }
  })
  
  # Second numeric variable
  output$num_var2_slider <- renderUI({
    req(input$num_var2)
    # changed for date variable
    if (input$num_var2 == "Date") {
      # Render date slider
      sliderInput("num_var2_range", "Select Date Range:",
                  min = min(df$Date, na.rm = TRUE),
                  max = max(df$Date, na.rm = TRUE),
                  value = c(min(df$Date, na.rm = TRUE), max(df$Date, na.rm = TRUE)),
                  timeFormat = "%Y-%m-%d")
    } else {
      # Render numeric slider
      numeric_var <- df[[input$num_var2]]
      sliderInput("num_var2_range", paste("Select Range of", input$num_var2),
                  min = min(numeric_var, na.rm = TRUE),
                  max = max(numeric_var, na.rm = TRUE),
                  value = c(min(numeric_var, na.rm = TRUE), max(numeric_var, na.rm = TRUE)))
    }
  })
  
  # Subset the data
  observeEvent(input$update_button, {
    # print(paste("Selected Levels for", input$cat_var1, ":", paste(input$cat_var1_level, collapse = ", ")))
    # print(paste("Selected Levels for", input$cat_var2, ":", paste(input$cat_var2_level, collapse = ", ")))
    
    # Filter data based on user inputs
    # changed for date variable
    subset_data <- df |>
      filter(
        (input$cat_var1_level == "All of the above" | .data[[input$cat_var1]] %in% input$cat_var1_level),
        (input$cat_var2_level == "All of the above" | .data[[input$cat_var2]] %in% input$cat_var2_level),
        if (input$num_var1 == "Date") {
          Date >= input$num_var1_range[1] & Date <= input$num_var1_range[2]
        } else {
          .data[[input$num_var1]] >= input$num_var1_range[1] & .data[[input$num_var1]] <= input$num_var1_range[2]
        },
        if (input$num_var2 == "Date") {
          Date >= input$num_var2_range[1] & Date <= input$num_var2_range[2]
        } else {
          .data[[input$num_var2]] >= input$num_var2_range[1] & .data[[input$num_var2]] <= input$num_var2_range[2]
        }
      )
    
    # Update the reactive values with the filtered data
    if (nrow(subset_data) == 0) {
      showNotification("No data available after applying filters. Please adjust your selections.", type = "warning")
      values$data <- NULL
      return(NULL)
    } else {
      values$data <- subset_data
    }
    
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
    # Summary for Cat variables
    # added second cat variable and set as None for Default
    if (input$summary_type == "Categorical Summaries") {
      tagList(
        selectInput("cat_var_summary1", "Select First Categorical Variable:",
                    choices = names(df)[sapply(df, is.factor)], selected = "Seasons"),
        selectInput("cat_var_summary2", "Select Second Categorical Variable:",
                    choices = c("None", names(df)[sapply(df, is.factor)]), selected = "None"),
        verbatimTextOutput("cat_summary")
      )
    } else if (input$summary_type == "Numeric Summaries") {
      # Summary for Numerical variables
      tagList(
        selectInput("num_var_summary", "Select Numeric Variable:",
                    choices = names(df)[sapply(df, is.numeric)]),
        selectInput("group_var", "Group By (Categorical Variable):",
                    choices = c("None", names(df)[sapply(df, is.factor)])),
        verbatimTextOutput("num_summary")
      )
    } else if (input$summary_type == "Plots") {
      # Selecting Plot type
      tagList(
        selectInput("plot_type", "Select Plot Type:",
                    choices = c("Scatter Plot", "Box Plot", "Histogram", "Density Plot",
                                 "Heatmap", "Time Series Plot")),
        uiOutput("plot_ui"),
        plotOutput("plot_output")
      )
    }
  })
  
  
  observe({
    req(input$summary_type == "Categorical Summaries")
    
    output$cat_summary <- renderPrint({
      req(input$cat_var_summary1)
      
      # Check if the user selected a second categorical variable
      if (input$cat_var_summary2 != "None") {
        req(input$cat_var_summary2)
        # Generate a two-way contingency table
        cat_table <- table(values$data[[input$cat_var_summary1]], values$data[[input$cat_var_summary2]])
        print(cat_table)
      } else {
        # Generate a one-way table if only one variable is selected
        cat_table <- table(values$data[[input$cat_var_summary1]])
        print(cat_table)
      }
    })
  })
  
  
  observe({
    req(input$summary_type)
    if (input$summary_type == "Categorical Summaries") {
      output$cat_summary <- renderPrint({
        req(input$cat_var_summary1)
        
        # Check if the user selected a second categorical variable
        if (input$cat_var_summary2 != "None") {
          req(input$cat_var_summary2)
          # Generate a two-way contingency table
          cat_table <- table(values$data[[input$cat_var_summary1]], values$data[[input$cat_var_summary2]])
          print(cat_table)
        } else {
          # Generate a one-way table if only one variable is selected
          cat_table <- table(values$data[[input$cat_var_summary1]])
          print(cat_table)
        }
      })
    } else if (input$summary_type == "Numeric Summaries") {
      # show numeric variable Summary according to group variables selection
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
      # selection of Axis variable and Colour fill variable.
      output$plot_ui <- renderUI({
        tagList(
          selectInput("x_var", "X-axis Variable:", choices = names(values$data)),
          selectInput("y_var", "Y-axis Variable:", choices = names(values$data)),
          selectInput("color_var", "Color By:", choices = c("None", names(values$data)))
        )
      })
      
      output$plot_output <- renderPlot({
        req(input$plot_type, input$x_var)
        
        tryCatch({
          p <- ggplot(values$data, aes_string(x = input$x_var, y = input$y_var))
          
          if (input$plot_type == "Scatter Plot") {
            if ( !(is.numeric(values$data[[input$x_var]])) | !(is.numeric(values$data[[input$x_var]])) ){
              showNotification("X and Y axis Variable ideally should be Numeric", type = "warning")
            }
            p <- p + geom_point()
            
          } else if (input$plot_type == "Box Plot") {
            p <- p + geom_boxplot()
            
          } else if (input$plot_type == "Histogram") {
            if (input$color_var == "None") {
              showNotification("Value of Color By: filter should not be None", type = "error")
              return (NULL)
            }
            showNotification("Y-axis Variable have no impact on Histogram", type = "message")
            p <- ggplot(values$data, aes_string(x = input$x_var, fill = input$color_var)) +
              geom_histogram(bins = 30, color = "black", alpha = 0.7)
            
          } else if (input$plot_type == "Density Plot") {
            # added else to get color fill
            # added Message to show useless Y variable to user
            if (input$color_var == "None") {
              showNotification("Y-axis Variable have no impact on Denesity Plot", type = "message")
              p <- ggplot(values$data, aes_string(x = input$x_var)) +
                geom_density(fill = "lightblue", alpha = 0.7) +
                labs(x = input$x_var, y = "Density")
            } else {
              # added Message to show useless Y variable to user
              showNotification("Y-axis Variable have no impact on Denesity Plot", type = "message")
              p <- ggplot(values$data, aes_string(x = input$x_var, fill = input$color_var)) +
                geom_density(alpha = 0.7) +
                labs(x = input$x_var, y = "Density")
            }
            
          } else if (input$plot_type == "Heatmap") {
            # added Error message to inform user for selection
            if (input$color_var %in% c("None","Date")) {
              showNotification("Select Color by: value other than Date or None", type = "error")
              return(NULL)
            }
            # changed color scale for continous and discrete variables
            if (is.numeric(values$data[[input$color_var]])) {
              p <- ggplot(values$data, aes_string(x = input$x_var, y = input$y_var, fill = input$color_var)) +
                geom_tile() +
                scale_fill_viridis_c()
            } else {
              p <- ggplot(values$data, aes_string(x = input$x_var, y = input$y_var, fill = input$color_var)) +
                geom_tile() +
                scale_fill_viridis_d()
            }
            
          } else if (input$plot_type == "Time Series Plot") {
            # added Error Message to ask user to put correct input.
            if (input$x_var != "Date") {
              showNotification("Please select 'Date' for the X-axis in a Time Series Plot.", type = "error")
              return(NULL)
            }
            
            p <- ggplot(values$data, aes_string(x = "Date", y = input$y_var)) +
              geom_line(color = "blue") +
              labs(x = "Date", y = input$y_var, title = "Time Series Plot")
          }
          
          if (input$color_var != "None" && input$plot_type != "Heatmap") {
            p <- p + aes_string(color = input$color_var)
          }
          
          p + theme_minimal()
          
        }, error = function(e) {
          showNotification("Error while generating Graph Plot.", type = "error")
          return(NULL)
        })
      })
      
      
    }
  })
  
  output$summaryText <- renderText({
    paste("Data last updated on:", Sys.Date())
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
