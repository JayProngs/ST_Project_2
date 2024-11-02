# Seoul Bike Sharing Data Explorer

Welcome to the Seoul Bike Sharing Data Explorer! This Shiny web application lets you dive into the Seoul Bike Sharing dataset. It allows user to subset and download the data. Also user can analyze and visualize the data to get more insights.

## Project Overview
This app provides an interactive and user-friendly way to explore patterns in bike-sharing demand across Seoul. This app is built with R and Shiny framework. It offer option to filter, download and visualize the dataset.
The application can be accessed [here](https://jsthakur.shinyapps.io/Seoul-Bike-Sharing-Data-Explorer/)

## Features

The application has into three main tabs:

#### 1. About Tab
Purpose: Introduces the app and summarizes its features.

Data Source: Provides a brief description and a link to the Kaggle dataset used.

Instructions: Guides you on how to use the sidebar for data subsetting and explains each tab's functionality.

Visuals: Displays an image relevant to the dataset for added context.

#### 2. Data Download Tab
Data View: Shows the current subset of data in an interactive table.

Data Export: Allows you to download the filtered data as a CSV file.

#### 3. Data Exploration Tab
Summaries & Visualization: Tools for analyzing and visualizing data.

Categorical Summaries: Displays counts and summaries for selected categorical variables.

Numeric Summaries: Calculates summary statistics (mean, median, quartiles, etc.) for numeric variables, optionally grouped by a selected categorical variable.

Plots: Generates various plots like scatter plots, histograms, density plots, and heatmaps. You can select variables for the x-axis, y-axis, and color to observe relationships and trends within the data.

## Dataset:
The dataset referred can be found [here](https://www.kaggle.com/datasets/saurabhshahane/seoul-bike-sharing-demand-prediction)

## User Guide

The sidebar on the left side of the app allows users to subset the data based on:

1. Categorical Variables: Two categorical variables (e.g., Seasons, Holiday) can be selected for filtering.

2. Numeric Variables: Two numeric variables (e.g., Temperature, Humidity) with adjustable sliders.

After choosing the filering options, the Update Data button applies the filters and refreshes the main panel.

## Setup and Deployment

To run the app locally:

1. Clone this repository.
2. Install required packages: install.packages(c("shiny", "dplyr", "ggplot2", "DT", "readr"))
3. Open the app.R file in RStudio.
4. Run the application by clicking Run App.

## License:
This project is licensed under the MIT License. Please refer to the LICENSE file for details.
