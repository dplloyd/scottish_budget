#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)  # Required to run any Shiny app
library(ggplot2)  # For creating pretty plots
library(dplyr)  # For filtering and manipulating data
library(agridat)  # The package where the data comes from

# Loading data ----
load("data/app_data.rdata")

# ui.R ----
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  
  titlePanel("2021-22 Budget"),
  
  sidebarLayout(
    sidebarPanel(
      h1("Portfolio explorer") ,
      selectInput("portfolio", 
                  label = "Choose a portfolio to display",
                  choices = c("All",unique(df_budget$Portfolio)),
                  selected = "All"), 
    ),
    mainPanel(
      
      tableOutput("selected_port")
      
 
    )
  )
)
# server.R ----
server <- function(input, output) {
  
  output$selected_port <- renderTable({ 
    df_budget %>% filter(Portfolio == input$portfolio)
  })
  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
