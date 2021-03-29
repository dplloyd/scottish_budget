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
      h1("Portfolio selection") ,
      selectInput(
        "portfolio",
        label = "Choose a portfolio to filter the table",
        choices = c("All", unique(df_budget$Portfolio)),
        selected = "All"
      ),
    ),
    mainPanel(DT::dataTableOutput("selected_port"))
  )
)
# server.R ----
server <- function(input, output) {
  output$selected_port <- DT::renderDataTable({
    if (input$portfolio != "All") {
      df_budget <-  df_budget %>% filter(Portfolio == input$portfolio)
    }
    
    #plot table
    df_budget %>% select(Portfolio, `Level 3` = level3, everything()) %>%
      mutate(`Cash terms change - %` = 100 * `Cash terms change - %`) %>%
      mutate_if(is.numeric , ~ round(., 2))     %>%
      DT::datatable(rownames = FALSE)
    
  })
  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
