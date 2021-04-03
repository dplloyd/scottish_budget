#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)# Required to run any Shiny app
library(ggplot2)  # For creating pretty plots
library(dplyr)  # For filtering and manipulating data
library(formattable) # For pretty table formatting
library(sunburstR)

# Loading data ----
load("data/app_data.rdata")

# ui.R ----
ui <- dashboardPage(
  
  dashboardHeader(title = "Demo Budget Dashboard with minimal formatting", titleWidth = 450),
  
  
  dashboardSidebar( 
    sidebarMenu(
      menuItem("2021-22 Budget", tabName = "2021-22_budget", icon = icon("coins")),
      menuItem("Outturn", icon = icon("receipt"), tabName = "widgets",
               badgeLabel = "new", badgeColor = "green")
    )
   
  ) ,
  dashboardBody(
    fluidRow(
      column(width = 3,
    h1("Portfolio selection") ,
    selectInput(
      "portfolio",
      label = "Select a portfolio",
      choices = c("All", unique(df_budget$Portfolio)),
      selected = "All"
    ),
    
    selectInput(
      "year_select",
      label = "Select a year",
      choices = c("2018-19", "2019-20", "2020-21", "2021-22"),
      selected = "2021-22"
    )
    ),
    
    column(width = 9,
    h4("Example: A table showing key Level 3 data for the selected Portfolio. The table is scrollable and sortable."),
    
    DT::dataTableOutput("selected_port"))
    
    
  
)
))

server <- function(input, output) {
  output$selected_port <- DT::renderDataTable({
    
    if (input$portfolio != "All") {
      df_budget <-  df_budget %>% filter(Portfolio == input$portfolio)
    }
    
    #plot table
    #First we make some edits to volumns names and rounding
   df_budget <-  df_budget %>% select(Portfolio, `Level 3` = level3, everything()) %>%
      mutate(`Cash terms change - %` = 100 * `Cash terms change - %`) %>%
      mutate_if(is.numeric , ~ round(., 2))  
   
   #Then we use formattable to add some nice formatting
   df_budget <- 
     df_budget %>% 
     mutate(dummy_pc_change = `Cash terms change - %` ) %>% 
     formattable(list(dummy_pc_change = formatter("span", style = ~style  (
       color =
         ifelse(`Cash terms change - %` < 0 ,'purple',ifelse(`Cash terms change - %` > 0,'orange','darkgray'))
       
         ),
       
       ~icontext(ifelse(`Cash terms change - %` > 0, "arrow-up", ifelse(`Cash terms change - %` < 0,"arrow-down","minus")))
     )))
   
   
   # Finally we convert to a datatable and display
   df_budget %>% formattable::as.datatable(
     rownames = FALSE,
     options = list(
       pageLength = 5,
       paging = FALSE,
       scrollInfinite = TRUE,
       scrollY = '300px',
       scrollCollapse = TRUE
     ) )
    
    
  })
  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
