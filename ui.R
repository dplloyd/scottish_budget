#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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
library(plotly)
library(DT)
library(tidyverse)
source("R/shape_for_treemap.R")

# Loading data ----
load("data/app_data.rdata")

# ui.R ----
ui <- navbarPage(
    position = "fixed-top",
    
    tags$head(includeCSS("www/style.css")),
    
    windowTitle = "Scottish Budget",
    navbarMenu("2021-22 Budget", icon = icon("coins"),
               tabPanel(
                   "Sunburst", icon = icon("sun"),
                   
                   fluidRow(column(1),
                            column(5,
                                   h1("Budget lines"),
                                   uiOutput("subheader")),
                            column(4, selectInput(
                                "portfolio",
                                label = "Select a portfolio",
                                choices = c("All", unique(df_budget$Portfolio)),
                                selected = "All"
                            ),),
                            column(1, #align = "right",
                                   style = "margin-top: 25px;",
                                   tags$a(img(src = "bilbo.png",
                                              height = 60,
                                              alt = "Bilbo"), 
                                          href = "https://www.google.co.uk")),
                            column(1)),
                   hr(),
                   
                   fluidRow(column(1),
                            column(5,
                                   h4("A sunburst plot shows the hierarchy of Level 2,3 and 4 lines within Portfolios."),
                                   sund2bOutput("s2b")
                            ),
                            column(6,
                                   h4(
                                       "We can embed tables with additional information, which react to user input. For example, here we show all Level 3 lines, which you can search, scroll and filter."
                                   ),
                                   
                                   dataTableOutput("selected_port")
                            )),
                   
                   
               ),
               
               tabPanel("Treemap",icon = icon("th"),
                        fluidRow(column(1),
                                 column(9,
                                        h4("A treemap plot also shows the hierarchy of Level 2,3 and 4 lines within Portfolios. There will be some distortion of shape sizes, but this can be adjusted. In this example, the size of the boxes are roughly in scale to each other."),
                                        hr(),
                                        h4("Clicking on a box provides more detail."),
                                        plotly::plotlyOutput("treemap")
                                 )
                                 
                                 
                        )
               ),
               
               tabPanel("Circle packing",icon = icon("circle"),
                        h3("To add a circle packing chart here- yet another way of visualising heirarchical data.")
                        
               )
               
               
    ),
    
    tabPanel(
        "Outturn",
        icon = icon("receipt"),
        
        fluidRow( column(1),
                  column(3,
            h4(
                "Example: Bar charts showing outturn and Level 2 line details. Selectable using input menu."
            ),
            
            selectInput(
                "portfolio_outturn",
                label = "Select a portfolio",
                choices = c("All", unique(df_budget$Portfolio)),
                selected = "All")
            ),
          column(6,
                   h4("A chart showing selected portfolio outturn."),
                   plotlyOutput("outturn_plot")),
          ),
        fluidRow( column(1),
            column(width = 9,
                   h4("A table showing selected portfolio Level 2 details."),
                   
                   dataTableOutput("outturn_table"))
            
            )
    ),
   
    tabPanel(
        "Notes",
        icon = icon("book-reader"),
        tabItem(tabName = "readme",
                column(1),
                column(4,
                       includeMarkdown("readme.md"))
                
        )
    )
    
    
)

