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
     
      fluidRow(
        h2(
          "Example: Bar charts showing outturn and Level 2 line details. Selectable using input menu."
        ),
        column((width = 4),
               h3("A chart"),
               plotlyOutput("outturn_plot")),
        
        column(width = 8,
               h3("A table"),
               
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



server <- function(input, output) {
  output$selected_port <- renderDataTable({
    if (input$portfolio != "All") {
      df_budget <-  df_budget %>% filter(Portfolio == input$portfolio)
    }
    
    #plot table
    #First we make some edits to columns names and rounding
    df_budget <-
      df_budget %>% select(Portfolio, `Level 3` = level3, everything()) %>%
      mutate(`Cash terms change - %` = 100 * `Cash terms change - %`) %>%
      mutate_if(is.numeric , ~ round(., 2))
    
    #Then we use formattable to add some nice formatting
    df_budget <-
      df_budget %>%
      mutate(dummy_pc_change = `Cash terms change - %`) %>%
      formattable(list(
        dummy_pc_change = formatter(
          "span",
          style = ~ formattable::style  (color =
                                           ifelse(
                                             `Cash terms change - %` < 0 ,
                                             'darkgray',
                                             ifelse(`Cash terms change - %` > 0, 'blue', 'darkgray')
                                           )),
          
          ~ formattable::icontext(ifelse(
            `Cash terms change - %` > 0,
            "arrow-up",
            ifelse(`Cash terms change - %` < 0, "arrow-down", "minus")
          ))
        )
      )) 
    
    
    # Finally we convert to a datatable and display
    df_budget %>% formattable::as.datatable(
      rownames = FALSE,
      class = 'row-border compact',
      colnames = c(
        "Portfolio",
        "Level 3",
        "2020-21 (£m)",
        "2021-22 (£m)",
        "Cash terms change (£m)",
        "Cash terms change (%)",
        ""
      ) ,
      options = list(
        pageLength = 5,
        paging = FALSE,
        scrollInfinite = TRUE,
        scrollY = '300px',
        scrollCollapse = TRUE
        
      )
    )
    
    
  })
  #options(shiny.trace = TRUE)
  output$s2b <- renderSund2b({
    toplot <-
      level4data %>% select(portfolio, level2, level3, level4, scottish_budget_2021) %>%
      mutate(scottish_budget_2021 = abs(scottish_budget_2021))
    
    if (input$portfolio != "All") {
      toplot <- toplot %>% filter(portfolio == input$portfolio)
    }
    
    test1 = toplot %>% mutate(
      v1 = paste(
        toplot$portfolio,
        toplot$level2,
        toplot$level3,
        toplot$level4,
        sep = "-"
      ),
      v2 = scottish_budget_2021
    ) %>%
      select(v1, v2)
    
    s2b <- sund2b(test1)
    s2b
    
    add_shiny(s2b)
  })
  
  output$outturn_table <- DT::renderDataTable({
    if (input$portfolio != "All") {
      outturn_toplot <- df_outturn %>%
        filter(portfolio == input$portfolio)
    }
    else{
      outturn_toplot <- df_outturn
    }
    
    
    outturn_toplot %>%
      select(portfolio, level2, year, outturn, measure) %>%
      pivot_wider(names_from = year, values_from = outturn) %>%
      DT::datatable(
        rownames = FALSE,
        options = list(
          pageLength = 5,
          paging = FALSE,
          scrollInfinite = TRUE,
          scrollY = '300px',
          scrollCollapse = TRUE
        )
      )
    
    
  })
  
  
  output$outturn_plot <- renderPlotly({
    if (input$portfolio != "All") {
      outturn_toplot <- df_outturn %>%
        filter(portfolio == input$portfolio)
    }
    else{
      outturn_toplot <- df_outturn
    }
    

    (
      outturn_toplot %>%
        group_by(year, measure) %>%
        summarise(total_outturn = sum(outturn, na.rm = TRUE)) %>%
        ggplot() +
        geom_col(
          aes(
            x = as.factor(year),
            y = total_outturn,
            fill = measure
          ),
          position = position_dodge()
        ) +
        theme(
          panel.background = element_rect(fill = "transparent"),
          # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA),
          # bg of the plot
          panel.grid.major = element_blank(),
          # get rid of major grid
          panel.grid.minor = element_blank(),
          # get rid of minor grid
          legend.background = element_rect(fill = "transparent"),
          # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
        )
    ) %>%
      ggplotly()
    
  })
  
  
  output$subheader <- renderUI({
    tagList(
      "This site is a sandbox demo of some Shiny data visualisations, using published Scottish Budget data"
    )
  })
  
  
  output$treemap <- plotly::renderPlotly({
    
    df_f <- shape_for_treemap(level4data)
    fig <- plot_ly(
      type = "treemap",
      ids = df_f$ids,
      labels = df_f$labels,
      parents = df_f$parents,
      values = df_f$scottish_budget_2021,
      textinfo = "label+value",
      #domain = list(column = 1),
      maxdepth = 5,
      tiling = list(packing = "binary"))
    
    fig
  })

}


# Run the app ----
shinyApp(ui = ui, server = server)
