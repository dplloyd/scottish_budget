#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(sparkline)


server <- function(input, output) {
    output$selected_port <- renderDataTable({
        if (input$portfolio != "All") {
            df_budget <-  df_budget %>% filter(Portfolio == input$portfolio)
        }
        
        level3_g <- 
        level4data %>%
            dplyr::select(portfolio, level3, fiscal_resource_2020, fiscal_resource_2021) %>%
            dplyr::group_by(portfolio, level3) %>%
            summarise(
                fiscal2020 = sum(fiscal_resource_2020),
                fiscal2021 = sum(fiscal_resource_2021)
            ) %>% 
            mutate(change_abs = fiscal2021 - fiscal2020,
                   change_pc =  scales::percent( (fiscal2021/fiscal2020)-1)) %>% 
        
        #plot table
        #First we make some edits to columns names and rounding
     
            mutate_if(is.numeric , ~ round(., 2))
        
        #Then we use formattable to add some nice formatting
        level3_g <-
            level3_g %>%
            formattable(list(
                dummy_pc_change = formatter(
                    "span",
                    style = ~ formattable::style  (color =
                                                       ifelse(
                                                           fiscal2021 < fiscal2020 ,
                                                           'darkgray',
                                                           ifelse(fiscal2021 > fiscal2020, 'blue', 'darkgray')
                                                       )),
                    
                    ~ formattable::icontext(ifelse(
                        fiscal2020 < fiscal2021  ,
                        "arrow-up",
                        ifelse(fiscal2020 > fiscal2021 , "arrow-down", "minus")
                    ))
                )
            )) 
        
        
        # Finally we convert to a datatable and display
        level3_g %>% formattable::as.datatable(
            rownames = FALSE,
            class = 'row-border compact',
            colnames = c(
                "Portfolio",
                "Level 3",
                "2020-21 resource (£m)",
                "2021-22 resource (£m)",
                "Cash terms change (£m)",
                "Cash terms change (%)"
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
            level4data %>% select(portfolio, level2, level3, level4, fiscal_resource_2021) %>%
            mutate(fiscal_resource_2021 = abs(fiscal_resource_2021))
        
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
            v2 = fiscal_resource_2021
        ) %>%
            select(v1, v2)
        
        s2b <- sund2b(test1)
        s2b
        
        add_shiny(s2b)
    })
    
    output$outturn_table <- DT::renderDataTable({
        if (input$portfolio_outturn != "All") {
            outturn_toplot <- df_outturn %>%
                filter(portfolio == input$portfolio_outturn)
        }
        else{
            outturn_toplot <- df_outturn
        }
        
        #sparkline data
        
        
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
        if (input$portfolio_outturn != "All") {
            outturn_toplot <- df_outturn %>%
                filter(portfolio == input$portfolio_outturn)
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
                ) + scale_fill_manual(values = c("#0065bd","#727272"))
        )  %>% 
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
            values = df_f$fiscal_resource_2021,
            textinfo = "label+value",
            #domain = list(column = 1),
            maxdepth = 5
           # tiling = list(packing = "binary")
           )
        
        fig
    })
    
}
