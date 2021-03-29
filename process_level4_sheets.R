## ---------------------------
##
##
##
## Purpose of script: Pre-processing of data for Shiny app
##
## Author: Diarmuid Lloyd
##
## Date Created: 2021-03-27
##
## Email: diarmuid.lloyd@gmail.com
##
## ---------------------------
##
## Notes:
##   https://parliament.scot/chamber-and-committees/research-prepared-for-parliament/financial-scrutiny?qry=budget%202021
## is a good place to get data
##
## ---------------------------


## Packages
library(tidyverse)
library(tidyxl)
library(readODS)
library(ggiraph)
library(ggiraphExtra)
library(plotly)

#Read in data, which has been marginally pre-processed
#These are historic outturn data
df_outturn <-
  readODS::read_ods("data/level2_timeseries.ods",
                    sheet = "outturn",
                    na = c("-", "n/a")) %>%
  filter(!str_detect(level2, "Total")) %>% # removing the total lines
  pivot_longer(
    cols = starts_with("Outturn"),         # picking the year from columns
    names_prefix = "Outturn_",
    names_to = "year",
    values_to = "outturn"
  ) %>%
  mutate(year = as.numeric(year))



# These are the latest budget figures
df_budget <-
  readODS::read_ods("data/level2_timeseries.ods", 
                    sheet = "budget_2021_level3",
                    na = c("-", "n/a"))


totals <- df_budget %>%
  group_by(Portfolio) %>%
  summarise(budget21 = sum(`2021-22 - £m (cash)`))



# Save to rds
save.image("data/app_data.rdata")

# # Testing plots
# fig <- totals %>% plot_ly(values = ~ budget21, labels = ~ Portfolio)
# fig <- fig %>% add_pie(hole = 0.6)
# fig <-
#   fig %>% layout(
#     title = "Donut charts using Plotly",
#     showlegend = F,
#     xaxis = list(
#       showgrid = FALSE,
#       zeroline = FALSE,
#       showticklabels = FALSE
#     ),
#     yaxis = list(
#       showgrid = FALSE,
#       zeroline = FALSE,
#       showticklabels = FALSE
#     )
#   )
# 
# fig
