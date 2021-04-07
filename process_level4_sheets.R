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
##  for data
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
  mutate(year = as.numeric(year),
         outturn = round(outturn,2))



# These are the latest budget figures
df_budget <-
  readODS::read_ods("data/level2_timeseries.ods", 
                    sheet = "budget_2021_level3",
                    na = c("-", "n/a"))


totals <- df_budget %>%
  group_by(Portfolio) %>%
  summarise(budget21 = sum(`2021-22 - £m (cash)`))



#These are the level 4 lines, nested with Level 1,2 and 3 lines
# Get list of all sheet names - the data are in all but contents sheet, so pick last 11.
sheets <- readODS::list_ods_sheets("data/Budget Level 4 2021 to 2022.ods") %>% 
  tail(11)


level4data = purrr::map_df(
  sheets,
.f = function(x)
    readODS::read_ods(
      "data/Budget Level 4 2021 to 2022.ods",
      sheet = x,col_names = FALSE
    )
)

variable_names <-  c("tmp",
                   "portfolio",
                   "level2",
                   "level3",
                   "level4",
                   "level4_alt",
                   "fiscal_resource_2020",
                   "non_cash_2020"	,
                   "capital_2020"	,
                   "financial_transactions_2020",
                   "UK_funded_AME_2020",
                   "scottish_budget_2020"	,
                   "fiscal_resource_2021",
                   "non_cash_2021"	,
                   "capital_2021",
                   "financial_transactions_2021"	,
                   "UK_funded_AME_2021",
                   "scottish_budget_2021",
                   "percentage_change",
                   "what_it_buys",
                   "change_explanation"
)
colnames(level4data) <- variable_names
  

#Now tidy up
level4data <- level4data %>% select(-tmp)  #delete first column
level4data <- level4data[!level4data$fiscal_resource_2020=="fiscal_resource_2020", ] # column names got mixed in with the data
level4data <- level4data[!is.na(level4data$level4) | (!is.na(level4data$portfolio)),]
level4data <- level4data %>% filter(!is.na(level4))


# Check data types in columns. We see there are characters where we want numbers
level4data %>% map_chr(class)

#converting character columns to numeric where needed
level4data <- level4data %>% mutate_at(vars(ends_with("2020")),as.numeric) %>% 
  mutate_at(vars(ends_with("2021")),as.numeric) %>% 
  mutate(percentage_change = as.numeric(percentage_change)) %>% 
  select(-level4_alt)

level4data %>% map_chr(class)

# dashes are used as delimiter character in the sunburst plot,
# so where they appear in hierarchy entry, they split to another"
# level. So, replace with colons.
level4data <-
  level4data %>% mutate(
    level2  = str_replace(level2, "-", ":"),
    level3  = str_replace(level3, "-", ":"),
    level4  = str_replace(level4, "-", ":"),
  )

# Save to rds
save.image("data/app_data.rdata")

