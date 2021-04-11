# load necessary libraries
library(tidyverse)
library(plotly)
library(RColorBrewer)

load('data/app_data.rdata')


shape_for_treemap <- function(level4data){
# Create subtotals for each hierarchy level.

level4data <- level4data %>%
  mutate(
    scottish_budget_2021 = abs(scottish_budget_2021),
    level2  = str_replace(level2, "/", " or "),
    level3  = str_replace(level3, "/", " or "),
    level4  = str_replace(level4, "/", " or "),
  )


treemap_df <- bind_rows(
  level4data %>%
    group_by(portfolio) %>%
    summarise(scottish_budget_2021 = sum(scottish_budget_2021)),
  level4data %>%
    group_by(portfolio, level2) %>%
    summarise(scottish_budget_2021 = sum(scottish_budget_2021)),
  level4data %>%
    group_by(portfolio, level2, level3) %>%
    summarise(scottish_budget_2021 = sum(scottish_budget_2021)),
  level4data %>%
    group_by(portfolio, level2, level3, level4) %>%
    summarise(scottish_budget_2021 = sum(scottish_budget_2021)),
) %>%
  bind_rows(level4data %>% summarise(scottish_budget_2021 =  sum(scottish_budget_2021))) %>%
  mutate(root = "Scottish Budget") %>%
  arrange(root, portfolio, level2, level3)

# Level 2,3 and 4 can't share names within the same tree, so check for those cases and
# append name as required.

treemap_df <- treemap_df %>%
  mutate(
    level4  = if_else(level4 == level3, paste(level4, " (lv 4, ", portfolio, ")"), level4),
    level3 = if_else(level3 == level2 , paste(level3, " (lv 3, ", portfolio, ")"), level3)
  )



shape_for_treemap <- treemap_df %>%
  mutate(ids = case_when(
    !is.na(level4) ~ paste0(level3, "-", level4),
    (is.na(level4) & !is.na(level3)) ~ paste0(level2, "-", level3),
    (is.na(level3) & !is.na(level2)) ~ paste0(portfolio, "-", level2),
    (is.na(level2) &
       !is.na(portfolio)) ~ paste0(root, "-", portfolio),
    TRUE ~ root
  )) %>%
  mutate(labels = case_when(
    !is.na(level4) ~ level4,
    (is.na(level4) & !is.na(level3)) ~ level3,
    (is.na(level3) & !is.na(level2)) ~ level2,
    (is.na(level2) & !is.na(portfolio)) ~ portfolio,
    TRUE ~ root
  )) %>%
  mutate(
    parents = case_when(
      labels == root ~ "",
      labels == portfolio ~ root,
      labels == level2 ~ paste0(root, "-", portfolio),
      labels == level3 ~ paste0(portfolio, "-", level2),
      labels == level4 ~ paste0(level2, "-", level3)
    )
  )

}


