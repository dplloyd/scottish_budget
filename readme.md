#  Summary

This dashboard is a sandpit for testing different visualisations. All data are 
publically available at https://parliament.scot/chamber-and-committees/research-prepared-for-parliament/financial-scrutiny?qry=budget%202021.

The dashboard is not formatted. It purpose is to test proof of principles, not final polished products. Dashboard built in R using:

```
library(shiny) # to build Shiny app
library(shinydashboard)# Required to run any Shiny app
library(ggplot2)  # For creating pretty plots
library(dplyr)  # For filtering and manipulating data
library(formattable) # For pretty table formatting
library(sunburstR) # for sunburst plot
library(plotly) # Interactive plots

```

A full app development would allow near complete control over appearance, layout, etc.

All information in this site is provided "as is", with no guarantee of completeness or accuracy - there are defintely errors in the processing, that the data should be treated as dummy set.

Diarmuid Lloyd, April 2021