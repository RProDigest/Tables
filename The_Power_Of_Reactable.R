#################################################################
# We scrape the EPL table from BBC and make a better one :)
# Showcasing the power of {reactable}
# Written by @RProDigest 
# 25th Dec 2023
###############################################################


#### Load libraries ##################

library(rvest)
library(tidyverse)
library(reactablefmtr)
library(magrittr)

###################################

# bbc link with EPL table

url_data <- "https://www.bbc.com/sport/football/tables"


##### Pipeline to scrape EPL table from the URL-----------------

# Read the HTML table and extract the first table

epl_table <- url_data |> 
  read_html() |> 
  html_table() |> 
  pluck(1)

# Rename the first two columns
names(epl_table)[1:2] <- c("Position", "Movement")

# Continue the pipeline with the renamed dataframe
epl_table |> 
  select(-2) |>  # Skip the second column
  slice(-21:-22) |> 
  select(1:10)-> table_epl  # Select rows and assign to table_epl


##### glimpse the data

table_epl |> glimpse() # we need to convert the columns to numeric

columns_to_convert <- c("P", "W", "D", "L", "F", "A", "GD", "Pts")

table_epl[columns_to_convert] <- map(table_epl[columns_to_convert], as.numeric)



### load logos

table_epl <- table_epl |> 
  mutate(
    img = case_when(
      Team  == "Arsenal" ~ "https://static.files.bbci.co.uk/core/website/assets/static/sport/football/arsenal.4af17eb85b7fc9be53ab.svg",
      Team == "Aston Villa" ~ "https://static.files.bbci.co.uk/core/website/assets/static/sport/football/aston-villa.43cde3ac1bd038be5942.svg",
      Team == "Liverpool" ~ "https://static.files.bbci.co.uk/core/website/assets/static/sport/football/liverpool.b35ee11689baed640716.svg",
      Team == "Tottenham" ~ "https://static.files.bbci.co.uk/core/website/assets/static/sport/football/tottenham-hotspur.1f8909bcef81af02b62e.svg",
      Team == "Man City" ~ "https://static.files.bbci.co.uk/core/website/assets/static/sport/football/manchester-city.e911d0774bd913c49384.svg",
      Team == "Brighton" ~ "https://static.files.bbci.co.uk/core/website/assets/static/sport/football/brighton-and-hove-albion.a039561df69d56a34b36.svg",
      Team == "Crystal Palace" ~ "https://static.files.bbci.co.uk/core/website/assets/static/sport/football/crystal-palace.83d2226c03245f1e92f1.svg",
      Team == "Burnley" ~ "https://static.files.bbci.co.uk/core/website/assets/static/sport/football/burnley.3f9aea23bfc6eb60b286.svg",
      Team == "West Ham" ~ "https://static.files.bbci.co.uk/core/website/assets/static/sport/football/west-ham-united.dba0057491d17b4d4921.svg",
      Team == "Bournemouth" ~ "https://static.files.bbci.co.uk/core/website/assets/static/sport/football/afc-bournemouth.61e599a34dcaafc153b2.svg",
      Team == "Man Utd" ~ "https://static.files.bbci.co.uk/core/website/assets/static/sport/football/manchester-united.908cc8bf40ad24e4c2ce.svg",
      Team == "Everton" ~  "https://static.files.bbci.co.uk/core/website/assets/static/sport/football/everton.4b0ea9968a1c7e6e7df7.svg",
      Team == "Sheff Utd" ~ "https://static.files.bbci.co.uk/core/website/assets/static/sport/football/sheffield-united.60acd68198a60f8d5e23.svg",
      Team == "Nottm Forest" ~ "https://static.files.bbci.co.uk/core/website/assets/static/sport/football/nottingham-forest.7918c989bcd8df9aa2a5.svg",
      Team == "Newcastle" ~ "https://static.files.bbci.co.uk/core/website/assets/static/sport/football/newcastle-united.54448d160d111f3c4dd4.svg",
      Team == "Brighton" ~ "https://static.files.bbci.co.uk/core/website/assets/static/sport/football/brighton-and-hove-albion.a039561df69d56a34b36.svg",
      Team == "Chelsea" ~ "https://static.files.bbci.co.uk/core/website/assets/static/sport/football/chelsea.161d2855f0589332c227.svg",
      Team == "Wolves" ~ "https://static.files.bbci.co.uk/core/website/assets/static/sport/football/wolverhampton-wanderers.cdf188d2f7ec28f7f055.svg",
      Team == "Fulham" ~ "https://static.files.bbci.co.uk/core/website/assets/static/sport/football/fulham.adeaafdcbe1b79290ee8.svg",
      Team == "Luton" ~ "https://static.files.bbci.co.uk/core/website/assets/static/sport/football/luton-town.59cb5518cc384917243a.svg",
      Team == "Brentford" ~ "https://static.files.bbci.co.uk/core/website/assets/static/sport/football/brentford.4485875ef230512fde8b.svg",
      TRUE ~ "NA"
    )
  ) |>
  relocate(img, .before = P)

##### Create fancy table using {reactable} package #############

table_epl |> 
  reactable(
    theme = fivethirtyeight(centered = TRUE, header_font_size = 16),
    defaultPageSize = 22,
    sortable = TRUE,
    compact = TRUE,
    columns = list(
    #  Position =colDef(footer = "Source:https://www.bbc.com/sport/football/tables"),
      img   = colDef(name = "Logo", cell = embed_img()),
      W     = colDef(style = color_scales(table_epl,colors = c("#8b0000", "white", "#013220"))),
      D     = colDef(style = color_scales(table_epl,colors = c("#013220", "white", "#8b0000"))),
      L     = colDef(style = color_scales(table_epl,colors = c("#013220", "white", "#8b0000"))),
      F     = colDef(style = color_scales(table_epl,colors = c("#8b0000", "white", "#013220"))),
      A     = colDef(style = color_scales(table_epl,colors = c("#013220", "white", "#8b0000"))),
      GD    = colDef(style = color_scales(table_epl,colors = c("#8b0000", "white", "#013220"))),
      Pts   = colDef(cell = data_bars(table_epl,fill_color = c("#8b0000", "white", "#013220")))
      ),
      
    columnGroups = list(
        colGroup(
        name = "Win Draw and Lose", 
        columns = c("W", "D", "L")
              )
        )
      
    ) 
  

