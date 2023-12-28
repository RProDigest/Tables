################################################################
#' Written by RProDigest
#' 28th Dec 2023
#' Goal replicate BBC chart using {reactable}
#' bbc link: https://www.bbc.com/news/world-us-canada-67830918
#' #############################################################


###### LOAD Libraries #####

library(tidyverse)
library(reactablefmtr)
library(scales)



##### CREATE Data frame From BBC Link #####

war_spending_df <- data.frame(
  Country = c(
    'US',
    'Germany',
    'UK',
    'Norway',
    'Denmark',
    'Poland',
    'Netherlands',
    'Canada',
    'Sweden',
    'Finland'
  ),
  Amount = c(46.3, 18.1, 6.9, 3.8, 3.7, 3.2, 2.6, 2.2, 2.1, 1.5) # in billions
) |>
  mutate(
    img = case_when(
      Country  == 'US' ~ "https://www.worldometers.info/img/flags/us-flag.gif",
      Country == "Germany" ~ "https://www.worldometers.info/img/flags/small/tn_gm-flag.gif",
      Country == "UK" ~ "https://www.worldometers.info/img/flags/small/tn_uk-flag.gif",
      Country == "Norway" ~ "https://www.worldometers.info/img/flags/small/tn_no-flag.gif",
      Country == "Denmark" ~ "https://www.worldometers.info/img/flags/da-flag.gif",
      Country == "Poland" ~ "https://www.worldometers.info/img/flags/pl-flag.gif",
      Country == "Netherlands" ~ "https://www.worldometers.info/img/flags/nl-flag.gif",
      Country == "Canada" ~ "https://www.worldometers.info/img/flags/small/tn_ca-flag.gif",
      Country == "Sweden" ~ "https://www.worldometers.info/img/flags/small/tn_sw-flag.gif",
      Country == "Finland" ~ "https://www.worldometers.info/img/flags/small/tn_fi-flag.gif",
      TRUE ~ "NA"
    )
  ) |>
  relocate(img, .before = Amount)


###### Define the maximum value for scaling the bar lengths ##############

max_amount <- max(war_spending_df$Amount)



####### Define custom dollar format function with "bn" suffix
dollar_format_with_bn <- function(x) {
  paste0(dollar_format(
    prefix = "$",
    accuracy = 0.1 ,
    scale = 1
  )(x), "bn")
}


# Custom function to embed images without stretching
embed_img <- function(value) {
  img(src = value, style = "height:auto; max-width:100%; max-height:100%;") # Adjust max-height as needed
}

##### Create reactable table with data bars ##########################

war_spending_df %>%
  reactable(
    .,
    pagination = FALSE,
    theme = void(
      cell_padding = 1.5,
      header_font_size = 0,
      font_color = "#000000"
    ),
    # theme = clean(),
    columns = list(
      Country = colDef(
        maxWidth = 90,
        align = "right",
        vAlign = "center",
        style = list(fontSize = '16px')
      ),
      img   = colDef(
        name = "Logo",
        cell = embed_img,
        maxWidth = 50,
        vAlign = "center"
      ),
      Amount = colDef(
        style = list(borderLeft = "3px solid #1c1c1c", paddingLeft = 0),
        cell = data_bars(
          .,
          fill_color = "#2596be",
          tooltip = TRUE,
          bar_height = 50,
          bold_text = TRUE,
          max_value = max_amount,
          background = "transparent",
          min_value = 0,
          text_size = 18,
          force_outside = c(0, 7),
          number_fmt = dollar_format_with_bn
        )
      )
      
    )
  ) %>%
  
  add_title(font_size = 36,
            "Top donor countries of military aid to Ukraine") %>%
  add_subtitle(
    font_size = 26,
    font_weight = "normal",
    "Commitments made for weapons and equipment, to 31 Oct 2023."
  ) %>%
  add_source(
    font_size = 14,
    #margin = margin(t=10),
    font_color = "#000000",
    margin = reactablefmtr::margin(
      t = 7,
      r = 0,
      b = 0,
      l = 0
    ),
    "Data: Kiel Institute for the World Economy | Visualization: {reactablefmtr}| @RProDigest"
  )



