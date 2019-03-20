#
# read the Pantone color charts from the web
#
# BvH, 12-04-2018
#

library(tidyverse)
library(rvest)

###############################################################################
# read the Pantone color charts from the web
###############################################################################
url <- "http://www.eagle-web-designs.com/cool_stuff/ColorChart.html"

pantone <- url %>%
  read_html(., 
            encoding = "iso-8859-1") %>%
  html_nodes("table") %>%
  html_table()

pantone.1 <-
  pantone  %>%
  as.data.frame() %>%
  dplyr::select(-X6) %>%
  dplyr::select(PMS = 1,
                Red = 2,
                Green = 3, 
                Blue = 4,
                HEX = 5) %>%
  dplyr::slice(2:n())

###############################################################################
# read the Pantone color charts from the web
###############################################################################
url <- "https://www.labelpartners.com/pantone_coated_table.html"

pantone <- url %>%
  read_html(., 
            encoding = "iso-8859-1") %>%
  html_nodes("table") %>%
  html_table()

pantone.2 <-
  pantone  %>%
  as.data.frame() %>%
  dplyr::select(PMS = 1,
                Red = 3,
                Green = 4, 
                Blue = 5,
                HEX = 2) %>%
  dplyr::slice(2:n())


###############################################################################
# store pantone tables
###############################################################################
save(pantone.1, 
     pantone.2,
     file = "./data/cleaned/pantone_table.RData")
