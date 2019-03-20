#
# 
#
#
# BvH, 12-04-2018
#

library(tidyverse)
library(imager)


###############################################################################
# define functions
###############################################################################

# lookup [R] Find the closest value in a list or matrix
# http://grokbase.com/t/r/r-help/087974st4g/r-find-the-closest-value-in-a-list-or-matrix
# 
# x=c(1:100)
# your.number=5.43
# which(abs(x-your.number)==min(abs(x-your.number)))
# 
# https://stackoverflow.com/questions/19957725/r-assign-column-value-based-on-closest-match-in-second-data-frame
# however data.table has a beautiful way of doing this
# 
# setkey(x, z)
# setkey(y, zP)
# y[x, roll=“nearest”]


color.distance <- function(color1, color2) {
  
  return( sqrt( (color1[1] - color2[1])^2 + (color1[2] - color2[2])^2 + (color1[3] - color2[3])^2 ) )
}

getPantoneScale <- function(color, pantone) {
  
  pantone <-
    pantone %>%
    dplyr::mutate(distance = color.distance(color, c(pantone$Red, pantone$Green, pantone$Blue))) %>%
    dplyr::order_by(desc(distance))
  
  
  return( abs(a.r - b.r) + Math.Abs(a.g - b.g) + Math.Abs(a.b - b.b) )
}

# convert RGB input-vector into one numerical value
convertRGBtoNumber <- function(Red, Green, Blue, ...) {
  return(as.integer(Red) * 1000000 +
         as.integer(Green) * 1000 +
         as.integer(Blue) * 1)
  }

###############################################################################
# load prepared Pantone-scale tables
###############################################################################
load(file = "./data/cleaned/pantone_table.RData")

# dev-version based on 
#   https://stackoverflow.com/questions/46531582/using-purrrpmap-within-mutate-to-create-list-column
#
# pantone.1 <-
#   pantone.1 %>%
#   dplyr::mutate(index = purrr::pmap(. , 
#                                     function(Red, Green, Blue, ...){
#                                       return(as.integer(Red) * 1000000 +
#                                              as.integer(Green) * 1000 +
#                                              as.integer(Blue) * 1)
#                                     })
#                 ) %>%
#   tidyr::unnest() %>%
#   dplyr::arrange(index)


pantone.1 <-
  pantone.1 %>%
  dplyr::mutate(index = purrr::pmap(. , 
                                    convertRGBtoNumber)
  ) %>%
  tidyr::unnest() %>%
  dplyr::arrange(index)

pantone.2 <-
  pantone.2 %>%
  dplyr::mutate(index = purrr::pmap(. , 
                                    convertRGBtoNumber)
  ) %>%
  tidyr::unnest() %>%
  dplyr::arrange(index)

###############################################################################
# read images from data directory
###############################################################################
#files <- list.files(path = "./ruby/humanae/",

files <- list.files(path = "./data/raw/",
                    pattern = "*.jpg",
                    full.names = TRUE)

# test: filename <- files[1]
for (filename in files) {

  imager::iminfo(filename)
  humanae.img <- imager::load.image(file = filename)
  
  # some info on the image:
  dim(humanae.img)
  glimpse(humanae.img)
  imager::spectrum(humanae.img)
  
###############################################################################
# read top-left pixel to determine color-value
###############################################################################
  color.topleft <- round(256 * humanae.img[1,1,1,], digits = 0)

###############################################################################
# convert color-value to Pantone-scale
###############################################################################
  color.index <- convertRGBtoNumber(Red = color.topleft[1], 
                                    Green = color.topleft[2],
                                    Blue = color.topleft[3])
  
  index <- which(abs(pantone.2$index - color.index) == min(abs(pantone.2$index - color.index)))
  
  pantone.scale <- pantone.2$PMS[index] # example "1234_C"

# check 
#  - https://stackoverflow.com/questions/44664766/r-lookup-interpolate-between-values-or-closest-match
#  - https://www.rdocumentation.org/packages/MALDIquant/versions/1.17/topics/match.closest

###############################################################################
# rename file, save image with Pantone-scale in name
###############################################################################
  filename <- paste0("./data/pantone/humanae_", stringr::str_replace(pantone.scale, " ", "_"), ".jpg")
  save.image(humanae.img, file = filename)

###############################################################################
# convert RGB to grayscale (only vary the intensity)
###############################################################################
  humanae.img.hsv <- RGBtoHSV(humanae.img)
  # Note that all display functions assume that your image is in RGB. So the image will look weird...
  plot(humanae.img.hsv)
  
  color.topleft <- round(humanae.img.hsv[1,1,1,], digits = 0)
  
  color.index <- convertRGBtoNumber(Red = color.topleft[1], 
                                    Green = color.topleft[2],
                                    Blue = color.topleft[3])
  
  filename <- paste0("./data/hsv/humanae_", as.character(color.index), ".jpg")
  save.image(humanae.img, file = filename)

  ###############################################################################
  # convert RGB to HSV (for a different type of sorting (on the hue))
  ###############################################################################
  humanae.img.grey <- grayscale(humanae.img)
    # Note that all display functions assume that your image is in RGB. So the image will look weird...
  plot(humanae.img.grey)
  
  color.topleft <- round(256 * humanae.img.grey[1,1,1,], digits = 0)
  
  color.index <- convertRGBtoNumber(Red = 0, 
                                    Green = color.topleft[1],
                                    Blue = 0)
  
  filename <- paste0("./data/grayscale/humanae_", as.character(color.index), ".jpg")
  save.image(humanae.img.grey, file = filename)
  
}

