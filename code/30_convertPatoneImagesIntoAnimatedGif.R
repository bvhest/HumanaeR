#
# Gif animatie van alle humanae foto's.
#
#
# BvH, 15-04-2018
#

library(fs)
library(tidyverse)
library(magick)


###############################################################################
# read images from data directory
###############################################################################
fs::dir_ls(path = "./ruby/humanae") %>%
  head(n=100) %>%
  sort() %>%
  magick::image_read() %>%
  magick::image_join() %>%
  magick::image_scale("800x800") %>%
  magick::image_animate(fps = 2, 
                        dispose = "none") %>%
  magick::image_write("./images/humanae_animation.gif")
