# Install/load libraries required for analysis
# Install/load pacman
if(!require(pacman)){
  install.packages("pacman")
  suppressPackageStartupMessages(library(pacman,quietly = TRUE))
}

renv::restore()


# pacman::p_load("tidyverse",
#                "sf",
#                "raster",
#                "rnaturalearth",
#                "cmocean",
#                "magrittr",
#                "DBI",
#                "ows4R", # https://github.com/eblondel/ows4R
#                "openxlsx",
#                "data.table",
#                "ggsci",
#                "colorspace",
#                "flextable",
#                "officer",
#                "patchwork",
#                "ggplot2",
#                "dotenv",
#                "here", 
#                "renv", 
#                "RPostgreSQL", update=FALSE)

# Addition for plotting the Pacific-centred maps
base::load(url("https://github.com/valentinitnelav/RandomScripts/blob/master/NaturalEarth.RData?raw=true"))


# Set chart theme to theme_bw
theme_set(theme_bw())

