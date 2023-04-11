# Clears the environment
rm(list = ls())
require(here)
withr::with_dir(here("initialisation"),{

# Includes defaults and helper functions
source("91_LIBS_EXTERNAL.R")
source("92_COLORS_SHAPES.R")
source("93_DATABASE_CONNECTIONS.R")
source("94_MAP_PACIFIC_PROJECTION_FUNCTION.R")

# Core charts and tables
source("01_SPATIAL_LAYERS_READING.R")
source("02_DATA_EXTRACTION.R")
source("03_1_MISLOCATION_ON_LAND_IDENTIFICATION.R")
source("03_2_MISLOCATION_ON_LAND_MAPPING.R")

})
