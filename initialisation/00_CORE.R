# Clears the environment
# rm(list = ls())
require(here)

# Includes defaults and helper functions
source(here("initialisation/91_LIBS_EXTERNAL.R"))
source(here("initialisation/92_COLORS_SHAPES.R"))
source(here("initialisation/93_DATABASE_CONNECTIONS.R"))
source(here("initialisation/02_02_SPECIES_KEPT.R"))
source(here("initialisation/94_MAP_PACIFIC_PROJECTION_FUNCTION.R"))

# Core charts and tables
source(here("initialisation/01_SPATIAL_LAYERS_READING.R"))
source(here("initialisation/02_DATA_EXTRACTION.R"))


source(here("initialisation/03_1_MISLOCATION_ON_LAND_IDENTIFICATION.R"))
source(here("initialisation/03_2_MISLOCATION_ON_LAND_MAPPING.R"))
source(here("initialisation/03_3_MISLOCATION_FACTORS_IDENTIFICATION.R"))

source(here("initialisation/03_4_MISLOCATION_ON_LAND_TIME_SERIES.R"))
source(here("initialisation/03_5_COMPETENCE_ZONE_INITIALIZATION.R"))
source(here("initialisation/03_6_OUTSIDE_COMPETENCE_AREA_IDENTIFICICATION.R"))

source(here("initialisation/04_1_OVERLAPPING_DECLARATION.R"))

source(here("initialisation/04_2_OVERLAPPING_IATTC_WCPFC.R"))
source(here("initialisation/04_3_OVERLAP_IATTC_WCPFC_CONVERTED.R"))

source(here("initialisation/04_4_OVERLAPPING_IOTC_WCPFC.R"))
source(here("initialisation/04_5_OVERLAP_IOTC_WCPFC_CONVERTED.R"))

source(here("initialisation/04_6_OVERLAPPING_SBF.R"))
