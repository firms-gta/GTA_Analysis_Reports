print("Reading the spatial layers from the database...")

# SOURCE: GTA DATABASE ####

## WORLD BORDERS ####
WORLD_WKT = dbGetQuery(con_GTA, "SELECT code, label, level, source, area, ST_AsText(geom) AS WKT FROM area.gshhs_world_coastlines;")
WORLD_SF = st_as_sf(WORLD_WKT, wkt = "wkt", crs = st_crs(4326))


## COUNTRIES ####
# Without disputed areas
if(!file.exists(here("inputs/shapes/wb_countries_admin0_10m/WB_countries_Admin0_10m.shp"))) unzip(here("inputs/shapes/wb_countries_admin0_10m.zip"), exdir = here("inputs/shapes/"))

COUNTRIES_UNDISPUTED_SF = st_read(here("inputs/shapes/wb_countries_admin0_10m/WB_countries_Admin0_10m.shp"))

url= "https://www.fao.org/fishery/geoserver/wfs" 
serviceVersion = "1.0.0" 
logger = "INFO"
# SOURCE: OGC ####
WFS = WFSClient$new(url = url, serviceVersion = serviceVersion, logger = logger)

library(data.table)
library(sf)
library(sp)

get_wfs_data <- function(url= "https://www.fao.org/fishery/geoserver/wfs", 
                         version = "1.0.0", 
                         layer_name, output_dir = "inputs/shapes",logger = "INFO") {
  # create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  # define file paths
  shapefile_path <- file.path(output_dir, paste0(layer_name, ".shp"))

  # check if shapefile already exists
  if (file.exists(shapefile_path)) {
    message(paste0("Shapefile for layer '", layer_name, "' already exists, skipping download."))
    return(sf::st_read(shapefile_path) %>% rename(the_geom = geometry))
  }
  
  cwp_sf <- WFS$getFeatures(layer_name)
  cwp <- as.data.table(cwp_sf)
  if("gml_id.1" %in% colnames(cwp)){
    cwp <- cwp %>% select(-"gml_id.1")
  }

  # save data as shapefile
  sf::st_write(cwp, shapefile_path, driver = "ESRI Shapefile")
  
  # return data
  return(cwp)
}

# get CWP grids
CWP11 <- get_wfs_data(layer_name = "cwp:cwp-grid-map-1deg_x_1deg")
CWP55 <- get_wfs_data(layer_name = "cwp:cwp-grid-map-5deg_x_5deg")
CWP1010 <- get_wfs_data(layer_name = "cwp:cwp-grid-map-10deg_x_10deg")
CWP2020 <- get_wfs_data(layer_name = "cwp:cwp-grid-map-20deg_x_20deg")
CWP3030 <- get_wfs_data(layer_name = "cwp:cwp-grid-map-30deg_x_30deg")

CWP11_ERASED <- get_wfs_data(layer_name = "cwp:cwp-grid-map-1deg_x_1deg_erased")
CWP55_ERASED <- get_wfs_data(layer_name = "cwp:cwp-grid-map-5deg_x_5deg_erased")
CWP1010_ERASED <- get_wfs_data(layer_name = "cwp:cwp-grid-map-10deg_x_10deg_erased")
CWP2020_ERASED <- get_wfs_data(layer_name = "cwp:cwp-grid-map-20deg_x_20deg_erased")
CWP3030_ERASED <- get_wfs_data(layer_name = "cwp:cwp-grid-map-30deg_x_30deg_erased")

CWP_GRIDS <- rbindlist(list(CWP11, CWP55, CWP1010, CWP2020, CWP3030))
CWP_GRIDS_ERASED <- rbindlist(list(CWP11_ERASED, CWP55_ERASED, CWP1010_ERASED, CWP2020_ERASED, CWP3030_ERASED))

COUNTRIES_SF = get_wfs_data(layer_name = "fifao:country_bounds")


print("Spatial layers read!")


