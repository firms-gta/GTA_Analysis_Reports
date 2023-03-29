print("Reading the spatial layers from the database...")

# SOURCE: GTA DATABASE ####

## WORLD BORDERS ####
WORLD_WKT = dbGetQuery(con_GTA, "SELECT code, label, level, source, area, ST_AsText(geom) AS WKT FROM area.gshhs_world_coastlines;")
WORLD_SF = st_as_sf(WORLD_WKT, wkt = "wkt", crs = st_crs(4326))

## CWP GRIDS ####
# 1x1 grids (stored in the database)
#CWP11_WKT = dbGetQuery(con_GTA, "SELECT code, label, gridcode_cwp_integer, quadrant, lon_min, lon_max, lat_min, lat_max, spatial_resolution, ST_AsText(geom) AS WKT FROM area.cwp_grid WHERE spatial_resolution = '1';")
#CWP11_SF = st_as_sf(CWP11_WKT, wkt = "wkt", crs = st_crs(4326))
#CWP11 = as.data.frame(CWP11_SF)
#setDT(CWP11)

# 5x5 grids (stored in the database)
#CWP55_WKT = dbGetQuery(con_GTA, "SELECT code, label, gridcode_cwp_integer, quadrant, lon_min, lon_max, lat_min, lat_max, spatial_resolution, ST_AsText(geom) AS WKT FROM area.cwp_grid WHERE spatial_resolution = '5';")
#CWP55_SF = st_as_sf(CWP55_WKT, wkt = "wkt", crs = st_crs(4326))
#CWP55 = as.data.frame(CWP55_SF)
#setDT(CWP55)

# SOURCE: SHAPEFILES ####

## COUNTRIES ####
# Without disputed areas
if(!file.exists("../inputs/shapes/wb_countries_admin0_10m/WB_countries_Admin0_10m.shp")) unzip("../inputs/shapes/wb_countries_admin0_10m.zip", exdir = "../inputs/shapes/")

COUNTRIES_UNDISPUTED_SF = st_read("../inputs/shapes/wb_countries_admin0_10m/WB_countries_Admin0_10m.shp")

# SOURCE: OGC ####
WFS = WFSClient$new(url = "https://www.fao.org/fishery/geoserver/wfs", serviceVersion = "1.0.0", logger = "INFO")

## CWP GRIDS ####
# 1x1 grids
CWP11_SF = WFS$getFeatures("cwp:cwp-grid-map-1deg_x_1deg")
CWP11 = as.data.table(CWP11_SF)
CWP11_ERASED_SF = WFS$getFeatures("cwp:cwp-grid-map-1deg_x_1deg_erased")
CWP11_ERASED = as.data.table(CWP11_ERASED_SF)

# 5x5 grids
CWP55_SF = WFS$getFeatures("cwp:cwp-grid-map-5deg_x_5deg")
CWP55 = as.data.table(CWP55_SF)
CWP55_ERASED_SF = WFS$getFeatures("cwp:cwp-grid-map-5deg_x_5deg_erased")
CWP55_ERASED = as.data.table(CWP55_ERASED_SF)

# 10x10 grids
CWP1010_SF = WFS$getFeatures("cwp:cwp-grid-map-10deg_x_10deg")
CWP1010 = as.data.table(CWP1010_SF)
CWP1010_ERASED_SF = WFS$getFeatures("cwp:cwp-grid-map-10deg_x_10deg_erased")
CWP1010_ERASED = as.data.table(CWP1010_ERASED_SF)

# 20x20 grids
CWP2020_SF = WFS$getFeatures("cwp:cwp-grid-map-20deg_x_20deg")
CWP2020 = as.data.table(CWP2020_SF)
CWP2020_ERASED_SF = WFS$getFeatures("cwp:cwp-grid-map-20deg_x_20deg_erased")
CWP2020_ERASED = as.data.table(CWP2020_ERASED_SF)

# 30x30 grids (stored in the database)
CWP3030_SF = WFS$getFeatures("cwp:cwp-grid-map-30deg_x_30deg")
CWP3030 = as.data.table(CWP3030_SF)
CWP3030_ERASED_SF = WFS$getFeatures("cwp:cwp-grid-map-30deg_x_30deg_erased")
CWP3030_ERASED = as.data.table(CWP3030_ERASED_SF)

# Combine all grids
CWP_GRIDS = rbindlist(list(CWP11, CWP55, CWP1010, CWP2020, CWP3030))
CWP_GRIDS_ERASED = rbindlist(list(CWP11_ERASED[, -c("gml_id.1")], CWP55_ERASED, CWP1010_ERASED[, -c("gml_id.1")], CWP2020_ERASED[, -c("gml_id.1")], CWP3030_ERASED[, -c("gml_id.1")]))

## COUNTRIES ####
# Countries (used by FIRMS to intersect the CWP grids)
COUNTRIES_SF = WFS$getFeatures("fifao:country_bounds")

print("Spatial layers read!")
