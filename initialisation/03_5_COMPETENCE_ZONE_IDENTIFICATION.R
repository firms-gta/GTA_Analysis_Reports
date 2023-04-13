print("Initialisation of overlapping zones area and grids")



# Checking all the square being in overlapping zone and setting all the shapes
require(sf)
query_area <- paste0("SELECT * FROM area.rfmos_convention_areas_fao")
COMPETENCE_AREA <- st_make_valid(st_read(con_GTA, query = query_area)) %>% filter(!st_is_empty(.))
IOTC_shape <- COMPETENCE_AREA %>% filter(code == "IOTC")
IATTC_shape <- COMPETENCE_AREA %>% filter(code == "IATTC")
WCPFC_shape <- COMPETENCE_AREA %>% filter(code == "WCPFC")
ICCAT_shape <- COMPETENCE_AREA %>% filter(code == "ICCAT")

# COMPETENCE_AREA_WITHOUT_GEOM <- COMPETENCE_AREA %>% st_set_geometry(NULL) 

CWP_GRIDS_WITH_COMPETENCE_AREA <- st_as_sf(CWP_GRIDS) %>% st_join(st_as_sf(COMPETENCE_AREA), join = st_intersects)


CWP_GRIDS_WITH_COMPETENCE_AREA_ON_OCEAN <- CWP_GRIDS_WITH_COMPETENCE_AREA %>%
  dplyr::filter(ON_LAND_P != 100)


