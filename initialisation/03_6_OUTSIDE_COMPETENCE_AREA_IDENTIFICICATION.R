
# Declaration in Grids having no competence area 


CWP_GRIDS_WITHOUT_COMPETENCE_AREA_ON_OCEAN <- CWP_GRIDS_WITH_COMPETENCE_AREA_ON_OCEAN %>%
  dplyr::filter(is.na(label)) 

SUM_ALL_GRIDS_WIHOUT_COMPETENCE_AREA <- CWP_GRIDS_WITHOUT_COMPETENCE_AREA_ON_OCEAN %>%
  left_join(CA %>% dplyr::group_by(geographic_identifier, source_authority) %>% 
              summarise(value =sum(value)), by = c("CWP_CODE" = "geographic_identifier")) %>% 
  dplyr::filter(!is.na(value)) %>% ungroup()


# 
# # ROBINSON PACIFIC-CENTRED PROJECTION ####
# sf::sf_use_s2(FALSE)
# rob_pacific = "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
# 
# COUNTRIES_PAC_SF = fSpatPlan_Convert2PacificRobinson(COUNTRIES_UNDISPUTED_SF)
# SUM_ALL_GRIDS_WIHOUT_COMPETENCE_AREA_PAC = st_transform(SUM_ALL_GRIDS_WIHOUT_COMPETENCE_AREA, crs = rob_pacific)
# 
# 
# list(IATTC_shape, WCPFC_shape, ICCAT_shape, IOTC_shape) <- lapply(list(IATTC_shape, WCPFC_shape, ICCAT_shape, IOTC_shape), fSpatPlan_Convert2PacificRobinson)

require(tmap)
map_of_grids_not_included_in_any_competence_area <- tm_shape(COUNTRIES_SF)+tm_fill("darkgrey")+tm_borders()+ tm_shape(SUM_ALL_GRIDS_WIHOUT_COMPETENCE_AREA)+tm_borders()+tm_shape(IOTC_shape)+tm_borders()+
  tm_shape(IATTC_shape)+tm_borders("red")+
  tm_shape(WCPFC_shape)+tm_borders("yellow")+
  tm_shape(ICCAT_shape)+tm_borders("green")
  



tmap_save(filename=here("outputs/maps/outside_competence_zone_grids.png"),tm=map_of_grids_not_included_in_any_competence_area)


SUM_ALL_GRIDS_WIHOUT_COMPETENCE_AREA_SUMMARY <- SUM_ALL_GRIDS_WIHOUT_COMPETENCE_AREA[, c("CWP_CODE", "GRIDTYPE","source_authority", "value" )] %>% 
  st_set_geometry(NULL)


write.xlsx(SUM_ALL_GRIDS_WIHOUT_COMPETENCE_AREA_SUMMARY, "outputs/datasets/SUM_ALL_GRIDS_WIHOUT_COMPETENCE_AREA_SUMMARY.xlsx")


# Declaration in grids with mistaken competence area

## Removing of double declared area

SUM_ALL_GRIDS_WITH_COMPETENCE_AREA <- CWP_GRIDS_WITH_COMPETENCE_AREA_ON_OCEAN %>%
  left_join(CA %>% dplyr::group_by(geographic_identifier, source_authority) %>% 
              summarise(value =sum(value)), by = c("CWP_CODE" = "geographic_identifier")) %>% 
  dplyr::filter(!is.na(value)) %>% ungroup()

GRIDS_WITH_DOUBLE_ALLOWED_DECLARATION <- CWP_GRIDS_WITH_COMPETENCE_AREA_ON_OCEAN %>% st_set_geometry(NULL) %>% 
  group_by(CWP_CODE) %>% summarise(source_authority_number = n_distinct(code)) %>% filter(source_authority_number != 1)



CA_WITH_DECLARATION_OUTSIDE_JURIDICTION_ZONE <- SUM_ALL_GRIDS_WITH_COMPETENCE_AREA  %>% 
  filter(code != "CCSBT" & source_authority != "CCSBT") %>% filter(code != source_authority) 

CA_WITH_DECLARATION_OUTSIDE_JURIDICTION_ZONE_GROUPED_GRID <- CA_WITH_DECLARATION_OUTSIDE_JURIDICTION_ZONE %>%
  group_by(source_authority, CWP_CODE, unit) %>% summarise(value=sum(value))

# We remove of the "outside juridiction zone" the grids that covers two juridiction zones

CA_WITH_DECLARATION_OUTSIDE_JURIDICTION_ZONE_GROUPED_GRID <- CA_WITH_DECLARATION_OUTSIDE_JURIDICTION_ZONE_GROUPED_GRID%>% 
  anti_join(GRIDS_WITH_DOUBLE_ALLOWED_DECLARATION) %>% mutate(source_authority = as.factor(source_authority))


CA_WITH_DECLARATION_OUTSIDE_JURIDICTION_ZONE_GROUPED_GRID_1deg <- CA_WITH_DECLARATION_OUTSIDE_JURIDICTION_ZONE_GROUPED_GRID %>% filter(startsWith(CWP_CODE,"5"))

CA_WITH_DECLARATION_OUTSIDE_JURIDICTION_ZONE_GROUPED_GRID_5deg <- CA_WITH_DECLARATION_OUTSIDE_JURIDICTION_ZONE_GROUPED_GRID %>% filter(startsWith(CWP_CODE,"6"))

CA_WITH_DECLARATION_OUTSIDE_JURIDICTION_ZONE_GROUPED_GRID_SUMMARY = CA_WITH_DECLARATION_OUTSIDE_JURIDICTION_ZONE_GROUPED_GRID %>%group_by(source_authority)%>%
  mutate(Number_of_declaration = n()) %>% dplyr::select(-value) %>% st_set_geometry(NULL)

write.xlsx(CA_WITH_DECLARATION_OUTSIDE_JURIDICTION_ZONE_GROUPED_GRID_SUMMARY, "outputs/datasets/CA_OUTSIDE_COMPETENCE_AREA.xlsx")


map_of_captures_outside_juridiction_zone <- tm_shape(COUNTRIES_SF)+tm_fill("blue")+tm_shape(CA_WITH_DECLARATION_OUTSIDE_JURIDICTION_ZONE_GROUPED_GRID)+tm_fill("source_authority")+tm_borders("red", lwd = 0.5)+tm_shape(IOTC_shape)+tm_borders()+
  tm_shape(IATTC_shape)+tm_borders()+#tm_text("code")+
  tm_shape(WCPFC_shape)+tm_borders()+#tm_text("code")+
  tm_shape(ICCAT_shape)+tm_borders()+#tm_text("code")+
tm_shape(IOTC_shape)+tm_borders()#+tm_text("code")


tmap_save(filename=here("outputs/maps/outside_competence_zone_grids.png"),tm=map_of_grids_not_included_in_any_competence_area)


