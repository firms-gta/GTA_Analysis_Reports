
# Declaration in Grids having no competence area 


CWP_GRIDS_WITHOUT_COMPETENCE_AREA_ON_OCEAN <- CWP_GRIDS_WITH_COMPETENCE_AREA_ON_OCEAN %>%
  dplyr::filter(is.na(label)) 

SUM_ALL_GRIDS_WIHOUT_COMPETENCE_AREA <- CWP_GRIDS_WITHOUT_COMPETENCE_AREA_ON_OCEAN %>%
  left_join(CA %>% dplyr::group_by(geographic_identifier, source_authority) %>% 
              summarise(value =sum(value)), by = c("CWP_CODE" = "geographic_identifier")) %>% 
  dplyr::filter(!is.na(value)) %>% ungroup()




require(tmap)
map_of_grids_not_included_in_any_competence_area <- tm_shape(SUM_ALL_GRIDS_WIHOUT_COMPETENCE_AREA)+tm_borders()+tm_shape(IOTC_shape)+tm_borders()+
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
  group_by(source_authority, CWP_CODE) %>% summarise(value=sum(value))

# We remove of the "outside juridiction zone" the grids that covers two juridiction zones

CA_WITH_DECLARATION_OUTSIDE_JURIDICTION_ZONE_GROUPED_GRID <- CA_WITH_DECLARATION_OUTSIDE_JURIDICTION_ZONE_GROUPED_GRID%>% 
  anti_join(GRIDS_WITH_DOUBLE_ALLOWED_DECLARATION)






map_of_captures_outside_juridiction_zone <- tm_shape(CA_WITH_DECLARATION_OUTSIDE_JURIDICTION_ZONE_GROUPED_GRID)+tm_fill("source_authority")+tm_borders("black")+tm_shape(IOTC_shape)+tm_borders()+
  tm_shape(IATTC_shape)+tm_borders()+#tm_text("code")+
  tm_shape(WCPFC_shape)+tm_borders()+#tm_text("code")+
  tm_shape(ICCAT_shape)+tm_borders()+#tm_text("code")+
tm_shape(IOTC_shape)+tm_borders()#+tm_text("code")


  
map_of_captures_outside_juridiction_zone


