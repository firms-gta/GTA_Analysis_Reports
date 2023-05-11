
# Declaration in Grids having no competence area 


CWP_GRIDS_WITHOUT_COMPETENCE_AREA_ON_OCEAN <- CWP_GRIDS_WITH_COMPETENCE_AREA_ON_OCEAN %>%
  dplyr::filter(is.na(label)) 

SUM_ALL_GRIDS_WIHOUT_COMPETENCE_AREA <- CWP_GRIDS_WITHOUT_COMPETENCE_AREA_ON_OCEAN %>%
  left_join(CA %>% dplyr::group_by(geographic_identifier, source_authority, unit) %>% 
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

CA_WITH_NO_COMPETENCE_AREA_MAP <- ggplot() + 
  geom_sf(data = COUNTRIES_SF, size = .2, fill = "darkgrey", col = NA) + 
  theme(panel.grid.major = element_line(color = gray(0.9), linetype = "dashed", linewidth = 0.1)) + 
  geom_sf(data = SUM_ALL_GRIDS_WIHOUT_COMPETENCE_AREA %>% rename(RFMO = source_authority), aes(fill = RFMO), size = 3)+
  geom_sf(data=IOTC_shape, size = .02,  fill = "transparent", col = "black")+
  geom_sf(data=IATTC_shape, size = .02,  fill = "transparent", col = "black")+
  geom_sf(data=WCPFC_shape, size = .02,  fill = "transparent", col = "black")+
  geom_sf(data=ICCAT_shape, size = .02,  fill = "transparent", col = "black")+
  theme_bw() +
  labs(x = "", y = "", title = "") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.3), 
        panel.background = element_rect(fill = "white")) + 
  guides(fill = guide_legend(title = "RFMO"))


ggsave(filename=here("outputs/charts/mislocation/CA_WITH_NO_COMPETENCE_AREA_MAP.png"),CA_WITH_NO_COMPETENCE_AREA_MAP)


SUM_ALL_GRIDS_WIHOUT_COMPETENCE_AREA_SUMMARY <- SUM_ALL_GRIDS_WIHOUT_COMPETENCE_AREA[, c("CWP_CODE", "GRIDTYPE","source_authority", "value", "unit" )] %>% 
  st_set_geometry(NULL)


write.xlsx(SUM_ALL_GRIDS_WIHOUT_COMPETENCE_AREA_SUMMARY, "outputs/datasets/SUM_ALL_GRIDS_WIHOUT_COMPETENCE_AREA_SUMMARY.xlsx")


# Declaration in grids with mistaken competence area

## Removing of double declared area

SUM_ALL_GRIDS_WITH_COMPETENCE_AREA <- CWP_GRIDS_WITH_COMPETENCE_AREA_ON_OCEAN %>%
  left_join(CA %>% dplyr::group_by(geographic_identifier, source_authority, unit) %>% 
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


CA_OUTSIDE_COMPETENCE_AREA_OF_DECLARANT_MAP <- ggplot() + 
  geom_sf(data = COUNTRIES_SF, size = .2, fill = "darkgrey", col = NA) + 
  theme(panel.grid.major = element_line(color = gray(0.9), linetype = "dashed", linewidth = 0.1)) + 
  geom_sf(data = CA_WITH_DECLARATION_OUTSIDE_JURIDICTION_ZONE_GROUPED_GRID %>% rename(RFMO = source_authority), aes(fill = RFMO), size = 3)+
  geom_sf(data=IOTC_shape, size = .02,  fill = "transparent", col = "blue")+
  geom_sf(data=IATTC_shape, size = .02,  fill = "transparent", col = "red")+
  geom_sf(data=WCPFC_shape, size = .02,  fill = "transparent", col = "black")+
  geom_sf(data=ICCAT_shape, size = .02,  fill = "transparent", col = "green")+
  theme_bw() +
  labs(x = "", y = "", title = "") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.3), 
        panel.background = element_rect(fill = "white")) + 
  guides(fill = guide_legend(title = "RFMO"))


# CA_OUTSIDE_COMPETENCE_AREA_OF_DECLARANT_MAP <- tm_shape(COUNTRIES_SF)+tm_fill("darkgrey")+tm_borders()+
#   tm_shape(CA_WITH_DECLARATION_OUTSIDE_JURIDICTION_ZONE_GROUPED_GRID)+tm_fill("source_authority", title = "RFMO")+
#   tm_borders("red", lwd = 0.5)+tm_shape(IOTC_shape)+tm_borders("darkblue")+
#   tm_shape(IATTC_shape)+tm_borders("darkblue")+#tm_text("code")+
#   tm_shape(WCPFC_shape)+tm_borders("darkblue")+#tm_text("code")+
#   tm_shape(ICCAT_shape)+tm_borders("darkblue")+#tm_text("code")+
#   tm_shape(IOTC_shape)+tm_borders("darkblue")#+tm_text("code")


ggsave(filename=here("outputs/charts/mislocation/CA_OUTSIDE_COMPETENCE_AREA_OF_DECLARANT_MAP.png"),CA_OUTSIDE_COMPETENCE_AREA_OF_DECLARANT_MAP)

print('Outside competence area data filtered')

