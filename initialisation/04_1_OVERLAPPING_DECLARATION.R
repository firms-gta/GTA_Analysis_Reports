
# Checking all the square having declarations for more than one tRFMO

CA_RAW <- readRDS(here("inputs/data/mapped_codelist.rds"))


overlapping_declarations <- as.data.frame(CA_RAW) %>% filter(source_authority != "CCSBT") %>% 
  dplyr::select(geographic_identifier, source_authority) %>% 
  dplyr::group_by(geographic_identifier) %>% 
  dplyr::summarise(number_of_declarant = n_distinct(source_authority), 
                   declarant_names = as.factor(paste(sort(unique(source_authority)), collapse = ", "))) %>% 
  dplyr::filter(number_of_declarant > 1) 



CA_GRID_DOUBLE_DECLARATIONS <- CA_RAW %>% inner_join(overlapping_declarations)



# Removing data outside juridiction zone 

CA_GRID_DOUBLE_DECLARATIONS_ALLOWED <- CA_GRID_DOUBLE_DECLARATIONS %>% anti_join(CA_WITH_DECLARATION_OUTSIDE_JURIDICTION_ZONE_GROUPED_GRID, by =c("geographic_identifier"="CWP_CODE"))

# Checking all the grid being in two juridictions areas
CWP_GRIDS_IN_OVERLAPPING_ZONES <- CWP_GRIDS_WITH_COMPETENCE_AREA %>% filter(label != "Southern hemisphere oceans") %>% group_by(CWP_CODE, GRIDTYPE) %>% st_set_geometry(NULL)  %>%   dplyr::summarise(number_of_declarant = n_distinct(label), 
                                                                              declarant_names = as.factor(paste(sort(unique(label)), collapse = ", "))) %>% 
  dplyr::filter(number_of_declarant > 1) 

CA_IN_OVERLAPPING_ZONES <- CA %>% inner_join(CWP_GRIDS_IN_OVERLAPPING_ZONES, by = c("geographic_identifier"= "CWP_CODE"))

CA_IN_OVERLAPPING_ZONES_WITH_GEOM <- CA_IN_OVERLAPPING_ZONES %>% left_join(CWP_GRIDS, by = c("geographic_identifier"= "CWP_CODE"))

CA_IN_OVERLAPPING_ZONES_WITH_GEOM_GROUPPED_CWP_CODE <- CA_IN_OVERLAPPING_ZONES_WITH_GEOM %>% group_by(geographic_identifier, declarant_names, the_geom,GRIDTYPE) %>% summarise(value = sum(value))

levels(droplevels(CA_IN_OVERLAPPING_ZONES_WITH_GEOM_GROUPPED_CWP_CODE$declarant_names))

CA_IN_OVERLAPPING_ZONES_MAP <- ggplot() + 
  geom_sf(data = COUNTRIES_SF, size = .2, fill = "darkgrey", col = NA) + 
  theme(panel.grid.major = element_line(color = gray(0.9), linetype = "dashed", linewidth = 0.1)) + 
  geom_sf(data = CA_IN_OVERLAPPING_ZONES_WITH_GEOM_GROUPPED_CWP_CODE, 
          aes(geometry = the_geom, fill = declarant_names), size = 3) +
  facet_grid(~GRIDTYPE) +  # add facet for GRIDTYPE
  theme_bw() +
  labs(x = "", y = "", title = "") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.3), 
        panel.background = element_rect(fill = "white")) + 
  guides(fill = guide_legend(title = "RFMO"))

ggsave(filename=here("outputs/charts/overlapping/CA_IN_OVERLAPPING_ZONES_MAP.png"),CA_IN_OVERLAPPING_ZONES_MAP)



CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID <- CA_GRID_DOUBLE_DECLARATIONS_ALLOWED %>% inner_join(CWP_GRIDS, by = c("geographic_identifier" = "CWP_CODE")) %>% 
  group_by(geographic_identifier,GRIDTYPE, the_geom) %>% summarise(value = sum(value))

CA_GRID_DOUBLE_DECLARATIONS_MAP <- ggplot() + 
  geom_sf(data = COUNTRIES_SF, size = .2, fill = "darkgrey", col = NA) + 
  theme(panel.grid.major = element_line(color = gray(0.9), linetype = "dashed", linewidth = 0.1)) + 
  geom_sf(data = CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID, 
          aes(geometry = the_geom), size = 3) +
  facet_grid(~GRIDTYPE) +  # add facet for GRIDTYPE
  theme_bw() +
  labs(x = "", y = "", title = "") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.3), 
        panel.background = element_rect(fill = "white")) + 
  guides(fill = guide_legend(title = "RFMO"))


ggsave(filename=here("outputs/charts/overlapping/CA_GRID_DOUBLE_DECLARATIONS_MAP.png"),CA_GRID_DOUBLE_DECLARATIONS_MAP)


## We handle now all the 1 deg square and we aggregates it in 5 deg to be able to compare them 

url_scripts_create_own_tuna_atlas <- "https://raw.githubusercontent.com/eblondel/geoflow-tunaatlas/master/tunaatlas_scripts/generation"

source(file.path(url_scripts_create_own_tuna_atlas, "aggregate_resolution.R")) #modified for geoflow


CA_IN_OVERLAPPING_ZONES_AGGREGATED_5DEG <- aggregate_resolution(con_GTA, CA_IN_OVERLAPPING_ZONES, "6")$df



DATA_IN_OVERLAPPING_ZONE_NOT_WITH_DOUBLE_DECLARATION <- CA_IN_OVERLAPPING_ZONES_AGGREGATED_5DEG %>% ungroup() %>% 
  anti_join(overlapping_declarations, by = "geographic_identifier") %>% inner_join(CWP_GRIDS, by = c("geographic_identifier" = "CWP_CODE")) %>% 
  group_by(geographic_identifier, GRIDTYPE, unit, the_geom) %>% summarise(value = sum(value))

DATA_IN_OVERLAPPING_ZONE_NOT_WITH_DOUBLE_DECLARATION_MAP <- ggplot() + 
  geom_sf(data = COUNTRIES_SF, size = .2, fill = "darkgrey", col = NA) + 
  theme(panel.grid.major = element_line(color = gray(0.9), linetype = "dashed", linewidth = 0.1)) + 
  geom_sf(data = DATA_IN_OVERLAPPING_ZONE_NOT_WITH_DOUBLE_DECLARATION, 
          aes(geometry = the_geom), size = 3) +
  facet_grid(~unit) +  # add facet for GRIDTYPE
  theme_bw() +
  labs(x = "", y = "", title = "") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.3), 
        panel.background = element_rect(fill = "white")) + 
  guides(fill = guide_legend(title = "RFMO"))


ggsave(filename=here("outputs/charts/overlapping/DATA_IN_OVERLAPPING_ZONE_NOT_WITH_DOUBLE_DECLARATION_MAP.png"),DATA_IN_OVERLAPPING_ZONE_NOT_WITH_DOUBLE_DECLARATION_MAP)


print('Overlapping declarations maps finished !')



