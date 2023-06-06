
# Checking all the square having declarations for more than one tRFMO

CA_RAW <- readRDS(here("inputs/data/mapped_codelist.rds"))  %>% 
  dplyr::mutate(unit = case_when(unit %in% c("MT","MTNO","t")~ "MT", unit %in% c( "NOMT", "NO", "no")~"NO"))

if(filter_species){
  CA_RAW <- CA_RAW %>% filter(species %in% cwp_codes)
}
  


overlapping_declarations <- as.data.frame(CA_RAW) %>% filter(source_authority != "CCSBT") %>% 
  dplyr::select(geographic_identifier, source_authority) %>% 
  dplyr::group_by(geographic_identifier) %>% 
  dplyr::summarise(number_of_declarant = n_distinct(source_authority), 
                   declarant_names = as.factor(paste(sort(unique(source_authority)), collapse = ", "))) %>% 
  dplyr::filter(number_of_declarant > 1) 


## Captures in a cwp grid where there is a double declaration
CA_GRID_DOUBLE_DECLARATIONS <- CA_RAW %>% inner_join(overlapping_declarations)



# Removing data outside juridiction zone 
CA_GRID_DOUBLE_DECLARATIONS_ALLOWED <- CA_GRID_DOUBLE_DECLARATIONS %>% anti_join(CA_WITH_DECLARATION_OUTSIDE_JURIDICTION_ZONE_GROUPED_GRID, by =c("geographic_identifier"="CWP_CODE"))

########################### GRID IN DOUBLE JURIDICTION AREA ###################

# Checking all the grid being in two juridictions areas
CWP_GRIDS_IN_OVERLAPPING_ZONES <- CWP_GRIDS_WITH_COMPETENCE_AREA %>% filter(label != "Southern hemisphere oceans") %>% group_by(CWP_CODE, GRIDTYPE) %>% st_set_geometry(NULL)  %>%   dplyr::summarise(number_of_declarant = n_distinct(label), 
                                                                              declarant_names = as.factor(paste(sort(unique(label)), collapse = ", "))) %>% 
  dplyr::filter(number_of_declarant > 1) %>% dplyr::select(-number_of_declarant)

CA_IN_OVERLAPPING_ZONES <- CA_RAW %>% inner_join(CWP_GRIDS_IN_OVERLAPPING_ZONES, by = c("geographic_identifier"= "CWP_CODE"))

CA_IN_OVERLAPPING_ZONES_WITH_GEOM <- CA_IN_OVERLAPPING_ZONES %>% left_join(CWP_GRIDS, by = c("geographic_identifier"= "CWP_CODE", "GRIDTYPE"))

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

############################################## GRIDS HAVING DATA FROM 2 tRFMOs

CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID <- CA_GRID_DOUBLE_DECLARATIONS_ALLOWED %>% inner_join(CWP_GRIDS, by = c("geographic_identifier" = "CWP_CODE")) %>% 
  group_by(geographic_identifier, GRIDTYPE, the_geom) %>% summarise(value = sum(value))

double_declared_cwp <- unique(CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID$geoographic_identifier)

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


CA_IN_OVERLAPPING_ZONES_AGGREGATED_5DEG <- aggregate_resolution(con = con_GTA, df_input = CA_IN_OVERLAPPING_ZONES, resolution = 6)$df


overlapping_declarations_5_DEG <- as.data.frame(CA_IN_OVERLAPPING_ZONES_AGGREGATED_5DEG) %>% filter(source_authority != "CCSBT") %>% 
  dplyr::select(geographic_identifier, source_authority) %>% 
  dplyr::group_by(geographic_identifier) %>% 
  dplyr::summarise(number_of_declarant = n_distinct(source_authority), 
                   declarant_names = as.factor(paste(sort(unique(source_authority)), collapse = ", "))) %>% 
  dplyr::filter(number_of_declarant > 1) 


CA_GRID_DOUBLE_DECLARATIONS_5_DEG <- CA_IN_OVERLAPPING_ZONES_AGGREGATED_5DEG %>% dplyr::select(-declarant_names)%>% inner_join(overlapping_declarations_5_DEG, by ="geographic_identifier")



# ##### SAME FOR AGGREGATED COMMENTED BECAUSE NOT CHANIGN ANYTHING
# 
# CA_GRID_DOUBLE_DECLARATIONS_5_DEG <- CA_GRID_DOUBLE_DECLARATIONS_5_DEG %>% inner_join(CWP_GRIDS, by = c("geographic_identifier" = "CWP_CODE", "GRIDTYPE")) %>% 
#   group_by(geographic_identifier, GRIDTYPE, the_geom) %>% summarise(value = sum(value))
# 
# CA_GRID_DOUBLE_DECLARATIONS_MAP_AGGREGATED <- ggplot() + 
#   geom_sf(data = COUNTRIES_SF, size = .2, fill = "darkgrey", col = NA) + 
#   theme(panel.grid.major = element_line(color = gray(0.9), linetype = "dashed", linewidth = 0.1)) + 
#   geom_sf(data = CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID, 
#           aes(geometry = the_geom), size = 3) +
#   facet_grid(~GRIDTYPE) +  # add facet for GRIDTYPE
#   theme_bw() +
#   labs(x = "", y = "", title = "") +
#   theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.3), 
#         panel.background = element_rect(fill = "white")) + 
#   guides(fill = guide_legend(title = "RFMO"))
# 
# 
# ggsave(filename=here("outputs/charts/overlapping/CA_GRID_DOUBLE_DECLARATIONS_MAP_AGGREGATED.png"),CA_GRID_DOUBLE_DECLARATIONS_MAP_AGGREGATED)

#########################################

DATA_IN_OVERLAPPING_ZONE_NOT_WITH_DOUBLE_DECLARATION <- CA_IN_OVERLAPPING_ZONES_AGGREGATED_5DEG %>% ungroup() %>% 
  anti_join(overlapping_declarations_5_DEG, by = "geographic_identifier") %>% inner_join(CWP_GRIDS, by = c("geographic_identifier" = "CWP_CODE", "GRIDTYPE")) %>% 
  group_by(geographic_identifier, GRIDTYPE, the_geom) %>% summarise(value = sum(value))

DATA_IN_OVERLAPPING_ZONE_NOT_WITH_DOUBLE_DECLARATION_MAP <- ggplot() + 
  geom_sf(data = COUNTRIES_SF, size = .2, fill = "darkgrey", col = NA) + 
  theme(panel.grid.major = element_line(color = gray(0.9), linetype = "dashed", linewidth = 0.1)) + 
  geom_sf(data = DATA_IN_OVERLAPPING_ZONE_NOT_WITH_DOUBLE_DECLARATION, 
          aes(geometry = the_geom), size = 3) +
  # facet_grid(~unit) +  # add facet for GRIDTYPE
  theme_bw() +
  labs(x = "", y = "", title = "") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.3), 
        panel.background = element_rect(fill = "white")) + 
  guides(fill = guide_legend(title = "RFMO"))


ggsave(filename=here("outputs/charts/overlapping/DATA_IN_OVERLAPPING_ZONE_NOT_WITH_DOUBLE_DECLARATION_MAP.png"),DATA_IN_OVERLAPPING_ZONE_NOT_WITH_DOUBLE_DECLARATION_MAP)



################## Converting unit

source(here("conversion_function.R"))

# if(!exists(here("data/conversion_factors.csv"))){


source(file.path(url_scripts_create_own_tuna_atlas, "map_codelists.R")) #modified for geoflow


iotc_conv_fact <- read_csv("data/conversion_factors_IOTC.csv", 
                           col_types = cols(geographic_identifier = col_character(), 
                                            time_start = col_character(), time_end = col_character())) %>% dplyr::rename(value = conversion_factor)
mapping_dataset <- read.csv("data/codelist_mapping_rfmos_to_global.csv")

iotc_conv_fact_mapped <- map_codelists(con_GTA, "catch", mapping_dataset = mapping_dataset, dataset_to_map = iotc_conv_fact, mapping_keep_src_code = FALSE,
                                       source_authority_to_map = c("IOTC"))$dataset_mapped%>% dplyr::rename( conversion_factor= value)#this map condelist function is to retieve the mapping dataset used

library(lubridate)

iotc_conv_fact_mapped$time_start <- as.Date(iotc_conv_fact_mapped$time_start)
iotc_conv_fact_mapped$time_start <- as.character(floor_date(iotc_conv_fact_mapped$time_start, "year"))
iotc_conv_fact_mapped$time_end <- as.Date(iotc_conv_fact_mapped$time_end)
iotc_conv_fact_mapped$time_end <- as.character(ceiling_date(iotc_conv_fact_mapped$time_end, "year") - days(1))




iotc_conversionned <- conversion(con = con,   fact="catch",
                                                          unit_conversion_csv_conversion_factor_url=iotc_conv_fact_mapped,
                                                          unit_conversion_codelist_geoidentifiers_conversion_factors="areas_tuna_rfmos_task2",
                                                          mapping_map_code_lists=TRUE,
                                                          georef_dataset=CA_GRID_DOUBLE_DECLARATIONS_5_DEG%>% filter(source_authority == "IOTC") , 
                                                          removing_numberfish_final = FALSE)


not_converted <- anti_join(CA_GRID_DOUBLE_DECLARATIONS_5_DEG, iotc_conversionned,by = join_by(source_authority, species, gear, fishingfleet, schooltype, time_start,
                                                                                               time_end, geographic_identifier, catchtype, GRIDTYPE, number_of_declarant,
                                                                                               declarant_names))


converted_overlapping <- conversion(con = con,
                                    fact="catch",
                                    unit_conversion_csv_conversion_factor_url="https://drive.google.com/open?id=1csQ5Ww8QRTaYd1DG8chwuw0UVUOGkjNL",
                                    unit_conversion_codelist_geoidentifiers_conversion_factors="areas_tuna_rfmos_task2",
                                    mapping_map_code_lists=TRUE,
                                    georef_dataset=not_converted, 
                                    removing_numberfish_final = FALSE) # do not remove number of fish as they will be converted later with other conversion factor data


converted_overlapping <- rbind(converted_overlapping , iotc_conversionned)
# }





print('Overlapping declarations maps finished !')

