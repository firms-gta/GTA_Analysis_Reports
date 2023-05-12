
# Checking all the square having declarations for more than one tRFMO

CA_RAW <- readRDS(here("inputs/data/mapped_codelist.rds"))  %>% 
  dplyr::mutate(unit = case_when(unit %in% c("MT","MTNO","t")~ "MT", unit %in% c( "NOMT", "NO", "no")~"NO"))


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


CA_GRID_DOUBLE_DECLARATIONS_5_DEG <- CA_IN_OVERLAPPING_ZONES_AGGREGATED_5DEG %>% inner_join(overlapping_declarations_5_DEG, by ="geographic_identifier")



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

################## Converting unit


conversion = function(con,fact,unit_conversion_csv_conversion_factor_url,unit_conversion_codelist_geoidentifiers_conversion_factors,mapping_map_code_lists = FALSE, georef_dataset, removing_numberfish_final = TRUE, converting_dataset_mapped  = TRUE){
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/extract_dataset.R")
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/tunaatlas_scripts/generation/convert_units.R")
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/list_metadata_datasets.R")
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/map_codelist.R")

  if(is.data.frame(unit_conversion_csv_conversion_factor_url)){
    
    df_conversion_factor <- unit_conversion_csv_conversion_factor_url
  } else {
    
    googledrive_baseurl <- "https://drive.google.com/open?id="
    if(startsWith(unit_conversion_csv_conversion_factor_url, googledrive_baseurl)){
      #managing download through google drive
      config$logger.info("Downloading file using Google Drive R interface")
      drive_id <- unlist(strsplit(unit_conversion_csv_conversion_factor_url, "id="))[2]
      drive_id <- unlist(strsplit(drive_id, "&export"))[1] #control in case export param is appended
      googledrive::drive_download(file = googledrive::as_id(drive_id), path = file.path("data", paste0(entity$identifiers[["id"]], "_conversion_factors.csv")), overwrite = TRUE)
      df_conversion_factor <- as.data.frame(readr::read_csv(file.path("data", paste0(entity$identifiers[["id"]], "_conversion_factors.csv")),guess_max=0))
    }else{
      df_conversion_factor <- as.data.frame(readr::read_csv(unit_conversion_csv_conversion_factor_url, guess_max=0))
    }
  }
  ## If we have not mapped the code lists (i.e. if mapping_map_code_lists==FALSE), we need to map the source gear coding system with ISSCFG coding system. In fact, the conversion factors dataset is expressed with ISSCFG coding system for gears, while the primary tRFMOs datasets are expressed with their own gear coding system.
  
  if("value" %in% colnames(df_conversion_factor)){
    df_conversion_factor <- df_conversion_factor %>% dplyr::rename(conversion_factor = value)
  }
  
  

  if(fact == "catch"){
    sum_no_before <- georef_dataset %>% filter(unit=="no")  %>% select(value)  %>% sum()
    species_no_before <- georef_dataset %>% filter(unit=="no") %>% distinct(species)
    cat(species_no_before$species)
    cat(intersect(species_no_before$species,unique(df_conversion_factor$species)))
  }
  source("https://raw.githubusercontent.com/eblondel/geoflow-tunaatlas/master/tunaatlas_scripts/generation/convert_units.R")
  georef_dataset<-convert_units(con = con,
                                df_input = georef_dataset,
                                df_conversion_factor = df_conversion_factor,
                                codelist_geoidentifiers_df_input = "areas_tuna_rfmos_task2",
                                codelist_geoidentifiers_conversion_factors = unit_conversion_codelist_geoidentifiers_conversion_factors
  )
  # to get stats on the process (useful for metadata)
  stats<-georef_dataset$stats
  georef_dataset<-georef_dataset$df
  
  #check what species didn't get  conversion factors from IRD file
  if(fact == "catch"){
    species_no_after <- georef_dataset %>% filter(unit=="no") %>% distinct(species)
    cat(setdiff(species_no_before$species,species_no_after$species))
    cat(intersect(species_no_after$species,unique(df_conversion_factor$species)))
  }
  
  
  if(fact == "catch"){
    sum_no_after<- georef_dataset %>% filter(unit=="no")  %>% select(value)  %>% sum()
    nrow_no <- nrow(georef_dataset %>% filter(unit=="no")  %>% select(value))
    if(removing_numberfish_final){
      georef_dataset <- georef_dataset[georef_dataset$unit == "t", ]}
    
  }
  
  
  # fill metadata elements

  return(georef_dataset)
}

georef_dataset <- conversion(con = con,
                                      fact="catch",
                                      unit_conversion_csv_conversion_factor_url=iotc_conv_fact_mapped,
                                      unit_conversion_codelist_geoidentifiers_conversion_factors="areas_tuna_rfmos_task2",
                                      mapping_map_code_lists=opts$mapping_map_code_lists,
                                      georef_dataset=georef_dataset, 
                                      removing_numberfish_final = FALSE) # do not remove number of fish as they will be converted later with other conversion factor data




