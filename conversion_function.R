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
      drive_id <- unlist(strsplit(unit_conversion_csv_conversion_factor_url, "id="))[2]
      drive_id <- unlist(strsplit(drive_id, "&export"))[1] #control in case export param is appended
      googledrive::drive_download(file = googledrive::as_id(drive_id), path = file.path("conversion_factors.csv"), overwrite = TRUE)
      df_conversion_factor <- as.data.frame(readr::read_csv(file.path("conversion_factors.csv"),guess_max=0))
    }else{
      df_conversion_factor <- as.data.frame(readr::read_csv(unit_conversion_csv_conversion_factor_url, guess_max=0))
    }
  }
  ## If we have not mapped the code lists (i.e. if mapping_map_code_lists==FALSE), we need to map the source gear coding system with ISSCFG coding system. In fact, the conversion factors dataset is expressed with ISSCFG coding system for gears, while the primary tRFMOs datasets are expressed with their own gear coding system.
  
  ### Add conversion factors for wcpfc 
  
  
  df_conversion_factor <- rbind(df_conversion_factor, 
                                df_conversion_factor %>% filter(source_authority == "IATTC") %>% 
                                  mutate(source_authority = "WCPFC"))
  df_conversion_factor <- rbind(df_conversion_factor, 
                                df_conversion_factor %>% filter(species == "SBF") %>% 
                                  mutate(source_authority = "CCSBT"))
  
  if("value" %in% colnames(df_conversion_factor)){
    df_conversion_factor <- df_conversion_factor %>% dplyr::rename(conversion_factor = value)
  }
  
  
  
  if(fact == "catch"){
    sum_no_before <- georef_dataset %>% dplyr::filter(unit=="no")  %>% dplyr::select(value)  %>% sum()
    species_no_before <- georef_dataset %>% dplyr::filter(unit=="no") %>% dplyr::distinct(species)
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
    sum_no_after<- georef_dataset %>% dplyr::filter(unit=="no")  %>% dplyr::select(value)  %>% sum()
    nrow_no <- nrow(georef_dataset %>% dplyr::filter(unit=="no")  %>% dplyr::select(value))
    if(removing_numberfish_final){
      georef_dataset <- georef_dataset[georef_dataset$unit == "t", ]}
    
  }
  
  
  # fill metadata elements
  
  return(georef_dataset)
}
