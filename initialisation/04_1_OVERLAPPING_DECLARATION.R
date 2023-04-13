
# Checking all the square having declarations for more than one tRFMO

overlapping_declarations <- as.data.frame(CA) %>% 
  dplyr::select(geographic_identifier, source_authority) %>% 
  dplyr::group_by(geographic_identifier) %>% 
  dplyr::summarise(number_of_declarant = n_distinct(source_authority), 
                   declarant_names = as.factor(paste(sort(unique(source_authority)), collapse = ", "))) %>% 
  dplyr::filter(number_of_declarant > 1)





CA_GRID_DOUBLE_DECLARATIONS <- CA %>% inner_join(overlapping_declarations)


# Removing data outside juridiction zone 

CA_GRID_DOUBLE_DECLARATIONS <- CA_GRID_DOUBLE_DECLARATIONS %>% anti_join(CA_WITH_DECLARATION_OUTSIDE_JURIDICTION_ZONE, by =c("geographic_identifier"="CWP_CODE"))

