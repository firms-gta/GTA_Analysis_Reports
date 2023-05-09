print('IATTC and WCPFC overlap analyse')


CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IATTC_WCPFC_DATA <- CA_GRID_DOUBLE_DECLARATIONS %>%
  dplyr::filter(declarant_names == "IATTC, WCPFC") %>% filter(source_authority %in% c("IATTC", "WCPFC"))




CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IATTC_WCPFC_DATA_SUMMARY <- CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IATTC_WCPFC_DATA %>% 
   dplyr::mutate(unit = case_when(unit %in% c("MT","MTNO")~ "Tons", unit %in% c( "NOMT", "NO")~"Number of fish")) %>% 
  mutate(Year = lubridate::year(time_start)) %>% 
  group_by(unit, source_authority, species, geographic_identifier, Year) %>% summarise(value = sum(value))



CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IATTC_WCPFC_DATA_SUMMARY_COMP <- 
  CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IATTC_WCPFC_DATA_SUMMARY %>%
  pivot_wider(names_from = "source_authority", values_from = "value") %>% inner_join(CWP_GRIDS, by = c("geographic_identifier" = "CWP_CODE")) %>%
  mutate(IATTC = ifelse(is.na(IATTC), 0, IATTC)) %>% 
  mutate(WCPFC = ifelse(is.na(WCPFC), 0, WCPFC)) %>% 
  mutate(Diff = 100*((IATTC-WCPFC)/IATTC)) %>% ungroup()

CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IATTC_WCPFC_DATA_SUMMARY_COMP_CLEANED <- CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IATTC_WCPFC_DATA_SUMMARY_COMP %>% 
  rowwise() %>% 
  mutate(Double = as.factor(ifelse(is.na(IATTC)| is.na(WCPFC)|IATTC == 0 | WCPFC == 0 , "SINGLE", "DOUBLE"))) %>% 
  pivot_longer(cols = c("IATTC", "WCPFC"), 
               names_to = "RFMO", 
               values_to = "value")

library(purrr)


plot_list <- map(unique(CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IATTC_WCPFC_DATA_SUMMARY_COMP$species), function(species){
  ggplot() +
    geom_line(data = CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IATTC_WCPFC_DATA_SUMMARY_COMP_CLEANED %>% 
              group_by(unit, RFMO, species, Year) %>%
              summarise(value =sum(value)) %>% 
              ungroup() %>% 
              filter(species == !!species), 
            aes(x = Year, y = value, color = RFMO)) +
    geom_point(shape = "circle", size = 1.5) +
    scale_color_hue(direction = 1) +
    theme_minimal()+
    facet_wrap(vars(unit), scales = "free_y")+
    ggtitle(paste0("Map of ", species))
  
})

walk2(plot_list, unique(CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IATTC_WCPFC_DATA_SUMMARY_COMP$species), function(plot, species){
  ggsave(paste0("outputs/charts/overlapping/IATTC_WCPFC/double_declaration_",species, "_plots.png"), plot, width = 12, height = 8, dpi = 300)
})



# Create a list of ggplots, one for each species
plot_list <- map(unique(CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IATTC_WCPFC_DATA_SUMMARY_COMP$species), function(species){
  ggplot() +
    geom_sf(data = COUNTRIES_SF, size = .2, fill = "darkgrey", col = NA) + 
    theme(panel.grid.major = element_line(color = gray(0.9), linetype = "dashed", linewidth = 0.1)) + 
    geom_sf(data = CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IATTC_WCPFC_DATA_SUMMARY_COMP %>% 
              ungroup() %>% 
              filter(species == !!species), 
            aes(geometry = the_geom, fill = Diff), size = 3) +
    facet_grid(~ unit) +  # add facet for GRIDTYPE
    theme_bw() +
    labs(x = "", y = "", title = "") +
    theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.3), 
          panel.background = element_rect(fill = "white")) + 
    scale_fill_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) + 
    guides(fill = guide_legend(title = "Diff")) +
    ggtitle(paste0("Map of ", species))
})

# Save each plot as a file with the name of the species
walk2(plot_list, unique(CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IATTC_WCPFC_DATA_SUMMARY_COMP$species), function(plot, species){
  ggsave(paste0("outputs/charts/overlapping/IATTC_WCPFC/double_declaration_",species, "_charts.png"), plot, width = 12, height = 8, dpi = 300)
})

## Analaysis of spatial resolution of the declarations in the overlapping area

CWP_GRIDS_IN_OVERLAPPING_ZONES_IATTC_WCPFC <- CWP_GRIDS_IN_OVERLAPPING_ZONES %>% filter(declarant_names == "East Pacific ocean, West Pacific ocean")

CA_IN_OVERLAPPING_ZONES_IATTC_WCPFC <- CA_RAW %>% filter(source_authority!="CCSBT") %>% inner_join(CWP_GRIDS_IN_OVERLAPPING_ZONES_IATTC_WCPFC, by = c("geographic_identifier" = "CWP_CODE"))

CA_IN_OVERLAPPING_ZONES_IATTC_WCPFC_SUMMARY_GRIDTYPE <- CA_IN_OVERLAPPING_ZONES_IATTC_WCPFC %>% 
  dplyr::mutate(unit = case_when(unit %in% c("MT","MTNO")~ "Tons", unit %in% c( "NOMT", "NO")~"Number of fish")) %>% 
  group_by(GRIDTYPE, source_authority, unit) %>% summarise(value =sum(value))


write.xlsx(CA_IN_OVERLAPPING_ZONES_IATTC_WCPFC_SUMMARY_GRIDTYPE, "outputs/datasets/CA_IN_OVERLAPPING_ZONES_IATTC_WCPFC_SUMMARY_GRIDTYPE.xlsx")


CA_IN_OVERLAPPING_ZONES_IATTC_WCPFC_SUMMARY_SPECIES <- CA_IN_OVERLAPPING_ZONES_IATTC_WCPFC %>% 
  dplyr::mutate(unit = case_when(unit %in% c("MT","MTNO")~ "Tons", unit %in% c( "NOMT", "NO")~"Number of fish")) %>% 
  group_by(species, source_authority, unit) %>% summarise(value =sum(value))


write.xlsx(CA_IN_OVERLAPPING_ZONES_IATTC_WCPFC_SUMMARY_SPECIES, "outputs/datasets/CA_IN_OVERLAPPING_ZONES_IATTC_WCPFC_SUMMARY_SPECIES.xslx")

