print('IOTC and WCPFC overlap analyse')


CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IOTC_WCPFC_DATA <- CA_GRID_DOUBLE_DECLARATIONS_5_DEG %>%
  dplyr::filter(declarant_names == "IOTC, WCPFC") %>% filter(source_authority %in% c("IOTC", "WCPFC"))




CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IOTC_WCPFC_DATA_SUMMARY <- CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IOTC_WCPFC_DATA %>% 
  dplyr::mutate(unit = case_when(unit %in% c("MT","MTNO","t")~ "Tons", unit %in% c( "NOMT", "NO", "no")~"Number of fish")) %>% 
  mutate(Year = lubridate::year(time_start)) %>% 
  group_by(unit, source_authority, species, geographic_identifier, Year) %>% summarise(value = sum(value))



CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IOTC_WCPFC_DATA_SUMMARY_COMP <- 
  CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IOTC_WCPFC_DATA_SUMMARY %>%
  pivot_wider(names_from = "source_authority", values_from = "value") %>% inner_join(CWP_GRIDS, by = c("geographic_identifier" = "CWP_CODE")) %>%
  mutate(IOTC = ifelse(is.na(IOTC), 0, IOTC)) %>% 
  mutate(WCPFC = ifelse(is.na(WCPFC), 0, WCPFC)) %>% 
  mutate(`Diff (IOTC-WCPFC)` = ((IOTC-WCPFC))) %>% ungroup()  %>% rowwise() %>% 
  mutate(Double = as.factor(case_when(is.na(IOTC)| IOTC==0 ~ "WCPFC", is.na(WCPFC) | WCPFC == 0 ~ "IOTC", TRUE~"DOUBLE")))


species_unique  <- (CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IOTC_WCPFC_DATA_SUMMARY %>% group_by(species) %>% 
                      summarise(count = n_distinct(source_authority)) %>% filter(count <2))$species


species_doubled  <- (CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IOTC_WCPFC_DATA_SUMMARY %>% group_by(species) %>% 
                       summarise(count = n_distinct(source_authority)) %>% filter(count ==2))$species

saveRDS(list(species_unique =species_unique, species_doubled = species_doubled), "data/species_unique_doubled_iotc_wpcfc.rds")


CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IOTC_WCPFC_DATA_SUMMARY_COMP_CLEANED <- CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IOTC_WCPFC_DATA_SUMMARY_COMP %>% 
  pivot_longer(cols = c("IOTC", "WCPFC"), 
               names_to = "RFMO", 
               values_to = "value")

library(purrr)

CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IOTC_WCPFC_DATA_SUMMARY_COMP_doubled_geographic <- CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IOTC_WCPFC_DATA_SUMMARY_COMP %>% 
  filter(Double == "DOUBLE")

# Create a list of plot of double and single area for each species having double declaration

plot_list_single_double <- map(species_doubled, function(species){
  ggplot(data = filter(CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IOTC_WCPFC_DATA_SUMMARY_COMP, species == !!species)) +
    geom_sf(data = COUNTRIES_SF, size = 0.2, fill = "darkgrey", color = NA) + 
    geom_sf(aes(geometry = the_geom, fill = Double), size = 3) +
    facet_grid(~ unit) + 
    scale_fill_manual(values = c("DOUBLE" = "green", "IOTC" = "blue", "WCPFC" = "red"), labels = c("DOUBLE" ="Double declaration", "IOTC" ="IOTC declaration", "WCPFC" ="WCPFC declaration")) +
    labs(x = "", y = "", title = paste0("Map of ", species)) +
    theme_bw() +
    theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", linewidth = 0.3), 
          panel.background = element_rect(fill = "white"))
})
# Save each plot as a file with the name of the species
walk2(plot_list_single_double, species_doubled, function(plot, species){
  ggsave(paste0("outputs/charts/overlapping/IOTC_WCPFC/species_doubled/single_double_declaration_",species, "_charts.png"), plot, width = 12, height = 8, dpi = 300)
})


#### For single species 

plot_list_single <- map(species_unique, function(species){
  ggplot(data = filter(CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IOTC_WCPFC_DATA_SUMMARY_COMP, species == !!species)) +
    geom_sf(data = COUNTRIES_SF, size = 0.2, fill = "darkgrey", color = NA) + 
    geom_sf(aes(geometry = the_geom, fill = Double), size = 3) +
    facet_grid(~ unit) + 
    scale_fill_manual(values = c("DOUBLE" = "green", "IOTC" = "blue", "WCPFC" = "red"), labels = c("DOUBLE" ="Double declaration", "IOTC" ="IOTC declaration", "WCPFC" ="WCPFC declaration")) +
    labs(x = "", y = "", title = paste0("Map of ", species)) +
    theme_bw() +
    theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", linewidth = 0.3), 
          panel.background = element_rect(fill = "white"))
})
# Save each plot as a file with the name of the species
walk2(plot_list_single, species_unique, function(plot, species){
  ggsave(paste0("outputs/charts/overlapping/IOTC_WCPFC/species_unique/single_double_declaration_",species, "_charts.png"), plot, width = 12, height = 8, dpi = 300)
})


# Create a list of ggplots, one for each species double declared, we are only using the doubled declared species from now

require(cowplot)

plot_list <- map(species_doubled, function(species){
  p <- cowplot::plot_grid(
    ggplot() +
      geom_sf(data = COUNTRIES_SF, size = .2, fill = "darkgrey", col = NA) + 
      theme(panel.grid.major = element_line(color = gray(0.9), linetype = "dashed", linewidth = 0.1)) + 
      geom_sf(data = CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IOTC_WCPFC_DATA_SUMMARY_COMP_CLEANED %>% 
                ungroup()  %>% 
                filter(species == !!species) %>% filter(unit == "Number of fish") %>% group_by(geographic_identifier, the_geom) %>% summarise(`Diff (IOTC-WCPFC)` = sum(`Diff (IOTC-WCPFC)`)), 
              aes(geometry = the_geom, fill = `Diff (IOTC-WCPFC)`), size = 3) +
      # facet_wrap(~unit)+   # add facet for GRIDTYPE
      theme_bw() +
      labs(x = "", y = "", title = "") +
      theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.3), 
            panel.background = element_rect(fill = "white")) + 
      scale_fill_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) + 
      guides(fill = guide_legend(title = "`Diff (IOTC-WCPFC)`")),
    
    ggplot() +
      geom_sf(data = COUNTRIES_SF, size = .2, fill = "darkgrey", col = NA) + 
      theme(panel.grid.major = element_line(color = gray(0.9), linetype = "dashed", linewidth = 0.1)) + 
      geom_sf(data = CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IOTC_WCPFC_DATA_SUMMARY_COMP_CLEANED %>% 
                ungroup()  %>% 
                filter(species == !!species) %>% filter(unit == "Tons")%>% group_by(geographic_identifier, the_geom) %>% summarise(`Diff (IOTC-WCPFC)` = sum(`Diff (IOTC-WCPFC)`)), 
              aes(geometry = the_geom, fill = `Diff (IOTC-WCPFC)`), size = 3) +
      # facet_wrap(~unit)+   # add facet for GRIDTYPE
      theme_bw() +
      labs(x = "", y = "", title = "") +
      theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.3), 
            panel.background = element_rect(fill = "white")) + 
      scale_fill_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) + 
      guides(fill = guide_legend(title = "`Diff (IOTC-WCPFC)`")), labels  = c("Number of fish", "Tons"))
  title <- ggdraw() + draw_label(paste0("Map of ", species), fontface='bold')
  p <- plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))
  
  plot <- ggplot() +
    geom_line(data = CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IOTC_WCPFC_DATA_SUMMARY_COMP_CLEANED %>% 
                group_by(unit, RFMO, species, Year) %>%
                summarise(value =sum(value)) %>% 
                ungroup() %>% 
                filter(species == !!species), 
              aes(x = Year, y = value, color = RFMO)) +
    geom_point(shape = "circle", size = 1.5) +
    scale_color_hue(direction = 1) +
    theme_minimal()+
    facet_wrap(vars(unit), scales = "free_y")+
    ggtitle(paste0("Time series captures for ", species, " in the overlapping zone IOTC / WCPFC"))
  
  
  return(plot_grid(p,plot, nrow = 2))
})

# Save each plot as a file with the name of the species
walk2(plot_list, species_doubled, function(plot, species){
  ggsave(paste0("outputs/charts/overlapping/IOTC_WCPFC/species_doubled/double_declaration_",species, "_charts_and_plots.png"), plot, width = 12, height = 8, dpi = 300)
})

## Analaysis of spatial resolution of the declarations in the overlapping area



CWP_GRIDS_IN_OVERLAPPING_ZONES_IOTC_WCPFC <- CWP_GRIDS_IN_OVERLAPPING_ZONES %>% filter(declarant_names == "Indian ocean, West Pacific ocean")

CA_IN_OVERLAPPING_ZONES_IOTC_WCPFC <- CA_RAW %>% filter(source_authority!="CCSBT") %>% inner_join(CWP_GRIDS_IN_OVERLAPPING_ZONES_IOTC_WCPFC, by = c("geographic_identifier" = "CWP_CODE"))

CA_IN_OVERLAPPING_ZONES_IOTC_WCPFC_SUMMARY_GRIDTYPE <- CA_IN_OVERLAPPING_ZONES_IOTC_WCPFC %>% 
  dplyr::mutate(unit = case_when(unit %in% c("MT","MTNO")~ "Tons", unit %in% c( "NOMT", "NO")~"Number of fish")) %>% 
  group_by(GRIDTYPE, source_authority, unit) %>% summarise(value =sum(value))


write.xlsx(CA_IN_OVERLAPPING_ZONES_IOTC_WCPFC_SUMMARY_GRIDTYPE, "outputs/datasets/CA_IN_OVERLAPPING_ZONES_IOTC_WCPFC_SUMMARY_GRIDTYPE.xlsx")


CA_IN_OVERLAPPING_ZONES_IOTC_WCPFC_SUMMARY_SPECIES <- CA_IN_OVERLAPPING_ZONES_IOTC_WCPFC %>% 
  dplyr::mutate(unit = case_when(unit %in% c("MT","MTNO")~ "Tons", unit %in% c( "NOMT", "NO")~"Number of fish")) %>% 
  group_by(species, source_authority, unit) %>% summarise(value =sum(value))


write.xlsx(CA_IN_OVERLAPPING_ZONES_IOTC_WCPFC_SUMMARY_SPECIES, "outputs/datasets/CA_IN_OVERLAPPING_ZONES_IOTC_WCPFC_SUMMARY_SPECIES.xslx")


############## ANALYSIS OF DATA FOR SPECIES WITH OVERLAPS ###############










