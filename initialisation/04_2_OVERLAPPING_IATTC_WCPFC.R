print('IATTC and WCPFC overlap analyse')


CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IATTC_WCPFC_DATA <- CA_IN_OVERLAPPING_ZONES_AGGREGATED_5DEG %>%
  dplyr::filter(declarant_names == "East Pacific ocean, West Pacific ocean") %>% filter(source_authority %in% c("IATTC", "WCPFC"))




CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IATTC_WCPFC_DATA_SUMMARY <- CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IATTC_WCPFC_DATA %>% 
   dplyr::mutate(unit = case_when(unit %in% c("MT","MTNO","t")~ "Tons", unit %in% c( "NOMT", "NO", "no")~"Number of fish")) %>% 
  mutate(Year = lubridate::year(time_start)) %>% 
  group_by(unit, source_authority, species, geographic_identifier, Year) %>% summarise(value = sum(value))



CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IATTC_WCPFC_DATA_SUMMARY_COMP <- 
  CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IATTC_WCPFC_DATA_SUMMARY %>%
  pivot_wider(names_from = "source_authority", values_from = "value") %>% inner_join(CWP_GRIDS, by = c("geographic_identifier" = "CWP_CODE")) %>%
  mutate(IATTC = ifelse(is.na(IATTC), 0, IATTC)) %>% 
  mutate(WCPFC = ifelse(is.na(WCPFC), 0, WCPFC)) %>% 
  mutate(Diff = ((IATTC-WCPFC))) %>% ungroup()  %>% rowwise() %>% 
  mutate(Double = as.factor(ifelse(is.na(IATTC)| is.na(WCPFC)|IATTC == 0 | WCPFC == 0 , "SINGLE", "DOUBLE")))
  

CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IATTC_WCPFC_DATA_SUMMARY_COMP_CLEANED <- CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IATTC_WCPFC_DATA_SUMMARY_COMP %>% 
  pivot_longer(cols = c("IATTC", "WCPFC"), 
               names_to = "RFMO", 
               values_to = "value")

library(purrr)

CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IATTC_WCPFC_DATA_SUMMARY_COMP_doubled_geographic <- CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IATTC_WCPFC_DATA_SUMMARY_COMP %>% 
  filter(Double == "DOUBLE")

# Create a list of plot of double and single area for each species 

plot_list_single_double <- map(unique(CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IATTC_WCPFC_DATA_SUMMARY_COMP_doubled_geographic$species), function(species){
  ggplot(data = filter(CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IATTC_WCPFC_DATA_SUMMARY_COMP_doubled_geographic, species == !!species)) +
    geom_sf(data = COUNTRIES_SF, size = 0.2, fill = "darkgrey", color = NA) + 
    geom_sf(aes(geometry = the_geom, fill = Double), size = 3) +
    facet_grid(~ unit) + 
    scale_fill_manual(values = c("DOUBLE" = "red", "SINGLE" = "blue"), labels = c("DOUBLE" ="Double declaration", "SINGLE" ="Single declaration")) +
    labs(x = "", y = "", title = paste0("Map of ", species)) +
    theme_bw() +
    theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", linewidth = 0.3), 
          panel.background = element_rect(fill = "white"))
})
# Save each plot as a file with the name of the species
walk2(plot_list_single_double, unique(CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IATTC_WCPFC_DATA_SUMMARY_COMP_doubled_geographic$species), function(plot, species){
  ggsave(paste0("outputs/charts/overlapping/IATTC_WCPFC/single_double_declaration_",species, "_charts.png"), plot, width = 12, height = 8, dpi = 300)
})

# Create a list of ggplots, one for each species

require(cowplot)

plot_list <- map(unique(CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IATTC_WCPFC_DATA_SUMMARY_COMP_doubled_geographic$species), function(species){
  p <- cowplot::plot_grid(
    ggplot() +
    geom_sf(data = COUNTRIES_SF, size = .2, fill = "darkgrey", col = NA) + 
    theme(panel.grid.major = element_line(color = gray(0.9), linetype = "dashed", linewidth = 0.1)) + 
    geom_sf(data = CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IATTC_WCPFC_DATA_SUMMARY_COMP_doubled_geographic %>% 
              ungroup()  %>% 
              filter(species == !!species) %>% filter(unit == "Number of fish") %>% group_by(geographic_identifier, the_geom) %>% summarise(Diff = sum(Diff)), 
            aes(geometry = the_geom, fill = Diff), size = 3) +
    # facet_wrap(~unit)+   # add facet for GRIDTYPE
    theme_bw() +
    labs(x = "", y = "", title = "") +
    theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.3), 
          panel.background = element_rect(fill = "white")) + 
    scale_fill_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) + 
    guides(fill = guide_legend(title = "Diff")),
    
    ggplot() +
      geom_sf(data = COUNTRIES_SF, size = .2, fill = "darkgrey", col = NA) + 
      theme(panel.grid.major = element_line(color = gray(0.9), linetype = "dashed", linewidth = 0.1)) + 
      geom_sf(data = CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IATTC_WCPFC_DATA_SUMMARY_COMP_doubled_geographic %>% 
                ungroup()  %>% 
                filter(species == !!species) %>% filter(unit == "Tons")%>% group_by(geographic_identifier, the_geom) %>% summarise(Diff = sum(Diff)), 
              aes(geometry = the_geom, fill = Diff), size = 3) +
      # facet_wrap(~unit)+   # add facet for GRIDTYPE
      theme_bw() +
      labs(x = "", y = "", title = "") +
      theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.3), 
            panel.background = element_rect(fill = "white")) + 
      scale_fill_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) + 
      guides(fill = guide_legend(title = "Diff")), labels  = c("Number of fish", "Tons"))
  title <- ggdraw() + draw_label(paste0("Map of ", species), fontface='bold')
  plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))
})

# Save each plot as a file with the name of the species
walk2(plot_list, unique(CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IATTC_WCPFC_DATA_SUMMARY_COMP_doubled_geographic$species), function(plot, species){
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


############## ANALYSIS OF DATA FOR SPECIES WITH OVERLAPS ###############

CA_DOUBLE_DECLARATION_IATTC_WCPFC<- CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IATTC_WCPFC_DATA_SUMMARY_COMP %>% 
  # dplyr::filter(Double == "DOUBLE") %>% 
  ungroup()

species_overlaped <- unique((CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IATTC_WCPFC_DATA_SUMMARY_COMP %>% 
                              dplyr::filter(Double == "DOUBLE") %>%
                              ungroup())$species)



# IATTC_WCPFC_sup <- CA_GRID_DOUBLE_DECLARATIONS_GROUPPED_GRID_IATTC_WCPFC_DATA_SUMMARY_COMP %>% 
#   dplyr::mutate(SUP = case_when(IATTC > WCPFC ~ "IATTC", WCPFC > IATTC ~ "WCPFC", TRUE ~ " EQUAL"))


plot_list <- map(species_overlaped, function(species){
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
    ggtitle(paste0("Time series captures for ", species, " in the overlapping zone IATTC / WCPFC"))
  
})

walk2(plot_list, species_overlaped, function(plot, species){
  ggsave(paste0("outputs/charts/overlapping/IATTC_WCPFC/double_declaration_",species, "_plots.png"), plot, width = 12, height = 8, dpi = 300)
})





