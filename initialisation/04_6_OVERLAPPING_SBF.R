print('SBF data anlyse')

CA_SBF <- CA_RAW %>% dplyr::filter(species == "SBF")
CA_SBF_GRID <- CA_SBF%>% inner_join(CWP_GRIDS, by = c("geographic_identifier" = "CWP_CODE")) 

###############################################################

CA_SBF_AGGREGATED <- aggregate_resolution(con = con_GTA, df_input = CA_SBF_GRID, resolution = 6)$df


converted_overlapping_SBF <- conversion(con = con,
                                    fact="catch",
                                    unit_conversion_csv_conversion_factor_url="https://drive.google.com/open?id=1csQ5Ww8QRTaYd1DG8chwuw0UVUOGkjNL",
                                    unit_conversion_codelist_geoidentifiers_conversion_factors="areas_tuna_rfmos_task2",
                                    mapping_map_code_lists=TRUE,
                                    georef_dataset=CA_SBF_AGGREGATED, 
                                    removing_numberfish_final = FALSE) # do not remove number of fish as they will be converted later with other conversion factor data


###############################################################################


CA_SBF_GRIDTYPE <- CA_SBF_GRID %>% 
  dplyr::mutate(unit = case_when(unit %in% c("MT","MTNO")~ "Tons", unit %in% c( "NOMT", "NO")~"Number of fish")) %>% 
  group_by(GRIDTYPE, source_authority, unit) %>% summarise(value =sum(value))



CA_SBF_GRIDTYPE_SUMMARY <- converted_overlapping_SBF %>% 
  dplyr::mutate(unit = case_when(unit %in% c("MT","MTNO","t")~ "Tons", unit %in% c( "NOMT", "NO", "no")~"Number of fish")) %>% 
  mutate(Year = (year(time_start))) %>% 
  group_by(unit, source_authority, species, geographic_identifier, Year, GRIDTYPE) %>% summarise(value = sum(value)) %>% ungroup()






plot <- ggplot(CA_SBF_GRIDTYPE_SUMMARY) +
  aes(
    x = Year,
    colour = source_authority,
    group = source_authority,
    weight = value
  ) +
  geom_bar(position = "fill", fill = "#112446") +
  scale_color_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(unit), scales = "free_y")+
  theme(axis.text.x = element_text(angle = 90))


ggsave(here("outputs/charts/overlapping/time_plot_captures_of_sbf.png"),plot, width = 12, height = 8, dpi = 300)


CA_SBF_GRIDTYPE_SUMMARY_COMP <- 
  converted_overlapping_SBF %>%
  pivot_wider(names_from = "source_authority", values_from = "value") %>% 
  mutate(IOTC = ifelse(is.na(IOTC), 0, IOTC)) %>% 
  mutate(ICCAT = ifelse(is.na(ICCAT), 0, ICCAT)) %>% 
  mutate(CCSBT = ifelse(is.na(CCSBT), 0, CCSBT)) %>% 
  mutate(Double = as.factor(case_when( IOTC==0 & ICCAT == 0 ~ "CCSBT", CCSBT!=0 & (ICCAT != 0 | IOTC != 0 ) ~ "CCSBT and Other", TRUE ~ "Other")))

CA_SBF_GRIDTYPE_SUMMARY_COMP_GRID <- CA_SBF_GRIDTYPE_SUMMARY_COMP %>% pivot_longer(cols = c(IOTC, ICCAT, CCSBT), names_to = "source_authority", values_to = "value") %>%distinct() %>% 
  group_by(Double, unit, geographic_identifier, the_geom, GRIDTYPE) %>% summarise(value = sum(value))

CA_SBF_GRIDTYPE_SUMMARY_COMP_GRID_grid <- CA_SBF_GRIDTYPE_SUMMARY_COMP_GRID %>% group_by(unit, Double) %>% summarise(value =sum(value))


write.xlsx(CA_SBF_GRIDTYPE, "outputs/datasets/CA_SBF_GRIDTYPE.xlsx")


plot <-ggplot(data = filter(CA_SBF_GRIDTYPE_SUMMARY_COMP_GRID %>% filter(GRIDTYPE =="1deg_x_1deg"))) +
  geom_sf(data = COUNTRIES_SF, size = 0.2, fill = "darkgrey", color = NA) + 
  geom_sf(aes(geometry = the_geom, fill = Double), size = 3) +
  facet_grid(~ unit) + 
  scale_fill_manual(values = c("CCSBT" = "green", "CCSBT and Other" = "blue", "Other" = "red"), labels = c("CCSBT" ="CCSBT declaration", "CCSBT and Other" ="CCSBT and Other declaration", "Other" ="Other declaration")) +
  labs(x = "", y = "", title = paste0("Map of SBF")) +
  theme_bw() +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", linewidth = 0.3), 
        panel.background = element_rect(fill = "white"))

plot2 <-ggplot(data = filter(CA_SBF_GRIDTYPE_SUMMARY_COMP_GRID%>% filter(GRIDTYPE =="5deg_x_5deg")))+
  geom_sf(data = COUNTRIES_SF, size = 0.2, fill = "darkgrey", color = NA) + 
  geom_sf(aes(geometry = the_geom, fill = Double), size = 3) +
  facet_grid(~ unit) + 
  scale_fill_manual(values = c("CCSBT" = "green", "CCSBT and Other" = "blue", "Other" = "red"), labels = c("CCSBT" ="CCSBT declaration", "CCSBT and Other" ="CCSBT and Other declaration", "Other" ="Other declaration")) +
  labs(x = "", y = "", title = paste0("Map of SBF")) +
  theme_bw() +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", linewidth = 0.3), 
        panel.background = element_rect(fill = "white"))

plot3 <- plot_grid(plot, plot2, nrow = 2)


ggsave(here("outputs/charts/overlapping/map_plot.png"),plot3, width = 12, height = 8, dpi = 300)



















