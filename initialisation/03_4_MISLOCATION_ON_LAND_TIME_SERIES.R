




time_series_mislocated <- lapply(list(CA_ON_LAND), function(x){
  x <- x %>%dplyr::mutate(Time = as.Date(time_start)) %>%
    dplyr::group_by(Time, unit) %>% 
    dplyr::summarise(Values = sum(value))
  
  
  ggplot(x) +
    aes(
      x = Time,
      y = Values
    ) +
    geom_point(size = 0.5) +
    scale_fill_hue(direction = 1) +
    scale_color_hue(direction = 1) +
    theme_dark() +
    theme(legend.position = "top") +
    facet_wrap(vars(unit), nrow = 2L)+
    labs(x = "Time", y = "Values")+
    facet_grid("unit", scales = "free_y")
  })

ggsave(paste0(here(),"/outputs/charts/mislocation/time_series_mislocated.png" ), time_series_mislocated[[1]], width = 8, height = 7)


time_series_mislocated_by_trfmo <- lapply(list(CA_ON_LAND), function(x){
  x <- x %>%dplyr::mutate(Time = as.Date(time_start)) %>%
    dplyr::group_by(Time, unit, source_authority) %>% 
    dplyr::summarise(Values = sum(value))
  
  
  ggplot(x) +
    aes(
      x = Time,
      y = Values
    ) +
    geom_point(size = 0.5) +
    scale_fill_hue(direction = 1) +
    scale_color_hue(direction = 1) +
    theme_dark() +
    theme(legend.position = "top") +
    facet_wrap(vars(unit), nrow = 2L)+
    labs(x = "Time", y = "Values")+
    facet_grid(c("unit", "source_authority"), scales = "free_y")
})

ggsave(paste0(here(),"/outputs/charts/mislocation/time_series_mislocated_by_trfmo.png" ), time_series_mislocated_by_trfmo[[1]], width = 8, height = 7)
  


