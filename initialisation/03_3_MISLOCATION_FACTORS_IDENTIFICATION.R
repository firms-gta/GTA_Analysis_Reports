print("Analysed of data on land initialisation !")

# Analyse of the data on land

require(studycatchesird)

# Initializing factors to explore

time_dimension <- c("time_start", "time_end")
geographical_dimension <- "geographic_identifier"
colnames_to_keep <- c("fishingfleet",         "gear",                 "time_start",                 
                      "geographic_identifier","schooltype",           "species",             # "catchtype",           
                      "unit",                 "value",                "source_authority")

list_pie_chart <- list(setdiff(colnames_to_keep, unlist(list(time_dimension, geographical_dimension, "unit", "value"))))[[1]]

pie_chart_plotting = function(dimension, first, topn = 4){
  
  first[is.na(first)] <- "NA"
  if (deparse(substitute(dimension)) == "X[[i]]") {
    r <- dimension
  }
  else {
    r <- deparse(substitute(dimension))
  }
  
  provisoire_i <- na.omit(first) %>% dplyr::group_by(dplyr::across(c(dimension, 
                                                                     "unit"))) %>% dplyr::summarise(value = sum(value, na.rm = TRUE)) %>% 
    dplyr::group_by(unit) %>% dplyr::arrange(desc(value)) %>% 
    dplyr::mutate(id = row_number()) %>% dplyr::mutate(class = as.factor(ifelse(id < 
                                                                                  topn, !!rlang::sym(dimension), "Others"))) %>% dplyr::group_by(class, 
                                                                                                                                                 unit) %>% dplyr::summarise(value = sum(value, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% dplyr::select(value, class, unit) %>% 
    dplyr::group_by(unit) %>% dplyr::mutate(pourcentage = prop.table(value) * 
                                              100) %>% dplyr::mutate(labels = paste0(pourcentage, 
                                                                                     " ", " % ")) %>% dplyr::arrange(desc(class)) %>% dplyr::mutate(ypos_ligne = cumsum(pourcentage) - 
                                                                                                                                                      0.5 * pourcentage) %>% dplyr::distinct() %>% dplyr::filter(!is.na(class))                                                                                                                                                    
  
  ggplot_i <- ggplot(provisoire_i %>% dplyr::filter(!is.na(class))) + 
    aes(x = "", fill = class, group = class, weight = pourcentage) + 
    geom_bar(position = "fill") + scale_fill_hue(direction = 1) + 
    scale_color_hue(direction = 1) + theme_minimal() + coord_polar("y", 
                                                                   start = 0) + geom_text(first = (provisoire_i %>% dplyr::filter(!is.na(class)) %>% 
                                                                                                     dplyr::mutate_if(is.numeric, round)), size = 3, aes(x = 1, 
                                                                                                                                                         y = ypos_ligne/100, label = paste0(round(pourcentage), 
                                                                                                                                                                                            "%")), color = "black") + theme(axis.ticks.x = element_blank(), 
                                                                                                                                                                                                                            axis.text.x = element_blank()) + labs(x = "", y = "") + 
    guides(fill = guide_legend(title = toupper(r))) + 
    facet_wrap("unit") + scale_fill_discrete(na.translate = F)
  
  return(ggplot_i) 
}



function_pie_chart_df <- function(dimension, ...){
  formals(pie_chart_plotting)$dimension <- dimension
  dimension_returned <- dimension
  pie_chart = pie_chart_plotting(...)
  return(list(pie_chart = pie_chart, dimension = dimension))
}

pie_charts_multiple <- lapply(list_pie_chart,FUN = function_pie_chart_df, first= CA_ON_LAND, topn = 5)

lapply(pie_charts_multiple, function(x){
  if(!dir.exists(here("outputs/charts/mislocation/pie_plot_factors"))){dir.create(here("outputs/charts/mislocation/pie_plot_factors"))}
  ggsave(paste0(here(),"/outputs/charts/mislocation/pie_plot_factors/",x$dimension,"_repartition_pie_plot.png" ), x$pie_chart, width = 8, height = 7)
  
})


print("Analysed of mislocated data done !")
