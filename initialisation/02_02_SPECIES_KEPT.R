
fish_GTA <- readr::read_csv("https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/firms/gta/cl_species_level0.csv")

fish_species <- unique(fish_GTA$label)
cwp_codes <- unique(fish_GTA$code)


filter_species = TRUE
