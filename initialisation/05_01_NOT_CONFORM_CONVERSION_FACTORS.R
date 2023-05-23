print('Not conform conversion factors')

CA_RAW <- readRDS(here("inputs/data/mapped_codelist.rds"))  %>% 
  dplyr::mutate(unit = case_when(unit %in% c("MT","t")~ "MT", unit %in% c( "NO", "no")~"NO", TRUE ~ unit)) %>% filter(species %in% cwp_codes)


# Filter rows with unit "NOMT" and "MTNO"
strata_nomt <- CA_RAW %>%
  filter(unit == "NOMT")
strata_mtno <- CA_RAW %>%
  filter(unit == "MTNO")

# Perform inner joins and calculate sums
conversion_factor_level0 <- bind_rows(
  # Join strata_nomt with strata_mtno, rename columns, and calculate sums
  inner_join(strata_nomt, strata_mtno, by = setdiff(names(strata_mtno), c("value", "unit"))) %>%
    rename(NO = value.x, MT = value.y) %>%
    group_by(gear, time_start, time_end, geographic_identifier, species, source_authority, schooltype, catchtype, fishingfleet) %>%
    summarise(NO = sum(NO), MT = sum(MT)) %>%
    ungroup(),
  
  # Join strata_mtno with strata_nomt, rename columns, and calculate sums
  inner_join(strata_mtno, strata_nomt, by = setdiff(names(strata_mtno), c("value", "unit"))) %>%
    rename(MT = value.x, NO = value.y) %>%
    group_by(gear, time_start, time_end, geographic_identifier, species, source_authority, schooltype, catchtype, fishingfleet) %>%
    summarise(NO = sum(NO), MT = sum(MT)) %>%
    ungroup()
) %>%
  distinct() %>%
  group_by(gear, time_start, time_end, geographic_identifier, species, source_authority, schooltype, catchtype, fishingfleet) %>%
  summarise(NO = sum(NO), MT = sum(MT)) %>%
  mutate(conversion_factor = MT/NO) %>%
  distinct()

####

# `%notin%` <- Negate(`%in%`)
# absurd_conversion_factor <- conversion_factor_level0 %>% 
#   dplyr::filter((conversion_factor>0.05 & species == "SKJ") | (conversion_factor > 0.09 & species == "ALB") |(conversion_factor>0.2 & species == "YFT") | (conversion_factor >0.5 & species =="SWO")| (conversion_factor>0.2 &species == "BET"))
# 



max_conversion_factor <- read_csv("data/max_conversion_factor.csv", col_types = cols(max_weight = col_double(), min_weight = col_double()))

percent_outside <- 100 - (100 * (((a %>% filter(conform == "INSIDE"))$sum) - ((a %>% filter(conform != "INSIDE"))$sum)) / ((a %>% filter(conform == "INSIDE"))$sum))





