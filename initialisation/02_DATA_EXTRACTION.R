print("Extracting catch data from the GTA database...")

# Annual nominal catches
if (!file.exists(here("inputs/data/NC.rds"))){
NC <- data.table(dbGetQuery(con_GTA, "SELECT source_authority, fishingfleet, gear, gear_group, species, species_group, schooltype, TO_CHAR(time_start, 'YYYY-MM-DD') AS date_start, TO_CHAR(time_end, 'YYYY-MM-DD') AS date_end, month, quarter, year, time_period, id_area, geographic_identifier, catchtype, unit, geom_wkt, longitude, latitude, value
FROM fact_tables.global_nominal_catch_firms_level0;"))
saveRDS(NC, here("inputs/data/NC.rds"))} else {NC = readRDS(here("inputs/data/NC.rds"))}




# Full geo-referenced catch data set
if (!file.exists(here("inputs/data/CA.rds"))){
  CA = data.table(dbGetQuery(con_GTA, "SELECT source_authority, fishingfleet, gear, gear_group, species, species_group, schooltype, TO_CHAR(time_start, 'YYYY-MM-DD') AS time_start, TO_CHAR(time_end, 'YYYY-MM-DD') AS time_end, month, quarter, year, time_period, id_area, geographic_identifier, catchtype, unit, geom_wkt, longitude, latitude, value
                             FROM fact_tables.global_catch_firms_level0"))
  saveRDS(CA, here("inputs/data/CA.rds"))} else {CA = readRDS(here("inputs/data/CA.rds"))}

# Geo-referenced catch data set by month and 5°x5° grid area
if (!file.exists(here("inputs/data/CA_55M.rds"))){
CA_55M = data.table(dbGetQuery(con_GTA, "SELECT source_authority, fishingfleet, gear, gear_group, species, species_group, schooltype, TO_CHAR(time_start, 'YYYY-MM-DD') AS time_start, TO_CHAR(time_end, 'YYYY-MM-DD') AS time_end, month, quarter, year, time_period, id_area, geographic_identifier, catchtype, unit, geom_wkt, longitude, latitude, value
                               FROM fact_tables.global_catch_5deg_1m_firms_level0"))
saveRDS(CA_55M, here("inputs/data/CA_55M.rds"))} else {CA_55M = readRDS(here("inputs/data/CA_55M.rds"))}

# Purse seine and baitboat geo-referenced catch data set by month and 1°x1° grid area 
if (!file.exists(here("inputs/data/CA_PS_BB_11M.rds"))){
  CA_PS_BB_11M = data.table(dbGetQuery(con_GTA, "SELECT source_authority, fishingfleet, gear, gear_group, species, species_group, schooltype, TO_CHAR(time_start, 'YYYY-MM-DD') AS time_start, TO_CHAR(time_end, 'YYYY-MM-DD') AS time_end, month, quarter, year, time_period, id_area, geographic_identifier, catchtype, unit, geom_wkt, longitude, latitude, value
                                       FROM fact_tables.global_catch_1deg_1m_ps_bb_firms_level0;"))
  saveRDS(CA_PS_BB_11M, here("inputs/data/CA_PS_BB_11M.rds"))} else {CA_PS_BB_11M = readRDS(here("inputs/data/CA_PS_BB_11M.rds"))}

print("Catch data extracted!")
