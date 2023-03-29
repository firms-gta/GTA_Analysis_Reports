print("Extracting catch data from the GTA database...")

# Annual nominal catches
NC = data.table(dbGetQuery(con_GTA, "SELECT * FROM fact_tables.global_nominal_catch_firms_level0;"))

# Full geo-referenced catch data set
if (!file.exists("../inputs/data/CA.rds")){
  CA = data.table(dbGetQuery(con_GTA, "SELECT * FROM fact_tables.global_catch_firms_level0 WHERE year = 2019;"))
  saveRDS(CA, "../inputs/data/CA.rds")} else CA = readRDS("../inputs/data/CA.rds")

# Geo-referenced catch data set by month and 5°x5° grid area
if (!file.exists("../inputs/data/CA_55M.rds")){
CA_55M = data.table(dbGetQuery(con_GTA, "SELECT * FROM fact_tables.global_catch_5deg_1m_firms_level0"))
saveRDS(CA_55M, "../inputs/data/CA_55M.rds")} else CA_55M = readRDS("../inputs/data/CA_55M.rds")

# Purse seine and baitboat geo-referenced catch data set by month and 1°x1° grid area 
if (!file.exists("../inputs/data/CA_PS_BB_11M.rds")){
  CA_PS_BB_11M = data.table(dbGetQuery(con_GTA, "SELECT * FROM fact_tables.global_catch_1deg_1m_ps_bb_firms_level0;"))
  saveRDS(CA_PS_BB_11M, "../inputs/data/CA_PS_BB_11M.rds")} else CA_PS_BB_11M = readRDS("../inputs/data/CA_PS_BB_11M.rds")

print("Catch data extracted!")