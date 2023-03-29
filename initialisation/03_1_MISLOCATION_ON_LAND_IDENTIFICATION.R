print("Identification of catch data mislocated on land...")

# 1x1 gridded data set ####

# Merge 1x1 catch data with grid
CA_PS_BB_11M_WITH_GRIDS = merge(CA_PS_BB_11M, CWP11, by.x = "geographic_identifier", by.y = "CWP_CODE")

# Identify catch data in grids on land
CA_PS_BB_11M_ON_LAND = CA_PS_BB_11M_WITH_GRIDS[ON_LAND_P == 100]

# Export data set
write.xlsx(CA_PS_BB_11M_ON_LAND[, -c("the_geom")], "../outputs/datasets/CA_PS_BB_11M_ON_LAND.xlsx")

# Summarize the results
CA_PS_BB_11M_ON_LAND_RFMO_UNIT = CA_PS_BB_11M_ON_LAND[, .(CATCH = sum(value, na.rm = TRUE)), keyby = .(RFMO = source_authority, UNIT = unit)]
CA_PS_BB_11M_ON_LAND_GRID_UNIT = CA_PS_BB_11M_ON_LAND[, .(CATCH = sum(value, na.rm = TRUE)), keyby = .(GRID = geographic_identifier, UNIT = unit)]

# 5x5 gridded data set ####

# Merge 5x5 catch data with grid
CA_55M_WITH_GRIDS = merge(CA_55M, CWP55, by.x = "geographic_identifier", by.y = "CWP_CODE")

# Identify catch data in grids on land
CA_55M_ON_LAND = CA_55M_WITH_GRIDS[ON_LAND_P == 100]

# Export data set
write.xlsx(CA_55M_ON_LAND[, -c("the_geom")], "../outputs/datasets/CA_55M_ON_LAND.xlsx")

# Summarize the results
CA_55M_ON_LAND_RFMO_UNIT = CA_55M_ON_LAND[, .(CATCH = sum(value, na.rm = TRUE)), keyby = .(RFMO = source_authority, UNIT = unit)]
CA_55M_ON_LAND_GRID_UNIT = CA_55M_ON_LAND[, .(CATCH = sum(value, na.rm = TRUE)), keyby = .(GRID = geographic_identifier, UNIT = unit)]
                                                                                         
# Full gridded data set ####

# Merge 5x5 catch data with grid
CA_WITH_GRIDS = merge(CA, CWP_GRIDS, by.x = "geographic_identifier", by.y = "CWP_CODE")

# Identify catch data in grids on land
CA_ON_LAND = CA_WITH_GRIDS[ON_LAND_P == 100]

# Export data set
write.xlsx(CA_ON_LAND[, -c("the_geom")], "../outputs/datasets/CA_ON_LAND.xlsx")

# Summarize the results
CA_ON_LAND_RFMO_UNIT = CA_ON_LAND[, .(CATCH = sum(value, na.rm = TRUE)), keyby = .(RFMO = source_authority, UNIT = unit)]
CA_ON_LAND_GRID_UNIT = CA_ON_LAND[, .(CATCH = sum(value, na.rm = TRUE)), keyby = .(GRID = geographic_identifier, UNIT = unit)]
CA_ON_LAND_GRIDTYPE_UNIT = CA_ON_LAND[, .(CATCH = sum(value, na.rm = TRUE)), keyby = .(GRIDTYPE, UNIT = unit)]

print("Catch data mislocated on land initialized!")
