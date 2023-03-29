print("Defining colors and symbols for tropical tuna species...")

# SPECIES COLORS AND SHAPES
SPECIES_COL_SHAPE    = data.table(species_code_fao = c("BET", "YFT", "SKJ"), species = c("Bigeye tuna", "Yellowfin tuna", "Skipjack tuna"), FILL = pal_jco(alpha = 0.6)(3), OUTLINE = darken(pal_jco(alpha = 0.6)(3), 0.2), SHAPE = c(1, 2, 0)) # c(19, 17, 15))

# OCEAN COLORS AND SHAPES
OCEAN_COL_SHAPE    = data.table(ocean_code = c("AO", "IO", "WCPO", "EPO"), ocean = c("Atlantic ocean", "Indian Ocean", "Western-Central Pacific Ocean", "Eastern Pacific Ocean"), FILL = pal_d3(alpha = 1)(4), OUTLINE = darken(pal_d3(alpha = 1)(4), 0.2), SHAPE = c(19, 17, 18, 15))

# SEX COLORS AND SHAPES
SEX_COL_SHAPE    = data.table(sex = c("M", "F", "Ind", "unknown"), sex_label = c("Male", "Female", "Indeterminate", "Unknown"), FILL = pal_jco(alpha = 1)(4), OUTLINE = darken(pal_jco(alpha = 1)(4), 0.2), SHAPE = c(1, 2, 3, 4))

print("Colors and symbols defined")