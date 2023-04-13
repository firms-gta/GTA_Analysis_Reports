print("Mapping the catches on land....")

# Map the 1x1 PS and BB catch data on land ####
CA_PS_BB_11M_ON_LAND_GRID_UNIT_WKT = CA_PS_BB_11M_ON_LAND[, .(CATCH = sum(value, na.rm = TRUE)), keyby = .(GRID = geographic_identifier, UNIT = unit, WKT = geom_wkt)]
CA_PS_BB_11M_ON_LAND_GRID_UNIT_SF = st_as_sf(CA_PS_BB_11M_ON_LAND_GRID_UNIT_WKT, wkt = "WKT", crs = st_crs(4326))

## MERCATOR PROJECTION ####

### Catch in metric tonnes ####
CA_PS_BB_11M_ON_LAND_TONNES_MAP = 
  ggplot() + 
  geom_sf(data = COUNTRIES_SF, size = .2, fill = "darkgrey", col = NA) + 
  theme(panel.grid.major = element_line(color = gray(0.9), linetype = "dashed", linewidth = 0.5)) + 
  geom_sf(data = CA_PS_BB_11M_ON_LAND_GRID_UNIT_SF %>% filter(UNIT == "t"), aes(fill = CATCH), size = .5) + 
  theme_bw() +
  labs(x = "", y = "", title = "(a)") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.3), 
        panel.background = element_rect(fill = "white")) + 
  scale_fill_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) + 
  guides(fill = guide_legend(title = "Catch (t)"))

### Catch in numbers of fish #####
CA_PS_BB_11M_ON_LAND_NUMBERS_MAP = 
  ggplot() + 
  geom_sf(data = COUNTRIES_SF, size = .2, fill = "darkgrey", col = NA) + 
  theme(panel.grid.major = element_line(color = gray(0.9), linetype = "dashed", linewidth = 0.5)) + 
  geom_sf(data = CA_PS_BB_11M_ON_LAND_GRID_UNIT_SF %>% filter(UNIT == "no"), aes(fill = CATCH), size = .5) + 
  theme_bw() +
  labs(x = "", y = "", title = "(b)") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.3), 
        panel.background = element_rect(fill = "white")) + 
  scale_fill_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) + 
  guides(fill = guide_legend(title = "Catch (no)"))

CA_PS_BB_11M_ON_LAND_MAP = CA_PS_BB_11M_ON_LAND_TONNES_MAP / CA_PS_BB_11M_ON_LAND_NUMBERS_MAP 

ggsave(paste0(here(),"/outputs/charts/mislocation/CA_PS_BB_11M_ON_LAND_MAP.png"), CA_PS_BB_11M_ON_LAND_MAP, width = 8, height = 7)

# Map the 5x5 catch data on land ####
CA_55M_ON_LAND_GRID_UNIT_WKT = CA_55M_ON_LAND[, .(CATCH = sum(value, na.rm = TRUE)), keyby = .(GRID = geographic_identifier, UNIT = unit, WKT = geom_wkt)]

CA_55M_ON_LAND_GRID_UNIT_SF = st_as_sf(CA_55M_ON_LAND_GRID_UNIT_WKT, wkt = "WKT", crs = st_crs(4326))

## MERCATOR PROJECTION ####

### Catch in metric tonnes ####
CA_55M_ON_LAND_TONNES_MAP = 
  ggplot() + 
  geom_sf(data = COUNTRIES_SF, size = .2, fill = "darkgrey", col = NA) + 
  theme(panel.grid.major = element_line(color = gray(0.9), linetype = "dashed", linewidth = 0.5)) + 
  geom_sf(data = CA_55M_ON_LAND_GRID_UNIT_SF %>% filter(UNIT == "t"), aes(fill = CATCH), size = .5) + 
  theme_bw() +
  labs(x = "", y = "", title = "(a)") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.3), 
        panel.background = element_rect(fill = "white")) + 
  scale_fill_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) + 
  guides(fill = guide_legend(title = "Catch (t)"))

### Catch in numbers of fish ####
CA_55M_ON_LAND_NUMBERS_MAP = 
  ggplot() + 
  geom_sf(data = COUNTRIES_SF, size = .2, fill = "darkgrey", col = NA) + 
  theme(panel.grid.major = element_line(color = gray(0.9), linetype = "dashed", linewidth = 0.5)) + 
  geom_sf(data = CA_55M_ON_LAND_GRID_UNIT_SF %>% filter(UNIT == "no"), aes(fill = CATCH), size = .5) + 
  theme_bw() +
  labs(x = "", y = "", title = "(b)") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.3), 
        panel.background = element_rect(fill = "white")) + 
  scale_fill_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) + 
  guides(fill = guide_legend(title = "Catch (no)"))

CA_55M_ON_LAND_MAP = CA_55M_ON_LAND_TONNES_MAP / CA_55M_ON_LAND_NUMBERS_MAP 

ggsave(paste0(here(),"/outputs/charts/mislocation/CA_55M_ON_LAND_MAP.png"),CA_55M_ON_LAND_MAP, width = 8, height = 7)

# ROBINSON PACIFIC-CENTRED PROJECTION ####
sf::sf_use_s2(FALSE)
rob_pacific = "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

COUNTRIES_PAC_SF = fSpatPlan_Convert2PacificRobinson(COUNTRIES_UNDISPUTED_SF)
CA_55M_ON_LAND_GRID_UNIT_SF_PAC = st_transform(CA_55M_ON_LAND_GRID_UNIT_SF, crs = rob_pacific)

# Catch in metric tonnes
CA_55M_ON_LAND_TONNES_MAP_PAC = 
  ggplot() + 
  geom_sf(data = COUNTRIES_PAC_SF, size = .2, fill = "darkgrey", col = NA) + 
  theme(panel.grid.major = element_line(color = gray(0.9), linetype = "dashed", linewidth = 0.5)) + 
  geom_sf(data = CA_55M_ON_LAND_GRID_UNIT_SF_PAC %>% filter(UNIT == "t"), aes(fill = CATCH), size = .5) + 
  theme_bw() +
  labs(x = "", y = "", title = "(a)") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.3), 
        panel.background = element_rect(fill = "white")) + 
  scale_fill_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) + 
  guides(fill = guide_legend(title = "Catch (t)"))

# Catch in numbers of fish
CA_55M_ON_LAND_NUMBERS_MAP_PAC = 
  ggplot() + 
  geom_sf(data = COUNTRIES_PAC_SF, size = .2, fill = "darkgrey", col = NA) + 
  theme(panel.grid.major = element_line(color = gray(0.9), linetype = "dashed", linewidth = 0.5)) + 
  geom_sf(data = CA_55M_ON_LAND_GRID_UNIT_SF_PAC %>% filter(UNIT == "no"), aes(fill = CATCH), size = .5) + 
  theme_bw() +
  labs(x = "", y = "", title = "(b)") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.3), 
        panel.background = element_rect(fill = "white")) + 
  scale_fill_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) + 
  guides(fill = guide_legend(title = "Catch (no)"))

CA_55M_ON_LAND_MAP_PAC = CA_55M_ON_LAND_TONNES_MAP_PAC / CA_55M_ON_LAND_NUMBERS_MAP_PAC 

ggsave(paste0(here(),"/outputs/charts/mislocation/CA_55M_ON_LAND_MAP_PAC.png"), CA_55M_ON_LAND_MAP_PAC, width = 8, height = 7)
