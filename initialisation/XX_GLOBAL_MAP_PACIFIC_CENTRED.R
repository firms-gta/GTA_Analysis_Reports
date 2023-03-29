# Source function used for cleaning the projection on the Pacific
source("fSpatPlan_Convert2PacificRobinson.R")

sf::sf_use_s2(FALSE)

world_sf = ne_countries(scale = "large", returnclass = "sf")
world_pac_sf = fSpatPlan_Convert2PacificRobinson(world_sf)

rob_pacific = "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

WORLD_MAP_BLUE_BACKGROUNG = 
  ggplot(data = world_pac_sf) + 
  geom_sf(fill = "darkgrey", color = "darkgrey") + 
  geom_sf(data = wcpfc_rob_sf, fill = "red", color = "black") + 
  theme(panel.background = element_rect(fill = "aliceblue")) + 
  theme(panel.grid.major = element_line(color = "lightgrey", linetype = "dashed", linewidth = 0.3))

WORLD_MAP_BLACK_BACKGROUNG = 
ggplot(data = world_pac_sf) + 
  geom_sf(fill = "lightgrey", color = "lightgrey") + 
  theme(panel.background = element_rect(fill = "black")) + 
  theme(panel.grid.major = element_line(color = "darkgrey", linetype = "dashed", linewidth = 0.3))
      
