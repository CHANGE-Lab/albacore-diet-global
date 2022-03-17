################################################################################################
# Map Longhurst provinces, align them with locations of albacore diet sampling
################################################################################################

#### Workspace ----

library(sf)
library(rnaturalearth)
library(ggplot2)
library(dplyr)
library(rgeos)
library(sp)
library(rgdal)
library(here)
"%notin%" = Negate('%in%')
here::here()

#### Map base ----

# Base map
world_map <- rnaturalearth::ne_countries(scale = 'small', returnclass = c("sf"))
baseMap <- ggplot() +
  geom_sf(data = world_map, size = .2, fill = "gray80", col = "gray90") +
  theme(panel.grid.major = element_line(color = gray(0.9), linetype = "dashed", size = 0.5))
baseMap

# Add longhurst regions information:
longhurst <- sf::read_sf(here::here("code/longhurst/Longhurst_world_v4_2010.shp")) 
# or wherever you have it stored
names(longhurst)
head(longhurst)

# simplify the object to make it 'usable'
# Gives a warning, one/some of the regions may be off?
sf::sf_use_s2(FALSE)
longhurst <- longhurst %>% 
  sf::st_simplify(dTolerance = 0.01) %>% 
  dplyr::group_by(ProvCode,ProvDescr) %>% 
  dplyr::summarise()

#Previous warning message:
#Warning messages:
#1: In st_is_longlat(x) :
#  bounding box has potentially an invalid value range for longlat data
#2: In st_simplify.sfc(st_geometry(x), preserveTopology, dTolerance) :
#  st_simplify does not correctly simplify longitude/latitude data, dTolerance needs to be in decimal degrees

#Now we get this warning message
#Error in s2_geography_from_wkb(x, oriented = oriented, check = check) : 
#Evaluation error: Found 3 features with invalid spherical geometry.
#[11] Loop 0 is not valid: Edge 4176 has duplicate vertex with edge 4180
#[43] Loop 8 is not valid: Edge 2120 has duplicate vertex with edge 2137
#[48] Loop 91 is not valid: Edge 216 has duplicate vertex with edge 224.
#In addition: Warning messages:
#1: In st_is_longlat(x) :
#  bounding box has potentially an invalid value range for longlat data
#2: In st_simplify.sfc(st_geometry(x), preserveTopology, dTolerance) :
#  argument preserveTopology is ignored
#3: In st_is_longlat(x) :
#  bounding box has potentially an invalid value range for longlat data

plot(longhurst)
glimpse(longhurst)

# Plot provinces
longMap <- baseMap + 
  geom_sf(data = longhurst, aes(fill = ProvCode), size = .2, col = "grey50", alpha=.4)+
  ggtitle(paste("Longhurst Biogeochemical Provinces -", length(unique(longhurst$ProvCode)),"provinces"))+
  theme(legend.position="none",
        axis.title = element_blank())+
  geom_sf_text(data = longhurst %>% dplyr::group_by(ProvCode) %>% dplyr::summarize(n()), aes(label = ProvCode), 
               colour = "grey20", check_overlap=TRUE)+
  scale_fill_viridis_d()+
  coord_sf(expand = FALSE)
longMap

# Longhurst shapefile
longs <- readOGR("code/1b_longhurst/longhurst/Longhurst_world_v4_2010.shp")

list.files(path="code/1b_longhurst")

#### Albacore data ----

##March 2021 data
# Albacore locations
# CHANGE DIRECTORY FOR THIS DOC
alb <- read.csv("data/output_data/alb_covars.csv", head = TRUE, sep= ",")

# Set coordinates
View(alb)
coordinates(alb) <- ~ LocatLongitude + LocatLatitude
# Set the projection of the SpatialPointsDataFrame using the projection of the shapefile
proj4string(alb) <- proj4string(longs)

# Extract province associated with each diet sampling data point
# Note diet study longitudes are inconsistent: some in degrees east, some in degrees west? See map below
provinces <- over(alb, longs)
alb$province <- provinces$ProvDescr
alb$code <- provinces$ProvCode
write.csv(alb, "data/output_data/alb_covars_longhurst.csv", row.names = FALSE)

# Map
albMap <- longMap + 
  geom_point(data = as.data.frame(alb), aes(x = LocatLongitude, y = LocatLatitude))
albMap

ggsave("outputs_figures/maps/alb_global_map2.pdf", plot = albMap, dpi = 300)
