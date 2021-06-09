

library(data.table)
#?fread
data <- fread("./data/occurrences/BD_Mammal_MA_raw.csv")

View(data)

names(data)
unique(data$scientificName)
table(data$scientificName)%>%sort() #to sort list of species by order of number of occurrences

# Subset information of only one species and then
# Remove NA from coordinates
library(tidyverse)
tterrestris <- data %>% 
  filter(scientificName == "Tapirus terrestris") %>% 
  drop_na(decimalLongitude) %>% 
  drop_na(decimalLatitude)

View(tterrestris)

# Put the occurrences points in the map
library(raster)

# Because Latitude and Longitude are characters, we need to convert them and to numeric
tterrestris$decimalLongitude <- as.numeric(as.character(tterrestris$decimalLongitude))
tterrestris$decimalLatitude <- as.numeric(as.character(tterrestris$decimalLongitude))

## Converting data.frame into a SpatialPointsDataFrame for spatial operations
### note that the lon and lat columns are in columns 35 and 36, respectively
### Get long and lat from your data.frame. Make sure that the order is in lon/lat.
xy <- tterrestris[,c(35,36)]

### convert
tterrestris.shp <- SpatialPointsDataFrame(coords = xy, 
                                  data = tterrestris,
                                  proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))


## Read the shapefile of Atlantic Forest
library(rgdal)

atlforest <- readOGR("./data/mata_atlantica_ibge_2006/shape_mata_atlantica_IBGE_5milhoes_policonica_sirgas2000.shp") #loading shapefile

## Check CRS information 
proj4string(atlforest)

## to assign the crs information  
## reproject shp to geographic system WGS 1984
crs(atlforest) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "
proj4string(atlforest)

## Plot 
library(ggplot2)

figura <- ggplot() + 
  geom_polygon(data=atlforest, aes(x = long, y = lat, group = group), fill="grey40", 
               colour = "grey90", alpha = 1) + 
  labs(x="", y="", title="Occurrence points") + #labels
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
        plot.title = element_text(lineheight=.8, face="bold", vjust=1)) + # make title bold and add space
  #geom_point(aes(x = decimalLongitude.1, y = decimalLatitude.1, color = .summary),
  #data = data_clean, alpha = 1, size = 3, color = "grey20") +# to get outline
  geom_point(data = tterrestris, aes(x = decimalLongitude.1, 
                                         y = decimalLatitude.1, 
                                         color = .summary), size = 1) +
  coord_equal(ratio=1) # square plot to avoid the distortion

figura

#Save figure
png("./figs/pts_spp_Gualaxo_AmericaSul.png", res = 300, width = 2400, height = 2400)
figura
#par(mfrow = c(2, 2),  mar = c(5, 5, 4, 1))
dev.off()
