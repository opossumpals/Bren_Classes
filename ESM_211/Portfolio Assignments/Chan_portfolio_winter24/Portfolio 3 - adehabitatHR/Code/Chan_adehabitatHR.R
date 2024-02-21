## adehabitatHR - DC
####################


### Our usual friends
library(tidyverse)
library(here)
library(janitor)

### The main package we're working with
install.packages("adehabitatHR")
library(adehabitatHR)

### For working with SpatialPointsDataFrames
library(sp)

### To help with display
library(scales)
library(ggmap)


### Read the csv
elk <- read_csv(here("Portfolio 3 - adehabitatHR", "Data", "elk_collar.csv")) #removed here

### Convert the elk ID column to character to prevent R from getting confused
elk <- elk %>% 
  mutate_at("id", as.character)

### To start, let's filter to 6 elk from the National Elk Refuge
elk_ner <- elk %>% 
  filter(feedground == "National_Elk_Refuge") %>%  #select a feedground of interest
  filter(id %in% c("631", "632", "633", "634", "635", "636")) #select id of elk of interest


#now lets make our dataframe into a spatial dataframe
### SpatialPointsDataFrame objects don't like missing values, so it helps to remove NA's
elk_ner <- elk_ner[!is.na(elk_ner$x) & !is.na(elk_ner$y),]

### Only need three columns (id, x, and y coordinates)
elk.sp <- elk_ner[, c("id", "x", "y")] 

### Define coordinates to create a SpatialPointsDataFrame 
coordinates(elk.sp) <- c("x", "y")

### Set the coordinate reference system (CRS)
### The data are UTM points in WGS84 from zone 12N
proj4string(elk.sp) <- CRS("+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs")




## 1. How does elk home range change relative to the different percentages of relocations being included? 
##### Based on what you know of elk movement, would it be suitable to include all relocation points? 
########### It isn't suitable to include all relocation points because elk are highly mobile animals. 


# calculate the areas of the MCPs for the elk - Change the Percentage of relocations being included in calculating the area of the MCP 
# Use the `mcp` function to calculate the Minimum Convex Polygons. Set the percentage of relocation points to be used to construct the home ranges. Examining the output will provide the home range area. 

### Calculate MCPs for each elk
elk.mcp <- mcp(elk.sp, percent = 100) #change the percentage & see how the polygon shapes & area change below

### Examine output
elk.mcp


#lets visualize our elk MCPs, then we can plot our home range polygons.


### library(scales) # Helps make polygons partly transparent using the alpha argument below
plot(elk.sp, col = as.factor(elk.sp@data$id), pch = 16)
plot(elk.mcp, col = alpha(1:5, 0.5), add = TRUE)


### Calculate the MCP by including 50 to 100 percent of points 
hrs <- mcp.area(elk.sp, percent = seq(50, 100, by = 5))

hrs

### Comprehensive map
# Transform the point and MCP objects. 
elk.spgeo <- spTransform(elk.sp, CRS("+proj=longlat"))
elk.mcpgeo <- spTransform(elk.mcp, CRS("+proj=longlat"))

### To use the stadiamaps basemaps, you will need to generate your own API key
### You can generate your own for free here: https://client.stadiamaps.com/signup/

register_stadiamaps(key = "YourAPIKey")

mybasemap <- get_stadiamap(bbox = c(left = min(elk.spgeo@coords[,1])-0.005, 
                                    bottom = min(elk.spgeo@coords[,2])-0.005, 
                                    right = max(elk.spgeo@coords[,1])+0.005, 
                                    top = max(elk.spgeo@coords[,2])+0.005), 
                           zoom = 12)

# Turn the spatial data frame of points into just a dataframe for plotting in ggmap
elk.geo <- data.frame(elk.spgeo@coords, 
                      id = elk.spgeo@data$id )

mymap.hr <- ggmap(mybasemap) + 
  geom_polygon(data = fortify(elk.mcpgeo),  
               # Polygon layer needs to be "fortified" to add geometry to the dataframe
               aes(long, lat, colour = id, fill = id),
               alpha = 0.3) + # alpha sets the transparency
  geom_point(data = elk.geo, 
             aes(x = x, y = y, colour = id))  +
  theme(legend.position = c(0.15, 0.80)) +
  labs(x = "Longitude", y = "Latitude") +
  scale_fill_manual(name = "Elk ID",
                    values = c("red", "blue", "purple", "green", "orange", "yellow"),
                    breaks = c("631", "632", "633", "634", "635", "636")) +
  scale_colour_manual(name = "Elk ID",
                      values = c("red", "blue", "purple", "green", "orange", "yellow"),
                      breaks = c("631", "632", "633", "634", "635", "636"))
mymap.hr


## 2. Compare the plots from the two different methods of choosing “‘h”. Which might be more suitable for the Elk? 


## 3. Explore how changing the feedground or elk individuals changes the home-range. 
##### What is one thing that stands out to you that is notable/different?
