## adehabitatHR - DC
####################


### packages
library(tidyverse)
library(here)
library(janitor)
# install.packages("adehabitatHR")
library(adehabitatHR)

### For working with SpatialPointsDataFrames
library(sp)

### To help with display
library(scales)
library(ggmap)
install.packages("sf")
library(sf)


### Read the csv
elk <- read_csv(here("Portfolio 3 - adehabitatHR", "Data", "elk_collar.csv")) #removed here

### Convert the elk ID column to character to prevent R from getting confused
elk <- elk %>% 
  mutate_at("id", as.character)

### To start, let's filter to 6 elk from the National Elk Refuge
elk_ner <- elk %>% 
  filter(feedground == "National_Elk_Refuge") %>%  #select a feedground of interest
  filter(id %in% c("631", "632", "633", "634", "635", "636")) #select id of elk of interest


# turn df into a spatial dataframe
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


### Comprehensive map
# Transform the point and MCP objects. 
elk.spgeo <- spTransform(elk.sp, CRS("+proj=longlat"))
elk.mcpgeo <- spTransform(elk.mcp, CRS("+proj=longlat"))

### To use the stadiamaps basemaps, you will need to generate your own API key
### You can generate your own for free here: https://client.stadiamaps.com/signup/

register_stadiamaps(key = "a1af856f-4055-4548-8dc9-a3bb09c041fc")

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
###### When choosing h, the default function is to use "reference bandwidth" but this has a tendency to oversmooth - estimating a larger home range
###### Least squares cross validation minimizes error by comparing prediction from all data points (great for GPS data, not so much for infrequent data)
###### Ultimately, by looking a the two plots, we can determine the LSCV is better because it does not over estimate their hr. Knowing that elk are 
###### highly mobile animals, it would be inaccurate to overestimate their home range when they may not actually be using those areas. 

kernel.ref <- kernelUD(elk.sp, h = "href")  # href = the reference bandwidth
image(kernel.ref) # plot

kernel.ref[[1]]@h # The smoothing factor is stored for each animal in the "h" slot

kernel.lscv <- kernelUD(elk.sp, h = "LSCV") ### Least squares cross validation

image(kernel.lscv) ### plot

plotLSCV(kernel.lscv) ### Look for a dip

## 3. Explore how changing the feedground or elk individuals changes the home-range. 
##### What is one thing that stands out to you that is notable/different?
##### In comparing the National Elk Refuge and Gros Ventre feedgrounds, it appears that Gros Ventre has a larger area and home range than those at the NER. 
##### This could warrant further research into differen herd dynamics and feedground management to determine why there is a difference between the two. 

#Gros Ventre Elk Feedground
elk_gv <- elk %>% 
  filter(feedground == "Gros_Ventre") %>%  #select a feedground of interest
  filter(id %in% c("245", "246", "247", "248", "249", "250")) #select id of elk of interest

elk_gv <- elk_gv[!is.na(elk_gv$x) & !is.na(elk_gv$y),]

### Only need three columns (id, x, and y coordinates)
elk2.sp <- elk_gv[, c("id", "x", "y")] 

### Define coordinates to create a SpatialPointsDataFrame 
coordinates(elk2.sp) <- c("x", "y")

proj4string(elk2.sp) <- CRS("+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs")

elk2.mcp <- mcp(elk2.sp, percent = 100)

plot(elk2.sp, col = as.factor(elk2.sp@data$id), pch = 16)
plot(elk2.mcp, col = alpha(1:5, 0.5), add = TRUE)


### Calculate the MCP by including 50 to 100 percent of points 
hrs2 <- mcp.area(elk2.sp, percent = seq(50, 100, by = 5))


### Comprehensive map
# Transform the point and MCP objects. 
elk2.spgeo <- spTransform(elk2.sp, CRS("+proj=longlat"))
elk2.mcpgeo <- spTransform(elk2.mcp, CRS("+proj=longlat"))


