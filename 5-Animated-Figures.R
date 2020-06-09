library(tidyverse)
library(feather)
library(marmap)
library(scales)
library(gganimate)

`%notin%` <- Negate(`%in%`)

setwd("~/Projects/Colombia-drug-IUU//")

aisdat <- read_feather("data/Colombia_Processed_2018-03-10_2018-04-22.feather")

faisdat <- filter(aisdat, timestamp == "2018-04-07 02:00:00")
nrow(faisdat)

lon1 = -96.517685
lon2 = -77.515387
lat1 = -5.186610
lat2 = 15.709619



lon1 = -96.52
lon2 = -77.52
lat1 = -5.19
lat2 = 15.71




# Remove Longbeach
# dat = dat[dat['vessel_A_lat'] <= 33.5]
# dat = dat[dat['vessel_A_lat'] >= 32.5]
# 
# dat = dat[dat['vessel_A_lon'] >= -118]
# dat = dat[dat['vessel_A_lon'] <= -116]
# 


bat <- getNOAA.bathy(lon1, lon2, lat1, lat2, res = 1, keep = TRUE)

# print("Building plot")


faisdat1 <- filter(faisdat, vessel_A_lat <= 10 & vessel_A_lon <= -85)



p1 <- autoplot(bat, geom=c("raster"), coast = TRUE) + 
  scale_fill_gradientn(colours = c("lightsteelblue4", "lightsteelblue2"), 
                         values = rescale(c(-1000, -500, 0)),
                         guide = "colorbar",
                       limits = c(-5000, 0)) +
  coord_cartesian(expand = 0)+
  
  
  
  geom_point(data= faisdat1, aes(vessel_A_lon, vessel_A_lat), size=0.5) +   
  
  
  # geom_point(data=filter(aisdat, vessel_A_lat <= 34 & vessel_A_lon >= -120), aes(vessel_A_lon, vessel_A_lat), size=0.5) +   
  # geom_point(data=NULL, aes(-117.1611, 32.7157), color="white") +    # San Diego Lat/Lon
  # geom_point(data=NULL, aes(-117.2653, 32.9595), color="red") +      # Del Mar Lat/Lon
  # geom_point(data=NULL, aes(-118.1937, 33.7701), color="white") +    # Long Beach Lat/Lon
  # geom_point(data=NULL, aes(-117.0618, 32.3661), color="white") +    # Rosarito Lat/Lon
  # annotate("text", x = -116.8, y = 32.9595, color="red", label="Del Mar") +
  # annotate("text", x = -116.6958, y = 32.7157, color="white", label="San Diego") +
  # annotate("text", x = -117.6284, y = 33.7701, color="white", label="Long Beach") +
  # annotate("text", x = -116.6055, y = 32.3661, color="white", label="Rosarito") +
  theme(legend.position = "none") +
  # labs(x="Longitude", y="Latitude", title = "{current_frame}") +
  labs(x="Longitude", y="Latitude") +
  # annotate("rect", xmin=-118, xmax = -116, ymin = 32.5, ymax = 33.5, color="red")
  
  # transition_manual(frames = timestamp) +
  
  NULL

p1

print("Animating plot")
ap1 <- animate(p1, nframes = 744)

print("Saving plot")
anim_save("figures/Sept2017_animation.gif", ap1)
