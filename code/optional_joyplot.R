# 30 Day Chart Challenge 2022
#possible mountains
library(ggplot2)
library(ggridges)
transects <- read.csv("C:/Users/Kellie Heney/OneDrive - Marine Institute/Documents/gis/INFOMAR_Bathy_10m_Merge_Esrirgid_Raster_WGS84/coords3.csv")
head(transects)
names(transects)[0] <- "ID"
names(transects)[1] <- "Elevation"
names(transects)[2] <- "Lon"
names(transects)[3] <- "Lat"
head(transects)

joy <- ggplot(transects,
              aes(x=Lon, y = Lat, group = Lat, height = Elevation))+
  geom_density_ridges(stat = "identity",
                      scale = 80,
                      fill = "black",
                      color = "white")+
  theme(panel.grid.major= element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(fill= "black"),
        axis.text.x = element_blank(),
        plot.background = element_rect(fill = "black"),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank())
joy