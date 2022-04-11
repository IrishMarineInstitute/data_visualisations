# 30 Day Chart Challenge 2022
#Day 7 Physical landscape version
library(ggplot2)
library(ggridges)
transects <- read.csv("C:/Users/Kellie Heney/OneDrive - Marine Institute/Documents/gis/clipped/again/coords.csv")
head(transects)
names(transects)[4] <- "Elevation"
names(transects)[2] <- "Lon"
names(transects)[3] <- "Lat"
names(transects)[5] <- "flip"
head(transects)

joy <- ggplot(transects,
              aes(x=Lon, y = Lat, group = Lat, height = Elevation))+
  geom_density_ridges(stat = "identity",
                      scale = 40,
                      fill = "black",
                      color = "white")+
  scale_x_continuous(name = "EDORAS BANK")  +
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
        axis.title.x = element_text(colour = 'white', size = 18))
joy 

### Portrait Version
# load requisite packages
library(ggplot2)
library(ggridges)
library(mapproj)

# Import the transect data
transects <- read.csv("C:/Users/Kellie Heney/OneDrive - Marine Institute/Documents/gis/clipped/again/coords.csv")

# view data frame and change column headers
head(transects)
names(transects)[1] <- "bath"
names(transects)[2] <- "Lon"
names(transects)[3] <- "Lat"
names(transects)[4] <- "Elev"
names(transects)[5] <- "flip"
# plot the transects with ggplot2 & ggridges
basic <- ggplot(CraterLake_80transects, 
                           aes(x = Lon, y = Lat, group = Lat, height = Elev)) + 
  geom_density_ridges(stat = "identity")

# Call the default plot variable
basic

# customize the appearance to mimic the Unknown Pleasures artwork
Joy <- ggplot(transects, 
                         aes(x = Lon, y = Lat, group = Lat, height = Elev)) + 
  geom_density_ridges(stat = "identity", scale = 20,
                      fill="black", color = "white") +
  
  # set the upper and lower y-axis limits
  #ylim(54.85062138, 56.1243318) +
  
  # add a title to the bottom of the plot frame
  scale_x_continuous(name = "EDORAS BANK")  +
  
  # use theme() to customize the background, axis labels, titles, etc.
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "black"),
        axis.line = element_blank(),
        axis.text.x=element_blank(),
        plot.background = element_rect(fill = "black"),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_text(colour = 'white', size = 18)) + 
  
  # projects the transect data to a specified PCS
  coord_map()

# Call the stylized plot variable
Joy

# Save the plot as a PNG or PDF
ggsave("Joy.png", dpi=300)
ggsave("Joy.pdf")

