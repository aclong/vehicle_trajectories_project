#################################################################################################################################################################################################################################

#Los Angeles Transportation Data TEST 

TD <- read.csv('detector-data.csv')

TD$Local_Time <- as.character(TD$Local_Time)
TD$Hour <- substring(TD$Local_Time, 1,2)
TD$Hour <- gsub( ":", "", TD$Hour)
TD$Hour <- as.numeric(TD$Hour)

ggplot(TDHour, aes(x = Time, y = , fill = cut)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")


TDHour <- aggregate(list(TD$Sp1, TD$Sp2, TD$Sp3, TD$Sp4, TD$Sp5), by = list(TD$Date, TD$Hour), FUN = mean)
names(TDHour) <- c("Date", "Hour", "AvSpeed1", "AvSpeed2", "AvSpeed3", "AvSpeed4", "AvSpeed5") 

library(ggridges)
library(ggplot2)
library(anytime)
library(tiff)
library(raster)
#################################################################################################################################################################################################################################

##Peachtree Street Atlanta 

Peach <- read.csv('NGSIM_Peachtree_Vehicle_Trajectories.csv')

PeachSB2Straight <- subset(Peach, Section_ID == 2 & Direction == 4 & Lane_ID <= 2)
PeachLane12 <- subset(Peach, Lane_ID <= 2 & Lane_ID > 0)

Peach$TimeTest <- as.Date(anytime(Peach$Global_Time))

ggplot(data= PeachSB2Straight, aes(x=Global_Time, y=Lane_ID, group=Vehicle_ID)) +
  geom_line(linetype="dashed", color = "red", size = 3, alpha = .5) 

#Plots using Corrdinates 
Peach20 <- subset(Peach, Vehicle_ID < 20)
Peach100 <- subset(Peach, Vehicle_ID < 100 & Vehicle_ID > 90)
Peach20$Vehicle_ID <- as.character(Peach20$Vehicle_ID)
Peach100$Vehicle_ID <- as.character(Peach100$Vehicle_ID)

ggplot(data = Peach100, aes(x= Global_X, y = Global_Y, group = Vehicle_ID, color = Vehicle_ID))+
  geom_point() + 
  scale_color_brewer(palette="Set1")

#################################################################################################################################################################################################################################

ROAD <- readOGR(".", "camera-coverage")
ROAD <- spTransform(ROAD, CRS("+proj=longlat + ellps=WGS84"))

#################################################################################################################################################################################################################################

Img <- raster("Atlanta-Peachtree.tif")


