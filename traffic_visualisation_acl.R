#this is where alfie is going to test code - starting with Atlanta

#list of files in the directory
#Atlanta-Peachtree.tfw
#Atlanta-Peachtree.tif
#camera-coverage.dbf
#camera-coverage.sbn
#camera-coverage.sbx
#camera-coverage.shp
#camera-coverage.shx
#detector-data.csv
#NGSIM_Peachtree_Vehicle_Trajectories.csv

#create link to james' data folder
data_folder <- "/data/store05/phd/data/zcfajat/LA Traffic Highway Detector Data /"

#this is the data as it is shown on the camera showing the vehicles by lane, 
#there is an average speed per lane as well as number of vehicles in each lane
detector_data <- read.csv(paste0(data_folder, "detector-data.csv"))


#this is the data on the individual vehicles
individ_vehicles <- read.csv(paste0(data_folder, "NGSIM_Peachtree_Vehicle_Trajectories.csv"))

#now get some packages that you want
library(raster)


#load in as raster
atlanta_peach_raster <- raster(paste0(data_folder, "Atlanta-Peachtree.tif"))

#load in as brick to preserve the rgb/colour info
atlanta_peach_brick <- brick(paste0(data_folder, "Atlanta-Peachtree.tif"),
                             options="TFW=YES")

plot(atlanta_peach_brick)

plotRGB(atlanta_peach_brick)

crs(atlanta_peach_brick)

