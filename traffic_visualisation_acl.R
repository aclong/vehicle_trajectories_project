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
library(dplyr)

#load in as raster
atlanta_peach_raster <- raster(paste0(data_folder, "Atlanta-Peachtree.tif"))

#load in as brick to preserve the rgb/colour info
atlanta_peach_brick <- brick(paste0(data_folder, "Atlanta-Peachtree.tif"),
                             options="TFW=YES")

plot(atlanta_peach_brick)

plotRGB(atlanta_peach_brick)

crs(atlanta_peach_brick)

#find out the column names for aggregating the data
colnames(individ_vehicles)

#look up classes
lapply(individ_vehicles, class)

#see the kinds of answers that come up


#summarise all the data together to use it
vehicle_agg_df <- individ_vehicles %>%
  group_by(Vehicle_ID) %>%
  arrange(Frame_ID) %>%
  mutate(lane_changed = if_else((Lane_ID - lag(Lane_ID))!=0,
                                1,
                                0)) %>%
  summarise(average_speed = sum(v_Vel)/n(), 
            sum_accel = sum(abs(v_Acc)), 
            n_lane_changes = sum(lane_changed))
  