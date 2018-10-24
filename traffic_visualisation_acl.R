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

lapply(individ_vehicles, class)

#add cumulative lane change counter for vehicles
individ_vehicles$cumu_lan_change <- for(i in individ_vehicles$Vehicle_ID){
  for j in individ_vehicles$Frame_ID[i]{
    if individ_vehicles$Lane_ID[j] != individ_vehicles$Lane_ID[j-1]
    
  }
}

vehicle_agg_df <- individ_vehicles %>%
  group_by(Vehicle_ID) %>%
  arrange(Frame_ID) %>%
  summarise(average_speed = sum(v_Vel)/n(), 
            sum_accel = sum(abs(v_Acc)), 
            n_lane_changes = if(Frame_ID))

vehicle_agg_df <- group_by(individ_vehicles, Vehicle_ID)

arrange(vehicle_agg_df)
  