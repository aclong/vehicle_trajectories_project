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

#test the datetime of the dataset
individ_vehicles$Global_Time_check <- as.POSIXlt(individ_vehicles$Global_Time, 
                                                 origin = '1970-01-01', 
                                                 tz = 'EST')

min(individ_vehicles$Global_Time_check)
max(individ_vehicles$Global_Time_check)

hist(individ_vehicles$Global_Time_check)

#try cutting up global time by hour
individ_vehicles$Global_Time_cut <- as.numeric(cut(individ_vehicles$Global_Time, breaks = seq(min(individ_vehicles$Global_Time)-216000, max(individ_vehicles$Global_Time)+216000, 216000)))

hist(individ_vehicles$Global_Time_cut)

#cut them up to see the spred over time
individ_vehicles$date <- date(individ_vehicles$Global_Time_check)

hist(individ_vehicles$date)

individ_vehicles$day_cut_num <- as.numeric(individ_vehicles$day_cut)

hist(individ_vehicles$day_cut_num)



#create bar chart of the frequency over
ggplot(individ_vehicles$day_cut) +
  geom_bar(aes(x = individ_vehicles$day_cut))

#now get some packages that you want
library(raster)
library(dplyr)
library(ggplot2)
library(cowplot)
library(corrplot)
library(psych)
library(GPArotation)
library(rgdal)
library(rgeos)
library(lubridate)

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
  group_by(Vehicle_ID, O_Zone, D_Zone) %>%
  arrange(Vehicle_ID, O_Zone, D_Zone, Global_Time) %>%
  mutate(lane_changed = if_else((Lane_ID != lag(Lane_ID)) 
                                & Section_ID > 0,
                                1,
                                0)) %>%
  summarise(vehic_class = median(v_Class),
            vehic_length = median(v_length),
            vehic_width = median(v_Width),
            median_direction = median(Direction),
            median_section = median(Section_ID),
            trip_start_time = min(Global_Time),
            trip_end_time = max(Global_Time),
            tot_time_mins = max(Global_Time/3600) - min(Global_Time/3600),
            avg_speed = sum(v_Vel)/n(), 
            sum_accel = sum(abs(v_Acc)), 
            avg_accel = mean(abs(v_Acc)),
            n_lane_changes = sum(lane_changed, 
                                 na.rm = TRUE)) %>%
  mutate(lane_changes_per_min = n_lane_changes/tot_time_mins,
         sum_accel_per_min = sum_accel/tot_time_mins)

#check out new datasset
summary(vehicle_agg_df)

#check classes
lapply(vehicle_agg_df, class)

#add lane change bins
vehicle_agg_df <- vehicle_agg_df %>%
  mutate(lane_change_cuts = if_else(n_lane_changes < 15, 
                                    as.integer(cut(n_lane_changes, 
                                                   breaks = c(seq(-0.5,15.5,1)),
                                                   labels = c(seq(0,15,1)),
                                                   left = FALSE,
                                                   include.lowest = TRUE,
                                                   dig.lab = 11))-1,
                                    15))



#change lane changes to int
vehicle_agg_df$n_lane_changes <- as.integer(vehicle_agg_df$n_lane_changes)

#see histogram of lane changes
hist(vehicle_agg_df$lane_change_cuts)


#histogram
ggplot(vehicle_agg_df) +
  geom_histogram(aes(x = vehicle_agg_df$n_lane_changes), binwidth = 1)

#get column names for dataframe
colnames(vehicle_agg_df)

#checkt a plot of lane changes against everyting else
ggplot(vehicle_agg_df, aes(x = lane_change_cuts, y = avg_speed)) +
  geom_point() +
  stat_smooth(method=lm)

ggplot(vehicle_agg_df, aes(x = lane_change_cuts, y = sum_accel_per_tottime)) +
  geom_point() +
  stat_smooth(method=lm)

ggplot(vehicle_agg_df, aes(x = lane_change_cuts, y = avg_accel)) +
  geom_point() +
  stat_smooth(method=lm)

ggplot(vehicle_agg_df, aes(x = lane_change_cuts, y = sum_accel)) +
  geom_point() +
  stat_smooth(method=lm)

ggplot(vehicle_agg_df, aes(x = lane_change_cuts, y = vehic_class)) +
  geom_point() +
  stat_smooth(method=lm)

ggplot(vehicle_agg_df, aes(x = lane_change_cuts, y = median_section)) +
  geom_point() +
  stat_smooth(method=lm)



#now to look at the data and ckecj for distribution
#function to make histogram with mean and median of variable

make_hist <- function(variable){
  hist(variable, plot = T, prob = T) +
    lines(density(variable),
          lwd = 2,
          col = "chocolate3") +
    abline(v = mean(variable),
           col = "royalblue",
           lwd = 2) +
    abline(v = median(variable),
           col = "red",
           lwd = 2)
}

make_hist(vehicle_agg_df$lane_change_cuts)

make_hist(vehicle_agg_df$avg_speed)

make_hist(vehicle_agg_df$sum_accel)

#look at a corrplot of all these
pairs.panels(vehicle_agg_df[ , 7:17])

multi.hist(vehicle_agg_df[ , 9:18])

#check out the time variable to make sure that it works
vehicle_agg_df$trip_start_date_time <- as.POSIXct(vehicle_agg_df$trip_start_time, origin = '1970-01-01', tz = 'EDT')

