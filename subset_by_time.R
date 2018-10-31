#need to find out how and where "sections" turn into "intersections" 
#where is the cut off
#then display this line on the photo

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

#this is the data on the individual vehicles
individ_vehicles <- read.csv(paste0(data_folder, "NGSIM_Peachtree_Vehicle_Trajectories.csv"))

#so cut out a segment of time 
#then subset by any cars that went through the section being studied

#select time segment
#5 mins is 300 seconds
time_segment_start = as.integer(min(individ_vehicles$Global_Time))
time_segment_end = time_segment_start + 300000

section_number = 5

lag_number = 5

#time selected now subset dataframe
individ_vehicles_sub <- individ_vehicles %>%
  group_by(Vehicle_ID) %>%
  arrange(Global_Time) %>%
  filter(Global_Time >= time_segment_start &
           Global_Time <= time_segment_end | 
           lag(Global_Time, n = lag_number) <= time_segment_end &
           lag(Global_Time, n = lag_number) >= time_segment_start |
           lead(Global_Time, n = lag_number) <= time_segment_end &
           lead(Global_Time, n = lag_number) >= time_segment_start,
         Section_ID == section_number |
           lag(Section_ID == section_number, n = lag_number) |
           lead(Section_ID == section_number, n = lag_number))

