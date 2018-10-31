#this script is for looking at how movemnet relates to direction

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

#look at all the different directions
#absolute
# 1 - east-bound (EB), 2 - north-bound (NB), 3 - west-bound (WB), 4 - south-bound (SB)
all_directions <- unique(individ_vehicles$Direction)

#movement keys
#subjective
# 1 - through (TH), 2 - left-turn (LT), 3 - right-turn (RT)

#find out if movement is restricted to intersections or cars can turn whilst in section
turning_in_section <- individ_vehicles %>% 
  group_by(Vehicle_ID) %>% 
  arrange(Global_Time) %>% 
  filter(Section_ID == lag(Section_ID) &
           Movement != lag(Movement) &
           Section_ID != 0)

#see if movement correlates with lane ID change
turn_and_lane_change <- individ_vehicles %>% 
  group_by(Vehicle_ID) %>% 
  arrange(Global_Time) %>% 
  filter(Section_ID == lag(Section_ID),
         Movement != lag(Movement),
         Section_ID != 0,
         Lane_ID != lag(Lane_ID) | Lane_ID != lead(Lane_ID))
