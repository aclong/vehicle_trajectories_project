#this script is for checking tha the same time error is not present in the other files on peachtree

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

new_data_folder <- "/data/store05/phd/data/ucfnacl/vehicle_trajectories_project/check_data/"

check_data1 <- "peachtree-verify-trajectories-1245pm-0100pm.csv"

check_data2 <- "peachtree-verify-trajectories-0400pm-0415pm.csv"

#now load in the new data and combine

peachtree_check1 <- read.csv(paste0(new_data_folder, check_data1))

peachtree_check2 <- read.csv(paste0(new_data_folder, check_data2))

peachtree_individ_v2 <- rbind(peachtree_check1, peachtree_check2, make.row.names = F)

#load in the old data for comparison

peachtree_individ_v1 <- read.csv(paste0(data_folder, "NGSIM_Peachtree_Vehicle_Trajectories.csv"))

colnames(peachtree_individ_v1)
colnames(peachtree_individ_v2)

#subset the v1 to get rid of the "location" variable before comparing if they are equal

peachtree_individ_v1 <- peachtree_individ_v1[ ,1:24]


all.equal(peachtree_individ_v1, peachtree_individ_v2)

colnames(peachtree_individ_v1)
colnames(peachtree_individ_v2)

all.equal(colnames(peachtree_individ_v1), colnames(peachtree_individ_v2))

#the new data is the same as the old data
#currently finding out how to improve this.