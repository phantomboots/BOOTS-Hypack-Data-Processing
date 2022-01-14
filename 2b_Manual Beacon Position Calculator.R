    #=====================================================================================================
# Script Name: Manual Tracking Data Processing
# Script Function: This script assumes the either the script '2_ASDL Data Parser_BOOTS.R' or '2_ASDL_Data_Parser_Phatom.R' has been 
#                  run. It reads in 'TrackMan_MasterLog.csv' and 'Hemipshere_GPS_Position.R'. It then computes the coordinates of each 
#                  beacon using the X, Y distance and bearing from the hydrophone specified in the TrackMan master file and converts
#                  these to new Lat/Long positions for the vehicle.
#
# Script Author: Ben Snow
# Script Date: Sep 4, 2019
# R Version: 3.5.1
#=====================================================================================================

#Check for the presence of packages shown below, install any packages that are missing
packages <- c("lubridate","readr","dplyr","stringr","geosphere")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)


#Required packages#

require(lubridate)
require(readr)
require(dplyr)
require(stringr)
require(geosphere)


########################################EDIT THESE DATA ##############################################

#Project folder name

project_folder <- "~/Projects/June2021_BOOTS_Cruise_PAC2021_036"

#Set path for the files processed from the first processing R script (with "1_" at the start)

Processed_path <- paste0(project_folder, "/Data/Processed_Data")

#Set the directory for saving of the master files.

MasterLogs <- paste0(project_folder, "/Data/ASDL/Full_Cruise")

dirs <- c(Processed_path, MasterLogs)

#Check and create directories as needed.

for(i in unique(dirs))
{
  if(dir.exists(i) == FALSE)
  {
    dir.create(i, recursive = TRUE)
  }
}

######################################READ IN HEMIPSHERE GPS AND TRACKMAN MASTER LOGS################

#Read in data

setwd(MasterLogs)
TrackMan <- read_csv("TrackMan_Beacons_MasterLog.csv")
Hemisphere <- read_csv("Hemisphere_GPS_Heading_MasterLog.csv")

#Filter out TrackMan readings where no DistanceX or Distance

TrackMan <- filter(TrackMan, DistanceX_m != 0 & Error_Code != 18)

#Join the Lat/Long positions to the TrackMan DF. Make a matrix of the Lat/Longs.

New_Tracking <- left_join(TrackMan, Hemisphere, by = "date_time")
Long_Lat <- cbind(New_Tracking$Long,New_Tracking$Lat)

#Calculate slant range from DistanceX and DistanceY variables. Set to numeric expicitly, just to be safe. Use absolute values for this.

New_Tracking$DistanceX_m <- as.numeric(New_Tracking$DistanceX_m)
New_Tracking$DistanceY_m <- as.numeric(New_Tracking$DistanceY_m)
New_Tracking$DistanceZ_m <- as.numeric(New_Tracking$DistanceZ_m)
New_Tracking$XY_Slant <- sqrt(abs(New_Tracking$DistanceX_m)^2 + abs(New_Tracking$DistanceY_m)^2)

#Generate new points with X,Y distance and bearing, from existing Lat/Longs

Beacon_Coords <- destPoint(Long_Lat, New_Tracking$Target_Bearing, New_Tracking$XY_Slant)

#Slot the new Beacon tracking points back into the New_Tracking DF.

New_Tracking$Beacon_Long <- Long_Lat[,1]
New_Tracking$Beacon_Long <- -(New_Tracking$Beacon_Long) #Explicitly set longitude values to negative, to ensure this data meshes well with those from other scripts.
New_Tracking$Beacon_Lat <- Long_Lat[,2]

#Drop unecessary columns, write a .CSV file. 

New_Tracking <- New_Tracking[,c(1,2,6:7,22:23)]
write.csv(New_Tracking, paste(MasterLogs,"Manual_Beacon_Tracking_MasterLog.csv", sep = "/"), quote = F, row.names = F)
