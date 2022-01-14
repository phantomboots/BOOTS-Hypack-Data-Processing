#=====================================================================================================
# Script Name: 6_Summary_Tables.R
# Script Function: This script assumes the either the all data processing scripts numbered 1 through 4 have been run allready
#                  This is an optional script that provides a quick means to generate summary tables for client's cruise reports, 
#                 or similar uses. Generate a table with start depth, end depth, max depth and the start & end lat and longs for each 
#                  transect. The lat/long values and the final, smoothed values generated from the LOESS smoothing algorithm in script
#                  '3_QAQC_Interpolation_Offset_and_Data_Smoothing.R"
#
# Script Author: Ben Snow
# Script Date: Aug 24, 2021
# R Version: 3.5.3
#=====================================================================================================

#Check for the presence of packages shown below, install any packages that are missing
packages <- c("lubridate","readr","dplyr")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)


#Required packages#

require(lubridate)
require(readr)
require(dplyr)


#Force display of long digit numbers

options(digits = 12)

########################################EDIT THESE DATA ##############################################

#Project folder name 

project_folder <- "~/Projects/June2021_BOOTS_Cruise_PAC2021_036"

######################################################################################################

#Directory where the ASDL files are stored
processed_data_dir <- paste0(project_folder, "/Data/NAV_with_CTD")

#Set the directory for saving of the master files.
save_dir <- paste0(project_folder, "/Data/Summary_Tables")

#Vector of directories to check for

dirs <- c(processed_data_dir, save_dir)

#Check and create directories as needed.

for(i in unique(dirs))
{
  if(dir.exists(i) == FALSE)
  {
    dir.create(i, recursive = TRUE)
  }
}

#Set working directory, wherever the master .CSV files are saved.
setwd(processed_data_dir)

#List processed files.

input_files <- list.files()

#Empty dataframe to fill
summary_data <- data.frame(Transect_Name = character(), start_lat= character(), start_long = character(), 
                           end_lat = character(), end_long = character(), start_depth = character(),
                           end_depth = character(), max_depth = character(), stringsAsFactors = F) 


#Read in the files, extract start depth, end depth, max depth and smoothed lat/long values
for(i in 1:length(input_files))
{

  name <- read_csv(input_files[i])
  last <- length(name$Transect_Name) #index of the last record for each input file
  summary_data <-rbind(summary_data, NA) #Add and empty row to fill with data in the next few rows.
  summary_data$Transect_Name[i] <- name$Transect_Name[1]
  summary_data$start_lat[i] <- name$Beacon_Lat_loess[1]
  summary_data$start_long[i] <- name$Beacon_Long_loess[1]
  summary_data$end_lat[i] <- name$Beacon_Lat_loess[last]
  summary_data$end_long[i] <- name$Beacon_Long_loess[last]
  summary_data$start_depth[i] <- name$Depth_m[1]
  summary_data$end_depth[i] <- name$Depth_m[last]
  summary_data$max_depth[i] <- max(name$Depth_m)
  
  summary_data <- bind_rows(summary_data)
}

#The loop above generate a new column of al NA value in the first column. Not sure why. Drop this column.

summary_data <- summary_data[,c(2:9)]

#Write the data to a .CSV

write.csv(summary_data, file = paste0(save_dir,"/Position_Depth_Summary.csv"), quote = F, row.names = F)



