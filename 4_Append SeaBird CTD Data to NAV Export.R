#=====================================================================================================
# Script Name: Append SeaBird CTD data to NAV Export
# Script Function: Reads in SeaBird CTD data that has been converted from a .HEX file to a .CNV file, using the SeaBird data processsing
#                 program. The program then reads in the Hypack NAV files and then searches for matches amongst the timestamps of the
#                  SeaBird data set and, when found, appends these data to the NAV export data writes the merged data to new .CSV files.
#
#                 In the event that the .HEX file cannot be located, the program searches for backup data in the ASDL log files, and appends these
#                 data to the missing record.
#
# Script Author: Ben Snow
# Script Date: May 29, 2019
# R Version: 3.5.3
#
#
######################################################################################################
#                                            CHANGE LOG                                              #
######################################################################################################
#
# May 13, 2020: Added a loop (lines 108-129) to check to see if CTD data was missing from any files, if so new loop 
#               will attempt to retrieve missing data from ASDL and will fill in missing records if possible
#               Note that this process will not provide Dissolved Oxygen Concentration of Density Anomaly data, as
#               these are calculated in Ruskin. May be possible to manually calculate these and add at a later date.
#
#=====================================================================================================

#Check if necessary packages are present, install as required.
packages <- c("lubridate","readxl","readr","dplyr")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)


#Required packages#

require(lubridate)
require(readxl)
require(readr)
require(dplyr)


################################################# EDIT THESE VALUES ##################################

#Set working directory for location of SeaBird .ASC files exported from SeaBird data processing program.

SBE_input <- "~/Projects/July2019_BOOTS Cruise_PAC2019_014/Data/SBE25"

#Set working directory for the ASDL SBE_Master, in case data needs to be recoverd

Master_ASDL <- "~/Projects/July2019_BOOTS Cruise_PAC2019_014/Data/ASDL/Full_Cruise"

#Set working directory for location of smoothed .CSV files from the Hypack Export

NAV_input <- "~/Projects/July2019_BOOTS Cruise_PAC2019_014/Data/Final Exports"

#Set a path for saving data exported at the end of this script

save_path <- "~/Projects/July2019_BOOTS Cruise_PAC2019_014/Data/NAV_with_CTD"

####################################READ IN SBE DATA FROM .ASC FILES####################################

#Read in SBE25 data from .ASC files.

setwd(SBE_input)

#Create blank data frame to fill in the loop below.

SBE_merged <- data.frame()

#Read in all SBE25 .ASC files in the directory, and merge them into one larger file.

SBE_files <- list.files(pattern = ".asc")

for(i in 1:length(SBE_files))
{
  name <- as.character(i)
  assign(name, read_csv(SBE_files[i]))
  SBE_merged <- bind_rows(SBE_merged, get(name))
  rm(list = c(i)) #Discard the temp files.
}

#Merge the date and time columns into a single date_time column

SBE_merged$date_time <- mdy_hms(paste0(SBE_merged$`mm/dd/yyyy`,"_",SBE_merged$`hh:mm:ss`))

#Drop, the date, time and Depth columns

SBE_merged <- SBE_merged[,c(15,6:14)]

######################################READ IN THE DATA FROM NAV FILES#########################################

#Move back to the directory with the Processed .CSV data.

setwd(NAV_input)
NAV_files <- list.files()

#Read in all NAV Data files into their own data frames.

for(i in 1:length(NAV_files))
{
  assign(NAV_files[i], read_csv(NAV_files[i]))
  names <- names(get(NAV_files[i])) #Store the columns names in a character vector, for use later.
}

#Match the RBR WQ data to each of the NAV data files, and append that WQ data to those files.

for(k in 1:length(NAV_files))
{
  temp <- get(NAV_files[k])
  temp <- left_join(temp, SBE_merged, by = "date_time")
  assign(NAV_files[k], temp)
}

############################################CHECK ASDL LOG FILES IN CASE MISSING DATA CAN BE RETRIEVED################################################

#Check to see if there are NA values in any of the SBE WQ data that was appended to the NAV files in the left_join() from
#the previous loop. If NAs are present, it may mean that the data was not saved as a .ASC file, or that SeaSave crashed during the dive, or during export 
#during the export. If this is the case, data may still be recoverable from the ADSL records. Check the SBE_Master file created by 
#'2_ASDL Data Parser_Phantom.R' to see if backup data can be found. If backup data is present, fill in the NA values with this data.


#Read in the SBE25 CTD Master Log from ASDL

setwd(Master_ASDL)
SBE_Master <- read_csv("SBE25_MasterLog.csv")

#Search for missing values, replace as needed. 

for(j in 1:length(NAV_files))
{
  fill <- get(NAV_files[j])
  temp1 <- which(is.na(fill$T090C)) #Indices where there are NA values in the CTD data set from the .ASC files
  temp2 <- fill$date_time[temp1] #Timestamps of the indices where the NA values are
  SBE_to_fill <- get("SBE_Master")
  index <- match(temp2, SBE_to_fill$date_time)
  
  #Try to fill in the missing water quality data from the SBE25 Master
  
  fill$T090C[temp1] <- SBE_to_fill$Temp_C[index]
  fill$`C0mS/cm`[temp1] <- SBE_to_fill$Conductivity_msCm[index]
  fill$Density00[temp1] <- SBE_to_fill$Density_kg_m[index]
  fill$Sal00[temp1] <- SBE_to_fill$Salinity_PSU[index]
  fill$SvWM[temp1] <- SBE_to_fill$SoundVelocity_Wilson[index]
  fill$`OxsatML/L`[temp1] <- SBE_to_fill$Ox_Sat_mlL[index]
  fill$Sbeox0PS[temp1] <- SBE_to_fill$SBE43_02Conc[index]
  fill$Flag[temp1] <- SBE_to_fill$Data_Flag[index]
  
  assign(NAV_files[j], fill)
}

#Rewrite the columns names, start with the names saved from the NAV file import earlier in this script.

full_names <- c(names, "Temperature_C","Conductivity_mScm","Density_kg_m3","Salinity_PSU","SoundVelocity_ms",
                "Ox_Sat_mlL","SBE43_02Conc","Descent_rate_ms","Data_Flag")

for(h in 1:length(NAV_files))
{
  temp <- get(NAV_files[h])
  names(temp) <- full_names
  assign(NAV_files[h], temp)
}

#Write new .CSV files for each of the data frames with merged data in it.

for(j in 1:length(NAV_files))
{
  temp <- get(NAV_files[j])
  write.csv(temp, paste0(save_path,"/",NAV_files[j]), quote = F, row.names = F)
}

