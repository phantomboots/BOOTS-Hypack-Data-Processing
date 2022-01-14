#=====================================================================================================
# Script Name: Hypack Data Parser_BOOTS.R
#
# Script Function: This script is designed to unpack the Hypack .LOG files, and to extract various pieces of the data that are contained
# within the logs. The script locates files with the .LOG extension, reads them in and extract the timestamps, device (sensor) names, and
# sensor data for positions, depths, heading, altitude, speed and slant range (from camera). The script searches for duplicate timestamp records
# for data sources that update faster than 1 Hz, and then removes extra records to preserved a common 1 Hz time series for all sensors.
# 
# The script design at this point is to merge all records from the .LOG files in the working directory into one 'master file', and to 
# then use transect start/end time to 'trim' the master file time series to the periods of interest. Specifically, data is trimmed to transect 
# start/end times based on timestamps provided in an accompanying Dive Log file, which is assumed to be an MS Excel file with start/end times for 
# each transect. 
#
# Relevant data are then extract, one parameter at a time, from the trimmed master file. In some cases, the script will search for the preffered
# data source first (i.e. CTD depth, rathen than onboard depth sensor) and will fall back to extracting the secondary source as required, while also
# writing a data flag to alert the user.
#
# In the particular case of position data records, the script will convert the projected UTM coordinates that are stored in the Hypack .LOG file into
# decimal degrees, to facilitate processing in later steps. The user is required to enter the UTM Zone at the start of the script run. 
#
# All data is merged into seperate data frame for each transect, and is written out as .CSV files.
#
# Script Author: Ben Snow
# Script Date: Aug 27, 2019
# R Version: 3.5.1
##################################################################################################################
#                                           CHANGE LOG
##################################################################################################################
#
# May 29, 2020: Padded transect start and end times by 5 minutes on either side, as per request from J.Nehphin and S. Humphries.
# May 29, 2020: Changed out of range values for Tritech PA500 altimeter (MiniZeus Slant Range) and Imagenex Altimeter (Altitude) to -9999, instead of N/A
# June 1, 2020: Both BOOTS heading and ship heading are now exported by this script; previously it was only ship heading. 
# Aug 20, 2021: Added check of subdirectories, and directory auto-creation of directories if they are not present.
# Aug 20, 2021: Added filter to exclude dive log entries with different UTM Zone numbers from further processing.
# Aug 24, 2021: Added loop to generate SpatialPoints data frames for each dive seperately, and convert to lat/long dive by dive. No need to specify UTM Zone any more.
#=================================================================================================================



#Check for the presence of packages shown below, install any packages that are missing
packages <- c("lubridate","sp","readxl","readr","dplyr","stringr","rgdal","zoo")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

#Required packages#

require(lubridate)
require(readxl)
require(readr)
require(dplyr)
require(stringr)
require(rgdal) #This loads package sp 
require(zoo)

###############################EDIT THESE VALUES######################################################

#Project folder name 

project_folder <- "~/Projects/June2021_BOOTS_Cruise_PAC2021_036"

#Names for prefferred devices for position, depth, heading, draft and heave data sources. Must match the names 
#as listed in hardware devices. If a device is not present, write NULL. MAKE SURE DEVICE NAMES MATCH .RAW FILES 
#EXACTLY!

pos_pref <- "AAE_1000_Responder"
depth_pref <- "SBE25_Depth_in"
HPR_pref <- "MiniZeus_Pitch_Roll"
BOOTS_heading_pref <- "BOOTS_string"
ship_heading_pref <- "Ship_Heading"
altitude_pref <- "BOOTS_Altitude_Imagenex"
slant_pref <- "Tritech_slant_range"

#Names for secondary hardware devices, for cases were primary device may have been malfunctioning

pos_secondary <- "Ship_GPS"
depth_secondary <- "BOOTS_string"



#################################CHECK AND MAKE DIRECTORIES AS NEEDED #################################

#Working directory for location of Hypack .RAW files

Hypack_input <- paste0(project_folder, "/Data/Hypack_Backup/Raw")

#Directory for the location of the Dive Log

Log_path <- paste0(project_folder,"/Data/Dive_Logs")

#Directory for saving both clipped and unclipped .CSV files

save_dir <- paste0(project_folder, "/Data/Processed_Data")

#Vector of directories to check for

dirs <- c(Hypack_input, Log_path, save_dir)

#Check and create directories as needed.

for(i in unique(dirs))
{
  if(dir.exists(i) == FALSE)
  {
    dir.create(i, recursive = TRUE)
  }
}

################################READ IN DATA##########################################################

#Files use a single space as a delimeter. Metadata in first 9 lines does not contain sufficient columns, so for now,
#it is skipped. No header row. Read in RAW file exports; loop through all files in the Hypack .RAW directory, 
#and merge them into one file with all the data for a cruise.

#This loop reads in the data into a data frame called 'name', the date for each .RAW file into a DF named 'day and
#the device name into a DF named 'dev'. All three DFs are bound together, and then each file is add to a master 
#'all_input' file. Column types for each read is component are defined explicitly, to expedite the read in process.

#######################################GO GET A COFFEE, THIS TAKES AWHILE!!!#########################


all_input <- data.frame() #Empty DF to fill with data rows.
dev <- data.frame() #Empty DF to fill with device names.

setwd(Hypack_input)

input_files <- list.files(pattern = ".RAW")

for(i in 1:length(input_files))
{
  name <- as.character(i)
  day <- as.character(i+1)
  dev <- as.character(i+2)
  zone <- as.character(i+3)
  assign(name, read_delim(input_files[i], delim = " ", skip = 9, col_names = F, 
                          col_types = cols(X1 = "c", X2 = "i", X3 = "c", X4 = "c", X5 = "c", X6 = "c", X7 = "c", X8 = "c"))) #Read in the data strings
  assign(day, mdy(read_delim(input_files[i], delim = " ", skip = 8, col_names = F, n_max = 1, 
                             col_types = cols_only(X3 = "c")))) #Read in the line in the .RAW file that contains the date, format it as such.
  assign(dev, read_delim(input_files[i], delim = " ", skip = 9, col_names = F, n_max = 50,
                             col_types = cols_only(X1 = "c", X2 = "i", X4 = "c"))) #Read in the lines with DEV; read enough lines to capture 20 or more devices.
  assign(zone, read_delim(input_files[i], delim = " ", col_names = F, skip = 4, n_max = 1, col_types = cols_only(X3 = "c"))) #Read in only column 3, which contains the information on the prime meridian of the UTM zone that was set at the time of data collection.
  name <- filter(get(name), X1 =="POS"| X1 == "EC1"| X1 == "HCP" | X1 == "GYR" | X1 == "DFT") #Filter to data records only
  name$X3 <- as.integer(name$X3)
  first_record <- name$X3[1]
  start_day <- get(day) #Get the day value
  next_day <- as.character(start_day + 1) #Get the next day value, as a character.
  start_day <- as.character(start_day) #Convert the first day value to a character.
  name$day_num[name$X3 >= first_record] <- start_day #If the seconds are less than or equal to the seconds value of the first record, use the starting day.
  name$day_num[name$X3 < first_record] <- next_day #If higher second cound than first record, increment the day by one.
  dev <- filter(get(dev), X1 == "DEV")
  name <- merge(name, dev, by.x = "X2", by.y = "X2") #Merge the device names with the data.
  name <- name[,c(1:5,9,11)] #Drop columns X6 to X8, which are all NAs. Keep day column and device
  name$zone <- NA #Create an empty column at the end of the 'name' dataframe, to store UTM zone values.
  prime_meridian <- get(zone)
  name$zone <- as.integer(prime_meridian) #Fill in the zone column with the value of the prime meridian.
  names(name) <- c("X1","X2","X3","X4","X5","date","device","zone") #Rename the columns
  name$X4 <- as.numeric(name$X4) 
  name$X5 <- as.numeric(name$X5)
  name$sort <- paste0(as.character(name$X2), as.character(name$X3), #A column of unique values to sort by.
                      name$device,as.character(name$date)) 
  name <- name[!duplicated(name$sort),] #Remove duplicates based on sort column values.
  name <- name[, c(1:8)] #Drop the sort column.
  
  all_input <- bind_rows(all_input, name)
  
   
  rm(list = c(i))
}
rm(name)

#Convert the seconds after midnight to a ymd_hms() value. 

all_input$date <- ymd(all_input$date)
all_input$date_time <- ymd_hms(all_input$date + seconds(all_input$X3))

#Convert the prime meridian values to UTM Zone numbers.
all_input$zone[all_input$zone == -123] <- 10
all_input$zone[all_input$zone == -129] <- 9
all_input$zone[all_input$zone == -135] <- 8


################################TRIM THE FILE TO TRANSECT START AND END TIMES#########################

#Read in the start and end time from the Dive Log

log <- read_xlsx(paste(Log_path, list.files(Log_path, pattern = ".xlsx"), sep ="/"), sheet = "Start_End_Times")

#Pad out the transect start/end times by 5 mins each, as requested by users

log$Start_time_UTC <- log$Start_time_UTC - minutes(5)
log$End_time_UTC <- log$End_time_UTC + minutes(5)

#Generate a second by second sequence of timestamps from the start to finish of each dive, coerce to DF
#which include the dive number. Bind sequenced for all dives together.

for(h in 1:length(log$Dive_Name))
{
  name <- log$Dive_Name[h]
  temp <- seq(log$Start_time_UTC[h],log$End_time_UTC[h], 1)
  sec_seq <- data.frame(Dive_Name = name, date_time = temp)
  if(h == 1)
  {full_seq <- sec_seq
  } else full_seq <- rbind(full_seq, sec_seq)
}
rm(sec_seq)

#Write the second by second sequence to a .CSV, for use in later processing stages

write.csv(full_seq, paste(Log_path,"Dive_Times_1Hz.csv", sep = "/"), quote = F, row.names = F)

#Merge the plotting_DF and net_calcs data frames using a common timestamp value.
dives_full <- left_join(full_seq, all_input, by = "date_time")


################################EXTRACT DEPTH DATA####################################################

#Select rows in the first column (X1) that have a 'EC1' identifier. Keep only the data from the primary and secondary
#depth data sources.

depth_data <- filter(dives_full, X2 == "EC1" & device == depth_pref)
depth_data2 <- filter(dives_full, X2 == "EC1" & device == depth_secondary)


depth_data <- depth_data[, c("date_time","Dive_Name","device","X4")]
names(depth_data) <- c("date_time","Dive_Name","device","Depth_m")
depth_data2 <- depth_data2[, c("date_time","Dive_Name","device","X4")]
names(depth_data2) <- c("date_time","Dive_Name","device","Depth_m")

#Where its availalble, insert the depth from the primary depth data source (the CTD). If not, 
#label these rows as NA. Remove duplicates, just to be safe.

depth_all <- left_join(full_seq, depth_data, by = "date_time")
depth_all <- depth_all[!duplicated(depth_all$date_time),]
depth_all <- depth_all[, c(1:2,4:5)] #Keep only the relevant columns

#Insert the secondary depth data source in a seperate column, adjacent to the primary depth source.

depth_all <- left_join(depth_all, depth_data2, by = "date_time")
depth_all <- depth_all[!duplicated(depth_all$date_time),]
depth_all <- depth_all[, c(1:4,6:7)] #Keep only the relevant columns

#Loop through the elements of depth_all, where the there are no depth records from the CTD, fill in with depth records from the StellarTech depth sensor.
#if there are no records from either devices, the depth_all value will remain as NA.

for(k in 1:length(depth_all$date_time))
{
  if(is.na(depth_all$Depth_m.x[k]))
  {depth_all$Depth_m.x[k] <- depth_all$Depth_m.y[k]
  depth_all$device.x[k] <- depth_all$device.y[k]
  }
}

#Keep only required columns, and rename them.

depth_all <- depth_all[,c(1:4)]
names(depth_all) <- c("Dive_Name","date_time","device","Depth_m")

################################EXTRACT HEADING DATA###################################################

#Select rows in the first column (X1) that have a 'GYR' identifier. These should heading values from 
#the ship's GPS source, as well as the onboard compass. Extract both parameters seperately

BOOTS_heading_data <- filter(dives_full, X2 == "GYR" & device == BOOTS_heading_pref) 
ship_heading_data <- filter(dives_full, X2 == "GYR" & device == ship_heading_pref)

#First, get the BOOTS heading data.
BOOTS_heading_data <- left_join(full_seq, BOOTS_heading_data, by = "date_time")
BOOTS_heading_data <- BOOTS_heading_data[!duplicated(BOOTS_heading_data$date_time),]
BOOTS_heading_data <- BOOTS_heading_data[, c(1,2,10,7)]
names(BOOTS_heading_data) <- c("Dive_Name","date_time","device","BOOTS_heading")

#Next, get the ship heading data
ship_heading_data <- left_join(full_seq, ship_heading_data, by = "date_time")
ship_heading_data <- ship_heading_data[!duplicated(ship_heading_data$date_time),]
ship_heading_data <- ship_heading_data[, c(1,2,10,7)]
names(ship_heading_data) <- c("Dive_Name","date_time","device","Ship_heading")

##################################EXTRACT ALTITUDE#####################################################

#BOOTS altitude is read in as a depth data source, through the Imagenex Altimeter. Select depth device ID (EC1), then
#select the prefered altitude device. No substitute device, so those rows that are missing data will retain NA values.

altitude_data <- filter(dives_full, X2 == "EC1" & device == altitude_pref)
altitude_data <- left_join(full_seq, altitude_data, by = "date_time")
altitude_data <- altitude_data[!duplicated(altitude_data$date_time),]
altitude_data <- altitude_data[, c(1,2,10,7)]
names(altitude_data) <- c("date_time","Dive_Name","device","altitude_m")

#If the altitude is 0 m this indicates an out of range reading. If the reading is 0.44 m, this indicates on-deck time. Substitute in -9999 for these cases.
altitude_data$altitude_m[altitude_data$altitude_m == 0] <- -9999
altitude_data$altitude_m[altitude_data$altitude_m == 0.44] <- -9999

#####################################EXTRACT MINIZEUS SLANT RANGE######################################

#BOOTS slant range altitude is read in as a depth data source, through the Tritech Altimeter. Select depth device ID (EC1), then
#select the prefered slant_range device. No substitute device, so those rows that are missing data will retain NA values.


slant_data <- filter(dives_full, X2 == "EC1" & device == slant_pref)
slant_data <- filter(slant_data, X4 > 0 & X4 < 50) 
slant_data <- left_join(full_seq, slant_data, by = "date_time")
slant_data <- slant_data[!duplicated(slant_data$date_time),]
slant_data <- slant_data[, c(1,2,10,7)]
names(slant_data) <- c("date_time","Dive_Name","device","slant_range_m")

#If the slant range is 0 m, 49.997m or 50 m, this indicates an out of range reading. Substitute in -9999 for these cases.
slant_data$slant_range_m[slant_data$Slant_range_m == 0] <- -9999
slant_data$slant_range_m[slant_data$Slant_range_m == 49.997] <- -9999
slant_data$slant_range_m[slant_data$Slant_range_m == 50] <- -9999

################################EXTRACT POSITION DATA#################################################

#Select rows in the first column (X1) that have a 'POS' identifier. Keep only the preffered and secondary POS devices
#If the prefferd source is unavailable, use the secondary source to fill larger gaps. Due to the extreme depth of operation
#of BOOTS, there are some points when the AAE Responder doesn't get a return every second. Ignore these small gaps, and
#focus on gaps larger then 60 secs; fill these in with the secondary position source (Ship_GPS) where possible.
#If not secondary position source, leave as NA and look into this in further post-processings steps.

position_data <- filter(dives_full, X2 == "POS" & device == pos_pref)
position_data2 <- filter(dives_full, X2 == "POS" & device == pos_secondary)


position_data <- position_data[, c(2,1,9,6,7,10)]
names(position_data) <- c("date_time","Dive_Name","device","Beacon_Easting","Beacon_Northing","zone")
position_data2 <- position_data2[, c(2,1,9,6,7,10)]
names(position_data2) <- c("date_time","Dive_Name","device","Ship_Easting","Ship_Northing", "zone")

#Where its availalble, insert the position from the primary position data source (the CTD). If not, 
#label these rows as NA. Remove duplicated just to be safe.

position_all <- left_join(full_seq, position_data, by = "date_time")
position_all <- position_all[!duplicated(position_all$date_time),]

#Insert the secondary position data source in a seperate column, adjacent to the primary depth source.

position_all <- left_join(position_all, position_data2, by = "date_time")
position_all <- position_all[!duplicated(position_all$date_time),]

#Find sequences where the AAE Responder position is missing for more than 60 seconds.

gaps <- rle(is.na(position_all[,5])) #Searched for NA values in column 5
gaps$values <- gaps$values & gaps$lengths >= 60 #This line searches for more than 60 NA values in a row.
position_all$gaps <- inverse.rle(gaps) #Put the TRUE/FALSE indices back into the data frame.

#Loop through the elements of position_all, where the there are no position records from the AAE Responder, 
#fill in with position records from the Ship"s GPS.

for(k in 1:length(position_all$date_time))
{
  if(position_all$gaps[k] == T) #If there is no Lat, there will be no Long either, so just have to search for one of the two
      {position_all$Easting.x[k] <- position_all$Easting.y[k]
       position_all$Northing.x[k] <- position_all$Northing.y[k]
       position_all$device.x[k] <- position_all$device.y[k]
      }
}

position_all <- position_all[,c(2,1,4:6,10:13)]
names(position_all) <- c("date_time","Dive_Name","device","Beacon_Easting","Beacon_Northing",
                         "Ship_Easting","Ship_Northing","zone","Gaps")

######################CONVERT THE POSITION DATA TO DECIMAL DEGREES####################################

#In order to convert the data that is recorded by Hypack as UTM values, the position_all data frame needs to be converted to
#a SpatialPointsDataFrame, which is class in the package(sp), loaded by package(rgdal) at the start of this script. 
#SpatialPointsDataFrames cannot contain NA values, so these value must first be selected for and set aside, since some of these rows with NA
#values are still of interest, they are re-inserted into the data when the SpatialPointsDataFrame object is converted back to a regular data.frame
#SpatialPointsDataFrames also are restricted to a single coordinate reference system (CRS). Since different dives could conceivably by from different
#UTM zones (i.e. different CRS), the loop below splits the position data into seperate transects and load the appropriate CRS that corresponds to the UTM
#Zone that was set in Hypack for each particular dive.

#Empty data frames to fill with the ship and beacon values converted into decimal degree values for latitude and longitude.
ship_geographic <- data.frame()
beacon_geographic <- data.frame()

#Strip NA values, generate spatial points DFs for each transect, transform to Lat/Longs, then save as a dataframe again.
for(i in unique(position_all$Dive_Name))
{
  beacon_no_NA <- filter(position_all, !is.na(Beacon_Easting) & position_all$Dive_Name == i)
  ship_no_NA <- filter(position_all, !is.na(Ship_Easting) & position_all$Dive_Name == i)
  beacon_coordinates <- beacon_no_NA[, c("Beacon_Easting","Beacon_Northing")] # UTM coordinates for the AAE transponder
  ship_coordinates <- ship_no_NA[, c("Ship_Easting","Ship_Northing")] # UTM coordinates for the Ship GPS
  beacon_data <- beacon_no_NA[, c("date_time","Dive_Name","device","Gaps")] # data to keep
  ship_data <- as.data.frame(ship_no_NA[, c("date_time")]) # data to keep
  zone_num <- unique(position_all$zone[position_all$Dive_Name == i]) #Find the unique zone number value associate with each dive number, this might include an NA value.
  zone_num <- zone_num[!is.na(zone_num)] #Control for NA values; drop index values that may be NA, keeping only integer zone number values (8,9 or 10).
  crs <- CRS(paste0("+proj=utm +zone=", zone_num ," +datum=WGS84")) #proj4string of coordinates.
  
  #Assemble the spatial data points DF.
  
  beacon_spatial <- SpatialPointsDataFrame(coords = beacon_coordinates, data = beacon_data, proj4string = crs)
  ship_spatial <- SpatialPointsDataFrame(coords = ship_coordinates, data = ship_data, proj4string = crs)
  
  #Transform the UTMs coordinates in lat/longs in decimal degrees. Then turn it back into a regular DF
  
  beacon_spatial <- spTransform(beacon_spatial, CRS("+proj=longlat +datum=WGS84"))
  ship_spatial <- spTransform(ship_spatial, CRS("+proj=longlat +datum=WGS84"))
  beacon_spatial <- as.data.frame(beacon_spatial)
  beacon_spatial <- beacon_spatial[,c(1:3,5:6)]
  ship_spatial <- as.data.frame(ship_spatial)
  
  beacon_geographic <- bind_rows(beacon_geographic, beacon_spatial)
  ship_geographic <- bind_rows(ship_geographic, ship_spatial)
  
}

#Rename the columns.

names(beacon_geographic) <- c("date_time","Dive_Name","device","Beacon_Long","Beacon_Lat")
names(ship_geographic) <- c("date_time","Ship_Long","Ship_Lat")


#Slot the Long/Lat positions back in the position_all DF, and remove the northing and eastings

position_all <- left_join(position_all, beacon_geographic, by = "date_time")
position_all$Beacon_Easting <- position_all$Beacon_Long
position_all$Beacon_Northing <- position_all$Beacon_Lat
position_all <- left_join(position_all, ship_geographic, by = "date_time")


position_all <- position_all[,c(1:3,12:15,9)]
names(position_all) <- c("date_time","Dive_Name","device","Beacon_Long","Beacon_Lat","Ship_Long","Ship_Lat","Gaps")

#Round all Lat/Long values to the 5th decimal, equivalent to ~ 1m precision.

position_all$Beacon_Lat <- round(position_all$Beacon_Lat, digits = 5)
position_all$Beacon_Long <- round(position_all$Beacon_Long, digits = 5)
position_all$Ship_Lat <- round(position_all$Ship_Lat, digits = 5)
position_all$Ship_Long <- round(position_all$Ship_Long, digits = 5)


#####################################ASSEMBLE ALL DATA TO SINGLE DATA FRAME###########################

#Join position data with depth, heading, altitude and slant range data records

all_data <- position_all
all_data$Depth_m <- depth_all$Depth_m
all_data$BOOTS_heading <- BOOTS_heading_data$BOOTS_heading
all_data$Ship_heading <- ship_heading_data$Ship_heading
all_data$Altitude_m <- altitude_data$altitude_m
all_data$Slant_Range_m <- slant_data$slant_range_m

#Add the data source for each depth records, and the column listing gaps in position

all_data$Depth_Source <- depth_all$device
all_data$Position_Source <- position_all$device

#Re-order columns

all_data <- all_data[,c(1:7,9:15,8)]

#######################################WRITE .CSV FILES FOR EACH DIVE##################################

#Loop through the Dive Names in the all_data frames, write one .CSV for each dive.

for(j in unique(all_data$Dive_Name))
{
  to_write <- filter(all_data, Dive_Name == j)
  write.csv(to_write, file = paste0(save_dir,"/",j,".csv"), quote = F, row.names = F)
}

