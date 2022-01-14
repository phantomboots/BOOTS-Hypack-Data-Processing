#=====================================================================================================
# Script Name: ASDL Data Processing_BOOTS
# Script Function: Reads in the data files created by Advanced Serial Data logger, parses each file and fills in missing
#                 date_time values. Decimates data to 1Hz and writes once master file for the whole cruise. Create files with 
#                 the suffix '_MasterLog', for use in later steps. Processes records for BOOTS string, Hemisphere GPS position and 
#                 heading, Imagenex Altimeter, Tritech Altimeter (Slant Range), Video Overlay string, MiniZeus Zoom/Focus/Aperture and
#                 TrackMan data records. 
#                 
#                 This script also reads in SBE25 data in ASCII formatted text (.asc), after it has been processed in
#                 the SBE25 data processing program.
#
#                 Each step is proceeded by and if() statement that checks if any data is present before attempting 
#                 process the records from a particular device.
#
# Script Author: Ben Snow
# Script Date: Sep 4, 2019
# R Version: 3.5.1
##################################################################################################################
#                                           CHANGE LOG
##################################################################################################################
#
# May 29, 2020: Changed out of range values for Tritech PA500 altimeter 0f 0, 49.997 and 50m (MiniZeus Slant Range)
#               and Imagenex Altimeter (Altitude) value of 0 and 0.44 m to -9999, instead of N/A
#
#=====================================================================================================

#Check for the presence of packages shown below, install any packages that are missing
packages <- c("lubridate","readr","dplyr","stringr","imputeTS","measurements")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)


#Required packages#

require(lubridate)
require(readr)
require(dplyr)
require(stringr)
require(imputeTS)
require(measurements) #Required to convert Hemisphere GPS from Deg/Decimal mins to Decimal degrees.

########################################EDIT THESE DATA ##############################################

#Set working directory, wherever the Advanced Serial data logger text file are saved.

ASDL_dir <- "~/Projects/July2019_BOOTS Cruise_PAC2019_014/Data/ASDL"
setwd(ASDL_dir)

#Set the directory for saving of the master files.

save_dir <- "~/Projects/July2019_BOOTS Cruise_PAC2019_014/Data/ASDL/Full_Cruise"

#Directory for the SBE25 data in .asc (text) file format.

SBE_dir <- "~/Projects/July2019_BOOTS Cruise_PAC2019_014/Data/SBE25"

##########################READ IN THE BOOTS STRINGS FROM ASDL#########################################

BOOTS_string_files <- list.files(ASDL_dir, pattern = "BOOTS_String")

if(length(BOOTS_string_files) != 0)
{
  
for(i in 1:length(BOOTS_string_files))
{
  name <- as.character(i)
  assign(name, read_csv(BOOTS_string_files[i], col_names = F, col_types = cols(X1 = "c", X2 = "c", X3 ="c", X4 = "c",
                          X5 = "c", X6 = "c", X7 = "c", X8 = "c", X9 = "c", X10 = "c", X11 = "c")))
  if(name == "1")
  {BOOTS_all <- get(name)
  }else BOOTS_all <- bind_rows(BOOTS_all, get(name))
  rm(list = c(i))
}

#Locate data stamp values in the first column of the data that is read in. Stringr:str_extract will return the first match.
#Match the first sequence of 8 contiguous digits, which will be a date (YYYYMMDD).

BOOTS_all$date <- str_extract(BOOTS_all$X1, "\\d{8}")

#Locate the time stamp value in the first column of data that is read in.  Stringr:str_extract will return the first match.
#Match a timestamps of the form hh:mm:ss. Discard milliseconds at this step.

BOOTS_all$time <- str_extract(BOOTS_all$X1, "\\d{2}\\:\\d{2}\\.\\d{2}")

#Replace the period (".") seperator used by Advanced Serial data logger with the more standard colon seperator, between mins 
#seconds.

BOOTS_all$time <- gsub("\\.",":",BOOTS_all$time)

#Create a date_time column from the extract data. Ignore warnings if some data fails to parse right now. These are the rows where
#ASDL put its own time stamp.

BOOTS_all$date_time <- ymd_hms(paste(BOOTS_all$date, BOOTS_all$time, sep = " "))

#Interpolate missing seconds data records. Use the imputeTS:na_interpolation() function. This function does a linear interpolation
#of the missing data records (NA values) use preceeding and proceeding values. Requires a numeric vector as input, which
#is converted back to POSIXct after the intepolation.

full <- na_interpolation(as.numeric(BOOTS_all$date_time))
full <- as.integer(full)
BOOTS_all$date_time <- full #Put it back into the data frame.
BOOTS_all <- BOOTS_all[!duplicated(BOOTS_all$date_time),]
BOOTS_all$date_time <- as.POSIXct(BOOTS_all$date_time, origin = "1970-01-01", tz = "UTC") #Standard R origin value


#Re-arrange the columns and keep the values of interest

BOOTS_all <- BOOTS_all[, c(14,3:10)]
BOOTS_all$X3 <- as.numeric(BOOTS_all$X3)
BOOTS_all$X4 <- as.numeric(BOOTS_all$X4)
BOOTS_all$X5 <- as.numeric(BOOTS_all$X5)
BOOTS_all$X6 <- as.numeric(BOOTS_all$X6)
BOOTS_all$X7 <- as.numeric(BOOTS_all$X7)
BOOTS_all$X8 <- as.numeric(BOOTS_all$X8)
BOOTS_all$X9 <- as.numeric(BOOTS_all$X9)
BOOTS_all$X10 <- as.numeric(BOOTS_all$X10)

#Drop any columns with Depths less than 1 m. This excludes any negative depth readings, or readings around 0, which are
#on deck time.

BOOTS_all <- filter(BOOTS_all, X6 >= 1)


#Rename the columns to the values that are contained int the BOOTS string.

names(BOOTS_all) <- c("date_time","Lat","Long","Heading","Depth_m","BOOTS_pitch","BOOTS_roll","MiniZeus_pan","MiniZeus_tilt")

#Write to .CSV 

write.csv(BOOTS_all, paste(save_dir,"BOOTS_string_MasterLog.csv", sep ="/"), quote = F, row.names = F)

}

#########################READ IN THE TRITECH ALTIMETER SLANT RANGE FROM THE BOOTS STRING#########################

#List files and read into one larger file. This is a comma seperate record, but some files have more columns than others, so need to
#read in files without comma delimeters to start. Use the semi-colon as a bogus delimeter.

Tritech_files <- list.files(ASDL_dir, pattern = "Tritech")

if(length(Tritech_files) !=0)
{

for(i in 1:length(Tritech_files))
{
  name <- as.character(i)
  assign(name, read_delim(Tritech_files[i], col_names = F, delim = ";"))
  if(name == "1")
  {Tritech_all <- get(name)
  }else Tritech_all <- bind_rows(Tritech_all, get(name))
  rm(list = c(i))
}


#Located date stamp values and time stamp values. Replace period in timestamp value with a colon. Parse date_time.

Tritech_all$date <- str_extract(Tritech_all$X1, "\\d{8}")
Tritech_all$time <- str_extract(Tritech_all$X1, "\\d{2}\\:\\d{2}\\.\\d{2}")
Tritech_all$time <- gsub("\\.",":",Tritech_all$time)
Tritech_all$date_time <- ymd_hms(paste(Tritech_all$date, Tritech_all$time, sep = " "))

#Extract the values of the Altimeter, using regular expression.

Tritech_all$slant_range_m <- str_extract(Tritech_all$X1, "\\d{3}\\.\\d{3}")
Tritech_all$slant_range_m <- as.numeric(Tritech_all$slant_range_m)

#Impute the time series, before filtering out any values

full <- na_interpolation(as.numeric(Tritech_all$date_time))
Tritech_all$date_time <- full #Put it back into the data frame.

#Remove duplicate time stamps row. Convert back to a POSIXct object.

Tritech_all$date_time <- as.integer(Tritech_all$date_time)
Tritech_all <- Tritech_all[!base::duplicated(Tritech_all$date_time),]
Tritech_all$date_time <- as.POSIXct(Tritech_all$date_time, origin = "1970-01-01", tz = "UTC") #Standard R origin value

#If the slant range is 0 m, 49.997m or 50 m, this indicates an out of range reading. Substitute in -9999 for these cases.
Tritech_all$slant_range_m[Tritech_all$slant_range_m == 0] <- -9999
Tritech_all$slant_range_m[Tritech_all$slant_range_m == 49.997] <- -9999
Tritech_all$slant_range_m[Tritech_all$slant_range_m == 50] <- -9999

#Keep only the relevant columns, and write to a .CSV file.

Tritech_all <- Tritech_all[,c(4:5)]
write.csv(Tritech_all, paste(save_dir,"Tritech_SlantRange_MasterLog.csv", sep ="/"), quote = F, row.names = F)

}

##################################READ IN THE IMAGENEX ALTIMETER AND CREATE MASTER LOG########################################

#List files and read into one larger file. This is a comma seperate record, but some files have more columns than others, so need to
#read in files without comma delimeters to start. Use the semi-colon as a bogus delimeter.

Imagenex_files <- list.files(ASDL_dir, pattern = "Imagenex")

if(length(Imagenex_files) != 0)
{

for(i in 1:length(Imagenex_files))
{
  name <- as.character(i)
  assign(name, read_delim(Imagenex_files[i], col_names = F, delim = ";", col_types = cols(X1 = "c")))
  if(name == "1")
  {Imagenex_all <- get(name)
  }else Imagenex_all <- bind_rows(Imagenex_all, get(name))
  rm(list = c(i))
}


#Located date stamp values and time stamp values. Replace period in timestamp value with a colon. Parse date_time.

Imagenex_all$date <- str_extract(Imagenex_all$X1, "\\d{8}")
Imagenex_all$time <- str_extract(Imagenex_all$X1, "\\d{2}\\:\\d{2}\\.\\d{2}")
Imagenex_all$time <- gsub("\\.",":",Imagenex_all$time)
Imagenex_all$date_time <- ymd_hms(paste(Imagenex_all$date, Imagenex_all$time, sep = " "))

#Extract altitude based on a regular expression. The (?=,M) means 'followed by ',M'

Imagenex_all$altitude_m <- as.numeric(str_extract(Imagenex_all$X1, "\\d{1,}\\.\\d{2}(?=,M)")) 

#Impute the time series, before filtering out any values

full <- na_interpolation(as.numeric(Imagenex_all$date_time))
Imagenex_all$date_time <- as.integer(full) #Put it back into the data frame as an integer, for filtering purposes.

#Remove duplicate values, convert the date_time back to a POSIXct object.

Imagenex_all <- Imagenex_all[!duplicated(Imagenex_all$date_time),]
Imagenex_all$date_time <- as.POSIXct(Imagenex_all$date_time, origin = "1970-01-01", tz = "UTC") #Standard R origin value

#Set values of 0.44 (on-deck values) or 0, which are indicative of out of range reading, to -9999

Imagenex_all$altitude_m[Imagenex_all$altitude_m == 0] <- -9999
Imagenex_all$altitude_m[Imagenex_all$altitude_m == 0.44] <- -9999


#Drop unused columns and write .CSV MasterLog for the altitude_m

Imagenex_all <- Imagenex_all[,c(4:5)]
write.csv(Imagenex_all, paste(save_dir,"Imagenex_Altitude_MasterLog.csv", sep = "/"), quote = F, row.names = F)

}
########################READ IN MINIZEUS ZOOM, FOCUS, APERTURE AND CREATE MASTER LOG##################################

#List files and read into one larger file. This is a comma seperate record, but some files have more columns than others, so need to
#read in files without comma delimeters to start. Use the semi-colon as a bogus delimeter.

MiniZeus_ZFA_files <- list.files(ASDL_dir, pattern = "MiniZeus_Z")

if(length(MiniZeus_ZFA_files) != 0)
{

for(i in 1:length(MiniZeus_ZFA_files))
{
  name <- as.character(i)
  assign(name, read_csv(MiniZeus_ZFA_files[i], skip = 1, col_names = F, col_types = cols(X1 = "c", X2 = "c", X3 = 'c', X4 = 'c', X5 = 'c',
                                                                               X6 = "c", X7 = "c")))
  if(name == "1")
  {MiniZeus_ZFA_all <- get(name)
  }else MiniZeus_ZFA_all <- bind_rows(MiniZeus_ZFA_all, get(name))
  rm(list = c(i))
}

#Remove and rows with NA values. Choose any column to search for NA values.

MiniZeus_ZFA_all <- filter(MiniZeus_ZFA_all, !is.na(X2))


#Locate date stamp values and time stamp values. Replace period in timestamp value with a colon. Parse date_time.

MiniZeus_ZFA_all$date <- str_extract(MiniZeus_ZFA_all$X1, "\\d{8}")
MiniZeus_ZFA_all$time <- str_extract(MiniZeus_ZFA_all$X1, "\\d{2}\\:\\d{2}\\.\\d{2}")
MiniZeus_ZFA_all$time <- gsub("\\.",":",MiniZeus_ZFA_all$time)
MiniZeus_ZFA_all$date_time <- ymd_hms(paste(MiniZeus_ZFA_all$date, MiniZeus_ZFA_all$time, sep = " "))

#Impute the time series, before filtering out any values

full <- na_interpolation(as.numeric(MiniZeus_ZFA_all$date_time))
MiniZeus_ZFA_all$date_time <- as.integer(full) #Put it back into the data frame as an integer, for filtering purposes.

#Remove duplicate values, convert the date_time back to a POSIXct object.

MiniZeus_ZFA_all <- MiniZeus_ZFA_all[!duplicated(MiniZeus_ZFA_all$date_time),]
MiniZeus_ZFA_all$date_time <- as.POSIXct(MiniZeus_ZFA_all$date_time, origin = "1970-01-01", tz = "UTC") #Standard R origin value

#Drop unused columns, rename kept columns and write .CSV MasterLog for the altitude_m

MiniZeus_ZFA_all <- MiniZeus_ZFA_all[,c(10,3,5,7)]
names(MiniZeus_ZFA_all) <- c("date_time","zoom_percent","focus_percent","aperture_percent")
write.csv(MiniZeus_ZFA_all, paste(save_dir,"MiniZeus_ZFA_MasterLog.csv", sep = "/"), quote = F, row.names = F)

}
#########################################READ IN THE HEMISPHERE GPS POSITION AND HEADING ################################

#List files and read into one larger file. This is a comma seperate record, but some files have more columns than others, so need to
#read in files by skipping the first line of each file to start. 

Hemisphere_GPS_files <- list.files(ASDL_dir, pattern = "Hemisphere_GPS")

if(length(Hemisphere_GPS_files) !=  0)
{

for(i in 1:length(Hemisphere_GPS_files))
{
  name <- as.character(i)
  assign(name, read_delim(Hemisphere_GPS_files[i], col_names = F, delim = ",", skip = 1, col_types = cols(X1 = "c", X2 = "c", X3 = "c",
                          X4 = "c", X5 = "c", X6 = "c", X7 = "c", X8 = "c", X9 = "c", X10 = "c", X11 = "c", X12 = "c", X13 = "c",
                          X14 = "c", X15 = "c")))
  if(name == "1")
  {Hemisphere_GPS_all <- get(name)
  }else Hemisphere_GPS_all <- bind_rows(Hemisphere_GPS_all, get(name))
  rm(list = c(i))
}

#Filter out the ASDL timestamps, don't need them here since $GPZDA string give a timestamp.

Hemisphere_GPS_all$X1 <- gsub("[[:print:]]{1,}>","",Hemisphere_GPS_all$X1)
Hemisphere_GPS_all$X2 <- gsub("[[:print:]]{1,}>","",Hemisphere_GPS_all$X2)

#Filter out to only the $GPGGA, $GPZDA and $HEHDT strings.

Hemisphere_GPS_all <- filter(Hemisphere_GPS_all, X1 == "$GPGGA" | X1 == "$HEHDT" | X1 == "$GPZDA")

#Located date stamp values and time stamp values in the $GPZDA strings. Parse the date_time. Ignore warnings.

Hemisphere_GPS_all$date <- dmy(paste(Hemisphere_GPS_all$X3,Hemisphere_GPS_all$X4, Hemisphere_GPS_all$X5, sep = "-"))
Hemisphere_GPS_all$X2 <- as.integer(Hemisphere_GPS_all$X2) #Ignore the warnings here.
Hemisphere_GPS_all$time <- str_extract(Hemisphere_GPS_all$X2, "\\d{6}")
Hemisphere_GPS_all$date_time <- ymd_hms(paste(Hemisphere_GPS_all$date, Hemisphere_GPS_all$time, sep = " "))

#Impute the time series, before filtering out any values. Set it as an integere before putting back in to original DF, to get rid of
#milliseconds.

full <- na_interpolation(as.numeric(Hemisphere_GPS_all$date_time))
full <- as.integer(full)
Hemisphere_GPS_all$date_time <- as.POSIXct(full, origin = "1970-01-01", tz = "UTC") #Standard R origin value

#Extract the degrees, minutes and seconds information. Combine to a single value with a space in-between.

GPS_position <- filter(Hemisphere_GPS_all, X1 == "$GPGGA")
GPS_position$Lat_deg <- str_extract(GPS_position$X3, "\\d{2}")
GPS_position$Lat_min <- str_extract(GPS_position$X3, "\\d{2}\\.\\d{5,}")
GPS_position$Long_deg <- str_extract(GPS_position$X5, "\\d{3}")
GPS_position$Long_min <- str_extract(GPS_position$X5, "\\d{2}\\.\\d{5,}")
GPS_position$Lat <- paste(GPS_position$Lat_deg, GPS_position$Lat_min, sep = " ")
GPS_position$Long <- paste(GPS_position$Long_deg, GPS_position$Long_min, sep = " ")

#Convert to decimal degrees

GPS_position$Lat <- conv_unit(GPS_position$Lat, "deg_dec_min", "dec_deg")
GPS_position$Long <- conv_unit(GPS_position$Long, "deg_dec_min", "dec_deg")

#Drop unused columns. Remove milliseconds

GPS_position <- GPS_position[,c(18,23:24)]
#GPS_position$date_time <- as_datetime(floor(seconds(GPS_position$date_time)))


Heading <- filter(Hemisphere_GPS_all, X1 == "$HEHDT")
Heading <- Heading[,c(18,2)]


#Join the GPS Position and Heading data, remove duplicated timestamp first

GPS_position <- GPS_position[!duplicated(GPS_position$date_time),]
Heading <- Heading[!duplicated(Heading$date_time),]
GPS_and_Heading <- left_join(GPS_position, Heading, by = "date_time")
names(GPS_and_Heading) <- c("date_time","Lat","Long","Heading")

#Round the Lat/Long values to 5 decimal places, which is equivalent to ~1m accuracy.

GPS_and_Heading$Lat <- as.numeric(GPS_and_Heading$Lat)
GPS_and_Heading$Long <- as.numeric(GPS_and_Heading$Long)
GPS_and_Heading$Lat <- round(GPS_and_Heading$Lat, digits = 5)
GPS_and_Heading$Long <- round(GPS_and_Heading$Long, digits = 5)

#Write to a .CSV.

write.csv(GPS_and_Heading, paste(save_dir,"Hemisphere_GPS_Heading_MasterLog.csv", sep = "/"), quote = F, row.names = F)

}
#######################################READ IN TRACKMAN DATA RECORDS####################################################

#List files and read into one larger file. This is a comma seperate record, but some files have more columns than others, so need to
#read in files by skipping the first line of each file to start. 

TrackMan_files <- list.files(ASDL_dir, pattern = "TrackMan")

if(length(TrackMan_files) != 0)
{

for(i in 1:length(TrackMan_files))
{
  name <- as.character(i)
  assign(name, read_delim(TrackMan_files[i], col_names = F, delim = ",", skip = 1, col_types = cols(X1 = "c", X2 = "c", X3 = "c",
                          X4 = "c", X5 = "c", X6 = "c", X7 = "c", X8 = "c", X9 = "c", X10 = "c", X11 = "c", X12 = "c", X13 = "c",
                          X14 = "c", X15 = "c", X16 = "c", X17 = "c", X18 = "c", X19 = "c")))
  if(name == "1")
  {TrackMan_all <- get(name)
  }else TrackMan_all <- bind_rows(TrackMan_all, get(name))
  rm(list = c(i))
}

#Parse date time from ASDL timestamp entries.

TrackMan_all$date <- str_extract(TrackMan_all$X1, "\\d{8}")
TrackMan_all$time <- str_extract(TrackMan_all$X1, "\\d{2}\\:\\d{2}\\.\\d{2}")
TrackMan_all$time <- gsub("\\.",":",TrackMan_all$time)
TrackMan_all$date_time <- ymd_hms(paste(TrackMan_all$date, TrackMan_all$time, sep = " "))

#Impute the time series, before filtering out any values

full <- na_interpolation(as.numeric(TrackMan_all$date_time))
TrackMan_all$date_time <- as.integer(full) #Put it back into the data frame as an integer, for filtering purposes.

#Remove duplicate values, convert the date_time back to a POSIXct object.

TrackMan_all <- TrackMan_all[!duplicated(TrackMan_all$date_time),]
TrackMan_all$date_time <- as.POSIXct(TrackMan_all$date_time, origin = "1970-01-01", tz = "UTC") #Standard R origin value

#Re-arrange columns. Conver to numeric.

TrackMan_all <- TrackMan_all[,c(22,2,4:18)]
TrackMan_all[,]
names(TrackMan_all) <- c("date_time","Beacon_ID","Phase_Counts_A","Phase_Counts_B","Phase_Counts_C","Quality_Factor","Error_Code",
                         "Target_Slant_Range_m","Depression_Angle","Target_Bearing","DistanceX_m","DistanceY_m","DistanceZ_m",
                         "Ship_Heading","TSS_Pitch","TSS_Roll","Temp_C")
TrackMan_all[,c(2:17)] <- as.numeric(unlist(TrackMan_all[,c(2:17)]))

#Write to a .CSV 

write.csv(TrackMan_all, paste(save_dir,"TrackMan_Beacons_MasterLog.csv", sep = "/"), quote = F, row.names = F)

}

###########################################READ IN THE HYPACK VIDEO OVERLAY STRING#######################################

#List files and read into one larger file. This is a comma seperate record, but some files have more columns than others, so need to
#read in files by skipping the first line of each file to start. 

Overlay_files <- list.files(ASDL_dir, pattern = "Overlay")

if(length(Overlay_files) != 0)
{

for(i in 1:length(Overlay_files))
{
  name <- as.character(i)
  assign(name, read_delim(Overlay_files[i], col_names = F, delim = ",", skip = 1, col_types = cols(X1 = "c", X2 = "c", X3 = "c",
                        X4 = "c", X5 = "c", X6 = "c", X7 = "c", X8 = "c", X9 = "c", X10 = "c", X11 = "c", X12 = "c", X13 = "c")))
  if(name == "1")
  {Overlay_all <- get(name)
  }else Overlay_all <- bind_rows(Overlay_all, get(name))
  rm(list = c(i))
}

#Drop unused columns

Overlay_all <- Overlay_all[,c(2:12)]

#Drop milliseconds from seconds column, filer to 1 seconds intervals (just in case). Write a date_time column.

Overlay_all$X3 <- str_extract(Overlay_all$X3, "\\d{2}\\:\\d{2}\\:\\d{2}")
Overlay_all <- Overlay_all[!duplicated(Overlay_all),]
Overlay_all$date_time <- mdy_hms(paste(Overlay_all$X2, Overlay_all$X3, sep = " "))

#Write column names, then write to .CSV

Overlay_all <- Overlay_all[, c(12,3:11)]
names(Overlay_all) <- c("date_time","Lat","Long","Dive_Name","CTD_Depth_m","BOOTS_Depth_m","Slant_Range_m","Altitude_m",
                        "MiniZeus_pan","MiniZeus_tilt")
write.csv(Overlay_all, paste(save_dir,"Hypack_Overlay_MasterLog.csv", sep = "/"), quote = F, row.names = F)

}

##########################READ IN THE SBE25 AND CREATE MASTER LOG########################################################

#List files and read into one larger file. This is a comma seperate record, but some files have more columns than others, so need to
#read in files without comma delimeters to start. Use the semi-colon as a bogus delimeter.

setwd(SBE_dir)
SBE25_files <- list.files(SBE_dir, pattern = ".asc")

if(length(SBE25_files) != 0)
{

for(i in 1:length(SBE25_files))
{
  name <- as.character(i)
  assign(name, read_csv(SBE25_files[i]))
  if(name == "1")
  {SBE25_all <- get(name)
  }else SBE25_all <- bind_rows(SBE25_all, get(name))
  rm(list = c(i))
}

#Rename column to more R friendly labels, parse date_time.

names(SBE25_all) <- c("date","time","Lat","Long","Depth_m","Temp_C","Conductivity_msCm","Density_kg_m","Salinity_PSU",
                      "SoundVelocity_Wilson","Ox_Sat_mlL","SBE43_02Conc","Data_Flag")
SBE25_all$date_time <- mdy_hms(paste(SBE25_all$date, SBE25_all$time, sep = " "))

#Drop columns where depths are less than 1, meaning on deck time. Filter to 1 Hz

SBE25_all <- SBE25_all[!duplicated(SBE25_all$date_time),]
SBE25_all <- filter(SBE25_all, Depth_m > 1)

#Re-arrange columns and write to .CSV

SBE25_all <- SBE25_all[,c(14,3:13)]
write.csv(SBE25_all, paste(save_dir, "SBE25_MasterLog.csv", sep ="/"), quote = F, row.names = F)

}
