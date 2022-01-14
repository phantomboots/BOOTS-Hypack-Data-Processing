#=====================================================================================================
# Script Name: 3_QAQC_Interpolation_Offset_and_Data_Smoothing.R
# Script Function: This script reads in the .CSV files created from "1_Hypack Data Parser_****.R', "2_ASDL Data Processing.R' and
#                  "2b_Manual Beacon Position Calculator.R". It fills in the best position and depth data source for each dive, and 
#                 interpolates Lat/Longs for Ship GPS position and vehicle beacon. Next, distance to GPS track is calculated for each
#                 beacon point, and points outside of median+1.5*IQR are identified as outliers, and beacon fixes at these time points 
#                 are removed. Beacon positions are interpolated a second time (with outliers removed). Beacon positions are then 
#                 smoothed using a rolling median (overlapping medians), using stats::runmed(). Bandwidth is set in the 'EDIT THESE DATA'
#                 portion of these scripts. Final data for each dive are written to .CSV at the end of this script.
#
# Script Author: Ben Snow
# Script Date: Sep 9, 2019
# R Version: 3.5.1
######################################################################################################
#                                            CHANGE LOG                                              #
######################################################################################################
#
# May 29, 2020: Based on conversations with J.Nephin and S. Humphries, added linear interpolation of the depth 
#               data stream. Depth record completion now checks again ASDL to see if additional data records can be found.
#
# May 29, 2020: Added a check to see if there were additional records that could be recovered from ASDL for Altitude and Slant Range.
#               Added linear interpolation of Slant Range and Altitude, but only for cases where there is just one NA value in a row. 
#               This is done by setting zoo::na.approx(..., maxgap = 1).
#
# June 1, 2020: Reworked the initial loop that checks ASDL to replace NA values from the Hypack read-in. This was previously
#               only working properly for the positional data streams (it checked that GAPS == T for all data, not just position)
#
# June 2, 2020: In some cases, interpolation of a -9999 and a normal Altitude or Slant Range values produced values in the range of -4000 or
#               -5000. These is now a check against this, and values are reset to -9999 after interpolation.
#
# July 6, 2020: Removed the creation of .KML and .SHP files from this script, and made that portion of the process into script 4b.
#=====================================================================================================

#Check if necessary packages are present, install as required.
packages <- c("lubridate","readxl","readr","dplyr","stringr","rgdal","zoo","geosphere")
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
require(geosphere)


#########################################EDIT THESE DATA################################################################

#Specify offsets for Ship GPS source. If more than one GPS is used, specify both sources independtely. Offset to the port side are 
#positive values for 'GPS_abeam' and offset towards the bow are positive for 'GPS_along'.

GPS_abeam <- 6.6
GPS_along <- -21.13

#Set working directory for location of Hypack .RAW files

Hypack_input <- "~/Projects/July2019_BOOTS Cruise_PAC2019_014/Data/Hypack Backup/Raw"

#Directory for processed dives

processed_dir <- "~/Projects/July2019_BOOTS Cruise_PAC2019_014/Data/Processed_Data"

#Set a directory for the location of the Dive Log

Log_path <- "~/Projects/July2019_BOOTS Cruise_PAC2019_014/Data/Dive Logs"

#Path for ASDL Master Log files

Master_ASDL <- "~/Projects/July2019_BOOTS Cruise_PAC2019_014/Data/ASDL/Full_Cruise"


#Path for final exports

final_dir <- "~/Projects/July2019_BOOTS Cruise_PAC2019_014/Data/Final Exports"


############################READ IN THE DIVE LOG AND BOOTS STRING #################################


#Read all Master Log files.

setwd(Master_ASDL)
BOOTS_Master <- read_csv("BOOTS_string_MasterLog.csv")
Altitude_Master <- read_csv("Imagenex_Altitude_MasterLog.csv")
Slant_Range_Master <- read_csv("Tritech_SlantRange_MasterLog.csv")
MiniZeus_ZFA_Master <- read_csv("MiniZeus_ZFA_MasterLog.csv")
SBE25_Master <- read_csv("SBE25_MasterLog.csv")
Manual_Tracking_Master <- read_csv("Manual_Beacon_Tracking_MasterLog.csv")
Hemisphere_Master <- read_csv("Hemisphere_GPS_Heading_MasterLog.csv")

#Append a designation column to BOOTS_Master, SBE25Master and Manual Tracking Master

BOOTS_Master$ID <- "BOOTS string backup"
SBE25_Master$ID <- "SBE25 backup"
Manual_Tracking_Master$ID <- "Manual tracking backup"

#Read in each processed transect file as its own data frame. Fill in missing position and depth data if it was missing from a file
#due to a Hypack crash (or failure to start logging!)

setwd(processed_dir)
Dives <- list.files(processed_dir)

for(i in unique(Dives))
{
  
#First, check and fill in any Hypack crash periods with the Ship_GPS and onboard Depth sensor, pull these values from the BOOTS string log file. In some 
#cases where there are many missing values at the start of the file, read_csv() may erroneously set the lat, long, heading or depth columns to character
#to protect against this, all parameters are explictly set to numeric after read in.
  
  name <- i
  assign(name, read_csv(i))
  fill <- get(name)
  temporary1 <- which(is.na(fill$Beacon_Long) & fill$Gaps == T) #The indices where GAPS = T and Longitude is NA
  temporary2 <- fill$date_time[temporary1] #The timestamps values at indices where GAPS = T and Longitude is NA
  Position_to_fill <- get("BOOTS_Master") #Use quote since the object is outside of the environment in the for() loop.
  index <- match(temporary2, Position_to_fill$date_time)
  fill$Beacon_Long[temporary1] <- Position_to_fill$Long[index] #The Long from the BOOTS_string
  fill$Beacon_Lat[temporary1] <- Position_to_fill$Lat[index]  #The Lat from the BOOTS_string
  fill$Position_Source[temporary1] <- Position_to_fill$ID[index]

#Next, check and see if better position data can be retrieved from the BOOTS transponder, fill it in if possible.
  
  Tracking_to_fill <- get("Manual_Tracking_Master")
  index <- match(temporary2, Tracking_to_fill$date_time)
  swap <- which(!is.na(index))
  if(length(swap) != 0)
  {fill$Beacon_Long[temporary1] <- Tracking_to_fill$Beacon_Long[swap] 
  fill$Beacon_Lat[temporary1] <- Tracking_to_fill$Beacon_Lat[swap]  
  fill$Position_Source[temporary1] <- Tracking_to_fill$ID[swap]
  }
  
  
#Same process for the BOOTS heading, but data could be missing from different index locations. Create new index locator variables, specific to heading parameter
  
  temporary1 <- which(is.na(fill$BOOTS_heading))
  temporary2 <- fill$date_time[temporary1]
  BOOTS_heading_to_fill <- get("BOOTS_Master")
  index <- match(temporary2, BOOTS_heading_to_fill$date_time)
  swap <- which(!is.na(index))
  if(length(swap) != 0)
  {fill$BOOTS_heading[temporary1] <- BOOTS_heading_to_fill$Heading[index] #Heading from BOOTS_string
  fill$BOOTS_heading <- as.numeric(fill$BOOTS_heading) #Set as numeric explicitly, just to be safe.
  }
  
#Same process for the ship's heading
  
  temporary1 <- which(is.na(fill$Ship_heading))
  temporary2 <- fill$date_time[temporary1]
  Ship_heading_to_fill <- get("Hemisphere_Master")
  index <- match(temporary2, Ship_heading_to_fill$date_time)
  swap <- which(!is.na(index))
  if(length(swap) != 0)
  {fill$Ship_heading[temporary1] <- Ship_heading_to_fill$Heading[index] #Heading from Hemisphere_GPS_Master Log
  fill$Ship_heading <- as.numeric(fill$Ship_heading) #Set as numeric explicitly, just to be safe.
  }
  
#Same process for the depth from the onboard StellarTech depth sensor, which is read in as part of the BOOTS string.
  
  temporary1 <- which(is.na(fill$Depth_m))
  temporary2 <- fill$date_time[temporary1]
  Depth_to_fill <- get("BOOTS_Master")
  index <- match(temporary2, Depth_to_fill$date_time)
  swap <- which(!is.na(index))
  if(length(swap) != 0)
  {fill$Depth_m[temporary1] <- Depth_to_fill$Depth_m[index] #Depth from BOOTS_Master Log
  fill$Depth_m <- as.numeric(fill$Depth_m) #Set as numeric explicitly, just to be safe.
  }
  
#Now check to see and if better depth data is available from the SBE25 depth data source, fill it in if possible. The indices that have NA values should
#be the same as those in the subsection above, so can use the same value of 'temporary2' variable here.
  
  SBE_to_fill <- get("SBE25_Master")
  index <- match(temporary2, SBE_to_fill$date_time)
  swap <- which(!is.na(index))
  if(length(swap) != 0)
  {fill$Depth_m[temporary1] <- SBE_to_fill$Depth_m[index] 
  fill$Depth_Source[temporary1] <- SBE_to_fill$ID[swap]
  fill$Depth_m <- as.numeric(fill$Depth_m) #Set to numeric explicitly, just to be safe.
  }
  
  
#Check to see if additional data records for MiniZeus Slant Range can be retrieved from the ASDL logs, fill in if possible. This will attempt to replace
#any remaining NA values in the data, but will leave out-of-range readings (-9999) in place.
  
  temporary1 <- which(is.na(fill$Slant_Range_m))
  temporary2 <- fill$date_time[temporary1]
  Slant_to_fill <- get("Slant_Range_Master")
  index <- match(temporary2, Slant_to_fill$date_time)
  swap <- which(!is.na(index))
  if(length(swap) != 0)
  {fill$Slant_Range_m[temporary1] <- Slant_to_fill$slant_range_m[index]
  fill$Slant_Range_m <- as.numeric(fill$Slant_Range_m)  #Need to explicitly set this parameter to numeric, in order to run interpolation with maxgap = 1
  }
  
#Check to see if additional data records for Imagenex Altitude can be retrieved from the ASDL logs, fill in if possible. This will replace attempt to
#replace any remaining NA values in the data, but will leave out of range readings (-9999) in place.
  
  temporary1 <- which(is.na(fill$Altitude_m))
  temporary2 <- fill$date_time[temporary1]
  Alt_to_fill <- get("Altitude_Master")
  index <- match(temporary2, Alt_to_fill$date_time)
  swap <- which(!is.na(index))
  if(length(swap) != 0)
  {fill$Altitude_m[temporary1] <- Alt_to_fill$altitude_m[index]
   fill$Altitude_m <- as.numeric(fill$Altitude_m)  #Need to explicitly set this parameter to numeric, in order to run interpolation with maxgap = 1
  }  
  
#Make sure the Long and Lat values area numeric.
  
  fill$Beacon_Long <- as.numeric(fill$Beacon_Long)
  fill$Beacon_Lat <- as.numeric(fill$Beacon_Lat)
  fill$Ship_Lat <- as.numeric(fill$Ship_Lat)
  fill$Ship_Long <- as.numeric(fill$Ship_Long)
  fill$Depth_m <- as.numeric(fill$Depth_m)
  
#Re-assign the filled in data to to the dive file.
  
  assign(i, fill) #Re-assign the filled in data to the dive file
  rm(list = c("Tracking_to_fill","Position_to_fill","SBE_to_fill","Depth_to_fill","BOOTS_heading_to_fill","Ship_heading_to_fill",
              "Alt_to_fill","Slant_to_fill","fill"))
}


############COERCE THE LAT AND LONGS TO A ZOO OBJECT AND INTERPOLATE MISSING POSITIONS##########

#Zoo objects are totally ordered observations, and each observation must be unique. Coerce the Lat/Long for the 
#beacon data to a Zoo object, and interpolate missing records.

for(i in unique(Dives))
{
  name <- get(i)
  Long <- zoo(name$Beacon_Long, order.by = name$date_time)
  Lat <- zoo(name$Beacon_Lat, order.by = name$date_time)
  Long_intepolated <- na.approx(Long, rule = 2) #Rule 2 means interpolate both forwards and backwards in the time series.
  Long_intepolated <- as.matrix(Long_intepolated) #Convert back to a base object class (a matrix) to extract the components of the zoo object.
  Beacon_Long_interp <- Long_intepolated[1:length(Long_intepolated)]
  Lat_intepolated <- na.approx(Lat, rule = 2) #Rule 2 means interpolate both forwards and backwards in the time series.
  Lat_intepolated <- as.matrix(Lat_intepolated) #Convert back to a base object class (a matrix) to extract the components of the zoo object.
  Beacon_Lat_interp <- Lat_intepolated[1:length(Lat_intepolated)]
  assign(i, cbind(name, Beacon_Long_interp, Beacon_Lat_interp))
}

#Coerce the Lat/Longs for the Ship's GPS position to a Zoo object and interpolate, in the same manner as above.

for(i in unique(Dives))
{
  name <- get(i)
  Long <- zoo(name$Ship_Long, order.by = name$date_time)
  Lat <- zoo(name$Ship_Lat, order.by = name$date_time)
  Long_intepolated <- na.approx(Long, rule = 2) #Rule 2 means interpolate both forwards and backwards in the time series.
  Long_intepolated <- as.matrix(Long_intepolated) #Convert back to a base object class (a matrix) to extract the components of the zoo object.
  Ship_Long_interp <- Long_intepolated[1:length(Long_intepolated)]
  Lat_intepolated <- na.approx(Lat, rule = 2) #Rule 2 means interpolate both forwards and backwards in the time series.
  Lat_intepolated <- as.matrix(Lat_intepolated) #Convert back to a base object class (a matrix) to extract the components of the zoo object.
  Ship_Lat_interp <- Lat_intepolated[1:length(Lat_intepolated)]
  assign(i, cbind(name, Ship_Long_interp, Ship_Lat_interp))
}

#############################ADD IN ALL OTHER DATA SOURCES#####################################################

for(i in unique(Dives))
{
  name <- get(i)
  name <- left_join(name, BOOTS_Master, by = "date_time")
  name <- left_join(name, Hemisphere_Master, by = "date_time")
  name <- left_join(name, MiniZeus_ZFA_Master, by = "date_time")
  name <- name[,c(1:7,16:19,8:15,24:27,32:34)]
  names(name) <- c("date_time","Transect_Name","device","Beacon_Long_raw","Beacon_Lat_raw","Ship_Long_Raw","Ship_Lat_Raw",
                    "Beacon_Long_interp","Beacon_Lat_interp","Ship_Long_interp","Ship_Lat_interp","Depth_m",
               "BOOTS_heading","Ship_heading","Altitude_m","Slant_Range_m","Best_Depth_Source","Best_Position_Source","Gaps","BOOTS_pitch","BOOTS_roll",
               "MiniZeus_pan","MiniZeus_tilt","MiniZeus_Zoom_percent","MiniZeus_Focus_percent","MiniZeus_aperture")
  name$Best_Position_Source[is.na(name$Best_Position_Source)] <- "Linear Interpolation"
  assign(i, name)
}


############COERCE THE SHIPS HEADING TO A ZOO OBJECT AND INTERPOLATE MISSING POSITIONS##########

# Coerce the Ship's Heading data to a zoo object, and interpolate missing records.

for(i in unique(Dives))
{
  name <- get(i)
  Heading <- zoo(name$Ship_heading, order.by = name$date_time)
  Heading_intepolated <- na.approx(Heading, rule = 2) #Rule 2 means interpolate both forwards and backwards in the time series.
  Heading_intepolated <- as.matrix(Heading_intepolated) #Convert back to a base object class (a matrix) to extract the components of the zoo object.
  Heading_intepolated <- Heading_intepolated[1:length(Heading_intepolated)]
  assign(i, mutate(name, Ship_heading = Heading_intepolated)) #Replace the column with missing values with the one that were just interpolated
}

############COERCE THE DEPTH DATA TO A ZOO OBJECT AND INTERPOLATE MISSING DATA##############

# Coerce the depth data to a zoo object, and interpolate missing records. The same process as in the previous section.

for(i in unique(Dives))
{
  name <- get(i)
  Depth <- zoo(name$Depth_m, order.by = name$date_time)
  Depth_interpolated <- na.approx(Depth, rule = 2) #Rule 2 means interpolate both forwards and backwards in the time series.
  Depth_interpolated <- as.matrix(Depth_interpolated) #Convert back to a base object class (a matrix) to extract the components of the zoo object.
  Depth_interpolated <- Depth_interpolated[1:length(Depth_interpolated)]
  assign(i, mutate(name, Depth_m = Depth_interpolated)) #Replace the column with missing values with the one that were just interpolated
}

############COERCE THE SLANT RANGE AND ALTITUDE DATA TO ZOO OBJECTS AND INTERPOLATE MISSING DATA##############

# Coerce the MiniZeus slant range and BOOTS altitude data to a zoo object, and interpolate missing records. This differs from the NA interpolation done above,
# since sequential NA values (i.e. more than one in a row) won't be filled. This is done by setting maxgap = 1.
# This process will generate a zoo object with a different length than the dive data frame that it is derived from, so a left_join is performed and the resulting
# vector of the correct lenght is extracted to be re-inserted back into the dive data frame via dplyr::mutate().

for(i in unique(Dives))
{
  name <- get(i)
  Altitude <- zoo(name$Altitude_m, order.by = name$date_time)
  Altitude_interpolated <- na.approx(Altitude, rule = 2, maxgap = 1) #Rule 2 means interpolate both forwards and backwards in the time series. Maxgap = 1 means that back to back NA values won't be replaced.
  Altitude_interpolated <- as.matrix(Altitude_interpolated) #Convert back to a base object class (a matrix) to extract the components of the zoo object.
  Altitude_names <- rownames(Altitude_interpolated) #Extract the rownames from the matrix (i.e. the timestamps)
  Altitude_interpolated <- Altitude_interpolated[1:length(Altitude_interpolated)] #Extract the data from the matrix
  Altitude_interpolated <- data.frame(date_time = ymd_hms(Altitude_names), Altitude_m = Altitude_interpolated) #Re-assemble the data into a data frame. Set the date_time to a POSIXct object
  Altitude_interpolated <- left_join(name, Altitude_interpolated, by = "date_time") #Join the interpolated date to the dive data frame. 
  Altitude_interpolated$Altitude_m.y[Altitude_interpolated$Altitude_m.y < 0] <- -9999  #Interpolation may have resulted in some negative values that are not -9999, reset all negative values to -9999
  assign(i, mutate(name, Altitude_m = Altitude_interpolated$Altitude_m.y)) #Replace the column with missing values with the one that were just interpolated
}

for(i in unique(Dives))
{
  name <- get(i)
  Slant <- zoo(name$Slant_Range_m, order.by = name$date_time)
  Slant_interpolated <- na.approx(Slant, rule = 2, maxgap = 1) #Rule 2 means interpolate both forwards and backwards in the time series. Maxgap = 1 means that back to back NA values won't be replaced.
  Slant_interpolated <- as.matrix(Slant_interpolated) #Convert back to a base object class (a matrix) to extract the components of the zoo object.
  Slant_names <- rownames(Slant_interpolated) #Extract the rownames from the matrix (i.e. the timestamps)
  Slant_interpolated <- Slant_interpolated[1:length(Slant_interpolated)] #Extract the data from the matrix
  Slant_interpolated <- data.frame(date_time = ymd_hms(Slant_names), Slant_Range_m = Slant_interpolated) #Re-assemble the data into a data frame. Set the date_time to a POSIXct object
  Slant_interpolated <- left_join(name, Slant_interpolated, by = "date_time") #Join the interpolated date to the dive data frame. 
  Slant_interpolated$Slant_Range_m.y[Slant_interpolated$Slant_Range_m.y < 0] <- -9999  #Interpolation may have resulted in some negative values that are not -9999, reset all negative values to -9999
  assign(i, mutate(name, Slant_Range_m = Slant_interpolated$Slant_Range_m.y)) #Replace the column with missing values with the one that were just interpolated
}

###################################APPLY OFFSETS TO POSITIONS DATA SOURCES############################

#Calculate angle created by the abeam and along ship centerline offset values, to be used in calculation of offset from center of ship
#for GPS antenna. For trigonometry purposes, abeam = opposite and along = adjacent.

#Compute length of hypotenuse to determine offset distance, in meters.
offset_dist = sqrt((GPS_abeam^2) + (GPS_along^2))

#For cases where the GPS antenna is offset in both abeam and along ship axes, calculate the angle from the center of the ship to the 
#antenna location.

offset_angle <- atan(GPS_abeam/GPS_along)
offset_angle <- offset_angle * (180/pi) #Convert to degrees.

for(i in unique(Dives))
{
  name <- get(i)
  
  if(GPS_abeam == 0 & GPS_along == 0) {   #Case 1: the GPS antenna is dead center on the ship 
    offset_dist = 0 
  } else if (GPS_abeam == 0) {  #Case 2: GPS antenna along keel line, but fore/aft of center of ship. 
    
    name$bearing <- name$Ship_heading - 180 #Subtract 180 if it is astern of the center of the ship
    name$bearing <- name$Ship_heading  #No change to the heading if it is ahead of the center of the ship
    
  } else if (GPS_along == 0) {  #Case 3: GPS antenna centered fore/aft, but not along keel 
    
    name$bearing <- name$Ship_heading - 90 #Subtract 90 if is to the port of center.
    name$bearing <- name$Ship_heading + 90 #Add 90 if it is to the stbd of center.
    
  } else if (GPS_along != 0 & GPS_abeam != 0) {   #Case 4: Standard case, GPS offset in both abeam and alongship axes.
    
    name$bearing <- name$Ship_heading + offset_angle
  }
  
  name$bearing[name$bearing > 360] <- name$bearing[name$bearing > 360] - 360 #Reduce integer >360 back to values less than of equal to 360
  name$bearing[name$bearing < 0] <- name$bearing[name$bearing < 0] + 360 #Increase negative values by 360, to get correct bearing between 0 and 360
  
  #Calculate the offset positions long/lat.
  
  for(k in 1:length(name$date_time))
  {
    start <- cbind(name$Beacon_Long_interp[k], name$Beacon_Lat_interp[k])
    offset <- destPoint(start, name$bearing[k], offset_dist)
    name$offset_long[k] <-  offset[1]
    name$offset_lat[k] <- offset[2]
    
  }
  mutate(name, Beacon_Long_interp = offset_long, Beacon_Lat_interp = offset_lat)
  name <- name[,1:26] #Drop the offset_lat and offset_long columns
  
  assign(i, name)
}


################################################REMOVE BEACON DATA OUTLIERS#################################

#Calculate cross track distance between GPS track of the Ship and beacon position. Do this by creating a point distance matrix between
#each interpolated beacon position, and the Ship_GPS point for that same second

for(k in unique(Dives))
{
  name <- get(k)
  for(i in 1:length(name$date_time))
  {
    ship <- cbind(name$Ship_Long_interp[i], name$Ship_Lat_interp[i])
    beacon <- cbind(name$Beacon_Long_interp[i], name$Beacon_Lat_interp[i])
    XTE <- distm(ship, beacon)
    name$XTE[i] <- as.numeric(XTE[,1])
  }
  assign(k, name)
}

#Calculate the median XTE value for each dive, and detect outliers as median +/- 1.5*Interquartile range. Set the lat/long values 
#for any indices outside of the values to NA.

for(k in unique(Dives))
{
name <- get(k)
upper <- median(name$XTE) + (1.5*IQR(name$XTE))
lower <- median(name$XTE) - (1.5*IQR(name$XTE))
name$Beacon_Lat_interp[name$XTE > upper | name$XTE < lower] <- NA
name$Beacon_Long_interp[name$XTE > upper | name$XTE < lower] <- NA
assign(k, name)
}

#Coerce to a zoo object and run linear interpolation on the Beacon Lat/Long that have had the outlier values set to NA

for(i in unique(Dives))
{
  name <- get(i)
  Long <- zoo(name$Beacon_Long_interp, order.by = name$date_time)
  Lat <- zoo(name$Beacon_Lat_interp, order.by = name$date_time)
  Long_intepolated <- na.approx(Long, rule = 2) #Rule 2 means interpolate both forwards and backwards in the time series.
  Long_intepolated <- as.matrix(Long_intepolated) #Convert back to a base object class (a matrix) to extract the components of the zoo object.
  name$Beacon_Long_interp <- Long_intepolated[1:length(Long_intepolated)]
  Lat_intepolated <- na.approx(Lat, rule = 2) #Rule 2 means interpolate both forwards and backwards in the time series.
  Lat_intepolated <- as.matrix(Lat_intepolated) #Convert back to a base object class (a matrix) to extract the components of the zoo object.
  name$Beacon_Lat_interp <- Lat_intepolated[1:length(Lat_intepolated)]
  assign(i, name)
}

#Smooth the interpolated beacon position with the Loess smoother, with an alpha value of 0.02

for(i in unique(Dives))
{
  name <- get(i)
  loess_lat <- loess(name$Beacon_Lat_interp ~ as.numeric(name$date_time), span = 0.02)
  name$loess_lat <- loess_lat$fitted
  loess_long <- loess(name$Beacon_Long_interp ~ as.numeric(name$date_time), span = 0.02)
  name$loess_long <- loess_long$fitted
  assign(i, name)
}


#Smooth the interpolated beacon position with a running median - set the bandwitdth to ~ 1 min (59 seconds). Must be an odd value.

for(i in unique(Dives))
{
  name <- get(i)
  name$Beacon_Lat_smooth <- runmed(name$Beacon_Lat_interp, 301)
  name$Beacon_Long_smooth <- runmed(name$Beacon_Long_interp, 301)
  assign(i, name)
}

###################################WRITE FINAL PROCESSED DATA TO FILE####################################################

#Create final processed files, and write to .CSV.

for(i in unique(Dives))
{
  name <- get(i)
  name <- name[,c(1:2,28:29,9:8,30:31,11:10,12:18,20:26)]
  names(name) <- c("date_time","Transect_Name","Beacon_Lat_loess","Beacon_Long_loess","Beacon_Lat_interp","Beacon_Long_interp",
                   "Beacon_Lat_smoothed","Beacon_Long_smoothed","Ship_Lat","Ship_Long","Depth_m",
                   "BOOTS_heading","Ship_Heading","Altitude_m","Slant_Range_m","Best_Depth_Source","Best_Position_Source"
                   ,"BOOTS_pitch","BOOTS_roll","MiniZeus_pan","MiniZeus_tilt","MiniZeus_Zoom_percent","MiniZeus_Focus_percent",
                   "MiniZeus_Aperture_percent")
  write.csv(name, file = paste0(final_dir,"/",i), quote = F, row.names = F)
  assign(i, name)
}
