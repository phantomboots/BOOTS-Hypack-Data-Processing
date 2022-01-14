#=====================================================================================================
# Script Name: Write .SRT files for NDST videos
# Script Function: This script pulls the metadata (file creation time and duration) from collected video files, and calculates video 
#                 duration. Parsed records from ASDL are queried for the interval or each video, and are concatenated to a string that
#                 is formatted as an SRT file, which can be played back along side the video. This script assumes that you have run
#                 NDST scripts "1_...R" through "3....R" to create the necessary files that will be used to generate the SRT.
# Script Author: Ben Snow
# Script Date: Sep 12, 2019
# R Version: 3.5.1
#=====================================================================================================

require(lubridate)
require(av)
require(stringr)
require(readr)
require(dplyr)

#########################################EDIT THESE VALUES####################################################

#Location of exiftool.exe

#exiftool_path <- "C:/temp/"

#Location of video files

video_path <- "D:/PAC2019-015/MiniZeus_StreamZ_Backup/PAC2019-015/HS01_Collingwood1"

#Location of ASDL processed log files

ASDL_path <- "~/Projects/May2019_Phantom_Cruise_PAC2019_015/Data/ASDL/Full_Cruise"

##############################READ IN VIDEO METADATA - NOT STREAMZ!##################################################
          
#Set working directory to the location of the Exiftool executable, and make a system call to this application.

## Exiftool is assumed to be accessible from the configured working dir. Invoking this system command opens exiftool.exe from the current
#working directory. It searches for video files at the directory listed in the system2() call, and extracts the date of creation and
#the duration (seconds) of each video file store there. Output is a character vector that requires additional parsing.
# 
# setwd(exiftool_path)
# args <- c(paste0(" -FileName -CreateDate -Duration -csv ", video_path))
# metadata <- system2("exiftool", args= " -FileName -CreateDate -DateTimeOriginal -Duration -csv E:/HyperDeck Backups", stdout = TRUE)
# 
# #Split the output vector, and create a list. Extract relevant portions in sections below.
# 
# metadata <- str_split(metadata, ",")
# 
# #First 3 list slots are information on the results of the metadata extraction. Ignore these.
# 
# end <- metadata[[4]][3] #Get the create date (the timestamp at the end of the video file)
# end <- ymd_hms(end) #Convert to POSIXct
# duration <- metadata[[4]][4] #Get the file duration
# duration <- as.numeric(gsub(" s","",duration)) #Convert it to numeric object with digits only
# start <- end - duration #Subtract duration from end to get the start time
# video_time <- interval(start, end) #Create an interval object with the date time data.



#Loop through the files, and extract the file name, start time, and duration then make a data frame

all_video <- data.frame()
setwd(video_path)
video_files <- list.files(video_path)

for(i in unique(video_files))
{
  duration <- av_video_info(i)
  duration <- duration[[1]]
  filename <- strsplit(i, "_")
  filename <- filename[[1]][1]
  date_time <- str_extract(i, "\\d{1,}\\_\\d{2}\\_\\d{4}\\_\\d{1,}\\_\\d{2}\\_\\d{2}\\ \\w{2}")
  date_time <- mdy_hms(date_time)
  video_interval <- data.frame(date_time = date_time, filename = filename, duration = duration)
  
  all_video <- bind_rows(all_video, video_interval)
}

#Combine all data timestamps, and generate an end time for the full video


full_duration <- sum(all_video$duration)
full_duration <- seconds_to_period(full_duration)

start <- all_video$date_time[1] #Start time value
end <- all_video$date_time[1] + full_duration #End time value
video_length <- seq(start, end, 1) #Sequence of time, in 1 second increments.

#Calculte elapsed time; generate the time Vector in the format required by .SRT files.

elapsed <- video_length
seconds <- elapsed - elapsed[1]
time <- format(as.POSIXct('0001-01-01 00:00:00') + seconds, "%H:%M:%S")
time <- paste(time, "000", sep = ",")



##############################READ IN THE DATA FROM ASDL#############################################

#Read in the relevant data from the ASDL log files

setwd(ASDL_path)

Depth <- read_csv("SBE25_MasterLog.csv")
Lat_Long <- read_csv("Hemisphere_GPS_Heading_MasterLog.csv")
Altitude <- read_csv("Imagenex_Altitude_MasterLog.csv")

#Remove extra columns from log file Master Logs.

Depth <- Depth[,c(1,4)]
Lat_Long <- Lat_Long[,c(1:3)]



#############################BUILD DF TO USE FOR THE SRT#############################################

#Build a data frame for the the .SRT, split the 1 second tie sequence into a date and time portion.

Date <- date(video_length)
Time <- str_extract(video_length, "\\d{1,}\\:\\d{1,}\\:\\d{1,}")
video <- data.frame(filename = filename, date_time = video_length, Date = Date, Time = Time)

#Join relevant data

video <- left_join(video, Depth, by = "date_time")
video <- left_join(video, Lat_Long, by = "date_time")
video <- left_join(video, Altitude, by = "date_time")

video <- video[,c(1:7)]
names(video) <- c("date_time","Date","Depth","Time","Lat","Long","Altitude")

#Round data in relevant columns

video$Depth_m <- round(video$Depth_m, 1)
video$Altitude_m <- round(video$Altitude_m, 2)
video$Lat <- round(video$Lat, 5)
video$Long <- round(video$Long, 5)


#############################CONSTRUCT SRT FILE######################################################

# create srt 

srt <- list()
for(i in 1:(length(time)-1)){
  s1 <- i
  s2 <- paste(time[i], "-->", time[i+1])
  s3 <- paste("Transect: ", video$filename[i])
  s4 <- paste(video$Date[i], video$Time[i], "UTC")
  s5 <- paste("Latitude =", video$Lat[i])
  s6 <- paste("Longitude =", video$Long[i])
  s7 <- paste("Depth: ", video$Depth_m[i], "m")
  s8 <- paste("Altitude: ", video$Altitude_m[i])
  s9 <- ""
  tmp <- list(s1,s2,s3,s4,s5,s6,s7,s8,s9)
  srt[[i]] <- tmp
}




##################################################
# export srt
setwd(video_path)

fileConn<-file(paste0(filename[1],".srt"))
writeLines(unlist(lapply(srt, paste, collapse="\n")), fileConn)
close(fileConn)




