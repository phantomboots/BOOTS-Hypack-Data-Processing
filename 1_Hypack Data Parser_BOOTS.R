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
#                                           DON'T PROCESS FILES FROM SEPERATE UTM ZONES IN THE SAME RUN!
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

#UTM Zone numbers that the dives were done in.

UTM_Zone_to_process = 9

#Project folder name 

project_folder <- "~/Projects/June2021_BOOTS_Cruise_PAC2021_036"

#Names for prefferred devices for position, depth, heading, draft and heave data sources. Must match the names 
#as listed in hardware devices. If a device is not present, write NULL. MAKE SURE DEVICE NAMES MA